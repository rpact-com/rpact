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
## |  File name: test-class_analysis_dataset.R
## |  Creation date: 08 November 2023, 15:29:21
## |  File version: $Revision: 7560 $
## |  Last changed: $Date: 2024-01-15 14:20:32 +0100 (Mo, 15 Jan 2024) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing the Class 'Dataset'")


test_that("Usage of 'getDataset'", {
	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetMeans}
	datasetOfMeans1 <- getDataset(
	    n1 = c(22, 11, 22, 11),
	    n2 = c(22, 13, 22, 13),
	    means1 = c(1, 1.1, 1, 1),
	    means2 = c(1.4, 1.5, 3, 2.5),
	    stDevs1 = c(1, 2, 2, 1.3),
	    stDevs2 = c(1, 2, 2, 1.3)
	)

	## Comparison of the results of DatasetMeans object 'datasetOfMeans1' with expected results
	expect_equal(datasetOfMeans1$stages, c(1, 1, 2, 2, 3, 3, 4, 4), label = paste0("c(", paste0(datasetOfMeans1$stages, collapse = ", "), ")"))
	expect_equal(datasetOfMeans1$groups, c(1, 2, 1, 2, 1, 2, 1, 2), label = paste0("c(", paste0(datasetOfMeans1$groups, collapse = ", "), ")"))
	expect_equal(datasetOfMeans1$subsets, c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_), label = paste0("c(", paste0(datasetOfMeans1$subsets, collapse = ", "), ")"))
	expect_equal(datasetOfMeans1$sampleSizes, c(22, 22, 11, 13, 22, 22, 11, 13), label = paste0("c(", paste0(datasetOfMeans1$sampleSizes, collapse = ", "), ")"))
	expect_equal(datasetOfMeans1$means, c(1, 1.4, 1.1, 1.5, 1, 3, 1, 2.5), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans1$means, collapse = ", "), ")"))
	expect_equal(datasetOfMeans1$stDevs, c(1, 1, 2, 2, 2, 2, 1.3, 1.3), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans1$stDevs, collapse = ", "), ")"))
	expect_equal(datasetOfMeans1$overallSampleSizes, c(22, 22, 33, 35, 55, 57, 66, 70), label = paste0("c(", paste0(datasetOfMeans1$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(datasetOfMeans1$overallMeans, c(1, 1.4, 1.0333333, 1.4371429, 1.02, 2.0403509, 1.0166667, 2.1257143), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans1$overallMeans, collapse = ", "), ")"))
	expect_equal(datasetOfMeans1$overallStDevs, c(1, 1, 1.3814998, 1.4254175, 1.6391506, 1.8228568, 1.5786638, 1.7387056), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans1$overallStDevs, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetOfMeans1), NA)))
	    expect_output(print(datasetOfMeans1)$show())
	    invisible(capture.output(expect_error(summary(datasetOfMeans1), NA)))
	    expect_output(summary(datasetOfMeans1)$show())
	    datasetOfMeans1CodeBased <- eval(parse(text = getObjectRCode(datasetOfMeans1, stringWrapParagraphWidth = NULL)))
	    expect_equal(datasetOfMeans1CodeBased$stages, datasetOfMeans1$stages, tolerance = 1e-07)
	    expect_equal(datasetOfMeans1CodeBased$groups, datasetOfMeans1$groups, tolerance = 1e-07)
	    expect_equal(datasetOfMeans1CodeBased$subsets, datasetOfMeans1$subsets, tolerance = 1e-07)
	    expect_equal(datasetOfMeans1CodeBased$sampleSizes, datasetOfMeans1$sampleSizes, tolerance = 1e-07)
	    expect_equal(datasetOfMeans1CodeBased$means, datasetOfMeans1$means, tolerance = 1e-07)
	    expect_equal(datasetOfMeans1CodeBased$stDevs, datasetOfMeans1$stDevs, tolerance = 1e-07)
	    expect_equal(datasetOfMeans1CodeBased$overallSampleSizes, datasetOfMeans1$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(datasetOfMeans1CodeBased$overallMeans, datasetOfMeans1$overallMeans, tolerance = 1e-07)
	    expect_equal(datasetOfMeans1CodeBased$overallStDevs, datasetOfMeans1$overallStDevs, tolerance = 1e-07)
	    expect_type(names(datasetOfMeans1), "character")
	    df <- as.data.frame(datasetOfMeans1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(datasetOfMeans1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()


	## Comparison of the results of data.frame object 'datasetOfMeans1$.data' with expected results
	expect_equal(datasetOfMeans1$.data$stage, factor(c(1, 1, 2, 2, 3, 3, 4, 4)), label = paste0("c(", paste0(datasetOfMeans1$.data$stage, collapse = ", "), ")"))
	expect_equal(datasetOfMeans1$.data$group, factor(c(1, 2, 1, 2, 1, 2, 1, 2)), label = paste0("c(", paste0(datasetOfMeans1$.data$group, collapse = ", "), ")"))
	expect_equal(datasetOfMeans1$.data$subset, factor(c(NA, NA, NA, NA, NA, NA, NA, NA)), label = paste0("c(", paste0(datasetOfMeans1$.data$subset, collapse = ", "), ")"))
	expect_equal(datasetOfMeans1$.data$sampleSize, c(22, 22, 11, 13, 22, 22, 11, 13), label = paste0("c(", paste0(datasetOfMeans1$.data$sampleSize, collapse = ", "), ")"))
	expect_equal(datasetOfMeans1$.data$mean, c(1, 1.4, 1.1, 1.5, 1, 3, 1, 2.5), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans1$.data$mean, collapse = ", "), ")"))
	expect_equal(datasetOfMeans1$.data$stDev, c(1, 1, 2, 2, 2, 2, 1.3, 1.3), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans1$.data$stDev, collapse = ", "), ")"))
	expect_equal(datasetOfMeans1$.data$overallSampleSize, c(22, 22, 33, 35, 55, 57, 66, 70), label = paste0("c(", paste0(datasetOfMeans1$.data$overallSampleSize, collapse = ", "), ")"))
	expect_equal(datasetOfMeans1$.data$overallMean, c(1, 1.4, 1.0333333, 1.4371429, 1.02, 2.0403509, 1.0166667, 2.1257143), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans1$.data$overallMean, collapse = ", "), ")"))
	expect_equal(datasetOfMeans1$.data$overallStDev, c(1, 1, 1.3814998, 1.4254175, 1.6391506, 1.8228568, 1.5786638, 1.7387056), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans1$.data$overallStDev, collapse = ", "), ")"))
	expect_equal(factor(datasetOfMeans1$stages), datasetOfMeans1$.data$stage, tolerance = 1e-07)
	expect_equal(factor(datasetOfMeans1$groups), datasetOfMeans1$.data$group, tolerance = 1e-07)
	expect_equal(datasetOfMeans1$sampleSizes, datasetOfMeans1$.data$sampleSize, tolerance = 1e-07)
	expect_equal(datasetOfMeans1$means, datasetOfMeans1$.data$mean, tolerance = 1e-07)
	expect_equal(datasetOfMeans1$stDevs, datasetOfMeans1$.data$stDev, tolerance = 1e-07)
	expect_equal(datasetOfMeans1$overallSampleSizes, datasetOfMeans1$.data$overallSampleSize, tolerance = 1e-07)
	expect_equal(datasetOfMeans1$overallMeans, datasetOfMeans1$.data$overallMean, tolerance = 1e-07)
	expect_equal(datasetOfMeans1$overallStDevs, datasetOfMeans1$.data$overallStDev, tolerance = 1e-07)

	x <- getMultipleStageResultsForDataset(datasetOfMeans1)

	## Comparison of the results of StageResultsMeans object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallMeans1, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallMeans2, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallStDevs1, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallStDevs2, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallSampleSizes1, c(22, 33, 55, 66, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallSampleSizes1, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallSampleSizes2, c(22, 35, 57, 70, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallSampleSizes2, collapse = ", "), ")"))
	expect_equal(x$stageResults1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$effectSizes, c(-0.4, -0.40380952, -1.0203509, -1.1090476, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$effectSizes, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	    x$stageResults1CodeBased <- eval(parse(text = getObjectRCode(x$stageResults1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults1CodeBased$overallTestStatistics, x$stageResults1$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallPValues, x$stageResults1$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallMeans1, x$stageResults1$overallMeans1, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallMeans2, x$stageResults1$overallMeans2, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallStDevs1, x$stageResults1$overallStDevs1, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallStDevs2, x$stageResults1$overallStDevs2, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallSampleSizes1, x$stageResults1$overallSampleSizes1, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallSampleSizes2, x$stageResults1$overallSampleSizes2, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$testStatistics, x$stageResults1$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$pValues, x$stageResults1$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$effectSizes, x$stageResults1$effectSizes, tolerance = 1e-07)
	    expect_type(names(x$stageResults1), "character")
	    df <- as.data.frame(x$stageResults1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallMeans1, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallMeans2, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallStDevs1, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallStDevs2, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallSampleSizes1, c(22, 33, 55, 66, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallSampleSizes1, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallSampleSizes2, c(22, 35, 57, 70, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallSampleSizes2, collapse = ", "), ")"))
	expect_equal(x$stageResults2$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$effectSizes, c(-0.4, -0.40380952, -1.0203509, -1.1090476, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$effectSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$combInverseNormal, collapse = ", "), ")"))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$weightsInverseNormal, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	    x$stageResults2CodeBased <- eval(parse(text = getObjectRCode(x$stageResults2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults2CodeBased$overallTestStatistics, x$stageResults2$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallPValues, x$stageResults2$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallMeans1, x$stageResults2$overallMeans1, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallMeans2, x$stageResults2$overallMeans2, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallStDevs1, x$stageResults2$overallStDevs1, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallStDevs2, x$stageResults2$overallStDevs2, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallSampleSizes1, x$stageResults2$overallSampleSizes1, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallSampleSizes2, x$stageResults2$overallSampleSizes2, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$testStatistics, x$stageResults2$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$pValues, x$stageResults2$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$effectSizes, x$stageResults2$effectSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$combInverseNormal, x$stageResults2$combInverseNormal, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$weightsInverseNormal, x$stageResults2$weightsInverseNormal, tolerance = 1e-07)
	    expect_type(names(x$stageResults2), "character")
	    df <- as.data.frame(x$stageResults2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallMeans1, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallMeans2, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallStDevs1, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallStDevs2, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallSampleSizes1, c(22, 33, 55, 66, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallSampleSizes1, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallSampleSizes2, c(22, 35, 57, 70, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallSampleSizes2, collapse = ", "), ")"))
	expect_equal(x$stageResults3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$effectSizes, c(-0.4, -0.40380952, -1.0203509, -1.1090476, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$effectSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$combFisher, collapse = ", "), ")"))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$weightsFisher, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	    x$stageResults3CodeBased <- eval(parse(text = getObjectRCode(x$stageResults3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults3CodeBased$overallTestStatistics, x$stageResults3$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallPValues, x$stageResults3$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallMeans1, x$stageResults3$overallMeans1, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallMeans2, x$stageResults3$overallMeans2, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallStDevs1, x$stageResults3$overallStDevs1, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallStDevs2, x$stageResults3$overallStDevs2, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallSampleSizes1, x$stageResults3$overallSampleSizes1, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallSampleSizes2, x$stageResults3$overallSampleSizes2, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$testStatistics, x$stageResults3$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$pValues, x$stageResults3$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$effectSizes, x$stageResults3$effectSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$combFisher, x$stageResults3$combFisher, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$weightsFisher, x$stageResults3$weightsFisher, tolerance = 1e-07)
	    expect_type(names(x$stageResults3), "character")
	    df <- as.data.frame(x$stageResults3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	datasetOfMeans2 <- getDataset(data.frame(
	    stages = 1:4,
	    n1 = c(22, 11, 22, 11),
	    n2 = c(22, 13, 22, 13),
	    means1 = c(1, 1.1, 1, 1),
	    means2 = c(1.4, 1.5, 3, 2.5),
	    stDevs1 = c(1, 2, 2, 1.3),
	    stDevs2 = c(1, 2, 2, 1.3)
	))
	x <- getMultipleStageResultsForDataset(datasetOfMeans2)

	## Comparison of the results of StageResultsMeans object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallMeans1, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallMeans2, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallStDevs1, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallStDevs2, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallSampleSizes1, c(22, 33, 55, 66, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallSampleSizes1, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallSampleSizes2, c(22, 35, 57, 70, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallSampleSizes2, collapse = ", "), ")"))
	expect_equal(x$stageResults1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$effectSizes, c(-0.4, -0.40380952, -1.0203509, -1.1090476, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$effectSizes, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	    x$stageResults1CodeBased <- eval(parse(text = getObjectRCode(x$stageResults1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults1CodeBased$overallTestStatistics, x$stageResults1$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallPValues, x$stageResults1$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallMeans1, x$stageResults1$overallMeans1, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallMeans2, x$stageResults1$overallMeans2, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallStDevs1, x$stageResults1$overallStDevs1, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallStDevs2, x$stageResults1$overallStDevs2, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallSampleSizes1, x$stageResults1$overallSampleSizes1, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallSampleSizes2, x$stageResults1$overallSampleSizes2, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$testStatistics, x$stageResults1$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$pValues, x$stageResults1$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$effectSizes, x$stageResults1$effectSizes, tolerance = 1e-07)
	    expect_type(names(x$stageResults1), "character")
	    df <- as.data.frame(x$stageResults1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallMeans1, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallMeans2, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallStDevs1, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallStDevs2, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallSampleSizes1, c(22, 33, 55, 66, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallSampleSizes1, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallSampleSizes2, c(22, 35, 57, 70, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallSampleSizes2, collapse = ", "), ")"))
	expect_equal(x$stageResults2$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$effectSizes, c(-0.4, -0.40380952, -1.0203509, -1.1090476, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$effectSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$combInverseNormal, collapse = ", "), ")"))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$weightsInverseNormal, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	    x$stageResults2CodeBased <- eval(parse(text = getObjectRCode(x$stageResults2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults2CodeBased$overallTestStatistics, x$stageResults2$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallPValues, x$stageResults2$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallMeans1, x$stageResults2$overallMeans1, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallMeans2, x$stageResults2$overallMeans2, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallStDevs1, x$stageResults2$overallStDevs1, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallStDevs2, x$stageResults2$overallStDevs2, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallSampleSizes1, x$stageResults2$overallSampleSizes1, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallSampleSizes2, x$stageResults2$overallSampleSizes2, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$testStatistics, x$stageResults2$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$pValues, x$stageResults2$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$effectSizes, x$stageResults2$effectSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$combInverseNormal, x$stageResults2$combInverseNormal, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$weightsInverseNormal, x$stageResults2$weightsInverseNormal, tolerance = 1e-07)
	    expect_type(names(x$stageResults2), "character")
	    df <- as.data.frame(x$stageResults2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallMeans1, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallMeans2, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallStDevs1, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallStDevs2, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallSampleSizes1, c(22, 33, 55, 66, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallSampleSizes1, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallSampleSizes2, c(22, 35, 57, 70, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallSampleSizes2, collapse = ", "), ")"))
	expect_equal(x$stageResults3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$effectSizes, c(-0.4, -0.40380952, -1.0203509, -1.1090476, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$effectSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$combFisher, collapse = ", "), ")"))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$weightsFisher, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	    x$stageResults3CodeBased <- eval(parse(text = getObjectRCode(x$stageResults3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults3CodeBased$overallTestStatistics, x$stageResults3$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallPValues, x$stageResults3$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallMeans1, x$stageResults3$overallMeans1, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallMeans2, x$stageResults3$overallMeans2, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallStDevs1, x$stageResults3$overallStDevs1, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallStDevs2, x$stageResults3$overallStDevs2, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallSampleSizes1, x$stageResults3$overallSampleSizes1, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallSampleSizes2, x$stageResults3$overallSampleSizes2, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$testStatistics, x$stageResults3$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$pValues, x$stageResults3$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$effectSizes, x$stageResults3$effectSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$combFisher, x$stageResults3$combFisher, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$weightsFisher, x$stageResults3$weightsFisher, tolerance = 1e-07)
	    expect_type(names(x$stageResults3), "character")
	    df <- as.data.frame(x$stageResults3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	datasetOfMeans3 <- getDataset(
	    overallSampleSizes1 = c(22, 33, 55, 66),
	    overallSampleSizes2 = c(22, 35, 57, 70),
	    overallMeans1 = c(1, 1.033333, 1.02, 1.016667),
	    overallMeans2 = c(1.4, 1.437143, 2.040351, 2.125714),
	    overallStDevs1 = c(1, 1.381500, 1.639151, 1.578664),
	    overallStDevs2 = c(1, 1.425418, 1.822857, 1.738706)
	)

	## Comparison of the results of DatasetMeans object 'datasetOfMeans3' with expected results
	expect_equal(datasetOfMeans3$stages, c(1, 1, 2, 2, 3, 3, 4, 4), label = paste0("c(", paste0(datasetOfMeans3$stages, collapse = ", "), ")"))
	expect_equal(datasetOfMeans3$groups, c(1, 2, 1, 2, 1, 2, 1, 2), label = paste0("c(", paste0(datasetOfMeans3$groups, collapse = ", "), ")"))
	expect_equal(datasetOfMeans3$subsets, c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_), label = paste0("c(", paste0(datasetOfMeans3$subsets, collapse = ", "), ")"))
	expect_equal(datasetOfMeans3$sampleSizes, c(22, 22, 11, 13, 22, 22, 11, 13), label = paste0("c(", paste0(datasetOfMeans3$sampleSizes, collapse = ", "), ")"))
	expect_equal(datasetOfMeans3$means, c(1, 1.4, 1.099999, 1.5000004, 1.0000005, 3.0000001, 1.000002, 2.4999979), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans3$means, collapse = ", "), ")"))
	expect_equal(datasetOfMeans3$stDevs, c(1, 1, 2.0000005, 2.0000009, 2.0000005, 1.9999999, 1.2999989, 1.3000023), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans3$stDevs, collapse = ", "), ")"))
	expect_equal(datasetOfMeans3$overallSampleSizes, c(22, 22, 33, 35, 55, 57, 66, 70), label = paste0("c(", paste0(datasetOfMeans3$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(datasetOfMeans3$overallMeans, c(1, 1.4, 1.033333, 1.437143, 1.02, 2.040351, 1.016667, 2.125714), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans3$overallMeans, collapse = ", "), ")"))
	expect_equal(datasetOfMeans3$overallStDevs, c(1, 1, 1.3815, 1.425418, 1.639151, 1.822857, 1.578664, 1.738706), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans3$overallStDevs, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetOfMeans3), NA)))
	    expect_output(print(datasetOfMeans3)$show())
	    invisible(capture.output(expect_error(summary(datasetOfMeans3), NA)))
	    expect_output(summary(datasetOfMeans3)$show())
	    datasetOfMeans3CodeBased <- eval(parse(text = getObjectRCode(datasetOfMeans3, stringWrapParagraphWidth = NULL)))
	    expect_equal(datasetOfMeans3CodeBased$stages, datasetOfMeans3$stages, tolerance = 1e-07)
	    expect_equal(datasetOfMeans3CodeBased$groups, datasetOfMeans3$groups, tolerance = 1e-07)
	    expect_equal(datasetOfMeans3CodeBased$subsets, datasetOfMeans3$subsets, tolerance = 1e-07)
	    expect_equal(datasetOfMeans3CodeBased$sampleSizes, datasetOfMeans3$sampleSizes, tolerance = 1e-07)
	    expect_equal(datasetOfMeans3CodeBased$means, datasetOfMeans3$means, tolerance = 1e-07)
	    expect_equal(datasetOfMeans3CodeBased$stDevs, datasetOfMeans3$stDevs, tolerance = 1e-07)
	    expect_equal(datasetOfMeans3CodeBased$overallSampleSizes, datasetOfMeans3$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(datasetOfMeans3CodeBased$overallMeans, datasetOfMeans3$overallMeans, tolerance = 1e-07)
	    expect_equal(datasetOfMeans3CodeBased$overallStDevs, datasetOfMeans3$overallStDevs, tolerance = 1e-07)
	    expect_type(names(datasetOfMeans3), "character")
	    df <- as.data.frame(datasetOfMeans3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(datasetOfMeans3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of data.frame object 'datasetOfMeans3$.data' with expected results
	expect_equal(datasetOfMeans3$.data$stage, factor(c(1, 1, 2, 2, 3, 3, 4, 4)), label = paste0("c(", paste0(datasetOfMeans3$.data$stage, collapse = ", "), ")"))
	expect_equal(datasetOfMeans3$.data$group, factor(c(1, 2, 1, 2, 1, 2, 1, 2)), label = paste0("c(", paste0(datasetOfMeans3$.data$group, collapse = ", "), ")"))
	expect_equal(datasetOfMeans3$.data$subset, factor(c(NA, NA, NA, NA, NA, NA, NA, NA)), label = paste0("c(", paste0(datasetOfMeans3$.data$subset, collapse = ", "), ")"))
	expect_equal(datasetOfMeans3$.data$sampleSize, c(22, 22, 11, 13, 22, 22, 11, 13), label = paste0("c(", paste0(datasetOfMeans3$.data$sampleSize, collapse = ", "), ")"))
	expect_equal(datasetOfMeans3$.data$mean, c(1, 1.4, 1.099999, 1.5000004, 1.0000005, 3.0000001, 1.000002, 2.4999979), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans3$.data$mean, collapse = ", "), ")"))
	expect_equal(datasetOfMeans3$.data$stDev, c(1, 1, 2.0000005, 2.0000009, 2.0000005, 1.9999999, 1.2999989, 1.3000023), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans3$.data$stDev, collapse = ", "), ")"))
	expect_equal(datasetOfMeans3$.data$overallSampleSize, c(22, 22, 33, 35, 55, 57, 66, 70), label = paste0("c(", paste0(datasetOfMeans3$.data$overallSampleSize, collapse = ", "), ")"))
	expect_equal(datasetOfMeans3$.data$overallMean, c(1, 1.4, 1.033333, 1.437143, 1.02, 2.040351, 1.016667, 2.125714), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans3$.data$overallMean, collapse = ", "), ")"))
	expect_equal(datasetOfMeans3$.data$overallStDev, c(1, 1, 1.3815, 1.425418, 1.639151, 1.822857, 1.578664, 1.738706), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans3$.data$overallStDev, collapse = ", "), ")"))

	x <- getMultipleStageResultsForDataset(datasetOfMeans3)

	## Comparison of the results of StageResultsMeans object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallMeans1, c(1, 1.033333, 1.02, 1.016667, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallMeans1, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallMeans2, c(1.4, 1.437143, 2.040351, 2.125714, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallMeans2, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallStDevs1, c(1, 1.3815, 1.639151, 1.578664, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallStDevs1, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallStDevs2, c(1, 1.425418, 1.822857, 1.738706, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallStDevs2, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallSampleSizes1, c(22, 33, 55, 66, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallSampleSizes1, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallSampleSizes2, c(22, 35, 57, 70, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallSampleSizes2, collapse = ", "), ")"))
	expect_equal(x$stageResults1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$effectSizes, c(-0.4, -0.40381, -1.020351, -1.109047, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$effectSizes, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	    x$stageResults1CodeBased <- eval(parse(text = getObjectRCode(x$stageResults1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults1CodeBased$overallTestStatistics, x$stageResults1$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallPValues, x$stageResults1$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallMeans1, x$stageResults1$overallMeans1, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallMeans2, x$stageResults1$overallMeans2, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallStDevs1, x$stageResults1$overallStDevs1, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallStDevs2, x$stageResults1$overallStDevs2, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallSampleSizes1, x$stageResults1$overallSampleSizes1, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallSampleSizes2, x$stageResults1$overallSampleSizes2, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$testStatistics, x$stageResults1$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$pValues, x$stageResults1$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$effectSizes, x$stageResults1$effectSizes, tolerance = 1e-07)
	    expect_type(names(x$stageResults1), "character")
	    df <- as.data.frame(x$stageResults1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallMeans1, c(1, 1.033333, 1.02, 1.016667, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallMeans1, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallMeans2, c(1.4, 1.437143, 2.040351, 2.125714, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallMeans2, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallStDevs1, c(1, 1.3815, 1.639151, 1.578664, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallStDevs1, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallStDevs2, c(1, 1.425418, 1.822857, 1.738706, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallStDevs2, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallSampleSizes1, c(22, 33, 55, 66, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallSampleSizes1, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallSampleSizes2, c(22, 35, 57, 70, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallSampleSizes2, collapse = ", "), ")"))
	expect_equal(x$stageResults2$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$effectSizes, c(-0.4, -0.40381, -1.020351, -1.109047, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$effectSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$combInverseNormal, collapse = ", "), ")"))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$weightsInverseNormal, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	    x$stageResults2CodeBased <- eval(parse(text = getObjectRCode(x$stageResults2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults2CodeBased$overallTestStatistics, x$stageResults2$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallPValues, x$stageResults2$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallMeans1, x$stageResults2$overallMeans1, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallMeans2, x$stageResults2$overallMeans2, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallStDevs1, x$stageResults2$overallStDevs1, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallStDevs2, x$stageResults2$overallStDevs2, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallSampleSizes1, x$stageResults2$overallSampleSizes1, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallSampleSizes2, x$stageResults2$overallSampleSizes2, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$testStatistics, x$stageResults2$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$pValues, x$stageResults2$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$effectSizes, x$stageResults2$effectSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$combInverseNormal, x$stageResults2$combInverseNormal, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$weightsInverseNormal, x$stageResults2$weightsInverseNormal, tolerance = 1e-07)
	    expect_type(names(x$stageResults2), "character")
	    df <- as.data.frame(x$stageResults2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallMeans1, c(1, 1.033333, 1.02, 1.016667, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallMeans1, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallMeans2, c(1.4, 1.437143, 2.040351, 2.125714, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallMeans2, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallStDevs1, c(1, 1.3815, 1.639151, 1.578664, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallStDevs1, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallStDevs2, c(1, 1.425418, 1.822857, 1.738706, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallStDevs2, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallSampleSizes1, c(22, 33, 55, 66, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallSampleSizes1, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallSampleSizes2, c(22, 35, 57, 70, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallSampleSizes2, collapse = ", "), ")"))
	expect_equal(x$stageResults3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$effectSizes, c(-0.4, -0.40381, -1.020351, -1.109047, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$effectSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$combFisher, collapse = ", "), ")"))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$weightsFisher, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	    x$stageResults3CodeBased <- eval(parse(text = getObjectRCode(x$stageResults3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults3CodeBased$overallTestStatistics, x$stageResults3$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallPValues, x$stageResults3$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallMeans1, x$stageResults3$overallMeans1, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallMeans2, x$stageResults3$overallMeans2, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallStDevs1, x$stageResults3$overallStDevs1, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallStDevs2, x$stageResults3$overallStDevs2, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallSampleSizes1, x$stageResults3$overallSampleSizes1, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallSampleSizes2, x$stageResults3$overallSampleSizes2, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$testStatistics, x$stageResults3$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$pValues, x$stageResults3$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$effectSizes, x$stageResults3$effectSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$combFisher, x$stageResults3$combFisher, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$weightsFisher, x$stageResults3$weightsFisher, tolerance = 1e-07)
	    expect_type(names(x$stageResults3), "character")
	    df <- as.data.frame(x$stageResults3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("Creation of a dataset of means using stage wise data (one group)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetMeans}
	datasetOfMeans4 <- getDataset(
	    n = c(22, 11, 22, 11),
	    means = c(1, 1.1, 1, 1),
	    stDevs = c(1, 2, 2, 1.3)
	)

	## Comparison of the results of DatasetMeans object 'datasetOfMeans4' with expected results
	expect_equal(datasetOfMeans4$stages, c(1, 2, 3, 4), label = paste0("c(", paste0(datasetOfMeans4$stages, collapse = ", "), ")"))
	expect_equal(datasetOfMeans4$groups, c(1, 1, 1, 1), label = paste0("c(", paste0(datasetOfMeans4$groups, collapse = ", "), ")"))
	expect_equal(datasetOfMeans4$subsets, c(NA_character_, NA_character_, NA_character_, NA_character_), label = paste0("c(", paste0(datasetOfMeans4$subsets, collapse = ", "), ")"))
	expect_equal(datasetOfMeans4$sampleSizes, c(22, 11, 22, 11), label = paste0("c(", paste0(datasetOfMeans4$sampleSizes, collapse = ", "), ")"))
	expect_equal(datasetOfMeans4$means, c(1, 1.1, 1, 1), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans4$means, collapse = ", "), ")"))
	expect_equal(datasetOfMeans4$stDevs, c(1, 2, 2, 1.3), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans4$stDevs, collapse = ", "), ")"))
	expect_equal(datasetOfMeans4$overallSampleSizes, c(22, 33, 55, 66), label = paste0("c(", paste0(datasetOfMeans4$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(datasetOfMeans4$overallMeans, c(1, 1.0333333, 1.02, 1.0166667), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans4$overallMeans, collapse = ", "), ")"))
	expect_equal(datasetOfMeans4$overallStDevs, c(1, 1.3814998, 1.6391506, 1.5786638), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans4$overallStDevs, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetOfMeans4), NA)))
	    expect_output(print(datasetOfMeans4)$show())
	    invisible(capture.output(expect_error(summary(datasetOfMeans4), NA)))
	    expect_output(summary(datasetOfMeans4)$show())
	    datasetOfMeans4CodeBased <- eval(parse(text = getObjectRCode(datasetOfMeans4, stringWrapParagraphWidth = NULL)))
	    expect_equal(datasetOfMeans4CodeBased$stages, datasetOfMeans4$stages, tolerance = 1e-07)
	    expect_equal(datasetOfMeans4CodeBased$groups, datasetOfMeans4$groups, tolerance = 1e-07)
	    expect_equal(datasetOfMeans4CodeBased$subsets, datasetOfMeans4$subsets, tolerance = 1e-07)
	    expect_equal(datasetOfMeans4CodeBased$sampleSizes, datasetOfMeans4$sampleSizes, tolerance = 1e-07)
	    expect_equal(datasetOfMeans4CodeBased$means, datasetOfMeans4$means, tolerance = 1e-07)
	    expect_equal(datasetOfMeans4CodeBased$stDevs, datasetOfMeans4$stDevs, tolerance = 1e-07)
	    expect_equal(datasetOfMeans4CodeBased$overallSampleSizes, datasetOfMeans4$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(datasetOfMeans4CodeBased$overallMeans, datasetOfMeans4$overallMeans, tolerance = 1e-07)
	    expect_equal(datasetOfMeans4CodeBased$overallStDevs, datasetOfMeans4$overallStDevs, tolerance = 1e-07)
	    expect_type(names(datasetOfMeans4), "character")
	    df <- as.data.frame(datasetOfMeans4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(datasetOfMeans4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of data.frame object 'datasetOfMeans4$.data' with expected results
	expect_equal(datasetOfMeans4$.data$stage, factor(c(1, 2, 3, 4)), label = paste0("c(", paste0(datasetOfMeans4$.data$stage, collapse = ", "), ")"))
	expect_equal(datasetOfMeans4$.data$group, factor(c(1, 1, 1, 1)), label = paste0("c(", paste0(datasetOfMeans4$.data$group, collapse = ", "), ")"))
	expect_equal(datasetOfMeans4$.data$subset, factor(c(NA, NA, NA, NA)), label = paste0("c(", paste0(datasetOfMeans4$.data$subset, collapse = ", "), ")"))
	expect_equal(datasetOfMeans4$.data$sampleSize, c(22, 11, 22, 11), label = paste0("c(", paste0(datasetOfMeans4$.data$sampleSize, collapse = ", "), ")"))
	expect_equal(datasetOfMeans4$.data$mean, c(1, 1.1, 1, 1), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans4$.data$mean, collapse = ", "), ")"))
	expect_equal(datasetOfMeans4$.data$stDev, c(1, 2, 2, 1.3), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans4$.data$stDev, collapse = ", "), ")"))
	expect_equal(datasetOfMeans4$.data$overallSampleSize, c(22, 33, 55, 66), label = paste0("c(", paste0(datasetOfMeans4$.data$overallSampleSize, collapse = ", "), ")"))
	expect_equal(datasetOfMeans4$.data$overallMean, c(1, 1.0333333, 1.02, 1.0166667), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans4$.data$overallMean, collapse = ", "), ")"))
	expect_equal(datasetOfMeans4$.data$overallStDev, c(1, 1.3814998, 1.6391506, 1.5786638), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans4$.data$overallStDev, collapse = ", "), ")"))

	x <- getMultipleStageResultsForDataset(datasetOfMeans4)

	## Comparison of the results of StageResultsMeans object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallMeans, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallMeans, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallStDevs, c(1, 1.3814998, 1.6391506, 1.5786638, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallStDevs, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallSampleSizes, c(22, 33, 55, 66, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$effectSizes, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$effectSizes, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	    x$stageResults1CodeBased <- eval(parse(text = getObjectRCode(x$stageResults1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults1CodeBased$overallTestStatistics, x$stageResults1$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallPValues, x$stageResults1$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallMeans, x$stageResults1$overallMeans, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallStDevs, x$stageResults1$overallStDevs, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallSampleSizes, x$stageResults1$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$testStatistics, x$stageResults1$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$pValues, x$stageResults1$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$effectSizes, x$stageResults1$effectSizes, tolerance = 1e-07)
	    expect_type(names(x$stageResults1), "character")
	    df <- as.data.frame(x$stageResults1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallMeans, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallMeans, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallStDevs, c(1, 1.3814998, 1.6391506, 1.5786638, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallStDevs, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallSampleSizes, c(22, 33, 55, 66, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults2$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$effectSizes, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$effectSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$combInverseNormal, collapse = ", "), ")"))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$weightsInverseNormal, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	    x$stageResults2CodeBased <- eval(parse(text = getObjectRCode(x$stageResults2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults2CodeBased$overallTestStatistics, x$stageResults2$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallPValues, x$stageResults2$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallMeans, x$stageResults2$overallMeans, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallStDevs, x$stageResults2$overallStDevs, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallSampleSizes, x$stageResults2$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$testStatistics, x$stageResults2$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$pValues, x$stageResults2$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$effectSizes, x$stageResults2$effectSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$combInverseNormal, x$stageResults2$combInverseNormal, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$weightsInverseNormal, x$stageResults2$weightsInverseNormal, tolerance = 1e-07)
	    expect_type(names(x$stageResults2), "character")
	    df <- as.data.frame(x$stageResults2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallMeans, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallMeans, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallStDevs, c(1, 1.3814998, 1.6391506, 1.5786638, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallStDevs, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallSampleSizes, c(22, 33, 55, 66, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$effectSizes, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$effectSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$combFisher, collapse = ", "), ")"))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$weightsFisher, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	    x$stageResults3CodeBased <- eval(parse(text = getObjectRCode(x$stageResults3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults3CodeBased$overallTestStatistics, x$stageResults3$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallPValues, x$stageResults3$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallMeans, x$stageResults3$overallMeans, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallStDevs, x$stageResults3$overallStDevs, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallSampleSizes, x$stageResults3$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$testStatistics, x$stageResults3$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$pValues, x$stageResults3$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$effectSizes, x$stageResults3$effectSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$combFisher, x$stageResults3$combFisher, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$weightsFisher, x$stageResults3$weightsFisher, tolerance = 1e-07)
	    expect_type(names(x$stageResults3), "character")
	    df <- as.data.frame(x$stageResults3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("Creation of a dataset of means using overall data (one group)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetMeans}
	datasetOfMeans5 <- getDataset(
	    overallSampleSizes = c(22, 33, 55, 66),
	    overallMeans = c(1.000, 1.033, 1.020, 1.017),
	    overallStDevs = c(1.00, 1.38, 1.64, 1.58)
	)

	## Comparison of the results of DatasetMeans object 'datasetOfMeans5' with expected results
	expect_equal(datasetOfMeans5$stages, c(1, 2, 3, 4), label = paste0("c(", paste0(datasetOfMeans5$stages, collapse = ", "), ")"))
	expect_equal(datasetOfMeans5$groups, c(1, 1, 1, 1), label = paste0("c(", paste0(datasetOfMeans5$groups, collapse = ", "), ")"))
	expect_equal(datasetOfMeans5$subsets, c(NA_character_, NA_character_, NA_character_, NA_character_), label = paste0("c(", paste0(datasetOfMeans5$subsets, collapse = ", "), ")"))
	expect_equal(datasetOfMeans5$sampleSizes, c(22, 11, 22, 11), label = paste0("c(", paste0(datasetOfMeans5$sampleSizes, collapse = ", "), ")"))
	expect_equal(datasetOfMeans5$means, c(1, 1.099, 1.0005, 1.002), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans5$means, collapse = ", "), ")"))
	expect_equal(datasetOfMeans5$stDevs, c(1, 1.9967205, 2.003374, 1.3047847), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans5$stDevs, collapse = ", "), ")"))
	expect_equal(datasetOfMeans5$overallSampleSizes, c(22, 33, 55, 66), label = paste0("c(", paste0(datasetOfMeans5$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(datasetOfMeans5$overallMeans, c(1, 1.033, 1.02, 1.017), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans5$overallMeans, collapse = ", "), ")"))
	expect_equal(datasetOfMeans5$overallStDevs, c(1, 1.38, 1.64, 1.58), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans5$overallStDevs, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetOfMeans5), NA)))
	    expect_output(print(datasetOfMeans5)$show())
	    invisible(capture.output(expect_error(summary(datasetOfMeans5), NA)))
	    expect_output(summary(datasetOfMeans5)$show())
	    datasetOfMeans5CodeBased <- eval(parse(text = getObjectRCode(datasetOfMeans5, stringWrapParagraphWidth = NULL)))
	    expect_equal(datasetOfMeans5CodeBased$stages, datasetOfMeans5$stages, tolerance = 1e-07)
	    expect_equal(datasetOfMeans5CodeBased$groups, datasetOfMeans5$groups, tolerance = 1e-07)
	    expect_equal(datasetOfMeans5CodeBased$subsets, datasetOfMeans5$subsets, tolerance = 1e-07)
	    expect_equal(datasetOfMeans5CodeBased$sampleSizes, datasetOfMeans5$sampleSizes, tolerance = 1e-07)
	    expect_equal(datasetOfMeans5CodeBased$means, datasetOfMeans5$means, tolerance = 1e-07)
	    expect_equal(datasetOfMeans5CodeBased$stDevs, datasetOfMeans5$stDevs, tolerance = 1e-07)
	    expect_equal(datasetOfMeans5CodeBased$overallSampleSizes, datasetOfMeans5$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(datasetOfMeans5CodeBased$overallMeans, datasetOfMeans5$overallMeans, tolerance = 1e-07)
	    expect_equal(datasetOfMeans5CodeBased$overallStDevs, datasetOfMeans5$overallStDevs, tolerance = 1e-07)
	    expect_type(names(datasetOfMeans5), "character")
	    df <- as.data.frame(datasetOfMeans5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(datasetOfMeans5)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of data.frame object 'datasetOfMeans5$.data' with expected results
	expect_equal(datasetOfMeans5$.data$stage, factor(c(1, 2, 3, 4)), label = paste0("c(", paste0(datasetOfMeans5$.data$stage, collapse = ", "), ")"))
	expect_equal(datasetOfMeans5$.data$group, factor(c(1, 1, 1, 1)), label = paste0("c(", paste0(datasetOfMeans5$.data$group, collapse = ", "), ")"))
	expect_equal(datasetOfMeans5$.data$subset, factor(c(NA, NA, NA, NA)), label = paste0("c(", paste0(datasetOfMeans5$.data$subset, collapse = ", "), ")"))
	expect_equal(datasetOfMeans5$.data$sampleSize, c(22, 11, 22, 11), label = paste0("c(", paste0(datasetOfMeans5$.data$sampleSize, collapse = ", "), ")"))
	expect_equal(datasetOfMeans5$.data$mean, c(1, 1.099, 1.0005, 1.002), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans5$.data$mean, collapse = ", "), ")"))
	expect_equal(datasetOfMeans5$.data$stDev, c(1, 1.9967205, 2.003374, 1.3047847), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans5$.data$stDev, collapse = ", "), ")"))
	expect_equal(datasetOfMeans5$.data$overallSampleSize, c(22, 33, 55, 66), label = paste0("c(", paste0(datasetOfMeans5$.data$overallSampleSize, collapse = ", "), ")"))
	expect_equal(datasetOfMeans5$.data$overallMean, c(1, 1.033, 1.02, 1.017), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans5$.data$overallMean, collapse = ", "), ")"))
	expect_equal(datasetOfMeans5$.data$overallStDev, c(1, 1.38, 1.64, 1.58), tolerance = 1e-07, label = paste0("c(", paste0(datasetOfMeans5$.data$overallStDev, collapse = ", "), ")"))

	x <- getMultipleStageResultsForDataset(datasetOfMeans5)

	## Comparison of the results of StageResultsMeans object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallMeans, c(1, 1.033, 1.02, 1.017, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallMeans, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallStDevs, c(1, 1.38, 1.64, 1.58, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallStDevs, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallSampleSizes, c(22, 33, 55, 66, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$effectSizes, c(1, 1.033, 1.02, 1.017, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$effectSizes, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	    x$stageResults1CodeBased <- eval(parse(text = getObjectRCode(x$stageResults1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults1CodeBased$overallTestStatistics, x$stageResults1$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallPValues, x$stageResults1$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallMeans, x$stageResults1$overallMeans, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallStDevs, x$stageResults1$overallStDevs, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallSampleSizes, x$stageResults1$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$testStatistics, x$stageResults1$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$pValues, x$stageResults1$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$effectSizes, x$stageResults1$effectSizes, tolerance = 1e-07)
	    expect_type(names(x$stageResults1), "character")
	    df <- as.data.frame(x$stageResults1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallMeans, c(1, 1.033, 1.02, 1.017, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallMeans, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallStDevs, c(1, 1.38, 1.64, 1.58, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallStDevs, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallSampleSizes, c(22, 33, 55, 66, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults2$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$effectSizes, c(1, 1.033, 1.02, 1.017, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$effectSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$combInverseNormal, collapse = ", "), ")"))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$weightsInverseNormal, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	    x$stageResults2CodeBased <- eval(parse(text = getObjectRCode(x$stageResults2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults2CodeBased$overallTestStatistics, x$stageResults2$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallPValues, x$stageResults2$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallMeans, x$stageResults2$overallMeans, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallStDevs, x$stageResults2$overallStDevs, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallSampleSizes, x$stageResults2$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$testStatistics, x$stageResults2$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$pValues, x$stageResults2$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$effectSizes, x$stageResults2$effectSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$combInverseNormal, x$stageResults2$combInverseNormal, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$weightsInverseNormal, x$stageResults2$weightsInverseNormal, tolerance = 1e-07)
	    expect_type(names(x$stageResults2), "character")
	    df <- as.data.frame(x$stageResults2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallMeans, c(1, 1.033, 1.02, 1.017, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallMeans, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallStDevs, c(1, 1.38, 1.64, 1.58, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallStDevs, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallSampleSizes, c(22, 33, 55, 66, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$effectSizes, c(1, 1.033, 1.02, 1.017, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$effectSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$combFisher, collapse = ", "), ")"))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$weightsFisher, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	    x$stageResults3CodeBased <- eval(parse(text = getObjectRCode(x$stageResults3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults3CodeBased$overallTestStatistics, x$stageResults3$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallPValues, x$stageResults3$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallMeans, x$stageResults3$overallMeans, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallStDevs, x$stageResults3$overallStDevs, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallSampleSizes, x$stageResults3$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$testStatistics, x$stageResults3$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$pValues, x$stageResults3$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$effectSizes, x$stageResults3$effectSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$combFisher, x$stageResults3$combFisher, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$weightsFisher, x$stageResults3$weightsFisher, tolerance = 1e-07)
	    expect_type(names(x$stageResults3), "character")
	    df <- as.data.frame(x$stageResults3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("Trim command works as expected for means", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetMeans}
	datasetOfMeansExpected <- getDataset(
	    n1 = c(13, 25),
	    n2 = c(15, NA),
	    n3 = c(14, 27),
	    n4 = c(12, 29),
	    means1 = c(24.2, 22.2),
	    means2 = c(18.8, NA),
	    means3 = c(26.7, 27.7),
	    means4 = c(9.2, 12.2),
	    stDevs1 = c(24.4, 22.1),
	    stDevs2 = c(21.2, NA),
	    stDevs3 = c(25.6, 23.2),
	    stDevs4 = c(21.5, 22.7)
	)
	datasetOfMeans <- getDataset(
	    n1 = c(13, 25),
	    n2 = c(15, NA),
	    n3 = c(14, 27),
	    n4 = c(12, 29),
	    means1 = c(24.2, 22.2),
	    means2 = c(18.8, NA),
	    means3 = c(26.7, 27.7),
	    means4 = c(9.2, 12.2),
	    stDevs1 = c(24.4, 22.1),
	    stDevs2 = c(21.2, NA),
	    stDevs3 = c(25.6, 23.2),
	    stDevs4 = c(21.5, 22.7)
	)
	datasetOfMeans$.fillWithNAs(4)
	datasetOfMeans$.trim(2)

	expect_equal(datasetOfMeans$stages, datasetOfMeansExpected$stages)
	expect_equal(datasetOfMeans$groups, datasetOfMeansExpected$groups)
	expect_equal(datasetOfMeans$overallMeans, datasetOfMeansExpected$overallMeans)
	expect_equal(datasetOfMeans$means, datasetOfMeansExpected$means)
	expect_equal(datasetOfMeans$overallStDevs, datasetOfMeansExpected$overallStDevs)
	expect_equal(datasetOfMeans$stDevs, datasetOfMeansExpected$stDevs)

	expect_equal(datasetOfMeans$.data$stage, datasetOfMeansExpected$.data$stage)
	expect_equal(datasetOfMeans$.data$group, datasetOfMeansExpected$.data$group)
	expect_equal(datasetOfMeans$.data$overallMeans, datasetOfMeansExpected$.data$overallMeans)
	expect_equal(datasetOfMeans$.data$means, datasetOfMeansExpected$.data$means)
	expect_equal(datasetOfMeans$.data$overallStDevs, datasetOfMeansExpected$.data$overallStDevs)
	expect_equal(datasetOfMeans$.data$stDevs, datasetOfMeansExpected$.data$stDevs)

})

test_that("Creation of a dataset of rates using stage wise data (one group)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetRates}
	datasetOfRates1 <- getDataset(
	    n = c(8, 10, 9, 11),
	    events = c(4, 5, 5, 6)
	)

	## Comparison of the results of DatasetRates object 'datasetOfRates1' with expected results
	expect_equal(datasetOfRates1$stages, c(1, 2, 3, 4), label = paste0("c(", paste0(datasetOfRates1$stages, collapse = ", "), ")"))
	expect_equal(datasetOfRates1$groups, c(1, 1, 1, 1), label = paste0("c(", paste0(datasetOfRates1$groups, collapse = ", "), ")"))
	expect_equal(datasetOfRates1$subsets, c(NA_character_, NA_character_, NA_character_, NA_character_), label = paste0("c(", paste0(datasetOfRates1$subsets, collapse = ", "), ")"))
	expect_equal(datasetOfRates1$sampleSizes, c(8, 10, 9, 11), label = paste0("c(", paste0(datasetOfRates1$sampleSizes, collapse = ", "), ")"))
	expect_equal(datasetOfRates1$events, c(4, 5, 5, 6), label = paste0("c(", paste0(datasetOfRates1$events, collapse = ", "), ")"))
	expect_equal(datasetOfRates1$overallSampleSizes, c(8, 18, 27, 38), label = paste0("c(", paste0(datasetOfRates1$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(datasetOfRates1$overallEvents, c(4, 9, 14, 20), label = paste0("c(", paste0(datasetOfRates1$overallEvents, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetOfRates1), NA)))
	    expect_output(print(datasetOfRates1)$show())
	    invisible(capture.output(expect_error(summary(datasetOfRates1), NA)))
	    expect_output(summary(datasetOfRates1)$show())
	    datasetOfRates1CodeBased <- eval(parse(text = getObjectRCode(datasetOfRates1, stringWrapParagraphWidth = NULL)))
	    expect_equal(datasetOfRates1CodeBased$stages, datasetOfRates1$stages, tolerance = 1e-07)
	    expect_equal(datasetOfRates1CodeBased$groups, datasetOfRates1$groups, tolerance = 1e-07)
	    expect_equal(datasetOfRates1CodeBased$subsets, datasetOfRates1$subsets, tolerance = 1e-07)
	    expect_equal(datasetOfRates1CodeBased$sampleSizes, datasetOfRates1$sampleSizes, tolerance = 1e-07)
	    expect_equal(datasetOfRates1CodeBased$events, datasetOfRates1$events, tolerance = 1e-07)
	    expect_equal(datasetOfRates1CodeBased$overallSampleSizes, datasetOfRates1$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(datasetOfRates1CodeBased$overallEvents, datasetOfRates1$overallEvents, tolerance = 1e-07)
	    expect_type(names(datasetOfRates1), "character")
	    df <- as.data.frame(datasetOfRates1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(datasetOfRates1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of data.frame object 'datasetOfRates1$.data' with expected results
	expect_equal(datasetOfRates1$.data$stage, factor(c(1, 2, 3, 4)), label = paste0("c(", paste0(datasetOfRates1$.data$stage, collapse = ", "), ")"))
	expect_equal(datasetOfRates1$.data$group, factor(c(1, 1, 1, 1)), label = paste0("c(", paste0(datasetOfRates1$.data$group, collapse = ", "), ")"))
	expect_equal(datasetOfRates1$.data$subset, factor(c(NA, NA, NA, NA)), label = paste0("c(", paste0(datasetOfRates1$.data$subset, collapse = ", "), ")"))
	expect_equal(datasetOfRates1$.data$sampleSize, c(8, 10, 9, 11), label = paste0("c(", paste0(datasetOfRates1$.data$sampleSize, collapse = ", "), ")"))
	expect_equal(datasetOfRates1$.data$event, c(4, 5, 5, 6), label = paste0("c(", paste0(datasetOfRates1$.data$event, collapse = ", "), ")"))
	expect_equal(datasetOfRates1$.data$overallSampleSize, c(8, 18, 27, 38), label = paste0("c(", paste0(datasetOfRates1$.data$overallSampleSize, collapse = ", "), ")"))
	expect_equal(datasetOfRates1$.data$overallEvent, c(4, 9, 14, 20), label = paste0("c(", paste0(datasetOfRates1$.data$overallEvent, collapse = ", "), ")"))

	x <- getMultipleStageResultsForDataset(datasetOfRates1, thetaH0 = 0.99)

	## Comparison of the results of StageResultsRates object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallTestStatistics, c(-13.929113, -20.89367, -24.622317, -28.727412, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallPValues, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallEvents, c(4, 9, 14, 20, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallEvents, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallSampleSizes, c(8, 18, 27, 38, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallPi1, c(0.5, 0.5, 0.51851852, 0.52631579, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallPi1, collapse = ", "), ")"))
	expect_equal(x$stageResults1$testStatistics, c(-13.929113, -15.573222, -13.098993, -14.818182, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$pValues, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults1$pValues, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	    x$stageResults1CodeBased <- eval(parse(text = getObjectRCode(x$stageResults1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults1CodeBased$overallTestStatistics, x$stageResults1$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallPValues, x$stageResults1$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallEvents, x$stageResults1$overallEvents, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallSampleSizes, x$stageResults1$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallPi1, x$stageResults1$overallPi1, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$testStatistics, x$stageResults1$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$pValues, x$stageResults1$pValues, tolerance = 1e-07)
	    expect_type(names(x$stageResults1), "character")
	    df <- as.data.frame(x$stageResults1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsRates object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallTestStatistics, c(-13.929113, -20.89367, -24.622317, -28.727412, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallPValues, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallEvents, c(4, 9, 14, 20, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallEvents, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallSampleSizes, c(8, 18, 27, 38, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallPi1, c(0.5, 0.5, 0.51851852, 0.52631579, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallPi1, collapse = ", "), ")"))
	expect_equal(x$stageResults2$testStatistics, c(-13.929113, -15.573222, -13.098993, -14.818182, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$pValues, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults2$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$combInverseNormal, c(-21.273454, -30.085207, -36.846702, -42.546907, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$combInverseNormal, collapse = ", "), ")"))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$weightsInverseNormal, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	    x$stageResults2CodeBased <- eval(parse(text = getObjectRCode(x$stageResults2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults2CodeBased$overallTestStatistics, x$stageResults2$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallPValues, x$stageResults2$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallEvents, x$stageResults2$overallEvents, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallSampleSizes, x$stageResults2$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallPi1, x$stageResults2$overallPi1, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$testStatistics, x$stageResults2$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$pValues, x$stageResults2$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$combInverseNormal, x$stageResults2$combInverseNormal, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$weightsInverseNormal, x$stageResults2$weightsInverseNormal, tolerance = 1e-07)
	    expect_type(names(x$stageResults2), "character")
	    df <- as.data.frame(x$stageResults2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsRates object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallTestStatistics, c(-13.929113, -20.89367, -24.622317, -28.727412, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallPValues, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallEvents, c(4, 9, 14, 20, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallEvents, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallSampleSizes, c(8, 18, 27, 38, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallPi1, c(0.5, 0.5, 0.51851852, 0.52631579, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallPi1, collapse = ", "), ")"))
	expect_equal(x$stageResults3$testStatistics, c(-13.929113, -15.573222, -13.098993, -14.818182, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$pValues, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults3$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$combFisher, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults3$combFisher, collapse = ", "), ")"))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$weightsFisher, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	    x$stageResults3CodeBased <- eval(parse(text = getObjectRCode(x$stageResults3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults3CodeBased$overallTestStatistics, x$stageResults3$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallPValues, x$stageResults3$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallEvents, x$stageResults3$overallEvents, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallSampleSizes, x$stageResults3$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallPi1, x$stageResults3$overallPi1, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$testStatistics, x$stageResults3$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$pValues, x$stageResults3$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$combFisher, x$stageResults3$combFisher, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$weightsFisher, x$stageResults3$weightsFisher, tolerance = 1e-07)
	    expect_type(names(x$stageResults3), "character")
	    df <- as.data.frame(x$stageResults3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("Creation of a dataset of rates using stage wise data (two groups)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetRates}
	datasetOfRates2 <- getDataset(
	    n2 = c(8, 10, 9, 11),
	    n1 = c(11, 13, 12, 13),
	    events2 = c(3, 5, 5, 6),
	    events1 = c(10, 10, 12, 12)
	)

	## Comparison of the results of DatasetRates object 'datasetOfRates2' with expected results
	expect_equal(datasetOfRates2$stages, c(1, 1, 2, 2, 3, 3, 4, 4), label = paste0("c(", paste0(datasetOfRates2$stages, collapse = ", "), ")"))
	expect_equal(datasetOfRates2$groups, c(1, 2, 1, 2, 1, 2, 1, 2), label = paste0("c(", paste0(datasetOfRates2$groups, collapse = ", "), ")"))
	expect_equal(datasetOfRates2$subsets, c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_), label = paste0("c(", paste0(datasetOfRates2$subsets, collapse = ", "), ")"))
	expect_equal(datasetOfRates2$sampleSizes, c(11, 8, 13, 10, 12, 9, 13, 11), label = paste0("c(", paste0(datasetOfRates2$sampleSizes, collapse = ", "), ")"))
	expect_equal(datasetOfRates2$events, c(10, 3, 10, 5, 12, 5, 12, 6), label = paste0("c(", paste0(datasetOfRates2$events, collapse = ", "), ")"))
	expect_equal(datasetOfRates2$overallSampleSizes, c(11, 8, 24, 18, 36, 27, 49, 38), label = paste0("c(", paste0(datasetOfRates2$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(datasetOfRates2$overallEvents, c(10, 3, 20, 8, 32, 13, 44, 19), label = paste0("c(", paste0(datasetOfRates2$overallEvents, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetOfRates2), NA)))
	    expect_output(print(datasetOfRates2)$show())
	    invisible(capture.output(expect_error(summary(datasetOfRates2), NA)))
	    expect_output(summary(datasetOfRates2)$show())
	    datasetOfRates2CodeBased <- eval(parse(text = getObjectRCode(datasetOfRates2, stringWrapParagraphWidth = NULL)))
	    expect_equal(datasetOfRates2CodeBased$stages, datasetOfRates2$stages, tolerance = 1e-07)
	    expect_equal(datasetOfRates2CodeBased$groups, datasetOfRates2$groups, tolerance = 1e-07)
	    expect_equal(datasetOfRates2CodeBased$subsets, datasetOfRates2$subsets, tolerance = 1e-07)
	    expect_equal(datasetOfRates2CodeBased$sampleSizes, datasetOfRates2$sampleSizes, tolerance = 1e-07)
	    expect_equal(datasetOfRates2CodeBased$events, datasetOfRates2$events, tolerance = 1e-07)
	    expect_equal(datasetOfRates2CodeBased$overallSampleSizes, datasetOfRates2$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(datasetOfRates2CodeBased$overallEvents, datasetOfRates2$overallEvents, tolerance = 1e-07)
	    expect_type(names(datasetOfRates2), "character")
	    df <- as.data.frame(datasetOfRates2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(datasetOfRates2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of data.frame object 'datasetOfRates2$.data' with expected results
	expect_equal(datasetOfRates2$.data$stage, factor(c(1, 1, 2, 2, 3, 3, 4, 4)), label = paste0("c(", paste0(datasetOfRates2$.data$stage, collapse = ", "), ")"))
	expect_equal(datasetOfRates2$.data$group, factor(c(1, 2, 1, 2, 1, 2, 1, 2)), label = paste0("c(", paste0(datasetOfRates2$.data$group, collapse = ", "), ")"))
	expect_equal(datasetOfRates2$.data$subset, factor(c(NA, NA, NA, NA, NA, NA, NA, NA)), label = paste0("c(", paste0(datasetOfRates2$.data$subset, collapse = ", "), ")"))
	expect_equal(datasetOfRates2$.data$sampleSize, c(11, 8, 13, 10, 12, 9, 13, 11), label = paste0("c(", paste0(datasetOfRates2$.data$sampleSize, collapse = ", "), ")"))
	expect_equal(datasetOfRates2$.data$event, c(10, 3, 10, 5, 12, 5, 12, 6), label = paste0("c(", paste0(datasetOfRates2$.data$event, collapse = ", "), ")"))
	expect_equal(datasetOfRates2$.data$overallSampleSize, c(11, 8, 24, 18, 36, 27, 49, 38), label = paste0("c(", paste0(datasetOfRates2$.data$overallSampleSize, collapse = ", "), ")"))
	expect_equal(datasetOfRates2$.data$overallEvent, c(10, 3, 20, 8, 32, 13, 44, 19), label = paste0("c(", paste0(datasetOfRates2$.data$overallEvent, collapse = ", "), ")"))

	x <- getMultipleStageResultsForDataset(datasetOfRates2, thetaH0 = 0.99)

	## Comparison of the results of StageResultsRates object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallTestStatistics, c(-13.397899, -26.707477, -31.300879, -37.503444, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallPValues, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallEvents1, c(10, 20, 32, 44, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallEvents1, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallEvents2, c(3, 8, 13, 19, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallEvents2, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallSampleSizes1, c(11, 24, 36, 49, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallSampleSizes1, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallSampleSizes2, c(8, 18, 27, 38, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallSampleSizes2, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallPi1, c(0.90909091, 0.83333333, 0.88888889, 0.89795918, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallPi1, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallPi2, c(0.375, 0.44444444, 0.48148148, 0.5, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallPi2, collapse = ", "), ")"))
	expect_equal(x$stageResults1$testStatistics, c(-13.397899, -23.909016, -16.449119, -20.614826, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$pValues, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults1$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$effectSizes, c(0.53409091, 0.38888889, 0.40740741, 0.39795918, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$effectSizes, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	    x$stageResults1CodeBased <- eval(parse(text = getObjectRCode(x$stageResults1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults1CodeBased$overallTestStatistics, x$stageResults1$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallPValues, x$stageResults1$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallEvents1, x$stageResults1$overallEvents1, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallEvents2, x$stageResults1$overallEvents2, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallSampleSizes1, x$stageResults1$overallSampleSizes1, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallSampleSizes2, x$stageResults1$overallSampleSizes2, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallPi1, x$stageResults1$overallPi1, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallPi2, x$stageResults1$overallPi2, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$testStatistics, x$stageResults1$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$pValues, x$stageResults1$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$effectSizes, x$stageResults1$effectSizes, tolerance = 1e-07)
	    expect_type(names(x$stageResults1), "character")
	    df <- as.data.frame(x$stageResults1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsRates object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallTestStatistics, c(-13.397899, -26.707477, -31.300879, -37.503444, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallPValues, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallEvents1, c(10, 20, 32, 44, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallEvents1, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallEvents2, c(3, 8, 13, 19, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallEvents2, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallSampleSizes1, c(11, 24, 36, 49, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallSampleSizes1, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallSampleSizes2, c(8, 18, 27, 38, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallSampleSizes2, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallPi1, c(0.90909091, 0.83333333, 0.88888889, 0.89795918, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallPi1, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallPi2, c(0.375, 0.44444444, 0.48148148, 0.5, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallPi2, collapse = ", "), ")"))
	expect_equal(x$stageResults2$testStatistics, c(-13.397899, -23.909016, -16.449119, -20.614826, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$pValues, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults2$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$effectSizes, c(0.53409091, 0.38888889, 0.40740741, 0.39795918, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$effectSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults2$combInverseNormal, c(-21.273454, -30.085207, -36.846702, -42.546907, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$combInverseNormal, collapse = ", "), ")"))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$weightsInverseNormal, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	    x$stageResults2CodeBased <- eval(parse(text = getObjectRCode(x$stageResults2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults2CodeBased$overallTestStatistics, x$stageResults2$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallPValues, x$stageResults2$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallEvents1, x$stageResults2$overallEvents1, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallEvents2, x$stageResults2$overallEvents2, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallSampleSizes1, x$stageResults2$overallSampleSizes1, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallSampleSizes2, x$stageResults2$overallSampleSizes2, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallPi1, x$stageResults2$overallPi1, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallPi2, x$stageResults2$overallPi2, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$testStatistics, x$stageResults2$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$pValues, x$stageResults2$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$effectSizes, x$stageResults2$effectSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$combInverseNormal, x$stageResults2$combInverseNormal, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$weightsInverseNormal, x$stageResults2$weightsInverseNormal, tolerance = 1e-07)
	    expect_type(names(x$stageResults2), "character")
	    df <- as.data.frame(x$stageResults2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsRates object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallTestStatistics, c(-13.397899, -26.707477, -31.300879, -37.503444, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallPValues, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallEvents1, c(10, 20, 32, 44, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallEvents1, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallEvents2, c(3, 8, 13, 19, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallEvents2, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallSampleSizes1, c(11, 24, 36, 49, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallSampleSizes1, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallSampleSizes2, c(8, 18, 27, 38, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallSampleSizes2, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallPi1, c(0.90909091, 0.83333333, 0.88888889, 0.89795918, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallPi1, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallPi2, c(0.375, 0.44444444, 0.48148148, 0.5, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallPi2, collapse = ", "), ")"))
	expect_equal(x$stageResults3$testStatistics, c(-13.397899, -23.909016, -16.449119, -20.614826, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$pValues, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults3$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$effectSizes, c(0.53409091, 0.38888889, 0.40740741, 0.39795918, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$effectSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults3$combFisher, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults3$combFisher, collapse = ", "), ")"))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$weightsFisher, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	    x$stageResults3CodeBased <- eval(parse(text = getObjectRCode(x$stageResults3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults3CodeBased$overallTestStatistics, x$stageResults3$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallPValues, x$stageResults3$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallEvents1, x$stageResults3$overallEvents1, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallEvents2, x$stageResults3$overallEvents2, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallSampleSizes1, x$stageResults3$overallSampleSizes1, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallSampleSizes2, x$stageResults3$overallSampleSizes2, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallPi1, x$stageResults3$overallPi1, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallPi2, x$stageResults3$overallPi2, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$testStatistics, x$stageResults3$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$pValues, x$stageResults3$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$effectSizes, x$stageResults3$effectSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$combFisher, x$stageResults3$combFisher, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$weightsFisher, x$stageResults3$weightsFisher, tolerance = 1e-07)
	    expect_type(names(x$stageResults3), "character")
	    df <- as.data.frame(x$stageResults3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("Creation of a dataset of rates using stage wise data (four groups)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetRates}
	datasetOfRates3 <- getDataset(
	    n1 = c(11, 13, 12, 13),
	    n2 = c(8, 10, 9, 11),
	    n3 = c(7, 10, 8, 9),
	    n4 = c(9, 11, 5, 2),
	    events1 = c(10, 10, 12, 12),
	    events2 = c(3, 5, 5, 6),
	    events3 = c(2, 4, 3, 5),
	    events4 = c(3, 4, 3, 0)
	)

	## Comparison of the results of DatasetRates object 'datasetOfRates3' with expected results
	expect_equal(datasetOfRates3$stages, c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4), label = paste0("c(", paste0(datasetOfRates3$stages, collapse = ", "), ")"))
	expect_equal(datasetOfRates3$groups, c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4), label = paste0("c(", paste0(datasetOfRates3$groups, collapse = ", "), ")"))
	expect_equal(datasetOfRates3$subsets, c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_), label = paste0("c(", paste0(datasetOfRates3$subsets, collapse = ", "), ")"))
	expect_equal(datasetOfRates3$sampleSizes, c(11, 8, 7, 9, 13, 10, 10, 11, 12, 9, 8, 5, 13, 11, 9, 2), label = paste0("c(", paste0(datasetOfRates3$sampleSizes, collapse = ", "), ")"))
	expect_equal(datasetOfRates3$events, c(10, 3, 2, 3, 10, 5, 4, 4, 12, 5, 3, 3, 12, 6, 5, 0), label = paste0("c(", paste0(datasetOfRates3$events, collapse = ", "), ")"))
	expect_equal(datasetOfRates3$overallSampleSizes, c(11, 8, 7, 9, 24, 18, 17, 20, 36, 27, 25, 25, 49, 38, 34, 27), label = paste0("c(", paste0(datasetOfRates3$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(datasetOfRates3$overallEvents, c(10, 3, 2, 3, 20, 8, 6, 7, 32, 13, 9, 10, 44, 19, 14, 10), label = paste0("c(", paste0(datasetOfRates3$overallEvents, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetOfRates3), NA)))
	    expect_output(print(datasetOfRates3)$show())
	    invisible(capture.output(expect_error(summary(datasetOfRates3), NA)))
	    expect_output(summary(datasetOfRates3)$show())
	    datasetOfRates3CodeBased <- eval(parse(text = getObjectRCode(datasetOfRates3, stringWrapParagraphWidth = NULL)))
	    expect_equal(datasetOfRates3CodeBased$stages, datasetOfRates3$stages, tolerance = 1e-07)
	    expect_equal(datasetOfRates3CodeBased$groups, datasetOfRates3$groups, tolerance = 1e-07)
	    expect_equal(datasetOfRates3CodeBased$subsets, datasetOfRates3$subsets, tolerance = 1e-07)
	    expect_equal(datasetOfRates3CodeBased$sampleSizes, datasetOfRates3$sampleSizes, tolerance = 1e-07)
	    expect_equal(datasetOfRates3CodeBased$events, datasetOfRates3$events, tolerance = 1e-07)
	    expect_equal(datasetOfRates3CodeBased$overallSampleSizes, datasetOfRates3$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(datasetOfRates3CodeBased$overallEvents, datasetOfRates3$overallEvents, tolerance = 1e-07)
	    expect_type(names(datasetOfRates3), "character")
	    df <- as.data.frame(datasetOfRates3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(datasetOfRates3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of data.frame object 'datasetOfRates3$.data' with expected results
	expect_equal(datasetOfRates3$.data$stage, factor(c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4)), label = paste0("c(", paste0(datasetOfRates3$.data$stage, collapse = ", "), ")"))
	expect_equal(datasetOfRates3$.data$group, factor(c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4)), label = paste0("c(", paste0(datasetOfRates3$.data$group, collapse = ", "), ")"))
	expect_equal(datasetOfRates3$.data$subset, factor(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)), label = paste0("c(", paste0(datasetOfRates3$.data$subset, collapse = ", "), ")"))
	expect_equal(datasetOfRates3$.data$sampleSize, c(11, 8, 7, 9, 13, 10, 10, 11, 12, 9, 8, 5, 13, 11, 9, 2), label = paste0("c(", paste0(datasetOfRates3$.data$sampleSize, collapse = ", "), ")"))
	expect_equal(datasetOfRates3$.data$event, c(10, 3, 2, 3, 10, 5, 4, 4, 12, 5, 3, 3, 12, 6, 5, 0), label = paste0("c(", paste0(datasetOfRates3$.data$event, collapse = ", "), ")"))
	expect_equal(datasetOfRates3$.data$overallSampleSize, c(11, 8, 7, 9, 24, 18, 17, 20, 36, 27, 25, 25, 49, 38, 34, 27), label = paste0("c(", paste0(datasetOfRates3$.data$overallSampleSize, collapse = ", "), ")"))
	expect_equal(datasetOfRates3$.data$overallEvent, c(10, 3, 2, 3, 20, 8, 6, 7, 32, 13, 9, 10, 44, 19, 14, 10), label = paste0("c(", paste0(datasetOfRates3$.data$overallEvent, collapse = ", "), ")"))

})

test_that("Creation of a dataset of rates using overall data (two groups)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetRates}
	datasetOfRates4 <- getDataset(
	    overallSampleSizes1 = c(11, 24, 36, 49),
	    overallSampleSizes2 = c(8, 18, 27, 38),
	    overallEvents1 = c(10, 20, 32, 44),
	    overallEvents2 = c(3, 8, 13, 19)
	)

	## Comparison of the results of DatasetRates object 'datasetOfRates4' with expected results
	expect_equal(datasetOfRates4$stages, c(1, 1, 2, 2, 3, 3, 4, 4), label = paste0("c(", paste0(datasetOfRates4$stages, collapse = ", "), ")"))
	expect_equal(datasetOfRates4$groups, c(1, 2, 1, 2, 1, 2, 1, 2), label = paste0("c(", paste0(datasetOfRates4$groups, collapse = ", "), ")"))
	expect_equal(datasetOfRates4$subsets, c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_), label = paste0("c(", paste0(datasetOfRates4$subsets, collapse = ", "), ")"))
	expect_equal(datasetOfRates4$sampleSizes, c(11, 8, 13, 10, 12, 9, 13, 11), label = paste0("c(", paste0(datasetOfRates4$sampleSizes, collapse = ", "), ")"))
	expect_equal(datasetOfRates4$events, c(10, 3, 10, 5, 12, 5, 12, 6), label = paste0("c(", paste0(datasetOfRates4$events, collapse = ", "), ")"))
	expect_equal(datasetOfRates4$overallSampleSizes, c(11, 8, 24, 18, 36, 27, 49, 38), label = paste0("c(", paste0(datasetOfRates4$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(datasetOfRates4$overallEvents, c(10, 3, 20, 8, 32, 13, 44, 19), label = paste0("c(", paste0(datasetOfRates4$overallEvents, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetOfRates4), NA)))
	    expect_output(print(datasetOfRates4)$show())
	    invisible(capture.output(expect_error(summary(datasetOfRates4), NA)))
	    expect_output(summary(datasetOfRates4)$show())
	    datasetOfRates4CodeBased <- eval(parse(text = getObjectRCode(datasetOfRates4, stringWrapParagraphWidth = NULL)))
	    expect_equal(datasetOfRates4CodeBased$stages, datasetOfRates4$stages, tolerance = 1e-07)
	    expect_equal(datasetOfRates4CodeBased$groups, datasetOfRates4$groups, tolerance = 1e-07)
	    expect_equal(datasetOfRates4CodeBased$subsets, datasetOfRates4$subsets, tolerance = 1e-07)
	    expect_equal(datasetOfRates4CodeBased$sampleSizes, datasetOfRates4$sampleSizes, tolerance = 1e-07)
	    expect_equal(datasetOfRates4CodeBased$events, datasetOfRates4$events, tolerance = 1e-07)
	    expect_equal(datasetOfRates4CodeBased$overallSampleSizes, datasetOfRates4$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(datasetOfRates4CodeBased$overallEvents, datasetOfRates4$overallEvents, tolerance = 1e-07)
	    expect_type(names(datasetOfRates4), "character")
	    df <- as.data.frame(datasetOfRates4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(datasetOfRates4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of data.frame object 'datasetOfRates4$.data' with expected results
	expect_equal(datasetOfRates4$.data$stage, factor(c(1, 1, 2, 2, 3, 3, 4, 4)), label = paste0("c(", paste0(datasetOfRates4$.data$stage, collapse = ", "), ")"))
	expect_equal(datasetOfRates4$.data$group, factor(c(1, 2, 1, 2, 1, 2, 1, 2)), label = paste0("c(", paste0(datasetOfRates4$.data$group, collapse = ", "), ")"))
	expect_equal(datasetOfRates4$.data$subset, factor(c(NA, NA, NA, NA, NA, NA, NA, NA)), label = paste0("c(", paste0(datasetOfRates4$.data$subset, collapse = ", "), ")"))
	expect_equal(datasetOfRates4$.data$sampleSize, c(11, 8, 13, 10, 12, 9, 13, 11), label = paste0("c(", paste0(datasetOfRates4$.data$sampleSize, collapse = ", "), ")"))
	expect_equal(datasetOfRates4$.data$event, c(10, 3, 10, 5, 12, 5, 12, 6), label = paste0("c(", paste0(datasetOfRates4$.data$event, collapse = ", "), ")"))
	expect_equal(datasetOfRates4$.data$overallSampleSize, c(11, 8, 24, 18, 36, 27, 49, 38), label = paste0("c(", paste0(datasetOfRates4$.data$overallSampleSize, collapse = ", "), ")"))
	expect_equal(datasetOfRates4$.data$overallEvent, c(10, 3, 20, 8, 32, 13, 44, 19), label = paste0("c(", paste0(datasetOfRates4$.data$overallEvent, collapse = ", "), ")"))

	x <- getMultipleStageResultsForDataset(datasetOfRates4, thetaH0 = 0.99)

	## Comparison of the results of StageResultsRates object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallTestStatistics, c(-13.397899, -26.707477, -31.300879, -37.503444, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallPValues, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallEvents1, c(10, 20, 32, 44, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallEvents1, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallEvents2, c(3, 8, 13, 19, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallEvents2, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallSampleSizes1, c(11, 24, 36, 49, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallSampleSizes1, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallSampleSizes2, c(8, 18, 27, 38, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallSampleSizes2, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallPi1, c(0.90909091, 0.83333333, 0.88888889, 0.89795918, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallPi1, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallPi2, c(0.375, 0.44444444, 0.48148148, 0.5, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallPi2, collapse = ", "), ")"))
	expect_equal(x$stageResults1$testStatistics, c(-13.397899, -23.909016, -16.449119, -20.614826, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$pValues, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults1$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$effectSizes, c(0.53409091, 0.38888889, 0.40740741, 0.39795918, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$effectSizes, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	    x$stageResults1CodeBased <- eval(parse(text = getObjectRCode(x$stageResults1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults1CodeBased$overallTestStatistics, x$stageResults1$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallPValues, x$stageResults1$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallEvents1, x$stageResults1$overallEvents1, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallEvents2, x$stageResults1$overallEvents2, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallSampleSizes1, x$stageResults1$overallSampleSizes1, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallSampleSizes2, x$stageResults1$overallSampleSizes2, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallPi1, x$stageResults1$overallPi1, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallPi2, x$stageResults1$overallPi2, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$testStatistics, x$stageResults1$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$pValues, x$stageResults1$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$effectSizes, x$stageResults1$effectSizes, tolerance = 1e-07)
	    expect_type(names(x$stageResults1), "character")
	    df <- as.data.frame(x$stageResults1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsRates object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallTestStatistics, c(-13.397899, -26.707477, -31.300879, -37.503444, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallPValues, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallEvents1, c(10, 20, 32, 44, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallEvents1, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallEvents2, c(3, 8, 13, 19, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallEvents2, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallSampleSizes1, c(11, 24, 36, 49, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallSampleSizes1, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallSampleSizes2, c(8, 18, 27, 38, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallSampleSizes2, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallPi1, c(0.90909091, 0.83333333, 0.88888889, 0.89795918, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallPi1, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallPi2, c(0.375, 0.44444444, 0.48148148, 0.5, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallPi2, collapse = ", "), ")"))
	expect_equal(x$stageResults2$testStatistics, c(-13.397899, -23.909016, -16.449119, -20.614826, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$pValues, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults2$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$effectSizes, c(0.53409091, 0.38888889, 0.40740741, 0.39795918, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$effectSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults2$combInverseNormal, c(-21.273454, -30.085207, -36.846702, -42.546907, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$combInverseNormal, collapse = ", "), ")"))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$weightsInverseNormal, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	    x$stageResults2CodeBased <- eval(parse(text = getObjectRCode(x$stageResults2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults2CodeBased$overallTestStatistics, x$stageResults2$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallPValues, x$stageResults2$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallEvents1, x$stageResults2$overallEvents1, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallEvents2, x$stageResults2$overallEvents2, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallSampleSizes1, x$stageResults2$overallSampleSizes1, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallSampleSizes2, x$stageResults2$overallSampleSizes2, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallPi1, x$stageResults2$overallPi1, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallPi2, x$stageResults2$overallPi2, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$testStatistics, x$stageResults2$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$pValues, x$stageResults2$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$effectSizes, x$stageResults2$effectSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$combInverseNormal, x$stageResults2$combInverseNormal, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$weightsInverseNormal, x$stageResults2$weightsInverseNormal, tolerance = 1e-07)
	    expect_type(names(x$stageResults2), "character")
	    df <- as.data.frame(x$stageResults2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsRates object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallTestStatistics, c(-13.397899, -26.707477, -31.300879, -37.503444, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallPValues, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallEvents1, c(10, 20, 32, 44, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallEvents1, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallEvents2, c(3, 8, 13, 19, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallEvents2, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallSampleSizes1, c(11, 24, 36, 49, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallSampleSizes1, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallSampleSizes2, c(8, 18, 27, 38, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallSampleSizes2, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallPi1, c(0.90909091, 0.83333333, 0.88888889, 0.89795918, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallPi1, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallPi2, c(0.375, 0.44444444, 0.48148148, 0.5, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallPi2, collapse = ", "), ")"))
	expect_equal(x$stageResults3$testStatistics, c(-13.397899, -23.909016, -16.449119, -20.614826, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$pValues, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults3$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$effectSizes, c(0.53409091, 0.38888889, 0.40740741, 0.39795918, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$effectSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults3$combFisher, c(1, 1, 1, 1, NA_real_), label = paste0("c(", paste0(x$stageResults3$combFisher, collapse = ", "), ")"))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$weightsFisher, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	    x$stageResults3CodeBased <- eval(parse(text = getObjectRCode(x$stageResults3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults3CodeBased$overallTestStatistics, x$stageResults3$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallPValues, x$stageResults3$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallEvents1, x$stageResults3$overallEvents1, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallEvents2, x$stageResults3$overallEvents2, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallSampleSizes1, x$stageResults3$overallSampleSizes1, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallSampleSizes2, x$stageResults3$overallSampleSizes2, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallPi1, x$stageResults3$overallPi1, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallPi2, x$stageResults3$overallPi2, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$testStatistics, x$stageResults3$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$pValues, x$stageResults3$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$effectSizes, x$stageResults3$effectSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$combFisher, x$stageResults3$combFisher, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$weightsFisher, x$stageResults3$weightsFisher, tolerance = 1e-07)
	    expect_type(names(x$stageResults3), "character")
	    df <- as.data.frame(x$stageResults3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("Creation of a dataset of rates using overall data (three groups)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetRates}
	datasetOfRates5 <- getDataset(
	    overallSampleSizes1 = c(11, 24, 36, 49),
	    overallSampleSizes2 = c(8, 18, 27, 38),
	    overallSampleSizes3 = c(8, 18, 27, 38),
	    overallEvents1 = c(10, 20, 32, 44),
	    overallEvents2 = c(3, 8, 13, 19),
	    overallEvents3 = c(3, 7, 12, 20)
	)

	## Comparison of the results of DatasetRates object 'datasetOfRates5' with expected results
	expect_equal(datasetOfRates5$stages, c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4), label = paste0("c(", paste0(datasetOfRates5$stages, collapse = ", "), ")"))
	expect_equal(datasetOfRates5$groups, c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3), label = paste0("c(", paste0(datasetOfRates5$groups, collapse = ", "), ")"))
	expect_equal(datasetOfRates5$subsets, c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_), label = paste0("c(", paste0(datasetOfRates5$subsets, collapse = ", "), ")"))
	expect_equal(datasetOfRates5$sampleSizes, c(11, 8, 8, 13, 10, 10, 12, 9, 9, 13, 11, 11), label = paste0("c(", paste0(datasetOfRates5$sampleSizes, collapse = ", "), ")"))
	expect_equal(datasetOfRates5$events, c(10, 3, 3, 10, 5, 4, 12, 5, 5, 12, 6, 8), label = paste0("c(", paste0(datasetOfRates5$events, collapse = ", "), ")"))
	expect_equal(datasetOfRates5$overallSampleSizes, c(11, 8, 8, 24, 18, 18, 36, 27, 27, 49, 38, 38), label = paste0("c(", paste0(datasetOfRates5$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(datasetOfRates5$overallEvents, c(10, 3, 3, 20, 8, 7, 32, 13, 12, 44, 19, 20), label = paste0("c(", paste0(datasetOfRates5$overallEvents, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetOfRates5), NA)))
	    expect_output(print(datasetOfRates5)$show())
	    invisible(capture.output(expect_error(summary(datasetOfRates5), NA)))
	    expect_output(summary(datasetOfRates5)$show())
	    datasetOfRates5CodeBased <- eval(parse(text = getObjectRCode(datasetOfRates5, stringWrapParagraphWidth = NULL)))
	    expect_equal(datasetOfRates5CodeBased$stages, datasetOfRates5$stages, tolerance = 1e-07)
	    expect_equal(datasetOfRates5CodeBased$groups, datasetOfRates5$groups, tolerance = 1e-07)
	    expect_equal(datasetOfRates5CodeBased$subsets, datasetOfRates5$subsets, tolerance = 1e-07)
	    expect_equal(datasetOfRates5CodeBased$sampleSizes, datasetOfRates5$sampleSizes, tolerance = 1e-07)
	    expect_equal(datasetOfRates5CodeBased$events, datasetOfRates5$events, tolerance = 1e-07)
	    expect_equal(datasetOfRates5CodeBased$overallSampleSizes, datasetOfRates5$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(datasetOfRates5CodeBased$overallEvents, datasetOfRates5$overallEvents, tolerance = 1e-07)
	    expect_type(names(datasetOfRates5), "character")
	    df <- as.data.frame(datasetOfRates5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(datasetOfRates5)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of data.frame object 'datasetOfRates5$.data' with expected results
	expect_equal(datasetOfRates5$.data$stage, factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)), label = paste0("c(", paste0(datasetOfRates5$.data$stage, collapse = ", "), ")"))
	expect_equal(datasetOfRates5$.data$group, factor(c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)), label = paste0("c(", paste0(datasetOfRates5$.data$group, collapse = ", "), ")"))
	expect_equal(datasetOfRates5$.data$subset, factor(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)), label = paste0("c(", paste0(datasetOfRates5$.data$subset, collapse = ", "), ")"))
	expect_equal(datasetOfRates5$.data$sampleSize, c(11, 8, 8, 13, 10, 10, 12, 9, 9, 13, 11, 11), label = paste0("c(", paste0(datasetOfRates5$.data$sampleSize, collapse = ", "), ")"))
	expect_equal(datasetOfRates5$.data$event, c(10, 3, 3, 10, 5, 4, 12, 5, 5, 12, 6, 8), label = paste0("c(", paste0(datasetOfRates5$.data$event, collapse = ", "), ")"))
	expect_equal(datasetOfRates5$.data$overallSampleSize, c(11, 8, 8, 24, 18, 18, 36, 27, 27, 49, 38, 38), label = paste0("c(", paste0(datasetOfRates5$.data$overallSampleSize, collapse = ", "), ")"))
	expect_equal(datasetOfRates5$.data$overallEvent, c(10, 3, 3, 20, 8, 7, 32, 13, 12, 44, 19, 20), label = paste0("c(", paste0(datasetOfRates5$.data$overallEvent, collapse = ", "), ")"))

})

test_that("Trim command works as expected for rates", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetRates}
	datasetOfRatesExpected <- getDataset(
	    overallSampleSizes1 = c(11, 24, 36, 49),
	    overallSampleSizes2 = c(8, 18, 27, 38),
	    overallSampleSizes3 = c(8, 18, 27, 38),
	    overallEvents1 = c(10, 20, 32, 44),
	    overallEvents2 = c(3, 8, 13, 19),
	    overallEvents3 = c(3, 7, 12, 20)
	)
	datasetOfRates <- getDataset(
	    overallSampleSizes1 = c(11, 24, 36, 49),
	    overallSampleSizes2 = c(8, 18, 27, 38),
	    overallSampleSizes3 = c(8, 18, 27, 38),
	    overallEvents1 = c(10, 20, 32, 44),
	    overallEvents2 = c(3, 8, 13, 19),
	    overallEvents3 = c(3, 7, 12, 20)
	)
	datasetOfRates$.fillWithNAs(6)
	datasetOfRates$.trim(4)

	expect_equal(datasetOfRates$stages, datasetOfRatesExpected$stages)
	expect_equal(datasetOfRates$groups, datasetOfRatesExpected$groups)
	expect_equal(datasetOfRates$overallEvents, datasetOfRatesExpected$overallEvents)
	expect_equal(datasetOfRates$events, datasetOfRatesExpected$events)

	expect_equal(datasetOfRates$.data$stage, datasetOfRatesExpected$.data$stage)
	expect_equal(datasetOfRates$.data$group, datasetOfRatesExpected$.data$group)
	expect_equal(datasetOfRates$.data$overallEvent, datasetOfRatesExpected$.data$overallEvent)
	expect_equal(datasetOfRates$.data$event, datasetOfRatesExpected$.data$event)

})

test_that("Creation of a dataset of survival data using stage wise data", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetSurvival}
	datasetSurvival1 <- getDataset(
	    events = c(8, 7, 4, 12),
	    allocationRatios = c(1, 1, 1, 3.58333333333333),
	    logRanks = c(1.520, 1.273, 0.503, 0.887)
	)

	## Comparison of the results of DatasetSurvival object 'datasetSurvival1' with expected results
	expect_equal(datasetSurvival1$stages, c(1, 2, 3, 4), label = paste0("c(", paste0(datasetSurvival1$stages, collapse = ", "), ")"))
	expect_equal(datasetSurvival1$groups, c(1, 1, 1, 1), label = paste0("c(", paste0(datasetSurvival1$groups, collapse = ", "), ")"))
	expect_equal(datasetSurvival1$subsets, c(NA_character_, NA_character_, NA_character_, NA_character_), label = paste0("c(", paste0(datasetSurvival1$subsets, collapse = ", "), ")"))
	expect_equal(datasetSurvival1$overallEvents, c(8, 15, 19, 31), label = paste0("c(", paste0(datasetSurvival1$overallEvents, collapse = ", "), ")"))
	expect_equal(datasetSurvival1$overallAllocationRatios, c(1, 1, 1, 2), tolerance = 1e-07, label = paste0("c(", paste0(datasetSurvival1$overallAllocationRatios, collapse = ", "), ")"))
	expect_equal(datasetSurvival1$overallLogRanks, c(1.52, 1.9796756, 1.9897802, 2.1096275), tolerance = 1e-07, label = paste0("c(", paste0(datasetSurvival1$overallLogRanks, collapse = ", "), ")"))
	expect_equal(datasetSurvival1$events, c(8, 7, 4, 12), label = paste0("c(", paste0(datasetSurvival1$events, collapse = ", "), ")"))
	expect_equal(datasetSurvival1$allocationRatios, c(1, 1, 1, 3.5833333), tolerance = 1e-07, label = paste0("c(", paste0(datasetSurvival1$allocationRatios, collapse = ", "), ")"))
	expect_equal(datasetSurvival1$logRanks, c(1.52, 1.273, 0.503, 0.887), tolerance = 1e-07, label = paste0("c(", paste0(datasetSurvival1$logRanks, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetSurvival1), NA)))
	    expect_output(print(datasetSurvival1)$show())
	    invisible(capture.output(expect_error(summary(datasetSurvival1), NA)))
	    expect_output(summary(datasetSurvival1)$show())
	    datasetSurvival1CodeBased <- eval(parse(text = getObjectRCode(datasetSurvival1, stringWrapParagraphWidth = NULL)))
	    expect_equal(datasetSurvival1CodeBased$stages, datasetSurvival1$stages, tolerance = 1e-07)
	    expect_equal(datasetSurvival1CodeBased$groups, datasetSurvival1$groups, tolerance = 1e-07)
	    expect_equal(datasetSurvival1CodeBased$subsets, datasetSurvival1$subsets, tolerance = 1e-07)
	    expect_equal(datasetSurvival1CodeBased$overallEvents, datasetSurvival1$overallEvents, tolerance = 1e-07)
	    expect_equal(datasetSurvival1CodeBased$overallAllocationRatios, datasetSurvival1$overallAllocationRatios, tolerance = 1e-07)
	    expect_equal(datasetSurvival1CodeBased$overallLogRanks, datasetSurvival1$overallLogRanks, tolerance = 1e-07)
	    expect_equal(datasetSurvival1CodeBased$events, datasetSurvival1$events, tolerance = 1e-07)
	    expect_equal(datasetSurvival1CodeBased$allocationRatios, datasetSurvival1$allocationRatios, tolerance = 1e-07)
	    expect_equal(datasetSurvival1CodeBased$logRanks, datasetSurvival1$logRanks, tolerance = 1e-07)
	    expect_type(names(datasetSurvival1), "character")
	    df <- as.data.frame(datasetSurvival1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(datasetSurvival1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of data.frame object 'datasetSurvival1$.data' with expected results
	expect_equal(datasetSurvival1$.data$stage, factor(c(1, 2, 3, 4)), label = paste0("c(", paste0(datasetSurvival1$.data$stage, collapse = ", "), ")"))
	expect_equal(datasetSurvival1$.data$group, factor(c(1, 1, 1, 1)), label = paste0("c(", paste0(datasetSurvival1$.data$group, collapse = ", "), ")"))
	expect_equal(datasetSurvival1$.data$subset, factor(c(NA, NA, NA, NA)), label = paste0("c(", paste0(datasetSurvival1$.data$subset, collapse = ", "), ")"))
	expect_equal(datasetSurvival1$.data$overallEvent, c(8, 15, 19, 31), label = paste0("c(", paste0(datasetSurvival1$.data$overallEvent, collapse = ", "), ")"))
	expect_equal(datasetSurvival1$.data$overallAllocationRatio, c(1, 1, 1, 2), tolerance = 1e-07, label = paste0("c(", paste0(datasetSurvival1$.data$overallAllocationRatio, collapse = ", "), ")"))
	expect_equal(datasetSurvival1$.data$overallLogRank, c(1.52, 1.9796756, 1.9897802, 2.1096275), tolerance = 1e-07, label = paste0("c(", paste0(datasetSurvival1$.data$overallLogRank, collapse = ", "), ")"))
	expect_equal(datasetSurvival1$.data$event, c(8, 7, 4, 12), label = paste0("c(", paste0(datasetSurvival1$.data$event, collapse = ", "), ")"))
	expect_equal(datasetSurvival1$.data$allocationRatio, c(1, 1, 1, 3.5833333), tolerance = 1e-07, label = paste0("c(", paste0(datasetSurvival1$.data$allocationRatio, collapse = ", "), ")"))
	expect_equal(datasetSurvival1$.data$logRank, c(1.52, 1.273, 0.503, 0.887), tolerance = 1e-07, label = paste0("c(", paste0(datasetSurvival1$.data$logRank, collapse = ", "), ")"))

	x <- getMultipleStageResultsForDataset(datasetSurvival1)

	## Comparison of the results of StageResultsSurvival object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallEvents, c(8, 15, 19, 31, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallEvents, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallAllocationRatios, c(1, 1, 1, 2, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$overallAllocationRatios, collapse = ", "), ")"))
	expect_equal(x$stageResults1$events, c(8, 7, 4, 12, NA_real_), label = paste0("c(", paste0(x$stageResults1$events, collapse = ", "), ")"))
	expect_equal(x$stageResults1$allocationRatios, c(1, 1, 1, 3.5833333, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$allocationRatios, collapse = ", "), ")"))
	expect_equal(x$stageResults1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$effectSizes, c(2.9294137, 2.7795807, 2.4917213, 2.2339445, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$effectSizes, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	    x$stageResults1CodeBased <- eval(parse(text = getObjectRCode(x$stageResults1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults1CodeBased$overallTestStatistics, x$stageResults1$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallPValues, x$stageResults1$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallEvents, x$stageResults1$overallEvents, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallAllocationRatios, x$stageResults1$overallAllocationRatios, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$events, x$stageResults1$events, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$allocationRatios, x$stageResults1$allocationRatios, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$testStatistics, x$stageResults1$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$pValues, x$stageResults1$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallPValues, x$stageResults1$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$effectSizes, x$stageResults1$effectSizes, tolerance = 1e-07)
	    expect_type(names(x$stageResults1), "character")
	    df <- as.data.frame(x$stageResults1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsSurvival object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallEvents, c(8, 15, 19, 31, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallEvents, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallAllocationRatios, c(1, 1, 1, 2, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$overallAllocationRatios, collapse = ", "), ")"))
	expect_equal(x$stageResults2$events, c(8, 7, 4, 12, NA_real_), label = paste0("c(", paste0(x$stageResults2$events, collapse = ", "), ")"))
	expect_equal(x$stageResults2$allocationRatios, c(1, 1, 1, 3.5833333, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$allocationRatios, collapse = ", "), ")"))
	expect_equal(x$stageResults2$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$effectSizes, c(2.9294137, 2.7795807, 2.4917213, 2.2339445, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$effectSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$combInverseNormal, collapse = ", "), ")"))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$weightsInverseNormal, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	    x$stageResults2CodeBased <- eval(parse(text = getObjectRCode(x$stageResults2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults2CodeBased$overallTestStatistics, x$stageResults2$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallPValues, x$stageResults2$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallEvents, x$stageResults2$overallEvents, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallAllocationRatios, x$stageResults2$overallAllocationRatios, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$events, x$stageResults2$events, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$allocationRatios, x$stageResults2$allocationRatios, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$testStatistics, x$stageResults2$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$pValues, x$stageResults2$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallPValues, x$stageResults2$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$effectSizes, x$stageResults2$effectSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$combInverseNormal, x$stageResults2$combInverseNormal, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$weightsInverseNormal, x$stageResults2$weightsInverseNormal, tolerance = 1e-07)
	    expect_type(names(x$stageResults2), "character")
	    df <- as.data.frame(x$stageResults2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsSurvival object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallEvents, c(8, 15, 19, 31, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallEvents, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallAllocationRatios, c(1, 1, 1, 2, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$overallAllocationRatios, collapse = ", "), ")"))
	expect_equal(x$stageResults3$events, c(8, 7, 4, 12, NA_real_), label = paste0("c(", paste0(x$stageResults3$events, collapse = ", "), ")"))
	expect_equal(x$stageResults3$allocationRatios, c(1, 1, 1, 3.5833333, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$allocationRatios, collapse = ", "), ")"))
	expect_equal(x$stageResults3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$effectSizes, c(2.9294137, 2.7795807, 2.4917213, 2.2339445, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$effectSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$combFisher, collapse = ", "), ")"))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$weightsFisher, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	    x$stageResults3CodeBased <- eval(parse(text = getObjectRCode(x$stageResults3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults3CodeBased$overallTestStatistics, x$stageResults3$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallPValues, x$stageResults3$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallEvents, x$stageResults3$overallEvents, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallAllocationRatios, x$stageResults3$overallAllocationRatios, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$events, x$stageResults3$events, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$allocationRatios, x$stageResults3$allocationRatios, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$testStatistics, x$stageResults3$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$pValues, x$stageResults3$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallPValues, x$stageResults3$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$effectSizes, x$stageResults3$effectSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$combFisher, x$stageResults3$combFisher, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$weightsFisher, x$stageResults3$weightsFisher, tolerance = 1e-07)
	    expect_type(names(x$stageResults3), "character")
	    df <- as.data.frame(x$stageResults3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	expect_equal(factor(datasetSurvival1$stages), datasetSurvival1$.data$stage, tolerance = 1e-07)
	expect_equal(factor(datasetSurvival1$groups), datasetSurvival1$.data$group, tolerance = 1e-07)
	expect_equal(datasetSurvival1$events, datasetSurvival1$.data$event, tolerance = 1e-07)
	expect_equal(datasetSurvival1$allocationRatios, datasetSurvival1$.data$allocationRatio, tolerance = 1e-07)
	expect_equal(datasetSurvival1$logRanks, datasetSurvival1$.data$logRank, tolerance = 1e-07)
	expect_equal(datasetSurvival1$overallEvents, datasetSurvival1$.data$overallEvent, tolerance = 1e-07)
	expect_equal(datasetSurvival1$overallAllocationRatios, datasetSurvival1$.data$overallAllocationRatio, tolerance = 1e-07)
	expect_equal(datasetSurvival1$overallLogRanks, datasetSurvival1$.data$overallLogRank, tolerance = 1e-07)

})

test_that("Creation of a dataset of survival data using overall data", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetSurvival}
	datasetSurvival2 <- getDataset(
	    overallEvents = c(8, 15, 19, 31),
	    overallAllocationRatios = c(1, 1, 1, 2),
	    overallLogRanks = c(1.52, 1.98, 1.99, 2.11)
	)

	## Comparison of the results of DatasetSurvival object 'datasetSurvival2' with expected results
	expect_equal(datasetSurvival2$stages, c(1, 2, 3, 4), label = paste0("c(", paste0(datasetSurvival2$stages, collapse = ", "), ")"))
	expect_equal(datasetSurvival2$groups, c(1, 1, 1, 1), label = paste0("c(", paste0(datasetSurvival2$groups, collapse = ", "), ")"))
	expect_equal(datasetSurvival2$subsets, c(NA_character_, NA_character_, NA_character_, NA_character_), label = paste0("c(", paste0(datasetSurvival2$subsets, collapse = ", "), ")"))
	expect_equal(datasetSurvival2$overallEvents, c(8, 15, 19, 31), label = paste0("c(", paste0(datasetSurvival2$overallEvents, collapse = ", "), ")"))
	expect_equal(datasetSurvival2$overallAllocationRatios, c(1, 1, 1, 2), label = paste0("c(", paste0(datasetSurvival2$overallAllocationRatios, collapse = ", "), ")"))
	expect_equal(datasetSurvival2$overallLogRanks, c(1.52, 1.98, 1.99, 2.11), tolerance = 1e-07, label = paste0("c(", paste0(datasetSurvival2$overallLogRanks, collapse = ", "), ")"))
	expect_equal(datasetSurvival2$events, c(8, 7, 4, 12), label = paste0("c(", paste0(datasetSurvival2$events, collapse = ", "), ")"))
	expect_equal(datasetSurvival2$allocationRatios, c(1, 1, 1, 3.5833333), tolerance = 1e-07, label = paste0("c(", paste0(datasetSurvival2$allocationRatios, collapse = ", "), ")"))
	expect_equal(datasetSurvival2$logRanks, c(1.52, 1.2734749, 0.50285094, 0.8873221), tolerance = 1e-07, label = paste0("c(", paste0(datasetSurvival2$logRanks, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetSurvival2), NA)))
	    expect_output(print(datasetSurvival2)$show())
	    invisible(capture.output(expect_error(summary(datasetSurvival2), NA)))
	    expect_output(summary(datasetSurvival2)$show())
	    datasetSurvival2CodeBased <- eval(parse(text = getObjectRCode(datasetSurvival2, stringWrapParagraphWidth = NULL)))
	    expect_equal(datasetSurvival2CodeBased$stages, datasetSurvival2$stages, tolerance = 1e-07)
	    expect_equal(datasetSurvival2CodeBased$groups, datasetSurvival2$groups, tolerance = 1e-07)
	    expect_equal(datasetSurvival2CodeBased$subsets, datasetSurvival2$subsets, tolerance = 1e-07)
	    expect_equal(datasetSurvival2CodeBased$overallEvents, datasetSurvival2$overallEvents, tolerance = 1e-07)
	    expect_equal(datasetSurvival2CodeBased$overallAllocationRatios, datasetSurvival2$overallAllocationRatios, tolerance = 1e-07)
	    expect_equal(datasetSurvival2CodeBased$overallLogRanks, datasetSurvival2$overallLogRanks, tolerance = 1e-07)
	    expect_equal(datasetSurvival2CodeBased$events, datasetSurvival2$events, tolerance = 1e-07)
	    expect_equal(datasetSurvival2CodeBased$allocationRatios, datasetSurvival2$allocationRatios, tolerance = 1e-07)
	    expect_equal(datasetSurvival2CodeBased$logRanks, datasetSurvival2$logRanks, tolerance = 1e-07)
	    expect_type(names(datasetSurvival2), "character")
	    df <- as.data.frame(datasetSurvival2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(datasetSurvival2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of data.frame object 'datasetSurvival2$.data' with expected results
	expect_equal(datasetSurvival2$.data$stage, factor(c(1, 2, 3, 4)), label = paste0("c(", paste0(datasetSurvival2$.data$stage, collapse = ", "), ")"))
	expect_equal(datasetSurvival2$.data$group, factor(c(1, 1, 1, 1)), label = paste0("c(", paste0(datasetSurvival2$.data$group, collapse = ", "), ")"))
	expect_equal(datasetSurvival2$.data$subset, factor(c(NA, NA, NA, NA)), label = paste0("c(", paste0(datasetSurvival2$.data$subset, collapse = ", "), ")"))
	expect_equal(datasetSurvival2$.data$overallEvent, c(8, 15, 19, 31), label = paste0("c(", paste0(datasetSurvival2$.data$overallEvent, collapse = ", "), ")"))
	expect_equal(datasetSurvival2$.data$overallAllocationRatio, c(1, 1, 1, 2), label = paste0("c(", paste0(datasetSurvival2$.data$overallAllocationRatio, collapse = ", "), ")"))
	expect_equal(datasetSurvival2$.data$overallLogRank, c(1.52, 1.98, 1.99, 2.11), tolerance = 1e-07, label = paste0("c(", paste0(datasetSurvival2$.data$overallLogRank, collapse = ", "), ")"))
	expect_equal(datasetSurvival2$.data$event, c(8, 7, 4, 12), label = paste0("c(", paste0(datasetSurvival2$.data$event, collapse = ", "), ")"))
	expect_equal(datasetSurvival2$.data$allocationRatio, c(1, 1, 1, 3.5833333), tolerance = 1e-07, label = paste0("c(", paste0(datasetSurvival2$.data$allocationRatio, collapse = ", "), ")"))
	expect_equal(datasetSurvival2$.data$logRank, c(1.52, 1.2734749, 0.50285094, 0.8873221), tolerance = 1e-07, label = paste0("c(", paste0(datasetSurvival2$.data$logRank, collapse = ", "), ")"))

	x <- getMultipleStageResultsForDataset(datasetSurvival2)

	## Comparison of the results of StageResultsSurvival object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallEvents, c(8, 15, 19, 31, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallEvents, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallAllocationRatios, c(1, 1, 1, 2, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallAllocationRatios, collapse = ", "), ")"))
	expect_equal(x$stageResults1$events, c(8, 7, 4, 12, NA_real_), label = paste0("c(", paste0(x$stageResults1$events, collapse = ", "), ")"))
	expect_equal(x$stageResults1$allocationRatios, c(1, 1, 1, 3.5833333, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$allocationRatios, collapse = ", "), ")"))
	expect_equal(x$stageResults1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults1$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults1$effectSizes, c(2.9294137, 2.7800464, 2.4919726, 2.2342616, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults1$effectSizes, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	    x$stageResults1CodeBased <- eval(parse(text = getObjectRCode(x$stageResults1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults1CodeBased$overallTestStatistics, x$stageResults1$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallPValues, x$stageResults1$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallEvents, x$stageResults1$overallEvents, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallAllocationRatios, x$stageResults1$overallAllocationRatios, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$events, x$stageResults1$events, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$allocationRatios, x$stageResults1$allocationRatios, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$testStatistics, x$stageResults1$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$pValues, x$stageResults1$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$overallPValues, x$stageResults1$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults1CodeBased$effectSizes, x$stageResults1$effectSizes, tolerance = 1e-07)
	    expect_type(names(x$stageResults1), "character")
	    df <- as.data.frame(x$stageResults1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsSurvival object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallEvents, c(8, 15, 19, 31, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallEvents, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallAllocationRatios, c(1, 1, 1, 2, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallAllocationRatios, collapse = ", "), ")"))
	expect_equal(x$stageResults2$events, c(8, 7, 4, 12, NA_real_), label = paste0("c(", paste0(x$stageResults2$events, collapse = ", "), ")"))
	expect_equal(x$stageResults2$allocationRatios, c(1, 1, 1, 3.5833333, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$allocationRatios, collapse = ", "), ")"))
	expect_equal(x$stageResults2$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults2$effectSizes, c(2.9294137, 2.7800464, 2.4919726, 2.2342616, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$effectSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults2$combInverseNormal, collapse = ", "), ")"))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults2$weightsInverseNormal, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	    x$stageResults2CodeBased <- eval(parse(text = getObjectRCode(x$stageResults2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults2CodeBased$overallTestStatistics, x$stageResults2$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallPValues, x$stageResults2$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallEvents, x$stageResults2$overallEvents, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallAllocationRatios, x$stageResults2$overallAllocationRatios, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$events, x$stageResults2$events, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$allocationRatios, x$stageResults2$allocationRatios, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$testStatistics, x$stageResults2$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$pValues, x$stageResults2$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$overallPValues, x$stageResults2$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$effectSizes, x$stageResults2$effectSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$combInverseNormal, x$stageResults2$combInverseNormal, tolerance = 1e-07)
	    expect_equal(x$stageResults2CodeBased$weightsInverseNormal, x$stageResults2$weightsInverseNormal, tolerance = 1e-07)
	    expect_type(names(x$stageResults2), "character")
	    df <- as.data.frame(x$stageResults2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of StageResultsSurvival object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallEvents, c(8, 15, 19, 31, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallEvents, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallAllocationRatios, c(1, 1, 1, 2, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallAllocationRatios, collapse = ", "), ")"))
	expect_equal(x$stageResults3$events, c(8, 7, 4, 12, NA_real_), label = paste0("c(", paste0(x$stageResults3$events, collapse = ", "), ")"))
	expect_equal(x$stageResults3$allocationRatios, c(1, 1, 1, 3.5833333, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$allocationRatios, collapse = ", "), ")"))
	expect_equal(x$stageResults3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$testStatistics, collapse = ", "), ")"))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$pValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$overallPValues, collapse = ", "), ")"))
	expect_equal(x$stageResults3$effectSizes, c(2.9294137, 2.7800464, 2.4919726, 2.2342616, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$effectSizes, collapse = ", "), ")"))
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x$stageResults3$combFisher, collapse = ", "), ")"))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07, label = paste0("c(", paste0(x$stageResults3$weightsFisher, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	    x$stageResults3CodeBased <- eval(parse(text = getObjectRCode(x$stageResults3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x$stageResults3CodeBased$overallTestStatistics, x$stageResults3$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallPValues, x$stageResults3$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallEvents, x$stageResults3$overallEvents, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallAllocationRatios, x$stageResults3$overallAllocationRatios, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$events, x$stageResults3$events, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$allocationRatios, x$stageResults3$allocationRatios, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$testStatistics, x$stageResults3$testStatistics, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$pValues, x$stageResults3$pValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$overallPValues, x$stageResults3$overallPValues, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$effectSizes, x$stageResults3$effectSizes, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$combFisher, x$stageResults3$combFisher, tolerance = 1e-07)
	    expect_equal(x$stageResults3CodeBased$weightsFisher, x$stageResults3$weightsFisher, tolerance = 1e-07)
	    expect_type(names(x$stageResults3), "character")
	    df <- as.data.frame(x$stageResults3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x$stageResults3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	datasetSurvival3 <- getDataset(
	    events1 = c(25, 32),
	    events2 = c(18, NA),
	    events3 = c(22, 36),
	    logRanks1 = -c(2.2, 1.8),
	    logRanks2 = -c(1.99, NA),
	    logRanks3 = -c(2.32, 2.11)
	)

	## Comparison of the results of DatasetSurvival object 'datasetSurvival3' with expected results
	expect_equal(datasetSurvival3$stages, c(1, 1, 1, 2, 2, 2), label = paste0("c(", paste0(datasetSurvival3$stages, collapse = ", "), ")"))
	expect_equal(datasetSurvival3$groups, c(1, 2, 3, 1, 2, 3), label = paste0("c(", paste0(datasetSurvival3$groups, collapse = ", "), ")"))
	expect_equal(datasetSurvival3$subsets, c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_), label = paste0("c(", paste0(datasetSurvival3$subsets, collapse = ", "), ")"))
	expect_equal(datasetSurvival3$overallEvents, c(25, 18, 22, 57, NA_real_, 58), label = paste0("c(", paste0(datasetSurvival3$overallEvents, collapse = ", "), ")"))
	expect_equal(datasetSurvival3$overallAllocationRatios, c(1, 1, 1, 1, NA_real_, 1), label = paste0("c(", paste0(datasetSurvival3$overallAllocationRatios, collapse = ", "), ")"))
	expect_equal(datasetSurvival3$overallLogRanks, c(-2.2, -1.99, -2.32, -2.8056692, NA_real_, -3.0911851), tolerance = 1e-07, label = paste0("c(", paste0(datasetSurvival3$overallLogRanks, collapse = ", "), ")"))
	expect_equal(datasetSurvival3$events, c(25, 18, 22, 32, NA_real_, 36), label = paste0("c(", paste0(datasetSurvival3$events, collapse = ", "), ")"))
	expect_equal(datasetSurvival3$allocationRatios, c(1, 1, 1, 1, NA_real_, 1), label = paste0("c(", paste0(datasetSurvival3$allocationRatios, collapse = ", "), ")"))
	expect_equal(datasetSurvival3$logRanks, c(-2.2, -1.99, -2.32, -1.8, NA_real_, -2.11), tolerance = 1e-07, label = paste0("c(", paste0(datasetSurvival3$logRanks, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetSurvival3), NA)))
	    expect_output(print(datasetSurvival3)$show())
	    invisible(capture.output(expect_error(summary(datasetSurvival3), NA)))
	    expect_output(summary(datasetSurvival3)$show())
	    datasetSurvival3CodeBased <- eval(parse(text = getObjectRCode(datasetSurvival3, stringWrapParagraphWidth = NULL)))
	    expect_equal(datasetSurvival3CodeBased$stages, datasetSurvival3$stages, tolerance = 1e-07)
	    expect_equal(datasetSurvival3CodeBased$groups, datasetSurvival3$groups, tolerance = 1e-07)
	    expect_equal(datasetSurvival3CodeBased$subsets, datasetSurvival3$subsets, tolerance = 1e-07)
	    expect_equal(datasetSurvival3CodeBased$overallEvents, datasetSurvival3$overallEvents, tolerance = 1e-07)
	    expect_equal(datasetSurvival3CodeBased$overallAllocationRatios, datasetSurvival3$overallAllocationRatios, tolerance = 1e-07)
	    expect_equal(datasetSurvival3CodeBased$overallLogRanks, datasetSurvival3$overallLogRanks, tolerance = 1e-07)
	    expect_equal(datasetSurvival3CodeBased$events, datasetSurvival3$events, tolerance = 1e-07)
	    expect_equal(datasetSurvival3CodeBased$allocationRatios, datasetSurvival3$allocationRatios, tolerance = 1e-07)
	    expect_equal(datasetSurvival3CodeBased$logRanks, datasetSurvival3$logRanks, tolerance = 1e-07)
	    expect_type(names(datasetSurvival3), "character")
	    df <- as.data.frame(datasetSurvival3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(datasetSurvival3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	## Comparison of the results of data.frame object 'datasetSurvival3$.data' with expected results
	expect_equal(datasetSurvival3$.data$stage, factor(c(1, 1, 1, 2, 2, 2)), label = paste0("c(", paste0(datasetSurvival3$.data$stage, collapse = ", "), ")"))
	expect_equal(datasetSurvival3$.data$group, factor(c(1, 2, 3, 1, 2, 3)), label = paste0("c(", paste0(datasetSurvival3$.data$group, collapse = ", "), ")"))
	expect_equal(datasetSurvival3$.data$subset, factor(c(NA, NA, NA, NA, NA, NA)), label = paste0("c(", paste0(datasetSurvival3$.data$subset, collapse = ", "), ")"))
	expect_equal(datasetSurvival3$.data$overallEvent, c(25, 18, 22, 57, NA_real_, 58), label = paste0("c(", paste0(datasetSurvival3$.data$overallEvent, collapse = ", "), ")"))
	expect_equal(datasetSurvival3$.data$overallAllocationRatio, c(1, 1, 1, 1, NA_real_, 1), label = paste0("c(", paste0(datasetSurvival3$.data$overallAllocationRatio, collapse = ", "), ")"))
	expect_equal(datasetSurvival3$.data$overallLogRank, c(-2.2, -1.99, -2.32, -2.8056692, NA_real_, -3.0911851), tolerance = 1e-07, label = paste0("c(", paste0(datasetSurvival3$.data$overallLogRank, collapse = ", "), ")"))
	expect_equal(datasetSurvival3$.data$event, c(25, 18, 22, 32, NA_real_, 36), label = paste0("c(", paste0(datasetSurvival3$.data$event, collapse = ", "), ")"))
	expect_equal(datasetSurvival3$.data$allocationRatio, c(1, 1, 1, 1, NA_real_, 1), label = paste0("c(", paste0(datasetSurvival3$.data$allocationRatio, collapse = ", "), ")"))
	expect_equal(datasetSurvival3$.data$logRank, c(-2.2, -1.99, -2.32, -1.8, NA_real_, -2.11), tolerance = 1e-07, label = paste0("c(", paste0(datasetSurvival3$.data$logRank, collapse = ", "), ")"))

})

test_that("Trim command works as expected for suvival data", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetSurvival}
	dataExampleSurvivalExpected <- getDataset(
	    events1   = c(25, 32),
	    events2   = c(18, NA),
	    events3   = c(22, 36),
	    logRanks1 = c(2.2, 1.8),
	    logRanks2 = c(1.99, NA),
	    logRanks3 = c(2.32, 2.11)
	)
	dataExampleSurvival <- getDataset(
	    events1   = c(25, 32),
	    events2   = c(18, NA),
	    events3   = c(22, 36),
	    logRanks1 = c(2.2, 1.8),
	    logRanks2 = c(1.99, NA),
	    logRanks3 = c(2.32, 2.11)
	)
	dataExampleSurvival$.fillWithNAs(4)
	dataExampleSurvival$.trim(2)

	expect_equal(dataExampleSurvival$stages, dataExampleSurvivalExpected$stages)
	expect_equal(dataExampleSurvival$groups, dataExampleSurvivalExpected$groups)
	expect_equal(dataExampleSurvival$overallEvents, dataExampleSurvivalExpected$overallEvents)
	expect_equal(dataExampleSurvival$overallAllocationRatios, dataExampleSurvivalExpected$overallAllocationRatios)
	expect_equal(dataExampleSurvival$overallLogRanks, dataExampleSurvivalExpected$overallLogRanks, tolerance = 1e-07)
	expect_equal(dataExampleSurvival$events, dataExampleSurvivalExpected$events)
	expect_equal(dataExampleSurvival$allocationRatios, dataExampleSurvivalExpected$allocationRatios)
	expect_equal(dataExampleSurvival$logRanks, dataExampleSurvivalExpected$logRanks, tolerance = 1e-07)

	expect_equal(dataExampleSurvival$.data$stage, dataExampleSurvivalExpected$.data$stage)
	expect_equal(dataExampleSurvival$.data$group, dataExampleSurvivalExpected$.data$group)
	expect_equal(dataExampleSurvival$.data$overallEvent, dataExampleSurvivalExpected$.data$overallEvent)
	expect_equal(dataExampleSurvival$.data$overallAllocationRatio, dataExampleSurvivalExpected$.data$overallAllocationRatio)
	expect_equal(dataExampleSurvival$.data$overallLogRank, dataExampleSurvivalExpected$.data$overallLogRank, tolerance = 1e-07)
	expect_equal(dataExampleSurvival$.data$event, dataExampleSurvivalExpected$.data$event)
	expect_equal(dataExampleSurvival$.data$allocationRatio, dataExampleSurvivalExpected$.data$allocationRatio)
	expect_equal(dataExampleSurvival$.data$logRank, dataExampleSurvivalExpected$.data$logRank, tolerance = 1e-07)

})

test_that("Dataset functions 'getNumberOfStages' and 'getNumberOfGroups' work as expected for means", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetMeans}
	suppressWarnings(
	    data1 <- getDataset(
	        overallN1 = c(22, 33, NA),
	        overallN2 = c(20, 34, 56),
	        overallN3 = c(22, 31, 52),
	        overallMeans1 = c(1.64, 1.54, NA),
	        overallMeans2 = c(1.7, 1.5, 1.77),
	        overallMeans3 = c(2.5, 2.06, 2.99),
	        overallStDevs1 = c(1.5, 1.9, NA),
	        overallStDevs2 = c(1.3, 1.3, 1.1),
	        overallStDevs3 = c(1, 1.3, 1.8)
	    )
	)

	expect_equal(data1$getNumberOfStages(), 3)
	expect_equal(data1$getNumberOfStages(FALSE), 3)
	expect_equal(data1$getNumberOfGroups(), 3)
	expect_equal(data1$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 3)

	.skipTestIfDisabled()

	data2 <- getDataset(
	    overallN1 = c(22, 33, 55),
	    overallN2 = c(20, 34, 56),
	    overallN3 = c(22, 31, 52),
	    overallMeans1 = c(1.64, 1.54, 2.10),
	    overallMeans2 = c(1.7, 1.5, 1.77),
	    overallMeans3 = c(2.5, 2.06, 2.99),
	    overallStDevs1 = c(1.5, 1.9, 1.7),
	    overallStDevs2 = c(1.3, 1.3, 1.1),
	    overallStDevs3 = c(1, 1.3, 1.8)
	)

	expect_equal(data2$getNumberOfStages(), 3)
	expect_equal(data2$getNumberOfStages(FALSE), 3)
	expect_equal(data2$getNumberOfGroups(), 3)
	expect_equal(data2$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 3)

	data3 <- getDataset(
	    overallN1 = c(22, 33, 55),
	    overallN2 = c(20, 34, 56),
	    overallN3 = c(22, 31, 52),
	    overallMeans1 = c(1.64, 1.54, 2.10),
	    overallMeans2 = c(1.7, 1.5, 1.77),
	    overallMeans3 = c(2.5, 2.06, 2.99),
	    overallStDevs1 = c(1.5, 1.9, 1.7),
	    overallStDevs2 = c(1.3, 1.3, 1.1),
	    overallStDevs3 = c(1, 1.3, 1.8)
	)

	expect_equal(data3$getNumberOfStages(), 3)
	expect_equal(data3$getNumberOfStages(FALSE), 3)
	expect_equal(data3$getNumberOfGroups(), 3)
	expect_equal(data3$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 3)

})

test_that("Dataset functions 'getNumberOfStages' and 'getNumberOfGroups' work as expected for rates", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetRates}
	data1 <- getDataset(
	    overallSampleSizes1 = c(11, 24, 36, NA),
	    overallSampleSizes2 = c(8, 18, 27, NA),
	    overallSampleSizes3 = c(8, 18, 27, NA),
	    overallEvents1 = c(10, 20, 32, NA),
	    overallEvents2 = c(3, 8, 13, NA),
	    overallEvents3 = c(3, 7, 12, NA)
	)

	expect_equal(data1$getNumberOfStages(), 3)
	expect_equal(data1$getNumberOfStages(FALSE), 4)
	expect_equal(data1$getNumberOfGroups(), 3)
	expect_equal(data1$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 3)

	.skipTestIfDisabled()

	data2 <- getDataset(
	    overallSampleSizes1 = c(11, 24, 36, 49),
	    overallSampleSizes2 = c(8, 18, 27, 38),
	    overallSampleSizes3 = c(8, 18, 27, 38),
	    overallEvents1 = c(10, 20, 32, 44),
	    overallEvents2 = c(3, 8, 13, 19),
	    overallEvents3 = c(3, 7, 12, 20)
	)

	expect_equal(data2$getNumberOfStages(), 4)
	expect_equal(data2$getNumberOfStages(FALSE), 4)
	expect_equal(data2$getNumberOfGroups(), 3)
	expect_equal(data2$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 3)

	data3 <- getDataset(
	    overallSampleSizes1 = c(11, 24, 36, 49),
	    overallSampleSizes2 = c(8, 18, NA, NA),
	    overallSampleSizes3 = c(8, 18, NA, NA),
	    overallSampleSizes4 = c(8, 18, 27, 38),
	    overallEvents1 = c(10, 20, 32, 44),
	    overallEvents2 = c(3, 8, NA, NA),
	    overallEvents3 = c(3, 8, NA, NA),
	    overallEvents4 = c(3, 7, 12, 20)
	)

	expect_equal(data3$getNumberOfStages(), 4)
	expect_equal(data3$getNumberOfStages(FALSE), 4)
	expect_equal(data3$getNumberOfGroups(), 4)
	expect_equal(data3$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 4)

	data4 <- getDataset(
	    overallSampleSizes1 = c(11, 24, 36),
	    overallSampleSizes2 = c(8, 18, 27),
	    overallEvents1 = c(10, 20, 32),
	    overallEvents2 = c(3, 7, 12)
	)

	expect_equal(data4$getNumberOfStages(), 3)
	expect_equal(data4$getNumberOfStages(FALSE), 3)
	expect_equal(data4$getNumberOfGroups(), 2)
	expect_equal(data4$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 2)

	data5 <- getDataset(
	    overallSampleSizes1 = c(11, 24, NA),
	    overallSampleSizes2 = c(8, 18, NA),
	    overallEvents1 = c(10, 20, NA),
	    overallEvents2 = c(3, 7, NA)
	)

	expect_equal(data5$getNumberOfStages(), 2)
	expect_equal(data5$getNumberOfStages(FALSE), 3)
	expect_equal(data5$getNumberOfGroups(), 2)
	expect_equal(data5$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 2)

	data6 <- getDataset(
	    overallSampleSizes = c(11, 24, NA),
	    overallEvents = c(3, 7, NA)
	)

	expect_equal(data6$getNumberOfStages(), 2)
	expect_equal(data6$getNumberOfStages(FALSE), 3)
	expect_equal(data6$getNumberOfGroups(), 1)
	expect_equal(data6$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 1)

})

test_that("Dataset functions 'getNumberOfStages' and 'getNumberOfGroups' work as expected for survival data", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetSurvival}
	data3 <- getDataset(
	    overallEvents1   = c(13, 33),
	    overallLogRanks1 = c(1.23, 1.55),
	    overallEvents2   = c(16, 33),
	    overallLogRanks2 = c(1.55, 2.2)
	)
	expect_equal(data3$getNumberOfStages(), 2)
	expect_equal(data3$getNumberOfStages(FALSE), 2)
	expect_equal(data3$getNumberOfGroups(), 3)
	expect_equal(data3$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 2)

	data4 <- getDataset(
	    events1   = c(13, NA),
	    logRanks1 = c(1.23, NA),
	    events2   = c(16, NA),
	    logRanks2 = c(1.55, NA)
	)
	expect_equal(data4$getNumberOfStages(), 1)
	expect_equal(data4$getNumberOfStages(FALSE), 2)
	expect_equal(data4$getNumberOfGroups(), 3)
	expect_equal(data4$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 2)

})

test_that("Function '.naOmitBackward' works as expected", {

	.skipTestIfDisabled()

	expect_equal(.naOmitBackward(c(1, NA_real_, 3, NA_real_)), c(1, NA_real_, 3))
	expect_equal(.naOmitBackward(c(1, NA_real_, 3, NA_real_, 5)), c(1, NA_real_, 3, NA_real_, 5))
	expect_equal(.naOmitBackward(c(1, NA_real_, NA_real_)), c(1))
	expect_equal(.naOmitBackward(c(1, NA_real_, NA_real_, 4)), c(1, NA_real_, NA_real_, 4))
	expect_equal(.naOmitBackward(c(1)), c(1))
	expect_equal(.naOmitBackward(c(NA_real_)), c(NA_real_))
	expect_equal(.naOmitBackward(c(1, 2, NA_real_)), c(1, 2))

})

test_plan_section("Testing that 'getDataset' Throws Exceptions as Expected")


test_that("Wrong parameter usage of 'getDataset'", {
	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	expect_error(getDataset(),
	    "Missing argument: data.frame, data vectors, or datasets expected",
	    fixed = TRUE
	)

	expect_error(getDataset(1),
	    "Illegal argument: all parameters must be named",
	    fixed = TRUE
	)

	expect_error(getDataset(n = 1),
	    "Illegal argument: failed to identify dataset type",
	    fixed = TRUE
	)

	expect_error(getDataset(1, x = 2),
	    "Illegal argument: all parameters must be named",
	    fixed = TRUE
	)

	expect_error(getDataset(
	    overallSampleSizes1 = c(11, 24, 36, 49),
	    overallSampleSizes2 = c(8, 18, 27, 38),
	    overallSampleSizes3 = c(8, 18, 27, 38),
	    overallEvents1 = c(10, 20, 32, 44),
	    overallEvents2 = c(3, 8, 13, 19),
	    overallEvents3 = c(3, 8, 13, 19),
	    overallEvents1 = c(3, 8, 13, 19)
	), "Illegal argument: the parameter names must be unique", fixed = TRUE)

})

test_plan_section("Testing datasets for enrichment designs")


test_that("Creation of a dataset of means with subsets", {
	.skipTestIfDisabled()

	x <- getDataset(
	    stage = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
	    subset = c("S1", "S2", "S12", "R", "S1", "S2", "S12", "R", "S1", "S2", "S12", "R"),
	    sampleSize1 = c(12, 14, 21, 33, 33, 22, 12, 14, 21, 33, 33, 22),
	    sampleSize2 = c(18, 11, 21, 9, 17, 18, 12, 14, 21, 33, 33, 22),
	    mean1 = c(107.7, 68.3, 84.9, 77.1, 77.7, 127.4, 107.7, 68.3, 84.9, 77.1, 77.7, 127.4),
	    mean2 = c(165.6, 120.1, 195.9, 162.4, 111.1, 100.9, 107.7, 68.3, 84.9, 77.1, 77.7, 127.4),
	    stDev1 = c(128.5, 124.0, 139.5, 163.5, 133.3, 134.7, 107.7, 68.3, 84.9, 77.1, 77.7, 127.4),
	    stDev2 = c(120.1, 116.8, 185.0, 120.6, 145.6, 133.7, 107.7, 68.3, 84.9, 77.1, 77.7, 127.4)
	)

	## Comparison of the results of DatasetMeans object 'x' with expected results
	expect_equal(x$stages, c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3), label = paste0("c(", paste0(x$stages, collapse = ", "), ")"))
	expect_equal(x$groups, c(1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2), label = paste0("c(", paste0(x$groups, collapse = ", "), ")"))
	expect_equal(x$subsets, c("S1", "S2", "S12", "R", "S1", "S2", "S12", "R", "S1", "S2", "S12", "R", "S1", "S2", "S12", "R", "S1", "S2", "S12", "R", "S1", "S2", "S12", "R"), label = paste0("c(", paste0(x$subsets, collapse = ", "), ")"))
	expect_equal(x$sampleSizes, c(12, 14, 21, 33, 18, 11, 21, 9, 33, 22, 12, 14, 17, 18, 12, 14, 21, 33, 33, 22, 21, 33, 33, 22), label = paste0("c(", paste0(x$sampleSizes, collapse = ", "), ")"))
	expect_equal(x$means, c(107.7, 68.3, 84.9, 77.1, 165.6, 120.1, 195.9, 162.4, 77.7, 127.4, 107.7, 68.3, 111.1, 100.9, 107.7, 68.3, 84.9, 77.1, 77.7, 127.4, 84.9, 77.1, 77.7, 127.4), tolerance = 1e-07, label = paste0("c(", paste0(x$means, collapse = ", "), ")"))
	expect_equal(x$stDevs, c(128.5, 124, 139.5, 163.5, 120.1, 116.8, 185, 120.6, 133.3, 134.7, 107.7, 68.3, 145.6, 133.7, 107.7, 68.3, 84.9, 77.1, 77.7, 127.4, 84.9, 77.1, 77.7, 127.4), tolerance = 1e-07, label = paste0("c(", paste0(x$stDevs, collapse = ", "), ")"))
	expect_equal(x$overallSampleSizes, c(12, 14, 21, 33, 18, 11, 21, 9, 45, 36, 33, 47, 35, 29, 33, 23, 66, 69, 66, 69, 56, 62, 66, 45), label = paste0("c(", paste0(x$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(x$overallMeans, c(107.7, 68.3, 84.9, 77.1, 165.6, 120.1, 195.9, 162.4, 85.7, 104.41667, 93.190909, 74.478723, 139.12857, 108.18276, 163.82727, 105.12174, 85.445455, 91.352174, 85.445455, 91.352174, 118.79286, 91.63871, 120.76364, 116.01333), tolerance = 1e-07, label = paste0("c(", paste0(x$overallMeans, collapse = ", "), ")"))
	expect_equal(x$overallStDevs, c(128.5, 124, 139.5, 163.5, 120.1, 116.8, 185, 120.6, 131.26649, 132.10351, 127.56945, 141.17802, 133.9849, 125.75856, 165.02815, 101.24395, 117.82181, 109.40115, 105.0948, 138.24808, 120.08511, 103.06452, 135.14016, 114.01099), tolerance = 1e-07, label = paste0("c(", paste0(x$overallStDevs, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL)))
	    expect_equal(xCodeBased$stages, x$stages, tolerance = 1e-07)
	    expect_equal(xCodeBased$groups, x$groups, tolerance = 1e-07)
	    expect_equal(xCodeBased$subsets, x$subsets, tolerance = 1e-07)
	    expect_equal(xCodeBased$sampleSizes, x$sampleSizes, tolerance = 1e-07)
	    expect_equal(xCodeBased$means, x$means, tolerance = 1e-07)
	    expect_equal(xCodeBased$stDevs, x$stDevs, tolerance = 1e-07)
	    expect_equal(xCodeBased$overallSampleSizes, x$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(xCodeBased$overallMeans, x$overallMeans, tolerance = 1e-07)
	    expect_equal(xCodeBased$overallStDevs, x$overallStDevs, tolerance = 1e-07)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

	x2 <- getDataset(
	    stages = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
	    subsets = c("S2", "S12", "S1", "R", "S2", "S12", "S1", "R", "S2", "S12", "S1", "R"),
	    overallSampleSizes1 = c(14, 21, 12, 33, 36, 33, 45, 47, 69, 66, 66, 69),
	    overallSampleSizes2 = c(11, 21, 18, 9, 29, 33, 35, 23, 62, 66, 56, 45),
	    overallMeans1 = c(68.3, 84.9, 107.7, 77.1, 104.417, 93.191, 85.7, 74.479, 91.352, 85.445, 85.445, 91.352),
	    overallMeans2 = c(120.1, 195.9, 165.6, 162.4, 108.183, 163.827, 139.129, 105.122, 91.639, 120.764, 118.793, 116.013),
	    overallStDevs1 = c(124, 139.5, 128.5, 163.5, 132.104, 127.569, 131.266, 141.178, 109.401, 105.095, 117.822, 138.248),
	    overallStDevs2 = c(116.8, 185, 120.1, 120.6, 125.759, 165.028, 133.985, 101.244, 103.065, 135.14, 120.085, 114.011)
	)

	## Comparison of the results of DatasetMeans object 'x2' with expected results
	expect_equal(x2$stages, c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3), label = paste0("c(", paste0(x2$stages, collapse = ", "), ")"))
	expect_equal(x2$groups, c(1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2), label = paste0("c(", paste0(x2$groups, collapse = ", "), ")"))
	expect_equal(x2$subsets, c("S1", "S2", "S12", "R", "S1", "S2", "S12", "R", "S1", "S2", "S12", "R", "S1", "S2", "S12", "R", "S1", "S2", "S12", "R", "S1", "S2", "S12", "R"), label = paste0("c(", paste0(x2$subsets, collapse = ", "), ")"))
	expect_equal(x2$sampleSizes, c(12, 14, 21, 33, 18, 11, 21, 9, 33, 22, 12, 14, 17, 18, 12, 14, 21, 33, 33, 22, 21, 33, 33, 22), label = paste0("c(", paste0(x2$sampleSizes, collapse = ", "), ")"))
	expect_equal(x2$means, c(107.7, 68.3, 84.9, 77.1, 165.6, 120.1, 195.9, 162.4, 77.7, 127.40055, 107.70025, 68.300929, 111.10088, 100.90039, 107.69925, 68.300429, 84.898571, 77.099273, 77.699, 127.39886, 84.899667, 77.100333, 77.701, 127.39905), tolerance = 1e-07, label = paste0("c(", paste0(x2$means, collapse = ", "), ")"))
	expect_equal(x2$stDevs, c(128.5, 124, 139.5, 163.5, 120.1, 116.8, 185, 120.6, 133.29934, 134.7007, 107.69841, 68.299913, 145.60038, 133.7007, 107.6989, 68.300382, 84.902527, 77.098435, 77.701172, 127.40021, 84.898999, 77.100624, 77.70049, 127.40009), tolerance = 1e-07, label = paste0("c(", paste0(x2$stDevs, collapse = ", "), ")"))
	expect_equal(x2$overallSampleSizes, c(12, 14, 21, 33, 18, 11, 21, 9, 45, 36, 33, 47, 35, 29, 33, 23, 66, 69, 66, 69, 56, 62, 66, 45), label = paste0("c(", paste0(x2$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(x2$overallMeans, c(107.7, 68.3, 84.9, 77.1, 165.6, 120.1, 195.9, 162.4, 85.7, 104.417, 93.191, 74.479, 139.129, 108.183, 163.827, 105.122, 85.445, 91.352, 85.445, 91.352, 118.793, 91.639, 120.764, 116.013), tolerance = 1e-07, label = paste0("c(", paste0(x2$overallMeans, collapse = ", "), ")"))
	expect_equal(x2$overallStDevs, c(128.5, 124, 139.5, 163.5, 120.1, 116.8, 185, 120.6, 131.266, 132.104, 127.569, 141.178, 133.985, 125.759, 165.028, 101.244, 117.822, 109.401, 105.095, 138.248, 120.085, 103.065, 135.14, 114.011), tolerance = 1e-07, label = paste0("c(", paste0(x2$overallStDevs, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	    x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x2CodeBased$stages, x2$stages, tolerance = 1e-07)
	    expect_equal(x2CodeBased$groups, x2$groups, tolerance = 1e-07)
	    expect_equal(x2CodeBased$subsets, x2$subsets, tolerance = 1e-07)
	    expect_equal(x2CodeBased$sampleSizes, x2$sampleSizes, tolerance = 1e-07)
	    expect_equal(x2CodeBased$means, x2$means, tolerance = 1e-07)
	    expect_equal(x2CodeBased$stDevs, x2$stDevs, tolerance = 1e-07)
	    expect_equal(x2CodeBased$overallSampleSizes, x2$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(x2CodeBased$overallMeans, x2$overallMeans, tolerance = 1e-07)
	    expect_equal(x2CodeBased$overallStDevs, x2$overallStDevs, tolerance = 1e-07)
	    expect_type(names(x2), "character")
	    df <- as.data.frame(x2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	expect_equal(x$sampleSizes, x2$sampleSizes)
	expect_equal(x$means, x2$means, tolerance = 1e-05)
	expect_equal(x$stDevs, x2$stDevs, tolerance = 1e-05)
	expect_equal(x$overallSampleSizes, x2$overallSampleSizes)
	expect_equal(x$overallMeans, x2$overallMeans, tolerance = 1e-05)
	expect_equal(x$overallStDevs, x2$overallStDevs, tolerance = 1e-05)

})

test_that("Creation of a dataset of rates with subsets", {

	.skipTestIfDisabled()
	suppressWarnings(
	    x <- getDataset(
	        stage = c(1, 1, 2, 2),
	        subset = c("S1", "R", "S1", "R"),
	        sampleSizes1 = c(11, 24, 36, 49),
	        sampleSizes2 = c(8, 18, 27, 38),
	        sampleSizes3 = c(8, 18, 27, 38),
	        events1 = c(10, 20, 32, 44),
	        events2 = c(3, 8, 13, 19),
	        events3 = c(3, 7, 12, 20)
	    )
	)

	## Comparison of the results of DatasetRates object 'x' with expected results
	expect_equal(x$stages, c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2), label = paste0("c(", paste0(x$stages, collapse = ", "), ")"))
	expect_equal(x$groups, c(1, 1, 2, 2, 3, 3, 1, 1, 2, 2, 3, 3), label = paste0("c(", paste0(x$groups, collapse = ", "), ")"))
	expect_equal(x$subsets, c("S1", "R", "S1", "R", "S1", "R", "S1", "R", "S1", "R", "S1", "R"), label = paste0("c(", paste0(x$subsets, collapse = ", "), ")"))
	expect_equal(x$sampleSizes, c(11, 24, 8, 18, 8, 18, 36, 49, 27, 38, 27, 38), label = paste0("c(", paste0(x$sampleSizes, collapse = ", "), ")"))
	expect_equal(x$events, c(10, 20, 3, 8, 3, 7, 32, 44, 13, 19, 12, 20), label = paste0("c(", paste0(x$events, collapse = ", "), ")"))
	expect_equal(x$overallSampleSizes, c(11, 24, 8, 18, 8, 18, 47, 73, 35, 56, 35, 56), label = paste0("c(", paste0(x$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(x$overallEvents, c(10, 20, 3, 8, 3, 7, 42, 64, 16, 27, 15, 27), label = paste0("c(", paste0(x$overallEvents, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    suppressWarnings(xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL))))
	    expect_equal(xCodeBased$stages, x$stages, tolerance = 1e-07)
	    expect_equal(xCodeBased$groups, x$groups, tolerance = 1e-07)
	    expect_equal(xCodeBased$subsets, x$subsets, tolerance = 1e-07)
	    expect_equal(xCodeBased$sampleSizes, x$sampleSizes, tolerance = 1e-07)
	    expect_equal(xCodeBased$events, x$events, tolerance = 1e-07)
	    expect_equal(xCodeBased$overallSampleSizes, x$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(xCodeBased$overallEvents, x$overallEvents, tolerance = 1e-07)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("Creation of a dataset of survival data with subsets", {

	.skipTestIfDisabled()

	suppressWarnings(
	    x <- getDataset(
	        stage = c(1, 1, 2, 2),
	        subset = c("S1", "R", "S1", "R"),
	        events1 = c(10, 20, 32, 44),
	        events2 = c(3, 8, 13, 19),
	        events3 = c(3, 7, 12, 20),
	        logRanks1 = c(2.2, 1.8, 1.9, 2.1),
	        logRanks2 = c(1.99, 2.01, 2.05, 2.09),
	        logRanks3 = c(2.32, 2.11, 2.14, 2.17)
	    )
	)

	## Comparison of the results of DatasetSurvival object 'x' with expected results
	expect_equal(x$stages, c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2), label = paste0("c(", paste0(x$stages, collapse = ", "), ")"))
	expect_equal(x$groups, c(1, 1, 2, 2, 3, 3, 1, 1, 2, 2, 3, 3), label = paste0("c(", paste0(x$groups, collapse = ", "), ")"))
	expect_equal(x$subsets, c("S1", "R", "S1", "R", "S1", "R", "S1", "R", "S1", "R", "S1", "R"), label = paste0("c(", paste0(x$subsets, collapse = ", "), ")"))
	expect_equal(x$overallEvents, c(10, 20, 3, 8, 3, 7, 42, 64, 16, 27, 15, 27), label = paste0("c(", paste0(x$overallEvents, collapse = ", "), ")"))
	expect_equal(x$overallAllocationRatios, c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), label = paste0("c(", paste0(x$overallAllocationRatios, collapse = ", "), ")"))
	expect_equal(x$overallLogRanks, c(2.2, 1.8, 1.99, 2.01, 2.32, 2.11, 2.731946, 2.7474586, 2.7095403, 2.8473447, 2.9516097, 2.941998), tolerance = 1e-07, label = paste0("c(", paste0(x$overallLogRanks, collapse = ", "), ")"))
	expect_equal(x$events, c(10, 20, 3, 8, 3, 7, 32, 44, 13, 19, 12, 20), label = paste0("c(", paste0(x$events, collapse = ", "), ")"))
	expect_equal(x$allocationRatios, c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), label = paste0("c(", paste0(x$allocationRatios, collapse = ", "), ")"))
	expect_equal(x$logRanks, c(2.2, 1.8, 1.99, 2.01, 2.32, 2.11, 1.9, 2.1, 2.05, 2.09, 2.14, 2.17), tolerance = 1e-07, label = paste0("c(", paste0(x$logRanks, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    suppressWarnings(xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL))))
	    expect_equal(xCodeBased$stages, x$stages, tolerance = 1e-07)
	    expect_equal(xCodeBased$groups, x$groups, tolerance = 1e-07)
	    expect_equal(xCodeBased$subsets, x$subsets, tolerance = 1e-07)
	    expect_equal(xCodeBased$overallEvents, x$overallEvents, tolerance = 1e-07)
	    expect_equal(xCodeBased$overallAllocationRatios, x$overallAllocationRatios, tolerance = 1e-07)
	    expect_equal(xCodeBased$overallLogRanks, x$overallLogRanks, tolerance = 1e-07)
	    expect_equal(xCodeBased$events, x$events, tolerance = 1e-07)
	    expect_equal(xCodeBased$allocationRatios, x$allocationRatios, tolerance = 1e-07)
	    expect_equal(xCodeBased$logRanks, x$logRanks, tolerance = 1e-07)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("Illegal creation of a dataset of means with subsets: invalid sample size", {

	.skipTestIfDisabled()

	expect_error(
	    getDataset(
	        sampleSize1 = c(NA, NA),
	        sampleSize2 = c(NA, NA),
	        mean1       = c(NA, NA),
	        mean2       = c(NA, NA),
	        stDev1      = c(NA, NA),
	        stDev2      = c(NA, NA)
	    ),
	    "Illegal argument: 'sampleSize1' is NA at first stage; a valid numeric value must be specified at stage 1",
	    fixed = TRUE
	)

})

test_that("Illegal creation of a dataset of means with subsets: too small standard deviation (one subset)", {

	.skipTestIfDisabled()

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
	    stDev1 = c(125.1485, NA),
	    stDev2 = c(118.888, NA)
	)

	expect_error(getDataset(S1 = S1, F = F),
	    "Conflicting arguments: 'stDev' F (125.148) must be > 'stDev' S1 (128.5) in group 1 at stage 1",
	    fixed = TRUE
	)

})

test_that("Illegal creation of a dataset of means with subsets: too small sample size in F (one group)", {

	.skipTestIfDisabled()

	S1 <- getDataset(
	    sampleSize1 = c(12, 21),
	    sampleSize2 = c(30, 21),
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

	expect_error(getDataset(S1 = S1, F = F),
	    "Conflicting arguments: 'sampleSize' F (29) must be >= 'sampleSize' S1 (30) in group 2 at stage 1",
	    fixed = TRUE
	)

})

test_that("Illegal creation of a dataset of means with subsets: wrong deselection (one group)", {

	.skipTestIfDisabled()

	S1 <- getDataset(
	    sampleSize1 = c(12, NA),
	    sampleSize2 = c(18, NA),
	    mean1       = c(107.7, NA),
	    mean2       = c(165.6, NA),
	    stDev1      = c(128.5, NA),
	    stDev2      = c(120.1, NA)
	)

	R <- getDataset(
	    sampleSize1 = c(14, 21),
	    sampleSize2 = c(11, 21),
	    mean1       = c(68.3, 84.9),
	    mean2       = c(120.1, 195.9),
	    stDev1      = c(124.0, 139.5),
	    stDev2      = c(116.8, 185.0)
	)

	expect_error(getDataset(S1 = S1, R = R),
	    paste0(
	        "Conflicting arguments: if S1 is deselected (NA) then R also must be deselected (NA) ",
	        "but, e.g., ", sQuote("sampleSize"), " R is 21 in group 1 at stage 2"
	    ),
	    fixed = TRUE
	)

})

test_that("Illegal creation of a dataset of means with subsets: inconsistent number of stages", {

	.skipTestIfDisabled()

	expect_error(
	    getDataset(
	        sampleSize1 = c(12, NA, 21),
	        sampleSize2 = c(18, NA, 21),
	        mean1       = c(107.7, NA, 84.9),
	        mean2       = c(165.6, NA, 195.9),
	        stDev1      = c(128.5, NA, 139.5),
	        stDev2      = c(120.1, NA, 185.0)
	    ),
	    paste0(
	        "Illegal argument: 'sampleSize1' contains a NA at stage 2 followed by a ",
	        "value for a higher stage; NA's must be the last values"
	    ),
	    fixed = TRUE
	)

	S1 <- getDataset(
	    sampleSize1 = c(12, 21),
	    sampleSize2 = c(18, 21),
	    mean1       = c(107.7, 84.9),
	    mean2       = c(165.6, 195.9),
	    stDev1      = c(128.5, 139.5),
	    stDev2      = c(120.1, 185.0)
	)

	R <- getDataset(
	    sampleSize1 = c(14, NA, NA),
	    sampleSize2 = c(11, NA, NA),
	    mean1       = c(68.3, NA, NA),
	    mean2       = c(120.1, NA, NA),
	    stDev1      = c(124.0, NA, NA),
	    stDev2      = c(116.8, NA, NA)
	)

	expect_error(getDataset(S1 = S1, R = R),
	    paste0(
	        "Conflicting arguments: all subsets must have the identical ",
	        "number of stages defined (kMax: S1 = 2, R = 3)"
	    ),
	    fixed = TRUE
	)

})

test_that("Illegal creation of a dataset of means with subsets: too small standard deviation in F (two subsets)", {

	.skipTestIfDisabled()

	S1N <- getDataset(
	    sampleSize1 = c(39, 34, NA),
	    sampleSize2 = c(33, 45, NA),
	    stDev1      = c(156.5026, 120.084, NA),
	    stDev2      = c(134.0254, 126.502, NA),
	    mean1       = c(131.146, 114.4, NA),
	    mean2       = c(93.191, 85.7, NA)
	)

	S2N <- getDataset(
	    sampleSize1 = c(32, NA, NA),
	    sampleSize2 = c(35, NA, NA),
	    stDev1      = c(163.645, NA, NA),
	    stDev2      = c(131.888, NA, NA),
	    mean1       = c(123.594, NA, NA),
	    mean2       = c(78.26, NA, NA)
	)

	F <- getDataset(
	    sampleSize1 = c(69, NA, NA),
	    sampleSize2 = c(80, NA, NA),
	    stDev1      = c(140.4682, NA, NA),
	    stDev2      = c(143.9796, NA, NA),
	    mean1       = c(129.2957, NA, NA),
	    mean2       = c(82.1875, NA, NA)
	)

	expect_error(getDataset(S1 = S1N, S2 = S2N, F = F),
	    paste0(
	        "Conflicting arguments: 'stDev' F (140.468) must ",
	        "be > 'stDev' S1 (156.503) in group 1 at stage 1"
	    ),
	    fixed = TRUE
	)

})

test_that("Illegal creation of a dataset of means with subsets: too small sample size in F (two subsets)", {

	.skipTestIfDisabled()

	S1N <- getDataset(
	    sampleSize1 = c(39, 34, NA),
	    sampleSize2 = c(33, 45, NA),
	    stDev1      = c(156.5026, 120.084, NA),
	    stDev2      = c(134.0254, 126.502, NA),
	    mean1       = c(131.146, 114.4, NA),
	    mean2       = c(93.191, 85.7, NA)
	)

	S2N <- getDataset(
	    sampleSize1 = c(32, NA, NA),
	    sampleSize2 = c(35, NA, NA),
	    stDev1      = c(163.645, NA, NA),
	    stDev2      = c(131.888, NA, NA),
	    mean1       = c(123.594, NA, NA),
	    mean2       = c(78.26, NA, NA)
	)

	F <- getDataset(
	    sampleSize1 = c(30, NA, NA),
	    sampleSize2 = c(80, NA, NA),
	    stDev1      = c(164.4682, NA, NA),
	    stDev2      = c(143.9796, NA, NA),
	    mean1       = c(129.2957, NA, NA),
	    mean2       = c(82.1875, NA, NA)
	)

	expect_error(getDataset(S1 = S1N, S2 = S2N, F = F),
	    paste0(
	        "Conflicting arguments: 'sampleSize' F (30) must ",
	        "be >= 'sampleSize' S1 (39) in group 1 at stage 1"
	    ),
	    fixed = TRUE
	)

})

test_that("Illegal creation of a dataset of means with subsets: wrong deselection (three subsets)", {

	.skipTestIfDisabled()

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
	    sampleSize2 = c(33, 33, NA),
	    sampleSize1 = c(19, 19, NA),
	    mean2       = c(77.1, 77.1, NA),
	    mean1       = c(142.4, 142.4, NA),
	    stDev2      = c(163.5, 163.5, NA),
	    stDev1      = c(120.6, 120.6, NA)
	)

	expect_error(getDataset(S1 = S1, S2 = S2, S12 = S12, R = R),
	    paste0(
	        "Conflicting arguments: if S2 is deselected (NA) then R also must be deselected ",
	        "(NA) but, e.g., ", sQuote("sampleSize"), " R is 19 in group 1 at stage 2"
	    ),
	    fixed = TRUE
	)

})

test_that("Valid creation of a dataset of means with subsets: no error occurs", {

	.skipTestIfDisabled()

	S1 <- getDataset(
	    sampleSize2 = c(12, 33, 21),
	    sampleSize1 = c(18, 17, 23),
	    mean2       = c(107.7, 77.7, 84.9),
	    mean1       = c(125.6, 111.1, 99.9),
	    stDev2      = c(128.5, 133.3, 84.9),
	    stDev1      = c(120.1, 145.6, 74.3)
	)

	S2 <- getDataset(
	    sampleSize2 = c(14, 22, NA),
	    sampleSize1 = c(11, 18, NA),
	    mean2       = c(68.3, 127.4, NA),
	    mean1       = c(100.1, 110.9, NA),
	    stDev2      = c(124.0, 134.7, NA),
	    stDev1      = c(116.8, 133.7, NA)
	)

	S12 <- getDataset(
	    sampleSize2 = c(21, NA, NA),
	    sampleSize1 = c(21, NA, NA),
	    mean2       = c(84.9, NA, NA),
	    mean1       = c(135.9, NA, NA),
	    stDev2      = c(139.5, NA, NA),
	    stDev1      = c(185.0, NA, NA)
	)

	R <- getDataset(
	    sampleSize2 = c(33, 33, NA),
	    sampleSize1 = c(19, 19, NA),
	    mean2       = c(77.1, 77.1, NA),
	    mean1       = c(142.4, 142.4, NA),
	    stDev2      = c(163.5, 163.5, NA),
	    stDev1      = c(120.6, 120.6, NA)
	)

	expect_error(getDataset(S1 = S1, S2 = S2, S12 = S12, R = R), NA)

})

test_that("Illegal creation of a dataset of rates with subsets: too small number of events in F (one subset)", {

	.skipTestIfDisabled()

	S1 <- getDataset(
	    sampleSize1 = c(22, 31, 37),
	    sampleSize2 = c(28, 33, 39),
	    events1     = c(17, 16, 17),
	    events2     = c(18, 21, 19)
	)

	F <- getDataset(
	    sampleSize1 = c(46, 54, NA),
	    sampleSize2 = c(49, 62, NA),
	    events1     = c(16, 31, NA),
	    events2     = c(29, 35, NA)
	)

	expect_error(getDataset(S1 = S1, F = F),
	    paste0("Conflicting arguments: 'event' F (16) must be >= 'event' S1 (17) in group 1 at stage 1"),
	    fixed = TRUE
	)

})

test_that("Illegal creation of a dataset of rates with subsets: too small sample size in F (one subset)", {

	.skipTestIfDisabled()

	S1 <- getDataset(
	    sampleSize1 = c(22, 31, 37),
	    sampleSize2 = c(28, 33, 39),
	    events1     = c(7, 16, 17),
	    events2     = c(18, 21, 19)
	)

	F <- getDataset(
	    sampleSize1 = c(46, 29, NA),
	    sampleSize2 = c(49, 62, NA),
	    events1     = c(16, 31, NA),
	    events2     = c(29, 35, NA)
	)

	expect_error(getDataset(S1 = S1, F = F),
	    paste0("Conflicting arguments: 'sampleSize' F (29) must be >= 'sampleSize' S1 (31) in group 1 at stage 2"),
	    fixed = TRUE
	)

})

test_that("Illegal creation of a dataset of rates with subsets: wrong deselection (one subset)", {

	.skipTestIfDisabled()

	S1 <- getDataset(
	    sampleSize1 = c(22, 31, NA),
	    sampleSize2 = c(28, 33, NA),
	    events1     = c(7, 16, NA),
	    events2     = c(18, 21, NA)
	)

	R <- getDataset(
	    sampleSize1 = c(24, 23, 37),
	    sampleSize2 = c(21, 29, 39),
	    events1     = c(9, 15, 10),
	    events2     = c(11, 14, 19)
	)

	expect_error(getDataset(S1 = S1, R = R),
	    paste0(
	        "Conflicting arguments: if S1 is deselected (NA) then R also must be ",
	        "deselected (NA) but, e.g., ", sQuote("sampleSize"), " R is 37 in group 1 at stage 3"
	    ),
	    fixed = TRUE
	)

})

test_that("Illegal creation of a dataset of rates with subsets: too small sample size in F (three subsets)", {

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
	    sampleSize1 = c(271, NA, NA),
	    sampleSize2 = c(74, NA, NA),
	    events1     = c(16, NA, NA),
	    events2     = c(21, NA, NA)
	)

	F <- getDataset(
	    sampleSize1 = c(248, NA, NA),
	    sampleSize2 = c(254, NA, NA),
	    events1     = c(75, NA, NA),
	    events2     = c(98, NA, NA)
	)

	expect_error(getDataset(S1 = S1, S2 = S2, S3 = S3, F = F),
	    paste0(
	        "Conflicting arguments: 'sampleSize' F (248) must ",
	        "be >= 'sampleSize' S3 (271) in group 1 at stage 1"
	    ),
	    fixed = TRUE
	)

})

test_that("Illegal creation of a dataset of rates with subsets: wrong deselection (three subsets)", {

	.skipTestIfDisabled()

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
	    sampleSize1 = c(43, 43, 43),
	    sampleSize2 = c(39, 39, 39),
	    events1     = c(17, 17, 17),
	    events2     = c(14, 14, 14)
	)

	expect_error(getDataset(S1 = S1, S2 = S2, S12 = S12, R = R),
	    paste0(
	        "Conflicting arguments: if S2 is deselected (NA) then R also must be ",
	        "deselected (NA) but, e.g., ", sQuote("sampleSize"), " R is 43 in group 1 at stage 2"
	    ),
	    fixed = TRUE
	)

})

test_that("Creation of a dataset of rates with subsets: empty subsets", {

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
	    events1     = c(12, NA, NA),
	    events2     = c(14, NA, NA)
	)

	expect_warning(getDataset(S1 = S1, S2 = S2, S3 = S3, R = R),
	    "The 4 undefined subsets S12, S13, S23, S123 were defined as empty subsets",
	    fixed = TRUE
	)

})

test_that("Illegal creation of a dataset of rates with subsets: wrong deselection (R)", {

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
	    sampleSize1 = c(12, 95, NA),
	    sampleSize2 = c(14, 64, NA),
	    events1     = c(12, 29, NA),
	    events2     = c(14, 26, NA)
	)

	expect_warning(expect_error(getDataset(S1 = S1, S2 = S2, S3 = S3, R = R),
	    paste0(
	        "Conflicting arguments: if S3 is deselected (NA) then R also must be ",
	        "deselected (NA) but, e.g., ", sQuote("sampleSize"), " R is 95 in group 1 at stage 2"
	    ),
	    fixed = TRUE
	))

})

test_that("Illegal creation of a dataset of survival data with subsets: too small number of events (one group)", {

	.skipTestIfDisabled()

	S1 <- getDataset(
	    events = c(37, 56, 22),
	    logRanks = c(1.66, 1.38, 1.22),
	    allocationRatios = c(1, 1, 1)
	)

	F <- getDataset(
	    events = c(66, 55, NA),
	    logRanks = c(1.98, 1.57, NA),
	    allocationRatios = c(1, 1, NA)
	)

	expect_error(getDataset(S1 = S1, F = F),
	    paste0(
	        "Conflicting arguments: 'event' F (55) must be >= ",
	        "'event' S1 (56) in group 1 at stage 2"
	    ),
	    fixed = TRUE
	)

})

test_that("Illegal creation of a dataset of survival data with subsets: wrong deselection (one group)", {

	.skipTestIfDisabled()

	S1 <- getDataset(
	    overallExpectedEvents = c(13.3, NA, NA),
	    overallEvents = c(16, NA, NA),
	    overallVarianceEvents = c(2.9, NA, NA),
	    overallAllocationRatios = c(1, NA, NA)
	)

	R <- getDataset(
	    overallExpectedEvents = c(23.4, 35.4, 43.7),
	    overallEvents = c(27, 38, 47),
	    overallVarianceEvents = c(3.8, 4.7, 3.4),
	    overallAllocationRatios = c(1, 1, 1)
	)

	expect_error(getDataset(S1 = S1, R = R),
	    paste0(
	        "Conflicting arguments: if S1 is deselected (NA) then R also must ",
	        "be deselected (NA) but, e.g., ", sQuote("overallEvent"), " R is 38 in group 1 at stage 2"
	    ),
	    fixed = TRUE
	)

})

test_that("Creation of a dataset of survival data with subsets: no error occurs", {

	.skipTestIfDisabled()

	S1 <- getDataset(
	    events = c(37, 13, 26),
	    logRanks = -c(1.66, 1.239, 0.785)
	)

	S2 <- getDataset(
	    events = c(31, 18, NA),
	    logRanks = -c(1.98, 1.064, NA)
	)

	F <- getDataset(
	    events = c(37, NA, NA),
	    logRanks = -c(2.18, NA, NA)
	)

	expect_error(getDataset(S1 = S1, S2 = S2, F = F), NA)

})

test_that("Illegal creation of a dataset of survival data with subsets: too small number of events (two groups)", {

	.skipTestIfDisabled()

	S1 <- getDataset(
	    events = c(37, 13, 26),
	    logRanks = -c(1.66, 1.239, 0.785)
	)

	S2 <- getDataset(
	    events = c(31, 18, NA),
	    logRanks = -c(1.98, 1.064, NA)
	)

	F <- getDataset(
	    events = c(30, NA, NA),
	    logRanks = -c(2.18, NA, NA)
	)

	expect_error(getDataset(S1 = S1, S2 = S2, F = F),
	    paste0(
	        "Conflicting arguments: 'event' F (30) must be ",
	        ">= 'event' S1 (37) in group 1 at stage 1"
	    ),
	    fixed = TRUE
	)

})

test_that("Illegal creation of a dataset of survival data with subsets: inconsistent deselection", {

	.skipTestIfDisabled()

	expect_error(getDataset(
	    overallExpectedEvents = c(13.4, 35.4, 43.7),
	    overallEvents = c(16, 37, 47),
	    overallVarianceEvents = c(2.8, 4.7, 3.4),
	    overallAllocationRatios = c(1, 1, NA)
	), paste0(
	    "Conflicting arguments: values of treatment 1 not correctly specified; if NA's exist, then they are ",
	    "mandatory for each parameter at the same stage"
	), fixed = TRUE)

	S1 <- getDataset(
	    overallExpectedEvents = c(13.4, 35.4, 43.7),
	    overallEvents = c(16, 37, 47),
	    overallVarianceEvents = c(2.8, 4.7, 3.4),
	    overallAllocationRatios = c(1, 1, 1)
	)

	expect_error(getDataset(
	    overallExpectedEvents = c(11.5, 31.1, NA),
	    overallEvents = c(15, 33, NA),
	    overallVarianceEvents = c(2.2, 4.4, NA),
	    overallAllocationRatios = c(1, 1, 1)
	), paste0(
	    "Conflicting arguments: values of treatment 1 not correctly specified; if NA's exist, then they are ",
	    "mandatory for each parameter at the same stage"
	), fixed = TRUE)

	S2 <- getDataset(
	    overallExpectedEvents = c(11.5, 31.1, NA),
	    overallEvents = c(15, 33, NA),
	    overallVarianceEvents = c(2.2, 4.4, NA),
	    overallAllocationRatios = c(1, 1, NA)
	)

	S12 <- getDataset(
	    overallExpectedEvents = c(10.1, 29.6, 39.1),
	    overallEvents = c(11, 31, 42),
	    overallVarianceEvents = c(2.8, 4.7, 3.4),
	    overallAllocationRatios = c(1, 1, 1)
	)

	R <- getDataset(
	    overallExpectedEvents = c(23.3, NA, NA),
	    overallEvents = c(25, NA, NA),
	    overallVarianceEvents = c(3.9, NA, NA),
	    overallAllocationRatios = c(1, NA, NA)
	)

	expect_error(getDataset(S1 = S1, S2 = S2, S12 = S12, R = R), NA)

})

test_that("Usage of the forward pipe operator", {

	.skipTestIfDisabled()

	.skipTestIfPipeOperatorNotAvailable()

	analysisResults <- getDesignGroupSequential(informationRates = c(20, 50, 80) / 80) |> getDataset(
	        n = c(20, 30, 30),
	        means = c(45, 51, 45),
	        stDevs = c(130, 140, 120)
	    ) |> getAnalysisResults()

	## Comparison of the results of AnalysisResultsGroupSequential object 'analysisResults' with expected results
	expect_equal(analysisResults$thetaH1, 47.25, tolerance = 1e-07, label = paste0("c(", paste0(analysisResults$thetaH1, collapse = ", "), ")"))
	expect_equal(analysisResults$assumedStDev, 128.66279, tolerance = 1e-07, label = paste0("c(", paste0(analysisResults$assumedStDev, collapse = ", "), ")"))
	expect_equal(analysisResults$testActions, c("continue", "continue", "reject"), label = paste0("c(", paste0(analysisResults$testActions, collapse = ", "), ")"))
	expect_equal(analysisResults$conditionalRejectionProbabilities, c(0.081070604, 0.46575384, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(analysisResults$conditionalRejectionProbabilities, collapse = ", "), ")"))
	expect_equal(analysisResults$conditionalPower, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(analysisResults$conditionalPower, collapse = ", "), ")"))
	expect_equal(analysisResults$repeatedConfidenceIntervalLowerBounds, c(-102.89842, -1.4031361, 18.073438), tolerance = 1e-07, label = paste0("c(", paste0(analysisResults$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(analysisResults$repeatedConfidenceIntervalUpperBounds, c(192.89842, 98.603136, 76.426562), tolerance = 1e-07, label = paste0("c(", paste0(analysisResults$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(analysisResults$repeatedPValues, c(0.29621451, 0.028427113, 0.00077442832), tolerance = 1e-07, label = paste0("c(", paste0(analysisResults$repeatedPValues, collapse = ", "), ")"))
	expect_equal(analysisResults$finalStage, 3, label = paste0("c(", paste0(analysisResults$finalStage, collapse = ", "), ")"))
	expect_equal(analysisResults$finalPValues, c(NA_real_, NA_real_, 0.0060885604), tolerance = 1e-07, label = paste0("c(", paste0(analysisResults$finalPValues, collapse = ", "), ")"))
	expect_equal(analysisResults$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 9.555415), tolerance = 1e-07, label = paste0("c(", paste0(analysisResults$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
	expect_equal(analysisResults$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 71.404491), tolerance = 1e-07, label = paste0("c(", paste0(analysisResults$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
	expect_equal(analysisResults$medianUnbiasedEstimates, c(NA_real_, NA_real_, 41.616412), tolerance = 1e-07, label = paste0("c(", paste0(analysisResults$medianUnbiasedEstimates, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(analysisResults), NA)))
	    expect_output(print(analysisResults)$show())
	    invisible(capture.output(expect_error(summary(analysisResults), NA)))
	    expect_output(summary(analysisResults)$show())
	    analysisResultsCodeBased <- eval(parse(text = getObjectRCode(analysisResults, stringWrapParagraphWidth = NULL)))
	    expect_equal(analysisResultsCodeBased$thetaH1, analysisResults$thetaH1, tolerance = 1e-07)
	    expect_equal(analysisResultsCodeBased$assumedStDev, analysisResults$assumedStDev, tolerance = 1e-07)
	    expect_equal(analysisResultsCodeBased$testActions, analysisResults$testActions, tolerance = 1e-07)
	    expect_equal(analysisResultsCodeBased$conditionalRejectionProbabilities, analysisResults$conditionalRejectionProbabilities, tolerance = 1e-07)
	    expect_equal(analysisResultsCodeBased$conditionalPower, analysisResults$conditionalPower, tolerance = 1e-07)
	    expect_equal(analysisResultsCodeBased$repeatedConfidenceIntervalLowerBounds, analysisResults$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(analysisResultsCodeBased$repeatedConfidenceIntervalUpperBounds, analysisResults$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(analysisResultsCodeBased$repeatedPValues, analysisResults$repeatedPValues, tolerance = 1e-07)
	    expect_equal(analysisResultsCodeBased$finalStage, analysisResults$finalStage, tolerance = 1e-07)
	    expect_equal(analysisResultsCodeBased$finalPValues, analysisResults$finalPValues, tolerance = 1e-07)
	    expect_equal(analysisResultsCodeBased$finalConfidenceIntervalLowerBounds, analysisResults$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
	    expect_equal(analysisResultsCodeBased$finalConfidenceIntervalUpperBounds, analysisResults$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
	    expect_equal(analysisResultsCodeBased$medianUnbiasedEstimates, analysisResults$medianUnbiasedEstimates, tolerance = 1e-07)
	    expect_type(names(analysisResults), "character")
	    df <- as.data.frame(analysisResults)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(analysisResults)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	finalConfidenceInterval <- getDesignGroupSequential() |> getDataset(
	        n = c(20, 30, 30),
	        means = c(45, 51, 45),
	        stDevs = c(130, 140, 120)
	    ) |> getFinalConfidenceInterval()

	## Comparison of the results of list object 'finalConfidenceInterval' with expected results
	expect_equal(finalConfidenceInterval$stage, 3, label = paste0("c(", paste0(finalConfidenceInterval$stage, collapse = ", "), ")"))
	expect_equal(finalConfidenceInterval$thetaH0, 0, label = paste0("c(", paste0(finalConfidenceInterval$thetaH0, collapse = ", "), ")"))
	expect_equal(finalConfidenceInterval$directionUpper, TRUE, label = paste0("c(", paste0(finalConfidenceInterval$directionUpper, collapse = ", "), ")"))
	expect_equal(finalConfidenceInterval$normalApproximation, FALSE, label = paste0("c(", paste0(finalConfidenceInterval$normalApproximation, collapse = ", "), ")"))
	expect_equal(finalConfidenceInterval$equalVariances, TRUE, label = paste0("c(", paste0(finalConfidenceInterval$equalVariances, collapse = ", "), ")"))
	expect_equal(finalConfidenceInterval$tolerance, 1e-06, tolerance = 1e-07, label = paste0("c(", paste0(finalConfidenceInterval$tolerance, collapse = ", "), ")"))
	expect_equal(finalConfidenceInterval$finalStage, 2, label = paste0("c(", paste0(finalConfidenceInterval$finalStage, collapse = ", "), ")"))
	expect_equal(finalConfidenceInterval$medianUnbiasedGeneral, 0.34738475, tolerance = 1e-07, label = paste0("c(", paste0(finalConfidenceInterval$medianUnbiasedGeneral, collapse = ", "), ")"))
	expect_equal(finalConfidenceInterval$finalConfidenceIntervalGeneral, c(0.069908879, 0.62467355), tolerance = 1e-07, label = paste0("c(", paste0(finalConfidenceInterval$finalConfidenceIntervalGeneral, collapse = ", "), ")"))
	expect_equal(finalConfidenceInterval$medianUnbiased, 46.815656, tolerance = 1e-07, label = paste0("c(", paste0(finalConfidenceInterval$medianUnbiased, collapse = ", "), ")"))
	expect_equal(finalConfidenceInterval$finalConfidenceInterval, c(9.4213407, 84.184763), tolerance = 1e-07, label = paste0("c(", paste0(finalConfidenceInterval$finalConfidenceInterval, collapse = ", "), ")"))

	suppressWarnings(
	    stageResults <- getDataset(
	            n = c(20, 30, 30),
	            means = c(45, 51, 45),
	            stDevs = c(130, 140, 120)
	        ) |> getStageResults()
	)

	## Comparison of the results of StageResultsMeans object 'stageResults' with expected results
	expect_equal(stageResults$overallTestStatistics, 1.5480471, tolerance = 1e-07, label = paste0("c(", paste0(stageResults$overallTestStatistics, collapse = ", "), ")"))
	expect_equal(stageResults$overallPValues, 0.0690533, tolerance = 1e-07, label = paste0("c(", paste0(stageResults$overallPValues, collapse = ", "), ")"))
	expect_equal(stageResults$overallMeans, 45, label = paste0("c(", paste0(stageResults$overallMeans, collapse = ", "), ")"))
	expect_equal(stageResults$overallStDevs, 130, label = paste0("c(", paste0(stageResults$overallStDevs, collapse = ", "), ")"))
	expect_equal(stageResults$overallSampleSizes, 20, label = paste0("c(", paste0(stageResults$overallSampleSizes, collapse = ", "), ")"))
	expect_equal(stageResults$testStatistics, 1.5480471, tolerance = 1e-07, label = paste0("c(", paste0(stageResults$testStatistics, collapse = ", "), ")"))
	expect_equal(stageResults$pValues, 0.0690533, tolerance = 1e-07, label = paste0("c(", paste0(stageResults$pValues, collapse = ", "), ")"))
	expect_equal(stageResults$effectSizes, 45, label = paste0("c(", paste0(stageResults$effectSizes, collapse = ", "), ")"))
	expect_equal(stageResults$combInverseNormal, 1.4828789, tolerance = 1e-07, label = paste0("c(", paste0(stageResults$combInverseNormal, collapse = ", "), ")"))
	expect_equal(stageResults$weightsInverseNormal, 1, label = paste0("c(", paste0(stageResults$weightsInverseNormal, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults), NA)))
	    expect_output(print(stageResults)$show())
	    invisible(capture.output(expect_error(summary(stageResults), NA)))
	    expect_output(summary(stageResults)$show())
	    suppressWarnings(stageResultsCodeBased <- eval(parse(text = getObjectRCode(stageResults, stringWrapParagraphWidth = NULL))))
	    expect_equal(stageResultsCodeBased$overallTestStatistics, stageResults$overallTestStatistics, tolerance = 1e-07)
	    expect_equal(stageResultsCodeBased$overallPValues, stageResults$overallPValues, tolerance = 1e-07)
	    expect_equal(stageResultsCodeBased$overallMeans, stageResults$overallMeans, tolerance = 1e-07)
	    expect_equal(stageResultsCodeBased$overallStDevs, stageResults$overallStDevs, tolerance = 1e-07)
	    expect_equal(stageResultsCodeBased$overallSampleSizes, stageResults$overallSampleSizes, tolerance = 1e-07)
	    expect_equal(stageResultsCodeBased$testStatistics, stageResults$testStatistics, tolerance = 1e-07)
	    expect_equal(stageResultsCodeBased$pValues, stageResults$pValues, tolerance = 1e-07)
	    expect_equal(stageResultsCodeBased$effectSizes, stageResults$effectSizes, tolerance = 1e-07)
	    expect_equal(stageResultsCodeBased$combInverseNormal, stageResults$combInverseNormal, tolerance = 1e-07)
	    expect_equal(stageResultsCodeBased$weightsInverseNormal, stageResults$weightsInverseNormal, tolerance = 1e-07)
	    expect_type(names(stageResults), "character")
	    df <- as.data.frame(stageResults)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(stageResults)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
})

