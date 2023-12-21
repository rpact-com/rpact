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
## |  Creation date: 21 December 2023, 08:52:44
## |  File version: $Revision$
## |  Last changed: $Date$
## |  Last changed by: $Author$
## |  

test_plan_section("Testing the Sample Size Calculation of Count Data Designs for Different Designs and Arguments")


test_that("'getSampleSizeCounts': TODO", {
	result <- getSampleSizeCounts(lambda = 1, theta = 0.7, fixedExposureTime = 1)


	## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
	expect_equal(result$directionUpper, FALSE, label = paste0("c(", paste0(result$directionUpper, collapse = ", "), ")"))
	expect_equal(result$lambda1, 0.82352941, tolerance = 1e-07, label = paste0("c(", paste0(result$lambda1, collapse = ", "), ")"))
	expect_equal(result$lambda2, 1.1764706, tolerance = 1e-07, label = paste0("c(", paste0(result$lambda2, collapse = ", "), ")"))
	expect_equal(result$nFixed, 256, label = paste0("c(", paste0(result$nFixed, collapse = ", "), ")"))
	expect_equal(result$maxInformation, 61.696776, tolerance = 1e-07, label = paste0("c(", paste0(result$maxInformation, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result), NA)))
	    expect_output(print(result)$show())
	    invisible(capture.output(expect_error(summary(result), NA)))
	    expect_output(summary(result)$show())
	    resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
	    expect_equal(resultCodeBased$directionUpper, result$directionUpper, tolerance = 1e-07)
	    expect_equal(resultCodeBased$lambda1, result$lambda1, tolerance = 1e-07)
	    expect_equal(resultCodeBased$lambda2, result$lambda2, tolerance = 1e-07)
	    expect_equal(resultCodeBased$nFixed, result$nFixed, tolerance = 1e-07)
	    expect_equal(resultCodeBased$maxInformation, result$maxInformation, tolerance = 1e-07)
	    expect_type(names(result), "character")
	    df <- as.data.frame(result)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
})

test_that("'getPowerCounts': TODO", {

	result <- getPowerCounts(getDesignGroupSequential(kMax = 3, futilityBounds = c(0, 0)),
	    maxNumberOfSubjects = 100,
	    fixedExposureTime = 1, lambda1 = seq(1.05, 1.35, 0.05), lambda2 = 1.4
	)


	## Comparison of the results of TrialDesignPlanCountData object 'result' with expected results
	expect_equal(result$theta, c(0.75, 0.78571429, 0.82142857, 0.85714286, 0.89285714, 0.92857143, 0.96428571), tolerance = 1e-07, label = paste0("c(", paste0(result$theta, collapse = ", "), ")"))
	expect_equal(result$overallReject, c(0.00023094049, 0.00052623691, 0.0011339971, 0.0023129201, 0.0044711043, 0.0082057831, 0.014326257), tolerance = 1e-07, label = paste0("c(", paste0(result$overallReject, collapse = ", "), ")"))
	expect_equal(result$rejectPerStage[1, ], c(5.9116205e-06, 1.0987615e-05, 1.9851638e-05, 3.4900933e-05, 5.9769995e-05, 9.981407e-05, 0.00016270981), tolerance = 1e-07, label = paste0("c(", paste0(result$rejectPerStage[1, ], collapse = ", "), ")"))
	expect_equal(result$rejectPerStage[2, ], c(8.9819666e-05, 0.0001906607, 0.00038584787, 0.00074599173, 0.0013807358, 0.0024514884, 0.004183669), tolerance = 1e-07, label = paste0("c(", paste0(result$rejectPerStage[2, ], collapse = ", "), ")"))
	expect_equal(result$rejectPerStage[3, ], c(0.0001352092, 0.00032458859, 0.00072829756, 0.0015320274, 0.0030305985, 0.0056544806, 0.0099798786), tolerance = 1e-07, label = paste0("c(", paste0(result$rejectPerStage[3, ], collapse = ", "), ")"))
	expect_equal(result$futilityStop, c(0.93396956, 0.9072135, 0.87404782, 0.83444719, 0.78879878, 0.73788853, 0.68283745), tolerance = 1e-07, label = paste0("c(", paste0(result$futilityStop, collapse = ", "), ")"))
	expect_equal(result$futilityPerStage[1, ], c(0.8185177, 0.78015677, 0.73829837, 0.69352558, 0.64653269, 0.59808566, 0.54897999), tolerance = 1e-07, label = paste0("c(", paste0(result$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(result$futilityPerStage[2, ], c(0.11545187, 0.12705674, 0.13574945, 0.14092161, 0.14226609, 0.13980288, 0.13385746), tolerance = 1e-07, label = paste0("c(", paste0(result$futilityPerStage[2, ], collapse = ", "), ")"))
	expect_equal(result$earlyStop, c(0.9340653, 0.90741515, 0.87445352, 0.83522808, 0.79023929, 0.74043984, 0.68718383), tolerance = 1e-07, label = paste0("c(", paste0(result$earlyStop, collapse = ", "), ")"))
	expect_equal(result$futilityBoundsPValueScale[1, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(result$futilityBoundsPValueScale[1, ], collapse = ", "), ")"))
	expect_equal(result$futilityBoundsPValueScale[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(result$futilityBoundsPValueScale[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result), NA)))
	    expect_output(print(result)$show())
	    invisible(capture.output(expect_error(summary(result), NA)))
	    expect_output(summary(result)$show())
	    resultCodeBased <- eval(parse(text = getObjectRCode(result, stringWrapParagraphWidth = NULL)))
	    expect_equal(resultCodeBased$theta, result$theta, tolerance = 1e-07)
	    expect_equal(resultCodeBased$overallReject, result$overallReject, tolerance = 1e-07)
	    expect_equal(resultCodeBased$rejectPerStage, result$rejectPerStage, tolerance = 1e-07)
	    expect_equal(resultCodeBased$futilityStop, result$futilityStop, tolerance = 1e-07)
	    expect_equal(resultCodeBased$futilityPerStage, result$futilityPerStage, tolerance = 1e-07)
	    expect_equal(resultCodeBased$earlyStop, result$earlyStop, tolerance = 1e-07)
	    expect_equal(resultCodeBased$futilityBoundsPValueScale, result$futilityBoundsPValueScale, tolerance = 1e-07)
	    expect_type(names(result), "character")
	    df <- as.data.frame(result)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(result)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
})

