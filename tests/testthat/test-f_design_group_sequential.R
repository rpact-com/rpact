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
## |  File name: test-f_design_group_sequential.R
## |  Creation date: 15 August 2022, 15:51:04
## |  File version: $Revision: 6492 $
## |  Last changed: $Date: 2022-08-15 15:52:28 +0200 (Mo, 15 Aug 2022) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing the Group Sequential and Inverse Normal Design Functionality")


test_that("'getGroupSequentialProbabilities' with one and two continuation regions for weighted test statistic", {
	# @refFS[Formula]{{fs:testStatisticGroupSequentialWeightedAverage}}
	xa <- getGroupSequentialProbabilities(matrix(c(rep(-qnorm(0.95), 4), rep(qnorm(0.95), 4)), nrow = 2, byrow = TRUE), (1:4) / 4)

	## Comparison of the results of matrixarray object 'xa' with expected results
	expect_equal(xa[1, ], c(0.05, 0.030074925, 0.020961248, 0.01595848), tolerance = 1e-07)
	expect_equal(xa[2, ], c(0.95, 0.86992507, 0.8188889, 0.78196917), tolerance = 1e-07)
	expect_equal(xa[3, ], c(1, 0.9, 0.83985015, 0.79792765), tolerance = 1e-07)

	xb <- getGroupSequentialProbabilities(matrix(c(rep(-qnorm(0.95), 4), rep(-1, 4), rep(1, 4), rep(qnorm(0.95), 4)), nrow = 4, byrow = TRUE), (1:4) / 4)

	## Comparison of the results of matrixarray object 'xb' with expected results
	expect_equal(xb[1, ], c(0.05, 0.016446517, 0.005264288, 0.0019569508), tolerance = 1e-07)
	expect_equal(xb[2, ], c(0.15865525, 0.048950554, 0.017478997, 0.0072417024), tolerance = 1e-07)
	expect_equal(xb[3, ], c(0.84134475, 0.16835995, 0.047529077, 0.017187717), tolerance = 1e-07)
	expect_equal(xb[4, ], c(0.95, 0.20086399, 0.059743786, 0.022472468), tolerance = 1e-07)
	expect_equal(xb[5, ], c(1, 0.21731051, 0.065008074, 0.024429419), tolerance = 1e-07)

})

test_that("'getDesignInverseNormal' with default parameters: parameters and results are as expected", {

	# @refFS[Tab.]{fs:tab:output:getDesignInverseNormal}
	# @refFS[Formula]{fs:criticalValuesOBrienFleming}
	x0 <- getDesignInverseNormal()

	## Comparison of the results of TrialDesignInverseNormal object 'x0' with expected results
	expect_equal(x0$alphaSpent, c(0.00025917372, 0.0071600594, 0.02499999), tolerance = 1e-07)
	expect_equal(x0$criticalValues, c(3.4710914, 2.4544323, 2.0040356), tolerance = 1e-07)
	expect_equal(x0$stageLevels, c(0.00025917372, 0.0070553616, 0.022533125), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x0), NA)))
	    expect_output(print(x0)$show())
	    invisible(capture.output(expect_error(summary(x0), NA)))
	    expect_output(summary(x0)$show())
	    x0CodeBased <- eval(parse(text = getObjectRCode(x0, stringWrapParagraphWidth = NULL)))
	    expect_equal(x0CodeBased$alphaSpent, x0$alphaSpent, tolerance = 1e-05)
	    expect_equal(x0CodeBased$criticalValues, x0$criticalValues, tolerance = 1e-05)
	    expect_equal(x0CodeBased$stageLevels, x0$stageLevels, tolerance = 1e-05)
	    expect_type(names(x0), "character")
	    df <- as.data.frame(x0)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x0)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignInverseNormal' with type of design = 'asHSD', 'bsHSD', 'asKD', and 'bsKD'", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getDesignInverseNormal}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingHwangShiDeCani}
	x1 <- getDesignInverseNormal(
	    kMax = 3, informationRates = c(0.2, 0.4, 1),
	    alpha = 0.03, sided = 1, beta = 0.14, typeOfDesign = "asHSD", gammaA = 0
	)

	## Comparison of the results of TrialDesignInverseNormal object 'x1' with expected results
	expect_equal(x1$alphaSpent, c(0.006, 0.012, 0.03), tolerance = 1e-07)
	expect_equal(x1$criticalValues, c(2.5121443, 2.4228747, 2.0280392), tolerance = 1e-07)
	expect_equal(x1$stageLevels, c(0.006, 0.0076991188, 0.021278125), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	    x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x1CodeBased$alphaSpent, x1$alphaSpent, tolerance = 1e-05)
	    expect_equal(x1CodeBased$criticalValues, x1$criticalValues, tolerance = 1e-05)
	    expect_equal(x1CodeBased$stageLevels, x1$stageLevels, tolerance = 1e-05)
	    expect_type(names(x1), "character")
	    df <- as.data.frame(x1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignCharacteristics}
	# @refFS[Formula]{fs:inflationFactor}
	# @refFS[Formula]{fs:expectedReduction}
	y1 <- getDesignCharacteristics(x1)

	## Comparison of the results of TrialDesignCharacteristics object 'y1' with expected results
	expect_equal(y1$nFixed, 8.7681899, tolerance = 1e-07)
	expect_equal(y1$shift, 9.4594101, tolerance = 1e-07)
	expect_equal(y1$inflationFactor, 1.0788327, tolerance = 1e-07)
	expect_equal(y1$information, c(1.891882, 3.7837641, 9.4594101), tolerance = 1e-07)
	expect_equal(y1$power, c(0.12783451, 0.34055165, 0.86), tolerance = 1e-07)
	expect_equal(y1$rejectionProbabilities, c(0.12783451, 0.21271713, 0.51944835), tolerance = 1e-07)
	expect_equal(y1$futilityProbabilities, c(0, 0))
	expect_equal(y1$averageSampleNumber1, 0.83081135, tolerance = 1e-07)
	expect_equal(y1$averageSampleNumber01, 1.0142116, tolerance = 1e-07)
	expect_equal(y1$averageSampleNumber0, 1.0697705, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(y1), NA)))
	    expect_output(print(y1)$show())
	    invisible(capture.output(expect_error(summary(y1), NA)))
	    expect_output(summary(y1)$show())
	    y1CodeBased <- eval(parse(text = getObjectRCode(y1, stringWrapParagraphWidth = NULL)))
	    expect_equal(y1CodeBased$nFixed, y1$nFixed, tolerance = 1e-05)
	    expect_equal(y1CodeBased$shift, y1$shift, tolerance = 1e-05)
	    expect_equal(y1CodeBased$inflationFactor, y1$inflationFactor, tolerance = 1e-05)
	    expect_equal(y1CodeBased$information, y1$information, tolerance = 1e-05)
	    expect_equal(y1CodeBased$power, y1$power, tolerance = 1e-05)
	    expect_equal(y1CodeBased$rejectionProbabilities, y1$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(y1CodeBased$futilityProbabilities, y1$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(y1CodeBased$averageSampleNumber1, y1$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(y1CodeBased$averageSampleNumber01, y1$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(y1CodeBased$averageSampleNumber0, y1$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(y1), "character")
	    df <- as.data.frame(y1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(y1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignInverseNormal}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingHwangShiDeCani}
	# @refFS[Formula]{fs:betaSpendingApproach}
	# @refFS[Formula]{fs:betaSpendingHwangShiDeCani}
	x2 <- getDesignInverseNormal(
	    kMax = 3, informationRates = c(0.2, 0.4, 1),
	    alpha = 0.07, sided = 1, beta = 0.14, typeOfDesign = "asHSD", gammaA = -1,
	    typeBetaSpending = "bsHSD", gammaB = -2
	)

	## Comparison of the results of TrialDesignInverseNormal object 'x2' with expected results
	expect_equal(x2$power, c(0.12038953, 0.32895265, 0.86), tolerance = 1e-07)
	expect_equal(x2$futilityBounds, c(-1.1063623, -0.35992439), tolerance = 1e-07)
	expect_equal(x2$alphaSpent, c(0.0090195874, 0.020036136, 0.07), tolerance = 1e-07)
	expect_equal(x2$betaSpent, c(0.010777094, 0.026854629, 0.14), tolerance = 1e-07)
	expect_equal(x2$criticalValues, c(2.364813, 2.1928805, 1.5660474), tolerance = 1e-07)
	expect_equal(x2$stageLevels, c(0.0090195874, 0.014157994, 0.058668761), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	    x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x2CodeBased$power, x2$power, tolerance = 1e-05)
	    expect_equal(x2CodeBased$futilityBounds, x2$futilityBounds, tolerance = 1e-05)
	    expect_equal(x2CodeBased$alphaSpent, x2$alphaSpent, tolerance = 1e-05)
	    expect_equal(x2CodeBased$betaSpent, x2$betaSpent, tolerance = 1e-05)
	    expect_equal(x2CodeBased$criticalValues, x2$criticalValues, tolerance = 1e-05)
	    expect_equal(x2CodeBased$stageLevels, x2$stageLevels, tolerance = 1e-05)
	    expect_type(names(x2), "character")
	    df <- as.data.frame(x2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignCharacteristics}
	# @refFS[Formula]{fs:inflationFactor}
	# @refFS[Formula]{fs:expectedReduction}
	y2 <- getDesignCharacteristics(x2)

	## Comparison of the results of TrialDesignCharacteristics object 'y2' with expected results
	expect_equal(y2$nFixed, 6.5337002, tolerance = 1e-07)
	expect_equal(y2$shift, 7.1015942, tolerance = 1e-07)
	expect_equal(y2$inflationFactor, 1.0869177, tolerance = 1e-07)
	expect_equal(y2$information, c(1.4203188, 2.8406377, 7.1015942), tolerance = 1e-07)
	expect_equal(y2$power, c(0.12038953, 0.32895265, 0.86), tolerance = 1e-07)
	expect_equal(y2$rejectionProbabilities, c(0.12038953, 0.20856311, 0.53104735), tolerance = 1e-07)
	expect_equal(y2$futilityProbabilities, c(0.010777094, 0.016077535), tolerance = 1e-07)
	expect_equal(y2$averageSampleNumber1, 0.82636428, tolerance = 1e-07)
	expect_equal(y2$averageSampleNumber01, 0.916142, tolerance = 1e-07)
	expect_equal(y2$averageSampleNumber0, 0.79471657, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(y2), NA)))
	    expect_output(print(y2)$show())
	    invisible(capture.output(expect_error(summary(y2), NA)))
	    expect_output(summary(y2)$show())
	    y2CodeBased <- eval(parse(text = getObjectRCode(y2, stringWrapParagraphWidth = NULL)))
	    expect_equal(y2CodeBased$nFixed, y2$nFixed, tolerance = 1e-05)
	    expect_equal(y2CodeBased$shift, y2$shift, tolerance = 1e-05)
	    expect_equal(y2CodeBased$inflationFactor, y2$inflationFactor, tolerance = 1e-05)
	    expect_equal(y2CodeBased$information, y2$information, tolerance = 1e-05)
	    expect_equal(y2CodeBased$power, y2$power, tolerance = 1e-05)
	    expect_equal(y2CodeBased$rejectionProbabilities, y2$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(y2CodeBased$futilityProbabilities, y2$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(y2CodeBased$averageSampleNumber1, y2$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(y2CodeBased$averageSampleNumber01, y2$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(y2CodeBased$averageSampleNumber0, y2$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(y2), "character")
	    df <- as.data.frame(y2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(y2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignInverseNormal}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingKimDeMets}
	# @refFS[Formula]{fs:betaSpendingApproach}
	# @refFS[Formula]{fs:betaSpendingKimDeMets}
	x3 <- getDesignInverseNormal(
	    kMax = 3, informationRates = c(0.3, 0.7, 1),
	    alpha = 0.03, sided = 1, beta = 0.34, typeOfDesign = "asKD", gammaA = 2.2,
	    typeBetaSpending = "bsKD", gammaB = 3.2
	)

	## Comparison of the results of TrialDesignInverseNormal object 'x3' with expected results
	expect_equal(x3$power, c(0.058336437, 0.398246, 0.66), tolerance = 1e-07)
	expect_equal(x3$futilityBounds, c(-1.1558435, 0.72836893), tolerance = 1e-07)
	expect_equal(x3$alphaSpent, c(0.0021222083, 0.013687904, 0.03), tolerance = 1e-07)
	expect_equal(x3$betaSpent, c(0.0072155083, 0.1085907, 0.34), tolerance = 1e-07)
	expect_equal(x3$criticalValues, c(2.8594012, 2.2435708, 1.9735737), tolerance = 1e-07)
	expect_equal(x3$stageLevels, c(0.0021222083, 0.012430014, 0.02421512), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	    x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x3CodeBased$power, x3$power, tolerance = 1e-05)
	    expect_equal(x3CodeBased$futilityBounds, x3$futilityBounds, tolerance = 1e-05)
	    expect_equal(x3CodeBased$alphaSpent, x3$alphaSpent, tolerance = 1e-05)
	    expect_equal(x3CodeBased$betaSpent, x3$betaSpent, tolerance = 1e-05)
	    expect_equal(x3CodeBased$criticalValues, x3$criticalValues, tolerance = 1e-05)
	    expect_equal(x3CodeBased$stageLevels, x3$stageLevels, tolerance = 1e-05)
	    expect_type(names(x3), "character")
	    df <- as.data.frame(x3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignCharacteristics}
	# @refFS[Formula]{fs:inflationFactor}
	# @refFS[Formula]{fs:expectedReduction}
	y3 <- getDesignCharacteristics(x3)

	## Comparison of the results of TrialDesignCharacteristics object 'y3' with expected results
	expect_equal(y3$nFixed, 5.2590265, tolerance = 1e-07)
	expect_equal(y3$shift, 5.551371, tolerance = 1e-07)
	expect_equal(y3$inflationFactor, 1.0555891, tolerance = 1e-07)
	expect_equal(y3$information, c(1.6654113, 3.8859597, 5.551371), tolerance = 1e-07)
	expect_equal(y3$power, c(0.058336437, 0.398246, 0.66), tolerance = 1e-07)
	expect_equal(y3$rejectionProbabilities, c(0.058336437, 0.33990957, 0.261754), tolerance = 1e-07)
	expect_equal(y3$futilityProbabilities, c(0.0072155083, 0.1013752), tolerance = 1e-07)
	expect_equal(y3$averageSampleNumber1, 0.86740735, tolerance = 1e-07)
	expect_equal(y3$averageSampleNumber01, 0.87361707, tolerance = 1e-07)
	expect_equal(y3$averageSampleNumber0, 0.75480974, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(y3), NA)))
	    expect_output(print(y3)$show())
	    invisible(capture.output(expect_error(summary(y3), NA)))
	    expect_output(summary(y3)$show())
	    y3CodeBased <- eval(parse(text = getObjectRCode(y3, stringWrapParagraphWidth = NULL)))
	    expect_equal(y3CodeBased$nFixed, y3$nFixed, tolerance = 1e-05)
	    expect_equal(y3CodeBased$shift, y3$shift, tolerance = 1e-05)
	    expect_equal(y3CodeBased$inflationFactor, y3$inflationFactor, tolerance = 1e-05)
	    expect_equal(y3CodeBased$information, y3$information, tolerance = 1e-05)
	    expect_equal(y3CodeBased$power, y3$power, tolerance = 1e-05)
	    expect_equal(y3CodeBased$rejectionProbabilities, y3$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(y3CodeBased$futilityProbabilities, y3$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(y3CodeBased$averageSampleNumber1, y3$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(y3CodeBased$averageSampleNumber01, y3$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(y3CodeBased$averageSampleNumber0, y3$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(y3), "character")
	    df <- as.data.frame(y3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(y3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignInverseNormal' with binding futility bounds", {

	# @refFS[Tab.]{fs:tab:output:getDesignInverseNormal}
	# @refFS[Formula]{fs:criticalValuesWithFutility}
	# @refFS[Formula]{fs:criticalValuesWangTiatis}
	x4 <- getDesignInverseNormal(
	    kMax = 4, alpha = 0.035, futilityBounds = rep(0.5244, 3),
	    bindingFutility = TRUE, typeOfDesign = "WT", deltaWT = 0.4
	)

	## Comparison of the results of TrialDesignInverseNormal object 'x4' with expected results
	expect_equal(x4$alphaSpent, c(0.0099446089, 0.020756912, 0.029001537, 0.03499999), tolerance = 1e-07)
	expect_equal(x4$criticalValues, c(2.3284312, 2.1725031, 2.0861776, 2.0270171), tolerance = 1e-07)
	expect_equal(x4$stageLevels, c(0.0099446089, 0.014908866, 0.018481267, 0.021330332), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	    x4CodeBased <- eval(parse(text = getObjectRCode(x4, stringWrapParagraphWidth = NULL)))
	    expect_equal(x4CodeBased$alphaSpent, x4$alphaSpent, tolerance = 1e-05)
	    expect_equal(x4CodeBased$criticalValues, x4$criticalValues, tolerance = 1e-05)
	    expect_equal(x4CodeBased$stageLevels, x4$stageLevels, tolerance = 1e-05)
	    expect_type(names(x4), "character")
	    df <- as.data.frame(x4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with type of design = 'asUser'", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	x5 <- getDesignGroupSequential(
	    typeOfDesign = "asUser",
	    userAlphaSpending = c(0.01, 0.02, 0.03, 0.05)
	)

	## Comparison of the results of TrialDesignGroupSequential object 'x5' with expected results
	expect_equal(x5$alphaSpent, c(0.01, 0.02, 0.03, 0.04999999), tolerance = 1e-07)
	expect_equal(x5$criticalValues, c(2.3263479, 2.2192994, 2.1201347, 1.8189562), tolerance = 1e-07)
	expect_equal(x5$stageLevels, c(0.01, 0.01323318, 0.016997342, 0.034459057), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x5), NA)))
	    expect_output(print(x5)$show())
	    invisible(capture.output(expect_error(summary(x5), NA)))
	    expect_output(summary(x5)$show())
	    x5CodeBased <- eval(parse(text = getObjectRCode(x5, stringWrapParagraphWidth = NULL)))
	    expect_equal(x5CodeBased$alphaSpent, x5$alphaSpent, tolerance = 1e-05)
	    expect_equal(x5CodeBased$criticalValues, x5$criticalValues, tolerance = 1e-05)
	    expect_equal(x5CodeBased$stageLevels, x5$stageLevels, tolerance = 1e-05)
	    expect_type(names(x5), "character")
	    df <- as.data.frame(x5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x5)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with type of design = 'asP' and 'bsUser' and non-binding futility bounds", {

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingPocock}
	# @refFS[Formula]{fs:betaSpendingApproach}
	x6a <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.13,
	    typeOfDesign = "asP", typeBetaSpending = "bsUser",
	    informationRates = c(0.35, 0.7, 1),
	    bindingFutility = FALSE,
	    userBetaSpending = c(0.01, 0.05, 0.3)
	)

	## Comparison of the results of TrialDesignGroupSequential object 'x6a' with expected results
	expect_equal(x6a$power, c(0.31774348, 0.5598179, 0.7), tolerance = 1e-07)
	expect_equal(x6a$futilityBounds, c(-1.2557044, -0.16828659), tolerance = 1e-07)
	expect_equal(x6a$alphaSpent, c(0.061214062, 0.10266465, 0.13), tolerance = 1e-07)
	expect_equal(x6a$betaSpent, c(0.01, 0.05, 0.3), tolerance = 1e-07)
	expect_equal(x6a$criticalValues, c(1.5446617, 1.4828682, 1.4620058), tolerance = 1e-07)
	expect_equal(x6a$stageLevels, c(0.061214062, 0.069054712, 0.071869812), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x6a), NA)))
	    expect_output(print(x6a)$show())
	    invisible(capture.output(expect_error(summary(x6a), NA)))
	    expect_output(summary(x6a)$show())
	    x6aCodeBased <- eval(parse(text = getObjectRCode(x6a, stringWrapParagraphWidth = NULL)))
	    expect_equal(x6aCodeBased$power, x6a$power, tolerance = 1e-05)
	    expect_equal(x6aCodeBased$futilityBounds, x6a$futilityBounds, tolerance = 1e-05)
	    expect_equal(x6aCodeBased$alphaSpent, x6a$alphaSpent, tolerance = 1e-05)
	    expect_equal(x6aCodeBased$betaSpent, x6a$betaSpent, tolerance = 1e-05)
	    expect_equal(x6aCodeBased$criticalValues, x6a$criticalValues, tolerance = 1e-05)
	    expect_equal(x6aCodeBased$stageLevels, x6a$stageLevels, tolerance = 1e-05)
	    expect_type(names(x6a), "character")
	    df <- as.data.frame(x6a)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x6a)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with type of design = 'asP' and information rate < 1 at maximum stage", {

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingPocock}
	x6b <- getDesignGroupSequential(informationRates = c(0.4, 0.7), typeOfDesign = "asP")

	## Comparison of the results of TrialDesignGroupSequential object 'x6b' with expected results
	expect_equal(x6b$alphaSpent, c(0.013078429, 0.0197432), tolerance = 1e-07)
	expect_equal(x6b$criticalValues, c(2.223875, 2.3050796), tolerance = 1e-07)
	expect_equal(x6b$stageLevels, c(0.013078429, 0.010581057), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x6b), NA)))
	    expect_output(print(x6b)$show())
	    invisible(capture.output(expect_error(summary(x6b), NA)))
	    expect_output(summary(x6b)$show())
	    x6bCodeBased <- eval(parse(text = getObjectRCode(x6b, stringWrapParagraphWidth = NULL)))
	    expect_equal(x6bCodeBased$alphaSpent, x6b$alphaSpent, tolerance = 1e-05)
	    expect_equal(x6bCodeBased$criticalValues, x6b$criticalValues, tolerance = 1e-05)
	    expect_equal(x6bCodeBased$stageLevels, x6b$stageLevels, tolerance = 1e-05)
	    expect_type(names(x6b), "character")
	    df <- as.data.frame(x6b)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x6b)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with type of design = 'asOF' and 'bsKD' and non-binding futility bounds (kMax = 3)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingOBrienFleming}
	# @refFS[Formula]{fs:betaSpendingApproach}
	# @refFS[Formula]{fs:betaSpendingKimDeMets}
	x7a <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.13, beta = 0.41,
	    typeOfDesign = "asOF", typeBetaSpending = "bsKD",
	    informationRates = c(0.4, 0.75, 1),
	    gammaB = 2.5, bindingFutility = FALSE
	)

	## Comparison of the results of TrialDesignGroupSequential object 'x7a' with expected results
	expect_equal(x7a$power, c(0.10903632, 0.42541278, 0.59), tolerance = 1e-07)
	expect_equal(x7a$futilityBounds, c(-0.83725762, 0.35992547), tolerance = 1e-07)
	expect_equal(x7a$alphaSpent, c(0.016665509, 0.080406163, 0.12999999), tolerance = 1e-07)
	expect_equal(x7a$betaSpent, c(0.041489083, 0.19972711, 0.41), tolerance = 1e-07)
	expect_equal(x7a$criticalValues, c(2.1280732, 1.4368565, 1.2468994), tolerance = 1e-07)
	expect_equal(x7a$stageLevels, c(0.016665509, 0.075379384, 0.1062172), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x7a), NA)))
	    expect_output(print(x7a)$show())
	    invisible(capture.output(expect_error(summary(x7a), NA)))
	    expect_output(summary(x7a)$show())
	    x7aCodeBased <- eval(parse(text = getObjectRCode(x7a, stringWrapParagraphWidth = NULL)))
	    expect_equal(x7aCodeBased$power, x7a$power, tolerance = 1e-05)
	    expect_equal(x7aCodeBased$futilityBounds, x7a$futilityBounds, tolerance = 1e-05)
	    expect_equal(x7aCodeBased$alphaSpent, x7a$alphaSpent, tolerance = 1e-05)
	    expect_equal(x7aCodeBased$betaSpent, x7a$betaSpent, tolerance = 1e-05)
	    expect_equal(x7aCodeBased$criticalValues, x7a$criticalValues, tolerance = 1e-05)
	    expect_equal(x7aCodeBased$stageLevels, x7a$stageLevels, tolerance = 1e-05)
	    expect_type(names(x7a), "character")
	    df <- as.data.frame(x7a)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x7a)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with type of design = 'asOF' and 'bsKD' and non-binding futility bounds (kMax = 4)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingOBrienFleming}
	# @refFS[Formula]{fs:betaSpendingApproach}
	# @refFS[Formula]{fs:betaSpendingKimDeMets}
	x7a <- getDesignGroupSequential(
	    kMax = 4, alpha = 0.13, beta = 0.41,
	    typeOfDesign = "asOF", typeBetaSpending = "bsKD",
	    informationRates = c(0.4, 0.75, 0.85, 1),
	    gammaB = 2.5, bindingFutility = FALSE
	)

	## Comparison of the results of TrialDesignGroupSequential object 'x7a' with expected results
	expect_equal(x7a$power, c(0.1110095, 0.43099683, 0.50326205, 0.59), tolerance = 1e-07)
	expect_equal(x7a$futilityBounds, c(-0.82676531, 0.3743303, 0.65077266), tolerance = 1e-07)
	expect_equal(x7a$alphaSpent, c(0.016665509, 0.080406163, 0.10053322, 0.13), tolerance = 1e-07)
	expect_equal(x7a$betaSpent, c(0.041489083, 0.19972711, 0.27310596, 0.41), tolerance = 1e-07)
	expect_equal(x7a$criticalValues, c(2.1280732, 1.4368565, 1.422873, 1.2970881), tolerance = 1e-07)
	expect_equal(x7a$stageLevels, c(0.016665509, 0.075379384, 0.077386492, 0.097300444), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x7a), NA)))
	    expect_output(print(x7a)$show())
	    invisible(capture.output(expect_error(summary(x7a), NA)))
	    expect_output(summary(x7a)$show())
	    x7aCodeBased <- eval(parse(text = getObjectRCode(x7a, stringWrapParagraphWidth = NULL)))
	    expect_equal(x7aCodeBased$power, x7a$power, tolerance = 1e-05)
	    expect_equal(x7aCodeBased$futilityBounds, x7a$futilityBounds, tolerance = 1e-05)
	    expect_equal(x7aCodeBased$alphaSpent, x7a$alphaSpent, tolerance = 1e-05)
	    expect_equal(x7aCodeBased$betaSpent, x7a$betaSpent, tolerance = 1e-05)
	    expect_equal(x7aCodeBased$criticalValues, x7a$criticalValues, tolerance = 1e-05)
	    expect_equal(x7aCodeBased$stageLevels, x7a$stageLevels, tolerance = 1e-05)
	    expect_type(names(x7a), "character")
	    df <- as.data.frame(x7a)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x7a)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with type of design = 'asP' and 'bsUser' and binding futility bounds", {

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingPocock}
	# @refFS[Formula]{fs:betaSpendingApproach}
	x6b <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.13,
	    typeOfDesign = "asP", typeBetaSpending = "bsUser",
	    informationRates = c(0.35, 0.7, 1),
	    bindingFutility = TRUE,
	    userBetaSpending = c(0.01, 0.05, 0.3)
	)

	## Comparison of the results of TrialDesignGroupSequential object 'x6b' with expected results
	expect_equal(x6b$power, c(0.31728597, 0.55917233, 0.7), tolerance = 1e-07)
	expect_equal(x6b$futilityBounds, c(-1.2569879, -0.17011271), tolerance = 1e-07)
	expect_equal(x6b$alphaSpent, c(0.061214062, 0.10266465, 0.13), tolerance = 1e-07)
	expect_equal(x6b$betaSpent, c(0.01, 0.05, 0.3), tolerance = 1e-07)
	expect_equal(x6b$criticalValues, c(1.5446617, 1.4827312, 1.4588737), tolerance = 1e-07)
	expect_equal(x6b$stageLevels, c(0.061214062, 0.069072925, 0.072299935), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x6b), NA)))
	    expect_output(print(x6b)$show())
	    invisible(capture.output(expect_error(summary(x6b), NA)))
	    expect_output(summary(x6b)$show())
	    x6bCodeBased <- eval(parse(text = getObjectRCode(x6b, stringWrapParagraphWidth = NULL)))
	    expect_equal(x6bCodeBased$power, x6b$power, tolerance = 1e-05)
	    expect_equal(x6bCodeBased$futilityBounds, x6b$futilityBounds, tolerance = 1e-05)
	    expect_equal(x6bCodeBased$alphaSpent, x6b$alphaSpent, tolerance = 1e-05)
	    expect_equal(x6bCodeBased$betaSpent, x6b$betaSpent, tolerance = 1e-05)
	    expect_equal(x6bCodeBased$criticalValues, x6b$criticalValues, tolerance = 1e-05)
	    expect_equal(x6bCodeBased$stageLevels, x6b$stageLevels, tolerance = 1e-05)
	    expect_type(names(x6b), "character")
	    df <- as.data.frame(x6b)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x6b)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with type of design = 'asOF' and 'bsKD' and binding futility bounds (kMax = 3)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingOBrienFleming}
	# @refFS[Formula]{fs:betaSpendingApproach}
	# @refFS[Formula]{fs:betaSpendingKimDeMets}
	x7b <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.13, beta = 0.41,
	    typeOfDesign = "asOF", typeBetaSpending = "bsKD",
	    informationRates = c(0.4, 0.75, 1),
	    gammaB = 2.5, bindingFutility = TRUE
	)

	## Comparison of the results of TrialDesignGroupSequential object 'x7b' with expected results
	expect_equal(x7b$power, c(0.1067887, 0.41918821, 0.59), tolerance = 1e-07)
	expect_equal(x7b$futilityBounds, c(-0.84937686, 0.34328914), tolerance = 1e-07)
	expect_equal(x7b$alphaSpent, c(0.016665509, 0.080406163, 0.12999999), tolerance = 1e-07)
	expect_equal(x7b$betaSpent, c(0.041489083, 0.19972711, 0.41), tolerance = 1e-07)
	expect_equal(x7b$criticalValues, c(2.1280732, 1.4362896, 1.2218662), tolerance = 1e-07)
	expect_equal(x7b$stageLevels, c(0.016665509, 0.075459972, 0.11087911), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x7b), NA)))
	    expect_output(print(x7b)$show())
	    invisible(capture.output(expect_error(summary(x7b), NA)))
	    expect_output(summary(x7b)$show())
	    x7bCodeBased <- eval(parse(text = getObjectRCode(x7b, stringWrapParagraphWidth = NULL)))
	    expect_equal(x7bCodeBased$power, x7b$power, tolerance = 1e-05)
	    expect_equal(x7bCodeBased$futilityBounds, x7b$futilityBounds, tolerance = 1e-05)
	    expect_equal(x7bCodeBased$alphaSpent, x7b$alphaSpent, tolerance = 1e-05)
	    expect_equal(x7bCodeBased$betaSpent, x7b$betaSpent, tolerance = 1e-05)
	    expect_equal(x7bCodeBased$criticalValues, x7b$criticalValues, tolerance = 1e-05)
	    expect_equal(x7bCodeBased$stageLevels, x7b$stageLevels, tolerance = 1e-05)
	    expect_type(names(x7b), "character")
	    df <- as.data.frame(x7b)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x7b)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with type of design = 'asOF' and 'bsKD' and binding futility bounds (kMax = 4)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingOBrienFleming}
	# @refFS[Formula]{fs:betaSpendingApproach}
	# @refFS[Formula]{fs:betaSpendingKimDeMets}
	x7b <- getDesignGroupSequential(
	    kMax = 4, alpha = 0.13, beta = 0.41,
	    typeOfDesign = "asOF", typeBetaSpending = "bsKD",
	    informationRates = c(0.4, 0.75, 0.85, 1),
	    gammaB = 2.5, bindingFutility = TRUE
	)

	## Comparison of the results of TrialDesignGroupSequential object 'x7b' with expected results
	expect_equal(x7b$power, c(0.10806422, 0.422855, 0.4950578, 0.59), tolerance = 1e-07)
	expect_equal(x7b$futilityBounds, c(-0.84247693, 0.35276055, 0.62744509), tolerance = 1e-07)
	expect_equal(x7b$alphaSpent, c(0.016665509, 0.080406163, 0.10053322, 0.13), tolerance = 1e-07)
	expect_equal(x7b$betaSpent, c(0.041489083, 0.19972711, 0.27310596, 0.41), tolerance = 1e-07)
	expect_equal(x7b$criticalValues, c(2.1280732, 1.4362706, 1.4203748, 1.2576258), tolerance = 1e-07)
	expect_equal(x7b$stageLevels, c(0.016665509, 0.075462674, 0.077749297, 0.10426357), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x7b), NA)))
	    expect_output(print(x7b)$show())
	    invisible(capture.output(expect_error(summary(x7b), NA)))
	    expect_output(summary(x7b)$show())
	    x7bCodeBased <- eval(parse(text = getObjectRCode(x7b, stringWrapParagraphWidth = NULL)))
	    expect_equal(x7bCodeBased$power, x7b$power, tolerance = 1e-05)
	    expect_equal(x7bCodeBased$futilityBounds, x7b$futilityBounds, tolerance = 1e-05)
	    expect_equal(x7bCodeBased$alphaSpent, x7b$alphaSpent, tolerance = 1e-05)
	    expect_equal(x7bCodeBased$betaSpent, x7b$betaSpent, tolerance = 1e-05)
	    expect_equal(x7bCodeBased$criticalValues, x7b$criticalValues, tolerance = 1e-05)
	    expect_equal(x7bCodeBased$stageLevels, x7b$stageLevels, tolerance = 1e-05)
	    expect_type(names(x7b), "character")
	    df <- as.data.frame(x7b)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x7b)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with type of design = 'asOF' and 'bsKD' and binding futility bounds, two-sided (kMax = 3)", {

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingOBrienFleming}
	# @refFS[Formula]{fs:betaSpendingApproach}
	# @refFS[Formula]{fs:betaSpendingKimDeMets}
	# @refFS[Formula]{fs:etaSpendingApproachTwoSided}
	# @refFS[Formula]{fs:betaSpendingAdjustment}
	suppressWarnings(x7c <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.09, beta = 0.11, sided = 2,
	    typeOfDesign = "asOF", typeBetaSpending = "bsKD",
	    informationRates = c(0.2, 0.55, 1),
	    gammaB = 2.5, bindingFutility = TRUE
	))

	## Comparison of the results of TrialDesignGroupSequential object 'x7c' with expected results
	expect_equal(x7c$power, c(0.0013105743, 0.39377047, 0.889997), tolerance = 1e-07)
	expect_equal(x7c$futilityBounds, c(NA_real_, 0.30419861), tolerance = 1e-07)
	expect_equal(x7c$alphaSpent, c(1.475171e-05, 0.013740227, 0.09), tolerance = 1e-07)
	expect_equal(x7c$betaSpent, c(0, 0.023123303, 0.11), tolerance = 1e-07)
	expect_equal(x7c$criticalValues, c(4.3323635, 2.4641251, 1.7013171), tolerance = 1e-07)
	expect_equal(x7c$stageLevels, c(7.375855e-06, 0.006867409, 0.044441733), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x7c), NA)))
	    expect_output(print(x7c)$show())
	    invisible(capture.output(expect_error(summary(x7c), NA)))
	    expect_output(summary(x7c)$show())
	    suppressWarnings(x7cCodeBased <- eval(parse(text = getObjectRCode(x7c, stringWrapParagraphWidth = NULL))))
	    expect_equal(x7cCodeBased$power, x7c$power, tolerance = 1e-05)
	    expect_equal(x7cCodeBased$futilityBounds, x7c$futilityBounds, tolerance = 1e-05)
	    expect_equal(x7cCodeBased$alphaSpent, x7c$alphaSpent, tolerance = 1e-05)
	    expect_equal(x7cCodeBased$betaSpent, x7c$betaSpent, tolerance = 1e-05)
	    expect_equal(x7cCodeBased$criticalValues, x7c$criticalValues, tolerance = 1e-05)
	    expect_equal(x7cCodeBased$stageLevels, x7c$stageLevels, tolerance = 1e-05)
	    expect_type(names(x7c), "character")
	    df <- as.data.frame(x7c)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x7c)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	suppressWarnings(x7d <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.05, beta = 0.2, sided = 2,
	    typeOfDesign = "asOF", typeBetaSpending = "bsKD",
	    informationRates = c(0.4, 0.65, 1),
	    gammaB = 1.5, bindingFutility = TRUE
	))

	## Comparison of the results of TrialDesignGroupSequential object 'x7d' with expected results
	expect_equal(x7d$power, c(0.063122463, 0.41229849, 0.79999885), tolerance = 1e-07)
	expect_equal(x7d$futilityBounds, c(0.32391511, 0.9194681), tolerance = 1e-07)
	expect_equal(x7d$alphaSpent, c(0.00078830351, 0.010867832, 0.04999999), tolerance = 1e-07)
	expect_equal(x7d$betaSpent, c(0.050596443, 0.10480935, 0.2), tolerance = 1e-07)
	expect_equal(x7d$criticalValues, c(3.3568694, 2.5549656, 1.9350784), tolerance = 1e-07)
	expect_equal(x7d$stageLevels, c(0.00039415176, 0.0053099152, 0.026490337), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x7d), NA)))
	    expect_output(print(x7d)$show())
	    invisible(capture.output(expect_error(summary(x7d), NA)))
	    expect_output(summary(x7d)$show())
	    suppressWarnings(x7dCodeBased <- eval(parse(text = getObjectRCode(x7d, stringWrapParagraphWidth = NULL))))
	    expect_equal(x7dCodeBased$power, x7d$power, tolerance = 1e-05)
	    expect_equal(x7dCodeBased$futilityBounds, x7d$futilityBounds, tolerance = 1e-05)
	    expect_equal(x7dCodeBased$alphaSpent, x7d$alphaSpent, tolerance = 1e-05)
	    expect_equal(x7dCodeBased$betaSpent, x7d$betaSpent, tolerance = 1e-05)
	    expect_equal(x7dCodeBased$criticalValues, x7d$criticalValues, tolerance = 1e-05)
	    expect_equal(x7dCodeBased$stageLevels, x7d$stageLevels, tolerance = 1e-05)
	    expect_type(names(x7d), "character")
	    df <- as.data.frame(x7d)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x7d)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with type of design = 'asOF' and 'bsKD' and non-binding futility bounds, no betaAdjustment, two-sided (kMax = 3)", {

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingOBrienFleming}
	# @refFS[Formula]{fs:betaSpendingApproach}
	# @refFS[Formula]{fs:betaSpendingKimDeMets}
	# @refFS[Formula]{fs:etaSpendingApproachTwoSided}
	# @refFS[Formula]{fs:betaSpendingAdjustment}
	suppressWarnings(x7e <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.09, beta = 0.11, sided = 2,
	    typeOfDesign = "asOF", typeBetaSpending = "bsKD",
	    informationRates = c(0.4, 0.65, 1),
	    betaAdjustment = FALSE,
	    gammaB = 2.5, bindingFutility = FALSE
	))

	## Comparison of the results of TrialDesignGroupSequential object 'x7e' with expected results
	expect_equal(x7e$power, c(0.14268064, 0.57037981, 0.88999701), tolerance = 1e-07)
	expect_equal(x7e$futilityBounds, c(NA_real_, 0.64692592), tolerance = 1e-07)
	expect_equal(x7e$alphaSpent, c(0.0030525896, 0.025803646, 0.09), tolerance = 1e-07)
	expect_equal(x7e$betaSpent, c(0, 0.037469343, 0.11), tolerance = 1e-07)
	expect_equal(x7e$criticalValues, c(2.9623919, 2.2442359, 1.7391729), tolerance = 1e-07)
	expect_equal(x7e$stageLevels, c(0.0015262948, 0.012408614, 0.041002179), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x7e), NA)))
	    expect_output(print(x7e)$show())
	    invisible(capture.output(expect_error(summary(x7e), NA)))
	    expect_output(summary(x7e)$show())
	    suppressWarnings(x7eCodeBased <- eval(parse(text = getObjectRCode(x7e, stringWrapParagraphWidth = NULL))))
	    expect_equal(x7eCodeBased$power, x7e$power, tolerance = 1e-05)
	    expect_equal(x7eCodeBased$futilityBounds, x7e$futilityBounds, tolerance = 1e-05)
	    expect_equal(x7eCodeBased$alphaSpent, x7e$alphaSpent, tolerance = 1e-05)
	    expect_equal(x7eCodeBased$betaSpent, x7e$betaSpent, tolerance = 1e-05)
	    expect_equal(x7eCodeBased$criticalValues, x7e$criticalValues, tolerance = 1e-05)
	    expect_equal(x7eCodeBased$stageLevels, x7e$stageLevels, tolerance = 1e-05)
	    expect_type(names(x7e), "character")
	    df <- as.data.frame(x7e)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x7e)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with type of design = 'asOF' and 'bsOF', binding futility bounds and delayed response (kMax = 3)", {

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingOBrienFleming}
	# @refFS[Formula]{fs:betaSpendingApproach}
	# @refFS[Formula]{fs:betaSpendingOBrienFleming}
	# @refFS[Formula]{fs:delayedResponseCondition1}
	# @refFS[Formula]{fs:delayedResponseCondition2}
	# @refFS[Formula]{fs:delayedResponsePower}
	suppressWarnings(dl1 <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.05, beta = 0.1, sided = 1,
	    typeOfDesign = "asOF", typeBetaSpending = "bsOF",
	    informationRates = c(0.4, 0.65, 1),
	    bindingFutility = TRUE,
	    delayedInformation = c(0.1, 0.2)
	))

	## Comparison of the results of TrialDesignGroupSequential object 'dl1' with expected results
	expect_equal(dl1$power, c(0.15998522, 0.59313184, 0.9), tolerance = 1e-07)
	expect_equal(dl1$futilityBounds, c(-0.46043472, 0.64445014), tolerance = 1e-07)
	expect_equal(dl1$alphaSpent, c(0.001941913, 0.015055713, 0.05), tolerance = 1e-07)
	expect_equal(dl1$betaSpent, c(0.00930224, 0.041331422, 0.1), tolerance = 1e-07)
	expect_equal(dl1$criticalValues, c(2.8874465, 2.1853011, 1.6575593), tolerance = 1e-07)
	expect_equal(dl1$stageLevels, c(0.001941913, 0.014433388, 0.048703222), tolerance = 1e-07)
	expect_equal(dl1$decisionCriticalValues, c(1.3388855, 1.5378695, 1.6575593), tolerance = 1e-07)
	expect_equal(dl1$reversalProbabilities, c(1.7563249e-06, 0.0014674026), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dl1), NA)))
	    expect_output(print(dl1)$show())
	    invisible(capture.output(expect_error(summary(dl1), NA)))
	    expect_output(summary(dl1)$show())
	    suppressWarnings(dl1CodeBased <- eval(parse(text = getObjectRCode(dl1, stringWrapParagraphWidth = NULL))))
	    expect_equal(dl1CodeBased$power, dl1$power, tolerance = 1e-05)
	    expect_equal(dl1CodeBased$futilityBounds, dl1$futilityBounds, tolerance = 1e-05)
	    expect_equal(dl1CodeBased$alphaSpent, dl1$alphaSpent, tolerance = 1e-05)
	    expect_equal(dl1CodeBased$betaSpent, dl1$betaSpent, tolerance = 1e-05)
	    expect_equal(dl1CodeBased$criticalValues, dl1$criticalValues, tolerance = 1e-05)
	    expect_equal(dl1CodeBased$stageLevels, dl1$stageLevels, tolerance = 1e-05)
	    expect_equal(dl1CodeBased$decisionCriticalValues, dl1$decisionCriticalValues, tolerance = 1e-05)
	    expect_equal(dl1CodeBased$reversalProbabilities, dl1$reversalProbabilities, tolerance = 1e-05)
	    expect_type(names(dl1), "character")
	    df <- as.data.frame(dl1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dl1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	dl2 <- getDesignCharacteristics(dl1)

	## Comparison of the results of TrialDesignCharacteristics object 'dl2' with expected results
	expect_equal(dl2$nFixed, 8.5638474, tolerance = 1e-07)
	expect_equal(dl2$shift, 8.8633082, tolerance = 1e-07)
	expect_equal(dl2$inflationFactor, 1.034968, tolerance = 1e-07)
	expect_equal(dl2$information, c(3.5453233, 5.7611503, 8.8633082), tolerance = 1e-07)
	expect_equal(dl2$power, c(0.15755984, 0.59089729, 0.9), tolerance = 1e-07)
	expect_equal(dl2$rejectionProbabilities, c(0.15755984, 0.43333745, 0.30910271), tolerance = 1e-07)
	expect_equal(dl2$futilityProbabilities, c(0.0095560402, 0.032904105), tolerance = 1e-07)
	expect_equal(dl2$averageSampleNumber1, 0.87652961, tolerance = 1e-07)
	expect_equal(dl2$averageSampleNumber01, 0.92477729, tolerance = 1e-07)
	expect_equal(dl2$averageSampleNumber0, 0.79932679, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dl2), NA)))
	    expect_output(print(dl2)$show())
	    invisible(capture.output(expect_error(summary(dl2), NA)))
	    expect_output(summary(dl2)$show())
	    suppressWarnings(dl2CodeBased <- eval(parse(text = getObjectRCode(dl2, stringWrapParagraphWidth = NULL))))
	    expect_equal(dl2CodeBased$nFixed, dl2$nFixed, tolerance = 1e-05)
	    expect_equal(dl2CodeBased$shift, dl2$shift, tolerance = 1e-05)
	    expect_equal(dl2CodeBased$inflationFactor, dl2$inflationFactor, tolerance = 1e-05)
	    expect_equal(dl2CodeBased$information, dl2$information, tolerance = 1e-05)
	    expect_equal(dl2CodeBased$power, dl2$power, tolerance = 1e-05)
	    expect_equal(dl2CodeBased$rejectionProbabilities, dl2$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(dl2CodeBased$futilityProbabilities, dl2$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(dl2CodeBased$averageSampleNumber1, dl2$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(dl2CodeBased$averageSampleNumber01, dl2$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(dl2CodeBased$averageSampleNumber0, dl2$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(dl2), "character")
	    df <- as.data.frame(dl2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dl2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	suppressWarnings(dl3 <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.05, beta = 0.1, sided = 1,
	    typeOfDesign = "asOF", typeBetaSpending = "bsOF",
	    informationRates = c(0.4, 0.65, 1),
	    bindingFutility = TRUE,
	    delayedInformation = c(0, 0.2)
	))

	## Comparison of the results of TrialDesignGroupSequential object 'dl3' with expected results
	expect_equal(dl3$power, c(0.15998522, 0.59313184, 0.9), tolerance = 1e-07)
	expect_equal(dl3$futilityBounds, c(-0.46043472, 0.64445014), tolerance = 1e-07)
	expect_equal(dl3$alphaSpent, c(0.001941913, 0.015055713, 0.05), tolerance = 1e-07)
	expect_equal(dl3$betaSpent, c(0.00930224, 0.041331422, 0.1), tolerance = 1e-07)
	expect_equal(dl3$criticalValues, c(2.8874465, 2.1853011, 1.6575593), tolerance = 1e-07)
	expect_equal(dl3$stageLevels, c(0.001941913, 0.014433388, 0.048703222), tolerance = 1e-07)
	expect_equal(dl3$decisionCriticalValues, c(NA_real_, 1.5378695, 1.6575593), tolerance = 1e-07)
	expect_equal(dl3$reversalProbabilities, c(NA_real_, 0.0014674026), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dl3), NA)))
	    expect_output(print(dl3)$show())
	    invisible(capture.output(expect_error(summary(dl3), NA)))
	    expect_output(summary(dl3)$show())
	    suppressWarnings(dl3CodeBased <- eval(parse(text = getObjectRCode(dl3, stringWrapParagraphWidth = NULL))))
	    expect_equal(dl3CodeBased$power, dl3$power, tolerance = 1e-05)
	    expect_equal(dl3CodeBased$futilityBounds, dl3$futilityBounds, tolerance = 1e-05)
	    expect_equal(dl3CodeBased$alphaSpent, dl3$alphaSpent, tolerance = 1e-05)
	    expect_equal(dl3CodeBased$betaSpent, dl3$betaSpent, tolerance = 1e-05)
	    expect_equal(dl3CodeBased$criticalValues, dl3$criticalValues, tolerance = 1e-05)
	    expect_equal(dl3CodeBased$stageLevels, dl3$stageLevels, tolerance = 1e-05)
	    expect_equal(dl3CodeBased$decisionCriticalValues, dl3$decisionCriticalValues, tolerance = 1e-05)
	    expect_equal(dl3CodeBased$reversalProbabilities, dl3$reversalProbabilities, tolerance = 1e-05)
	    expect_type(names(dl3), "character")
	    df <- as.data.frame(dl3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dl3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	dl4 <- getDesignCharacteristics(dl3)

	## Comparison of the results of TrialDesignCharacteristics object 'dl4' with expected results
	expect_equal(dl4$nFixed, 8.5638474, tolerance = 1e-07)
	expect_equal(dl4$shift, 8.8633608, tolerance = 1e-07)
	expect_equal(dl4$inflationFactor, 1.0349742, tolerance = 1e-07)
	expect_equal(dl4$information, c(3.5453443, 5.7611845, 8.8633608), tolerance = 1e-07)
	expect_equal(dl4$power, c(0.15755967, 0.59089852, 0.9), tolerance = 1e-07)
	expect_equal(dl4$rejectionProbabilities, c(0.15755967, 0.43333886, 0.30910148), tolerance = 1e-07)
	expect_equal(dl4$futilityProbabilities, c(0.0095558971, 0.032903612), tolerance = 1e-07)
	expect_equal(dl4$averageSampleNumber1, 0.85923802, tolerance = 1e-07)
	expect_equal(dl4$averageSampleNumber01, 0.91378094, tolerance = 1e-07)
	expect_equal(dl4$averageSampleNumber0, 0.76574207, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dl4), NA)))
	    expect_output(print(dl4)$show())
	    invisible(capture.output(expect_error(summary(dl4), NA)))
	    expect_output(summary(dl4)$show())
	    suppressWarnings(dl4CodeBased <- eval(parse(text = getObjectRCode(dl4, stringWrapParagraphWidth = NULL))))
	    expect_equal(dl4CodeBased$nFixed, dl4$nFixed, tolerance = 1e-05)
	    expect_equal(dl4CodeBased$shift, dl4$shift, tolerance = 1e-05)
	    expect_equal(dl4CodeBased$inflationFactor, dl4$inflationFactor, tolerance = 1e-05)
	    expect_equal(dl4CodeBased$information, dl4$information, tolerance = 1e-05)
	    expect_equal(dl4CodeBased$power, dl4$power, tolerance = 1e-05)
	    expect_equal(dl4CodeBased$rejectionProbabilities, dl4$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(dl4CodeBased$futilityProbabilities, dl4$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(dl4CodeBased$averageSampleNumber1, dl4$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(dl4CodeBased$averageSampleNumber01, dl4$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(dl4CodeBased$averageSampleNumber0, dl4$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(dl4), "character")
	    df <- as.data.frame(dl4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dl4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	suppressWarnings(dl5 <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.05, beta = 0.1, sided = 1,
	    typeOfDesign = "asOF", typeBetaSpending = "bsOF",
	    informationRates = c(0.4, 0.65, 1),
	    bindingFutility = TRUE,
	    delayedInformation = 0.3
	))

	## Comparison of the results of TrialDesignGroupSequential object 'dl5' with expected results
	expect_equal(dl5$power, c(0.15998522, 0.59313184, 0.9), tolerance = 1e-07)
	expect_equal(dl5$futilityBounds, c(-0.46043472, 0.64445014), tolerance = 1e-07)
	expect_equal(dl5$alphaSpent, c(0.001941913, 0.015055713, 0.05), tolerance = 1e-07)
	expect_equal(dl5$betaSpent, c(0.00930224, 0.041331422, 0.1), tolerance = 1e-07)
	expect_equal(dl5$criticalValues, c(2.8874465, 2.1853011, 1.6575593), tolerance = 1e-07)
	expect_equal(dl5$stageLevels, c(0.001941913, 0.014433388, 0.048703222), tolerance = 1e-07)
	expect_equal(dl5$decisionCriticalValues, c(1.505831, 1.5735979, 1.6575593), tolerance = 1e-07)
	expect_equal(dl5$reversalProbabilities, c(0.00018341474, 0.0027022502), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dl5), NA)))
	    expect_output(print(dl5)$show())
	    invisible(capture.output(expect_error(summary(dl5), NA)))
	    expect_output(summary(dl5)$show())
	    suppressWarnings(dl5CodeBased <- eval(parse(text = getObjectRCode(dl5, stringWrapParagraphWidth = NULL))))
	    expect_equal(dl5CodeBased$power, dl5$power, tolerance = 1e-05)
	    expect_equal(dl5CodeBased$futilityBounds, dl5$futilityBounds, tolerance = 1e-05)
	    expect_equal(dl5CodeBased$alphaSpent, dl5$alphaSpent, tolerance = 1e-05)
	    expect_equal(dl5CodeBased$betaSpent, dl5$betaSpent, tolerance = 1e-05)
	    expect_equal(dl5CodeBased$criticalValues, dl5$criticalValues, tolerance = 1e-05)
	    expect_equal(dl5CodeBased$stageLevels, dl5$stageLevels, tolerance = 1e-05)
	    expect_equal(dl5CodeBased$decisionCriticalValues, dl5$decisionCriticalValues, tolerance = 1e-05)
	    expect_equal(dl5CodeBased$reversalProbabilities, dl5$reversalProbabilities, tolerance = 1e-05)
	    expect_type(names(dl5), "character")
	    df <- as.data.frame(dl5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dl5)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	dl6 <- getDesignCharacteristics(dl5)

	## Comparison of the results of TrialDesignCharacteristics object 'dl6' with expected results
	expect_equal(dl6$nFixed, 8.5638474, tolerance = 1e-07)
	expect_equal(dl6$shift, 8.7180222, tolerance = 1e-07)
	expect_equal(dl6$inflationFactor, 1.018003, tolerance = 1e-07)
	expect_equal(dl6$information, c(3.4872089, 5.6667144, 8.7180222), tolerance = 1e-07)
	expect_equal(dl6$power, c(0.15429254, 0.58752252, 0.9), tolerance = 1e-07)
	expect_equal(dl6$rejectionProbabilities, c(0.15429254, 0.43322998, 0.31247748), tolerance = 1e-07)
	expect_equal(dl6$futilityProbabilities, c(0.0099602552, 0.03429374), tolerance = 1e-07)
	expect_equal(dl6$averageSampleNumber1, 0.94451255, tolerance = 1e-07)
	expect_equal(dl6$averageSampleNumber01, 0.96721799, tolerance = 1e-07)
	expect_equal(dl6$averageSampleNumber0, 0.89669187, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dl6), NA)))
	    expect_output(print(dl6)$show())
	    invisible(capture.output(expect_error(summary(dl6), NA)))
	    expect_output(summary(dl6)$show())
	    suppressWarnings(dl6CodeBased <- eval(parse(text = getObjectRCode(dl6, stringWrapParagraphWidth = NULL))))
	    expect_equal(dl6CodeBased$nFixed, dl6$nFixed, tolerance = 1e-05)
	    expect_equal(dl6CodeBased$shift, dl6$shift, tolerance = 1e-05)
	    expect_equal(dl6CodeBased$inflationFactor, dl6$inflationFactor, tolerance = 1e-05)
	    expect_equal(dl6CodeBased$information, dl6$information, tolerance = 1e-05)
	    expect_equal(dl6CodeBased$power, dl6$power, tolerance = 1e-05)
	    expect_equal(dl6CodeBased$rejectionProbabilities, dl6$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(dl6CodeBased$futilityProbabilities, dl6$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(dl6CodeBased$averageSampleNumber1, dl6$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(dl6CodeBased$averageSampleNumber01, dl6$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(dl6CodeBased$averageSampleNumber0, dl6$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(dl6), "character")
	    df <- as.data.frame(dl6)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dl6)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with type of design = 'asP' and 'bsP', non-binding futility bounds and delayed response (kMax = 3)", {

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingOBrienFleming}
	# @refFS[Formula]{fs:betaSpendingApproach}
	# @refFS[Formula]{fs:betaSpendingOBrienFleming}
	# @refFS[Formula]{fs:delayedResponseCondition1}
	# @refFS[Formula]{fs:delayedResponseCondition2}
	# @refFS[Formula]{fs:delayedResponsePower}
	suppressWarnings(dl1 <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.05, beta = 0.1, sided = 1,
	    typeOfDesign = "asP", typeBetaSpending = "bsP",
	    informationRates = c(0.4, 0.65, 1),
	    bindingFutility = FALSE,
	    delayedInformation = c(0.1, 0.2)
	))

	## Comparison of the results of TrialDesignGroupSequential object 'dl1' with expected results
	expect_equal(dl1$power, c(0.58983431, 0.79279807, 0.9), tolerance = 1e-07)
	expect_equal(dl1$futilityBounds, c(0.5448398, 1.0899149), tolerance = 1e-07)
	expect_equal(dl1$alphaSpent, c(0.026156858, 0.037497241, 0.05), tolerance = 1e-07)
	expect_equal(dl1$betaSpent, c(0.052313716, 0.074994481, 0.099999999), tolerance = 1e-07)
	expect_equal(dl1$criticalValues, c(1.9405431, 2.0327662, 1.9734104), tolerance = 1e-07)
	expect_equal(dl1$stageLevels, c(0.026156858, 0.021038075, 0.02422441), tolerance = 1e-07)
	expect_equal(dl1$decisionCriticalValues, c(1.3362296, 1.657468, 1.9734104), tolerance = 1e-07)
	expect_equal(dl1$reversalProbabilities, c(0.0020439695, 0.0026967589), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dl1), NA)))
	    expect_output(print(dl1)$show())
	    invisible(capture.output(expect_error(summary(dl1), NA)))
	    expect_output(summary(dl1)$show())
	    suppressWarnings(dl1CodeBased <- eval(parse(text = getObjectRCode(dl1, stringWrapParagraphWidth = NULL))))
	    expect_equal(dl1CodeBased$power, dl1$power, tolerance = 1e-05)
	    expect_equal(dl1CodeBased$futilityBounds, dl1$futilityBounds, tolerance = 1e-05)
	    expect_equal(dl1CodeBased$alphaSpent, dl1$alphaSpent, tolerance = 1e-05)
	    expect_equal(dl1CodeBased$betaSpent, dl1$betaSpent, tolerance = 1e-05)
	    expect_equal(dl1CodeBased$criticalValues, dl1$criticalValues, tolerance = 1e-05)
	    expect_equal(dl1CodeBased$stageLevels, dl1$stageLevels, tolerance = 1e-05)
	    expect_equal(dl1CodeBased$decisionCriticalValues, dl1$decisionCriticalValues, tolerance = 1e-05)
	    expect_equal(dl1CodeBased$reversalProbabilities, dl1$reversalProbabilities, tolerance = 1e-05)
	    expect_type(names(dl1), "character")
	    df <- as.data.frame(dl1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dl1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	dl2 <- getDesignCharacteristics(dl1)

	## Comparison of the results of TrialDesignCharacteristics object 'dl2' with expected results
	expect_equal(dl2$nFixed, 8.5638474, tolerance = 1e-07)
	expect_equal(dl2$shift, 11.345796, tolerance = 1e-07)
	expect_equal(dl2$inflationFactor, 1.324848, tolerance = 1e-07)
	expect_equal(dl2$information, c(4.5383183, 7.3747672, 11.345796), tolerance = 1e-07)
	expect_equal(dl2$power, c(0.57788702, 0.78847934, 0.9), tolerance = 1e-07)
	expect_equal(dl2$rejectionProbabilities, c(0.57788702, 0.21059232, 0.11152066), tolerance = 1e-07)
	expect_equal(dl2$futilityProbabilities, c(0.056427171, 0.024888086), tolerance = 1e-07)
	expect_equal(dl2$averageSampleNumber1, 0.86088771, tolerance = 1e-07)
	expect_equal(dl2$averageSampleNumber01, 0.9483049, tolerance = 1e-07)
	expect_equal(dl2$averageSampleNumber0, 0.80259202, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dl2), NA)))
	    expect_output(print(dl2)$show())
	    invisible(capture.output(expect_error(summary(dl2), NA)))
	    expect_output(summary(dl2)$show())
	    suppressWarnings(dl2CodeBased <- eval(parse(text = getObjectRCode(dl2, stringWrapParagraphWidth = NULL))))
	    expect_equal(dl2CodeBased$nFixed, dl2$nFixed, tolerance = 1e-05)
	    expect_equal(dl2CodeBased$shift, dl2$shift, tolerance = 1e-05)
	    expect_equal(dl2CodeBased$inflationFactor, dl2$inflationFactor, tolerance = 1e-05)
	    expect_equal(dl2CodeBased$information, dl2$information, tolerance = 1e-05)
	    expect_equal(dl2CodeBased$power, dl2$power, tolerance = 1e-05)
	    expect_equal(dl2CodeBased$rejectionProbabilities, dl2$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(dl2CodeBased$futilityProbabilities, dl2$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(dl2CodeBased$averageSampleNumber1, dl2$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(dl2CodeBased$averageSampleNumber01, dl2$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(dl2CodeBased$averageSampleNumber0, dl2$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(dl2), "character")
	    df <- as.data.frame(dl2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dl2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	suppressWarnings(dl3 <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.05, beta = 0.1, sided = 1,
	    typeOfDesign = "asP", typeBetaSpending = "bsP",
	    informationRates = c(0.4, 0.65, 1),
	    bindingFutility = FALSE,
	    delayedInformation = c(0, 0.2)
	))

	## Comparison of the results of TrialDesignGroupSequential object 'dl3' with expected results
	expect_equal(dl3$power, c(0.58983431, 0.79279807, 0.9), tolerance = 1e-07)
	expect_equal(dl3$futilityBounds, c(0.5448398, 1.0899149), tolerance = 1e-07)
	expect_equal(dl3$alphaSpent, c(0.026156858, 0.037497241, 0.05), tolerance = 1e-07)
	expect_equal(dl3$betaSpent, c(0.052313716, 0.074994481, 0.099999999), tolerance = 1e-07)
	expect_equal(dl3$criticalValues, c(1.9405431, 2.0327662, 1.9734104), tolerance = 1e-07)
	expect_equal(dl3$stageLevels, c(0.026156858, 0.021038075, 0.02422441), tolerance = 1e-07)
	expect_equal(dl3$decisionCriticalValues, c(NA_real_, 1.657468, 1.9734104), tolerance = 1e-07)
	expect_equal(dl3$reversalProbabilities, c(NA_real_, 0.0026967589), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dl3), NA)))
	    expect_output(print(dl3)$show())
	    invisible(capture.output(expect_error(summary(dl3), NA)))
	    expect_output(summary(dl3)$show())
	    suppressWarnings(dl3CodeBased <- eval(parse(text = getObjectRCode(dl3, stringWrapParagraphWidth = NULL))))
	    expect_equal(dl3CodeBased$power, dl3$power, tolerance = 1e-05)
	    expect_equal(dl3CodeBased$futilityBounds, dl3$futilityBounds, tolerance = 1e-05)
	    expect_equal(dl3CodeBased$alphaSpent, dl3$alphaSpent, tolerance = 1e-05)
	    expect_equal(dl3CodeBased$betaSpent, dl3$betaSpent, tolerance = 1e-05)
	    expect_equal(dl3CodeBased$criticalValues, dl3$criticalValues, tolerance = 1e-05)
	    expect_equal(dl3CodeBased$stageLevels, dl3$stageLevels, tolerance = 1e-05)
	    expect_equal(dl3CodeBased$decisionCriticalValues, dl3$decisionCriticalValues, tolerance = 1e-05)
	    expect_equal(dl3CodeBased$reversalProbabilities, dl3$reversalProbabilities, tolerance = 1e-05)
	    expect_type(names(dl3), "character")
	    df <- as.data.frame(dl3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dl3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	dl4 <- getDesignCharacteristics(dl3)

	## Comparison of the results of TrialDesignCharacteristics object 'dl4' with expected results
	expect_equal(dl4$nFixed, 8.5638474, tolerance = 1e-07)
	expect_equal(dl4$shift, 11.462579, tolerance = 1e-07)
	expect_equal(dl4$inflationFactor, 1.3384848, tolerance = 1e-07)
	expect_equal(dl4$information, c(4.5850317, 7.4506765, 11.462579), tolerance = 1e-07)
	expect_equal(dl4$power, c(0.57954342, 0.78973163, 0.9), tolerance = 1e-07)
	expect_equal(dl4$rejectionProbabilities, c(0.57954342, 0.21018821, 0.11026837), tolerance = 1e-07)
	expect_equal(dl4$futilityProbabilities, c(0.055196532, 0.024225352), tolerance = 1e-07)
	expect_equal(dl4$averageSampleNumber1, 0.7829433, tolerance = 1e-07)
	expect_equal(dl4$averageSampleNumber01, 0.89251343, tolerance = 1e-07)
	expect_equal(dl4$averageSampleNumber0, 0.71271214, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dl4), NA)))
	    expect_output(print(dl4)$show())
	    invisible(capture.output(expect_error(summary(dl4), NA)))
	    expect_output(summary(dl4)$show())
	    suppressWarnings(dl4CodeBased <- eval(parse(text = getObjectRCode(dl4, stringWrapParagraphWidth = NULL))))
	    expect_equal(dl4CodeBased$nFixed, dl4$nFixed, tolerance = 1e-05)
	    expect_equal(dl4CodeBased$shift, dl4$shift, tolerance = 1e-05)
	    expect_equal(dl4CodeBased$inflationFactor, dl4$inflationFactor, tolerance = 1e-05)
	    expect_equal(dl4CodeBased$information, dl4$information, tolerance = 1e-05)
	    expect_equal(dl4CodeBased$power, dl4$power, tolerance = 1e-05)
	    expect_equal(dl4CodeBased$rejectionProbabilities, dl4$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(dl4CodeBased$futilityProbabilities, dl4$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(dl4CodeBased$averageSampleNumber1, dl4$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(dl4CodeBased$averageSampleNumber01, dl4$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(dl4CodeBased$averageSampleNumber0, dl4$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(dl4), "character")
	    df <- as.data.frame(dl4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dl4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	expect_warning(
	    dl5 <- getDesignGroupSequential(
	        kMax = 3, alpha = 0.05, beta = 0.1, sided = 1,
	        typeOfDesign = "asP", typeBetaSpending = "bsP",
	        informationRates = c(0.4, 0.65, 1),
	        bindingFutility = FALSE,
	        delayedInformation = 0
	    )
	)

	## Comparison of the results of TrialDesignGroupSequential object 'dl5' with expected results
	expect_equal(dl5$power, c(0.58983431, 0.79279807, 0.9), tolerance = 1e-07)
	expect_equal(dl5$futilityBounds, c(0.5448398, 1.0899149), tolerance = 1e-07)
	expect_equal(dl5$alphaSpent, c(0.026156858, 0.037497241, 0.05), tolerance = 1e-07)
	expect_equal(dl5$betaSpent, c(0.052313716, 0.074994481, 0.099999999), tolerance = 1e-07)
	expect_equal(dl5$criticalValues, c(1.9405431, 2.0327662, 1.9734104), tolerance = 1e-07)
	expect_equal(dl5$stageLevels, c(0.026156858, 0.021038075, 0.02422441), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dl5), NA)))
	    expect_output(print(dl5)$show())
	    invisible(capture.output(expect_error(summary(dl5), NA)))
	    expect_output(summary(dl5)$show())
	    suppressWarnings(dl5CodeBased <- eval(parse(text = getObjectRCode(dl5, stringWrapParagraphWidth = NULL))))
	    expect_equal(dl5CodeBased$power, dl5$power, tolerance = 1e-05)
	    expect_equal(dl5CodeBased$futilityBounds, dl5$futilityBounds, tolerance = 1e-05)
	    expect_equal(dl5CodeBased$alphaSpent, dl5$alphaSpent, tolerance = 1e-05)
	    expect_equal(dl5CodeBased$betaSpent, dl5$betaSpent, tolerance = 1e-05)
	    expect_equal(dl5CodeBased$criticalValues, dl5$criticalValues, tolerance = 1e-05)
	    expect_equal(dl5CodeBased$stageLevels, dl5$stageLevels, tolerance = 1e-05)
	    expect_type(names(dl5), "character")
	    df <- as.data.frame(dl5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dl5)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	dl6 <- getDesignCharacteristics(dl5)

	## Comparison of the results of TrialDesignCharacteristics object 'dl6' with expected results
	expect_equal(dl6$nFixed, 8.5638474, tolerance = 1e-07)
	expect_equal(dl6$shift, 11.746896, tolerance = 1e-07)
	expect_equal(dl6$inflationFactor, 1.3716844, tolerance = 1e-07)
	expect_equal(dl6$information, c(4.6987583, 7.6354822, 11.746896), tolerance = 1e-07)
	expect_equal(dl6$power, c(0.58983431, 0.79279807, 0.9), tolerance = 1e-07)
	expect_equal(dl6$rejectionProbabilities, c(0.58983431, 0.20296375, 0.10720193), tolerance = 1e-07)
	expect_equal(dl6$futilityProbabilities, c(0.052313716, 0.022680765), tolerance = 1e-07)
	expect_equal(dl6$averageSampleNumber1, 0.73486016, tolerance = 1e-07)
	expect_equal(dl6$averageSampleNumber01, 0.8455149, tolerance = 1e-07)
	expect_equal(dl6$averageSampleNumber0, 0.67993383, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(dl6), NA)))
	    expect_output(print(dl6)$show())
	    invisible(capture.output(expect_error(summary(dl6), NA)))
	    expect_output(summary(dl6)$show())
	    suppressWarnings(dl6CodeBased <- eval(parse(text = getObjectRCode(dl6, stringWrapParagraphWidth = NULL))))
	    expect_equal(dl6CodeBased$nFixed, dl6$nFixed, tolerance = 1e-05)
	    expect_equal(dl6CodeBased$shift, dl6$shift, tolerance = 1e-05)
	    expect_equal(dl6CodeBased$inflationFactor, dl6$inflationFactor, tolerance = 1e-05)
	    expect_equal(dl6CodeBased$information, dl6$information, tolerance = 1e-05)
	    expect_equal(dl6CodeBased$power, dl6$power, tolerance = 1e-05)
	    expect_equal(dl6CodeBased$rejectionProbabilities, dl6$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(dl6CodeBased$futilityProbabilities, dl6$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(dl6CodeBased$averageSampleNumber1, dl6$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(dl6CodeBased$averageSampleNumber01, dl6$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(dl6CodeBased$averageSampleNumber0, dl6$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(dl6), "character")
	    df <- as.data.frame(dl6)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(dl6)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with binding futility bounds", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:criticalValuesWithFutility}
	# @refFS[Formula]{fs:criticalValuesWangTiatis}
	x8a <- getDesignGroupSequential(
	    kMax = 4, alpha = 0.025, futilityBounds = rep(0.5244, 3),
	    bindingFutility = TRUE, typeOfDesign = "WT", deltaWT = 0.4
	)

	## Comparison of the results of TrialDesignGroupSequential object 'x8a' with expected results
	expect_equal(x8a$alphaSpent, c(0.0062828133, 0.013876673, 0.02015684, 0.02499999), tolerance = 1e-07)
	expect_equal(x8a$criticalValues, c(2.4958485, 2.328709, 2.2361766, 2.1727623), tolerance = 1e-07)
	expect_equal(x8a$stageLevels, c(0.0062828133, 0.0099372444, 0.012670104, 0.014899106), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x8a), NA)))
	    expect_output(print(x8a)$show())
	    invisible(capture.output(expect_error(summary(x8a), NA)))
	    expect_output(summary(x8a)$show())
	    x8aCodeBased <- eval(parse(text = getObjectRCode(x8a, stringWrapParagraphWidth = NULL)))
	    expect_equal(x8aCodeBased$alphaSpent, x8a$alphaSpent, tolerance = 1e-05)
	    expect_equal(x8aCodeBased$criticalValues, x8a$criticalValues, tolerance = 1e-05)
	    expect_equal(x8aCodeBased$stageLevels, x8a$stageLevels, tolerance = 1e-05)
	    expect_type(names(x8a), "character")
	    df <- as.data.frame(x8a)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x8a)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:criticalValuesWangTiatis}
	x8b <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.025, sided = 2, informationRates = c(0.3, 0.8, 1),
	    typeOfDesign = "WT", deltaWT = 0.24
	)

	## Comparison of the results of TrialDesignGroupSequential object 'x8b' with expected results
	expect_equal(x8b$alphaSpent, c(0.0013603353, 0.013978861, 0.02499999), tolerance = 1e-07)
	expect_equal(x8b$criticalValues, c(3.2029374, 2.4819703, 2.3420706), tolerance = 1e-07)
	expect_equal(x8b$stageLevels, c(0.00068016766, 0.0065329078, 0.0095885436), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x8b), NA)))
	    expect_output(print(x8b)$show())
	    invisible(capture.output(expect_error(summary(x8b), NA)))
	    expect_output(summary(x8b)$show())
	    x8bCodeBased <- eval(parse(text = getObjectRCode(x8b, stringWrapParagraphWidth = NULL)))
	    expect_equal(x8bCodeBased$alphaSpent, x8b$alphaSpent, tolerance = 1e-05)
	    expect_equal(x8bCodeBased$criticalValues, x8b$criticalValues, tolerance = 1e-05)
	    expect_equal(x8bCodeBased$stageLevels, x8b$stageLevels, tolerance = 1e-05)
	    expect_type(names(x8b), "character")
	    df <- as.data.frame(x8b)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x8b)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:criticalValuesWangTiatis}
	x8c <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.025, sided = 1, informationRates = c(0.3, 0.8, 1),
	    typeOfDesign = "WToptimum", beta = 0.23
	)

	## Comparison of the results of TrialDesignGroupSequential object 'x8c' with expected results
	expect_equal(x8c$power, c(0.17785982, 0.63184407, 0.77), tolerance = 1e-07)
	expect_equal(x8c$deltaWT, 0.393, tolerance = 1e-07)
	expect_equal(x8c$alphaSpent, c(0.0067542296, 0.01805085, 0.025), tolerance = 1e-07)
	expect_equal(x8c$criticalValues, c(2.4700754, 2.2239834, 2.1715117), tolerance = 1e-07)
	expect_equal(x8c$stageLevels, c(0.0067542296, 0.013074779, 0.014946256), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x8c), NA)))
	    expect_output(print(x8c)$show())
	    invisible(capture.output(expect_error(summary(x8c), NA)))
	    expect_output(summary(x8c)$show())
	    x8cCodeBased <- eval(parse(text = getObjectRCode(x8c, stringWrapParagraphWidth = NULL)))
	    expect_equal(x8cCodeBased$power, x8c$power, tolerance = 1e-05)
	    expect_equal(x8cCodeBased$deltaWT, x8c$deltaWT, tolerance = 1e-05)
	    expect_equal(x8cCodeBased$alphaSpent, x8c$alphaSpent, tolerance = 1e-05)
	    expect_equal(x8cCodeBased$criticalValues, x8c$criticalValues, tolerance = 1e-05)
	    expect_equal(x8cCodeBased$stageLevels, x8c$stageLevels, tolerance = 1e-05)
	    expect_type(names(x8c), "character")
	    df <- as.data.frame(x8c)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x8c)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:criticalValuesWangTiatis}
	x8d <- getDesignGroupSequential(
	    kMax = 4, alpha = 0.025, sided = 2, informationRates = c(0.3, 0.6, 0.8, 1),
	    typeOfDesign = "WToptimum", beta = 0.1, optimizationCriterion = "ASNH1"
	)

	## Comparison of the results of TrialDesignGroupSequential object 'x8d' with expected results
	expect_equal(x8d$power, c(0.27905065, 0.63899817, 0.80432197, 0.9), tolerance = 1e-07)
	expect_equal(x8d$deltaWT, 0.479, tolerance = 1e-07)
	expect_equal(x8d$alphaSpent, c(0.0082066211, 0.015417447, 0.020576899, 0.025), tolerance = 1e-07)
	expect_equal(x8d$criticalValues, c(2.6434487, 2.6052491, 2.5895574, 2.577451), tolerance = 1e-07)
	expect_equal(x8d$stageLevels, c(0.0041033106, 0.0045903747, 0.0048049705, 0.0049765989), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x8d), NA)))
	    expect_output(print(x8d)$show())
	    invisible(capture.output(expect_error(summary(x8d), NA)))
	    expect_output(summary(x8d)$show())
	    x8dCodeBased <- eval(parse(text = getObjectRCode(x8d, stringWrapParagraphWidth = NULL)))
	    expect_equal(x8dCodeBased$power, x8d$power, tolerance = 1e-05)
	    expect_equal(x8dCodeBased$deltaWT, x8d$deltaWT, tolerance = 1e-05)
	    expect_equal(x8dCodeBased$alphaSpent, x8d$alphaSpent, tolerance = 1e-05)
	    expect_equal(x8dCodeBased$criticalValues, x8d$criticalValues, tolerance = 1e-05)
	    expect_equal(x8dCodeBased$stageLevels, x8d$stageLevels, tolerance = 1e-05)
	    expect_type(names(x8d), "character")
	    df <- as.data.frame(x8d)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x8d)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:criticalValuesWangTiatis}
	x8e <- getDesignGroupSequential(
	    kMax = 4, alpha = 0.025, sided = 2, informationRates = c(0.3, 0.6, 0.8, 1),
	    typeOfDesign = "WToptimum", beta = 0.1, optimizationCriterion = "ASNsum"
	)

	## Comparison of the results of TrialDesignGroupSequential object 'x8e' with expected results
	expect_equal(x8e$power, c(0.068425642, 0.50677837, 0.76253381, 0.9), tolerance = 1e-07)
	expect_equal(x8e$deltaWT, 0.181, tolerance = 1e-07)
	expect_equal(x8e$alphaSpent, c(0.00055484217, 0.0059655413, 0.01417086, 0.02499999), tolerance = 1e-07)
	expect_equal(x8e$criticalValues, c(3.4527796, 2.7678356, 2.5251363, 2.3516384), tolerance = 1e-07)
	expect_equal(x8e$stageLevels, c(0.00027742108, 0.0028214959, 0.0057826708, 0.0093454685), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x8e), NA)))
	    expect_output(print(x8e)$show())
	    invisible(capture.output(expect_error(summary(x8e), NA)))
	    expect_output(summary(x8e)$show())
	    x8eCodeBased <- eval(parse(text = getObjectRCode(x8e, stringWrapParagraphWidth = NULL)))
	    expect_equal(x8eCodeBased$power, x8e$power, tolerance = 1e-05)
	    expect_equal(x8eCodeBased$deltaWT, x8e$deltaWT, tolerance = 1e-05)
	    expect_equal(x8eCodeBased$alphaSpent, x8e$alphaSpent, tolerance = 1e-05)
	    expect_equal(x8eCodeBased$criticalValues, x8e$criticalValues, tolerance = 1e-05)
	    expect_equal(x8eCodeBased$stageLevels, x8e$stageLevels, tolerance = 1e-05)
	    expect_type(names(x8e), "character")
	    df <- as.data.frame(x8e)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x8e)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with Haybittle Peto boundaries", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:criticalValuesHaybittlePeto}
	x9 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, typeOfDesign = "HP")

	## Comparison of the results of TrialDesignGroupSequential object 'x9' with expected results
	expect_equal(x9$alphaSpent, c(0.001349898, 0.0024617416, 0.0033695882, 0.025), tolerance = 1e-07)
	expect_equal(x9$criticalValues, c(3, 3, 3, 1.9827514), tolerance = 1e-07)
	expect_equal(x9$stageLevels, c(0.001349898, 0.001349898, 0.001349898, 0.023697604), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x9), NA)))
	    expect_output(print(x9)$show())
	    invisible(capture.output(expect_error(summary(x9), NA)))
	    expect_output(summary(x9)$show())
	    x9CodeBased <- eval(parse(text = getObjectRCode(x9, stringWrapParagraphWidth = NULL)))
	    expect_equal(x9CodeBased$alphaSpent, x9$alphaSpent, tolerance = 1e-05)
	    expect_equal(x9CodeBased$criticalValues, x9$criticalValues, tolerance = 1e-05)
	    expect_equal(x9CodeBased$stageLevels, x9$stageLevels, tolerance = 1e-05)
	    expect_type(names(x9), "character")
	    df <- as.data.frame(x9)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x9)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with Pampallona Tsiatis boundaries, binding and non-binding futility bounds", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:criticalValuesPampallonaTiatis}
	x10 <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.035, beta = 0.1,
	    informationRates = c(0.3, 0.8, 1), typeOfDesign = "PT", sided = 1,
	    bindingFutility = TRUE, deltaPT1 = 0.2, deltaPT0 = 0.3
	)

	## Comparison of the results of TrialDesignGroupSequential object 'x10' with expected results
	expect_equal(x10$power, c(0.19834666, 0.83001122, 0.9), tolerance = 1e-07)
	expect_equal(x10$futilityBounds, c(-0.042079545, 1.4407359), tolerance = 1e-07)
	expect_equal(x10$alphaSpent, c(0.0038332428, 0.024917169, 0.035), tolerance = 1e-07)
	expect_equal(x10$betaSpent, c(0.031375368, 0.080734151, 0.1), tolerance = 1e-07)
	expect_equal(x10$criticalValues, c(2.6664156, 1.9867225, 1.8580792), tolerance = 1e-07)
	expect_equal(x10$stageLevels, c(0.0038332428, 0.023476576, 0.031578886), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x10), NA)))
	    expect_output(print(x10)$show())
	    invisible(capture.output(expect_error(summary(x10), NA)))
	    expect_output(summary(x10)$show())
	    x10CodeBased <- eval(parse(text = getObjectRCode(x10, stringWrapParagraphWidth = NULL)))
	    expect_equal(x10CodeBased$power, x10$power, tolerance = 1e-05)
	    expect_equal(x10CodeBased$futilityBounds, x10$futilityBounds, tolerance = 1e-05)
	    expect_equal(x10CodeBased$alphaSpent, x10$alphaSpent, tolerance = 1e-05)
	    expect_equal(x10CodeBased$betaSpent, x10$betaSpent, tolerance = 1e-05)
	    expect_equal(x10CodeBased$criticalValues, x10$criticalValues, tolerance = 1e-05)
	    expect_equal(x10CodeBased$stageLevels, x10$stageLevels, tolerance = 1e-05)
	    expect_type(names(x10), "character")
	    df <- as.data.frame(x10)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x10)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:criticalValuesPampallonaTiatis}
	x11 <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.035, beta = 0.05,
	    informationRates = c(0.3, 0.8, 1), typeOfDesign = "PT", sided = 2,
	    bindingFutility = TRUE, deltaPT1 = 0.2, deltaPT0 = 0.3
	)

	## Comparison of the results of TrialDesignGroupSequential object 'x11' with expected results
	expect_equal(x11$power, c(0.16615376, 0.88013007, 0.94999991), tolerance = 1e-07)
	expect_equal(x11$futilityBounds, c(NA_real_, 1.671433), tolerance = 1e-07)
	expect_equal(x11$alphaSpent, c(0.0019236202, 0.022017713, 0.035), tolerance = 1e-07)
	expect_equal(x11$betaSpent, c(0, 0.035025978, 0.05), tolerance = 1e-07)
	expect_equal(x11$criticalValues, c(3.1017782, 2.3111074, 2.1614596), tolerance = 1e-07)
	expect_equal(x11$stageLevels, c(0.00096181012, 0.010413463, 0.015329928), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x11), NA)))
	    expect_output(print(x11)$show())
	    invisible(capture.output(expect_error(summary(x11), NA)))
	    expect_output(summary(x11)$show())
	    x11CodeBased <- eval(parse(text = getObjectRCode(x11, stringWrapParagraphWidth = NULL)))
	    expect_equal(x11CodeBased$power, x11$power, tolerance = 1e-05)
	    expect_equal(x11CodeBased$futilityBounds, x11$futilityBounds, tolerance = 1e-05)
	    expect_equal(x11CodeBased$alphaSpent, x11$alphaSpent, tolerance = 1e-05)
	    expect_equal(x11CodeBased$betaSpent, x11$betaSpent, tolerance = 1e-05)
	    expect_equal(x11CodeBased$criticalValues, x11$criticalValues, tolerance = 1e-05)
	    expect_equal(x11CodeBased$stageLevels, x11$stageLevels, tolerance = 1e-05)
	    expect_type(names(x11), "character")
	    df <- as.data.frame(x11)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x11)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:criticalValuesPampallonaTiatis}
	x12 <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.035, beta = 0.05,
	    informationRates = c(0.3, 0.8, 1), typeOfDesign = "PT", sided = 2,
	    bindingFutility = FALSE, deltaPT1 = 0.2, deltaPT0 = 0.3
	)

	## Comparison of the results of TrialDesignGroupSequential object 'x12' with expected results
	expect_equal(x12$power, c(0.15712278, 0.87874666, 0.94999994), tolerance = 1e-07)
	expect_equal(x12$futilityBounds, c(NA_real_, 1.7090472), tolerance = 1e-07)
	expect_equal(x12$alphaSpent, c(0.0015647742, 0.019435851, 0.035), tolerance = 1e-07)
	expect_equal(x12$betaSpent, c(0, 0.034947415, 0.05), tolerance = 1e-07)
	expect_equal(x12$criticalValues, c(3.1623945, 2.356272, 2.2036998), tolerance = 1e-07)
	expect_equal(x12$stageLevels, c(0.00078238709, 0.0092296971, 0.013772733), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x12), NA)))
	    expect_output(print(x12)$show())
	    invisible(capture.output(expect_error(summary(x12), NA)))
	    expect_output(summary(x12)$show())
	    x12CodeBased <- eval(parse(text = getObjectRCode(x12, stringWrapParagraphWidth = NULL)))
	    expect_equal(x12CodeBased$power, x12$power, tolerance = 1e-05)
	    expect_equal(x12CodeBased$futilityBounds, x12$futilityBounds, tolerance = 1e-05)
	    expect_equal(x12CodeBased$alphaSpent, x12$alphaSpent, tolerance = 1e-05)
	    expect_equal(x12CodeBased$betaSpent, x12$betaSpent, tolerance = 1e-05)
	    expect_equal(x12CodeBased$criticalValues, x12$criticalValues, tolerance = 1e-05)
	    expect_equal(x12CodeBased$stageLevels, x12$stageLevels, tolerance = 1e-05)
	    expect_type(names(x12), "character")
	    df <- as.data.frame(x12)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x12)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:criticalValuesPampallonaTiatis}
	x13 <- getDesignGroupSequential(
	    kMax = 4, alpha = 0.035, beta = 0.05,
	    informationRates = c(0.2, 0.4, 0.8, 1), typeOfDesign = "PT", sided = 1,
	    bindingFutility = FALSE, deltaPT1 = 0.1, deltaPT0 = 0.45
	)

	## Comparison of the results of TrialDesignGroupSequential object 'x13' with expected results
	expect_equal(x13$power, c(0.029518378, 0.38853658, 0.90760886, 0.95), tolerance = 1e-07)
	expect_equal(x13$futilityBounds, c(-0.41499566, 0.38106631, 1.4738957), tolerance = 1e-07)
	expect_equal(x13$alphaSpent, c(0.00014050218, 0.0030266381, 0.0199021, 0.035), tolerance = 1e-07)
	expect_equal(x13$betaSpent, c(0.015413989, 0.028721092, 0.043215976, 0.049999999), tolerance = 1e-07)
	expect_equal(x13$criticalValues, c(3.6322099, 2.7527004, 2.0861568, 1.9080201), tolerance = 1e-07)
	expect_equal(x13$stageLevels, c(0.00014050218, 0.002955298, 0.018482211, 0.02819431), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x13), NA)))
	    expect_output(print(x13)$show())
	    invisible(capture.output(expect_error(summary(x13), NA)))
	    expect_output(summary(x13)$show())
	    x13CodeBased <- eval(parse(text = getObjectRCode(x13, stringWrapParagraphWidth = NULL)))
	    expect_equal(x13CodeBased$power, x13$power, tolerance = 1e-05)
	    expect_equal(x13CodeBased$futilityBounds, x13$futilityBounds, tolerance = 1e-05)
	    expect_equal(x13CodeBased$alphaSpent, x13$alphaSpent, tolerance = 1e-05)
	    expect_equal(x13CodeBased$betaSpent, x13$betaSpent, tolerance = 1e-05)
	    expect_equal(x13CodeBased$criticalValues, x13$criticalValues, tolerance = 1e-05)
	    expect_equal(x13CodeBased$stageLevels, x13$stageLevels, tolerance = 1e-05)
	    expect_type(names(x13), "character")
	    df <- as.data.frame(x13)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x13)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:criticalValuesPampallonaTiatis}
	x14 <- getDesignGroupSequential(
	    kMax = 6, alpha = 0.25, beta = 0.01,
	    typeOfDesign = "PT", sided = 2,
	    bindingFutility = TRUE, deltaPT1 = 0.02, deltaPT0 = 0.49, twoSidedPower = TRUE
	)

	## Comparison of the results of TrialDesignGroupSequential object 'x14' with expected results
	expect_equal(x14$power, c(0.076493626, 0.52863814, 0.83456395, 0.94950066, 0.98346861, 0.99), tolerance = 1e-07)
	expect_equal(x14$futilityBounds, c(NA_real_, NA_real_, 0.12661836, 0.55308248, 0.92800873), tolerance = 1e-07)
	expect_equal(x14$alphaSpent, c(0.0027626806, 0.03301126, 0.088857236, 0.15440485, 0.2156594, 0.25), tolerance = 1e-07)
	expect_equal(x14$betaSpent, c(0, 0, 0.0026196847, 0.0066701045, 0.008949341, 0.01), tolerance = 1e-07)
	expect_equal(x14$criticalValues, c(2.9929798, 2.1458995, 1.7663859, 1.5385619, 1.3822869, 1.2664591), tolerance = 1e-07)
	expect_equal(x14$stageLevels, c(0.0013813403, 0.015940498, 0.038665568, 0.061955638, 0.08344182, 0.10267438), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x14), NA)))
	    expect_output(print(x14)$show())
	    invisible(capture.output(expect_error(summary(x14), NA)))
	    expect_output(summary(x14)$show())
	    x14CodeBased <- eval(parse(text = getObjectRCode(x14, stringWrapParagraphWidth = NULL)))
	    expect_equal(x14CodeBased$power, x14$power, tolerance = 1e-05)
	    expect_equal(x14CodeBased$futilityBounds, x14$futilityBounds, tolerance = 1e-05)
	    expect_equal(x14CodeBased$alphaSpent, x14$alphaSpent, tolerance = 1e-05)
	    expect_equal(x14CodeBased$betaSpent, x14$betaSpent, tolerance = 1e-05)
	    expect_equal(x14CodeBased$criticalValues, x14$criticalValues, tolerance = 1e-05)
	    expect_equal(x14CodeBased$stageLevels, x14$stageLevels, tolerance = 1e-05)
	    expect_type(names(x14), "character")
	    df <- as.data.frame(x14)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x14)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with type of design = 'noEarlyEfficacy'", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getDesignInverseNormal}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	x15 <- getDesignGroupSequential(
	    typeOfDesign = "noEarlyEfficacy",
	    futilityBounds = c(0, 0.5)
	)


	## Comparison of the results of TrialDesignGroupSequential object 'x15' with expected results
	expect_equal(x15$alphaSpent, c(6.6613381e-16, -1.3145041e-13, 0.025), tolerance = 1e-07)
	expect_equal(x15$criticalValues, c(Inf, Inf, 1.959964), tolerance = 1e-07)
	expect_equal(x15$stageLevels, c(0, 0, 0.025), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x15), NA)))
	    expect_output(print(x15)$show())
	    invisible(capture.output(expect_error(summary(x15), NA)))
	    expect_output(summary(x15)$show())
	    x15CodeBased <- eval(parse(text = getObjectRCode(x15, stringWrapParagraphWidth = NULL)))
	    expect_equal(x15CodeBased$alphaSpent, x15$alphaSpent, tolerance = 1e-05)
	    expect_equal(x15CodeBased$criticalValues, x15$criticalValues, tolerance = 1e-05)
	    expect_equal(x15CodeBased$stageLevels, x15$stageLevels, tolerance = 1e-05)
	    expect_type(names(x15), "character")
	    df <- as.data.frame(x15)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x15)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
	x16 <- getDesignGroupSequential(
	    typeOfDesign = "noEarlyEfficacy",
	    futilityBounds = c(0, 0.5, 1),
	    bindingFutility = TRUE
	)


	## Comparison of the results of TrialDesignGroupSequential object 'x16' with expected results
	expect_equal(x16$alphaSpent, c(6.6613381e-16, 1.110223e-15, 4.8067383e-11, 0.02499999), tolerance = 1e-07)
	expect_equal(x16$criticalValues, c(Inf, Inf, Inf, 1.8848634), tolerance = 1e-07)
	expect_equal(x16$stageLevels, c(0, 0, 0, 0.029724142), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x16), NA)))
	    expect_output(print(x16)$show())
	    invisible(capture.output(expect_error(summary(x16), NA)))
	    expect_output(summary(x16)$show())
	    x16CodeBased <- eval(parse(text = getObjectRCode(x16, stringWrapParagraphWidth = NULL)))
	    expect_equal(x16CodeBased$alphaSpent, x16$alphaSpent, tolerance = 1e-05)
	    expect_equal(x16CodeBased$criticalValues, x16$criticalValues, tolerance = 1e-05)
	    expect_equal(x16CodeBased$stageLevels, x16$stageLevels, tolerance = 1e-05)
	    expect_type(names(x16), "character")
	    df <- as.data.frame(x16)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x16)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
})

test_that("'getDesignInverseNormal': illegal arguments throw exceptions as expected", {

	expect_error(getDesignInverseNormal(
	    typeOfDesign = C_TYPE_OF_DESIGN_AS_USER,
	    userAlphaSpending = c(0.01, 0.02, 0.023, 0.023, 0.025), kMax = 4
	),
	paste0(
	    "Conflicting arguments: length of 'userAlphaSpending' (5) ",
	    "must be equal to 'kMax' (4)"
	),
	fixed = TRUE
	)

	expect_error(getDesignInverseNormal(
	    typeOfDesign = C_TYPE_OF_DESIGN_AS_USER,
	    userAlphaSpending = c(0.01, 0.02, 0.023, 0.023, 0.021)
	),
	paste0(
	    "'userAlphaSpending' = c(0.01, 0.02, 0.023, 0.023, 0.021) must be a vector that ",
	    "satisfies the following condition: 0 <= alpha_1 <= .. <= alpha_5 <= alpha = 0.021"
	),
	fixed = TRUE
	)

	expect_error(getDesignInverseNormal(
	    typeOfDesign = "asUser",
	    userAlphaSpending = c(0.01, 0.02, 0.023), alpha = 0.02
	),
	paste0(
	    "'userAlphaSpending' = c(0.01, 0.02, 0.023) must be a vector that ",
	    "satisfies the following condition: 0 <= alpha_1 <= .. <= alpha_3 <= alpha = 0.02"
	),
	fixed = TRUE
	)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_WT, deltaWT = NA_real_),
	    "Missing argument: parameter 'deltaWT' must be specified in design",
	    fixed = TRUE
	)

	expect_error(getDesignInverseNormal(
	    typeOfDesign = C_TYPE_OF_DESIGN_WT_OPTIMUM,
	    optimizationCriterion = "x"
	),
	"Illegal argument: optimization criterion must be one of the following: 'ASNH1', 'ASNIFH1', 'ASNsum'",
	fixed = TRUE
	)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_AS_KD, gammaA = NA_real_),
	    "Missing argument: parameter 'gammaA' must be specified in design",
	    fixed = TRUE
	)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_AS_HSD, gammaA = NA_real_),
	    "Missing argument: parameter 'gammaA' must be specified in design",
	    fixed = TRUE
	)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER),
	    "Missing argument: parameter 'userAlphaSpending' must be specified in design",
	    fixed = TRUE
	)

	expect_error(getDesignInverseNormal(
	    typeOfDesign = C_TYPE_OF_DESIGN_AS_USER,
	    userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = "x"
	),
	"Illegal argument: type of beta spending must be one of the following: 'none', 'bsP', 'bsOF', 'bsKD', 'bsHSD', 'bsUser'",
	fixed = TRUE
	)

	expect_error(getDesignInverseNormal(
	    typeOfDesign = C_TYPE_OF_DESIGN_AS_USER,
	    userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER
	),
	"Missing argument: parameter 'userBetaSpending' must be specified in design",
	fixed = TRUE
	)

	expect_error(getDesignInverseNormal(
	    typeOfDesign = C_TYPE_OF_DESIGN_AS_USER,
	    userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER,
	    userBetaSpending = c(0.1, 0.2)
	),
	paste0(
	    "Conflicting arguments: length of 'userBetaSpending' (2) must ",
	    "be equal to length of 'informationRates' (3)"
	),
	fixed = TRUE
	)

	expect_error(getDesignInverseNormal(
	    typeOfDesign = C_TYPE_OF_DESIGN_AS_USER,
	    userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER,
	    userBetaSpending = c(0.2, 0.1, 0.05)
	),
	paste0(
	    "'userBetaSpending' = c(0.2, 0.1, 0.05) must be a vector that satisfies the ",
	    "following condition: 0 <= beta_1 <= .. <= beta_3 <= beta = 0.05"
	),
	fixed = TRUE
	)

	expect_error(getDesignInverseNormal(
	    typeOfDesign = C_TYPE_OF_DESIGN_AS_USER,
	    userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER,
	    userBetaSpending = c(0.1, 0.2, 0.3), beta = 0.2
	),
	paste0(
	    "'userBetaSpending' = c(0.1, 0.2, 0.3) must be a vector that satisfies the ",
	    "following condition: 0 <= beta_1 <= .. <= beta_3 <= beta = 0.2"
	),
	fixed = TRUE
	)

	expect_error(getDesignInverseNormal(kMax = Inf),
	    paste0(
	        "Argument out of bounds: 'kMax' (Inf) is out of bounds [1; ",
	        C_KMAX_UPPER_BOUND, "]"
	    ),
	    fixed = TRUE
	)

	expect_error(getDesignInverseNormal(kMax = -Inf),
	    paste0(
	        "Argument out of bounds: 'kMax' (-Inf) is out of bounds [1; ",
	        C_KMAX_UPPER_BOUND, "]"
	    ),
	    fixed = TRUE
	)

	expect_error(getDesignInverseNormal(kMax = -Inf), "Argument out of bounds: 'kMax' (-Inf) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -10), "Argument out of bounds: 'kMax' (-10) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -9), "Argument out of bounds: 'kMax' (-9) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -8), "Argument out of bounds: 'kMax' (-8) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -7), "Argument out of bounds: 'kMax' (-7) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -6), "Argument out of bounds: 'kMax' (-6) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -5), "Argument out of bounds: 'kMax' (-5) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -4), "Argument out of bounds: 'kMax' (-4) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -3), "Argument out of bounds: 'kMax' (-3) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -2), "Argument out of bounds: 'kMax' (-2) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -1), "Argument out of bounds: 'kMax' (-1) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 0), "Argument out of bounds: 'kMax' (0) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 21), "Argument out of bounds: 'kMax' (21) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 22), "Argument out of bounds: 'kMax' (22) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 23), "Argument out of bounds: 'kMax' (23) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 24), "Argument out of bounds: 'kMax' (24) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 25), "Argument out of bounds: 'kMax' (25) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 26), "Argument out of bounds: 'kMax' (26) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 27), "Argument out of bounds: 'kMax' (27) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 28), "Argument out of bounds: 'kMax' (28) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 29), "Argument out of bounds: 'kMax' (29) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 30), "Argument out of bounds: 'kMax' (30) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = Inf), "Argument out of bounds: 'kMax' (Inf) is out of bounds [1; 20]", fixed = TRUE)

	expect_error(getDesignInverseNormal(kMax = 2, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (2) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 3, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (3) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 4, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (4) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 6, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (6) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 7, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (7) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 8, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (8) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 9, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (9) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 10, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (10) - 1", fixed = TRUE)

	expect_warning(expect_error(getDesignInverseNormal(kMax = 11, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (11) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 12, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (12) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 13, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (13) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 14, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (14) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 15, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (15) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 16, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (16) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 17, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (17) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 18, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (18) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 19, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (19) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 20, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (20) - 1", fixed = TRUE))

	expect_error(getDesignInverseNormal(futilityBounds = c(-7, 5)),
	    "Illegal argument: 'futilityBounds' (-7, 5) too extreme for this situation",
	    fixed = TRUE
	)

	expect_error(getDesignInverseNormal(futilityBounds = c(1, 7)),
	    "Argument out of bounds: 'futilityBounds' (1, 7) is out of bounds [-Inf; 6]",
	    fixed = TRUE
	)

})

test_that("'getDesignGroupSequential': illegal arguments throw exceptions as expected", {

	expect_error(getDesignGroupSequential(
	    typeOfDesign = C_TYPE_OF_DESIGN_AS_USER,
	    userAlphaSpending = c(0.01, 0.02, 0.023, 0.023, 0.025), kMax = 4
	),
	paste0(
	    "Conflicting arguments: length of 'userAlphaSpending' (5) ",
	    "must be equal to 'kMax' (4)"
	),
	fixed = TRUE
	)

	expect_error(getDesignGroupSequential(
	    typeOfDesign = C_TYPE_OF_DESIGN_AS_USER,
	    userAlphaSpending = c(0.01, 0.02, 0.023, 0.023, 0.021)
	),
	paste0(
	    "'userAlphaSpending' = c(0.01, 0.02, 0.023, 0.023, 0.021) must be a vector that ",
	    "satisfies the following condition: 0 <= alpha_1 <= .. <= alpha_5 <= alpha = 0.021"
	),
	fixed = TRUE
	)

	expect_error(getDesignGroupSequential(
	    typeOfDesign = "asUser",
	    userAlphaSpending = c(0.01, 0.02, 0.023), alpha = 0.02
	),
	paste0(
	    "'userAlphaSpending' = c(0.01, 0.02, 0.023) must be a vector that ",
	    "satisfies the following condition: 0 <= alpha_1 <= .. <= alpha_3 <= alpha = 0.02"
	),
	fixed = TRUE
	)

	expect_equal(getDesignGroupSequential(
	    typeOfDesign = "asUser",
	    userAlphaSpending = c(0.01, 0.02, 0.023)
	)$alpha, 0.023)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_WT, deltaWT = NA_real_),
	    "Missing argument: parameter 'deltaWT' must be specified in design",
	    fixed = TRUE
	)

	expect_error(getDesignGroupSequential(
	    typeOfDesign = C_TYPE_OF_DESIGN_WT_OPTIMUM,
	    optimizationCriterion = "x"
	),
	"Illegal argument: optimization criterion must be one of the following: 'ASNH1', 'ASNIFH1', 'ASNsum'",
	fixed = TRUE
	)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_AS_KD, gammaA = NA_real_),
	    "Missing argument: parameter 'gammaA' must be specified in design",
	    fixed = TRUE
	)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_AS_HSD, gammaA = NA_real_),
	    "Missing argument: parameter 'gammaA' must be specified in design",
	    fixed = TRUE
	)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER),
	    "Missing argument: parameter 'userAlphaSpending' must be specified in design",
	    fixed = TRUE
	)

	expect_error(getDesignGroupSequential(
	    typeOfDesign = C_TYPE_OF_DESIGN_AS_USER,
	    userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = "x"
	),
	paste0(
	    "Illegal argument: type of beta spending must be one of the following: ",
	    "'none', 'bsP', 'bsOF', 'bsKD', 'bsHSD', 'bsUser'"
	),
	fixed = TRUE
	)

	expect_error(getDesignGroupSequential(
	    typeOfDesign = C_TYPE_OF_DESIGN_AS_USER,
	    userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER
	),
	"Missing argument: parameter 'userBetaSpending' must be specified in design",
	fixed = TRUE
	)

	expect_error(getDesignGroupSequential(
	    typeOfDesign = C_TYPE_OF_DESIGN_AS_USER,
	    userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER,
	    userBetaSpending = c(0.1, 0.2)
	),
	paste0(
	    "Conflicting arguments: length of 'userBetaSpending' (2) must ",
	    "be equal to length of 'informationRates' (3)"
	),
	fixed = TRUE
	)

	expect_error(getDesignGroupSequential(
	    typeOfDesign = C_TYPE_OF_DESIGN_AS_USER,
	    userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER,
	    userBetaSpending = c(0.2, 0.1, 0.05)
	),
	paste0(
	    "'userBetaSpending' = c(0.2, 0.1, 0.05) must be a vector that satisfies the ",
	    "following condition: 0 <= beta_1 <= .. <= beta_3 <= beta = 0.05"
	),
	fixed = TRUE
	)

	expect_error(getDesignGroupSequential(
	    typeOfDesign = C_TYPE_OF_DESIGN_AS_USER,
	    userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER,
	    userBetaSpending = c(0.1, 0.2, 0.3), beta = 0.2
	),
	paste0(
	    "'userBetaSpending' = c(0.1, 0.2, 0.3) must be a vector that satisfies the ",
	    "following condition: 0 <= beta_1 <= .. <= beta_3 <= beta = 0.2"
	),
	fixed = TRUE
	)

	expect_error(getDesignGroupSequential(kMax = Inf),
	    paste0(
	        "Argument out of bounds: 'kMax' (Inf) is out of bounds [1; ",
	        C_KMAX_UPPER_BOUND, "]"
	    ),
	    fixed = TRUE
	)

	expect_error(getDesignGroupSequential(kMax = -Inf),
	    paste0(
	        "Argument out of bounds: 'kMax' (-Inf) is out of bounds [1; ",
	        C_KMAX_UPPER_BOUND, "]"
	    ),
	    fixed = TRUE
	)

	expect_error(getDesignInverseNormal(kMax = -5), "Argument out of bounds: 'kMax' (-5) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 0), "Argument out of bounds: 'kMax' (0) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 21), "Argument out of bounds: 'kMax' (21) is out of bounds [1; 20]", fixed = TRUE)

	expect_error(getDesignInverseNormal(kMax = 2, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (2) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 3, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (3) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 4, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (4) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 6, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (6) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 7, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (7) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 8, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (8) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 9, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (9) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 10, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (10) - 1", fixed = TRUE)

	expect_warning(expect_error(getDesignInverseNormal(kMax = 11, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (11) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 12, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (12) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 13, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (13) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 14, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (14) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 15, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (15) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 16, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (16) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 17, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (17) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 18, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (18) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 19, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (19) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 20, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (20) - 1", fixed = TRUE))

	expect_error(getDesignGroupSequential(futilityBounds = c(-7, 5)),
	    "Illegal argument: 'futilityBounds' (-7, 5) too extreme for this situation",
	    fixed = TRUE
	)

	expect_error(getDesignGroupSequential(futilityBounds = c(1, 7)),
	    "Argument out of bounds: 'futilityBounds' (1, 7) is out of bounds [-Inf; 6]",
	    fixed = TRUE
	)
})

