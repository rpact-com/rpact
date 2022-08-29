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
## |  File name: test-f_design_group_sequential_beta_spending.R
## |  Creation date: 15 August 2022, 15:01:13
## |  File version: $Revision: 6491 $
## |  Last changed: $Date: 2022-08-15 15:37:18 +0200 (Mo, 15 Aug 2022) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing the Group Sequential Design Functionality with Beta Spending")


test_that("'getDesignGroupSequential' with two-sided beta spending (1)", {
	suppressWarnings(x <- getDesignGroupSequential(
	    informationRates = c(0.3, 0.4, 0.8, 1),
	    alpha = 0.05, typeOfDesign = "asKD", gammaA = 2.5, beta = 0.1, sided = 2,
	    typeBetaSpending = "bsKD", gammaB = 1.5, bindingFutility = FALSE
	))

	## Comparison of the results of TrialDesignGroupSequential object 'x' with expected results
	expect_equal(x$power, c(0.12305271, 0.24472466, 0.79581714, 0.89999927), tolerance = 1e-07)
	expect_equal(x$futilityBounds, c(NA_real_, 0.13460523, 1.5007674), tolerance = 1e-07)
	expect_equal(x$alphaSpent, c(0.0024647515, 0.0050596443, 0.028621671, 0.05), tolerance = 1e-07)
	expect_equal(x$betaSpent, c(0, 0.010609935, 0.065960997, 0.1), tolerance = 1e-07)
	expect_equal(x$criticalValues, c(3.0276355, 2.8984727, 2.2289649, 2.0639032), tolerance = 1e-07)
	expect_equal(x$stageLevels, c(0.0012323758, 0.0018749246, 0.01290812, 0.019513449), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    suppressWarnings(xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL))))
	    expect_equal(xCodeBased$power, x$power, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityBounds, x$futilityBounds, tolerance = 1e-05)
	    expect_equal(xCodeBased$alphaSpent, x$alphaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$betaSpent, x$betaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$criticalValues, x$criticalValues, tolerance = 1e-05)
	    expect_equal(xCodeBased$stageLevels, x$stageLevels, tolerance = 1e-05)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	y <- getDesignCharacteristics(x)

	## Comparison of the results of TrialDesignCharacteristics object 'y' with expected results
	expect_equal(y$nFixed, 10.507423, tolerance = 1e-07)
	expect_equal(y$shift, 11.628636, tolerance = 1e-07)
	expect_equal(y$inflationFactor, 1.1067068, tolerance = 1e-07)
	expect_equal(y$information, c(3.4885908, 4.6514544, 9.3029088, 11.628636), tolerance = 1e-07)
	expect_equal(y$power, c(0.1230532, 0.24472551, 0.79581823, 0.9), tolerance = 1e-07)
	expect_equal(y$rejectionProbabilities, c(0.1230532, 0.12167232, 0.55109272, 0.10418177), tolerance = 1e-07)
	expect_equal(y$futilityProbabilities, c(0, 0.010609873, 0.055350639), tolerance = 1e-07)
	expect_equal(y$averageSampleNumber1, 0.78930804, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber01, 0.89600313, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber0, 0.85810726, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(y), NA)))
	    expect_output(print(y)$show())
	    invisible(capture.output(expect_error(summary(y), NA)))
	    expect_output(summary(y)$show())
	    suppressWarnings(yCodeBased <- eval(parse(text = getObjectRCode(y, stringWrapParagraphWidth = NULL))))
	    expect_equal(yCodeBased$nFixed, y$nFixed, tolerance = 1e-05)
	    expect_equal(yCodeBased$shift, y$shift, tolerance = 1e-05)
	    expect_equal(yCodeBased$inflationFactor, y$inflationFactor, tolerance = 1e-05)
	    expect_equal(yCodeBased$information, y$information, tolerance = 1e-05)
	    expect_equal(yCodeBased$power, y$power, tolerance = 1e-05)
	    expect_equal(yCodeBased$rejectionProbabilities, y$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$futilityProbabilities, y$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber1, y$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber01, y$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber0, y$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(y), "character")
	    df <- as.data.frame(y)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(y)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with two-sided beta spending (2)", {

	.skipTestIfDisabled()

	suppressWarnings(x <- getDesignGroupSequential(
	    informationRates = c(0.4, 0.65, 0.8, 1),
	    alpha = 0.05, typeOfDesign = "asKD", gammaA = 2.5, beta = 0.1, sided = 2,
	    typeBetaSpending = "bsKD", gammaB = 1.5, bindingFutility = TRUE
	))

	## Comparison of the results of TrialDesignGroupSequential object 'x' with expected results
	expect_equal(x$power, c(0.25984202, 0.62910001, 0.78681992, 0.89999952), tolerance = 1e-07)
	expect_equal(x$futilityBounds, c(0.3085062, 0.97461473, 1.3896954), tolerance = 1e-07)
	expect_equal(x$alphaSpent, c(0.0050596443, 0.017031519, 0.02862167, 0.04999999), tolerance = 1e-07)
	expect_equal(x$betaSpent, c(0.025298221, 0.052404675, 0.071554176, 0.1), tolerance = 1e-07)
	expect_equal(x$criticalValues, c(2.8032117, 2.4453423, 2.2956849, 1.9878913), tolerance = 1e-07)
	expect_equal(x$stageLevels, c(0.0025298221, 0.0072357361, 0.010846951, 0.023411854), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    suppressWarnings(xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL))))
	    expect_equal(xCodeBased$power, x$power, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityBounds, x$futilityBounds, tolerance = 1e-05)
	    expect_equal(xCodeBased$alphaSpent, x$alphaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$betaSpent, x$betaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$criticalValues, x$criticalValues, tolerance = 1e-05)
	    expect_equal(xCodeBased$stageLevels, x$stageLevels, tolerance = 1e-05)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	y <- getDesignCharacteristics(x)

	## Comparison of the results of TrialDesignCharacteristics object 'y' with expected results
	expect_equal(y$nFixed, 10.507423, tolerance = 1e-07)
	expect_equal(y$shift, 11.657317, tolerance = 1e-07)
	expect_equal(y$inflationFactor, 1.1094363, tolerance = 1e-07)
	expect_equal(y$information, c(4.6629267, 7.5772558, 9.3258533, 11.657317), tolerance = 1e-07)
	expect_equal(y$power, c(0.25984263, 0.62910091, 0.78682068, 0.9), tolerance = 1e-07)
	expect_equal(y$rejectionProbabilities, c(0.25984263, 0.36925828, 0.15771977, 0.11317932), tolerance = 1e-07)
	expect_equal(y$futilityProbabilities, c(0.025298122, 0.027106314, 0.019149403), tolerance = 1e-07)
	expect_equal(y$averageSampleNumber1, 0.72647427, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber01, 0.80995959, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber0, 0.72430062, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(y), NA)))
	    expect_output(print(y)$show())
	    invisible(capture.output(expect_error(summary(y), NA)))
	    expect_output(summary(y)$show())
	    suppressWarnings(yCodeBased <- eval(parse(text = getObjectRCode(y, stringWrapParagraphWidth = NULL))))
	    expect_equal(yCodeBased$nFixed, y$nFixed, tolerance = 1e-05)
	    expect_equal(yCodeBased$shift, y$shift, tolerance = 1e-05)
	    expect_equal(yCodeBased$inflationFactor, y$inflationFactor, tolerance = 1e-05)
	    expect_equal(yCodeBased$information, y$information, tolerance = 1e-05)
	    expect_equal(yCodeBased$power, y$power, tolerance = 1e-05)
	    expect_equal(yCodeBased$rejectionProbabilities, y$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$futilityProbabilities, y$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber1, y$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber01, y$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber0, y$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(y), "character")
	    df <- as.data.frame(y)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(y)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with two-sided beta spending (3)", {

	.skipTestIfDisabled()

	suppressWarnings(x <- getDesignGroupSequential(
	    informationRates = c(0.15, 0.25, 0.8, 1),
	    alpha = 0.025, typeOfDesign = "asOF", beta = 0.07, sided = 2,
	    typeBetaSpending = "bsUser", userBetaSpending = c(0.15, 0.25, 0.8, 1) * 0.07, 
	    bindingFutility = FALSE
	))

	## Comparison of the results of TrialDesignGroupSequential object 'x' with expected results
	expect_equal(x$power, c(6.6585304e-07, 0.0017900571, 0.82193405, 0.93), tolerance = 1e-07)
	expect_equal(x$futilityBounds, c(NA_real_, NA_real_, 1.8509555), tolerance = 1e-07)
	expect_equal(x$alphaSpent, c(2.2511015e-10, 1.1742122e-06, 0.01045986, 0.025), tolerance = 1e-07)
	expect_equal(x$betaSpent, c(0, 0, 0.051333333, 0.07), tolerance = 1e-07)
	expect_equal(x$criticalValues, c(6.3431527, 4.8600403, 2.560259, 2.292451), tolerance = 1e-07)
	expect_equal(x$stageLevels, c(1.1255508e-10, 5.8680939e-07, 0.0052297087, 0.010939815), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    suppressWarnings(xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL))))
	    expect_equal(xCodeBased$power, x$power, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityBounds, x$futilityBounds, tolerance = 1e-05)
	    expect_equal(xCodeBased$alphaSpent, x$alphaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$betaSpent, x$betaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$criticalValues, x$criticalValues, tolerance = 1e-05)
	    expect_equal(xCodeBased$stageLevels, x$stageLevels, tolerance = 1e-05)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	y <- getDesignCharacteristics(x)

	## Comparison of the results of TrialDesignCharacteristics object 'y' with expected results
	expect_equal(y$nFixed, 13.817529, tolerance = 1e-07)
	expect_equal(y$shift, 15.164247, tolerance = 1e-07)
	expect_equal(y$inflationFactor, 1.0974644, tolerance = 1e-07)
	expect_equal(y$information, c(2.274637, 3.7910617, 12.131397, 15.164247), tolerance = 1e-07)
	expect_equal(y$power, c(6.6585305e-07, 0.0017900572, 0.82193406, 0.93), tolerance = 1e-07)
	expect_equal(y$rejectionProbabilities, c(6.6585305e-07, 0.0017893913, 0.820144, 0.10806594), tolerance = 1e-07)
	expect_equal(y$futilityProbabilities, c(0, 0, 0.051333332), tolerance = 1e-07)
	expect_equal(y$averageSampleNumber1, 0.90470787, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber01, 0.93283887, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber0, 0.88976115, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(y), NA)))
	    expect_output(print(y)$show())
	    invisible(capture.output(expect_error(summary(y), NA)))
	    expect_output(summary(y)$show())
	    suppressWarnings(yCodeBased <- eval(parse(text = getObjectRCode(y, stringWrapParagraphWidth = NULL))))
	    expect_equal(yCodeBased$nFixed, y$nFixed, tolerance = 1e-05)
	    expect_equal(yCodeBased$shift, y$shift, tolerance = 1e-05)
	    expect_equal(yCodeBased$inflationFactor, y$inflationFactor, tolerance = 1e-05)
	    expect_equal(yCodeBased$information, y$information, tolerance = 1e-05)
	    expect_equal(yCodeBased$power, y$power, tolerance = 1e-05)
	    expect_equal(yCodeBased$rejectionProbabilities, y$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$futilityProbabilities, y$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber1, y$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber01, y$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber0, y$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(y), "character")
	    df <- as.data.frame(y)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(y)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with two-sided beta spending (4)", {

	.skipTestIfDisabled()

	suppressWarnings(x <- getDesignGroupSequential(
	    informationRates = c(0.35, 0.55, 0.8, 1),
	    alpha = 0.035, beta = 0.065, sided = 2,
	    typeOfDesign = "asKD", gammaA = 1.44,
	    typeBetaSpending = "bsKD", gammaB = 1.35, bindingFutility = TRUE
	))

	## Comparison of the results of TrialDesignGroupSequential object 'x' with expected results
	expect_equal(x$power, c(0.36066758, 0.64302509, 0.86790998, 0.93499965), tolerance = 1e-07)
	expect_equal(x$futilityBounds, c(0.26890478, 0.80339676, 1.6440939), tolerance = 1e-07)
	expect_equal(x$alphaSpent, c(0.0077183777, 0.014797567, 0.02538152, 0.035), tolerance = 1e-07)
	expect_equal(x$betaSpent, c(0.015754521, 0.029000333, 0.048093329, 0.065), tolerance = 1e-07)
	expect_equal(x$criticalValues, c(2.6641472, 2.5778904, 2.3970592, 2.2417317), tolerance = 1e-07)
	expect_equal(x$stageLevels, c(0.0038591889, 0.0049702762, 0.0082636274, 0.01248936), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    suppressWarnings(xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL))))
	    expect_equal(xCodeBased$power, x$power, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityBounds, x$futilityBounds, tolerance = 1e-05)
	    expect_equal(xCodeBased$alphaSpent, x$alphaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$betaSpent, x$betaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$criticalValues, x$criticalValues, tolerance = 1e-05)
	    expect_equal(xCodeBased$stageLevels, x$stageLevels, tolerance = 1e-05)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	y <- getDesignCharacteristics(x)

	## Comparison of the results of TrialDesignCharacteristics object 'y' with expected results
	expect_equal(y$nFixed, 13.122219, tolerance = 1e-07)
	expect_equal(y$shift, 15.212676, tolerance = 1e-07)
	expect_equal(y$inflationFactor, 1.1593067, tolerance = 1e-07)
	expect_equal(y$information, c(5.3244366, 8.3669718, 12.170141, 15.212676), tolerance = 1e-07)
	expect_equal(y$power, c(0.36066826, 0.64302593, 0.86791055, 0.935), tolerance = 1e-07)
	expect_equal(y$rejectionProbabilities, c(0.36066826, 0.28235767, 0.22488463, 0.067089448), tolerance = 1e-07)
	expect_equal(y$futilityProbabilities, c(0.015754457, 0.013245738, 0.019092883), tolerance = 1e-07)
	expect_equal(y$averageSampleNumber1, 0.66487165, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber01, 0.81339132, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber0, 0.7082656, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(y), NA)))
	    expect_output(print(y)$show())
	    invisible(capture.output(expect_error(summary(y), NA)))
	    expect_output(summary(y)$show())
	    suppressWarnings(yCodeBased <- eval(parse(text = getObjectRCode(y, stringWrapParagraphWidth = NULL))))
	    expect_equal(yCodeBased$nFixed, y$nFixed, tolerance = 1e-05)
	    expect_equal(yCodeBased$shift, y$shift, tolerance = 1e-05)
	    expect_equal(yCodeBased$inflationFactor, y$inflationFactor, tolerance = 1e-05)
	    expect_equal(yCodeBased$information, y$information, tolerance = 1e-05)
	    expect_equal(yCodeBased$power, y$power, tolerance = 1e-05)
	    expect_equal(yCodeBased$rejectionProbabilities, y$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$futilityProbabilities, y$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber1, y$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber01, y$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber0, y$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(y), "character")
	    df <- as.data.frame(y)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(y)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with two-sided beta spending (5)", {

	.skipTestIfDisabled()

	suppressWarnings(x <- getDesignGroupSequential(
	    informationRates = c(0.35, 0.4, 0.8, 1),
	    alpha = 0.025, typeOfDesign = "asOF", beta = 0.07, sided = 2,
	    typeBetaSpending = "bsKD", gammaB = 1, bindingFutility = TRUE
	))

	## Comparison of the results of TrialDesignGroupSequential object 'x' with expected results
	expect_equal(x$power, c(0.040854392, 0.095590668, 0.82438045, 0.93), tolerance = 1e-07)
	expect_equal(x$futilityBounds, c(0.40613204, 0.2966315, 1.7350381), tolerance = 1e-07)
	expect_equal(x$alphaSpent, c(4.8451862e-05, 0.00015681312, 0.010459859, 0.025), tolerance = 1e-07)
	expect_equal(x$betaSpent, c(0.0245, 0.028, 0.056, 0.07), tolerance = 1e-07)
	expect_equal(x$criticalValues, c(4.0629719, 3.8052638, 2.5524087, 2.1888976), tolerance = 1e-07)
	expect_equal(x$stageLevels, c(2.4225931e-05, 7.0826578e-05, 0.0053490474, 0.014302143), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    suppressWarnings(xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL))))
	    expect_equal(xCodeBased$power, x$power, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityBounds, x$futilityBounds, tolerance = 1e-05)
	    expect_equal(xCodeBased$alphaSpent, x$alphaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$betaSpent, x$betaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$criticalValues, x$criticalValues, tolerance = 1e-05)
	    expect_equal(xCodeBased$stageLevels, x$stageLevels, tolerance = 1e-05)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	y <- getDesignCharacteristics(x)

	## Comparison of the results of TrialDesignCharacteristics object 'y' with expected results
	expect_equal(y$nFixed, 13.817529, tolerance = 1e-07)
	expect_equal(y$shift, 15.406346, tolerance = 1e-07)
	expect_equal(y$inflationFactor, 1.1149856, tolerance = 1e-07)
	expect_equal(y$information, c(5.3922213, 6.1625386, 12.325077, 15.406346), tolerance = 1e-07)
	expect_equal(y$power, c(0.040854392, 0.095590669, 0.82438046, 0.93), tolerance = 1e-07)
	expect_equal(y$rejectionProbabilities, c(0.040854392, 0.054736277, 0.72878979, 0.10561954), tolerance = 1e-07)
	expect_equal(y$futilityProbabilities, c(0.0245, 0.0034999999, 0.028), tolerance = 1e-07)
	expect_equal(y$averageSampleNumber1, 0.85989912, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber01, 0.85050502, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber0, 0.71709153, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(y), NA)))
	    expect_output(print(y)$show())
	    invisible(capture.output(expect_error(summary(y), NA)))
	    expect_output(summary(y)$show())
	    suppressWarnings(yCodeBased <- eval(parse(text = getObjectRCode(y, stringWrapParagraphWidth = NULL))))
	    expect_equal(yCodeBased$nFixed, y$nFixed, tolerance = 1e-05)
	    expect_equal(yCodeBased$shift, y$shift, tolerance = 1e-05)
	    expect_equal(yCodeBased$inflationFactor, y$inflationFactor, tolerance = 1e-05)
	    expect_equal(yCodeBased$information, y$information, tolerance = 1e-05)
	    expect_equal(yCodeBased$power, y$power, tolerance = 1e-05)
	    expect_equal(yCodeBased$rejectionProbabilities, y$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$futilityProbabilities, y$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber1, y$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber01, y$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber0, y$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(y), "character")
	    df <- as.data.frame(y)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(y)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with two-sided beta spending (6)", {

	.skipTestIfDisabled()

	suppressWarnings(x <- getDesignGroupSequential(
	    informationRates = c(0.35, 0.4, 0.8, 1),
	    alpha = 0.025, typeOfDesign = "asOF", beta = 0.07, sided = 2,
	    typeBetaSpending = "bsUser", userBetaSpending = c(0.15, 0.4, 0.8, 1) * 0.01, 
	    bindingFutility = TRUE
	))

	## Comparison of the results of TrialDesignGroupSequential object 'x' with expected results
	expect_equal(x$power, c(0.10679919, 0.21860668, 0.95487449, 0.99), tolerance = 1e-07)
	expect_equal(x$futilityBounds, c(NA_real_, 0.30649568, 1.7163392), tolerance = 1e-07)
	expect_equal(x$alphaSpent, c(4.8451862e-05, 0.00015681312, 0.010459859, 0.025), tolerance = 1e-07)
	expect_equal(x$betaSpent, c(0, 0.0029411764, 0.0076470588, 0.01), tolerance = 1e-07)
	expect_equal(x$criticalValues, c(4.0629719, 3.8052638, 2.559217, 2.2179478), tolerance = 1e-07)
	expect_equal(x$stageLevels, c(2.4225931e-05, 7.0826578e-05, 0.0052454112, 0.013279197), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    suppressWarnings(xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL))))
	    expect_equal(xCodeBased$power, x$power, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityBounds, x$futilityBounds, tolerance = 1e-05)
	    expect_equal(xCodeBased$alphaSpent, x$alphaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$betaSpent, x$betaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$criticalValues, x$criticalValues, tolerance = 1e-05)
	    expect_equal(xCodeBased$stageLevels, x$stageLevels, tolerance = 1e-05)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	y <- getDesignCharacteristics(x)

	## Comparison of the results of TrialDesignCharacteristics object 'y' with expected results
	expect_equal(y$nFixed, 13.817529, tolerance = 1e-07)
	expect_equal(y$shift, 14.763145, tolerance = 1e-07)
	expect_equal(y$inflationFactor, 1.068436, tolerance = 1e-07)
	expect_equal(y$information, c(5.1671008, 5.9052581, 11.810516, 14.763145), tolerance = 1e-07)
	expect_equal(y$power, c(0.036739326, 0.087021046, 0.80817834, 0.93), tolerance = 1e-07)
	expect_equal(y$rejectionProbabilities, c(0.036739326, 0.05028172, 0.7211573, 0.12182166), tolerance = 1e-07)
	expect_equal(y$futilityProbabilities, c(0, 0.013748548, 0.0362213), tolerance = 1e-07)
	expect_equal(y$averageSampleNumber1, 0.84003165, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber01, 0.86354683, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber0, 0.76707979, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(y), NA)))
	    expect_output(print(y)$show())
	    invisible(capture.output(expect_error(summary(y), NA)))
	    expect_output(summary(y)$show())
	    suppressWarnings(yCodeBased <- eval(parse(text = getObjectRCode(y, stringWrapParagraphWidth = NULL))))
	    expect_equal(yCodeBased$nFixed, y$nFixed, tolerance = 1e-05)
	    expect_equal(yCodeBased$shift, y$shift, tolerance = 1e-05)
	    expect_equal(yCodeBased$inflationFactor, y$inflationFactor, tolerance = 1e-05)
	    expect_equal(yCodeBased$information, y$information, tolerance = 1e-05)
	    expect_equal(yCodeBased$power, y$power, tolerance = 1e-05)
	    expect_equal(yCodeBased$rejectionProbabilities, y$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$futilityProbabilities, y$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber1, y$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber01, y$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber0, y$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(y), "character")
	    df <- as.data.frame(y)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(y)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with two-sided beta spending (7)", {

	.skipTestIfDisabled()

	suppressWarnings(x <- getDesignGroupSequential(
	    informationRates = c(0.15, 0.4, 0.8, 1),
	    alpha = 0.025, typeOfDesign = "asOF", beta = 0.01, sided = 2,
	    typeBetaSpending = "bsKD", gammaB = 1, bindingFutility = FALSE
	))

	## Comparison of the results of TrialDesignGroupSequential object 'x' with expected results
	expect_equal(x$power, c(3.8817388e-06, 0.23459419, 0.95988861, 0.99), tolerance = 1e-07)
	expect_equal(x$futilityBounds, c(NA_real_, 0.33832477, 1.7778049), tolerance = 1e-07)
	expect_equal(x$alphaSpent, c(2.2511015e-10, 0.00015681311, 0.010459859, 0.02499999), tolerance = 1e-07)
	expect_equal(x$betaSpent, c(0, 0.0029411765, 0.0076470589, 0.0099999999), tolerance = 1e-07)
	expect_equal(x$criticalValues, c(6.3431527, 3.7800251, 2.5620799, 2.2927506), tolerance = 1e-07)
	expect_equal(x$stageLevels, c(1.1255508e-10, 7.8406284e-05, 0.0052023689, 0.010931184), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    suppressWarnings(xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL))))
	    expect_equal(xCodeBased$power, x$power, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityBounds, x$futilityBounds, tolerance = 1e-05)
	    expect_equal(xCodeBased$alphaSpent, x$alphaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$betaSpent, x$betaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$criticalValues, x$criticalValues, tolerance = 1e-05)
	    expect_equal(xCodeBased$stageLevels, x$stageLevels, tolerance = 1e-05)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	y <- getDesignCharacteristics(x)

	## Comparison of the results of TrialDesignCharacteristics object 'y' with expected results
	expect_equal(y$nFixed, 20.864346, tolerance = 1e-07)
	expect_equal(y$shift, 23.351275, tolerance = 1e-07)
	expect_equal(y$inflationFactor, 1.1191952, tolerance = 1e-07)
	expect_equal(y$information, c(3.5026912, 9.3405098, 18.68102, 23.351275), tolerance = 1e-07)
	expect_equal(y$power, c(3.8817388e-06, 0.23459418, 0.9598886, 0.99), tolerance = 1e-07)
	expect_equal(y$rejectionProbabilities, c(3.8817388e-06, 0.2345903, 0.72529442, 0.030111395), tolerance = 1e-07)
	expect_equal(y$futilityProbabilities, c(0, 0.0029411765, 0.0047058824), tolerance = 1e-07)
	expect_equal(y$averageSampleNumber1, 0.79628246, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber01, 0.91655265, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber0, 0.79046909, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(y), NA)))
	    expect_output(print(y)$show())
	    invisible(capture.output(expect_error(summary(y), NA)))
	    expect_output(summary(y)$show())
	    suppressWarnings(yCodeBased <- eval(parse(text = getObjectRCode(y, stringWrapParagraphWidth = NULL))))
	    expect_equal(yCodeBased$nFixed, y$nFixed, tolerance = 1e-05)
	    expect_equal(yCodeBased$shift, y$shift, tolerance = 1e-05)
	    expect_equal(yCodeBased$inflationFactor, y$inflationFactor, tolerance = 1e-05)
	    expect_equal(yCodeBased$information, y$information, tolerance = 1e-05)
	    expect_equal(yCodeBased$power, y$power, tolerance = 1e-05)
	    expect_equal(yCodeBased$rejectionProbabilities, y$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$futilityProbabilities, y$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber1, y$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber01, y$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber0, y$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(y), "character")
	    df <- as.data.frame(y)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(y)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with two-sided beta spending (8)", {

	.skipTestIfDisabled()

	suppressWarnings(x <- getDesignGroupSequential(
	    informationRates = c(0.15, 0.4, 0.8, 1),
	    alpha = 0.025, typeOfDesign = "asOF", beta = 0.01, sided = 2,
	    typeBetaSpending = "bsUser", userBetaSpending = c(0.15, 0.4, 0.8, 1) * 0.01, 
	    bindingFutility = FALSE
	))

	## Comparison of the results of TrialDesignGroupSequential object 'x' with expected results
	expect_equal(x$power, c(3.8817388e-06, 0.23459419, 0.95988861, 0.99), tolerance = 1e-07)
	expect_equal(x$futilityBounds, c(NA_real_, 0.33832477, 1.7778049), tolerance = 1e-07)
	expect_equal(x$alphaSpent, c(2.2511015e-10, 0.00015681311, 0.010459859, 0.02499999), tolerance = 1e-07)
	expect_equal(x$betaSpent, c(0, 0.0029411765, 0.0076470589, 0.0099999999), tolerance = 1e-07)
	expect_equal(x$criticalValues, c(6.3431527, 3.7800251, 2.5620799, 2.2927506), tolerance = 1e-07)
	expect_equal(x$stageLevels, c(1.1255508e-10, 7.8406284e-05, 0.0052023689, 0.010931184), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    suppressWarnings(xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL))))
	    expect_equal(xCodeBased$power, x$power, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityBounds, x$futilityBounds, tolerance = 1e-05)
	    expect_equal(xCodeBased$alphaSpent, x$alphaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$betaSpent, x$betaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$criticalValues, x$criticalValues, tolerance = 1e-05)
	    expect_equal(xCodeBased$stageLevels, x$stageLevels, tolerance = 1e-05)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	y <- getDesignCharacteristics(x)

	## Comparison of the results of TrialDesignCharacteristics object 'y' with expected results
	expect_equal(y$nFixed, 20.864346, tolerance = 1e-07)
	expect_equal(y$shift, 23.351275, tolerance = 1e-07)
	expect_equal(y$inflationFactor, 1.1191952, tolerance = 1e-07)
	expect_equal(y$information, c(3.5026912, 9.3405098, 18.68102, 23.351275), tolerance = 1e-07)
	expect_equal(y$power, c(3.8817388e-06, 0.23459418, 0.9598886, 0.99), tolerance = 1e-07)
	expect_equal(y$rejectionProbabilities, c(3.8817388e-06, 0.2345903, 0.72529442, 0.030111395), tolerance = 1e-07)
	expect_equal(y$futilityProbabilities, c(0, 0.0029411765, 0.0047058824), tolerance = 1e-07)
	expect_equal(y$averageSampleNumber1, 0.79628246, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber01, 0.91655265, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber0, 0.79046909, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(y), NA)))
	    expect_output(print(y)$show())
	    invisible(capture.output(expect_error(summary(y), NA)))
	    expect_output(summary(y)$show())
	    suppressWarnings(yCodeBased <- eval(parse(text = getObjectRCode(y, stringWrapParagraphWidth = NULL))))
	    expect_equal(yCodeBased$nFixed, y$nFixed, tolerance = 1e-05)
	    expect_equal(yCodeBased$shift, y$shift, tolerance = 1e-05)
	    expect_equal(yCodeBased$inflationFactor, y$inflationFactor, tolerance = 1e-05)
	    expect_equal(yCodeBased$information, y$information, tolerance = 1e-05)
	    expect_equal(yCodeBased$power, y$power, tolerance = 1e-05)
	    expect_equal(yCodeBased$rejectionProbabilities, y$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$futilityProbabilities, y$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber1, y$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber01, y$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber0, y$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(y), "character")
	    df <- as.data.frame(y)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(y)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with two-sided beta spending (9)", {

	.skipTestIfDisabled()

	suppressWarnings(x <- getDesignGroupSequential(
	    informationRates = c(0.35, 0.55, 0.8, 1),
	    alpha = 0.035, beta = 0.065, sided = 2,
	    typeOfDesign = "asKD", gammaA = 1.44,
	    typeBetaSpending = "bsKD", gammaB = 1.35, bindingFutility = FALSE
	))

	## Comparison of the results of TrialDesignGroupSequential object 'x' with expected results
	expect_equal(x$power, c(0.37576504, 0.66123596, 0.8775957, 0.93499972), tolerance = 1e-07)
	expect_equal(x$futilityBounds, c(0.29185889, 0.85901114, 1.708006), tolerance = 1e-07)
	expect_equal(x$alphaSpent, c(0.0077183777, 0.014797567, 0.02538152, 0.03499999), tolerance = 1e-07)
	expect_equal(x$betaSpent, c(0.015754521, 0.029000333, 0.048093329, 0.065), tolerance = 1e-07)
	expect_equal(x$criticalValues, c(2.6641472, 2.5781312, 2.4084661, 2.3291234), tolerance = 1e-07)
	expect_equal(x$stageLevels, c(0.0038591889, 0.0049668138, 0.0080098571, 0.0099262635), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    suppressWarnings(xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL))))
	    expect_equal(xCodeBased$power, x$power, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityBounds, x$futilityBounds, tolerance = 1e-05)
	    expect_equal(xCodeBased$alphaSpent, x$alphaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$betaSpent, x$betaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$criticalValues, x$criticalValues, tolerance = 1e-05)
	    expect_equal(xCodeBased$stageLevels, x$stageLevels, tolerance = 1e-05)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	y <- getDesignCharacteristics(x)

	## Comparison of the results of TrialDesignCharacteristics object 'y' with expected results
	expect_equal(y$nFixed, 13.122219, tolerance = 1e-07)
	expect_equal(y$shift, 15.745369, tolerance = 1e-07)
	expect_equal(y$inflationFactor, 1.1999015, tolerance = 1e-07)
	expect_equal(y$information, c(5.5108793, 8.6599532, 12.596296, 15.745369), tolerance = 1e-07)
	expect_equal(y$power, c(0.3757656, 0.66123663, 0.87759614, 0.935), tolerance = 1e-07)
	expect_equal(y$rejectionProbabilities, c(0.3757656, 0.28547103, 0.21635951, 0.057403857), tolerance = 1e-07)
	expect_equal(y$futilityProbabilities, c(0.015754468, 0.013245752, 0.019092905), tolerance = 1e-07)
	expect_equal(y$averageSampleNumber1, 0.67674292, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber01, 0.83002063, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber0, 0.7178349, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(y), NA)))
	    expect_output(print(y)$show())
	    invisible(capture.output(expect_error(summary(y), NA)))
	    expect_output(summary(y)$show())
	    suppressWarnings(yCodeBased <- eval(parse(text = getObjectRCode(y, stringWrapParagraphWidth = NULL))))
	    expect_equal(yCodeBased$nFixed, y$nFixed, tolerance = 1e-05)
	    expect_equal(yCodeBased$shift, y$shift, tolerance = 1e-05)
	    expect_equal(yCodeBased$inflationFactor, y$inflationFactor, tolerance = 1e-05)
	    expect_equal(yCodeBased$information, y$information, tolerance = 1e-05)
	    expect_equal(yCodeBased$power, y$power, tolerance = 1e-05)
	    expect_equal(yCodeBased$rejectionProbabilities, y$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$futilityProbabilities, y$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber1, y$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber01, y$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber0, y$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(y), "character")
	    df <- as.data.frame(y)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(y)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with two-sided beta spending (10)", {

	.skipTestIfDisabled()

	suppressWarnings(x <- getDesignGroupSequential(
	    informationRates = c(0.15, 0.4, 0.8, 1),
	    alpha = 0.025, typeOfDesign = "asOF", beta = 0.01, sided = 2,
	    typeBetaSpending = "bsUser", userBetaSpending = c(0.15, 0.4, 0.8, 1) * 0.01, 
	    bindingFutility = FALSE
	))

	## Comparison of the results of TrialDesignGroupSequential object 'x' with expected results
	expect_equal(x$power, c(3.8817388e-06, 0.23459419, 0.95988861, 0.99), tolerance = 1e-07)
	expect_equal(x$futilityBounds, c(NA_real_, 0.33832477, 1.7778049), tolerance = 1e-07)
	expect_equal(x$alphaSpent, c(2.2511015e-10, 0.00015681311, 0.010459859, 0.02499999), tolerance = 1e-07)
	expect_equal(x$betaSpent, c(0, 0.0029411765, 0.0076470589, 0.0099999999), tolerance = 1e-07)
	expect_equal(x$criticalValues, c(6.3431527, 3.7800251, 2.5620799, 2.2927506), tolerance = 1e-07)
	expect_equal(x$stageLevels, c(1.1255508e-10, 7.8406284e-05, 0.0052023689, 0.010931184), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    suppressWarnings(xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL))))
	    expect_equal(xCodeBased$power, x$power, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityBounds, x$futilityBounds, tolerance = 1e-05)
	    expect_equal(xCodeBased$alphaSpent, x$alphaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$betaSpent, x$betaSpent, tolerance = 1e-05)
	    expect_equal(xCodeBased$criticalValues, x$criticalValues, tolerance = 1e-05)
	    expect_equal(xCodeBased$stageLevels, x$stageLevels, tolerance = 1e-05)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	y <- getDesignCharacteristics(x)

	## Comparison of the results of TrialDesignCharacteristics object 'y' with expected results
	expect_equal(y$nFixed, 20.864346, tolerance = 1e-07)
	expect_equal(y$shift, 23.351275, tolerance = 1e-07)
	expect_equal(y$inflationFactor, 1.1191952, tolerance = 1e-07)
	expect_equal(y$information, c(3.5026912, 9.3405098, 18.68102, 23.351275), tolerance = 1e-07)
	expect_equal(y$power, c(3.8817388e-06, 0.23459418, 0.9598886, 0.99), tolerance = 1e-07)
	expect_equal(y$rejectionProbabilities, c(3.8817388e-06, 0.2345903, 0.72529442, 0.030111395), tolerance = 1e-07)
	expect_equal(y$futilityProbabilities, c(0, 0.0029411765, 0.0047058824), tolerance = 1e-07)
	expect_equal(y$averageSampleNumber1, 0.79628246, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber01, 0.91655265, tolerance = 1e-07)
	expect_equal(y$averageSampleNumber0, 0.79046909, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(y), NA)))
	    expect_output(print(y)$show())
	    invisible(capture.output(expect_error(summary(y), NA)))
	    expect_output(summary(y)$show())
	    suppressWarnings(yCodeBased <- eval(parse(text = getObjectRCode(y, stringWrapParagraphWidth = NULL))))
	    expect_equal(yCodeBased$nFixed, y$nFixed, tolerance = 1e-05)
	    expect_equal(yCodeBased$shift, y$shift, tolerance = 1e-05)
	    expect_equal(yCodeBased$inflationFactor, y$inflationFactor, tolerance = 1e-05)
	    expect_equal(yCodeBased$information, y$information, tolerance = 1e-05)
	    expect_equal(yCodeBased$power, y$power, tolerance = 1e-05)
	    expect_equal(yCodeBased$rejectionProbabilities, y$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$futilityProbabilities, y$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber1, y$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber01, y$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(yCodeBased$averageSampleNumber0, y$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(y), "character")
	    df <- as.data.frame(y)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(y)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
})

