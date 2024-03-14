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
## |  File name: test-f_design_fisher_combination_test.R
## |  Creation date: 08 November 2023, 09:09:43
## |  File version: $Revision$
## |  Last changed: $Date$
## |  Last changed by: $Author$
## |  

test_plan_section("Testing the Fisher Design Functionality")


test_that("'getDesignFisher' with default parameters: parameters and results are as expected", {
	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationEqualAlpha}
	designFisher0 <- getDesignFisher()

	## Comparison of the results of TrialDesignFisher object 'designFisher0' with expected results
	expect_equal(designFisher0$alphaSpent, c(0.012308547, 0.01962413, 0.025), tolerance = 1e-07, label = paste0("c(", paste0(designFisher0$alphaSpent, collapse = ", "), ")"))
	expect_equal(designFisher0$criticalValues, c(0.012308547, 0.0016635923, 0.00029106687), tolerance = 1e-07, label = paste0("c(", paste0(designFisher0$criticalValues, collapse = ", "), ")"))
	expect_equal(designFisher0$stageLevels, c(0.012308547, 0.012308547, 0.012308547), tolerance = 1e-07, label = paste0("c(", paste0(designFisher0$stageLevels, collapse = ", "), ")"))
	expect_equal(designFisher0$nonStochasticCurtailment, FALSE, label = paste0("c(", paste0(designFisher0$nonStochasticCurtailment, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher0), NA)))
	    expect_output(print(designFisher0)$show())
	    invisible(capture.output(expect_error(summary(designFisher0), NA)))
	    expect_output(summary(designFisher0)$show())
	    designFisher0CodeBased <- eval(parse(text = getObjectRCode(designFisher0, stringWrapParagraphWidth = NULL)))
	    expect_equal(designFisher0CodeBased$alphaSpent, designFisher0$alphaSpent, tolerance = 1e-07)
	    expect_equal(designFisher0CodeBased$criticalValues, designFisher0$criticalValues, tolerance = 1e-07)
	    expect_equal(designFisher0CodeBased$stageLevels, designFisher0$stageLevels, tolerance = 1e-07)
	    expect_equal(designFisher0CodeBased$nonStochasticCurtailment, designFisher0$nonStochasticCurtailment, tolerance = 1e-07)
	    expect_type(names(designFisher0), "character")
	    df <- as.data.frame(designFisher0)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(designFisher0)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignFisher' with default parameters and simulated alpha: parameters and results are as expected", {

	.skipTestIfDisabled()
	.skipTestIfNotX64()

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationEqualAlpha}
	designFisher <- getDesignFisher(iterations = 10000, seed = 1234567)

	## Comparison of the results of TrialDesignFisher object 'designFisher' with expected results
	expect_equal(designFisher$alphaSpent, c(0.012308547, 0.01962413, 0.025), tolerance = 1e-07, label = paste0("c(", paste0(designFisher$alphaSpent, collapse = ", "), ")"))
	expect_equal(designFisher$criticalValues, c(0.012308547, 0.0016635923, 0.00029106687), tolerance = 1e-07, label = paste0("c(", paste0(designFisher$criticalValues, collapse = ", "), ")"))
	expect_equal(designFisher$stageLevels, c(0.012308547, 0.012308547, 0.012308547), tolerance = 1e-07, label = paste0("c(", paste0(designFisher$stageLevels, collapse = ", "), ")"))
	expect_equal(designFisher$simAlpha, 0.0243, tolerance = 1e-07, label = paste0("c(", paste0(designFisher$simAlpha, collapse = ", "), ")"))
	expect_equal(designFisher$nonStochasticCurtailment, FALSE, label = paste0("c(", paste0(designFisher$nonStochasticCurtailment, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher), NA)))
	    expect_output(print(designFisher)$show())
	    invisible(capture.output(expect_error(summary(designFisher), NA)))
	    expect_output(summary(designFisher)$show())
	    designFisherCodeBased <- eval(parse(text = getObjectRCode(designFisher, stringWrapParagraphWidth = NULL)))
	    expect_equal(designFisherCodeBased$alphaSpent, designFisher$alphaSpent, tolerance = 1e-07)
	    expect_equal(designFisherCodeBased$criticalValues, designFisher$criticalValues, tolerance = 1e-07)
	    expect_equal(designFisherCodeBased$stageLevels, designFisher$stageLevels, tolerance = 1e-07)
	    expect_equal(designFisherCodeBased$simAlpha, designFisher$simAlpha, tolerance = 1e-07)
	    expect_equal(designFisherCodeBased$nonStochasticCurtailment, designFisher$nonStochasticCurtailment, tolerance = 1e-07)
	    expect_type(names(designFisher), "character")
	    df <- as.data.frame(designFisher)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(designFisher)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignFisher' with kMax = 2,3,..,6: parameters and results are as expected for different arguments", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationFullAlpha}
	designFisher1 <- getDesignFisher(kMax = 2, alpha = 0.05, alpha0Vec = 0.5, method = "fullAlpha")

	## Comparison of the results of TrialDesignFisher object 'designFisher1' with expected results
	expect_equal(designFisher1$alphaSpent, c(0.023314852, 0.05), tolerance = 1e-07, label = paste0("c(", paste0(designFisher1$alphaSpent, collapse = ", "), ")"))
	expect_equal(designFisher1$criticalValues, c(0.023314852, 0.0087049407), tolerance = 1e-07, label = paste0("c(", paste0(designFisher1$criticalValues, collapse = ", "), ")"))
	expect_equal(designFisher1$stageLevels, c(0.023314852, 0.05), tolerance = 1e-07, label = paste0("c(", paste0(designFisher1$stageLevels, collapse = ", "), ")"))
	expect_equal(designFisher1$nonStochasticCurtailment, FALSE, label = paste0("c(", paste0(designFisher1$nonStochasticCurtailment, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher1), NA)))
	    expect_output(print(designFisher1)$show())
	    invisible(capture.output(expect_error(summary(designFisher1), NA)))
	    expect_output(summary(designFisher1)$show())
	    designFisher1CodeBased <- eval(parse(text = getObjectRCode(designFisher1, stringWrapParagraphWidth = NULL)))
	    expect_equal(designFisher1CodeBased$alphaSpent, designFisher1$alphaSpent, tolerance = 1e-07)
	    expect_equal(designFisher1CodeBased$criticalValues, designFisher1$criticalValues, tolerance = 1e-07)
	    expect_equal(designFisher1CodeBased$stageLevels, designFisher1$stageLevels, tolerance = 1e-07)
	    expect_equal(designFisher1CodeBased$nonStochasticCurtailment, designFisher1$nonStochasticCurtailment, tolerance = 1e-07)
	    expect_type(names(designFisher1), "character")
	    df <- as.data.frame(designFisher1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(designFisher1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationEqualAlpha}
	designFisher2 <- getDesignFisher(kMax = 3, alpha0Vec = c(0.7, 0.5), informationRates = c(0.1, 0.3, 1), method = "equalAlpha")

	## Comparison of the results of TrialDesignFisher object 'designFisher2' with expected results
	expect_equal(designFisher2$alphaSpent, c(0.011823636, 0.019807903, 0.025), tolerance = 1e-07, label = paste0("c(", paste0(designFisher2$alphaSpent, collapse = ", "), ")"))
	expect_equal(designFisher2$criticalValues, c(0.011823636, 0.00036698794, 3.0631293e-07), tolerance = 1e-07, label = paste0("c(", paste0(designFisher2$criticalValues, collapse = ", "), ")"))
	expect_equal(designFisher2$stageLevels, c(0.011823636, 0.011823636, 0.011823636), tolerance = 1e-07, label = paste0("c(", paste0(designFisher2$stageLevels, collapse = ", "), ")"))
	expect_equal(designFisher2$nonStochasticCurtailment, FALSE, label = paste0("c(", paste0(designFisher2$nonStochasticCurtailment, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher2), NA)))
	    expect_output(print(designFisher2)$show())
	    invisible(capture.output(expect_error(summary(designFisher2), NA)))
	    expect_output(summary(designFisher2)$show())
	    designFisher2CodeBased <- eval(parse(text = getObjectRCode(designFisher2, stringWrapParagraphWidth = NULL)))
	    expect_equal(designFisher2CodeBased$alphaSpent, designFisher2$alphaSpent, tolerance = 1e-07)
	    expect_equal(designFisher2CodeBased$criticalValues, designFisher2$criticalValues, tolerance = 1e-07)
	    expect_equal(designFisher2CodeBased$stageLevels, designFisher2$stageLevels, tolerance = 1e-07)
	    expect_equal(designFisher2CodeBased$nonStochasticCurtailment, designFisher2$nonStochasticCurtailment, tolerance = 1e-07)
	    expect_type(names(designFisher2), "character")
	    df <- as.data.frame(designFisher2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(designFisher2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationEqualAlpha}
	designFisher3 <- getDesignFisher(kMax = 4, alpha0Vec = c(0.7, 0.5, 0.3), informationRates = c(0.1, 0.3, 0.6, 1), bindingFutility = FALSE, method = "equalAlpha")

	## Comparison of the results of TrialDesignFisher object 'designFisher3' with expected results
	expect_equal(designFisher3$alphaSpent, c(0.0082575405, 0.014885188, 0.020347598, 0.025), tolerance = 1e-07, label = paste0("c(", paste0(designFisher3$alphaSpent, collapse = ", "), ")"))
	expect_equal(designFisher3$criticalValues, c(0.0082575405, 0.00021760942, 4.7163541e-06, 8.3369321e-08), tolerance = 1e-07, label = paste0("c(", paste0(designFisher3$criticalValues, collapse = ", "), ")"))
	expect_equal(designFisher3$stageLevels, c(0.0082575405, 0.0082575405, 0.0082575405, 0.0082575405), tolerance = 1e-07, label = paste0("c(", paste0(designFisher3$stageLevels, collapse = ", "), ")"))
	expect_equal(designFisher3$nonStochasticCurtailment, FALSE, label = paste0("c(", paste0(designFisher3$nonStochasticCurtailment, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher3), NA)))
	    expect_output(print(designFisher3)$show())
	    invisible(capture.output(expect_error(summary(designFisher3), NA)))
	    expect_output(summary(designFisher3)$show())
	    designFisher3CodeBased <- eval(parse(text = getObjectRCode(designFisher3, stringWrapParagraphWidth = NULL)))
	    expect_equal(designFisher3CodeBased$alphaSpent, designFisher3$alphaSpent, tolerance = 1e-07)
	    expect_equal(designFisher3CodeBased$criticalValues, designFisher3$criticalValues, tolerance = 1e-07)
	    expect_equal(designFisher3CodeBased$stageLevels, designFisher3$stageLevels, tolerance = 1e-07)
	    expect_equal(designFisher3CodeBased$nonStochasticCurtailment, designFisher3$nonStochasticCurtailment, tolerance = 1e-07)
	    expect_type(names(designFisher3), "character")
	    df <- as.data.frame(designFisher3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(designFisher3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationEqualAlpha}
	designFisher4 <- getDesignFisher(kMax = 5, alpha0Vec = c(0.7, 0.5, 0.3, 0.3), informationRates = c(0.1, 0.3, 0.5, 0.6, 1), method = "equalAlpha")

	## Comparison of the results of TrialDesignFisher object 'designFisher4' with expected results
	expect_equal(designFisher4$alphaSpent, c(0.011157609, 0.018733282, 0.022750003, 0.024162936, 0.025), tolerance = 1e-07, label = paste0("c(", paste0(designFisher4$alphaSpent, collapse = ", "), ")"))
	expect_equal(designFisher4$criticalValues, c(0.011157609, 0.00033722277, 2.3068413e-05, 5.4825339e-06, 9.8015456e-08), tolerance = 1e-07, label = paste0("c(", paste0(designFisher4$criticalValues, collapse = ", "), ")"))
	expect_equal(designFisher4$stageLevels, c(0.011157609, 0.011157609, 0.011157609, 0.011157609, 0.011157609), tolerance = 1e-07, label = paste0("c(", paste0(designFisher4$stageLevels, collapse = ", "), ")"))
	expect_equal(designFisher4$nonStochasticCurtailment, FALSE, label = paste0("c(", paste0(designFisher4$nonStochasticCurtailment, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher4), NA)))
	    expect_output(print(designFisher4)$show())
	    invisible(capture.output(expect_error(summary(designFisher4), NA)))
	    expect_output(summary(designFisher4)$show())
	    designFisher4CodeBased <- eval(parse(text = getObjectRCode(designFisher4, stringWrapParagraphWidth = NULL)))
	    expect_equal(designFisher4CodeBased$alphaSpent, designFisher4$alphaSpent, tolerance = 1e-07)
	    expect_equal(designFisher4CodeBased$criticalValues, designFisher4$criticalValues, tolerance = 1e-07)
	    expect_equal(designFisher4CodeBased$stageLevels, designFisher4$stageLevels, tolerance = 1e-07)
	    expect_equal(designFisher4CodeBased$nonStochasticCurtailment, designFisher4$nonStochasticCurtailment, tolerance = 1e-07)
	    expect_type(names(designFisher4), "character")
	    df <- as.data.frame(designFisher4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(designFisher4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationEqualAlpha}
	designFisher5 <- getDesignFisher(kMax = 5, alpha = 0.2, alpha0Vec = c(0.7, 0.5, 0.3, 0.2), method = "equalAlpha")

	## Comparison of the results of TrialDesignFisher object 'designFisher5' with expected results
	expect_equal(designFisher5$alphaSpent, c(0.12649082, 0.17362071, 0.19349017, 0.19931765, 0.2), tolerance = 1e-07, label = paste0("c(", paste0(designFisher5$alphaSpent, collapse = ", "), ")"))
	expect_equal(designFisher5$criticalValues, c(0.12649082, 0.027546669, 0.0068856935, 0.0018391192, 0.00051168366), tolerance = 1e-07, label = paste0("c(", paste0(designFisher5$criticalValues, collapse = ", "), ")"))
	expect_equal(designFisher5$stageLevels, c(0.12649082, 0.12649082, 0.12649082, 0.12649082, 0.12649082), tolerance = 1e-07, label = paste0("c(", paste0(designFisher5$stageLevels, collapse = ", "), ")"))
	expect_equal(designFisher5$nonStochasticCurtailment, FALSE, label = paste0("c(", paste0(designFisher5$nonStochasticCurtailment, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher5), NA)))
	    expect_output(print(designFisher5)$show())
	    invisible(capture.output(expect_error(summary(designFisher5), NA)))
	    expect_output(summary(designFisher5)$show())
	    designFisher5CodeBased <- eval(parse(text = getObjectRCode(designFisher5, stringWrapParagraphWidth = NULL)))
	    expect_equal(designFisher5CodeBased$alphaSpent, designFisher5$alphaSpent, tolerance = 1e-07)
	    expect_equal(designFisher5CodeBased$criticalValues, designFisher5$criticalValues, tolerance = 1e-07)
	    expect_equal(designFisher5CodeBased$stageLevels, designFisher5$stageLevels, tolerance = 1e-07)
	    expect_equal(designFisher5CodeBased$nonStochasticCurtailment, designFisher5$nonStochasticCurtailment, tolerance = 1e-07)
	    expect_type(names(designFisher5), "character")
	    df <- as.data.frame(designFisher5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(designFisher5)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationFullAlpha}
	designFisher6 <- getDesignFisher(kMax = 4, informationRates = c(0.1, 0.3, 0.7, 1), method = "fullAlpha")

	## Comparison of the results of TrialDesignFisher object 'designFisher6' with expected results
	expect_equal(designFisher6$alphaSpent, c(1.0550077e-06, 0.00020026524, 0.0065266359, 0.025), tolerance = 1e-07, label = paste0("c(", paste0(designFisher6$alphaSpent, collapse = ", "), ")"))
	expect_equal(designFisher6$criticalValues, c(1.0550077e-06, 1.0550077e-06, 1.0550077e-06, 1.0550077e-06), tolerance = 1e-07, label = paste0("c(", paste0(designFisher6$criticalValues, collapse = ", "), ")"))
	expect_equal(designFisher6$stageLevels, c(1.0550077e-06, 0.00020026524, 0.0065266359, 0.025), tolerance = 1e-07, label = paste0("c(", paste0(designFisher6$stageLevels, collapse = ", "), ")"))
	expect_equal(designFisher6$nonStochasticCurtailment, TRUE, label = paste0("c(", paste0(designFisher6$nonStochasticCurtailment, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher6), NA)))
	    expect_output(print(designFisher6)$show())
	    invisible(capture.output(expect_error(summary(designFisher6), NA)))
	    expect_output(summary(designFisher6)$show())
	    designFisher6CodeBased <- eval(parse(text = getObjectRCode(designFisher6, stringWrapParagraphWidth = NULL)))
	    expect_equal(designFisher6CodeBased$alphaSpent, designFisher6$alphaSpent, tolerance = 1e-07)
	    expect_equal(designFisher6CodeBased$criticalValues, designFisher6$criticalValues, tolerance = 1e-07)
	    expect_equal(designFisher6CodeBased$stageLevels, designFisher6$stageLevels, tolerance = 1e-07)
	    expect_equal(designFisher6CodeBased$nonStochasticCurtailment, designFisher6$nonStochasticCurtailment, tolerance = 1e-07)
	    expect_type(names(designFisher6), "character")
	    df <- as.data.frame(designFisher6)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(designFisher6)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationFullAlpha}
	designFisher7 <- getDesignFisher(kMax = 3, alpha0Vec = c(0.7, 0.6), informationRates = c(0.1, 0.7, 1), method = "fullAlpha")

	## Comparison of the results of TrialDesignFisher object 'designFisher7' with expected results
	expect_equal(designFisher7$alphaSpent, c(2.1580149e-06, 0.0066525356, 0.01947245), tolerance = 1e-07, label = paste0("c(", paste0(designFisher7$alphaSpent, collapse = ", "), ")"))
	expect_equal(designFisher7$criticalValues, c(2.1580149e-06, 2.1580149e-06, 2.1580149e-06), tolerance = 1e-07, label = paste0("c(", paste0(designFisher7$criticalValues, collapse = ", "), ")"))
	expect_equal(designFisher7$stageLevels, c(2.1580149e-06, 0.008216166, 0.025), tolerance = 1e-07, label = paste0("c(", paste0(designFisher7$stageLevels, collapse = ", "), ")"))
	expect_equal(designFisher7$nonStochasticCurtailment, TRUE, label = paste0("c(", paste0(designFisher7$nonStochasticCurtailment, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher7), NA)))
	    expect_output(print(designFisher7)$show())
	    invisible(capture.output(expect_error(summary(designFisher7), NA)))
	    expect_output(summary(designFisher7)$show())
	    designFisher7CodeBased <- eval(parse(text = getObjectRCode(designFisher7, stringWrapParagraphWidth = NULL)))
	    expect_equal(designFisher7CodeBased$alphaSpent, designFisher7$alphaSpent, tolerance = 1e-07)
	    expect_equal(designFisher7CodeBased$criticalValues, designFisher7$criticalValues, tolerance = 1e-07)
	    expect_equal(designFisher7CodeBased$stageLevels, designFisher7$stageLevels, tolerance = 1e-07)
	    expect_equal(designFisher7CodeBased$nonStochasticCurtailment, designFisher7$nonStochasticCurtailment, tolerance = 1e-07)
	    expect_type(names(designFisher7), "character")
	    df <- as.data.frame(designFisher7)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(designFisher7)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationNoTreatmentStageInteraction}
	designFisher8 <- getDesignFisher(kMax = 4, alpha0Vec = c(0.7, 0.6, 0.5), method = "noInteraction")

	## Comparison of the results of TrialDesignFisher object 'designFisher8' with expected results
	expect_equal(designFisher8$alphaSpent, c(0.0098603693, 0.012073314, 0.018133935, 0.025), tolerance = 1e-07, label = paste0("c(", paste0(designFisher8$alphaSpent, collapse = ", "), ")"))
	expect_equal(designFisher8$criticalValues, c(0.0098603693, 0.00051915905, 0.00031149543, 0.00015574772), tolerance = 1e-07, label = paste0("c(", paste0(designFisher8$criticalValues, collapse = ", "), ")"))
	expect_equal(designFisher8$stageLevels, c(0.0098603693, 0.0044457148, 0.012979977, 0.025), tolerance = 1e-07, label = paste0("c(", paste0(designFisher8$stageLevels, collapse = ", "), ")"))
	expect_equal(designFisher8$nonStochasticCurtailment, FALSE, label = paste0("c(", paste0(designFisher8$nonStochasticCurtailment, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher8), NA)))
	    expect_output(print(designFisher8)$show())
	    invisible(capture.output(expect_error(summary(designFisher8), NA)))
	    expect_output(summary(designFisher8)$show())
	    designFisher8CodeBased <- eval(parse(text = getObjectRCode(designFisher8, stringWrapParagraphWidth = NULL)))
	    expect_equal(designFisher8CodeBased$alphaSpent, designFisher8$alphaSpent, tolerance = 1e-07)
	    expect_equal(designFisher8CodeBased$criticalValues, designFisher8$criticalValues, tolerance = 1e-07)
	    expect_equal(designFisher8CodeBased$stageLevels, designFisher8$stageLevels, tolerance = 1e-07)
	    expect_equal(designFisher8CodeBased$nonStochasticCurtailment, designFisher8$nonStochasticCurtailment, tolerance = 1e-07)
	    expect_type(names(designFisher8), "character")
	    df <- as.data.frame(designFisher8)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(designFisher8)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationNoTreatmentStageInteraction}
	designFisher9 <- getDesignFisher(kMax = 6, alpha = 0.1, alpha0Vec = c(0.7, 0.6, 0.5, 0.4, 0.3), method = "noInteraction")

	## Comparison of the results of TrialDesignFisher object 'designFisher9' with expected results
	expect_equal(designFisher9$alphaSpent, c(0.058031958, 0.064517887, 0.079453273, 0.092924559, 0.098794775, 0.1), tolerance = 1e-07, label = paste0("c(", paste0(designFisher9$alphaSpent, collapse = ", "), ")"))
	expect_equal(designFisher9$criticalValues, c(0.058031958, 0.0026047006, 0.0015628203, 0.00078141017, 0.00031256407, 9.3769221e-05), tolerance = 1e-07, label = paste0("c(", paste0(designFisher9$criticalValues, collapse = ", "), ")"))
	expect_equal(designFisher9$stageLevels, c(0.058031958, 0.018103809, 0.044282865, 0.074062827, 0.095655516, 0.1), tolerance = 1e-07, label = paste0("c(", paste0(designFisher9$stageLevels, collapse = ", "), ")"))
	expect_equal(designFisher9$nonStochasticCurtailment, FALSE, label = paste0("c(", paste0(designFisher9$nonStochasticCurtailment, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher9), NA)))
	    expect_output(print(designFisher9)$show())
	    invisible(capture.output(expect_error(summary(designFisher9), NA)))
	    expect_output(summary(designFisher9)$show())
	    designFisher9CodeBased <- eval(parse(text = getObjectRCode(designFisher9, stringWrapParagraphWidth = NULL)))
	    expect_equal(designFisher9CodeBased$alphaSpent, designFisher9$alphaSpent, tolerance = 1e-07)
	    expect_equal(designFisher9CodeBased$criticalValues, designFisher9$criticalValues, tolerance = 1e-07)
	    expect_equal(designFisher9CodeBased$stageLevels, designFisher9$stageLevels, tolerance = 1e-07)
	    expect_equal(designFisher9CodeBased$nonStochasticCurtailment, designFisher9$nonStochasticCurtailment, tolerance = 1e-07)
	    expect_type(names(designFisher9), "character")
	    df <- as.data.frame(designFisher9)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(designFisher9)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationNoTreatmentStageInteraction}
	designFisher10 <- getDesignFisher(
	    kMax = 6, alpha = 0.1, alpha0Vec = c(0.7, 0.6, 0.5, 0.4, 0.3), method = "noInteraction",
	    informationRates = c(0.1, 0.15, 0.3, 0.4, 0.9, 1)
	)

	## Comparison of the results of TrialDesignFisher object 'designFisher10' with expected results
	expect_equal(designFisher10$alphaSpent, c(0.082381502, 0.082401579, 0.084330144, 0.086806556, 0.10023391, 0.1), tolerance = 1e-07, label = paste0("c(", paste0(designFisher10$alphaSpent, collapse = ", "), ")"))
	expect_equal(designFisher10$criticalValues, c(0.082381502, 0.00017925198, 0.00011812048, 5.906024e-05, 3.9204058e-05, 1.1761218e-05), tolerance = 1e-07, label = paste0("c(", paste0(designFisher10$criticalValues, collapse = ", "), ")"))
	expect_equal(designFisher10$stageLevels, c(0.082381502, 0.0005998602, 0.0062212598, 0.012409923, 0.09943647, 0.1), tolerance = 1e-07, label = paste0("c(", paste0(designFisher10$stageLevels, collapse = ", "), ")"))
	expect_equal(designFisher10$nonStochasticCurtailment, FALSE, label = paste0("c(", paste0(designFisher10$nonStochasticCurtailment, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher10), NA)))
	    expect_output(print(designFisher10)$show())
	    invisible(capture.output(expect_error(summary(designFisher10), NA)))
	    expect_output(summary(designFisher10)$show())
	    designFisher10CodeBased <- eval(parse(text = getObjectRCode(designFisher10, stringWrapParagraphWidth = NULL)))
	    expect_equal(designFisher10CodeBased$alphaSpent, designFisher10$alphaSpent, tolerance = 1e-07)
	    expect_equal(designFisher10CodeBased$criticalValues, designFisher10$criticalValues, tolerance = 1e-07)
	    expect_equal(designFisher10CodeBased$stageLevels, designFisher10$stageLevels, tolerance = 1e-07)
	    expect_equal(designFisher10CodeBased$nonStochasticCurtailment, designFisher10$nonStochasticCurtailment, tolerance = 1e-07)
	    expect_type(names(designFisher10), "character")
	    df <- as.data.frame(designFisher10)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(designFisher10)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationUserDefinedAlphaSpending}
	designFisher11 <- getDesignFisher(kMax = 4, alpha0Vec = c(0.7, 0.6, 0.5), method = "userDefinedAlpha", userAlphaSpending = c(0.01, 0.015, 0.02, 0.025))

	## Comparison of the results of TrialDesignFisher object 'designFisher11' with expected results
	expect_equal(designFisher11$alphaSpent, c(0.01, 0.015, 0.02, 0.025), tolerance = 1e-07, label = paste0("c(", paste0(designFisher11$alphaSpent, collapse = ", "), ")"))
	expect_equal(designFisher11$criticalValues, c(0.01, 0.0011768873, 0.00031357454, 0.00011586425), tolerance = 1e-07, label = paste0("c(", paste0(designFisher11$criticalValues, collapse = ", "), ")"))
	expect_equal(designFisher11$stageLevels, c(0.01, 0.0091148534, 0.013047692, 0.020300118), tolerance = 1e-07, label = paste0("c(", paste0(designFisher11$stageLevels, collapse = ", "), ")"))
	expect_equal(designFisher11$nonStochasticCurtailment, FALSE, label = paste0("c(", paste0(designFisher11$nonStochasticCurtailment, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher11), NA)))
	    expect_output(print(designFisher11)$show())
	    invisible(capture.output(expect_error(summary(designFisher11), NA)))
	    expect_output(summary(designFisher11)$show())
	    designFisher11CodeBased <- eval(parse(text = getObjectRCode(designFisher11, stringWrapParagraphWidth = NULL)))
	    expect_equal(designFisher11CodeBased$alphaSpent, designFisher11$alphaSpent, tolerance = 1e-07)
	    expect_equal(designFisher11CodeBased$criticalValues, designFisher11$criticalValues, tolerance = 1e-07)
	    expect_equal(designFisher11CodeBased$stageLevels, designFisher11$stageLevels, tolerance = 1e-07)
	    expect_equal(designFisher11CodeBased$nonStochasticCurtailment, designFisher11$nonStochasticCurtailment, tolerance = 1e-07)
	    expect_type(names(designFisher11), "character")
	    df <- as.data.frame(designFisher11)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(designFisher11)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationUserDefinedAlphaSpending}
	designFisher12 <- getDesignFisher(
	    kMax = 4, alpha0Vec = c(0.7, 0.6, 0.5), informationRates = c(0.1, 0.3, 0.7, 1),
	    method = "userDefinedAlpha", userAlphaSpending = c(0.01, 0.015, 0.02, 0.025)
	)

	## Comparison of the results of TrialDesignFisher object 'designFisher12' with expected results
	expect_equal(designFisher12$alphaSpent, c(0.01, 0.015, 0.02, 0.025), tolerance = 1e-07, label = paste0("c(", paste0(designFisher12$alphaSpent, collapse = ", "), ")"))
	expect_equal(designFisher12$criticalValues, c(0.01, 0.00018389153, 2.6484943e-06, 5.2344628e-07), tolerance = 1e-07, label = paste0("c(", paste0(designFisher12$criticalValues, collapse = ", "), ")"))
	expect_equal(designFisher12$stageLevels, c(0.01, 0.0073532156, 0.0101804, 0.018500415), tolerance = 1e-07, label = paste0("c(", paste0(designFisher12$stageLevels, collapse = ", "), ")"))
	expect_equal(designFisher12$nonStochasticCurtailment, FALSE, label = paste0("c(", paste0(designFisher12$nonStochasticCurtailment, collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher12), NA)))
	    expect_output(print(designFisher12)$show())
	    invisible(capture.output(expect_error(summary(designFisher12), NA)))
	    expect_output(summary(designFisher12)$show())
	    designFisher12CodeBased <- eval(parse(text = getObjectRCode(designFisher12, stringWrapParagraphWidth = NULL)))
	    expect_equal(designFisher12CodeBased$alphaSpent, designFisher12$alphaSpent, tolerance = 1e-07)
	    expect_equal(designFisher12CodeBased$criticalValues, designFisher12$criticalValues, tolerance = 1e-07)
	    expect_equal(designFisher12CodeBased$stageLevels, designFisher12$stageLevels, tolerance = 1e-07)
	    expect_equal(designFisher12CodeBased$nonStochasticCurtailment, designFisher12$nonStochasticCurtailment, tolerance = 1e-07)
	    expect_type(names(designFisher12), "character")
	    df <- as.data.frame(designFisher12)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(designFisher12)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignFisher': illegal arguments throw exceptions as expected", {

	expect_error(getDesignFisher(
	    method = C_FISHER_METHOD_USER_DEFINED_ALPHA,
	    userAlphaSpending = c(0.01, 0.02, 0.023, 0.023, 0.025), kMax = 4
	),
	paste0(
	    "Conflicting arguments: length of 'userAlphaSpending' (5) ",
	    "must be equal to 'kMax' (4)"
	),
	fixed = TRUE
	)

	expect_error(getDesignFisher(
	    method = C_FISHER_METHOD_USER_DEFINED_ALPHA,
	    userAlphaSpending = c(0.01, 0.02, 0.025), informationRates = c(0.5, 1)
	),
	paste0(
	    "Conflicting arguments: length of 'userAlphaSpending' (3) ",
	    "must be equal to length of 'informationRates' (2)"
	),
	fixed = TRUE
	)

	expect_error(getDesignFisher(
	    method = C_FISHER_METHOD_USER_DEFINED_ALPHA,
	    userAlphaSpending = c(0.01, 0.02, 0.025), informationRates = c(0.4, 1)
	),
	paste0(
	    "Conflicting arguments: length of 'userAlphaSpending' (3) ",
	    "must be equal to length of 'informationRates' (2)"
	),
	fixed = TRUE
	)

	expect_error(getDesignFisher(
	    method = C_FISHER_METHOD_USER_DEFINED_ALPHA,
	    userAlphaSpending = c(0.01, 0.02, 0.023, 0.023, 0.021)
	),
	paste0(
	    "'userAlphaSpending' = c(0.01, 0.02, 0.023, 0.023, 0.021) must be a vector that ",
	    "satisfies the following condition: 0 <= alpha_1 <= .. <= alpha_5 <= alpha = 0.021"
	),
	fixed = TRUE
	)

	expect_error(getDesignFisher(
	    method = C_FISHER_METHOD_USER_DEFINED_ALPHA,
	    userAlphaSpending = c(0.01, 0.02, 0.023), alpha = 0.02
	),
	paste0(
	    "'userAlphaSpending' = c(0.01, 0.02, 0.023) must be a vector that ",
	    "satisfies the following condition: 0 <= alpha_1 <= .. <= alpha_3 <= alpha = 0.02"
	),
	fixed = TRUE
	)

	expect_equal(getDesignFisher(
	    method = C_FISHER_METHOD_USER_DEFINED_ALPHA,
	    userAlphaSpending = c(0.01, 0.02, 0.023)
	)$alpha, 0.023)

	expect_error(getDesignFisher(method = C_FISHER_METHOD_USER_DEFINED_ALPHA),
	    "Missing argument: parameter 'userAlphaSpending' must be specified in design",
	    fixed = TRUE
	)

	expect_error(getDesignFisher(kMax = Inf),
	    paste0(
	        "Argument out of bounds: 'kMax' (Inf) is out of bounds [1; ",
	        C_KMAX_UPPER_BOUND_FISHER, "]"
	    ),
	    fixed = TRUE
	)

	expect_error(getDesignFisher(kMax = -Inf),
	    paste0(
	        "Argument out of bounds: 'kMax' (-Inf) is out of bounds [1; ",
	        C_KMAX_UPPER_BOUND_FISHER, "]"
	    ),
	    fixed = TRUE
	)

	expect_error(getDesignFisher(kMax = -Inf), "Argument out of bounds: 'kMax' (-Inf) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -10), "Argument out of bounds: 'kMax' (-10) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -9), "Argument out of bounds: 'kMax' (-9) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -8), "Argument out of bounds: 'kMax' (-8) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -7), "Argument out of bounds: 'kMax' (-7) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -6), "Argument out of bounds: 'kMax' (-6) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -5), "Argument out of bounds: 'kMax' (-5) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -4), "Argument out of bounds: 'kMax' (-4) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -3), "Argument out of bounds: 'kMax' (-3) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -2), "Argument out of bounds: 'kMax' (-2) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -1), "Argument out of bounds: 'kMax' (-1) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 0), "Argument out of bounds: 'kMax' (0) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 7), "Argument out of bounds: 'kMax' (7) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 8), "Argument out of bounds: 'kMax' (8) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 9), "Argument out of bounds: 'kMax' (9) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 10), "Argument out of bounds: 'kMax' (10) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 11), "Argument out of bounds: 'kMax' (11) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 12), "Argument out of bounds: 'kMax' (12) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 13), "Argument out of bounds: 'kMax' (13) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 14), "Argument out of bounds: 'kMax' (14) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 15), "Argument out of bounds: 'kMax' (15) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 16), "Argument out of bounds: 'kMax' (16) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = Inf), "Argument out of bounds: 'kMax' (Inf) is out of bounds [1; 6]", fixed = TRUE)

	expect_error(getDesignFisher(kMax = 2, informationRates = c(0.01, 0.02, 0.04, 0.05)), "Conflicting arguments: length of 'informationRates' (4) must be equal to 'kMax' (2)", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 3, informationRates = c(0.01, 0.02, 0.04, 0.05)), "Conflicting arguments: length of 'informationRates' (4) must be equal to 'kMax' (3)", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 5, informationRates = c(0.01, 0.02, 0.04, 0.05)), "Conflicting arguments: length of 'informationRates' (4) must be equal to 'kMax' (5)", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 6, informationRates = c(0.01, 0.02, 0.04, 0.05)), "Conflicting arguments: length of 'informationRates' (4) must be equal to 'kMax' (6)", fixed = TRUE)

	expect_error(getDesignFisher(alpha0Vec = c(0, 1)),
	    "Argument out of bounds: 'alpha0Vec' (0, 1) is out of bounds (0; 1]",
	    fixed = TRUE
	)

	expect_error(getDesignFisher(alpha0Vec = c(0.1, 1.01)),
	    "Argument out of bounds: 'alpha0Vec' (0.1, 1.01) is out of bounds (0; 1]",
	    fixed = TRUE
	)

})

