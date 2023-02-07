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
## |  File name: test-class_time.R
## |  Creation date: 06 February 2023, 12:04:17
## |  File version: $Revision: 6801 $
## |  Last changed: $Date: 2023-02-06 15:29:57 +0100 (Mon, 06 Feb 2023) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing Class 'PiecewiseSurvivalTime'")


test_that("Testing 'getPiecewiseSurvivalTime': isPiecewiseSurvivalEnabled()", {
	# @refFS[Tab.]{fs:tab:output:getPiecewiseSurvivalTime}
	expect_false(getPiecewiseSurvivalTime()$isPiecewiseSurvivalEnabled())
	expect_false(getPiecewiseSurvivalTime(piecewiseSurvivalTime = NA)$isPiecewiseSurvivalEnabled())

})

test_that("Testing 'getPiecewiseSurvivalTime': simple vector based definition", {

	# @refFS[Tab.]{fs:tab:output:getPiecewiseSurvivalTime}
	pwSurvivalTime1 <- getPiecewiseSurvivalTime(lambda2 = 0.5, hazardRatio = 0.8)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime1' with expected results
	expect_equal(pwSurvivalTime1$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime1$lambda1, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$lambda2, 0.5, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$pi1, NA_real_)
	expect_equal(pwSurvivalTime1$pi2, NA_real_)
	expect_equal(pwSurvivalTime1$median1, 1.732868, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$median2, 1.3862944, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$eventTime, NA_real_)
	expect_equal(pwSurvivalTime1$kappa, 1)
	expect_equal(pwSurvivalTime1$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime1$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime1$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime1), NA)))
	    expect_output(print(pwSurvivalTime1)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime1), NA)))
	    expect_output(summary(pwSurvivalTime1)$show())
	    pwSurvivalTime1CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime1, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime1CodeBased$piecewiseSurvivalTime, pwSurvivalTime1$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$lambda1, pwSurvivalTime1$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$lambda2, pwSurvivalTime1$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$hazardRatio, pwSurvivalTime1$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$pi1, pwSurvivalTime1$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$pi2, pwSurvivalTime1$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$median1, pwSurvivalTime1$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$median2, pwSurvivalTime1$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$eventTime, pwSurvivalTime1$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$kappa, pwSurvivalTime1$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime1$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$delayedResponseAllowed, pwSurvivalTime1$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$delayedResponseEnabled, pwSurvivalTime1$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime1), "character")
	    df <- as.data.frame(pwSurvivalTime1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime2 <- getPiecewiseSurvivalTime(lambda2 = 0.5, lambda1 = 0.4)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime2' with expected results
	expect_equal(pwSurvivalTime2$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime2$lambda1, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$lambda2, 0.5, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$pi1, NA_real_)
	expect_equal(pwSurvivalTime2$pi2, NA_real_)
	expect_equal(pwSurvivalTime2$median1, 1.732868, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$median2, 1.3862944, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$eventTime, NA_real_)
	expect_equal(pwSurvivalTime2$kappa, 1)
	expect_equal(pwSurvivalTime2$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime2$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime2$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime2), NA)))
	    expect_output(print(pwSurvivalTime2)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime2), NA)))
	    expect_output(summary(pwSurvivalTime2)$show())
	    pwSurvivalTime2CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime2, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime2CodeBased$piecewiseSurvivalTime, pwSurvivalTime2$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$lambda1, pwSurvivalTime2$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$lambda2, pwSurvivalTime2$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$hazardRatio, pwSurvivalTime2$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$pi1, pwSurvivalTime2$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$pi2, pwSurvivalTime2$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$median1, pwSurvivalTime2$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$median2, pwSurvivalTime2$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$eventTime, pwSurvivalTime2$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$kappa, pwSurvivalTime2$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime2$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$delayedResponseAllowed, pwSurvivalTime2$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$delayedResponseEnabled, pwSurvivalTime2$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime2), "character")
	    df <- as.data.frame(pwSurvivalTime2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

	pwSurvivalTime2 <- getPiecewiseSurvivalTime(pi2 = 0.5, hazardRatio = 0.8)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime2' with expected results
	expect_equal(pwSurvivalTime2$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime2$lambda1, 0.046209812, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$lambda2, 0.057762265, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$pi1, 0.42565082, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$pi2, 0.5, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$median1, 15)
	expect_equal(pwSurvivalTime2$median2, 12)
	expect_equal(pwSurvivalTime2$eventTime, 12)
	expect_equal(pwSurvivalTime2$kappa, 1)
	expect_equal(pwSurvivalTime2$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime2$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime2$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime2), NA)))
	    expect_output(print(pwSurvivalTime2)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime2), NA)))
	    expect_output(summary(pwSurvivalTime2)$show())
	    pwSurvivalTime2CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime2, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime2CodeBased$piecewiseSurvivalTime, pwSurvivalTime2$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$lambda1, pwSurvivalTime2$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$lambda2, pwSurvivalTime2$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$hazardRatio, pwSurvivalTime2$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$pi1, pwSurvivalTime2$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$pi2, pwSurvivalTime2$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$median1, pwSurvivalTime2$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$median2, pwSurvivalTime2$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$eventTime, pwSurvivalTime2$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$kappa, pwSurvivalTime2$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime2$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$delayedResponseAllowed, pwSurvivalTime2$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$delayedResponseEnabled, pwSurvivalTime2$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime2), "character")
	    df <- as.data.frame(pwSurvivalTime2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime2 <- getPiecewiseSurvivalTime(pi2 = 0.5, pi1 = 0.4)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime2' with expected results
	expect_equal(pwSurvivalTime2$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime2$lambda1, 0.042568802, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$lambda2, 0.057762265, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$hazardRatio, 0.73696559, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$pi1, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$pi2, 0.5, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$median1, 16.282985, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$median2, 12)
	expect_equal(pwSurvivalTime2$eventTime, 12)
	expect_equal(pwSurvivalTime2$kappa, 1)
	expect_equal(pwSurvivalTime2$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime2$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime2$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime2), NA)))
	    expect_output(print(pwSurvivalTime2)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime2), NA)))
	    expect_output(summary(pwSurvivalTime2)$show())
	    pwSurvivalTime2CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime2, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime2CodeBased$piecewiseSurvivalTime, pwSurvivalTime2$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$lambda1, pwSurvivalTime2$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$lambda2, pwSurvivalTime2$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$hazardRatio, pwSurvivalTime2$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$pi1, pwSurvivalTime2$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$pi2, pwSurvivalTime2$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$median1, pwSurvivalTime2$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$median2, pwSurvivalTime2$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$eventTime, pwSurvivalTime2$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$kappa, pwSurvivalTime2$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime2$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$delayedResponseAllowed, pwSurvivalTime2$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$delayedResponseEnabled, pwSurvivalTime2$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime2), "character")
	    df <- as.data.frame(pwSurvivalTime2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime3 <- getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), lambda2 = 0.4)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime3' with expected results
	expect_equal(pwSurvivalTime3$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime3$lambda1, c(0.24, 0.32), tolerance = 1e-07)
	expect_equal(pwSurvivalTime3$lambda2, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime3$hazardRatio, c(0.6, 0.8), tolerance = 1e-07)
	expect_equal(pwSurvivalTime3$pi1, NA_real_)
	expect_equal(pwSurvivalTime3$pi2, NA_real_)
	expect_equal(pwSurvivalTime3$median1, c(2.8881133, 2.1660849), tolerance = 1e-07)
	expect_equal(pwSurvivalTime3$median2, 1.732868, tolerance = 1e-07)
	expect_equal(pwSurvivalTime3$eventTime, NA_real_)
	expect_equal(pwSurvivalTime3$kappa, 1)
	expect_equal(pwSurvivalTime3$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime3$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime3$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime3), NA)))
	    expect_output(print(pwSurvivalTime3)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime3), NA)))
	    expect_output(summary(pwSurvivalTime3)$show())
	    pwSurvivalTime3CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime3, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime3CodeBased$piecewiseSurvivalTime, pwSurvivalTime3$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$lambda1, pwSurvivalTime3$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$lambda2, pwSurvivalTime3$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$hazardRatio, pwSurvivalTime3$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$pi1, pwSurvivalTime3$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$pi2, pwSurvivalTime3$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$median1, pwSurvivalTime3$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$median2, pwSurvivalTime3$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$eventTime, pwSurvivalTime3$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$kappa, pwSurvivalTime3$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime3$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$delayedResponseAllowed, pwSurvivalTime3$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$delayedResponseEnabled, pwSurvivalTime3$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime3), "character")
	    df <- as.data.frame(pwSurvivalTime3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime8 <- getPiecewiseSurvivalTime(pi2 = 0.4, pi1 = 0.3)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime8' with expected results
	expect_equal(pwSurvivalTime8$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime8$lambda1, 0.029722912, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$lambda2, 0.042568802, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$hazardRatio, 0.69823229, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$pi1, 0.3, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$pi2, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$median1, 23.320299, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$median2, 16.282985, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$eventTime, 12)
	expect_equal(pwSurvivalTime8$kappa, 1)
	expect_equal(pwSurvivalTime8$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime8$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime8$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime8), NA)))
	    expect_output(print(pwSurvivalTime8)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime8), NA)))
	    expect_output(summary(pwSurvivalTime8)$show())
	    pwSurvivalTime8CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime8, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime8CodeBased$piecewiseSurvivalTime, pwSurvivalTime8$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$lambda1, pwSurvivalTime8$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$lambda2, pwSurvivalTime8$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$hazardRatio, pwSurvivalTime8$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$pi1, pwSurvivalTime8$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$pi2, pwSurvivalTime8$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$median1, pwSurvivalTime8$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$median2, pwSurvivalTime8$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$eventTime, pwSurvivalTime8$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$kappa, pwSurvivalTime8$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime8$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$delayedResponseAllowed, pwSurvivalTime8$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$delayedResponseEnabled, pwSurvivalTime8$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime8), "character")
	    df <- as.data.frame(pwSurvivalTime8)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime8)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime9 <- getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), pi2 = 0.3)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime9' with expected results
	expect_equal(pwSurvivalTime9$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime9$lambda1, c(0.017833747, 0.02377833), tolerance = 1e-07)
	expect_equal(pwSurvivalTime9$lambda2, 0.029722912, tolerance = 1e-07)
	expect_equal(pwSurvivalTime9$hazardRatio, c(0.6, 0.8), tolerance = 1e-07)
	expect_equal(pwSurvivalTime9$pi1, c(0.19265562, 0.24824135), tolerance = 1e-07)
	expect_equal(pwSurvivalTime9$pi2, 0.3, tolerance = 1e-07)
	expect_equal(pwSurvivalTime9$median1, c(38.867164, 29.150373), tolerance = 1e-07)
	expect_equal(pwSurvivalTime9$median2, 23.320299, tolerance = 1e-07)
	expect_equal(pwSurvivalTime9$eventTime, 12)
	expect_equal(pwSurvivalTime9$kappa, 1)
	expect_equal(pwSurvivalTime9$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime9$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime9$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime9), NA)))
	    expect_output(print(pwSurvivalTime9)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime9), NA)))
	    expect_output(summary(pwSurvivalTime9)$show())
	    pwSurvivalTime9CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime9, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime9CodeBased$piecewiseSurvivalTime, pwSurvivalTime9$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime9CodeBased$lambda1, pwSurvivalTime9$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime9CodeBased$lambda2, pwSurvivalTime9$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime9CodeBased$hazardRatio, pwSurvivalTime9$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime9CodeBased$pi1, pwSurvivalTime9$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime9CodeBased$pi2, pwSurvivalTime9$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime9CodeBased$median1, pwSurvivalTime9$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime9CodeBased$median2, pwSurvivalTime9$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime9CodeBased$eventTime, pwSurvivalTime9$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime9CodeBased$kappa, pwSurvivalTime9$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime9CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime9$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime9CodeBased$delayedResponseAllowed, pwSurvivalTime9$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime9CodeBased$delayedResponseEnabled, pwSurvivalTime9$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime9), "character")
	    df <- as.data.frame(pwSurvivalTime9)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime9)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime10 <- getPiecewiseSurvivalTime(median2 = 1.386294, hazardRatio = 0.8)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime10' with expected results
	expect_equal(pwSurvivalTime10$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime10$lambda1, 0.4000001, tolerance = 1e-07)
	expect_equal(pwSurvivalTime10$lambda2, 0.50000013, tolerance = 1e-07)
	expect_equal(pwSurvivalTime10$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime10$pi1, NA_real_)
	expect_equal(pwSurvivalTime10$pi2, NA_real_)
	expect_equal(pwSurvivalTime10$median1, 1.7328675, tolerance = 1e-07)
	expect_equal(pwSurvivalTime10$median2, 1.386294, tolerance = 1e-07)
	expect_equal(pwSurvivalTime10$eventTime, NA_real_)
	expect_equal(pwSurvivalTime10$kappa, 1)
	expect_equal(pwSurvivalTime10$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime10$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime10$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime10), NA)))
	    expect_output(print(pwSurvivalTime10)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime10), NA)))
	    expect_output(summary(pwSurvivalTime10)$show())
	    pwSurvivalTime10CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime10, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime10CodeBased$piecewiseSurvivalTime, pwSurvivalTime10$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$lambda1, pwSurvivalTime10$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$lambda2, pwSurvivalTime10$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$hazardRatio, pwSurvivalTime10$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$pi1, pwSurvivalTime10$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$pi2, pwSurvivalTime10$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$median1, pwSurvivalTime10$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$median2, pwSurvivalTime10$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$eventTime, pwSurvivalTime10$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$kappa, pwSurvivalTime10$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime10$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$delayedResponseAllowed, pwSurvivalTime10$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$delayedResponseEnabled, pwSurvivalTime10$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime10), "character")
	    df <- as.data.frame(pwSurvivalTime10)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime10)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime11 <- getPiecewiseSurvivalTime(median2 = 1.386294, lambda1 = 0.4)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime11' with expected results
	expect_equal(pwSurvivalTime11$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime11$lambda1, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime11$lambda2, 0.50000013, tolerance = 1e-07)
	expect_equal(pwSurvivalTime11$hazardRatio, 0.79999979, tolerance = 1e-07)
	expect_equal(pwSurvivalTime11$pi1, NA_real_)
	expect_equal(pwSurvivalTime11$pi2, NA_real_)
	expect_equal(pwSurvivalTime11$median1, 1.732868, tolerance = 1e-07)
	expect_equal(pwSurvivalTime11$median2, 1.386294, tolerance = 1e-07)
	expect_equal(pwSurvivalTime11$eventTime, NA_real_)
	expect_equal(pwSurvivalTime11$kappa, 1)
	expect_equal(pwSurvivalTime11$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime11$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime11$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime11), NA)))
	    expect_output(print(pwSurvivalTime11)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime11), NA)))
	    expect_output(summary(pwSurvivalTime11)$show())
	    pwSurvivalTime11CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime11, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime11CodeBased$piecewiseSurvivalTime, pwSurvivalTime11$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$lambda1, pwSurvivalTime11$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$lambda2, pwSurvivalTime11$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$hazardRatio, pwSurvivalTime11$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$pi1, pwSurvivalTime11$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$pi2, pwSurvivalTime11$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$median1, pwSurvivalTime11$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$median2, pwSurvivalTime11$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$eventTime, pwSurvivalTime11$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$kappa, pwSurvivalTime11$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime11$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$delayedResponseAllowed, pwSurvivalTime11$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$delayedResponseEnabled, pwSurvivalTime11$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime11), "character")
	    df <- as.data.frame(pwSurvivalTime11)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime11)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime12 <- getPiecewiseSurvivalTime(median2 = 5, median1 = 6)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime12' with expected results
	expect_equal(pwSurvivalTime12$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime12$lambda1, 0.11552453, tolerance = 1e-07)
	expect_equal(pwSurvivalTime12$lambda2, 0.13862944, tolerance = 1e-07)
	expect_equal(pwSurvivalTime12$hazardRatio, 0.83333333, tolerance = 1e-07)
	expect_equal(pwSurvivalTime12$pi1, NA_real_)
	expect_equal(pwSurvivalTime12$pi2, NA_real_)
	expect_equal(pwSurvivalTime12$median1, 6)
	expect_equal(pwSurvivalTime12$median2, 5)
	expect_equal(pwSurvivalTime12$eventTime, NA_real_)
	expect_equal(pwSurvivalTime12$kappa, 1)
	expect_equal(pwSurvivalTime12$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime12$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime12$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime12), NA)))
	    expect_output(print(pwSurvivalTime12)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime12), NA)))
	    expect_output(summary(pwSurvivalTime12)$show())
	    pwSurvivalTime12CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime12, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime12CodeBased$piecewiseSurvivalTime, pwSurvivalTime12$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$lambda1, pwSurvivalTime12$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$lambda2, pwSurvivalTime12$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$hazardRatio, pwSurvivalTime12$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$pi1, pwSurvivalTime12$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$pi2, pwSurvivalTime12$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$median1, pwSurvivalTime12$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$median2, pwSurvivalTime12$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$eventTime, pwSurvivalTime12$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$kappa, pwSurvivalTime12$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime12$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$delayedResponseAllowed, pwSurvivalTime12$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$delayedResponseEnabled, pwSurvivalTime12$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime12), "character")
	    df <- as.data.frame(pwSurvivalTime12)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime12)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime13 <- getPiecewiseSurvivalTime(median2 = 1.386294, lambda1 = c(0.3, 0.4))

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime13' with expected results
	expect_equal(pwSurvivalTime13$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime13$lambda1, c(0.3, 0.4), tolerance = 1e-07)
	expect_equal(pwSurvivalTime13$lambda2, 0.50000013, tolerance = 1e-07)
	expect_equal(pwSurvivalTime13$hazardRatio, c(0.59999984, 0.79999979), tolerance = 1e-07)
	expect_equal(pwSurvivalTime13$pi1, NA_real_)
	expect_equal(pwSurvivalTime13$pi2, NA_real_)
	expect_equal(pwSurvivalTime13$median1, c(2.3104906, 1.732868), tolerance = 1e-07)
	expect_equal(pwSurvivalTime13$median2, 1.386294, tolerance = 1e-07)
	expect_equal(pwSurvivalTime13$eventTime, NA_real_)
	expect_equal(pwSurvivalTime13$kappa, 1)
	expect_equal(pwSurvivalTime13$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime13$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime13$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime13), NA)))
	    expect_output(print(pwSurvivalTime13)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime13), NA)))
	    expect_output(summary(pwSurvivalTime13)$show())
	    pwSurvivalTime13CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime13, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime13CodeBased$piecewiseSurvivalTime, pwSurvivalTime13$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$lambda1, pwSurvivalTime13$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$lambda2, pwSurvivalTime13$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$hazardRatio, pwSurvivalTime13$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$pi1, pwSurvivalTime13$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$pi2, pwSurvivalTime13$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$median1, pwSurvivalTime13$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$median2, pwSurvivalTime13$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$eventTime, pwSurvivalTime13$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$kappa, pwSurvivalTime13$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime13$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$delayedResponseAllowed, pwSurvivalTime13$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$delayedResponseEnabled, pwSurvivalTime13$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime13), "character")
	    df <- as.data.frame(pwSurvivalTime13)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime13)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime14 <- getPiecewiseSurvivalTime(median2 = 5, median1 = c(6:8))

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime14' with expected results
	expect_equal(pwSurvivalTime14$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime14$lambda1, c(0.11552453, 0.099021026, 0.086643398), tolerance = 1e-07)
	expect_equal(pwSurvivalTime14$lambda2, 0.13862944, tolerance = 1e-07)
	expect_equal(pwSurvivalTime14$hazardRatio, c(0.83333333, 0.71428571, 0.625), tolerance = 1e-07)
	expect_equal(pwSurvivalTime14$pi1, NA_real_)
	expect_equal(pwSurvivalTime14$pi2, NA_real_)
	expect_equal(pwSurvivalTime14$median1, c(6, 7, 8))
	expect_equal(pwSurvivalTime14$median2, 5)
	expect_equal(pwSurvivalTime14$eventTime, NA_real_)
	expect_equal(pwSurvivalTime14$kappa, 1)
	expect_equal(pwSurvivalTime14$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime14$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime14$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime14), NA)))
	    expect_output(print(pwSurvivalTime14)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime14), NA)))
	    expect_output(summary(pwSurvivalTime14)$show())
	    pwSurvivalTime14CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime14, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime14CodeBased$piecewiseSurvivalTime, pwSurvivalTime14$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime14CodeBased$lambda1, pwSurvivalTime14$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime14CodeBased$lambda2, pwSurvivalTime14$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime14CodeBased$hazardRatio, pwSurvivalTime14$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime14CodeBased$pi1, pwSurvivalTime14$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime14CodeBased$pi2, pwSurvivalTime14$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime14CodeBased$median1, pwSurvivalTime14$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime14CodeBased$median2, pwSurvivalTime14$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime14CodeBased$eventTime, pwSurvivalTime14$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime14CodeBased$kappa, pwSurvivalTime14$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime14CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime14$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime14CodeBased$delayedResponseAllowed, pwSurvivalTime14$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime14CodeBased$delayedResponseEnabled, pwSurvivalTime14$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime14), "character")
	    df <- as.data.frame(pwSurvivalTime14)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime14)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime15 <- getPiecewiseSurvivalTime(median2 = 2, hazardRatio = 0.8)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime15' with expected results
	expect_equal(pwSurvivalTime15$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime15$lambda1, 0.27725887, tolerance = 1e-07)
	expect_equal(pwSurvivalTime15$lambda2, 0.34657359, tolerance = 1e-07)
	expect_equal(pwSurvivalTime15$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime15$pi1, NA_real_)
	expect_equal(pwSurvivalTime15$pi2, NA_real_)
	expect_equal(pwSurvivalTime15$median1, 2.5, tolerance = 1e-07)
	expect_equal(pwSurvivalTime15$median2, 2)
	expect_equal(pwSurvivalTime15$eventTime, NA_real_)
	expect_equal(pwSurvivalTime15$kappa, 1)
	expect_equal(pwSurvivalTime15$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime15$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime15$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime15), NA)))
	    expect_output(print(pwSurvivalTime15)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime15), NA)))
	    expect_output(summary(pwSurvivalTime15)$show())
	    pwSurvivalTime15CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime15, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime15CodeBased$piecewiseSurvivalTime, pwSurvivalTime15$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime15CodeBased$lambda1, pwSurvivalTime15$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime15CodeBased$lambda2, pwSurvivalTime15$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime15CodeBased$hazardRatio, pwSurvivalTime15$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime15CodeBased$pi1, pwSurvivalTime15$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime15CodeBased$pi2, pwSurvivalTime15$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime15CodeBased$median1, pwSurvivalTime15$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime15CodeBased$median2, pwSurvivalTime15$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime15CodeBased$eventTime, pwSurvivalTime15$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime15CodeBased$kappa, pwSurvivalTime15$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime15CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime15$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime15CodeBased$delayedResponseAllowed, pwSurvivalTime15$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime15CodeBased$delayedResponseEnabled, pwSurvivalTime15$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime15), "character")
	    df <- as.data.frame(pwSurvivalTime15)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime15)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime16 <- getPiecewiseSurvivalTime(median1 = c(2, 2), hazardRatio = c(1.4, 1.4))

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime16' with expected results
	expect_equal(pwSurvivalTime16$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime16$lambda1, c(0.34657359, 0.34657359), tolerance = 1e-07)
	expect_equal(pwSurvivalTime16$lambda2, c(0.24755256, 0.24755256), tolerance = 1e-07)
	expect_equal(pwSurvivalTime16$hazardRatio, c(1.4, 1.4), tolerance = 1e-07)
	expect_equal(pwSurvivalTime16$pi1, NA_real_)
	expect_equal(pwSurvivalTime16$pi2, NA_real_)
	expect_equal(pwSurvivalTime16$median1, c(2, 2))
	expect_equal(pwSurvivalTime16$median2, c(2.8, 2.8), tolerance = 1e-07)
	expect_equal(pwSurvivalTime16$eventTime, NA_real_)
	expect_equal(pwSurvivalTime16$kappa, 1)
	expect_equal(pwSurvivalTime16$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime16$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime16$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime16), NA)))
	    expect_output(print(pwSurvivalTime16)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime16), NA)))
	    expect_output(summary(pwSurvivalTime16)$show())
	    pwSurvivalTime16CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime16, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime16CodeBased$piecewiseSurvivalTime, pwSurvivalTime16$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime16CodeBased$lambda1, pwSurvivalTime16$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime16CodeBased$lambda2, pwSurvivalTime16$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime16CodeBased$hazardRatio, pwSurvivalTime16$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime16CodeBased$pi1, pwSurvivalTime16$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime16CodeBased$pi2, pwSurvivalTime16$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime16CodeBased$median1, pwSurvivalTime16$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime16CodeBased$median2, pwSurvivalTime16$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime16CodeBased$eventTime, pwSurvivalTime16$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime16CodeBased$kappa, pwSurvivalTime16$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime16CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime16$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime16CodeBased$delayedResponseAllowed, pwSurvivalTime16$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime16CodeBased$delayedResponseEnabled, pwSurvivalTime16$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime16), "character")
	    df <- as.data.frame(pwSurvivalTime16)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime16)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime17 <- getPiecewiseSurvivalTime(median1 = c(2, 3), median2 = 4)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime17' with expected results
	expect_equal(pwSurvivalTime17$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime17$lambda1, c(0.34657359, 0.23104906), tolerance = 1e-07)
	expect_equal(pwSurvivalTime17$lambda2, 0.1732868, tolerance = 1e-07)
	expect_equal(pwSurvivalTime17$hazardRatio, c(2, 1.3333333), tolerance = 1e-07)
	expect_equal(pwSurvivalTime17$pi1, NA_real_)
	expect_equal(pwSurvivalTime17$pi2, NA_real_)
	expect_equal(pwSurvivalTime17$median1, c(2, 3))
	expect_equal(pwSurvivalTime17$median2, 4)
	expect_equal(pwSurvivalTime17$eventTime, NA_real_)
	expect_equal(pwSurvivalTime17$kappa, 1)
	expect_equal(pwSurvivalTime17$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime17$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime17$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime17), NA)))
	    expect_output(print(pwSurvivalTime17)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime17), NA)))
	    expect_output(summary(pwSurvivalTime17)$show())
	    pwSurvivalTime17CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime17, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime17CodeBased$piecewiseSurvivalTime, pwSurvivalTime17$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime17CodeBased$lambda1, pwSurvivalTime17$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime17CodeBased$lambda2, pwSurvivalTime17$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime17CodeBased$hazardRatio, pwSurvivalTime17$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime17CodeBased$pi1, pwSurvivalTime17$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime17CodeBased$pi2, pwSurvivalTime17$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime17CodeBased$median1, pwSurvivalTime17$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime17CodeBased$median2, pwSurvivalTime17$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime17CodeBased$eventTime, pwSurvivalTime17$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime17CodeBased$kappa, pwSurvivalTime17$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime17CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime17$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime17CodeBased$delayedResponseAllowed, pwSurvivalTime17$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime17CodeBased$delayedResponseEnabled, pwSurvivalTime17$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime17), "character")
	    df <- as.data.frame(pwSurvivalTime17)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime17)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime18 <- getPiecewiseSurvivalTime(median1 = c(2, 3), lambda2 = 0.4)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime18' with expected results
	expect_equal(pwSurvivalTime18$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime18$lambda1, c(0.34657359, 0.23104906), tolerance = 1e-07)
	expect_equal(pwSurvivalTime18$lambda2, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime18$hazardRatio, c(0.86643398, 0.57762265), tolerance = 1e-07)
	expect_equal(pwSurvivalTime18$pi1, NA_real_)
	expect_equal(pwSurvivalTime18$pi2, NA_real_)
	expect_equal(pwSurvivalTime18$median1, c(2, 3))
	expect_equal(pwSurvivalTime18$median2, 1.732868, tolerance = 1e-07)
	expect_equal(pwSurvivalTime18$eventTime, NA_real_)
	expect_equal(pwSurvivalTime18$kappa, 1)
	expect_equal(pwSurvivalTime18$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime18$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime18$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime18), NA)))
	    expect_output(print(pwSurvivalTime18)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime18), NA)))
	    expect_output(summary(pwSurvivalTime18)$show())
	    pwSurvivalTime18CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime18, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime18CodeBased$piecewiseSurvivalTime, pwSurvivalTime18$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime18CodeBased$lambda1, pwSurvivalTime18$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime18CodeBased$lambda2, pwSurvivalTime18$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime18CodeBased$hazardRatio, pwSurvivalTime18$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime18CodeBased$pi1, pwSurvivalTime18$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime18CodeBased$pi2, pwSurvivalTime18$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime18CodeBased$median1, pwSurvivalTime18$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime18CodeBased$median2, pwSurvivalTime18$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime18CodeBased$eventTime, pwSurvivalTime18$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime18CodeBased$kappa, pwSurvivalTime18$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime18CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime18$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime18CodeBased$delayedResponseAllowed, pwSurvivalTime18$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime18CodeBased$delayedResponseEnabled, pwSurvivalTime18$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime18), "character")
	    df <- as.data.frame(pwSurvivalTime18)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime18)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime19 <- getPiecewiseSurvivalTime(pi1 = 0.45)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime19' with expected results
	expect_equal(pwSurvivalTime19$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime19$lambda1, 0.04981975, tolerance = 1e-07)
	expect_equal(pwSurvivalTime19$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(pwSurvivalTime19$hazardRatio, 2.6791588, tolerance = 1e-07)
	expect_equal(pwSurvivalTime19$pi1, 0.45, tolerance = 1e-07)
	expect_equal(pwSurvivalTime19$pi2, 0.2, tolerance = 1e-07)
	expect_equal(pwSurvivalTime19$median1, 13.9131, tolerance = 1e-07)
	expect_equal(pwSurvivalTime19$median2, 37.275405, tolerance = 1e-07)
	expect_equal(pwSurvivalTime19$eventTime, 12)
	expect_equal(pwSurvivalTime19$kappa, 1)
	expect_equal(pwSurvivalTime19$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime19$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime19$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime19), NA)))
	    expect_output(print(pwSurvivalTime19)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime19), NA)))
	    expect_output(summary(pwSurvivalTime19)$show())
	    pwSurvivalTime19CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime19, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime19CodeBased$piecewiseSurvivalTime, pwSurvivalTime19$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime19CodeBased$lambda1, pwSurvivalTime19$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime19CodeBased$lambda2, pwSurvivalTime19$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime19CodeBased$hazardRatio, pwSurvivalTime19$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime19CodeBased$pi1, pwSurvivalTime19$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime19CodeBased$pi2, pwSurvivalTime19$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime19CodeBased$median1, pwSurvivalTime19$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime19CodeBased$median2, pwSurvivalTime19$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime19CodeBased$eventTime, pwSurvivalTime19$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime19CodeBased$kappa, pwSurvivalTime19$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime19CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime19$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime19CodeBased$delayedResponseAllowed, pwSurvivalTime19$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime19CodeBased$delayedResponseEnabled, pwSurvivalTime19$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime19), "character")
	    df <- as.data.frame(pwSurvivalTime19)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime19)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime20 <- getPiecewiseSurvivalTime(median1 = c(2, 4), hazardRatio = c(1.4, 0.7))

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime20' with expected results
	expect_equal(pwSurvivalTime20$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime20$lambda1, c(0.34657359, 0.1732868), tolerance = 1e-07)
	expect_equal(pwSurvivalTime20$lambda2, c(0.24755256, 0.24755256), tolerance = 1e-07)
	expect_equal(pwSurvivalTime20$hazardRatio, c(1.4, 0.7), tolerance = 1e-07)
	expect_equal(pwSurvivalTime20$pi1, NA_real_)
	expect_equal(pwSurvivalTime20$pi2, NA_real_)
	expect_equal(pwSurvivalTime20$median1, c(2, 4))
	expect_equal(pwSurvivalTime20$median2, c(2.8, 2.8), tolerance = 1e-07)
	expect_equal(pwSurvivalTime20$eventTime, NA_real_)
	expect_equal(pwSurvivalTime20$kappa, 1)
	expect_equal(pwSurvivalTime20$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime20$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime20$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime20), NA)))
	    expect_output(print(pwSurvivalTime20)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime20), NA)))
	    expect_output(summary(pwSurvivalTime20)$show())
	    pwSurvivalTime20CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime20, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime20CodeBased$piecewiseSurvivalTime, pwSurvivalTime20$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime20CodeBased$lambda1, pwSurvivalTime20$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime20CodeBased$lambda2, pwSurvivalTime20$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime20CodeBased$hazardRatio, pwSurvivalTime20$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime20CodeBased$pi1, pwSurvivalTime20$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime20CodeBased$pi2, pwSurvivalTime20$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime20CodeBased$median1, pwSurvivalTime20$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime20CodeBased$median2, pwSurvivalTime20$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime20CodeBased$eventTime, pwSurvivalTime20$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime20CodeBased$kappa, pwSurvivalTime20$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime20CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime20$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime20CodeBased$delayedResponseAllowed, pwSurvivalTime20$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime20CodeBased$delayedResponseEnabled, pwSurvivalTime20$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime20), "character")
	    df <- as.data.frame(pwSurvivalTime20)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime20)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime21 <- getPiecewiseSurvivalTime(median1 = 3, hazardRatio = 0.8)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime21' with expected results
	expect_equal(pwSurvivalTime21$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime21$lambda1, 0.23104906, tolerance = 1e-07)
	expect_equal(pwSurvivalTime21$lambda2, 0.28881133, tolerance = 1e-07)
	expect_equal(pwSurvivalTime21$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime21$pi1, NA_real_)
	expect_equal(pwSurvivalTime21$pi2, NA_real_)
	expect_equal(pwSurvivalTime21$median1, 3)
	expect_equal(pwSurvivalTime21$median2, 2.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime21$eventTime, NA_real_)
	expect_equal(pwSurvivalTime21$kappa, 1)
	expect_equal(pwSurvivalTime21$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime21$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime21$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime21), NA)))
	    expect_output(print(pwSurvivalTime21)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime21), NA)))
	    expect_output(summary(pwSurvivalTime21)$show())
	    pwSurvivalTime21CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime21, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime21CodeBased$piecewiseSurvivalTime, pwSurvivalTime21$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime21CodeBased$lambda1, pwSurvivalTime21$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime21CodeBased$lambda2, pwSurvivalTime21$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime21CodeBased$hazardRatio, pwSurvivalTime21$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime21CodeBased$pi1, pwSurvivalTime21$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime21CodeBased$pi2, pwSurvivalTime21$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime21CodeBased$median1, pwSurvivalTime21$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime21CodeBased$median2, pwSurvivalTime21$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime21CodeBased$eventTime, pwSurvivalTime21$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime21CodeBased$kappa, pwSurvivalTime21$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime21CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime21$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime21CodeBased$delayedResponseAllowed, pwSurvivalTime21$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime21CodeBased$delayedResponseEnabled, pwSurvivalTime21$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime21), "character")
	    df <- as.data.frame(pwSurvivalTime21)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime21)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	expect_error(getPiecewiseSurvivalTime(median2 = 1.386294, lambda2 = 0.4, hazardRatio = 0.8))
	expect_error(getPiecewiseSurvivalTime(median2 = c(1.5, 1.7), lambda1 = c(0.3, 0.4)))
	expect_error(getPiecewiseSurvivalTime(median1 = c(2, 4), hazardRatio = c(1, 0.7)))
	expect_error(getPiecewiseSurvivalTime(median1 = c(2, 4), hazardRatio = 0.7))

})

test_that("Testing 'getPiecewiseSurvivalTime': vector based definition", {

	# @refFS[Tab.]{fs:tab:output:getPiecewiseSurvivalTime}
	pwSurvivalTime1 <- getPiecewiseSurvivalTime(
	    piecewiseSurvivalTime = c(0, 6, 9),
	    lambda2 = c(0.025, 0.04, 0.015), hazardRatio = 0.8
	)
	expect_equal(pwSurvivalTime1$hazardRatio, 0.8)
	expect_equal(pwSurvivalTime1$lambda1, c(0.025, 0.04, 0.015) * 0.8)
	expect_false(pwSurvivalTime1$isDelayedResponseEnabled())

	.skipTestIfDisabled()

	pwSurvivalTime2 <- getPiecewiseSurvivalTime(
	    piecewiseSurvivalTime = c(0, 5, 10),
	    lambda2 = c(0.1, 0.2, 0.8), hazardRatio = 0.8
	)
	expect_true(pwSurvivalTime2$isPiecewiseSurvivalEnabled())
	expect_false(pwSurvivalTime2$isDelayedResponseEnabled())
	expect_equal(pwSurvivalTime2$hazardRatio, 0.8)
	expect_equal(pwSurvivalTime2$piecewiseSurvivalTime, c(0, 5, 10))
	expect_equal(pwSurvivalTime2$lambda2, c(0.1, 0.2, 0.8))

	pwSurvivalTime3 <- getPiecewiseSurvivalTime(c(0, 6), lambda2 = c(0.01, 0.03), hazardRatio = 0.8)
	expect_true(pwSurvivalTime3$isPiecewiseSurvivalEnabled())
	expect_false(pwSurvivalTime3$isDelayedResponseEnabled())
	expect_equal(pwSurvivalTime3$hazardRatio, 0.8)
	expect_equal(pwSurvivalTime3$piecewiseSurvivalTime, c(0, 6))
	expect_equal(pwSurvivalTime3$lambda2, c(0.01, 0.03))

	pwSurvivalTime4 <- getPiecewiseSurvivalTime(0, lambda2 = 0.01, hazardRatio = 0.8)
	expect_true(pwSurvivalTime4$isPiecewiseSurvivalEnabled())
	expect_false(pwSurvivalTime4$isDelayedResponseEnabled())
	expect_equal(pwSurvivalTime4$hazardRatio, 0.8)
	expect_equal(pwSurvivalTime4$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime4$lambda2, 0.01)
	expect_equal(pwSurvivalTime4$lambda1, 0.01 * 0.8)

	pwSurvivalTime5 <- getPiecewiseSurvivalTime(NA_real_, lambda2 = 0.01, hazardRatio = 0.8)
	expect_true(pwSurvivalTime5$isPiecewiseSurvivalEnabled())
	expect_false(pwSurvivalTime5$isDelayedResponseEnabled())
	expect_equal(pwSurvivalTime5$hazardRatio, 0.8)
	expect_equal(pwSurvivalTime5$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime5$lambda2, 0.01)
	expect_equal(pwSurvivalTime5$lambda1, 0.01 * 0.8)

	pwSurvivalTime6 <- getPiecewiseSurvivalTime(0, lambda2 = 0.01, lambda1 = 0.008)
	expect_true(pwSurvivalTime6$isPiecewiseSurvivalEnabled())
	expect_false(pwSurvivalTime6$isDelayedResponseEnabled())
	expect_equal(pwSurvivalTime6$hazardRatio, 0.8)
	expect_equal(pwSurvivalTime6$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime6$lambda2, 0.01)
	expect_equal(pwSurvivalTime6$lambda1, 0.008)

	pwSurvivalTime7 <- getPiecewiseSurvivalTime(NA_real_, lambda2 = 0.01, lambda1 = 0.008)
	expect_true(pwSurvivalTime7$isPiecewiseSurvivalEnabled())
	expect_false(pwSurvivalTime7$isDelayedResponseEnabled())
	expect_equal(pwSurvivalTime7$hazardRatio, 0.8)
	expect_equal(pwSurvivalTime7$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime7$lambda2, 0.01)
	expect_equal(pwSurvivalTime7$lambda1, 0.008)

	# case 2.2
	pwSurvivalTime9 <- getPiecewiseSurvivalTime(
	    piecewiseSurvivalTime = c(0, 6, 9),
	    lambda2 = c(0.025, 0.04, 0.015),
	    lambda1 = c(0.025, 0.04, 0.015) * 0.8
	)
	expect_true(pwSurvivalTime9$isPiecewiseSurvivalEnabled())
	expect_false(pwSurvivalTime9$isDelayedResponseEnabled())
	expect_equal(pwSurvivalTime9$hazardRatio, 0.8)

	pwSurvivalTime10 <- getPiecewiseSurvivalTime(lambda2 = 0.025, hazardRatio = 0.8)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime10' with expected results
	expect_equal(pwSurvivalTime10$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime10$lambda1, 0.02, tolerance = 1e-07)
	expect_equal(pwSurvivalTime10$lambda2, 0.025, tolerance = 1e-07)
	expect_equal(pwSurvivalTime10$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime10$pi1, NA_real_)
	expect_equal(pwSurvivalTime10$pi2, NA_real_)
	expect_equal(pwSurvivalTime10$median1, 34.657359, tolerance = 1e-07)
	expect_equal(pwSurvivalTime10$median2, 27.725887, tolerance = 1e-07)
	expect_equal(pwSurvivalTime10$eventTime, NA_real_)
	expect_equal(pwSurvivalTime10$kappa, 1)
	expect_equal(pwSurvivalTime10$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime10$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime10$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime10), NA)))
	    expect_output(print(pwSurvivalTime10)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime10), NA)))
	    expect_output(summary(pwSurvivalTime10)$show())
	    pwSurvivalTime10CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime10, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime10CodeBased$piecewiseSurvivalTime, pwSurvivalTime10$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$lambda1, pwSurvivalTime10$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$lambda2, pwSurvivalTime10$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$hazardRatio, pwSurvivalTime10$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$pi1, pwSurvivalTime10$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$pi2, pwSurvivalTime10$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$median1, pwSurvivalTime10$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$median2, pwSurvivalTime10$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$eventTime, pwSurvivalTime10$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$kappa, pwSurvivalTime10$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime10$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$delayedResponseAllowed, pwSurvivalTime10$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime10CodeBased$delayedResponseEnabled, pwSurvivalTime10$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime10), "character")
	    df <- as.data.frame(pwSurvivalTime10)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime10)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime11 <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = 0, lambda2 = 0.025, hazardRatio = 0.8)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime11' with expected results
	expect_equal(pwSurvivalTime11$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime11$lambda1, 0.02, tolerance = 1e-07)
	expect_equal(pwSurvivalTime11$lambda2, 0.025, tolerance = 1e-07)
	expect_equal(pwSurvivalTime11$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime11$pi1, NA_real_)
	expect_equal(pwSurvivalTime11$pi2, NA_real_)
	expect_equal(pwSurvivalTime11$median1, 34.657359, tolerance = 1e-07)
	expect_equal(pwSurvivalTime11$median2, 27.725887, tolerance = 1e-07)
	expect_equal(pwSurvivalTime11$eventTime, NA_real_)
	expect_equal(pwSurvivalTime11$kappa, 1)
	expect_equal(pwSurvivalTime11$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime11$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime11$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime11), NA)))
	    expect_output(print(pwSurvivalTime11)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime11), NA)))
	    expect_output(summary(pwSurvivalTime11)$show())
	    pwSurvivalTime11CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime11, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime11CodeBased$piecewiseSurvivalTime, pwSurvivalTime11$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$lambda1, pwSurvivalTime11$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$lambda2, pwSurvivalTime11$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$hazardRatio, pwSurvivalTime11$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$pi1, pwSurvivalTime11$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$pi2, pwSurvivalTime11$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$median1, pwSurvivalTime11$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$median2, pwSurvivalTime11$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$eventTime, pwSurvivalTime11$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$kappa, pwSurvivalTime11$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime11$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$delayedResponseAllowed, pwSurvivalTime11$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime11CodeBased$delayedResponseEnabled, pwSurvivalTime11$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime11), "character")
	    df <- as.data.frame(pwSurvivalTime11)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime11)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime12 <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = c(0, 6), lambda2 = c(0.025, 0.01), hazardRatio = c(0.8, 0.9))

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime12' with expected results
	expect_equal(pwSurvivalTime12$piecewiseSurvivalTime, c(0, 6))
	expect_equal(pwSurvivalTime12$lambda1, NA_real_)
	expect_equal(pwSurvivalTime12$lambda2, c(0.025, 0.01), tolerance = 1e-07)
	expect_equal(pwSurvivalTime12$hazardRatio, c(0.8, 0.9), tolerance = 1e-07)
	expect_equal(pwSurvivalTime12$pi1, NA_real_)
	expect_equal(pwSurvivalTime12$pi2, NA_real_)
	expect_equal(pwSurvivalTime12$median1, NA_real_)
	expect_equal(pwSurvivalTime12$median2, NA_real_)
	expect_equal(pwSurvivalTime12$eventTime, NA_real_)
	expect_equal(pwSurvivalTime12$kappa, 1)
	expect_equal(pwSurvivalTime12$piecewiseSurvivalEnabled, TRUE)
	expect_equal(pwSurvivalTime12$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime12$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime12), NA)))
	    expect_output(print(pwSurvivalTime12)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime12), NA)))
	    expect_output(summary(pwSurvivalTime12)$show())
	    pwSurvivalTime12CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime12, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime12CodeBased$piecewiseSurvivalTime, pwSurvivalTime12$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$lambda1, pwSurvivalTime12$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$lambda2, pwSurvivalTime12$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$hazardRatio, pwSurvivalTime12$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$pi1, pwSurvivalTime12$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$pi2, pwSurvivalTime12$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$median1, pwSurvivalTime12$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$median2, pwSurvivalTime12$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$eventTime, pwSurvivalTime12$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$kappa, pwSurvivalTime12$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime12$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$delayedResponseAllowed, pwSurvivalTime12$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime12CodeBased$delayedResponseEnabled, pwSurvivalTime12$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime12), "character")
	    df <- as.data.frame(pwSurvivalTime12)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime12)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime13 <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = c(0, 6), lambda2 = c(0.025, 0.01), hazardRatio = c(0.8, 0.9), delayedResponseAllowed = TRUE)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime13' with expected results
	expect_equal(pwSurvivalTime13$piecewiseSurvivalTime, c(0, 6))
	expect_equal(pwSurvivalTime13$lambda1, c(0.02, 0.009), tolerance = 1e-07)
	expect_equal(pwSurvivalTime13$lambda2, c(0.025, 0.01), tolerance = 1e-07)
	expect_equal(pwSurvivalTime13$hazardRatio, c(0.8, 0.9), tolerance = 1e-07)
	expect_equal(pwSurvivalTime13$pi1, NA_real_)
	expect_equal(pwSurvivalTime13$pi2, NA_real_)
	expect_equal(pwSurvivalTime13$median1, NA_real_)
	expect_equal(pwSurvivalTime13$median2, NA_real_)
	expect_equal(pwSurvivalTime13$eventTime, NA_real_)
	expect_equal(pwSurvivalTime13$kappa, 1)
	expect_equal(pwSurvivalTime13$piecewiseSurvivalEnabled, TRUE)
	expect_equal(pwSurvivalTime13$delayedResponseAllowed, TRUE)
	expect_equal(pwSurvivalTime13$delayedResponseEnabled, TRUE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime13), NA)))
	    expect_output(print(pwSurvivalTime13)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime13), NA)))
	    expect_output(summary(pwSurvivalTime13)$show())
	    pwSurvivalTime13CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime13, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime13CodeBased$piecewiseSurvivalTime, pwSurvivalTime13$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$lambda1, pwSurvivalTime13$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$lambda2, pwSurvivalTime13$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$hazardRatio, pwSurvivalTime13$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$pi1, pwSurvivalTime13$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$pi2, pwSurvivalTime13$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$median1, pwSurvivalTime13$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$median2, pwSurvivalTime13$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$eventTime, pwSurvivalTime13$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$kappa, pwSurvivalTime13$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime13$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$delayedResponseAllowed, pwSurvivalTime13$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime13CodeBased$delayedResponseEnabled, pwSurvivalTime13$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime13), "character")
	    df <- as.data.frame(pwSurvivalTime13)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime13)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# case 2.2: error expected
	expect_error(getPiecewiseSurvivalTime(
	    piecewiseSurvivalTime = c(0, 6, 9),
	    lambda2 = c(0.025, 0.04, 0.015),
	    lambda1 = c(0.03, 0.04, 0.025)
	),
	paste0(
	    "Illegal argument: 'hazardRatio' can only be calculated if ",
	    "'unique(lambda1 / lambda2)' result in a single value; ",
	    "current result = c(1.2, 1, 1.667) (e.g., delayed response is not allowed)"
	),
	fixed = TRUE
	)

	# case 3
	expect_false(getPiecewiseSurvivalTime(delayedResponseAllowed = TRUE)$isPiecewiseSurvivalEnabled())
	expect_false(getPiecewiseSurvivalTime(
	    piecewiseSurvivalTime = NA,
	    delayedResponseAllowed = TRUE
	)$isPiecewiseSurvivalEnabled())

	# case 3.1
	pwSurvivalTimeSim1 <- getPiecewiseSurvivalTime(
	    piecewiseSurvivalTime = c(0, 6, 9),
	    lambda2 = c(0.025, 0.04, 0.015), hazardRatio = 0.8,
	    delayedResponseAllowed = TRUE
	)
	expect_equal(pwSurvivalTimeSim1$hazardRatio, 0.8)
	expect_equal(pwSurvivalTimeSim1$lambda1, c(0.025, 0.04, 0.015) * 0.8)
	expect_false(pwSurvivalTimeSim1$isDelayedResponseEnabled())

	# case 3.2
	pwSurvivalTimeSim2 <- getPiecewiseSurvivalTime(
	    piecewiseSurvivalTime = c(0, 6, 9),
	    lambda2 = c(0.025, 0.04, 0.015),
	    lambda1 = c(0.03, 0.04, 0.025), delayedResponseAllowed = TRUE
	)
	expect_true(pwSurvivalTimeSim2$isPiecewiseSurvivalEnabled())
	expect_true(pwSurvivalTimeSim2$isDelayedResponseEnabled())
	expect_equal(pwSurvivalTimeSim2$hazardRatio, c(1.2, 1, 5 / 3))

	pwsTime1 <- getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), lambda2 = 0.4)
	expect_equal(pwsTime1$.isLambdaBased(minNumberOfLambdas = 1), TRUE)

})

test_that("Testing 'getPiecewiseSurvivalTime': check error and warnings", {

	# @refFS[Tab.]{fs:tab:output:getPiecewiseSurvivalTime}
	expect_error(getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), lambda2 = 0.4, pi2 = 0.4),
	    "Conflicting arguments: it is not allowed to specify 'pi2' (0.4) and 'lambda2' (0.4) concurrently",
	    fixed = TRUE
	)

	expect_error(getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), lambda2 = 0.4, pi2 = 0.4, pi1 = 0.3),
	    "Conflicting arguments: it is not allowed to specify 'pi1' (0.3) and 'lambda2' (0.4) concurrently",
	    fixed = TRUE
	)

	expect_error(getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), lambda2 = 0.4, pi2 = 0.4, pi1 = 0.3),
	    "Conflicting arguments: it is not allowed to specify 'pi1' (0.3) and 'lambda2' (0.4) concurrently",
	    fixed = TRUE
	)

	expect_error(getPiecewiseSurvivalTime(lambda2 = 0.4, lambda1 = 0.3, pi2 = 0.4, pi1 = 0.3),
	    "Conflicting arguments: it is not allowed to specify 'pi1' (0.3) and 'lambda1' (0.3) concurrently",
	    fixed = TRUE
	)

	expect_error(getPiecewiseSurvivalTime(lambda2 = 0.4, lambda1 = 0.3, pi2 = 0.4, pi1 = 0.3),
	    "Conflicting arguments: it is not allowed to specify 'pi1' (0.3) and 'lambda1' (0.3) concurrently",
	    fixed = TRUE
	)

	expect_equal(getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), pi2 = 0.4)$.isPiBased(), TRUE)

	expect_warning(getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), pi2 = 0.4, pi1 = 0.3),
	    "'hazardRatio' (0.6, 0.8) will be ignored because it will be calculated",
	    fixed = TRUE
	)

	expect_warning(getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), pi1 = 0.3),
	    "'hazardRatio' (0.6, 0.8) will be ignored because it will be calculated",
	    fixed = TRUE
	)

	expect_error(getPiecewiseSurvivalTime(piecewiseSurvivalTime = c(0, 6), lambda2 = 0.025, hazardRatio = 0.8, delayedResponseAllowed = TRUE),
	    "Illegal argument: length of 'piecewiseSurvivalTime' (2) and length of 'lambda2' (1) must be equal",
	    fixed = TRUE
	)

	expect_error(getPiecewiseSurvivalTime(piecewiseSurvivalTime = c(0, 6, 12), lambda2 = 0.025, hazardRatio = 0.8, delayedResponseAllowed = TRUE),
	    "Illegal argument: length of 'piecewiseSurvivalTime' (3) and length of 'lambda2' (1) must be equal",
	    fixed = TRUE
	)

	expect_error(getPiecewiseSurvivalTime(piecewiseSurvivalTime = c(0, 6), lambda2 = 0.025, hazardRatio = 0.8),
	    "Illegal argument: length of 'piecewiseSurvivalTime' (2) and length of 'lambda2' (1) must be equal",
	    fixed = TRUE
	)

})

test_that("Testing 'getPiecewiseSurvivalTime': list-wise definition", {

	# @refFS[Tab.]{fs:tab:output:getPiecewiseSurvivalTime}
	pwSurvivalTime8 <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = list(
	    "<6"       = 0.025,
	    "6 - <9"   = 0.04,
	    "9 - <15"  = 0.015,
	    "15 - <21" = 0.01,
	    ">=21"     = 0.007
	), hazardRatio = 0.6)
	expect_true(pwSurvivalTime8$isPiecewiseSurvivalEnabled())
	expect_false(pwSurvivalTime8$isDelayedResponseEnabled())
	expect_equal(pwSurvivalTime8$hazardRatio, 0.6)
	expect_equal(pwSurvivalTime8$piecewiseSurvivalTime, c(0, 6, 9, 15, 21))
	expect_equal(pwSurvivalTime8$lambda2, c(0.025, 0.040, 0.015, 0.010, 0.007))
	expect_equal(pwSurvivalTime8$lambda1, c(0.0150, 0.0240, 0.0090, 0.0060, 0.0042))

	.skipTestIfDisabled()

	result1 <- getPiecewiseSurvivalTime(list(
	    "<5" = 0.1,
	    "5 - <10" = 0.2,
	    ">=10" = 0.8
	), hazardRatio = 0.8)
	expect_equal(result1$piecewiseSurvivalTime, c(0, 5, 10))
	expect_equal(result1$lambda2, c(0.1, 0.2, 0.8))

	result2 <- getPiecewiseSurvivalTime(list(
	    "0 - <5" = 0.1,
	    "5 - <10" = 0.2,
	    "10 - Inf" = 0.8
	), hazardRatio = 0.8)
	expect_equal(result2$piecewiseSurvivalTime, c(0, 5, 10))
	expect_equal(result2$lambda2, c(0.1, 0.2, 0.8))

	pwSurvivalTime2 <- getPiecewiseSurvivalTime(
	    piecewiseSurvivalTime = c(0, 5, 10),
	    lambda2 = c(0.1, 0.2, 0.8), hazardRatio = 0.8
	)
	expect_equal(pwSurvivalTime2$piecewiseSurvivalTime, c(0, 5, 10))
	expect_equal(pwSurvivalTime2$lambda2, c(0.1, 0.2, 0.8))

	pwSurvivalTime3 <- getPiecewiseSurvivalTime(c(0, 6), lambda2 = c(0.01, 0.03), hazardRatio = 0.8)
	expect_equal(pwSurvivalTime3$piecewiseSurvivalTime, c(0, 6))
	expect_equal(pwSurvivalTime3$lambda2, c(0.01, 0.03))

	pwSurvivalTime4 <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = list("0 - ?" = 0.025), 
	    hazardRatio = 0.8, delayedResponseAllowed = TRUE)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime4' with expected results
	expect_equal(pwSurvivalTime4$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime4$lambda1, 0.02, tolerance = 1e-07)
	expect_equal(pwSurvivalTime4$lambda2, 0.025, tolerance = 1e-07)
	expect_equal(pwSurvivalTime4$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime4$pi1, NA_real_)
	expect_equal(pwSurvivalTime4$pi2, NA_real_)
	expect_equal(pwSurvivalTime4$median1, 34.657359, tolerance = 1e-07)
	expect_equal(pwSurvivalTime4$median2, 27.725887, tolerance = 1e-07)
	expect_equal(pwSurvivalTime4$eventTime, NA_real_)
	expect_equal(pwSurvivalTime4$kappa, 1)
	expect_equal(pwSurvivalTime4$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime4$delayedResponseAllowed, TRUE)
	expect_equal(pwSurvivalTime4$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime4), NA)))
	    expect_output(print(pwSurvivalTime4)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime4), NA)))
	    expect_output(summary(pwSurvivalTime4)$show())
	    pwSurvivalTime4CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime4, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime4CodeBased$piecewiseSurvivalTime, pwSurvivalTime4$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime4CodeBased$lambda1, pwSurvivalTime4$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime4CodeBased$lambda2, pwSurvivalTime4$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime4CodeBased$hazardRatio, pwSurvivalTime4$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime4CodeBased$pi1, pwSurvivalTime4$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime4CodeBased$pi2, pwSurvivalTime4$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime4CodeBased$median1, pwSurvivalTime4$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime4CodeBased$median2, pwSurvivalTime4$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime4CodeBased$eventTime, pwSurvivalTime4$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime4CodeBased$kappa, pwSurvivalTime4$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime4CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime4$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime4CodeBased$delayedResponseAllowed, pwSurvivalTime4$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime4CodeBased$delayedResponseEnabled, pwSurvivalTime4$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime4), "character")
	    df <- as.data.frame(pwSurvivalTime4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime5 <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = list("x" = 0.025), 
	    hazardRatio = 0.8, delayedResponseAllowed = TRUE)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime5' with expected results
	expect_equal(pwSurvivalTime5$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime5$lambda1, 0.02, tolerance = 1e-07)
	expect_equal(pwSurvivalTime5$lambda2, 0.025, tolerance = 1e-07)
	expect_equal(pwSurvivalTime5$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime5$pi1, NA_real_)
	expect_equal(pwSurvivalTime5$pi2, NA_real_)
	expect_equal(pwSurvivalTime5$median1, 34.657359, tolerance = 1e-07)
	expect_equal(pwSurvivalTime5$median2, 27.725887, tolerance = 1e-07)
	expect_equal(pwSurvivalTime5$eventTime, NA_real_)
	expect_equal(pwSurvivalTime5$kappa, 1)
	expect_equal(pwSurvivalTime5$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime5$delayedResponseAllowed, TRUE)
	expect_equal(pwSurvivalTime5$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime5), NA)))
	    expect_output(print(pwSurvivalTime5)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime5), NA)))
	    expect_output(summary(pwSurvivalTime5)$show())
	    pwSurvivalTime5CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime5, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime5CodeBased$piecewiseSurvivalTime, pwSurvivalTime5$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime5CodeBased$lambda1, pwSurvivalTime5$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime5CodeBased$lambda2, pwSurvivalTime5$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime5CodeBased$hazardRatio, pwSurvivalTime5$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime5CodeBased$pi1, pwSurvivalTime5$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime5CodeBased$pi2, pwSurvivalTime5$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime5CodeBased$median1, pwSurvivalTime5$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime5CodeBased$median2, pwSurvivalTime5$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime5CodeBased$eventTime, pwSurvivalTime5$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime5CodeBased$kappa, pwSurvivalTime5$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime5CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime5$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime5CodeBased$delayedResponseAllowed, pwSurvivalTime5$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime5CodeBased$delayedResponseEnabled, pwSurvivalTime5$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime5), "character")
	    df <- as.data.frame(pwSurvivalTime5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime5)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime6 <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = list("0 - <x" = 0.025), 
	    hazardRatio = 0.8, delayedResponseAllowed = TRUE)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime6' with expected results
	expect_equal(pwSurvivalTime6$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime6$lambda1, 0.02, tolerance = 1e-07)
	expect_equal(pwSurvivalTime6$lambda2, 0.025, tolerance = 1e-07)
	expect_equal(pwSurvivalTime6$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime6$pi1, NA_real_)
	expect_equal(pwSurvivalTime6$pi2, NA_real_)
	expect_equal(pwSurvivalTime6$median1, 34.657359, tolerance = 1e-07)
	expect_equal(pwSurvivalTime6$median2, 27.725887, tolerance = 1e-07)
	expect_equal(pwSurvivalTime6$eventTime, NA_real_)
	expect_equal(pwSurvivalTime6$kappa, 1)
	expect_equal(pwSurvivalTime6$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime6$delayedResponseAllowed, TRUE)
	expect_equal(pwSurvivalTime6$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime6), NA)))
	    expect_output(print(pwSurvivalTime6)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime6), NA)))
	    expect_output(summary(pwSurvivalTime6)$show())
	    pwSurvivalTime6CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime6, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime6CodeBased$piecewiseSurvivalTime, pwSurvivalTime6$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime6CodeBased$lambda1, pwSurvivalTime6$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime6CodeBased$lambda2, pwSurvivalTime6$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime6CodeBased$hazardRatio, pwSurvivalTime6$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime6CodeBased$pi1, pwSurvivalTime6$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime6CodeBased$pi2, pwSurvivalTime6$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime6CodeBased$median1, pwSurvivalTime6$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime6CodeBased$median2, pwSurvivalTime6$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime6CodeBased$eventTime, pwSurvivalTime6$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime6CodeBased$kappa, pwSurvivalTime6$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime6CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime6$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime6CodeBased$delayedResponseAllowed, pwSurvivalTime6$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime6CodeBased$delayedResponseEnabled, pwSurvivalTime6$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime6), "character")
	    df <- as.data.frame(pwSurvivalTime6)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime6)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime7 <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = list("x" = 0.025), 
	    hazardRatio = 0.8, delayedResponseAllowed = FALSE)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime7' with expected results
	expect_equal(pwSurvivalTime7$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime7$lambda1, 0.02, tolerance = 1e-07)
	expect_equal(pwSurvivalTime7$lambda2, 0.025, tolerance = 1e-07)
	expect_equal(pwSurvivalTime7$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime7$pi1, NA_real_)
	expect_equal(pwSurvivalTime7$pi2, NA_real_)
	expect_equal(pwSurvivalTime7$median1, 34.657359, tolerance = 1e-07)
	expect_equal(pwSurvivalTime7$median2, 27.725887, tolerance = 1e-07)
	expect_equal(pwSurvivalTime7$eventTime, NA_real_)
	expect_equal(pwSurvivalTime7$kappa, 1)
	expect_equal(pwSurvivalTime7$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime7$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime7$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime7), NA)))
	    expect_output(print(pwSurvivalTime7)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime7), NA)))
	    expect_output(summary(pwSurvivalTime7)$show())
	    pwSurvivalTime7CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime7, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime7CodeBased$piecewiseSurvivalTime, pwSurvivalTime7$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime7CodeBased$lambda1, pwSurvivalTime7$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime7CodeBased$lambda2, pwSurvivalTime7$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime7CodeBased$hazardRatio, pwSurvivalTime7$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime7CodeBased$pi1, pwSurvivalTime7$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime7CodeBased$pi2, pwSurvivalTime7$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime7CodeBased$median1, pwSurvivalTime7$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime7CodeBased$median2, pwSurvivalTime7$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime7CodeBased$eventTime, pwSurvivalTime7$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime7CodeBased$kappa, pwSurvivalTime7$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime7CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime7$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime7CodeBased$delayedResponseAllowed, pwSurvivalTime7$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime7CodeBased$delayedResponseEnabled, pwSurvivalTime7$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime7), "character")
	    df <- as.data.frame(pwSurvivalTime7)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime7)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime8 <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = list("0 - <x" = 0.025), 
	    hazardRatio = 0.8, delayedResponseAllowed = FALSE)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime8' with expected results
	expect_equal(pwSurvivalTime8$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime8$lambda1, 0.02, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$lambda2, 0.025, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$pi1, NA_real_)
	expect_equal(pwSurvivalTime8$pi2, NA_real_)
	expect_equal(pwSurvivalTime8$median1, 34.657359, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$median2, 27.725887, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$eventTime, NA_real_)
	expect_equal(pwSurvivalTime8$kappa, 1)
	expect_equal(pwSurvivalTime8$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime8$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime8$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime8), NA)))
	    expect_output(print(pwSurvivalTime8)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime8), NA)))
	    expect_output(summary(pwSurvivalTime8)$show())
	    pwSurvivalTime8CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime8, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime8CodeBased$piecewiseSurvivalTime, pwSurvivalTime8$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$lambda1, pwSurvivalTime8$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$lambda2, pwSurvivalTime8$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$hazardRatio, pwSurvivalTime8$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$pi1, pwSurvivalTime8$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$pi2, pwSurvivalTime8$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$median1, pwSurvivalTime8$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$median2, pwSurvivalTime8$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$eventTime, pwSurvivalTime8$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$kappa, pwSurvivalTime8$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime8$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$delayedResponseAllowed, pwSurvivalTime8$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime8CodeBased$delayedResponseEnabled, pwSurvivalTime8$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime8), "character")
	    df <- as.data.frame(pwSurvivalTime8)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime8)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	expect_warning(getPiecewiseSurvivalTime(piecewiseSurvivalTime = list("<6" = 0.025), hazardRatio = 0.8),
	    "Defined time period \"0 - <6\" will be ignored because 'piecewiseSurvivalTime' list has only 1 entry",
	    fixed = TRUE
	)

})

test_plan_section("Testing Class 'AccrualTime'")


test_that("Testing 'getAccrualTime': isAccrualTimeEnabled()", {
	expect_true(getAccrualTime()$isAccrualTimeEnabled())
	expect_true(getAccrualTime(maxNumberOfSubjects = 100)$isAccrualTimeEnabled())

})

test_that("Testing 'getAccrualTime': vector based definition", {

	accrualTime1 <- getAccrualTime(
	    accrualTime = c(0, 6, 9, 15),
	    accrualIntensity = c(15, 21, 27), maxNumberOfSubjects = 315
	)
	expect_equal(accrualTime1$accrualTime, c(0, 6, 9, 15))
	expect_equal(accrualTime1$accrualIntensity, c(15, 21, 27))
	expect_equal(accrualTime1$remainingTime, NA_real_)

	accrualTime2 <- getAccrualTime(
	    accrualTime = c(0, 6, 9),
	    accrualIntensity = c(15, 21, 27), maxNumberOfSubjects = 1000
	)
	expect_equal(accrualTime2$accrualTime, c(0, 6, 9, 40.37037))
	expect_equal(accrualTime2$accrualIntensity, c(15, 21, 27))
	expect_equal(accrualTime2$remainingTime, 31.37037)

	.skipTestIfDisabled()

	accrualTime3 <- getAccrualTime(
	    accrualTime = c(0, 12, 13, 14, 15, 16),
	    accrualIntensity = c(15, 21, 27, 33, 39, 45), maxNumberOfSubjects = 1405
	)
	expect_equal(accrualTime3$accrualTime, c(0, 12, 13, 14, 15, 16, 40.55555556))
	expect_equal(accrualTime3$accrualIntensity, c(15, 21, 27, 33, 39, 45))
	expect_equal(accrualTime3$remainingTime, 24.55555556)

	accrualTime4 <- getAccrualTime(
	    accrualTime = c(0, 24),
	    accrualIntensity = c(30), maxNumberOfSubjects = 720
	)

	## Comparison of the results of AccrualTime object 'accrualTime4' with expected results
	expect_equal(accrualTime4$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime4$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime4$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime4$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime4$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime4$accrualTime, c(0, 24))
	expect_equal(accrualTime4$accrualIntensity, 30)
	expect_equal(accrualTime4$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime4$maxNumberOfSubjects, 720)
	expect_equal(accrualTime4$remainingTime, NA_real_)
	expect_equal(accrualTime4$piecewiseAccrualEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime4), NA)))
	    expect_output(print(accrualTime4)$show())
	    invisible(capture.output(expect_error(summary(accrualTime4), NA)))
	    expect_output(summary(accrualTime4)$show())
	    accrualTime4CodeBased <- eval(parse(text = getObjectRCode(accrualTime4, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime4CodeBased$endOfAccrualIsUserDefined, accrualTime4$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$followUpTimeMustBeUserDefined, accrualTime4$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime4$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime4$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$absoluteAccrualIntensityEnabled, accrualTime4$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$accrualTime, accrualTime4$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$accrualIntensity, accrualTime4$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$accrualIntensityRelative, accrualTime4$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$maxNumberOfSubjects, accrualTime4$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$remainingTime, accrualTime4$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$piecewiseAccrualEnabled, accrualTime4$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime4), "character")
	    df <- as.data.frame(accrualTime4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime5 <- getAccrualTime(
	    accrualTime = c(0, 24, 30),
	    accrualIntensity = c(30, 45)
	)

	## Comparison of the results of AccrualTime object 'accrualTime5' with expected results
	expect_equal(accrualTime5$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime5$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime5$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime5$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime5$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime5$accrualTime, c(0, 24, 30))
	expect_equal(accrualTime5$accrualIntensity, c(30, 45))
	expect_equal(accrualTime5$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime5$maxNumberOfSubjects, 990)
	expect_equal(accrualTime5$remainingTime, 6)
	expect_equal(accrualTime5$piecewiseAccrualEnabled, TRUE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime5), NA)))
	    expect_output(print(accrualTime5)$show())
	    invisible(capture.output(expect_error(summary(accrualTime5), NA)))
	    expect_output(summary(accrualTime5)$show())
	    accrualTime5CodeBased <- eval(parse(text = getObjectRCode(accrualTime5, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime5CodeBased$endOfAccrualIsUserDefined, accrualTime5$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$followUpTimeMustBeUserDefined, accrualTime5$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime5$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime5$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$absoluteAccrualIntensityEnabled, accrualTime5$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$accrualTime, accrualTime5$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$accrualIntensity, accrualTime5$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$accrualIntensityRelative, accrualTime5$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$maxNumberOfSubjects, accrualTime5$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$remainingTime, accrualTime5$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$piecewiseAccrualEnabled, accrualTime5$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime5), "character")
	    df <- as.data.frame(accrualTime5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime5)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime6 <- getAccrualTime(
	    accrualTime = c(0, 24, 30),
	    accrualIntensity = c(20, 25, 45), maxNumberOfSubjects = 720
	)

	## Comparison of the results of AccrualTime object 'accrualTime6' with expected results
	expect_equal(accrualTime6$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime6$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime6$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime6$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime6$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime6$accrualTime, c(0, 24, 30, 32))
	expect_equal(accrualTime6$accrualIntensity, c(20, 25, 45))
	expect_equal(accrualTime6$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime6$maxNumberOfSubjects, 720)
	expect_equal(accrualTime6$remainingTime, 2)
	expect_equal(accrualTime6$piecewiseAccrualEnabled, TRUE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime6), NA)))
	    expect_output(print(accrualTime6)$show())
	    invisible(capture.output(expect_error(summary(accrualTime6), NA)))
	    expect_output(summary(accrualTime6)$show())
	    accrualTime6CodeBased <- eval(parse(text = getObjectRCode(accrualTime6, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime6CodeBased$endOfAccrualIsUserDefined, accrualTime6$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$followUpTimeMustBeUserDefined, accrualTime6$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime6$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime6$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$absoluteAccrualIntensityEnabled, accrualTime6$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$accrualTime, accrualTime6$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$accrualIntensity, accrualTime6$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$accrualIntensityRelative, accrualTime6$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$maxNumberOfSubjects, accrualTime6$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$remainingTime, accrualTime6$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$piecewiseAccrualEnabled, accrualTime6$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime6), "character")
	    df <- as.data.frame(accrualTime6)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime6)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime8 <- getAccrualTime(accrualTime = 0, accrualIntensity = 15, maxNumberOfSubjects = 1000)

	## Comparison of the results of AccrualTime object 'accrualTime8' with expected results
	expect_equal(accrualTime8$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime8$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime8$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime8$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime8$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime8$accrualTime, c(0, 66.666667), tolerance = 1e-07)
	expect_equal(accrualTime8$accrualIntensity, 15)
	expect_equal(accrualTime8$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime8$maxNumberOfSubjects, 1000)
	expect_equal(accrualTime8$remainingTime, 66.666667, tolerance = 1e-07)
	expect_equal(accrualTime8$piecewiseAccrualEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime8), NA)))
	    expect_output(print(accrualTime8)$show())
	    invisible(capture.output(expect_error(summary(accrualTime8), NA)))
	    expect_output(summary(accrualTime8)$show())
	    accrualTime8CodeBased <- eval(parse(text = getObjectRCode(accrualTime8, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime8CodeBased$endOfAccrualIsUserDefined, accrualTime8$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$followUpTimeMustBeUserDefined, accrualTime8$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime8$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime8$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$absoluteAccrualIntensityEnabled, accrualTime8$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$accrualTime, accrualTime8$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$accrualIntensity, accrualTime8$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$accrualIntensityRelative, accrualTime8$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$maxNumberOfSubjects, accrualTime8$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$remainingTime, accrualTime8$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$piecewiseAccrualEnabled, accrualTime8$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime8), "character")
	    df <- as.data.frame(accrualTime8)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime8)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime9 <- getAccrualTime(accrualTime = c(0, 5), accrualIntensity = 15)

	## Comparison of the results of AccrualTime object 'accrualTime9' with expected results
	expect_equal(accrualTime9$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime9$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime9$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime9$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime9$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime9$accrualTime, c(0, 5))
	expect_equal(accrualTime9$accrualIntensity, 15)
	expect_equal(accrualTime9$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime9$maxNumberOfSubjects, 75)
	expect_equal(accrualTime9$remainingTime, 5)
	expect_equal(accrualTime9$piecewiseAccrualEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime9), NA)))
	    expect_output(print(accrualTime9)$show())
	    invisible(capture.output(expect_error(summary(accrualTime9), NA)))
	    expect_output(summary(accrualTime9)$show())
	    accrualTime9CodeBased <- eval(parse(text = getObjectRCode(accrualTime9, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime9CodeBased$endOfAccrualIsUserDefined, accrualTime9$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$followUpTimeMustBeUserDefined, accrualTime9$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime9$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime9$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$absoluteAccrualIntensityEnabled, accrualTime9$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$accrualTime, accrualTime9$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$accrualIntensity, accrualTime9$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$accrualIntensityRelative, accrualTime9$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$maxNumberOfSubjects, accrualTime9$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$remainingTime, accrualTime9$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$piecewiseAccrualEnabled, accrualTime9$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime9), "character")
	    df <- as.data.frame(accrualTime9)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime9)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime10 <- getAccrualTime(accrualTime = 0, accrualIntensity = 15, maxNumberOfSubjects = 10)

	## Comparison of the results of AccrualTime object 'accrualTime10' with expected results
	expect_equal(accrualTime10$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime10$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime10$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime10$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime10$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime10$accrualTime, c(0, 0.66666667), tolerance = 1e-07)
	expect_equal(accrualTime10$accrualIntensity, 15)
	expect_equal(accrualTime10$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime10$maxNumberOfSubjects, 10)
	expect_equal(accrualTime10$remainingTime, 0.66666667, tolerance = 1e-07)
	expect_equal(accrualTime10$piecewiseAccrualEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime10), NA)))
	    expect_output(print(accrualTime10)$show())
	    invisible(capture.output(expect_error(summary(accrualTime10), NA)))
	    expect_output(summary(accrualTime10)$show())
	    accrualTime10CodeBased <- eval(parse(text = getObjectRCode(accrualTime10, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime10CodeBased$endOfAccrualIsUserDefined, accrualTime10$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$followUpTimeMustBeUserDefined, accrualTime10$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime10$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime10$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$absoluteAccrualIntensityEnabled, accrualTime10$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$accrualTime, accrualTime10$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$accrualIntensity, accrualTime10$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$accrualIntensityRelative, accrualTime10$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$maxNumberOfSubjects, accrualTime10$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$remainingTime, accrualTime10$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$piecewiseAccrualEnabled, accrualTime10$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime10), "character")
	    df <- as.data.frame(accrualTime10)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime10)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime11 <- getAccrualTime(accrualTime = c(0, 5), accrualIntensity = 15, maxNumberOfSubjects = 75)

	## Comparison of the results of AccrualTime object 'accrualTime11' with expected results
	expect_equal(accrualTime11$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime11$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime11$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime11$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime11$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime11$accrualTime, c(0, 5))
	expect_equal(accrualTime11$accrualIntensity, 15)
	expect_equal(accrualTime11$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime11$maxNumberOfSubjects, 75)
	expect_equal(accrualTime11$remainingTime, NA_real_)
	expect_equal(accrualTime11$piecewiseAccrualEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime11), NA)))
	    expect_output(print(accrualTime11)$show())
	    invisible(capture.output(expect_error(summary(accrualTime11), NA)))
	    expect_output(summary(accrualTime11)$show())
	    accrualTime11CodeBased <- eval(parse(text = getObjectRCode(accrualTime11, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime11CodeBased$endOfAccrualIsUserDefined, accrualTime11$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime11CodeBased$followUpTimeMustBeUserDefined, accrualTime11$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime11CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime11$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime11CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime11$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime11CodeBased$absoluteAccrualIntensityEnabled, accrualTime11$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime11CodeBased$accrualTime, accrualTime11$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime11CodeBased$accrualIntensity, accrualTime11$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime11CodeBased$accrualIntensityRelative, accrualTime11$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime11CodeBased$maxNumberOfSubjects, accrualTime11$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime11CodeBased$remainingTime, accrualTime11$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime11CodeBased$piecewiseAccrualEnabled, accrualTime11$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime11), "character")
	    df <- as.data.frame(accrualTime11)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime11)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime12 <- getAccrualTime(accrualTime = c(0, 6, 15, 25), accrualIntensity = c(22, 0, 33))

	## Comparison of the results of AccrualTime object 'accrualTime12' with expected results
	expect_equal(accrualTime12$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime12$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime12$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime12$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime12$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime12$accrualTime, c(0, 6, 15, 25))
	expect_equal(accrualTime12$accrualIntensity, c(22, 0, 33))
	expect_equal(accrualTime12$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime12$maxNumberOfSubjects, 462)
	expect_equal(accrualTime12$remainingTime, 10)
	expect_equal(accrualTime12$piecewiseAccrualEnabled, TRUE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime12), NA)))
	    expect_output(print(accrualTime12)$show())
	    invisible(capture.output(expect_error(summary(accrualTime12), NA)))
	    expect_output(summary(accrualTime12)$show())
	    accrualTime12CodeBased <- eval(parse(text = getObjectRCode(accrualTime12, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime12CodeBased$endOfAccrualIsUserDefined, accrualTime12$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$followUpTimeMustBeUserDefined, accrualTime12$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime12$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime12$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$absoluteAccrualIntensityEnabled, accrualTime12$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$accrualTime, accrualTime12$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$accrualIntensity, accrualTime12$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$accrualIntensityRelative, accrualTime12$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$maxNumberOfSubjects, accrualTime12$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$remainingTime, accrualTime12$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$piecewiseAccrualEnabled, accrualTime12$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime12), "character")
	    df <- as.data.frame(accrualTime12)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime12)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime13 <- getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(22, 33), maxNumberOfSubjects = 1000)

	## Comparison of the results of AccrualTime object 'accrualTime13' with expected results
	expect_equal(accrualTime13$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime13$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime13$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime13$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime13$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime13$accrualTime, c(0, 6, 32.30303), tolerance = 1e-07)
	expect_equal(accrualTime13$accrualIntensity, c(22, 33))
	expect_equal(accrualTime13$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime13$maxNumberOfSubjects, 1000)
	expect_equal(accrualTime13$remainingTime, 26.30303, tolerance = 1e-07)
	expect_equal(accrualTime13$piecewiseAccrualEnabled, TRUE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime13), NA)))
	    expect_output(print(accrualTime13)$show())
	    invisible(capture.output(expect_error(summary(accrualTime13), NA)))
	    expect_output(summary(accrualTime13)$show())
	    accrualTime13CodeBased <- eval(parse(text = getObjectRCode(accrualTime13, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime13CodeBased$endOfAccrualIsUserDefined, accrualTime13$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$followUpTimeMustBeUserDefined, accrualTime13$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime13$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime13$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$absoluteAccrualIntensityEnabled, accrualTime13$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$accrualTime, accrualTime13$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$accrualIntensity, accrualTime13$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$accrualIntensityRelative, accrualTime13$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$maxNumberOfSubjects, accrualTime13$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$remainingTime, accrualTime13$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$piecewiseAccrualEnabled, accrualTime13$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime13), "character")
	    df <- as.data.frame(accrualTime13)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime13)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("Testing 'getAccrualTime': test absolute and relative definition", {

	# @refFS[Tab.]{fs:tab:output:getAccrualTime}
	accrualTime1 <- getAccrualTime(
	    accrualTime = c(0, 6, 30),
	    accrualIntensity = c(22, 33), maxNumberOfSubjects = 924
	)

	## Comparison of the results of AccrualTime object 'accrualTime1' with expected results
	expect_equal(accrualTime1$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime1$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime1$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime1$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime1$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime1$accrualTime, c(0, 6, 30))
	expect_equal(accrualTime1$accrualIntensity, c(22, 33))
	expect_equal(accrualTime1$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime1$maxNumberOfSubjects, 924)
	expect_equal(accrualTime1$remainingTime, NA_real_)
	expect_equal(accrualTime1$piecewiseAccrualEnabled, TRUE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime1), NA)))
	    expect_output(print(accrualTime1)$show())
	    invisible(capture.output(expect_error(summary(accrualTime1), NA)))
	    expect_output(summary(accrualTime1)$show())
	    accrualTime1CodeBased <- eval(parse(text = getObjectRCode(accrualTime1, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime1CodeBased$endOfAccrualIsUserDefined, accrualTime1$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime1CodeBased$followUpTimeMustBeUserDefined, accrualTime1$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime1CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime1$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime1CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime1$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime1CodeBased$absoluteAccrualIntensityEnabled, accrualTime1$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime1CodeBased$accrualTime, accrualTime1$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime1CodeBased$accrualIntensity, accrualTime1$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime1CodeBased$accrualIntensityRelative, accrualTime1$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime1CodeBased$maxNumberOfSubjects, accrualTime1$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime1CodeBased$remainingTime, accrualTime1$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime1CodeBased$piecewiseAccrualEnabled, accrualTime1$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime1), "character")
	    df <- as.data.frame(accrualTime1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime2 <- getAccrualTime(list(
	    "0 - <6"   = 22,
	    "6 - <=30" = 33
	),
	maxNumberOfSubjects = 924
	)

	## Comparison of the results of AccrualTime object 'accrualTime2' with expected results
	expect_equal(accrualTime2$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime2$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime2$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime2$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime2$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime2$accrualTime, c(0, 6, 30))
	expect_equal(accrualTime2$accrualIntensity, c(22, 33))
	expect_equal(accrualTime2$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime2$maxNumberOfSubjects, 924)
	expect_equal(accrualTime2$remainingTime, NA_real_)
	expect_equal(accrualTime2$piecewiseAccrualEnabled, TRUE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime2), NA)))
	    expect_output(print(accrualTime2)$show())
	    invisible(capture.output(expect_error(summary(accrualTime2), NA)))
	    expect_output(summary(accrualTime2)$show())
	    accrualTime2CodeBased <- eval(parse(text = getObjectRCode(accrualTime2, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime2CodeBased$endOfAccrualIsUserDefined, accrualTime2$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime2CodeBased$followUpTimeMustBeUserDefined, accrualTime2$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime2CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime2$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime2CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime2$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime2CodeBased$absoluteAccrualIntensityEnabled, accrualTime2$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime2CodeBased$accrualTime, accrualTime2$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime2CodeBased$accrualIntensity, accrualTime2$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime2CodeBased$accrualIntensityRelative, accrualTime2$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime2CodeBased$maxNumberOfSubjects, accrualTime2$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime2CodeBased$remainingTime, accrualTime2$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime2CodeBased$piecewiseAccrualEnabled, accrualTime2$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime2), "character")
	    df <- as.data.frame(accrualTime2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

	accrualTime3 <- getAccrualTime(
	    accrualTime = c(0, 6, 30),
	    accrualIntensity = c(0.22, 0.33), maxNumberOfSubjects = 1000
	)

	## Comparison of the results of AccrualTime object 'accrualTime3' with expected results
	expect_equal(accrualTime3$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime3$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime3$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime3$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime3$absoluteAccrualIntensityEnabled, FALSE)
	expect_equal(accrualTime3$accrualTime, c(0, 6, 30))
	expect_equal(accrualTime3$accrualIntensity, c(23.809524, 35.714286), tolerance = 1e-07)
	expect_equal(accrualTime3$accrualIntensityRelative, c(0.22, 0.33), tolerance = 1e-07)
	expect_equal(accrualTime3$maxNumberOfSubjects, 1000)
	expect_equal(accrualTime3$remainingTime, 24)
	expect_equal(accrualTime3$piecewiseAccrualEnabled, TRUE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime3), NA)))
	    expect_output(print(accrualTime3)$show())
	    invisible(capture.output(expect_error(summary(accrualTime3), NA)))
	    expect_output(summary(accrualTime3)$show())
	    accrualTime3CodeBased <- eval(parse(text = getObjectRCode(accrualTime3, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime3CodeBased$endOfAccrualIsUserDefined, accrualTime3$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime3CodeBased$followUpTimeMustBeUserDefined, accrualTime3$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime3CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime3$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime3CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime3$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime3CodeBased$absoluteAccrualIntensityEnabled, accrualTime3$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime3CodeBased$accrualTime, accrualTime3$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime3CodeBased$accrualIntensity, accrualTime3$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime3CodeBased$accrualIntensityRelative, accrualTime3$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime3CodeBased$maxNumberOfSubjects, accrualTime3$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime3CodeBased$remainingTime, accrualTime3$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime3CodeBased$piecewiseAccrualEnabled, accrualTime3$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime3), "character")
	    df <- as.data.frame(accrualTime3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime4 <- getAccrualTime(list(
	    "0 - <6"   = 0.22,
	    "6 - <=30" = 0.33
	),
	maxNumberOfSubjects = 1000
	)

	## Comparison of the results of AccrualTime object 'accrualTime4' with expected results
	expect_equal(accrualTime4$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime4$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime4$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime4$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime4$absoluteAccrualIntensityEnabled, FALSE)
	expect_equal(accrualTime4$accrualTime, c(0, 6, 30))
	expect_equal(accrualTime4$accrualIntensity, c(23.809524, 35.714286), tolerance = 1e-07)
	expect_equal(accrualTime4$accrualIntensityRelative, c(0.22, 0.33), tolerance = 1e-07)
	expect_equal(accrualTime4$maxNumberOfSubjects, 1000)
	expect_equal(accrualTime4$remainingTime, 24)
	expect_equal(accrualTime4$piecewiseAccrualEnabled, TRUE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime4), NA)))
	    expect_output(print(accrualTime4)$show())
	    invisible(capture.output(expect_error(summary(accrualTime4), NA)))
	    expect_output(summary(accrualTime4)$show())
	    accrualTime4CodeBased <- eval(parse(text = getObjectRCode(accrualTime4, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime4CodeBased$endOfAccrualIsUserDefined, accrualTime4$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$followUpTimeMustBeUserDefined, accrualTime4$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime4$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime4$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$absoluteAccrualIntensityEnabled, accrualTime4$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$accrualTime, accrualTime4$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$accrualIntensity, accrualTime4$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$accrualIntensityRelative, accrualTime4$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$maxNumberOfSubjects, accrualTime4$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$remainingTime, accrualTime4$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime4CodeBased$piecewiseAccrualEnabled, accrualTime4$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime4), "character")
	    df <- as.data.frame(accrualTime4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime5 <- getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(22, 33))

	## Comparison of the results of AccrualTime object 'accrualTime5' with expected results
	expect_equal(accrualTime5$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime5$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime5$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime5$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime5$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime5$accrualTime, c(0, 6, 30))
	expect_equal(accrualTime5$accrualIntensity, c(22, 33))
	expect_equal(accrualTime5$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime5$maxNumberOfSubjects, 924)
	expect_equal(accrualTime5$remainingTime, 24)
	expect_equal(accrualTime5$piecewiseAccrualEnabled, TRUE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime5), NA)))
	    expect_output(print(accrualTime5)$show())
	    invisible(capture.output(expect_error(summary(accrualTime5), NA)))
	    expect_output(summary(accrualTime5)$show())
	    accrualTime5CodeBased <- eval(parse(text = getObjectRCode(accrualTime5, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime5CodeBased$endOfAccrualIsUserDefined, accrualTime5$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$followUpTimeMustBeUserDefined, accrualTime5$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime5$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime5$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$absoluteAccrualIntensityEnabled, accrualTime5$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$accrualTime, accrualTime5$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$accrualIntensity, accrualTime5$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$accrualIntensityRelative, accrualTime5$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$maxNumberOfSubjects, accrualTime5$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$remainingTime, accrualTime5$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime5CodeBased$piecewiseAccrualEnabled, accrualTime5$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime5), "character")
	    df <- as.data.frame(accrualTime5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime5)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime6 <- getAccrualTime(list(
	    "0 - <6"   = 22,
	    "6 - <=30" = 33
	))

	## Comparison of the results of AccrualTime object 'accrualTime6' with expected results
	expect_equal(accrualTime6$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime6$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime6$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime6$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime6$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime6$accrualTime, c(0, 6, 30))
	expect_equal(accrualTime6$accrualIntensity, c(22, 33))
	expect_equal(accrualTime6$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime6$maxNumberOfSubjects, 924)
	expect_equal(accrualTime6$remainingTime, 24)
	expect_equal(accrualTime6$piecewiseAccrualEnabled, TRUE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime6), NA)))
	    expect_output(print(accrualTime6)$show())
	    invisible(capture.output(expect_error(summary(accrualTime6), NA)))
	    expect_output(summary(accrualTime6)$show())
	    accrualTime6CodeBased <- eval(parse(text = getObjectRCode(accrualTime6, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime6CodeBased$endOfAccrualIsUserDefined, accrualTime6$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$followUpTimeMustBeUserDefined, accrualTime6$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime6$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime6$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$absoluteAccrualIntensityEnabled, accrualTime6$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$accrualTime, accrualTime6$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$accrualIntensity, accrualTime6$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$accrualIntensityRelative, accrualTime6$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$maxNumberOfSubjects, accrualTime6$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$remainingTime, accrualTime6$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime6CodeBased$piecewiseAccrualEnabled, accrualTime6$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime6), "character")
	    df <- as.data.frame(accrualTime6)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime6)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime7 <- getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(0.22, 0.33))

	## Comparison of the results of AccrualTime object 'accrualTime7' with expected results
	expect_equal(accrualTime7$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime7$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime7$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime7$maxNumberOfSubjectsCanBeCalculatedDirectly, FALSE)
	expect_equal(accrualTime7$absoluteAccrualIntensityEnabled, FALSE)
	expect_equal(accrualTime7$accrualTime, c(0, 6, 30))
	expect_equal(accrualTime7$accrualIntensity, c(0.22, 0.33), tolerance = 1e-07)
	expect_equal(accrualTime7$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime7$maxNumberOfSubjects, NA_real_)
	expect_equal(accrualTime7$remainingTime, NA_real_)
	expect_equal(accrualTime7$piecewiseAccrualEnabled, TRUE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime7), NA)))
	    expect_output(print(accrualTime7)$show())
	    invisible(capture.output(expect_error(summary(accrualTime7), NA)))
	    expect_output(summary(accrualTime7)$show())
	    accrualTime7CodeBased <- eval(parse(text = getObjectRCode(accrualTime7, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime7CodeBased$endOfAccrualIsUserDefined, accrualTime7$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime7CodeBased$followUpTimeMustBeUserDefined, accrualTime7$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime7CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime7$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime7CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime7$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime7CodeBased$absoluteAccrualIntensityEnabled, accrualTime7$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime7CodeBased$accrualTime, accrualTime7$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime7CodeBased$accrualIntensity, accrualTime7$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime7CodeBased$accrualIntensityRelative, accrualTime7$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime7CodeBased$maxNumberOfSubjects, accrualTime7$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime7CodeBased$remainingTime, accrualTime7$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime7CodeBased$piecewiseAccrualEnabled, accrualTime7$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime7), "character")
	    df <- as.data.frame(accrualTime7)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime7)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime8 <- getAccrualTime(list(
	    "0 - <6"   = 0.22,
	    "6 - <=30" = 0.33
	))

	## Comparison of the results of AccrualTime object 'accrualTime8' with expected results
	expect_equal(accrualTime8$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime8$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime8$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime8$maxNumberOfSubjectsCanBeCalculatedDirectly, FALSE)
	expect_equal(accrualTime8$absoluteAccrualIntensityEnabled, FALSE)
	expect_equal(accrualTime8$accrualTime, c(0, 6, 30))
	expect_equal(accrualTime8$accrualIntensity, c(0.22, 0.33), tolerance = 1e-07)
	expect_equal(accrualTime8$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime8$maxNumberOfSubjects, NA_real_)
	expect_equal(accrualTime8$remainingTime, NA_real_)
	expect_equal(accrualTime8$piecewiseAccrualEnabled, TRUE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime8), NA)))
	    expect_output(print(accrualTime8)$show())
	    invisible(capture.output(expect_error(summary(accrualTime8), NA)))
	    expect_output(summary(accrualTime8)$show())
	    accrualTime8CodeBased <- eval(parse(text = getObjectRCode(accrualTime8, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime8CodeBased$endOfAccrualIsUserDefined, accrualTime8$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$followUpTimeMustBeUserDefined, accrualTime8$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime8$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime8$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$absoluteAccrualIntensityEnabled, accrualTime8$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$accrualTime, accrualTime8$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$accrualIntensity, accrualTime8$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$accrualIntensityRelative, accrualTime8$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$maxNumberOfSubjects, accrualTime8$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$remainingTime, accrualTime8$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime8CodeBased$piecewiseAccrualEnabled, accrualTime8$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime8), "character")
	    df <- as.data.frame(accrualTime8)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime8)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime9 <- getAccrualTime(
	    accrualTime = c(0, 6),
	    accrualIntensity = c(22, 33), maxNumberOfSubjects = 1000
	)

	## Comparison of the results of AccrualTime object 'accrualTime9' with expected results
	expect_equal(accrualTime9$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime9$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime9$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime9$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime9$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime9$accrualTime, c(0, 6, 32.30303), tolerance = 1e-07)
	expect_equal(accrualTime9$accrualIntensity, c(22, 33))
	expect_equal(accrualTime9$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime9$maxNumberOfSubjects, 1000)
	expect_equal(accrualTime9$remainingTime, 26.30303, tolerance = 1e-07)
	expect_equal(accrualTime9$piecewiseAccrualEnabled, TRUE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime9), NA)))
	    expect_output(print(accrualTime9)$show())
	    invisible(capture.output(expect_error(summary(accrualTime9), NA)))
	    expect_output(summary(accrualTime9)$show())
	    accrualTime9CodeBased <- eval(parse(text = getObjectRCode(accrualTime9, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime9CodeBased$endOfAccrualIsUserDefined, accrualTime9$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$followUpTimeMustBeUserDefined, accrualTime9$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime9$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime9$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$absoluteAccrualIntensityEnabled, accrualTime9$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$accrualTime, accrualTime9$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$accrualIntensity, accrualTime9$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$accrualIntensityRelative, accrualTime9$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$maxNumberOfSubjects, accrualTime9$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$remainingTime, accrualTime9$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime9CodeBased$piecewiseAccrualEnabled, accrualTime9$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime9), "character")
	    df <- as.data.frame(accrualTime9)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime9)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime10 <- getAccrualTime(list(
	    "0 - <6" = 22,
	    "6"      = 33
	),
	maxNumberOfSubjects = 1000
	)

	## Comparison of the results of AccrualTime object 'accrualTime10' with expected results
	expect_equal(accrualTime10$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime10$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime10$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime10$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime10$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime10$accrualTime, c(0, 6, 32.30303), tolerance = 1e-07)
	expect_equal(accrualTime10$accrualIntensity, c(22, 33))
	expect_equal(accrualTime10$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime10$maxNumberOfSubjects, 1000)
	expect_equal(accrualTime10$remainingTime, 26.30303, tolerance = 1e-07)
	expect_equal(accrualTime10$piecewiseAccrualEnabled, TRUE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime10), NA)))
	    expect_output(print(accrualTime10)$show())
	    invisible(capture.output(expect_error(summary(accrualTime10), NA)))
	    expect_output(summary(accrualTime10)$show())
	    accrualTime10CodeBased <- eval(parse(text = getObjectRCode(accrualTime10, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime10CodeBased$endOfAccrualIsUserDefined, accrualTime10$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$followUpTimeMustBeUserDefined, accrualTime10$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime10$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime10$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$absoluteAccrualIntensityEnabled, accrualTime10$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$accrualTime, accrualTime10$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$accrualIntensity, accrualTime10$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$accrualIntensityRelative, accrualTime10$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$maxNumberOfSubjects, accrualTime10$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$remainingTime, accrualTime10$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime10CodeBased$piecewiseAccrualEnabled, accrualTime10$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime10), "character")
	    df <- as.data.frame(accrualTime10)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime10)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime12 <- getAccrualTime(list(
	    "0 - <6" = 0.22,
	    "6 - <=30"      = 0.33
	),
	maxNumberOfSubjects = 1000
	)

	## Comparison of the results of AccrualTime object 'accrualTime12' with expected results
	expect_equal(accrualTime12$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime12$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime12$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime12$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime12$absoluteAccrualIntensityEnabled, FALSE)
	expect_equal(accrualTime12$accrualTime, c(0, 6, 30))
	expect_equal(accrualTime12$accrualIntensity, c(23.809524, 35.714286), tolerance = 1e-07)
	expect_equal(accrualTime12$accrualIntensityRelative, c(0.22, 0.33), tolerance = 1e-07)
	expect_equal(accrualTime12$maxNumberOfSubjects, 1000)
	expect_equal(accrualTime12$remainingTime, 24)
	expect_equal(accrualTime12$piecewiseAccrualEnabled, TRUE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime12), NA)))
	    expect_output(print(accrualTime12)$show())
	    invisible(capture.output(expect_error(summary(accrualTime12), NA)))
	    expect_output(summary(accrualTime12)$show())
	    accrualTime12CodeBased <- eval(parse(text = getObjectRCode(accrualTime12, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime12CodeBased$endOfAccrualIsUserDefined, accrualTime12$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$followUpTimeMustBeUserDefined, accrualTime12$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime12$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime12$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$absoluteAccrualIntensityEnabled, accrualTime12$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$accrualTime, accrualTime12$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$accrualIntensity, accrualTime12$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$accrualIntensityRelative, accrualTime12$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$maxNumberOfSubjects, accrualTime12$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$remainingTime, accrualTime12$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime12CodeBased$piecewiseAccrualEnabled, accrualTime12$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime12), "character")
	    df <- as.data.frame(accrualTime12)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime12)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime13 <- getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(22, 33))

	## Comparison of the results of AccrualTime object 'accrualTime13' with expected results
	expect_equal(accrualTime13$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime13$followUpTimeMustBeUserDefined, TRUE)
	expect_equal(accrualTime13$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime13$maxNumberOfSubjectsCanBeCalculatedDirectly, FALSE)
	expect_equal(accrualTime13$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime13$accrualTime, c(0, 6))
	expect_equal(accrualTime13$accrualIntensity, c(22, 33))
	expect_equal(accrualTime13$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime13$maxNumberOfSubjects, NA_real_)
	expect_equal(accrualTime13$remainingTime, NA_real_)
	expect_equal(accrualTime13$piecewiseAccrualEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime13), NA)))
	    expect_output(print(accrualTime13)$show())
	    invisible(capture.output(expect_error(summary(accrualTime13), NA)))
	    expect_output(summary(accrualTime13)$show())
	    accrualTime13CodeBased <- eval(parse(text = getObjectRCode(accrualTime13, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime13CodeBased$endOfAccrualIsUserDefined, accrualTime13$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$followUpTimeMustBeUserDefined, accrualTime13$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime13$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime13$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$absoluteAccrualIntensityEnabled, accrualTime13$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$accrualTime, accrualTime13$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$accrualIntensity, accrualTime13$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$accrualIntensityRelative, accrualTime13$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$maxNumberOfSubjects, accrualTime13$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$remainingTime, accrualTime13$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime13CodeBased$piecewiseAccrualEnabled, accrualTime13$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime13), "character")
	    df <- as.data.frame(accrualTime13)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime13)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	accrualTime14 <- getAccrualTime(list(
	    "0 - <6" = 22,
	    "6 - <=30"      = 33
	))

	## Comparison of the results of AccrualTime object 'accrualTime14' with expected results
	expect_equal(accrualTime14$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime14$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime14$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime14$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime14$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime14$accrualTime, c(0, 6, 30))
	expect_equal(accrualTime14$accrualIntensity, c(22, 33))
	expect_equal(accrualTime14$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime14$maxNumberOfSubjects, 924)
	expect_equal(accrualTime14$remainingTime, 24)
	expect_equal(accrualTime14$piecewiseAccrualEnabled, TRUE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(accrualTime14), NA)))
	    expect_output(print(accrualTime14)$show())
	    invisible(capture.output(expect_error(summary(accrualTime14), NA)))
	    expect_output(summary(accrualTime14)$show())
	    accrualTime14CodeBased <- eval(parse(text = getObjectRCode(accrualTime14, stringWrapParagraphWidth = NULL)))
	    expect_equal(accrualTime14CodeBased$endOfAccrualIsUserDefined, accrualTime14$endOfAccrualIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime14CodeBased$followUpTimeMustBeUserDefined, accrualTime14$followUpTimeMustBeUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime14CodeBased$maxNumberOfSubjectsIsUserDefined, accrualTime14$maxNumberOfSubjectsIsUserDefined, tolerance = 1e-05)
	    expect_equal(accrualTime14CodeBased$maxNumberOfSubjectsCanBeCalculatedDirectly, accrualTime14$maxNumberOfSubjectsCanBeCalculatedDirectly, tolerance = 1e-05)
	    expect_equal(accrualTime14CodeBased$absoluteAccrualIntensityEnabled, accrualTime14$absoluteAccrualIntensityEnabled, tolerance = 1e-05)
	    expect_equal(accrualTime14CodeBased$accrualTime, accrualTime14$accrualTime, tolerance = 1e-05)
	    expect_equal(accrualTime14CodeBased$accrualIntensity, accrualTime14$accrualIntensity, tolerance = 1e-05)
	    expect_equal(accrualTime14CodeBased$accrualIntensityRelative, accrualTime14$accrualIntensityRelative, tolerance = 1e-05)
	    expect_equal(accrualTime14CodeBased$maxNumberOfSubjects, accrualTime14$maxNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(accrualTime14CodeBased$remainingTime, accrualTime14$remainingTime, tolerance = 1e-05)
	    expect_equal(accrualTime14CodeBased$piecewiseAccrualEnabled, accrualTime14$piecewiseAccrualEnabled, tolerance = 1e-05)
	    expect_type(names(accrualTime14), "character")
	    df <- as.data.frame(accrualTime14)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(accrualTime14)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("Testing 'getAccrualTime': check expected warnings and errors", {

	# @refFS[Tab.]{fs:tab:output:getAccrualTime}
	expect_warning(getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(0.22, 0.33)),
	    paste0("The specified accrual time and intensity cannot be supplemented ",
	        "automatically with the missing information; therefore further calculations are not possible"),
	    fixed = TRUE
	)

	expect_warning(getAccrualTime(accrualTime = c(0, 24), accrualIntensity = c(30, 45), maxNumberOfSubjects = 720),
	    "Last accrual intensity value (45) ignored",
	    fixed = TRUE
	)

	.skipTestIfDisabled()

	suppressWarnings(expect_warning(getAccrualTime(accrualTime = c(0, 24, 30), 
	        accrualIntensity = c(30, 45, 55), maxNumberOfSubjects = 720),
	    "Last 2 accrual intensity values (45, 55) ignored",
	    fixed = TRUE
	))

	suppressWarnings(expect_warning(getAccrualTime(accrualTime = c(0, 24, 30, 40), 
	        accrualIntensity = c(30, 45, 55, 66), maxNumberOfSubjects = 720),
	    "Last 2 accrual time values (30, 40) ignored",
	    fixed = TRUE
	))

	suppressWarnings(expect_warning(getAccrualTime(accrualTime = c(0, 24, 30, 40), 
	        accrualIntensity = c(30, 45, 55, 66), maxNumberOfSubjects = 720),
	    "Last 3 accrual intensity values (45, 55, 66) ignored",
	    fixed = TRUE
	))

	expect_warning(getAccrualTime(accrualTime = c(0, 6, 15, 25), accrualIntensity = c(0, 22, 33)),
	    "It makes no sense to start 'accrualIntensity' (0, 22, 33) with 0",
	    fixed = TRUE
	)

	expect_error(getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(0)),
	    "Illegal argument: at least one 'accrualIntensity' value must be > 0",
	    fixed = TRUE
	)

	expect_error(getAccrualTime(
	    accrualTime = c(0, 6, 30), accrualIntensity = c(22, 33),
	    maxNumberOfSubjects = 1000
	),
	paste0(
	    "Conflicting arguments: 'maxNumberOfSubjects' (1000) disagrees with the defined ",
	    "accrual time (0, 6, 30) and intensity: 6 * 22 + 24 * 33 = 924"
	),
	fixed = TRUE
	)

})

test_that("Testing 'getAccrualTime': list-wise definition", {

	accrualTime1 <- list(
	    "0  - <12"  = 15,
	    "12 - <13" = 21,
	    "13 - <14" = 27,
	    "14 - <15" = 33,
	    "15 - <16" = 39,
	    ">=16"     = 45
	)

	# @refFS[Tab.]{fs:tab:output:getAccrualTime}
	accrualTime4 <- getAccrualTime(accrualTime = accrualTime1, maxNumberOfSubjects = 1405)
	expect_equal(accrualTime4$accrualTime, c(0, 12, 13, 14, 15, 16, 40.55555556))
	expect_equal(accrualTime4$accrualIntensity, c(15, 21, 27, 33, 39, 45))
	expect_equal(accrualTime4$remainingTime, 24.55555556)

	.skipTestIfDisabled()

	accrualTime2 <- list(
	    "0  - <12"  = 15,
	    "12 - <13" = 21,
	    "13 - <14" = 27,
	    "14 - <15" = 33,
	    "15 - <16" = 39,
	    "16 - ?"   = 45
	)
	accrualTime5 <- getAccrualTime(accrualTime = accrualTime2, maxNumberOfSubjects = 1405)
	expect_equal(accrualTime5$accrualTime, c(0, 12, 13, 14, 15, 16, 40.55555556))
	expect_equal(accrualTime5$accrualIntensity, c(15, 21, 27, 33, 39, 45))
	expect_equal(accrualTime5$remainingTime, 24.55555556)

	accrualTime3 <- list(
	    "0 - <11"  = 20,
	    "11 - <16" = 40,
	    ">=16"     = 60
	)
	accrualTime6 <- getAccrualTime(accrualTime = accrualTime3, maxNumberOfSubjects = 800)
	expect_equal(accrualTime6$accrualTime, c(0, 11, 16, 22.3333333))
	expect_equal(accrualTime6$accrualIntensity, c(20, 40, 60))
	expect_equal(accrualTime6$remainingTime, 6.33333333)

	accrualTime7 <- list(
	    "0 - <11"  = 20,
	    "11 - <16" = 40,
	    "16 - ?"   = 60
	)
	accrualTime8 <- getAccrualTime(accrualTime = accrualTime7, maxNumberOfSubjects = 800)
	expect_equal(accrualTime8$accrualTime, c(0, 11, 16, 22.3333333))
	expect_equal(accrualTime8$accrualIntensity, c(20, 40, 60))
	expect_equal(accrualTime8$remainingTime, 6.33333333)

})

test_that("Testing 'getPiecewiseSurvivalTime': mixed arguments", {

	# @refFS[Tab.]{fs:tab:output:getPiecewiseSurvivalTime}
	pwSurvivalTime1 <- getPiecewiseSurvivalTime(median1 = 37, hazardRatio = 0.8)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime1' with expected results
	expect_equal(pwSurvivalTime1$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime1$lambda1, 0.018733708, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$lambda2, 0.023417134, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$pi1, NA_real_)
	expect_equal(pwSurvivalTime1$pi2, NA_real_)
	expect_equal(pwSurvivalTime1$median1, 37)
	expect_equal(pwSurvivalTime1$median2, 29.6, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$eventTime, NA_real_)
	expect_equal(pwSurvivalTime1$kappa, 1)
	expect_equal(pwSurvivalTime1$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime1$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime1$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime1), NA)))
	    expect_output(print(pwSurvivalTime1)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime1), NA)))
	    expect_output(summary(pwSurvivalTime1)$show())
	    pwSurvivalTime1CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime1, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime1CodeBased$piecewiseSurvivalTime, pwSurvivalTime1$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$lambda1, pwSurvivalTime1$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$lambda2, pwSurvivalTime1$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$hazardRatio, pwSurvivalTime1$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$pi1, pwSurvivalTime1$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$pi2, pwSurvivalTime1$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$median1, pwSurvivalTime1$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$median2, pwSurvivalTime1$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$eventTime, pwSurvivalTime1$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$kappa, pwSurvivalTime1$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime1$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$delayedResponseAllowed, pwSurvivalTime1$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime1CodeBased$delayedResponseEnabled, pwSurvivalTime1$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime1), "character")
	    df <- as.data.frame(pwSurvivalTime1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime2 <- getPiecewiseSurvivalTime(lambda1 = 0.01873371, median2 = 29.6)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime2' with expected results
	expect_equal(pwSurvivalTime2$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime2$lambda1, 0.01873371, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$lambda2, 0.023417134, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$hazardRatio, 0.8000001, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$pi1, NA_real_)
	expect_equal(pwSurvivalTime2$pi2, NA_real_)
	expect_equal(pwSurvivalTime2$median1, 36.999995, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$median2, 29.6, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$eventTime, NA_real_)
	expect_equal(pwSurvivalTime2$kappa, 1)
	expect_equal(pwSurvivalTime2$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime2$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime2$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime2), NA)))
	    expect_output(print(pwSurvivalTime2)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime2), NA)))
	    expect_output(summary(pwSurvivalTime2)$show())
	    pwSurvivalTime2CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime2, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime2CodeBased$piecewiseSurvivalTime, pwSurvivalTime2$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$lambda1, pwSurvivalTime2$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$lambda2, pwSurvivalTime2$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$hazardRatio, pwSurvivalTime2$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$pi1, pwSurvivalTime2$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$pi2, pwSurvivalTime2$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$median1, pwSurvivalTime2$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$median2, pwSurvivalTime2$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$eventTime, pwSurvivalTime2$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$kappa, pwSurvivalTime2$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime2$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$delayedResponseAllowed, pwSurvivalTime2$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime2CodeBased$delayedResponseEnabled, pwSurvivalTime2$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime2), "character")
	    df <- as.data.frame(pwSurvivalTime2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	pwSurvivalTime3 <- getPiecewiseSurvivalTime(median1 = 37, lambda2 = 0.02341713)

	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime3' with expected results
	expect_equal(pwSurvivalTime3$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime3$lambda1, 0.018733708, tolerance = 1e-07)
	expect_equal(pwSurvivalTime3$lambda2, 0.02341713, tolerance = 1e-07)
	expect_equal(pwSurvivalTime3$hazardRatio, 0.80000015, tolerance = 1e-07)
	expect_equal(pwSurvivalTime3$pi1, NA_real_)
	expect_equal(pwSurvivalTime3$pi2, NA_real_)
	expect_equal(pwSurvivalTime3$median1, 37)
	expect_equal(pwSurvivalTime3$median2, 29.600006, tolerance = 1e-07)
	expect_equal(pwSurvivalTime3$eventTime, NA_real_)
	expect_equal(pwSurvivalTime3$kappa, 1)
	expect_equal(pwSurvivalTime3$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime3$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime3$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(pwSurvivalTime3), NA)))
	    expect_output(print(pwSurvivalTime3)$show())
	    invisible(capture.output(expect_error(summary(pwSurvivalTime3), NA)))
	    expect_output(summary(pwSurvivalTime3)$show())
	    pwSurvivalTime3CodeBased <- eval(parse(text = getObjectRCode(pwSurvivalTime3, stringWrapParagraphWidth = NULL)))
	    expect_equal(pwSurvivalTime3CodeBased$piecewiseSurvivalTime, pwSurvivalTime3$piecewiseSurvivalTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$lambda1, pwSurvivalTime3$lambda1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$lambda2, pwSurvivalTime3$lambda2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$hazardRatio, pwSurvivalTime3$hazardRatio, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$pi1, pwSurvivalTime3$pi1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$pi2, pwSurvivalTime3$pi2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$median1, pwSurvivalTime3$median1, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$median2, pwSurvivalTime3$median2, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$eventTime, pwSurvivalTime3$eventTime, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$kappa, pwSurvivalTime3$kappa, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$piecewiseSurvivalEnabled, pwSurvivalTime3$piecewiseSurvivalEnabled, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$delayedResponseAllowed, pwSurvivalTime3$delayedResponseAllowed, tolerance = 1e-05)
	    expect_equal(pwSurvivalTime3CodeBased$delayedResponseEnabled, pwSurvivalTime3$delayedResponseEnabled, tolerance = 1e-05)
	    expect_type(names(pwSurvivalTime3), "character")
	    df <- as.data.frame(pwSurvivalTime3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(pwSurvivalTime3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	expect_warning(getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), pi1 = 0.3),
	    "'hazardRatio' (0.6, 0.8) will be ignored because it will be calculated",
	    fixed = TRUE
	)
})

