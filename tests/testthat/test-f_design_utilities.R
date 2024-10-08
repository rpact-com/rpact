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
## |  File name: test-f_design_utilities.R
## |  Creation date: 08 November 2023, 09:10:53
## |  File version: $Revision: 8274 $
## |  Last changed: $Date: 2024-09-26 11:33:59 +0200 (Do, 26 Sep 2024) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing Design Utility Functions")


test_that("'getPiByLambda' and 'getLambdaByPi' produce corresponding results", {
	expect_equal(getLambdaByPi(getPiByLambda(0.01, eventTime = 1, kappa = 1), eventTime = 1, kappa = 1), 0.01, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.03, eventTime = 1, kappa = 1), eventTime = 1, kappa = 1), 0.03, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.05, eventTime = 1, kappa = 1), eventTime = 1, kappa = 1), 0.05, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.07, eventTime = 1, kappa = 1), eventTime = 1, kappa = 1), 0.07, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.09, eventTime = 1, kappa = 1), eventTime = 1, kappa = 1), 0.09, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.01, eventTime = 1, kappa = 3), eventTime = 1, kappa = 3), 0.01, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.03, eventTime = 1, kappa = 3), eventTime = 1, kappa = 3), 0.03, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.05, eventTime = 1, kappa = 3), eventTime = 1, kappa = 3), 0.05, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.07, eventTime = 1, kappa = 3), eventTime = 1, kappa = 3), 0.07, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.09, eventTime = 1, kappa = 3), eventTime = 1, kappa = 3), 0.09, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.01, eventTime = 1, kappa = 5), eventTime = 1, kappa = 5), 0.01, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.03, eventTime = 1, kappa = 5), eventTime = 1, kappa = 5), 0.03, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.05, eventTime = 1, kappa = 5), eventTime = 1, kappa = 5), 0.05, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.07, eventTime = 1, kappa = 5), eventTime = 1, kappa = 5), 0.07, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.09, eventTime = 1, kappa = 5), eventTime = 1, kappa = 5), 0.09, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.01, eventTime = 6, kappa = 1), eventTime = 6, kappa = 1), 0.01, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.03, eventTime = 6, kappa = 1), eventTime = 6, kappa = 1), 0.03, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.05, eventTime = 6, kappa = 1), eventTime = 6, kappa = 1), 0.05, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.07, eventTime = 6, kappa = 1), eventTime = 6, kappa = 1), 0.07, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.09, eventTime = 6, kappa = 1), eventTime = 6, kappa = 1), 0.09, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.01, eventTime = 6, kappa = 3), eventTime = 6, kappa = 3), 0.01, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.03, eventTime = 6, kappa = 3), eventTime = 6, kappa = 3), 0.03, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.05, eventTime = 6, kappa = 3), eventTime = 6, kappa = 3), 0.05, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.07, eventTime = 6, kappa = 3), eventTime = 6, kappa = 3), 0.07, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.09, eventTime = 6, kappa = 3), eventTime = 6, kappa = 3), 0.09, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.01, eventTime = 6, kappa = 5), eventTime = 6, kappa = 5), 0.01, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.03, eventTime = 6, kappa = 5), eventTime = 6, kappa = 5), 0.03, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.05, eventTime = 6, kappa = 5), eventTime = 6, kappa = 5), 0.05, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.07, eventTime = 6, kappa = 5), eventTime = 6, kappa = 5), 0.07, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.09, eventTime = 6, kappa = 5), eventTime = 6, kappa = 5), 0.09, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.01, eventTime = 11, kappa = 1), eventTime = 11, kappa = 1), 0.01, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.03, eventTime = 11, kappa = 1), eventTime = 11, kappa = 1), 0.03, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.05, eventTime = 11, kappa = 1), eventTime = 11, kappa = 1), 0.05, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.07, eventTime = 11, kappa = 1), eventTime = 11, kappa = 1), 0.07, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.09, eventTime = 11, kappa = 1), eventTime = 11, kappa = 1), 0.09, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.01, eventTime = 11, kappa = 3), eventTime = 11, kappa = 3), 0.01, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.03, eventTime = 11, kappa = 3), eventTime = 11, kappa = 3), 0.03, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.05, eventTime = 11, kappa = 3), eventTime = 11, kappa = 3), 0.05, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.07, eventTime = 11, kappa = 3), eventTime = 11, kappa = 3), 0.07, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.09, eventTime = 11, kappa = 3), eventTime = 11, kappa = 3), 0.09, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.01, eventTime = 11, kappa = 5), eventTime = 11, kappa = 5), 0.01, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.03, eventTime = 11, kappa = 5), eventTime = 11, kappa = 5), 0.03, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.05, eventTime = 11, kappa = 5), eventTime = 11, kappa = 5), 0.05, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.07, eventTime = 11, kappa = 5), eventTime = 11, kappa = 5), 0.07, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.09, eventTime = 11, kappa = 5), eventTime = 11, kappa = 5), 0.09, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.01, eventTime = 16, kappa = 1), eventTime = 16, kappa = 1), 0.01, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.03, eventTime = 16, kappa = 1), eventTime = 16, kappa = 1), 0.03, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.05, eventTime = 16, kappa = 1), eventTime = 16, kappa = 1), 0.05, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.07, eventTime = 16, kappa = 1), eventTime = 16, kappa = 1), 0.07, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.09, eventTime = 16, kappa = 1), eventTime = 16, kappa = 1), 0.09, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.01, eventTime = 16, kappa = 3), eventTime = 16, kappa = 3), 0.01, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.03, eventTime = 16, kappa = 3), eventTime = 16, kappa = 3), 0.03, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.05, eventTime = 16, kappa = 3), eventTime = 16, kappa = 3), 0.05, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.07, eventTime = 16, kappa = 3), eventTime = 16, kappa = 3), 0.07, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.09, eventTime = 16, kappa = 3), eventTime = 16, kappa = 3), 0.09, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.01, eventTime = 16, kappa = 5), eventTime = 16, kappa = 5), 0.01, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.03, eventTime = 16, kappa = 5), eventTime = 16, kappa = 5), 0.03, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.05, eventTime = 16, kappa = 5), eventTime = 16, kappa = 5), 0.05, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.07, eventTime = 16, kappa = 5), eventTime = 16, kappa = 5), 0.07, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.09, eventTime = 16, kappa = 5), eventTime = 16, kappa = 5), 0.09, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.01, eventTime = 21, kappa = 1), eventTime = 21, kappa = 1), 0.01, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.03, eventTime = 21, kappa = 1), eventTime = 21, kappa = 1), 0.03, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.05, eventTime = 21, kappa = 1), eventTime = 21, kappa = 1), 0.05, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.07, eventTime = 21, kappa = 1), eventTime = 21, kappa = 1), 0.07, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.09, eventTime = 21, kappa = 1), eventTime = 21, kappa = 1), 0.09, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.01, eventTime = 21, kappa = 3), eventTime = 21, kappa = 3), 0.01, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.03, eventTime = 21, kappa = 3), eventTime = 21, kappa = 3), 0.03, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.05, eventTime = 21, kappa = 3), eventTime = 21, kappa = 3), 0.05, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.07, eventTime = 21, kappa = 3), eventTime = 21, kappa = 3), 0.07, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.09, eventTime = 21, kappa = 3), eventTime = 21, kappa = 3), 0.09, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.01, eventTime = 21, kappa = 5), eventTime = 21, kappa = 5), 0.01, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.03, eventTime = 21, kappa = 5), eventTime = 21, kappa = 5), 0.03, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.05, eventTime = 21, kappa = 5), eventTime = 21, kappa = 5), 0.05, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.07, eventTime = 21, kappa = 5), eventTime = 21, kappa = 5), 0.07, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.09, eventTime = 21, kappa = 5), eventTime = 21, kappa = 5), 0.09, tolerance = 1e-04)




})

test_that("'getPiecewiseExponentialDistribution' and 'getPiecewiseExponentialQuantile' produce corresponding results", {

	# @refFS[Formula]{fs:pieceWiseExponentialSurvival}
	piecewiseLambda <- c(0.03, 0.05, 0.08)
	piecewiseSurvivalTime <- c(0, 16, 22)
	time <- seq(2, 50, 4)
	quantile <- getPiecewiseExponentialDistribution(time,
	    piecewiseSurvivalTime = piecewiseSurvivalTime, piecewiseLambda = piecewiseLambda
	)
	y <- getPiecewiseExponentialQuantile(quantile,
	    piecewiseSurvivalTime = piecewiseSurvivalTime, piecewiseLambda = piecewiseLambda
	)

	expect_equal(y, time, tolerance = 1e-06)

})

test_that("'ppwexp' and 'qpwexp' produce corresponding results", {

	# @refFS[Formula]{fs:pieceWiseExponentialSurvival}
	piecewiseLambda <- c(0.03, 0.05, 0.08)
	piecewiseSurvivalTime <- c(0, 16, 22)
	time <- seq(2, 50, 4)
	quantile <- ppwexp(time,
	    s = piecewiseSurvivalTime, lambda = piecewiseLambda
	)
	y <- qpwexp(quantile,
	    s = piecewiseSurvivalTime, lambda = piecewiseLambda
	)

	expect_equal(y, time, tolerance = 1e-06)

})

test_that("'getPiecewiseExponentialDistribution' and 'getPiecewiseExponentialQuantile' produce corresponding results ('piecewiseSurvivalTime' defined as list)", {

	# @refFS[Formula]{fs:pieceWiseExponentialSurvival}
	piecewiseSurvivalTime <- list(
	    "<16"      = 0.03,
	    "16 - <22" = 0.05,
	    ">=22"      = 0.08
	)
	time <- seq(2, 50, 4)
	quantile <- getPiecewiseExponentialDistribution(time,
	    piecewiseSurvivalTime = piecewiseSurvivalTime
	)
	y <- getPiecewiseExponentialQuantile(quantile,
	    piecewiseSurvivalTime = piecewiseSurvivalTime
	)

	expect_equal(y, time, tolerance = 1e-06)

})

test_that("'ppwexp' and 'qpwexp' produce corresponding results ('piecewiseSurvivalTime' defined as list)", {

	# @refFS[Formula]{fs:pieceWiseExponentialSurvival}
	piecewiseSurvivalTime <- list(
	    "<16"      = 0.03,
	    "16 - <22" = 0.05,
	    ">=22"      = 0.08
	)
	time <- seq(2, 50, 4)
	quantile <- ppwexp(time, s = piecewiseSurvivalTime)
	y <- qpwexp(quantile, s = piecewiseSurvivalTime)

	expect_equal(y, time, tolerance = 1e-06)

})

test_that("'getPiecewiseExponentialRandomNumbers': test that mean random numbers are as expected", {

	# @refFS[Formula]{fs:pieceWiseExponentialSurvival}
	set.seed(123456)
	piecewiseSurvivalTime <- c(0, 16, 22)
	piecewiseLambda <- c(0.003, 0.003, 0.003)
	y <- 1 / mean(getPiecewiseExponentialRandomNumbers(100000,
	    piecewiseSurvivalTime = piecewiseSurvivalTime, piecewiseLambda = piecewiseLambda, kappa = 1
	))
	expect_equal(y, piecewiseLambda[1], tolerance = 5e-04)

})

test_that("'rpwexp': test that mean random numbers are as expected", {

	# @refFS[Formula]{fs:pieceWiseExponentialSurvival}
	set.seed(123456)
	piecewiseSurvivalTime <- c(0, 16, 22)
	piecewiseLambda <- c(0.003, 0.003, 0.003)
	y <- 1 / mean(rpwexp(100000, s = piecewiseSurvivalTime, lambda = piecewiseLambda, kappa = 1))

	expect_equal(y, piecewiseLambda[1], tolerance = 5e-04)

})

test_that("'getPiecewiseExponentialRandomNumbers': test that mean random numbers are as expected ('piecewiseSurvivalTime' defined as list)", {

	# @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
	set.seed(123456)
	piecewiseSurvivalTime <- list(
	    "<16"      = 0.003,
	    "16 - <22" = 0.003,
	    ">=22"      = 0.003
	)
	y <- 1 / mean(getPiecewiseExponentialRandomNumbers(100000,
	    piecewiseSurvivalTime = piecewiseSurvivalTime, kappa = 1
	))

	expect_equal(y, 0.003, tolerance = 5e-04)

})

test_that("'rpwexp': test that mean random numbers are as expected ('piecewiseSurvivalTime' defined as list)", {

	# @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
	set.seed(123456)
	piecewiseSurvivalTime <- list(
	    "<16"      = 0.003,
	    "16 - <22" = 0.003,
	    ">=22"      = 0.003
	)
	y <- 1 / mean(rpwexp(100000, s = piecewiseSurvivalTime, kappa = 1))

	expect_equal(y, 0.003, tolerance = 5e-04)

})

test_that("'getPiecewiseExponentialDistribution': test that function call with singel lambda is working", {

	expect_equal(getPiecewiseExponentialDistribution(4, piecewiseLambda = 0.003), 0.01192829, tolerance = 5e-05)

})

test_that("'.convertStageWiseToOverallValues': test that function is working as expected", {

	x1 <- .convertStageWiseToOverallValues(c(1:5))

	## Comparison of the results of matrixarray object 'x1' with expected results
	expect_equal(x1[1, ], 1, label = paste0("c(", paste0(x1[1, ], collapse = ", "), ")"))
	expect_equal(x1[2, ], 3, label = paste0("c(", paste0(x1[2, ], collapse = ", "), ")"))
	expect_equal(x1[3, ], 6, label = paste0("c(", paste0(x1[3, ], collapse = ", "), ")"))
	expect_equal(x1[4, ], 10, label = paste0("c(", paste0(x1[4, ], collapse = ", "), ")"))
	expect_equal(x1[5, ], 15, label = paste0("c(", paste0(x1[5, ], collapse = ", "), ")"))

	x2 <- .convertStageWiseToOverallValues(matrix(c(1:5), ncol = 1))

	## Comparison of the results of matrixarray object 'x2' with expected results
	expect_equal(x2[1, ], 1, label = paste0("c(", paste0(x2[1, ], collapse = ", "), ")"))
	expect_equal(x2[2, ], 3, label = paste0("c(", paste0(x2[2, ], collapse = ", "), ")"))
	expect_equal(x2[3, ], 6, label = paste0("c(", paste0(x2[3, ], collapse = ", "), ")"))
	expect_equal(x2[4, ], 10, label = paste0("c(", paste0(x2[4, ], collapse = ", "), ")"))
	expect_equal(x2[5, ], 15, label = paste0("c(", paste0(x2[5, ], collapse = ", "), ")"))

	x3 <- .convertStageWiseToOverallValues(matrix(c(1:5), nrow = 1))

	## Comparison of the results of matrixarray object 'x3' with expected results
	expect_equal(x3[1, ], c(1, 2, 3, 4, 5), label = paste0("c(", paste0(x3[1, ], collapse = ", "), ")"))

	x4 <- .convertStageWiseToOverallValues(matrix(c(1:5, 1:5), ncol = 2))

	## Comparison of the results of matrixarray object 'x4' with expected results
	expect_equal(x4[1, ], c(1, 1), label = paste0("c(", paste0(x4[1, ], collapse = ", "), ")"))
	expect_equal(x4[2, ], c(3, 3), label = paste0("c(", paste0(x4[2, ], collapse = ", "), ")"))
	expect_equal(x4[3, ], c(6, 6), label = paste0("c(", paste0(x4[3, ], collapse = ", "), ")"))
	expect_equal(x4[4, ], c(10, 10), label = paste0("c(", paste0(x4[4, ], collapse = ", "), ")"))
	expect_equal(x4[5, ], c(15, 15), label = paste0("c(", paste0(x4[5, ], collapse = ", "), ")"))

	x5 <- .convertStageWiseToOverallValues(matrix(sort(rep(1:5, 2)), nrow = 2))

	## Comparison of the results of matrixarray object 'x5' with expected results
	expect_equal(x5[1, ], c(1, 2, 3, 4, 5), label = paste0("c(", paste0(x5[1, ], collapse = ", "), ")"))
	expect_equal(x5[2, ], c(2, 4, 6, 8, 10), label = paste0("c(", paste0(x5[2, ], collapse = ", "), ")"))
})

test_that("The default value functions for all designs have all arguments included", {
    testthat::expect_error(.getDesignGroupSequentialDefaultValues(designFun = getDesignGroupSequential), NA)
    testthat::expect_error(.getDesignFisherDefaultValues(designFun = getDesignFisher), NA)
})

