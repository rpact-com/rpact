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
## |  File name: test-f_simulation_base_rates.R
## |  Creation date: 23 February 2022, 14:06:36
## |  File version: $Revision: 5881 $
## |  Last changed: $Date: 2022-02-24 12:35:06 +0100 (Do, 24 Feb 2022) $
## |  Last changed by: $Author: pahlke $
## |  

context("Testing Simulation Rates Function")


test_that("'getSimulationRates': check several configurations", {
	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
	# @refFS[Tab.]{fs:tab:output:getSimulationRates}
	# @refFS[Formula]{fs:simulationOneArmRatesGenerate}
	# @refFS[Formula]{fs:pValuesOneRateAlternativeGreater}
	# @refFS[Formula]{fs:pValuesOneRateAlternativeSmaller}
	# @refFS[Formula]{fs:simulationTwoArmRatesGenerate}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeGreater}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeSmaller}
	# @refFS[Formula]{fs:pValuesTwoRatesApproximationAlternativeGreater}
	# @refFS[Formula]{fs:pValuesTwoRatesApproximationAlternativeSmaller}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	seed <- 99123
	maxNumberOfIterations <- 100
	options(width = 180)
	maxNumberOfSubjects <- 90
	informationRates <- (1:3) / 3
	plannedSubjects <- round(informationRates * maxNumberOfSubjects)

	x1 <- getSimulationRates(
	    design = getDesignInverseNormal(
	        futilityBounds = c(-0.5, 0.5),
	        informationRates = informationRates
	    ), groups = 2, riskRatio = TRUE, thetaH0 = 0.8,
	    plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations,
	    allocationRatioPlanned = 3, seed = seed
	)

	## Comparison of the results of SimulationResultsRates object 'x1' with expected results
	expect_equal(x1$effect, c(0.2, 0.7, 1.2, 1.7), tolerance = 1e-07)
	expect_equal(x1$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(x1$iterations[2, ], c(78, 93, 99, 96))
	expect_equal(x1$iterations[3, ], c(41, 68, 56, 40))
	expect_equal(x1$overallReject, c(0.05, 0.23, 0.74, 0.88), tolerance = 1e-07)
	expect_equal(x1$rejectPerStage[1, ], c(0, 0, 0, 0.04), tolerance = 1e-07)
	expect_equal(x1$rejectPerStage[2, ], c(0.02, 0.04, 0.34, 0.54), tolerance = 1e-07)
	expect_equal(x1$rejectPerStage[3, ], c(0.03, 0.19, 0.4, 0.3), tolerance = 1e-07)
	expect_equal(x1$futilityStop, c(0.57, 0.28, 0.1, 0.02), tolerance = 1e-07)
	expect_equal(x1$futilityPerStage[1, ], c(0.22, 0.07, 0.01, 0), tolerance = 1e-07)
	expect_equal(x1$futilityPerStage[2, ], c(0.35, 0.21, 0.09, 0.02), tolerance = 1e-07)
	expect_equal(x1$earlyStop, c(0.59, 0.32, 0.44, 0.6), tolerance = 1e-07)
	expect_equal(x1$expectedNumberOfSubjects, c(65.7, 78.3, 76.5, 70.8), tolerance = 1e-07)
	expect_equal(x1$sampleSizes[1, ], c(30, 30, 30, 30))
	expect_equal(x1$sampleSizes[2, ], c(30, 30, 30, 30))
	expect_equal(x1$sampleSizes[3, ], c(30, 30, 30, 30))
	expect_equal(x1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$conditionalPowerAchieved[2, ], c(0.090943215, 0.15808459, 0.48521663, 0.52642331), tolerance = 1e-07)
	expect_equal(x1$conditionalPowerAchieved[3, ], c(0.22475932, 0.38294099, 0.60961381, 0.67377136), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	    x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x1CodeBased$effect, x1$effect, tolerance = 1e-05)
	    expect_equal(x1CodeBased$iterations, x1$iterations, tolerance = 1e-05)
	    expect_equal(x1CodeBased$overallReject, x1$overallReject, tolerance = 1e-05)
	    expect_equal(x1CodeBased$rejectPerStage, x1$rejectPerStage, tolerance = 1e-05)
	    expect_equal(x1CodeBased$futilityStop, x1$futilityStop, tolerance = 1e-05)
	    expect_equal(x1CodeBased$futilityPerStage, x1$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x1CodeBased$earlyStop, x1$earlyStop, tolerance = 1e-05)
	    expect_equal(x1CodeBased$expectedNumberOfSubjects, x1$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x1CodeBased$sampleSizes, x1$sampleSizes, tolerance = 1e-05)
	    expect_equal(x1CodeBased$conditionalPowerAchieved, x1$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x1), "character")
	    df <- as.data.frame(x1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x2 <- getSimulationRates(
	    design = getDesignInverseNormal(
	        futilityBounds = c(-0.5, 0.5),
	        informationRates = informationRates
	    ), groups = 2, riskRatio = FALSE, thetaH0 = -0.1,
	    plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations,
	    allocationRatioPlanned = 3, seed = seed
	)

	## Comparison of the results of SimulationResultsRates object 'x2' with expected results
	expect_equal(x2$effect, c(0.1, 0.2, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x2$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(x2$iterations[2, ], c(84, 95, 100, 97))
	expect_equal(x2$iterations[3, ], c(55, 73, 64, 42))
	expect_equal(x2$overallReject, c(0.08, 0.39, 0.81, 0.88), tolerance = 1e-07)
	expect_equal(x2$rejectPerStage[1, ], c(0, 0, 0, 0.03), tolerance = 1e-07)
	expect_equal(x2$rejectPerStage[2, ], c(0.02, 0.09, 0.33, 0.53), tolerance = 1e-07)
	expect_equal(x2$rejectPerStage[3, ], c(0.06, 0.3, 0.48, 0.32), tolerance = 1e-07)
	expect_equal(x2$futilityStop, c(0.43, 0.18, 0.03, 0.02), tolerance = 1e-07)
	expect_equal(x2$futilityPerStage[1, ], c(0.16, 0.05, 0, 0), tolerance = 1e-07)
	expect_equal(x2$futilityPerStage[2, ], c(0.27, 0.13, 0.03, 0.02), tolerance = 1e-07)
	expect_equal(x2$earlyStop, c(0.45, 0.27, 0.36, 0.58), tolerance = 1e-07)
	expect_equal(x2$expectedNumberOfSubjects, c(71.7, 80.4, 79.2, 71.7), tolerance = 1e-07)
	expect_equal(x2$sampleSizes[1, ], c(30, 30, 30, 30))
	expect_equal(x2$sampleSizes[2, ], c(30, 30, 30, 30))
	expect_equal(x2$sampleSizes[3, ], c(30, 30, 30, 30))
	expect_equal(x2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$conditionalPowerAchieved[2, ], c(0.10237911, 0.25306891, 0.43740091, 0.54067879), tolerance = 1e-07)
	expect_equal(x2$conditionalPowerAchieved[3, ], c(0.30171473, 0.4623858, 0.59071853, 0.68245332), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	    x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x2CodeBased$effect, x2$effect, tolerance = 1e-05)
	    expect_equal(x2CodeBased$iterations, x2$iterations, tolerance = 1e-05)
	    expect_equal(x2CodeBased$overallReject, x2$overallReject, tolerance = 1e-05)
	    expect_equal(x2CodeBased$rejectPerStage, x2$rejectPerStage, tolerance = 1e-05)
	    expect_equal(x2CodeBased$futilityStop, x2$futilityStop, tolerance = 1e-05)
	    expect_equal(x2CodeBased$futilityPerStage, x2$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x2CodeBased$earlyStop, x2$earlyStop, tolerance = 1e-05)
	    expect_equal(x2CodeBased$expectedNumberOfSubjects, x2$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x2CodeBased$sampleSizes, x2$sampleSizes, tolerance = 1e-05)
	    expect_equal(x2CodeBased$conditionalPowerAchieved, x2$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x2), "character")
	    df <- as.data.frame(x2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

	x3 <- getSimulationRates(
	    design = getDesignInverseNormal(
	        futilityBounds = c(-0.5, 0.5),
	        informationRates = informationRates
	    ), groups = 1, thetaH0 = 0.2, pi1 = seq(0.2, 0.4, 0.05),
	    plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, seed = seed
	)

	## Comparison of the results of SimulationResultsRates object 'x3' with expected results
	expect_equal(x3$effect, c(0, 0.05, 0.1, 0.15, 0.2), tolerance = 1e-07)
	expect_equal(x3$iterations[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x3$iterations[2, ], c(78, 91, 96, 90, 72))
	expect_equal(x3$iterations[3, ], c(32, 65, 62, 37, 6))
	expect_equal(x3$overallReject, c(0.03, 0.3, 0.6, 0.93, 0.99), tolerance = 1e-07)
	expect_equal(x3$rejectPerStage[1, ], c(0, 0.02, 0.04, 0.1, 0.28), tolerance = 1e-07)
	expect_equal(x3$rejectPerStage[2, ], c(0.01, 0.06, 0.28, 0.53, 0.66), tolerance = 1e-07)
	expect_equal(x3$rejectPerStage[3, ], c(0.02, 0.22, 0.28, 0.3, 0.05), tolerance = 1e-07)
	expect_equal(x3$futilityStop, c(0.67, 0.27, 0.06, 0, 0), tolerance = 1e-07)
	expect_equal(x3$futilityPerStage[1, ], c(0.22, 0.07, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x3$futilityPerStage[2, ], c(0.45, 0.2, 0.06, 0, 0), tolerance = 1e-07)
	expect_equal(x3$earlyStop, c(0.68, 0.35, 0.38, 0.63, 0.94), tolerance = 1e-07)
	expect_equal(x3$expectedNumberOfSubjects, c(63, 76.8, 77.4, 68.1, 53.4), tolerance = 1e-07)
	expect_equal(x3$sampleSizes[1, ], c(30, 30, 30, 30, 30))
	expect_equal(x3$sampleSizes[2, ], c(30, 30, 30, 30, 30))
	expect_equal(x3$sampleSizes[3, ], c(30, 30, 30, 30, 30))
	expect_equal(x3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$conditionalPowerAchieved[2, ], c(0.12773913, 0.18983473, 0.36146118, 0.53982038, 0.7268178), tolerance = 1e-07)
	expect_equal(x3$conditionalPowerAchieved[3, ], c(0.32676971, 0.35596086, 0.46114911, 0.56126649, 0.75350644), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	    x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x3CodeBased$effect, x3$effect, tolerance = 1e-05)
	    expect_equal(x3CodeBased$iterations, x3$iterations, tolerance = 1e-05)
	    expect_equal(x3CodeBased$overallReject, x3$overallReject, tolerance = 1e-05)
	    expect_equal(x3CodeBased$rejectPerStage, x3$rejectPerStage, tolerance = 1e-05)
	    expect_equal(x3CodeBased$futilityStop, x3$futilityStop, tolerance = 1e-05)
	    expect_equal(x3CodeBased$futilityPerStage, x3$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x3CodeBased$earlyStop, x3$earlyStop, tolerance = 1e-05)
	    expect_equal(x3CodeBased$expectedNumberOfSubjects, x3$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x3CodeBased$sampleSizes, x3$sampleSizes, tolerance = 1e-05)
	    expect_equal(x3CodeBased$conditionalPowerAchieved, x3$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x3), "character")
	    df <- as.data.frame(x3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x4 <- getSimulationRates(
	    design = getDesignInverseNormal(
	        futilityBounds = c(0.5, 0.5),
	        informationRates = informationRates
	    ), groups = 2, riskRatio = TRUE, thetaH0 = 1.5,
	    pi1 = seq(0.05, 0.25, 0.05), plannedSubjects = plannedSubjects,
	    maxNumberOfIterations = maxNumberOfIterations, directionUpper = FALSE,
	    allocationRatioPlanned = 3, seed = seed
	)

	## Comparison of the results of SimulationResultsRates object 'x4' with expected results
	expect_equal(x4$effect, c(-1.25, -1, -0.75, -0.5, -0.25), tolerance = 1e-07)
	expect_equal(x4$iterations[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x4$iterations[2, ], c(74, 64, 47, 36, 39))
	expect_equal(x4$iterations[3, ], c(28, 28, 30, 20, 25))
	expect_equal(x4$overallReject, c(0.66, 0.51, 0.19, 0.08, 0.1), tolerance = 1e-07)
	expect_equal(x4$rejectPerStage[1, ], c(0.06, 0.05, 0.02, 0, 0), tolerance = 1e-07)
	expect_equal(x4$rejectPerStage[2, ], c(0.43, 0.29, 0.09, 0.04, 0.04), tolerance = 1e-07)
	expect_equal(x4$rejectPerStage[3, ], c(0.17, 0.17, 0.08, 0.04, 0.06), tolerance = 1e-07)
	expect_equal(x4$futilityStop, c(0.23, 0.38, 0.59, 0.76, 0.71), tolerance = 1e-07)
	expect_equal(x4$futilityPerStage[1, ], c(0.2, 0.31, 0.51, 0.64, 0.61), tolerance = 1e-07)
	expect_equal(x4$futilityPerStage[2, ], c(0.03, 0.07, 0.08, 0.12, 0.1), tolerance = 1e-07)
	expect_equal(x4$earlyStop, c(0.72, 0.72, 0.7, 0.8, 0.75), tolerance = 1e-07)
	expect_equal(x4$expectedNumberOfSubjects, c(60.6, 57.6, 53.1, 46.8, 49.2), tolerance = 1e-07)
	expect_equal(x4$sampleSizes[1, ], c(30, 30, 30, 30, 30))
	expect_equal(x4$sampleSizes[2, ], c(30, 30, 30, 30, 30))
	expect_equal(x4$sampleSizes[3, ], c(30, 30, 30, 30, 30))
	expect_equal(x4$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$conditionalPowerAchieved[2, ], c(0.65569733, 0.50411153, 0.40992455, 0.37112776, 0.28877148), tolerance = 1e-07)
	expect_equal(x4$conditionalPowerAchieved[3, ], c(0.52876953, 0.55375049, 0.46252843, 0.37280654, 0.34687207), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	    x4CodeBased <- eval(parse(text = getObjectRCode(x4, stringWrapParagraphWidth = NULL)))
	    expect_equal(x4CodeBased$effect, x4$effect, tolerance = 1e-05)
	    expect_equal(x4CodeBased$iterations, x4$iterations, tolerance = 1e-05)
	    expect_equal(x4CodeBased$overallReject, x4$overallReject, tolerance = 1e-05)
	    expect_equal(x4CodeBased$rejectPerStage, x4$rejectPerStage, tolerance = 1e-05)
	    expect_equal(x4CodeBased$futilityStop, x4$futilityStop, tolerance = 1e-05)
	    expect_equal(x4CodeBased$futilityPerStage, x4$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x4CodeBased$earlyStop, x4$earlyStop, tolerance = 1e-05)
	    expect_equal(x4CodeBased$expectedNumberOfSubjects, x4$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x4CodeBased$sampleSizes, x4$sampleSizes, tolerance = 1e-05)
	    expect_equal(x4CodeBased$conditionalPowerAchieved, x4$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x4), "character")
	    df <- as.data.frame(x4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x5 <- getSimulationRates(
	    design = getDesignInverseNormal(
	        futilityBounds = c(0.5, 0.5),
	        informationRates = informationRates
	    ), groups = 2, riskRatio = FALSE, thetaH0 = 0.1,
	    plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations,
	    allocationRatioPlanned = 3, directionUpper = FALSE, seed = seed
	)

	## Comparison of the results of SimulationResultsRates object 'x5' with expected results
	expect_equal(x5$effect, c(-0.1, 2.7755576e-17, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x5$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(x5$iterations[2, ], c(50, 41, 12, 2))
	expect_equal(x5$iterations[3, ], c(34, 29, 3, 0))
	expect_equal(x5$overallReject, c(0.22, 0.03, 0, 0), tolerance = 1e-07)
	expect_equal(x5$rejectPerStage[1, ], c(0.01, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x5$rejectPerStage[2, ], c(0.09, 0.02, 0, 0), tolerance = 1e-07)
	expect_equal(x5$rejectPerStage[3, ], c(0.12, 0.01, 0, 0), tolerance = 1e-07)
	expect_equal(x5$futilityStop, c(0.56, 0.69, 0.97, 1), tolerance = 1e-07)
	expect_equal(x5$futilityPerStage[1, ], c(0.49, 0.59, 0.88, 0.98), tolerance = 1e-07)
	expect_equal(x5$futilityPerStage[2, ], c(0.07, 0.1, 0.09, 0.02), tolerance = 1e-07)
	expect_equal(x5$earlyStop, c(0.66, 0.71, 0.97, 1), tolerance = 1e-07)
	expect_equal(x5$expectedNumberOfSubjects, c(55.2, 51, 34.5, 30.6), tolerance = 1e-07)
	expect_equal(x5$sampleSizes[1, ], c(30, 30, 30, 30))
	expect_equal(x5$sampleSizes[2, ], c(30, 30, 30, 30))
	expect_equal(x5$sampleSizes[3, ], c(30, 30, 30, 0))
	expect_equal(x5$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$conditionalPowerAchieved[2, ], c(0.36523014, 0.20927326, 0.16995311, 0.25129054), tolerance = 1e-07)
	expect_equal(x5$conditionalPowerAchieved[3, ], c(0.43064609, 0.32068397, 0.041565592, NaN), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x5), NA)))
	    expect_output(print(x5)$show())
	    invisible(capture.output(expect_error(summary(x5), NA)))
	    expect_output(summary(x5)$show())
	    x5CodeBased <- eval(parse(text = getObjectRCode(x5, stringWrapParagraphWidth = NULL)))
	    expect_equal(x5CodeBased$effect, x5$effect, tolerance = 1e-05)
	    expect_equal(x5CodeBased$iterations, x5$iterations, tolerance = 1e-05)
	    expect_equal(x5CodeBased$overallReject, x5$overallReject, tolerance = 1e-05)
	    expect_equal(x5CodeBased$rejectPerStage, x5$rejectPerStage, tolerance = 1e-05)
	    expect_equal(x5CodeBased$futilityStop, x5$futilityStop, tolerance = 1e-05)
	    expect_equal(x5CodeBased$futilityPerStage, x5$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x5CodeBased$earlyStop, x5$earlyStop, tolerance = 1e-05)
	    expect_equal(x5CodeBased$expectedNumberOfSubjects, x5$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x5CodeBased$sampleSizes, x5$sampleSizes, tolerance = 1e-05)
	    expect_equal(x5CodeBased$conditionalPowerAchieved, x5$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x5), "character")
	    df <- as.data.frame(x5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x5)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x6 <- getSimulationRates(
	    design = getDesignInverseNormal(
	        futilityBounds = c(0.5, 0.5),
	        informationRates = informationRates
	    ), groups = 1, thetaH0 = 0.4, pi1 = seq(0.2, 0.4, 0.05),
	    plannedSubjects = plannedSubjects, directionUpper = FALSE,
	    maxNumberOfIterations = maxNumberOfIterations, seed = seed
	)

	## Comparison of the results of SimulationResultsRates object 'x6' with expected results
	expect_equal(x6$effect, c(-0.2, -0.15, -0.1, -0.05, 0), tolerance = 1e-07)
	expect_equal(x6$iterations[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x6$iterations[2, ], c(91, 89, 66, 56, 39))
	expect_equal(x6$iterations[3, ], c(19, 49, 51, 48, 24))
	expect_equal(x6$overallReject, c(0.92, 0.78, 0.4, 0.15, 0.03), tolerance = 1e-07)
	expect_equal(x6$rejectPerStage[1, ], c(0.03, 0.01, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x6$rejectPerStage[2, ], c(0.72, 0.4, 0.14, 0.01, 0.01), tolerance = 1e-07)
	expect_equal(x6$rejectPerStage[3, ], c(0.17, 0.37, 0.26, 0.14, 0.02), tolerance = 1e-07)
	expect_equal(x6$futilityStop, c(0.06, 0.1, 0.35, 0.51, 0.75), tolerance = 1e-07)
	expect_equal(x6$futilityPerStage[1, ], c(0.06, 0.1, 0.34, 0.44, 0.61), tolerance = 1e-07)
	expect_equal(x6$futilityPerStage[2, ], c(0, 0, 0.01, 0.07, 0.14), tolerance = 1e-07)
	expect_equal(x6$earlyStop, c(0.81, 0.51, 0.49, 0.52, 0.76), tolerance = 1e-07)
	expect_equal(x6$expectedNumberOfSubjects, c(63, 71.4, 65.1, 61.2, 48.9), tolerance = 1e-07)
	expect_equal(x6$sampleSizes[1, ], c(30, 30, 30, 30, 30))
	expect_equal(x6$sampleSizes[2, ], c(30, 30, 30, 30, 30))
	expect_equal(x6$sampleSizes[3, ], c(30, 30, 30, 30, 30))
	expect_equal(x6$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$conditionalPowerAchieved[2, ], c(0.72335875, 0.55247274, 0.3843863, 0.29482523, 0.18598438), tolerance = 1e-07)
	expect_equal(x6$conditionalPowerAchieved[3, ], c(0.71459365, 0.68392316, 0.54740245, 0.39208559, 0.15519282), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x6), NA)))
	    expect_output(print(x6)$show())
	    invisible(capture.output(expect_error(summary(x6), NA)))
	    expect_output(summary(x6)$show())
	    x6CodeBased <- eval(parse(text = getObjectRCode(x6, stringWrapParagraphWidth = NULL)))
	    expect_equal(x6CodeBased$effect, x6$effect, tolerance = 1e-05)
	    expect_equal(x6CodeBased$iterations, x6$iterations, tolerance = 1e-05)
	    expect_equal(x6CodeBased$overallReject, x6$overallReject, tolerance = 1e-05)
	    expect_equal(x6CodeBased$rejectPerStage, x6$rejectPerStage, tolerance = 1e-05)
	    expect_equal(x6CodeBased$futilityStop, x6$futilityStop, tolerance = 1e-05)
	    expect_equal(x6CodeBased$futilityPerStage, x6$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x6CodeBased$earlyStop, x6$earlyStop, tolerance = 1e-05)
	    expect_equal(x6CodeBased$expectedNumberOfSubjects, x6$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x6CodeBased$sampleSizes, x6$sampleSizes, tolerance = 1e-05)
	    expect_equal(x6CodeBased$conditionalPowerAchieved, x6$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x6), "character")
	    df <- as.data.frame(x6)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x6)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x7 <- getSimulationRates(
	    design = getDesignInverseNormal(futilityBounds = c(0.5), typeOfDesign = "P"),
	    thetaH0 = 0.3, groups = 1, plannedSubjects = c(30, 60),
	    pi1 = seq(0.3, 0.5, 0.05), maxNumberOfIterations = maxNumberOfIterations,
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(30, 30),
	    maxNumberOfSubjectsPerStage = 5 * c(NA, 30),
	    directionUpper = TRUE, seed = seed
	)

	## Comparison of the results of SimulationResultsRates object 'x7' with expected results
	expect_equal(x7$effect, c(0, 0.05, 0.1, 0.15, 0.2), tolerance = 1e-07)
	expect_equal(x7$iterations[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x7$iterations[2, ], c(25, 41, 53, 50, 35))
	expect_equal(x7$overallReject, c(0.05, 0.18, 0.47, 0.77, 0.91), tolerance = 1e-07)
	expect_equal(x7$rejectPerStage[1, ], c(0.02, 0.06, 0.15, 0.36, 0.59), tolerance = 1e-07)
	expect_equal(x7$rejectPerStage[2, ], c(0.03, 0.12, 0.32, 0.41, 0.32), tolerance = 1e-07)
	expect_equal(x7$futilityPerStage[1, ], c(0.73, 0.53, 0.32, 0.14, 0.06), tolerance = 1e-07)
	expect_equal(x7$earlyStop, c(0.75, 0.59, 0.47, 0.5, 0.65), tolerance = 1e-07)
	expect_equal(x7$expectedNumberOfSubjects, c(58.56, 77.43, 83.21, 80.96, 58.83), tolerance = 1e-07)
	expect_equal(x7$sampleSizes[1, ], c(30, 30, 30, 30, 30))
	expect_equal(x7$sampleSizes[2, ], c(114.24, 115.68293, 100.39623, 101.92, 82.371429), tolerance = 1e-07)
	expect_equal(x7$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$conditionalPowerAchieved[2, ], c(0.60107965, 0.60407724, 0.68409402, 0.68536207, 0.68807468), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x7), NA)))
	    expect_output(print(x7)$show())
	    invisible(capture.output(expect_error(summary(x7), NA)))
	    expect_output(summary(x7)$show())
	    x7CodeBased <- eval(parse(text = getObjectRCode(x7, stringWrapParagraphWidth = NULL)))
	    expect_equal(x7CodeBased$effect, x7$effect, tolerance = 1e-05)
	    expect_equal(x7CodeBased$iterations, x7$iterations, tolerance = 1e-05)
	    expect_equal(x7CodeBased$overallReject, x7$overallReject, tolerance = 1e-05)
	    expect_equal(x7CodeBased$rejectPerStage, x7$rejectPerStage, tolerance = 1e-05)
	    expect_equal(x7CodeBased$futilityPerStage, x7$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x7CodeBased$earlyStop, x7$earlyStop, tolerance = 1e-05)
	    expect_equal(x7CodeBased$expectedNumberOfSubjects, x7$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x7CodeBased$sampleSizes, x7$sampleSizes, tolerance = 1e-05)
	    expect_equal(x7CodeBased$conditionalPowerAchieved, x7$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x7), "character")
	    df <- as.data.frame(x7)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x7)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x8 <- getSimulationRates(
	    design = getDesignGroupSequential(
	        futilityBounds = c(0.5, 0.5), typeOfDesign = "P"
	    ),
	    thetaH0 = 0.3, groups = 2, allocationRatioPlanned = 3, plannedSubjects = (1:3) * 100,
	    pi1 = seq(0.2, 0.4, 0.05), pi2 = 0.2, maxNumberOfIterations = maxNumberOfIterations,
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(100, 100, 100),
	    maxNumberOfSubjectsPerStage = 5 * c(NA, 100, 100),
	    directionUpper = FALSE, seed = seed
	)

	## Comparison of the results of SimulationResultsRates object 'x8' with expected results
	expect_equal(x8$effect, c(-0.3, -0.25, -0.2, -0.15, -0.1), tolerance = 1e-07)
	expect_equal(x8$iterations[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x8$iterations[2, ], c(7, 23, 41, 52, 59))
	expect_equal(x8$iterations[3, ], c(0, 1, 1, 11, 20))
	expect_equal(x8$overallReject, c(1, 0.98, 0.95, 0.81, 0.61), tolerance = 1e-07)
	expect_equal(x8$rejectPerStage[1, ], c(0.93, 0.75, 0.54, 0.29, 0.1), tolerance = 1e-07)
	expect_equal(x8$rejectPerStage[2, ], c(0.07, 0.22, 0.4, 0.41, 0.37), tolerance = 1e-07)
	expect_equal(x8$rejectPerStage[3, ], c(0, 0.01, 0.01, 0.11, 0.14), tolerance = 1e-07)
	expect_equal(x8$futilityStop, c(0, 0.02, 0.05, 0.19, 0.33), tolerance = 1e-07)
	expect_equal(x8$futilityPerStage[1, ], c(0, 0.02, 0.05, 0.19, 0.31), tolerance = 1e-07)
	expect_equal(x8$futilityPerStage[2, ], c(0, 0, 0, 0, 0.02), tolerance = 1e-07)
	expect_equal(x8$earlyStop, c(1, 0.99, 0.99, 0.89, 0.8), tolerance = 1e-07)
	expect_equal(x8$expectedNumberOfSubjects, c(115.79, 135.33, 201.46, 331.88, 420.15), tolerance = 1e-07)
	expect_equal(x8$sampleSizes[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x8$sampleSizes[2, ], c(225.57143, 148.73913, 239.7561, 361.73077, 405.05085), tolerance = 1e-07)
	expect_equal(x8$sampleSizes[3, ], c(0, 112, 316, 398, 405.85), tolerance = 1e-07)
	expect_equal(x8$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$conditionalPowerAchieved[2, ], c(0.79294349, 0.80728899, 0.77763316, 0.64160567, 0.53147513), tolerance = 1e-07)
	expect_equal(x8$conditionalPowerAchieved[3, ], c(NaN, 0.80069037, 0.80071364, 0.56677072, 0.57523679), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x8), NA)))
	    expect_output(print(x8)$show())
	    invisible(capture.output(expect_error(summary(x8), NA)))
	    expect_output(summary(x8)$show())
	    x8CodeBased <- eval(parse(text = getObjectRCode(x8, stringWrapParagraphWidth = NULL)))
	    expect_equal(x8CodeBased$effect, x8$effect, tolerance = 1e-05)
	    expect_equal(x8CodeBased$iterations, x8$iterations, tolerance = 1e-05)
	    expect_equal(x8CodeBased$overallReject, x8$overallReject, tolerance = 1e-05)
	    expect_equal(x8CodeBased$rejectPerStage, x8$rejectPerStage, tolerance = 1e-05)
	    expect_equal(x8CodeBased$futilityStop, x8$futilityStop, tolerance = 1e-05)
	    expect_equal(x8CodeBased$futilityPerStage, x8$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x8CodeBased$earlyStop, x8$earlyStop, tolerance = 1e-05)
	    expect_equal(x8CodeBased$expectedNumberOfSubjects, x8$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x8CodeBased$sampleSizes, x8$sampleSizes, tolerance = 1e-05)
	    expect_equal(x8CodeBased$conditionalPowerAchieved, x8$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x8), "character")
	    df <- as.data.frame(x8)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x8)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x9 <- getSimulationRates(
	    design = getDesignGroupSequential(
	        futilityBounds = c(0), typeOfDesign = "P"
	    ),
	    thetaH0 = 0.8, groups = 2, riskRatio = TRUE, allocationRatioPlanned = 3,
	    maxNumberOfIterations = maxNumberOfIterations,
	    plannedSubjects = c(100, 200), pi1 = seq(0.15, 0.4, 0.05), pi2 = 0.2,
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(100, 100),
	    maxNumberOfSubjectsPerStage = 5 * c(NA, 100), directionUpper = TRUE, seed = seed
	)

	## Comparison of the results of SimulationResultsRates object 'x9' with expected results
	expect_equal(x9$effect, c(-0.05, 0.2, 0.45, 0.7, 0.95, 1.2), tolerance = 1e-07)
	expect_equal(x9$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x9$iterations[2, ], c(48, 66, 75, 74, 57, 35))
	expect_equal(x9$overallReject, c(0.01, 0.07, 0.45, 0.86, 0.92, 1), tolerance = 1e-07)
	expect_equal(x9$rejectPerStage[1, ], c(0.01, 0.02, 0.11, 0.24, 0.41, 0.65), tolerance = 1e-07)
	expect_equal(x9$rejectPerStage[2, ], c(0, 0.05, 0.34, 0.62, 0.51, 0.35), tolerance = 1e-07)
	expect_equal(x9$futilityPerStage[1, ], c(0.51, 0.32, 0.14, 0.02, 0.02, 0), tolerance = 1e-07)
	expect_equal(x9$earlyStop, c(0.52, 0.34, 0.25, 0.26, 0.43, 0.65), tolerance = 1e-07)
	expect_equal(x9$expectedNumberOfSubjects, c(323.82, 368.88, 387.13, 364.4, 246.27, 193.96), tolerance = 1e-07)
	expect_equal(x9$sampleSizes[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x9$sampleSizes[2, ], c(466.29167, 407.39394, 382.84, 357.2973, 256.61404, 268.45714), tolerance = 1e-07)
	expect_equal(x9$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x9$conditionalPowerAchieved[2, ], c(0.32248415, 0.49314797, 0.522945, 0.55888112, 0.72047998, 0.75410423), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x9), NA)))
	    expect_output(print(x9)$show())
	    invisible(capture.output(expect_error(summary(x9), NA)))
	    expect_output(summary(x9)$show())
	    x9CodeBased <- eval(parse(text = getObjectRCode(x9, stringWrapParagraphWidth = NULL)))
	    expect_equal(x9CodeBased$effect, x9$effect, tolerance = 1e-05)
	    expect_equal(x9CodeBased$iterations, x9$iterations, tolerance = 1e-05)
	    expect_equal(x9CodeBased$overallReject, x9$overallReject, tolerance = 1e-05)
	    expect_equal(x9CodeBased$rejectPerStage, x9$rejectPerStage, tolerance = 1e-05)
	    expect_equal(x9CodeBased$futilityPerStage, x9$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x9CodeBased$earlyStop, x9$earlyStop, tolerance = 1e-05)
	    expect_equal(x9CodeBased$expectedNumberOfSubjects, x9$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x9CodeBased$sampleSizes, x9$sampleSizes, tolerance = 1e-05)
	    expect_equal(x9CodeBased$conditionalPowerAchieved, x9$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x9), "character")
	    df <- as.data.frame(x9)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x9)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	calcSubjectsFunctionSimulationBaseRates <- function(..., stage,
	        plannedSubjects,
	        minNumberOfSubjectsPerStage,
	        maxNumberOfSubjectsPerStage,
	        conditionalPower,
	        conditionalCriticalValue,
	        overallRate) {
	    if (overallRate[1] - overallRate[2] < 0.1) {
	        return(plannedSubjects[stage] - plannedSubjects[stage - 1])
	    } else {
	        rateUnderH0 <- (overallRate[1] + overallRate[2]) / 2
	        stageSubjects <- 2 * (max(0, conditionalCriticalValue *
	            sqrt(2 * rateUnderH0 * (1 - rateUnderH0)) +
	            stats::qnorm(conditionalPower) * sqrt(overallRate[1] * (1 - overallRate[1]) +
	                overallRate[2] * (1 - overallRate[2]))))^2 /
	            (max(1e-12, (overallRate[1] - overallRate[2])))^2
	        stageSubjects <- ceiling(min(max(
	            minNumberOfSubjectsPerStage[stage],
	            stageSubjects
	        ), maxNumberOfSubjectsPerStage[stage]))
	        return(stageSubjects)
	    }
	}
	x10 <- getSimulationRates(
	    design = getDesignInverseNormal(kMax = 2),
	    pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, plannedSubjects = c(40, 80),
	    minNumberOfSubjectsPerStage = c(40, 20),
	    maxNumberOfSubjectsPerStage = c(40, 160),
	    conditionalPower = 0.8, calcSubjectsFunction = calcSubjectsFunctionSimulationBaseRates,
	    maxNumberOfIterations = maxNumberOfIterations, seed = seed
	)

	## Comparison of the results of SimulationResultsRates object 'x10' with expected results
	expect_equal(x10$effect, c(0, 0.1, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x10$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(x10$iterations[2, ], c(100, 99, 95, 75))
	expect_equal(x10$overallReject, c(0.02, 0.2, 0.52, 0.89), tolerance = 1e-07)
	expect_equal(x10$rejectPerStage[1, ], c(0, 0.01, 0.05, 0.25), tolerance = 1e-07)
	expect_equal(x10$rejectPerStage[2, ], c(0.02, 0.19, 0.47, 0.64), tolerance = 1e-07)
	expect_equal(x10$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x10$earlyStop, c(0, 0.01, 0.05, 0.25), tolerance = 1e-07)
	expect_equal(x10$expectedNumberOfSubjects, c(104.34, 113.7, 101.87, 83.69), tolerance = 1e-07)
	expect_equal(x10$sampleSizes[1, ], c(40, 40, 40, 40))
	expect_equal(x10$sampleSizes[2, ], c(64.34, 74.444444, 65.126316, 58.253333), tolerance = 1e-07)
	expect_equal(x10$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x10$conditionalPowerAchieved[2, ], c(0.20349537, 0.39194633, 0.57556995, 0.71162895), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x10), NA)))
	    expect_output(print(x10)$show())
	    invisible(capture.output(expect_error(summary(x10), NA)))
	    expect_output(summary(x10)$show())
	    x10CodeBased <- eval(parse(text = getObjectRCode(x10, stringWrapParagraphWidth = NULL)))
	    expect_equal(x10CodeBased$effect, x10$effect, tolerance = 1e-05)
	    expect_equal(x10CodeBased$iterations, x10$iterations, tolerance = 1e-05)
	    expect_equal(x10CodeBased$overallReject, x10$overallReject, tolerance = 1e-05)
	    expect_equal(x10CodeBased$rejectPerStage, x10$rejectPerStage, tolerance = 1e-05)
	    expect_equal(x10CodeBased$futilityPerStage, x10$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x10CodeBased$earlyStop, x10$earlyStop, tolerance = 1e-05)
	    expect_equal(x10CodeBased$expectedNumberOfSubjects, x10$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x10CodeBased$sampleSizes, x10$sampleSizes, tolerance = 1e-05)
	    expect_equal(x10CodeBased$conditionalPowerAchieved, x10$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x10), "character")
	    df <- as.data.frame(x10)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x10)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getSimulationRates': comparison with getPowerRates() results for a inverse normal design", {

	.skipTestIfNotX64()
	.skipTestIfDisabled()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
	# @refFS[Tab.]{fs:tab:output:getSimulationRates}
	# @refFS[Formula]{fs:simulationOneArmRatesGenerate}
	# @refFS[Formula]{fs:pValuesOneRateAlternativeGreater}
	# @refFS[Formula]{fs:pValuesOneRateAlternativeSmaller}
	# @refFS[Formula]{fs:simulationTwoArmRatesGenerate}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeGreater}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeSmaller}
	# @refFS[Formula]{fs:pValuesTwoRatesApproximationAlternativeGreater}
	# @refFS[Formula]{fs:pValuesTwoRatesApproximationAlternativeSmaller}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	design <- getDesignInverseNormal(futilityBounds = c(-1), informationRates = c(0.5, 1), typeOfDesign = "P")
	x <- getSimulationRates(design,
	    thetaH0 = 0.4, groups = 1, plannedSubjects = c(150, 300), pi1 = seq(0.3, 0.4, 0.02),
	    maxNumberOfIterations = 1000,
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(NA_real_, 100),
	    maxNumberOfSubjectsPerStage = c(NA_real_, 500), directionUpper = FALSE, seed = 123
	)
	y <- getPowerRates(design,
	    thetaH0 = 0.4, groups = 1, pi1 = seq(0.3, 0.4, 0.02),
	    directionUpper = FALSE, maxNumberOfSubjects = 300
	)

	expectedNumberOfSubjectsDiff <- round((x$expectedNumberOfSubjects - y$expectedNumberOfSubjects) / 300, 4)

	## Comparison of the results of numeric object 'expectedNumberOfSubjectsDiff' with expected results
	expect_equal(expectedNumberOfSubjectsDiff, c(0.2203, 0.4265, 0.625, 0.8158, 0.9639, 0.9543), tolerance = 1e-07)

	overallRejectDiff <- round(x$overallReject - y$overallReject, 4)

	## Comparison of the results of numeric object 'overallRejectDiff' with expected results
	expect_equal(overallRejectDiff, c(0.052, 0.1567, 0.2226, 0.1407, 0.0249, -0.008), tolerance = 1e-07)

	rejectPerStageDiff <- round(x$rejectPerStage - y$rejectPerStage, 4)

	## Comparison of the results of matrixarray object 'rejectPerStageDiff' with expected results
	expect_equal(rejectPerStageDiff[1, ], c(-0.0439, -0.0644, -0.027, -0.0138, 0.0042, -0.0067), tolerance = 1e-07)
	expect_equal(rejectPerStageDiff[2, ], c(0.0959, 0.2211, 0.2497, 0.1545, 0.0207, -0.0013), tolerance = 1e-07)

	futilityPerStageDiff <- round(x$futilityPerStage - y$futilityPerStage, 4)

	## Comparison of the results of matrixarray object 'futilityPerStageDiff' with expected results
	expect_equal(futilityPerStageDiff[1, ], c(-2e-04, 0.0018, -0.0011, -0.0092, -0.0279, -0.0147), tolerance = 1e-07)

})

test_that("'getSimulationRates': comparison with getPowerRates() results for a group sequential design", {

	.skipTestIfNotX64()
	.skipTestIfDisabled()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
	# @refFS[Tab.]{fs:tab:output:getSimulationRates}
	# @refFS[Formula]{fs:simulationTwoArmRatesGenerate}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeGreater}
	# @refFS[Formula]{fs:pValuesTwoRatesAlternativeSmaller}
	# @refFS[Formula]{fs:pValuesTwoRatesApproximationAlternativeGreater}
	# @refFS[Formula]{fs:pValuesTwoRatesApproximationAlternativeSmaller}
	# @refFS[Formula]{fs:testStatisticGroupSequential}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	design <- getDesignGroupSequential(futilityBounds = c(-1, 1), typeOfDesign = "P")
	x <- getSimulationRates(design,
	    thetaH0 = 0.3, groups = 2, allocationRatioPlanned = 2, plannedSubjects = (1:3) * 100,
	    pi1 = seq(0.2, 0.4, 0.05), pi2 = 0.1,
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(100, 100, 100),
	    maxNumberOfSubjectsPerStage = 1 * c(100, 100, 100), directionUpper = FALSE, seed = 123
	)

	y <- getPowerRates(design,
	    thetaH0 = 0.3, groups = 2, allocationRatioPlanned = 2, pi1 = seq(0.2, 0.4, 0.05),
	    pi2 = 0.1, directionUpper = FALSE, maxNumberOfSubjects = 300
	)

	expectedNumberOfSubjectsDiff <- round((x$expectedNumberOfSubjects - y$expectedNumberOfSubjects) / 300, 4)

	## Comparison of the results of numeric object 'expectedNumberOfSubjectsDiff' with expected results
	expect_equal(expectedNumberOfSubjectsDiff, c(-0.0076, -0.0264, -0.0251, -0.0066, -0.0023), tolerance = 1e-07)

	overallRejectDiff <- round(x$overallReject - y$overallReject, 4)

	## Comparison of the results of numeric object 'overallRejectDiff' with expected results
	expect_equal(overallRejectDiff, c(9e-04, 0.0072, 0.0177, -9e-04, -6e-04), tolerance = 1e-07)

	rejectPerStageDiff <- round(x$rejectPerStage - y$rejectPerStage, 4)

	## Comparison of the results of matrixarray object 'rejectPerStageDiff' with expected results
	expect_equal(rejectPerStageDiff[1, ], c(0.0121, 0.0444, 0.0355, 0.0081, 0.001), tolerance = 1e-07)
	expect_equal(rejectPerStageDiff[2, ], c(-0.0032, -0.0171, 0.009, -0.0062, -0.0019), tolerance = 1e-07)
	expect_equal(rejectPerStageDiff[3, ], c(-0.008, -0.02, -0.0268, -0.0028, 3e-04), tolerance = 1e-07)

	futilityPerStageDiff <- round(x$futilityPerStage - y$futilityPerStage, 4)

	## Comparison of the results of matrixarray object 'futilityPerStageDiff' with expected results
	expect_equal(futilityPerStageDiff[1, ], c(-1e-04, 0, 0.0049, 0.0058, 0.0053), tolerance = 1e-07)
	expect_equal(futilityPerStageDiff[2, ], c(0.0018, 0.0077, -0.0146, -0.0016, -0.0038), tolerance = 1e-07)

	## --

	x2 <- getSimulationRates(
	    design = getDesignGroupSequential(futilityBounds = c(-1, 1), typeOfDesign = "P"),
	    thetaH0 = 0.8, groups = 2, riskRatio = TRUE, allocationRatioPlanned = 2,
	    plannedSubjects = c(100, 200, 300), pi1 = seq(0.15, 0.4, 0.05), pi2 = 0.2,
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(NA_real_, 150, 300),
	    maxNumberOfSubjectsPerStage = c(NA_real_, 200, 300), directionUpper = TRUE,
	    maxNumberOfIterations = 1000, seed = 123
	)

	y2 <- getPowerRates(
	    design = getDesignGroupSequential(futilityBounds = c(-1, 1), typeOfDesign = "P"),
	    thetaH0 = 0.8, groups = 2, riskRatio = TRUE, allocationRatioPlanned = 2,
	    pi1 = seq(0.15, 0.4, 0.05), pi2 = 0.2, maxNumberOfSubjects = 300,
	    directionUpper = TRUE
	)

	expectedNumberOfSubjectsDiff2 <- round((x2$expectedNumberOfSubjects - y2$expectedNumberOfSubjects) / 300, 4)

	## Comparison of the results of numeric object 'expectedNumberOfSubjectsDiff2' with expected results
	expect_equal(expectedNumberOfSubjectsDiff2, c(0.336, 0.5853, 0.5882, 0.3089, 0.1411, 0.079), tolerance = 1e-07)

	overallRejectDiff2 <- round(x2$overallReject - y2$overallReject, 4)

	## Comparison of the results of numeric object 'overallRejectDiff2' with expected results
	expect_equal(overallRejectDiff2, c(0.0032, 0.0559, 0.2444, 0.1617, 0.0401, 0.0038), tolerance = 1e-07)

	rejectPerStageDiff2 <- round(x2$rejectPerStage - y2$rejectPerStage, 4)

	## Comparison of the results of matrixarray object 'rejectPerStageDiff2' with expected results
	expect_equal(rejectPerStageDiff2[1, ], c(6e-04, -0.0126, -0.0203, -0.0149, -0.0029, -0.0228), tolerance = 1e-07)
	expect_equal(rejectPerStageDiff2[2, ], c(0.0025, 0.0084, 0.104, 0.1808, 0.1029, 0.0508), tolerance = 1e-07)
	expect_equal(rejectPerStageDiff2[3, ], c(1e-04, 0.0601, 0.1607, -0.0041, -0.06, -0.0242), tolerance = 1e-07)

	futilityPerStageDiff2 <- round(x2$futilityPerStage - y2$futilityPerStage, 4)

	## Comparison of the results of matrixarray object 'futilityPerStageDiff2' with expected results
	expect_equal(futilityPerStageDiff2[1, ], c(-0.0028, -0.016, -0.0034, -3e-04, -5e-04, -1e-04), tolerance = 1e-07)
	expect_equal(futilityPerStageDiff2[2, ], c(-0.0068, -0.0474, -0.0917, -0.0386, -0.0101, -0.0011), tolerance = 1e-07)
})

