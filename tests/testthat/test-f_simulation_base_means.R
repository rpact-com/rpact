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
## |  File name: test-f_simulation_base_means.R
## |  Creation date: 06 February 2023, 12:13:46
## |  File version: $Revision: 7067 $
## |  Last changed: $Date: 2023-06-09 12:58:32 +0200 (Fr, 09 Jun 2023) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing Simulation Means Function")


test_that("'getSimulationMeans': several configurations", {
	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
	# @refFS[Tab.]{fs:tab:output:getSimulationMeans}
	# @refFS[Formula]{fs:simulationOneArmMeansTestStatistics}
	# @refFS[Formula]{fs:simulationTwoArmMeansTestStatisticsDiff}
	# @refFS[Formula]{fs:simulationTwoArmMeansTestStatisticsRatio}
	# @refFS[Formula]{fs:testStatisticGroupSequentialWeightedAverage}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	maxNumberOfIterations <- 100
	seed <- 99123
	options(width = 180)
	maxNumberOfSubjects <- 90
	informationRates <- c(0.2, 0.5, 1)
	plannedSubjects <- round(informationRates * maxNumberOfSubjects)

	x1 <- getSimulationMeans(
	    design = getDesignInverseNormal(
	        futilityBounds = c(-0.5, 0.5),
	        informationRates = c(0.2, 0.5, 1)
	    ), groups = 2, meanRatio = TRUE, thetaH0 = 0.4,
	    plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations,
	    allocationRatioPlanned = 3, stDev = 1.5, seed = seed
	)

	## Comparison of the results of SimulationResultsMeans object 'x1' with expected results
	expect_equal(x1$effect, c(0.6, 0.8, 1, 1.2, 1.4, 1.6), tolerance = 1e-07)
	expect_equal(x1$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x1$iterations[2, ], c(96, 100, 100, 94, 97, 95))
	expect_equal(x1$iterations[3, ], c(72, 68, 37, 16, 2, 2))
	expect_equal(x1$overallReject, c(0.81, 0.93, 0.99, 0.99, 1, 1), tolerance = 1e-07)
	expect_equal(x1$rejectPerStage[1, ], c(0, 0, 0, 0.05, 0.03, 0.05), tolerance = 1e-07)
	expect_equal(x1$rejectPerStage[2, ], c(0.2, 0.29, 0.62, 0.78, 0.95, 0.93), tolerance = 1e-07)
	expect_equal(x1$rejectPerStage[3, ], c(0.61, 0.64, 0.37, 0.16, 0.02, 0.02), tolerance = 1e-07)
	expect_equal(x1$futilityStop, c(0.08, 0.03, 0.01, 0.01, 0, 0), tolerance = 1e-07)
	expect_equal(x1$futilityPerStage[1, ], c(0.04, 0, 0, 0.01, 0, 0), tolerance = 1e-07)
	expect_equal(x1$futilityPerStage[2, ], c(0.04, 0.03, 0.01, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x1$earlyStop, c(0.28, 0.32, 0.63, 0.84, 0.98, 0.98), tolerance = 1e-07)
	expect_equal(x1$expectedNumberOfSubjects, c(76.32, 75.6, 61.65, 50.58, 45.09, 44.55), tolerance = 1e-07)
	expect_equal(x1$sampleSizes[1, ], c(18, 18, 18, 18, 18, 18))
	expect_equal(x1$sampleSizes[2, ], c(27, 27, 27, 27, 27, 27))
	expect_equal(x1$sampleSizes[3, ], c(45, 45, 45, 45, 45, 45))
	expect_equal(x1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$conditionalPowerAchieved[2, ], c(0.26405311, 0.35839614, 0.48830732, 0.63603264, 0.77682482, 0.82707873), tolerance = 1e-07)
	expect_equal(x1$conditionalPowerAchieved[3, ], c(0.60511343, 0.74281632, 0.84083206, 0.87094401, 0.89751119, 0.97110806), tolerance = 1e-07)
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

	.skipTestIfDisabled()

	x2 <- getSimulationMeans(
	    design = getDesignInverseNormal(
	        futilityBounds = c(-0.5, 0.5),
	        informationRates = c(0.2, 0.5, 1)
	    ), groups = 2, meanRatio = FALSE, thetaH0 = 0.2,
	    plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations,
	    allocationRatioPlanned = 3, stDev = 1.5, seed = seed
	)

	## Comparison of the results of SimulationResultsMeans object 'x2' with expected results
	expect_equal(x2$effect, c(-0.2, 0, 0.2, 0.4, 0.6, 0.8), tolerance = 1e-07)
	expect_equal(x2$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x2$iterations[2, ], c(63, 73, 84, 83, 89, 96))
	expect_equal(x2$iterations[3, ], c(15, 24, 42, 53, 69, 76))
	expect_equal(x2$overallReject, c(0, 0.02, 0.07, 0.18, 0.33, 0.53), tolerance = 1e-07)
	expect_equal(x2$rejectPerStage[1, ], c(0, 0, 0, 0, 0, 0))
	expect_equal(x2$rejectPerStage[2, ], c(0, 0, 0.02, 0.03, 0.06, 0.1), tolerance = 1e-07)
	expect_equal(x2$rejectPerStage[3, ], c(0, 0.02, 0.05, 0.15, 0.27, 0.43), tolerance = 1e-07)
	expect_equal(x2$futilityStop, c(0.85, 0.76, 0.56, 0.44, 0.25, 0.14), tolerance = 1e-07)
	expect_equal(x2$futilityPerStage[1, ], c(0.37, 0.27, 0.16, 0.17, 0.11, 0.04), tolerance = 1e-07)
	expect_equal(x2$futilityPerStage[2, ], c(0.48, 0.49, 0.4, 0.27, 0.14, 0.1), tolerance = 1e-07)
	expect_equal(x2$earlyStop, c(0.85, 0.76, 0.58, 0.47, 0.31, 0.24), tolerance = 1e-07)
	expect_equal(x2$expectedNumberOfSubjects, c(41.76, 48.51, 59.58, 64.26, 73.08, 78.12), tolerance = 1e-07)
	expect_equal(x2$sampleSizes[1, ], c(18, 18, 18, 18, 18, 18))
	expect_equal(x2$sampleSizes[2, ], c(27, 27, 27, 27, 27, 27))
	expect_equal(x2$sampleSizes[3, ], c(45, 45, 45, 45, 45, 45))
	expect_equal(x2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$conditionalPowerAchieved[2, ], c(0.056595809, 0.082243527, 0.1171868, 0.14183443, 0.20192022, 0.18371302), tolerance = 1e-07)
	expect_equal(x2$conditionalPowerAchieved[3, ], c(0.36165449, 0.31543938, 0.36771185, 0.4758946, 0.54527876, 0.61204049), tolerance = 1e-07)
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

	x3 <- getSimulationMeans(
	    design = getDesignInverseNormal(
	        futilityBounds = c(-0.5, 0.5),
	        informationRates = c(0.2, 0.5, 1)
	    ), groups = 1, thetaH0 = 0.2,
	    plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations,
	    stDev = 1.5, seed = seed
	)

	## Comparison of the results of SimulationResultsMeans object 'x3' with expected results
	expect_equal(x3$effect, c(-0.2, 0, 0.2, 0.4, 0.6, 0.8), tolerance = 1e-07)
	expect_equal(x3$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x3$iterations[2, ], c(50, 71, 87, 96, 97, 99))
	expect_equal(x3$iterations[3, ], c(9, 21, 63, 67, 49, 29))
	expect_equal(x3$overallReject, c(0, 0.02, 0.21, 0.59, 0.94, 1), tolerance = 1e-07)
	expect_equal(x3$rejectPerStage[1, ], c(0, 0, 0, 0, 0, 0.01), tolerance = 1e-07)
	expect_equal(x3$rejectPerStage[2, ], c(0, 0, 0.03, 0.21, 0.47, 0.7), tolerance = 1e-07)
	expect_equal(x3$rejectPerStage[3, ], c(0, 0.02, 0.18, 0.38, 0.47, 0.29), tolerance = 1e-07)
	expect_equal(x3$futilityStop, c(0.91, 0.79, 0.34, 0.12, 0.04, 0), tolerance = 1e-07)
	expect_equal(x3$futilityPerStage[1, ], c(0.5, 0.29, 0.13, 0.04, 0.03, 0), tolerance = 1e-07)
	expect_equal(x3$futilityPerStage[2, ], c(0.41, 0.5, 0.21, 0.08, 0.01, 0), tolerance = 1e-07)
	expect_equal(x3$earlyStop, c(0.91, 0.79, 0.37, 0.33, 0.51, 0.71), tolerance = 1e-07)
	expect_equal(x3$expectedNumberOfSubjects, c(35.55, 46.62, 69.84, 74.07, 66.24, 57.78), tolerance = 1e-07)
	expect_equal(x3$sampleSizes[1, ], c(18, 18, 18, 18, 18, 18))
	expect_equal(x3$sampleSizes[2, ], c(27, 27, 27, 27, 27, 27))
	expect_equal(x3$sampleSizes[3, ], c(45, 45, 45, 45, 45, 45))
	expect_equal(x3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$conditionalPowerAchieved[2, ], c(0.047252355, 0.074094582, 0.18424333, 0.30402818, 0.54078356, 0.67131653), tolerance = 1e-07)
	expect_equal(x3$conditionalPowerAchieved[3, ], c(0.27249296, 0.30454177, 0.45212728, 0.62638376, 0.84307565, 0.91215549), tolerance = 1e-07)
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

	x4 <- getSimulationMeans(
	    design = getDesignInverseNormal(
	        futilityBounds = c(-0.5, 0.5),
	        informationRates = c(0.2, 0.5, 1)
	    ), groups = 2, meanRatio = TRUE, thetaH0 = 1.1,
	    plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations,
	    allocationRatioPlanned = 3, stDev = 1.5, directionUpper = FALSE, seed = seed
	)

	## Comparison of the results of SimulationResultsMeans object 'x4' with expected results
	expect_equal(x4$effect, c(-0.1, 0.1, 0.3, 0.5, 0.7, 0.9), tolerance = 1e-07)
	expect_equal(x4$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x4$iterations[2, ], c(76, 71, 52, 52, 45, 23))
	expect_equal(x4$iterations[3, ], c(31, 27, 10, 12, 3, 3))
	expect_equal(x4$overallReject, c(0.01, 0.02, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x4$rejectPerStage[1, ], c(0, 0, 0, 0, 0, 0))
	expect_equal(x4$rejectPerStage[2, ], c(0, 0, 0, 0, 0, 0))
	expect_equal(x4$rejectPerStage[3, ], c(0.01, 0.02, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x4$futilityStop, c(0.69, 0.73, 0.9, 0.88, 0.97, 0.97), tolerance = 1e-07)
	expect_equal(x4$futilityPerStage[1, ], c(0.24, 0.29, 0.48, 0.48, 0.55, 0.77), tolerance = 1e-07)
	expect_equal(x4$futilityPerStage[2, ], c(0.45, 0.44, 0.42, 0.4, 0.42, 0.2), tolerance = 1e-07)
	expect_equal(x4$earlyStop, c(0.69, 0.73, 0.9, 0.88, 0.97, 0.97), tolerance = 1e-07)
	expect_equal(x4$expectedNumberOfSubjects, c(52.47, 49.32, 36.54, 37.44, 31.5, 25.56), tolerance = 1e-07)
	expect_equal(x4$sampleSizes[1, ], c(18, 18, 18, 18, 18, 18))
	expect_equal(x4$sampleSizes[2, ], c(27, 27, 27, 27, 27, 27))
	expect_equal(x4$sampleSizes[3, ], c(45, 45, 45, 45, 45, 45))
	expect_equal(x4$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$conditionalPowerAchieved[2, ], c(0.088210955, 0.073662665, 0.032364394, 0.040456333, 0.047760081, 0.047799584), tolerance = 1e-07)
	expect_equal(x4$conditionalPowerAchieved[3, ], c(0.34802745, 0.34204022, 0.18915629, 0.18461746, 0.36492317, 0.12863193), tolerance = 1e-07)
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

	x5 <- getSimulationMeans(
	    design = getDesignInverseNormal(
	        futilityBounds = c(-0.5, 0.5),
	        informationRates = c(0.2, 0.5, 1)
	    ), groups = 2, meanRatio = FALSE, thetaH0 = 1.1,
	    plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations,
	    allocationRatioPlanned = 3, stDev = 1.5, directionUpper = FALSE, seed = seed
	)

	## Comparison of the results of SimulationResultsMeans object 'x5' with expected results
	expect_equal(x5$effect, c(-1.1, -0.9, -0.7, -0.5, -0.3, -0.1), tolerance = 1e-07)
	expect_equal(x5$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x5$iterations[2, ], c(98, 96, 88, 84, 82, 79))
	expect_equal(x5$iterations[3, ], c(77, 74, 69, 58, 54, 43))
	expect_equal(x5$overallReject, c(0.78, 0.71, 0.51, 0.27, 0.13, 0.04), tolerance = 1e-07)
	expect_equal(x5$rejectPerStage[1, ], c(0, 0, 0, 0, 0, 0))
	expect_equal(x5$rejectPerStage[2, ], c(0.19, 0.14, 0.08, 0.06, 0, 0), tolerance = 1e-07)
	expect_equal(x5$rejectPerStage[3, ], c(0.59, 0.57, 0.43, 0.21, 0.13, 0.04), tolerance = 1e-07)
	expect_equal(x5$futilityStop, c(0.04, 0.12, 0.23, 0.36, 0.46, 0.57), tolerance = 1e-07)
	expect_equal(x5$futilityPerStage[1, ], c(0.02, 0.04, 0.12, 0.16, 0.18, 0.21), tolerance = 1e-07)
	expect_equal(x5$futilityPerStage[2, ], c(0.02, 0.08, 0.11, 0.2, 0.28, 0.36), tolerance = 1e-07)
	expect_equal(x5$earlyStop, c(0.23, 0.26, 0.31, 0.42, 0.46, 0.57), tolerance = 1e-07)
	expect_equal(x5$expectedNumberOfSubjects, c(79.11, 77.22, 72.81, 66.78, 64.44, 58.68), tolerance = 1e-07)
	expect_equal(x5$sampleSizes[1, ], c(18, 18, 18, 18, 18, 18))
	expect_equal(x5$sampleSizes[2, ], c(27, 27, 27, 27, 27, 27))
	expect_equal(x5$sampleSizes[3, ], c(45, 45, 45, 45, 45, 45))
	expect_equal(x5$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$conditionalPowerAchieved[2, ], c(0.33588936, 0.25194744, 0.19824827, 0.19178721, 0.11444971, 0.092566355), tolerance = 1e-07)
	expect_equal(x5$conditionalPowerAchieved[3, ], c(0.74226501, 0.69902839, 0.55641803, 0.50033698, 0.45636572, 0.33236099), tolerance = 1e-07)
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

	x6 <- getSimulationMeans(
	    design = getDesignInverseNormal(
	        futilityBounds = c(-0.5, 0.5),
	        informationRates = c(0.2, 0.5, 1)
	    ), groups = 1, thetaH0 = 0.8,
	    plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations,
	    stDev = 1.5, directionUpper = FALSE, seed = seed
	)

	## Comparison of the results of SimulationResultsMeans object 'x6' with expected results
	expect_equal(x6$effect, c(-0.8, -0.6, -0.4, -0.2, 0, 0.2), tolerance = 1e-07)
	expect_equal(x6$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x6$iterations[2, ], c(100, 99, 96, 81, 70, 49))
	expect_equal(x6$iterations[3, ], c(22, 43, 75, 57, 27, 7))
	expect_equal(x6$overallReject, c(1, 0.96, 0.66, 0.26, 0.02, 0), tolerance = 1e-07)
	expect_equal(x6$rejectPerStage[1, ], c(0, 0, 0, 0, 0, 0))
	expect_equal(x6$rejectPerStage[2, ], c(0.78, 0.56, 0.13, 0.05, 0, 0), tolerance = 1e-07)
	expect_equal(x6$rejectPerStage[3, ], c(0.22, 0.4, 0.53, 0.21, 0.02, 0), tolerance = 1e-07)
	expect_equal(x6$futilityStop, c(0, 0.01, 0.12, 0.38, 0.73, 0.93), tolerance = 1e-07)
	expect_equal(x6$futilityPerStage[1, ], c(0, 0.01, 0.04, 0.19, 0.3, 0.51), tolerance = 1e-07)
	expect_equal(x6$futilityPerStage[2, ], c(0, 0, 0.08, 0.19, 0.43, 0.42), tolerance = 1e-07)
	expect_equal(x6$earlyStop, c(0.78, 0.57, 0.25, 0.43, 0.73, 0.93), tolerance = 1e-07)
	expect_equal(x6$expectedNumberOfSubjects, c(54.9, 64.08, 77.67, 65.52, 49.05, 34.38), tolerance = 1e-07)
	expect_equal(x6$sampleSizes[1, ], c(18, 18, 18, 18, 18, 18))
	expect_equal(x6$sampleSizes[2, ], c(27, 27, 27, 27, 27, 27))
	expect_equal(x6$sampleSizes[3, ], c(45, 45, 45, 45, 45, 45))
	expect_equal(x6$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$conditionalPowerAchieved[2, ], c(0.67267344, 0.52857476, 0.27194206, 0.18361852, 0.064769395, 0.04670856), tolerance = 1e-07)
	expect_equal(x6$conditionalPowerAchieved[3, ], c(0.81011604, 0.77276452, 0.65795757, 0.50391481, 0.35327029, 0.24591214), tolerance = 1e-07)
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

	x7 <- getSimulationMeans(
	    design = getDesignInverseNormal(
	        futilityBounds = c(-0.5, 0.5),
	        informationRates = c(0.2, 0.5, 1)
	    ), groups = 1, thetaH0 = -0.2,
	    plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations,
	    stDev = 3.5, alternative = seq(-1.2, -0.2, 0.2),
	    conditionalPower = 0.8,
	    minNumberOfSubjectsPerStage = c(NA, 10, 10), maxNumberOfSubjectsPerStage = c(NA, 100, 100),
	    directionUpper = FALSE, seed = seed
	)

	## Comparison of the results of SimulationResultsMeans object 'x7' with expected results
	expect_equal(x7$effect, c(-1, -0.8, -0.6, -0.4, -0.2, 0), tolerance = 1e-07)
	expect_equal(x7$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x7$iterations[2, ], c(93, 97, 88, 78, 78, 74))
	expect_equal(x7$iterations[3, ], c(52, 77, 69, 57, 51, 35))
	expect_equal(x7$overallReject, c(0.81, 0.82, 0.59, 0.32, 0.12, 0.03), tolerance = 1e-07)
	expect_equal(x7$rejectPerStage[1, ], c(0, 0, 0, 0, 0, 0))
	expect_equal(x7$rejectPerStage[2, ], c(0.4, 0.19, 0.12, 0.07, 0, 0), tolerance = 1e-07)
	expect_equal(x7$rejectPerStage[3, ], c(0.41, 0.63, 0.47, 0.25, 0.12, 0.03), tolerance = 1e-07)
	expect_equal(x7$futilityStop, c(0.08, 0.04, 0.19, 0.36, 0.49, 0.65), tolerance = 1e-07)
	expect_equal(x7$futilityPerStage[1, ], c(0.07, 0.03, 0.12, 0.22, 0.22, 0.26), tolerance = 1e-07)
	expect_equal(x7$futilityPerStage[2, ], c(0.01, 0.01, 0.07, 0.14, 0.27, 0.39), tolerance = 1e-07)
	expect_equal(x7$earlyStop, c(0.48, 0.23, 0.31, 0.43, 0.49, 0.65), tolerance = 1e-07)
	expect_equal(x7$expectedNumberOfSubjects, c(105.75972, 141.87769, 144.85789, 134.64079, 139.03875, 121.42333), tolerance = 1e-07)
	expect_equal(x7$sampleSizes[1, ], c(18, 18, 18, 18, 18, 18))
	expect_equal(x7$sampleSizes[2, ], c(74.918717, 83.151367, 90.734126, 88.517379, 94.605927, 95.502536), tolerance = 1e-07)
	expect_equal(x7$sampleSizes[3, ], c(34.779445, 56.130993, 68.133125, 83.503922, 92.63947, 93.575595), tolerance = 1e-07)
	expect_equal(x7$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$conditionalPowerAchieved[2, ], c(0.48960058, 0.35501907, 0.33230293, 0.3239724, 0.20164899, 0.17099815), tolerance = 1e-07)
	expect_equal(x7$conditionalPowerAchieved[3, ], c(0.75975737, 0.70067902, 0.61722401, 0.51061814, 0.40378864, 0.28388391), tolerance = 1e-07)
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
	    expect_equal(x7CodeBased$futilityStop, x7$futilityStop, tolerance = 1e-05)
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

	x8 <- getSimulationMeans(
	    design = getDesignInverseNormal(futilityBounds = c(-0.5, 0.5)),
	    groups = 2, meanRatio = FALSE, thetaH0 = -0.1, plannedSubjects = plannedSubjects,
	    maxNumberOfIterations = maxNumberOfIterations, allocationRatioPlanned = 3, stDev = 3.5,
	    conditionalPower = 0.8,
	    minNumberOfSubjectsPerStage = c(NA, 10, 10), maxNumberOfSubjectsPerStage = c(NA, 100, 100),
	    seed = seed
	)

	## Comparison of the results of SimulationResultsMeans object 'x8' with expected results
	expect_equal(x8$effect, c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1), tolerance = 1e-07)
	expect_equal(x8$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x8$iterations[2, ], c(74, 78, 81, 81, 90, 86))
	expect_equal(x8$iterations[3, ], c(30, 33, 52, 55, 67, 65))
	expect_equal(x8$overallReject, c(0.04, 0.03, 0.09, 0.19, 0.35, 0.32), tolerance = 1e-07)
	expect_equal(x8$rejectPerStage[1, ], c(0, 0, 0, 0.01, 0, 0), tolerance = 1e-07)
	expect_equal(x8$rejectPerStage[2, ], c(0.02, 0.01, 0.02, 0.06, 0.1, 0.07), tolerance = 1e-07)
	expect_equal(x8$rejectPerStage[3, ], c(0.02, 0.02, 0.07, 0.12, 0.25, 0.25), tolerance = 1e-07)
	expect_equal(x8$futilityStop, c(0.68, 0.66, 0.46, 0.38, 0.23, 0.28), tolerance = 1e-07)
	expect_equal(x8$futilityPerStage[1, ], c(0.26, 0.22, 0.19, 0.18, 0.1, 0.14), tolerance = 1e-07)
	expect_equal(x8$futilityPerStage[2, ], c(0.42, 0.44, 0.27, 0.2, 0.13, 0.14), tolerance = 1e-07)
	expect_equal(x8$earlyStop, c(0.7, 0.67, 0.48, 0.45, 0.33, 0.35), tolerance = 1e-07)
	expect_equal(x8$expectedNumberOfSubjects, c(111.53284, 119.9607, 137.10925, 136.56279, 151.62676, 145.91552), tolerance = 1e-07)
	expect_equal(x8$sampleSizes[1, ], c(18, 18, 18, 18, 18, 18))
	expect_equal(x8$sampleSizes[2, ], c(89.604753, 93.952606, 89.473054, 86.745314, 84.630171, 89.414885), tolerance = 1e-07)
	expect_equal(x8$sampleSizes[3, ], c(90.75107, 86.902014, 89.684764, 87.816529, 85.760605, 78.490341), tolerance = 1e-07)
	expect_equal(x8$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$conditionalPowerAchieved[2, ], c(0.22129636, 0.2212372, 0.27604385, 0.2610371, 0.30108411, 0.26964038), tolerance = 1e-07)
	expect_equal(x8$conditionalPowerAchieved[3, ], c(0.30043836, 0.34051211, 0.31802231, 0.36816554, 0.50585406, 0.52804861), tolerance = 1e-07)
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

	x9 <- getSimulationMeans(
	    design = getDesignInverseNormal(futilityBounds = c(-0.5, 0.5)),
	    groups = 2, meanRatio = TRUE, thetaH0 = 1.6, plannedSubjects = plannedSubjects,
	    maxNumberOfIterations = maxNumberOfIterations, allocationRatioPlanned = c(1, 3, 3), stDev = 1.5,
	    alternative = seq(0.8, 1.6, 0.2), conditionalPower = 0.8,
	    minNumberOfSubjectsPerStage = c(NA, 10, 10), maxNumberOfSubjectsPerStage = c(NA, 100, 100),
	    directionUpper = FALSE, seed = seed
	)

	## Comparison of the results of SimulationResultsMeans object 'x9' with expected results
	expect_equal(x9$effect, c(-0.8, -0.6, -0.4, -0.2, 0), tolerance = 1e-07)
	expect_equal(x9$iterations[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x9$iterations[2, ], c(95, 90, 82, 75, 68))
	expect_equal(x9$iterations[3, ], c(73, 68, 53, 48, 26))
	expect_equal(x9$overallReject, c(0.55, 0.37, 0.22, 0.1, 0.01), tolerance = 1e-07)
	expect_equal(x9$rejectPerStage[1, ], c(0, 0, 0, 0, 0))
	expect_equal(x9$rejectPerStage[2, ], c(0.13, 0.08, 0.06, 0.04, 0), tolerance = 1e-07)
	expect_equal(x9$rejectPerStage[3, ], c(0.42, 0.29, 0.16, 0.06, 0.01), tolerance = 1e-07)
	expect_equal(x9$futilityStop, c(0.14, 0.24, 0.41, 0.48, 0.74), tolerance = 1e-07)
	expect_equal(x9$futilityPerStage[1, ], c(0.05, 0.1, 0.18, 0.25, 0.32), tolerance = 1e-07)
	expect_equal(x9$futilityPerStage[2, ], c(0.09, 0.14, 0.23, 0.23, 0.42), tolerance = 1e-07)
	expect_equal(x9$earlyStop, c(0.27, 0.32, 0.47, 0.52, 0.74), tolerance = 1e-07)
	expect_equal(x9$expectedNumberOfSubjects, c(159.13638, 155.22411, 142.49895, 133.05841, 108.89569), tolerance = 1e-07)
	expect_equal(x9$sampleSizes[1, ], c(18, 18, 18, 18, 18))
	expect_equal(x9$sampleSizes[2, ], c(85.987506, 91.370107, 92.601585, 94.55466, 96.567372), tolerance = 1e-07)
	expect_equal(x9$sampleSizes[3, ], c(81.435959, 80.869134, 91.633298, 91.963359, 97.037972), tolerance = 1e-07)
	expect_equal(x9$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x9$conditionalPowerAchieved[2, ], c(0.43130186, 0.31089581, 0.32119313, 0.2350347, 0.16563188), tolerance = 1e-07)
	expect_equal(x9$conditionalPowerAchieved[3, ], c(0.64594535, 0.57199764, 0.39418023, 0.33812857, 0.31423783), tolerance = 1e-07)
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
	    expect_equal(x9CodeBased$futilityStop, x9$futilityStop, tolerance = 1e-05)
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

	calcSubjectsFunctionSimulationBaseMeans <- function(..., stage, thetaH0, allocationRatioPlanned,
	        minNumberOfSubjectsPerStage, maxNumberOfSubjectsPerStage,
	        sampleSizesPerStage, thetaH1, conditionalPower, conditionalCriticalValue) {
	    mult <- 1
	    if (stage == 2) {
	        stageSubjects <- (1 + 1 / allocationRatioPlanned + thetaH0^2 * (1 + allocationRatioPlanned)) *
	            (max(0, conditionalCriticalValue + stats::qnorm(conditionalPower)))^2 * mult /
	            (max(1e-12, thetaH1))^2
	        stageSubjects <- min(
	            max(minNumberOfSubjectsPerStage[stage], stageSubjects),
	            maxNumberOfSubjectsPerStage[stage]
	        )
	    } else {
	        stageSubjects <- sampleSizesPerStage[stage - 1]
	    }
	    return(stageSubjects)
	}
	x10 <- getSimulationMeans(
	    design = getDesignInverseNormal(futilityBounds = c(0.5, 0.5)), groups = 2, meanRatio = TRUE, thetaH0 = 1.6,
	    plannedSubjects = c(80, 160, 240), maxNumberOfIterations = maxNumberOfIterations, stDev = 1.5, alternative = seq(0.8, 1.6, 0.2),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(NA, 40, 40), maxNumberOfSubjectsPerStage = c(NA, 400, 400),
	    allocationRatioPlanned = 3, directionUpper = FALSE, seed = seed, calcSubjectsFunction = calcSubjectsFunctionSimulationBaseMeans
	)
    
	## Comparison of the results of SimulationResultsMeans object 'x10' with expected results
	expect_equal(x10$effect, c(-0.8, -0.6, -0.4, -0.2, 0), tolerance = 1e-07)
	expect_equal(x10$iterations[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x10$iterations[2, ], c(80, 73, 59, 46, 29))
	expect_equal(x10$iterations[3, ], c(47, 49, 53, 37, 23))
	expect_equal(x10$overallReject, c(0.71, 0.59, 0.3, 0.16, 0.03), tolerance = 1e-07)
	expect_equal(x10$rejectPerStage[1, ], c(0.01, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x10$rejectPerStage[2, ], c(0.33, 0.24, 0.05, 0.03, 0.02), tolerance = 1e-07)
	expect_equal(x10$rejectPerStage[3, ], c(0.37, 0.35, 0.25, 0.13, 0.01), tolerance = 1e-07)
	expect_equal(x10$futilityStop, c(0.19, 0.27, 0.42, 0.6, 0.75), tolerance = 1e-07)
	expect_equal(x10$futilityPerStage[1, ], c(0.19, 0.27, 0.41, 0.54, 0.71), tolerance = 1e-07)
	expect_equal(x10$futilityPerStage[2, ], c(0, 0, 0.01, 0.06, 0.04), tolerance = 1e-07)
	expect_equal(x10$earlyStop, c(0.53, 0.51, 0.47, 0.63, 0.77), tolerance = 1e-07)
	expect_equal(x10$expectedNumberOfSubjects, c(275.20455, 279.99813, 331.87372, 312.93302, 202.36219), tolerance = 1e-07)
	expect_equal(x10$sampleSizes[1, ], c(80, 80, 80, 80, 80))
	expect_equal(x10$sampleSizes[2, ], c(160.20991, 162.95615, 228.62104, 285.92049, 236.43279), tolerance = 1e-07)
	expect_equal(x10$sampleSizes[3, ], c(142.63111, 165.38805, 220.73076, 274.07999, 233.89861), tolerance = 1e-07)
	expect_equal(x10$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x10$conditionalPowerAchieved[2, ], c(0.61849372, 0.63239423, 0.52503669, 0.48190934, 0.5387573), tolerance = 1e-07)
	expect_equal(x10$conditionalPowerAchieved[3, ], c(0.77627313, 0.69241344, 0.58084669, 0.41531587, 0.35026151), tolerance = 1e-07)
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
	    expect_equal(x10CodeBased$futilityStop, x10$futilityStop, tolerance = 1e-05)
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

test_plan_section("Testing Simulation Means Function in a Systematic Way")


test_that("'getSimulationMeans': Fisher design with several configurations", {
	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
	# @refFS[Tab.]{fs:tab:output:getSimulationMeans}
	# @refFS[Formula]{fs:simulationOneArmMeansTestStatistics}
	# @refFS[Formula]{fs:simulationTwoArmMeansTestStatisticsDiff}
	# @refFS[Formula]{fs:simulationTwoArmMeansTestStatisticsRatio}
	# @refFS[Formula]{fs:testStatisticGroupSequentialWeightedAverage}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	x1 <- getSimulationMeans(
	    seed = 1234, getDesignFisher(informationRates = c(0.3333, 1)),
	    normalApproximation = TRUE, groups = 1, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = TRUE, maxNumberOfIterations = 100, thetaH0 = 0.05
	)

	## Comparison of the results of SimulationResultsMeans object 'x1' with expected results
	expect_equal(x1$effect, c(-0.05, 0.35, 0.75), tolerance = 1e-07)
	expect_equal(x1$iterations[1, ], c(100, 100, 100))
	expect_equal(x1$iterations[2, ], c(100, 91, 53))
	expect_equal(x1$overallReject, c(0.01, 0.67, 0.93), tolerance = 1e-07)
	expect_equal(x1$rejectPerStage[1, ], c(0, 0.09, 0.47), tolerance = 1e-07)
	expect_equal(x1$rejectPerStage[2, ], c(0.01, 0.58, 0.46), tolerance = 1e-07)
	expect_equal(x1$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x1$earlyStop, c(0, 0.09, 0.47), tolerance = 1e-07)
	expect_equal(x1$expectedNumberOfSubjects, c(100.13629, 75.286263, 37.754027), tolerance = 1e-07)
	expect_equal(x1$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x1$sampleSizes[2, ], c(90.136293, 71.743146, 52.366088), tolerance = 1e-07)
	expect_equal(x1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x1$conditionalPowerAchieved[2, ], c(0.20283076, 0.49941507, 0.64819831), tolerance = 1e-07)
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

	.skipTestIfDisabled()

	x2 <- getSimulationMeans(
	    seed = 1234, getDesignFisher(informationRates = c(0.3333, 1)),
	    normalApproximation = TRUE, groups = 1, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = FALSE, maxNumberOfIterations = 100, thetaH0 = 0.8
	)

	## Comparison of the results of SimulationResultsMeans object 'x2' with expected results
	expect_equal(x2$effect, c(-0.8, -0.4, 0), tolerance = 1e-07)
	expect_equal(x2$iterations[1, ], c(100, 100, 100))
	expect_equal(x2$iterations[2, ], c(38, 94, 97))
	expect_equal(x2$overallReject, c(0.96, 0.74, 0.06), tolerance = 1e-07)
	expect_equal(x2$rejectPerStage[1, ], c(0.62, 0.06, 0.03), tolerance = 1e-07)
	expect_equal(x2$rejectPerStage[2, ], c(0.34, 0.68, 0.03), tolerance = 1e-07)
	expect_equal(x2$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x2$earlyStop, c(0.62, 0.06, 0.03), tolerance = 1e-07)
	expect_equal(x2$expectedNumberOfSubjects, c(25.921375, 81.226383, 97.518855), tolerance = 1e-07)
	expect_equal(x2$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x2$sampleSizes[2, ], c(41.898355, 75.772748, 90.225624), tolerance = 1e-07)
	expect_equal(x2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x2$conditionalPowerAchieved[2, ], c(0.66927179, 0.47487279, 0.2338584), tolerance = 1e-07)
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

	x3 <- getSimulationMeans(
	    seed = 1234, getDesignFisher(informationRates = c(0.3333, 1)),
	    normalApproximation = FALSE, groups = 1, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = TRUE, maxNumberOfIterations = 100, thetaH0 = 0.05
	)

	## Comparison of the results of SimulationResultsMeans object 'x3' with expected results
	expect_equal(x3$effect, c(-0.05, 0.35, 0.75), tolerance = 1e-07)
	expect_equal(x3$iterations[1, ], c(100, 100, 100))
	expect_equal(x3$iterations[2, ], c(100, 92, 64))
	expect_equal(x3$overallReject, c(0, 0.62, 0.92), tolerance = 1e-07)
	expect_equal(x3$rejectPerStage[1, ], c(0, 0.08, 0.36), tolerance = 1e-07)
	expect_equal(x3$rejectPerStage[2, ], c(0, 0.54, 0.56), tolerance = 1e-07)
	expect_equal(x3$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x3$earlyStop, c(0, 0.08, 0.36), tolerance = 1e-07)
	expect_equal(x3$expectedNumberOfSubjects, c(101.14709, 82.477228, 37.608934), tolerance = 1e-07)
	expect_equal(x3$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x3$sampleSizes[2, ], c(91.147091, 78.779596, 43.13896), tolerance = 1e-07)
	expect_equal(x3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x3$conditionalPowerAchieved[2, ], c(0.15986579, 0.45599322, 0.69664803), tolerance = 1e-07)
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

	x4 <- getSimulationMeans(
	    seed = 1234, getDesignFisher(informationRates = c(0.3333, 1)),
	    normalApproximation = FALSE, groups = 1, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = FALSE, maxNumberOfIterations = 100, thetaH0 = 0.8
	)

	## Comparison of the results of SimulationResultsMeans object 'x4' with expected results
	expect_equal(x4$effect, c(-0.8, -0.4, 0), tolerance = 1e-07)
	expect_equal(x4$iterations[1, ], c(100, 100, 100))
	expect_equal(x4$iterations[2, ], c(65, 91, 100))
	expect_equal(x4$overallReject, c(0.91, 0.73, 0.01), tolerance = 1e-07)
	expect_equal(x4$rejectPerStage[1, ], c(0.35, 0.09, 0), tolerance = 1e-07)
	expect_equal(x4$rejectPerStage[2, ], c(0.56, 0.64, 0.01), tolerance = 1e-07)
	expect_equal(x4$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x4$earlyStop, c(0.35, 0.09, 0), tolerance = 1e-07)
	expect_equal(x4$expectedNumberOfSubjects, c(38.729726, 74.553457, 106.20499), tolerance = 1e-07)
	expect_equal(x4$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x4$sampleSizes[2, ], c(44.199579, 70.937865, 96.204991), tolerance = 1e-07)
	expect_equal(x4$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x4$conditionalPowerAchieved[2, ], c(0.65544931, 0.50900228, 0.13524564), tolerance = 1e-07)
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

	x5 <- getSimulationMeans(
	    seed = 1234, getDesignFisher(informationRates = c(0.3333, 1)),
	    normalApproximation = TRUE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = TRUE, meanRatio = FALSE, maxNumberOfIterations = 100, thetaH0 = 0.05
	)

	## Comparison of the results of SimulationResultsMeans object 'x5' with expected results
	expect_equal(x5$effect, c(-0.05, 0.35, 0.75), tolerance = 1e-07)
	expect_equal(x5$iterations[1, ], c(100, 100, 100))
	expect_equal(x5$iterations[2, ], c(100, 94, 85))
	expect_equal(x5$overallReject, c(0.02, 0.3, 0.65), tolerance = 1e-07)
	expect_equal(x5$rejectPerStage[1, ], c(0, 0.06, 0.15), tolerance = 1e-07)
	expect_equal(x5$rejectPerStage[2, ], c(0.02, 0.24, 0.5), tolerance = 1e-07)
	expect_equal(x5$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x5$earlyStop, c(0, 0.06, 0.15), tolerance = 1e-07)
	expect_equal(x5$expectedNumberOfSubjects, c(99.262844, 92.628587, 72.466684), tolerance = 1e-07)
	expect_equal(x5$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x5$sampleSizes[2, ], c(89.262844, 87.902752, 73.490217), tolerance = 1e-07)
	expect_equal(x5$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x5$conditionalPowerAchieved[2, ], c(0.21679818, 0.32589621, 0.46073426), tolerance = 1e-07)
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

	x6 <- getSimulationMeans(
	    seed = 1234, getDesignFisher(informationRates = c(0.3333, 1)),
	    normalApproximation = TRUE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = FALSE, meanRatio = FALSE, maxNumberOfIterations = 100, thetaH0 = 0.8
	)

	## Comparison of the results of SimulationResultsMeans object 'x6' with expected results
	expect_equal(x6$effect, c(-0.8, -0.4, 0), tolerance = 1e-07)
	expect_equal(x6$iterations[1, ], c(100, 100, 100))
	expect_equal(x6$iterations[2, ], c(85, 94, 97))
	expect_equal(x6$overallReject, c(0.73, 0.2, 0.05), tolerance = 1e-07)
	expect_equal(x6$rejectPerStage[1, ], c(0.15, 0.06, 0.03), tolerance = 1e-07)
	expect_equal(x6$rejectPerStage[2, ], c(0.58, 0.14, 0.02), tolerance = 1e-07)
	expect_equal(x6$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x6$earlyStop, c(0.15, 0.06, 0.03), tolerance = 1e-07)
	expect_equal(x6$expectedNumberOfSubjects, c(62.256855, 90.679118, 97.117191), tolerance = 1e-07)
	expect_equal(x6$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x6$sampleSizes[2, ], c(61.478653, 85.828849, 89.811537), tolerance = 1e-07)
	expect_equal(x6$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x6$conditionalPowerAchieved[2, ], c(0.5750772, 0.31560556, 0.25161462), tolerance = 1e-07)
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

	x7 <- getSimulationMeans(
	    seed = 1234, getDesignFisher(informationRates = c(0.3333, 1)),
	    normalApproximation = FALSE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = TRUE, meanRatio = FALSE, maxNumberOfIterations = 100, thetaH0 = 0.05
	)

	## Comparison of the results of SimulationResultsMeans object 'x7' with expected results
	expect_equal(x7$effect, c(-0.05, 0.35, 0.75), tolerance = 1e-07)
	expect_equal(x7$iterations[1, ], c(100, 100, 100))
	expect_equal(x7$iterations[2, ], c(100, 98, 89))
	expect_equal(x7$overallReject, c(0, 0.15, 0.75), tolerance = 1e-07)
	expect_equal(x7$rejectPerStage[1, ], c(0, 0.02, 0.11), tolerance = 1e-07)
	expect_equal(x7$rejectPerStage[2, ], c(0, 0.13, 0.64), tolerance = 1e-07)
	expect_equal(x7$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x7$earlyStop, c(0, 0.02, 0.11), tolerance = 1e-07)
	expect_equal(x7$expectedNumberOfSubjects, c(99.499784, 89.67646, 74.321885), tolerance = 1e-07)
	expect_equal(x7$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x7$sampleSizes[2, ], c(89.499784, 81.30251, 72.27178), tolerance = 1e-07)
	expect_equal(x7$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x7$conditionalPowerAchieved[2, ], c(0.19464679, 0.38425169, 0.50691811), tolerance = 1e-07)
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

	x8 <- getSimulationMeans(
	    seed = 1234, getDesignFisher(informationRates = c(0.3333, 1)),
	    normalApproximation = FALSE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0.1, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = FALSE, meanRatio = TRUE, maxNumberOfIterations = 100, thetaH0 = 0.8
	)

	## Comparison of the results of SimulationResultsMeans object 'x8' with expected results
	expect_equal(x8$effect, c(-0.7, -0.4, 0), tolerance = 1e-07)
	expect_equal(x8$iterations[1, ], c(100, 100, 100))
	expect_equal(x8$iterations[2, ], c(92, 96, 100))
	expect_equal(x8$overallReject, c(0.6, 0.28, 0.01), tolerance = 1e-07)
	expect_equal(x8$rejectPerStage[1, ], c(0.08, 0.04, 0), tolerance = 1e-07)
	expect_equal(x8$rejectPerStage[2, ], c(0.52, 0.24, 0.01), tolerance = 1e-07)
	expect_equal(x8$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x8$earlyStop, c(0.08, 0.04, 0), tolerance = 1e-07)
	expect_equal(x8$expectedNumberOfSubjects, c(75.059866, 89.365281, 105.96832), tolerance = 1e-07)
	expect_equal(x8$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x8$sampleSizes[2, ], c(70.717246, 82.672167, 95.968315), tolerance = 1e-07)
	expect_equal(x8$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x8$conditionalPowerAchieved[2, ], c(0.47813695, 0.33190551, 0.14267564), tolerance = 1e-07)
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

	x9 <- getSimulationMeans(
	    seed = 1234, getDesignFisher(informationRates = c(0.3333, 1)), normalApproximation = TRUE,
	    groups = 2, plannedSubjects = c(10, 30), alternative = c(0.1, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = TRUE, meanRatio = TRUE, maxNumberOfIterations = 100, thetaH0 = 0.05
	)

	## Comparison of the results of SimulationResultsMeans object 'x9' with expected results
	expect_equal(x9$effect, c(0.05, 0.35, 0.75), tolerance = 1e-07)
	expect_equal(x9$iterations[1, ], c(100, 100, 100))
	expect_equal(x9$iterations[2, ], c(99, 94, 80))
	expect_equal(x9$overallReject, c(0.06, 0.4, 0.86), tolerance = 1e-07)
	expect_equal(x9$rejectPerStage[1, ], c(0.01, 0.06, 0.2), tolerance = 1e-07)
	expect_equal(x9$rejectPerStage[2, ], c(0.05, 0.34, 0.66), tolerance = 1e-07)
	expect_equal(x9$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x9$earlyStop, c(0.01, 0.06, 0.2), tolerance = 1e-07)
	expect_equal(x9$expectedNumberOfSubjects, c(96.293417, 87.052198, 59.545442), tolerance = 1e-07)
	expect_equal(x9$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x9$sampleSizes[2, ], c(87.165067, 81.970424, 61.931803), tolerance = 1e-07)
	expect_equal(x9$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x9$conditionalPowerAchieved[2, ], c(0.23503536, 0.37772778, 0.53734864), tolerance = 1e-07)
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

	x10 <- getSimulationMeans(
	    seed = 1234, getDesignFisher(informationRates = c(0.3333, 1)),
	    normalApproximation = TRUE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0.1, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = FALSE, meanRatio = TRUE, maxNumberOfIterations = 100, thetaH0 = 0.8
	)

	## Comparison of the results of SimulationResultsMeans object 'x10' with expected results
	expect_equal(x10$effect, c(-0.7, -0.4, 0), tolerance = 1e-07)
	expect_equal(x10$iterations[1, ], c(100, 100, 100))
	expect_equal(x10$iterations[2, ], c(89, 93, 98))
	expect_equal(x10$overallReject, c(0.66, 0.31, 0.04), tolerance = 1e-07)
	expect_equal(x10$rejectPerStage[1, ], c(0.11, 0.07, 0.02), tolerance = 1e-07)
	expect_equal(x10$rejectPerStage[2, ], c(0.55, 0.24, 0.02), tolerance = 1e-07)
	expect_equal(x10$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x10$earlyStop, c(0.11, 0.07, 0.02), tolerance = 1e-07)
	expect_equal(x10$expectedNumberOfSubjects, c(64.458245, 88.745903, 98.117191), tolerance = 1e-07)
	expect_equal(x10$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x10$sampleSizes[2, ], c(61.189039, 84.673014, 89.915501), tolerance = 1e-07)
	expect_equal(x10$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x10$conditionalPowerAchieved[2, ], c(0.53544626, 0.3174792, 0.23558604), tolerance = 1e-07)
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

	x11 <- getSimulationMeans(
	    seed = 1234, getDesignFisher(informationRates = c(0.3333, 1)),
	    normalApproximation = FALSE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0.1, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = TRUE, meanRatio = TRUE, maxNumberOfIterations = 100, thetaH0 = 0.05
	)

	## Comparison of the results of SimulationResultsMeans object 'x11' with expected results
	expect_equal(x11$effect, c(0.05, 0.35, 0.75), tolerance = 1e-07)
	expect_equal(x11$iterations[1, ], c(100, 100, 100))
	expect_equal(x11$iterations[2, ], c(98, 96, 79))
	expect_equal(x11$overallReject, c(0.03, 0.32, 0.77), tolerance = 1e-07)
	expect_equal(x11$rejectPerStage[1, ], c(0.02, 0.04, 0.21), tolerance = 1e-07)
	expect_equal(x11$rejectPerStage[2, ], c(0.01, 0.28, 0.56), tolerance = 1e-07)
	expect_equal(x11$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x11$earlyStop, c(0.02, 0.04, 0.21), tolerance = 1e-07)
	expect_equal(x11$expectedNumberOfSubjects, c(96.685833, 88.962444, 54.461927), tolerance = 1e-07)
	expect_equal(x11$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x11$sampleSizes[2, ], c(88.454932, 82.252546, 56.28092), tolerance = 1e-07)
	expect_equal(x11$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x11$conditionalPowerAchieved[2, ], c(0.21899188, 0.34972634, 0.63085287), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x11), NA)))
	    expect_output(print(x11)$show())
	    invisible(capture.output(expect_error(summary(x11), NA)))
	    expect_output(summary(x11)$show())
	    x11CodeBased <- eval(parse(text = getObjectRCode(x11, stringWrapParagraphWidth = NULL)))
	    expect_equal(x11CodeBased$effect, x11$effect, tolerance = 1e-05)
	    expect_equal(x11CodeBased$iterations, x11$iterations, tolerance = 1e-05)
	    expect_equal(x11CodeBased$overallReject, x11$overallReject, tolerance = 1e-05)
	    expect_equal(x11CodeBased$rejectPerStage, x11$rejectPerStage, tolerance = 1e-05)
	    expect_equal(x11CodeBased$futilityPerStage, x11$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x11CodeBased$earlyStop, x11$earlyStop, tolerance = 1e-05)
	    expect_equal(x11CodeBased$expectedNumberOfSubjects, x11$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x11CodeBased$sampleSizes, x11$sampleSizes, tolerance = 1e-05)
	    expect_equal(x11CodeBased$conditionalPowerAchieved, x11$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x11), "character")
	    df <- as.data.frame(x11)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x11)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x12 <- getSimulationMeans(
	    seed = 1234, getDesignFisher(informationRates = c(0.3333, 1)),
	    normalApproximation = FALSE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0.1, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = FALSE, meanRatio = TRUE, maxNumberOfIterations = 100, thetaH0 = 0.8
	)

	## Comparison of the results of SimulationResultsMeans object 'x12' with expected results
	expect_equal(x12$effect, c(-0.7, -0.4, 0), tolerance = 1e-07)
	expect_equal(x12$iterations[1, ], c(100, 100, 100))
	expect_equal(x12$iterations[2, ], c(92, 96, 100))
	expect_equal(x12$overallReject, c(0.6, 0.28, 0.01), tolerance = 1e-07)
	expect_equal(x12$rejectPerStage[1, ], c(0.08, 0.04, 0), tolerance = 1e-07)
	expect_equal(x12$rejectPerStage[2, ], c(0.52, 0.24, 0.01), tolerance = 1e-07)
	expect_equal(x12$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x12$earlyStop, c(0.08, 0.04, 0), tolerance = 1e-07)
	expect_equal(x12$expectedNumberOfSubjects, c(75.059866, 89.365281, 105.96832), tolerance = 1e-07)
	expect_equal(x12$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x12$sampleSizes[2, ], c(70.717246, 82.672167, 95.968315), tolerance = 1e-07)
	expect_equal(x12$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x12$conditionalPowerAchieved[2, ], c(0.47813695, 0.33190551, 0.14267564), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x12), NA)))
	    expect_output(print(x12)$show())
	    invisible(capture.output(expect_error(summary(x12), NA)))
	    expect_output(summary(x12)$show())
	    x12CodeBased <- eval(parse(text = getObjectRCode(x12, stringWrapParagraphWidth = NULL)))
	    expect_equal(x12CodeBased$effect, x12$effect, tolerance = 1e-05)
	    expect_equal(x12CodeBased$iterations, x12$iterations, tolerance = 1e-05)
	    expect_equal(x12CodeBased$overallReject, x12$overallReject, tolerance = 1e-05)
	    expect_equal(x12CodeBased$rejectPerStage, x12$rejectPerStage, tolerance = 1e-05)
	    expect_equal(x12CodeBased$futilityPerStage, x12$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x12CodeBased$earlyStop, x12$earlyStop, tolerance = 1e-05)
	    expect_equal(x12CodeBased$expectedNumberOfSubjects, x12$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x12CodeBased$sampleSizes, x12$sampleSizes, tolerance = 1e-05)
	    expect_equal(x12CodeBased$conditionalPowerAchieved, x12$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x12), "character")
	    df <- as.data.frame(x12)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x12)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getSimulationMeans': inverse normal design with several configurations", {

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
	# @refFS[Tab.]{fs:tab:output:getSimulationMeans}
	# @refFS[Formula]{fs:simulationOneArmMeansTestStatistics}
	# @refFS[Formula]{fs:simulationTwoArmMeansTestStatisticsDiff}
	# @refFS[Formula]{fs:simulationTwoArmMeansTestStatisticsRatio}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	x1 <- getSimulationMeans(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.3333, 1)),
	    normalApproximation = TRUE, groups = 1, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = TRUE, maxNumberOfIterations = 100, thetaH0 = 0.05
	)

	## Comparison of the results of SimulationResultsMeans object 'x1' with expected results
	expect_equal(x1$effect, c(-0.05, 0.35, 0.75), tolerance = 1e-07)
	expect_equal(x1$iterations[1, ], c(100, 100, 100))
	expect_equal(x1$iterations[2, ], c(100, 99, 93))
	expect_equal(x1$overallReject, c(0.01, 0.62, 0.84), tolerance = 1e-07)
	expect_equal(x1$rejectPerStage[1, ], c(0, 0.01, 0.07), tolerance = 1e-07)
	expect_equal(x1$rejectPerStage[2, ], c(0.01, 0.61, 0.77), tolerance = 1e-07)
	expect_equal(x1$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x1$earlyStop, c(0, 0.01, 0.07), tolerance = 1e-07)
	expect_equal(x1$expectedNumberOfSubjects, c(97.726214, 61.386317, 35.456429), tolerance = 1e-07)
	expect_equal(x1$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x1$sampleSizes[2, ], c(87.726214, 51.905371, 27.372504), tolerance = 1e-07)
	expect_equal(x1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x1$conditionalPowerAchieved[2, ], c(0.21948944, 0.59945542, 0.74761937), tolerance = 1e-07)
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

	.skipTestIfDisabled()

	x2 <- getSimulationMeans(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.3333, 1)),
	    normalApproximation = TRUE, groups = 1, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = FALSE,
	    maxNumberOfIterations = 100, thetaH0 = 0.8
	)

	## Comparison of the results of SimulationResultsMeans object 'x2' with expected results
	expect_equal(x2$effect, c(-0.8, -0.4, 0), tolerance = 1e-07)
	expect_equal(x2$iterations[1, ], c(100, 100, 100))
	expect_equal(x2$iterations[2, ], c(92, 98, 100))
	expect_equal(x2$overallReject, c(0.88, 0.7, 0.05), tolerance = 1e-07)
	expect_equal(x2$rejectPerStage[1, ], c(0.08, 0.02, 0), tolerance = 1e-07)
	expect_equal(x2$rejectPerStage[2, ], c(0.8, 0.68, 0.05), tolerance = 1e-07)
	expect_equal(x2$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x2$earlyStop, c(0.08, 0.02, 0), tolerance = 1e-07)
	expect_equal(x2$expectedNumberOfSubjects, c(30.529806, 74.585778, 94.761842), tolerance = 1e-07)
	expect_equal(x2$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x2$sampleSizes[2, ], c(22.315007, 65.903855, 84.761842), tolerance = 1e-07)
	expect_equal(x2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x2$conditionalPowerAchieved[2, ], c(0.78002751, 0.51035441, 0.27866093), tolerance = 1e-07)
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

	x3 <- getSimulationMeans(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.3333, 1)),
	    normalApproximation = FALSE, groups = 1, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = TRUE,
	    maxNumberOfIterations = 100, thetaH0 = 0.05
	)

	## Comparison of the results of SimulationResultsMeans object 'x3' with expected results
	expect_equal(x3$effect, c(-0.05, 0.35, 0.75), tolerance = 1e-07)
	expect_equal(x3$iterations[1, ], c(100, 100, 100))
	expect_equal(x3$iterations[2, ], c(100, 100, 98))
	expect_equal(x3$overallReject, c(0.01, 0.58, 0.86), tolerance = 1e-07)
	expect_equal(x3$rejectPerStage[1, ], c(0, 0, 0.02), tolerance = 1e-07)
	expect_equal(x3$rejectPerStage[2, ], c(0.01, 0.58, 0.84), tolerance = 1e-07)
	expect_equal(x3$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x3$earlyStop, c(0, 0, 0.02), tolerance = 1e-07)
	expect_equal(x3$expectedNumberOfSubjects, c(99.571933, 69.623473, 35.859349), tolerance = 1e-07)
	expect_equal(x3$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x3$sampleSizes[2, ], c(89.571933, 59.623473, 26.38709), tolerance = 1e-07)
	expect_equal(x3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x3$conditionalPowerAchieved[2, ], c(0.17531256, 0.54483038, 0.79325539), tolerance = 1e-07)
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

	x4 <- getSimulationMeans(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.3333, 1)),
	    normalApproximation = FALSE, groups = 1, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = FALSE,
	    maxNumberOfIterations = 100, thetaH0 = 0.8
	)

	## Comparison of the results of SimulationResultsMeans object 'x4' with expected results
	expect_equal(x4$effect, c(-0.8, -0.4, 0), tolerance = 1e-07)
	expect_equal(x4$iterations[1, ], c(100, 100, 100))
	expect_equal(x4$iterations[2, ], c(97, 100, 100))
	expect_equal(x4$overallReject, c(0.83, 0.69, 0.01), tolerance = 1e-07)
	expect_equal(x4$rejectPerStage[1, ], c(0.03, 0, 0), tolerance = 1e-07)
	expect_equal(x4$rejectPerStage[2, ], c(0.8, 0.69, 0.01), tolerance = 1e-07)
	expect_equal(x4$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x4$earlyStop, c(0.03, 0, 0), tolerance = 1e-07)
	expect_equal(x4$expectedNumberOfSubjects, c(34.808208, 66.656932, 104.30185), tolerance = 1e-07)
	expect_equal(x4$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x4$sampleSizes[2, ], c(25.575472, 56.656932, 94.301853), tolerance = 1e-07)
	expect_equal(x4$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x4$conditionalPowerAchieved[2, ], c(0.78184392, 0.58902983, 0.17869551), tolerance = 1e-07)
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

	x5 <- getSimulationMeans(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.3333, 1)),
	    normalApproximation = TRUE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = TRUE, meanRatio = FALSE,
	    maxNumberOfIterations = 100, thetaH0 = 0.05
	)

	## Comparison of the results of SimulationResultsMeans object 'x5' with expected results
	expect_equal(x5$effect, c(-0.05, 0.35, 0.75), tolerance = 1e-07)
	expect_equal(x5$iterations[1, ], c(100, 100, 100))
	expect_equal(x5$iterations[2, ], c(100, 100, 100))
	expect_equal(x5$overallReject, c(0.02, 0.29, 0.63), tolerance = 1e-07)
	expect_equal(x5$rejectPerStage[1, ], c(0, 0, 0))
	expect_equal(x5$rejectPerStage[2, ], c(0.02, 0.29, 0.63), tolerance = 1e-07)
	expect_equal(x5$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x5$earlyStop, c(0, 0, 0))
	expect_equal(x5$expectedNumberOfSubjects, c(96.372889, 89.619156, 71.907268), tolerance = 1e-07)
	expect_equal(x5$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x5$sampleSizes[2, ], c(86.372889, 79.619156, 61.907268), tolerance = 1e-07)
	expect_equal(x5$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x5$conditionalPowerAchieved[2, ], c(0.23254121, 0.37156759, 0.50696796), tolerance = 1e-07)
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

	x6 <- getSimulationMeans(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.3333, 1)),
	    normalApproximation = TRUE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = FALSE, meanRatio = FALSE,
	    maxNumberOfIterations = 100, thetaH0 = 0.8
	)

	## Comparison of the results of SimulationResultsMeans object 'x6' with expected results
	expect_equal(x6$effect, c(-0.8, -0.4, 0), tolerance = 1e-07)
	expect_equal(x6$iterations[1, ], c(100, 100, 100))
	expect_equal(x6$iterations[2, ], c(98, 98, 100))
	expect_equal(x6$overallReject, c(0.71, 0.28, 0.05), tolerance = 1e-07)
	expect_equal(x6$rejectPerStage[1, ], c(0.02, 0.02, 0), tolerance = 1e-07)
	expect_equal(x6$rejectPerStage[2, ], c(0.69, 0.26, 0.05), tolerance = 1e-07)
	expect_equal(x6$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x6$earlyStop, c(0.02, 0.02, 0), tolerance = 1e-07)
	expect_equal(x6$expectedNumberOfSubjects, c(61.262488, 89.754099, 94.761842), tolerance = 1e-07)
	expect_equal(x6$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x6$sampleSizes[2, ], c(52.308661, 81.381734, 84.761842), tolerance = 1e-07)
	expect_equal(x6$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x6$conditionalPowerAchieved[2, ], c(0.57015973, 0.32347024, 0.27139992), tolerance = 1e-07)
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

	x7 <- getSimulationMeans(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.3333, 1)),
	    normalApproximation = FALSE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = TRUE, meanRatio = FALSE,
	    maxNumberOfIterations = 100, thetaH0 = 0.05
	)

	## Comparison of the results of SimulationResultsMeans object 'x7' with expected results
	expect_equal(x7$effect, c(-0.05, 0.35, 0.75), tolerance = 1e-07)
	expect_equal(x7$iterations[1, ], c(100, 100, 100))
	expect_equal(x7$iterations[2, ], c(100, 100, 99))
	expect_equal(x7$overallReject, c(0.01, 0.2, 0.7), tolerance = 1e-07)
	expect_equal(x7$rejectPerStage[1, ], c(0, 0, 0.01), tolerance = 1e-07)
	expect_equal(x7$rejectPerStage[2, ], c(0.01, 0.2, 0.69), tolerance = 1e-07)
	expect_equal(x7$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x7$earlyStop, c(0, 0, 0.01), tolerance = 1e-07)
	expect_equal(x7$expectedNumberOfSubjects, c(99.874349, 85.385224, 62.337209), tolerance = 1e-07)
	expect_equal(x7$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x7$sampleSizes[2, ], c(89.874349, 75.385224, 52.865867), tolerance = 1e-07)
	expect_equal(x7$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x7$conditionalPowerAchieved[2, ], c(0.19979349, 0.40152955, 0.59344307), tolerance = 1e-07)
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

	x8 <- getSimulationMeans(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.3333, 1)),
	    normalApproximation = FALSE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0.1, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = FALSE, meanRatio = TRUE,
	    maxNumberOfIterations = 100, thetaH0 = 0.8
	)

	## Comparison of the results of SimulationResultsMeans object 'x8' with expected results
	expect_equal(x8$effect, c(-0.7, -0.4, 0), tolerance = 1e-07)
	expect_equal(x8$iterations[1, ], c(100, 100, 100))
	expect_equal(x8$iterations[2, ], c(99, 100, 100))
	expect_equal(x8$overallReject, c(0.57, 0.35, 0.01), tolerance = 1e-07)
	expect_equal(x8$rejectPerStage[1, ], c(0.01, 0, 0), tolerance = 1e-07)
	expect_equal(x8$rejectPerStage[2, ], c(0.56, 0.35, 0.01), tolerance = 1e-07)
	expect_equal(x8$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x8$earlyStop, c(0.01, 0, 0), tolerance = 1e-07)
	expect_equal(x8$expectedNumberOfSubjects, c(65.632546, 86.865451, 105.50507), tolerance = 1e-07)
	expect_equal(x8$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x8$sampleSizes[2, ], c(56.194491, 76.865451, 95.50507), tolerance = 1e-07)
	expect_equal(x8$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x8$conditionalPowerAchieved[2, ], c(0.52087291, 0.38731069, 0.15906003), tolerance = 1e-07)
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

	x9 <- getSimulationMeans(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.3333, 1)),
	    normalApproximation = TRUE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0.1, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = TRUE, meanRatio = TRUE,
	    maxNumberOfIterations = 100, thetaH0 = 0.05
	)

	## Comparison of the results of SimulationResultsMeans object 'x9' with expected results
	expect_equal(x9$effect, c(0.05, 0.35, 0.75), tolerance = 1e-07)
	expect_equal(x9$iterations[1, ], c(100, 100, 100))
	expect_equal(x9$iterations[2, ], c(100, 99, 98))
	expect_equal(x9$overallReject, c(0.04, 0.36, 0.79), tolerance = 1e-07)
	expect_equal(x9$rejectPerStage[1, ], c(0, 0.01, 0.02), tolerance = 1e-07)
	expect_equal(x9$rejectPerStage[2, ], c(0.04, 0.35, 0.77), tolerance = 1e-07)
	expect_equal(x9$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x9$earlyStop, c(0, 0.01, 0.02), tolerance = 1e-07)
	expect_equal(x9$expectedNumberOfSubjects, c(93.166381, 72.993336, 56.443486), tolerance = 1e-07)
	expect_equal(x9$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x9$sampleSizes[2, ], c(83.166381, 63.629633, 47.391312), tolerance = 1e-07)
	expect_equal(x9$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x9$conditionalPowerAchieved[2, ], c(0.26023971, 0.52016331, 0.61018937), tolerance = 1e-07)
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

	x10 <- getSimulationMeans(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.3333, 1)),
	    normalApproximation = TRUE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0.1, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = FALSE, meanRatio = TRUE,
	    maxNumberOfIterations = 100, thetaH0 = 0.8
	)

	## Comparison of the results of SimulationResultsMeans object 'x10' with expected results
	expect_equal(x10$effect, c(-0.7, -0.4, 0), tolerance = 1e-07)
	expect_equal(x10$iterations[1, ], c(100, 100, 100))
	expect_equal(x10$iterations[2, ], c(98, 98, 100))
	expect_equal(x10$overallReject, c(0.71, 0.32, 0.05), tolerance = 1e-07)
	expect_equal(x10$rejectPerStage[1, ], c(0.02, 0.02, 0), tolerance = 1e-07)
	expect_equal(x10$rejectPerStage[2, ], c(0.69, 0.3, 0.05), tolerance = 1e-07)
	expect_equal(x10$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x10$earlyStop, c(0.02, 0.02, 0), tolerance = 1e-07)
	expect_equal(x10$expectedNumberOfSubjects, c(62.435526, 88.169977, 94.761842), tolerance = 1e-07)
	expect_equal(x10$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x10$sampleSizes[2, ], c(53.505639, 79.765282, 84.761842), tolerance = 1e-07)
	expect_equal(x10$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x10$conditionalPowerAchieved[2, ], c(0.54108822, 0.32455187, 0.25936079), tolerance = 1e-07)
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

	x11 <- getSimulationMeans(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.3333, 1)),
	    normalApproximation = FALSE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0.1, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = TRUE, meanRatio = TRUE,
	    maxNumberOfIterations = 100, thetaH0 = 0.05
	)

	## Comparison of the results of SimulationResultsMeans object 'x11' with expected results
	expect_equal(x11$effect, c(0.05, 0.35, 0.75), tolerance = 1e-07)
	expect_equal(x11$iterations[1, ], c(100, 100, 100))
	expect_equal(x11$iterations[2, ], c(100, 100, 98))
	expect_equal(x11$overallReject, c(0.04, 0.33, 0.76), tolerance = 1e-07)
	expect_equal(x11$rejectPerStage[1, ], c(0, 0, 0.02), tolerance = 1e-07)
	expect_equal(x11$rejectPerStage[2, ], c(0.04, 0.33, 0.74), tolerance = 1e-07)
	expect_equal(x11$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x11$earlyStop, c(0, 0, 0.02), tolerance = 1e-07)
	expect_equal(x11$expectedNumberOfSubjects, c(97.820553, 79.30135, 45.942964), tolerance = 1e-07)
	expect_equal(x11$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x11$sampleSizes[2, ], c(87.820553, 69.30135, 36.676494), tolerance = 1e-07)
	expect_equal(x11$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x11$conditionalPowerAchieved[2, ], c(0.24110629, 0.45389272, 0.70091861), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x11), NA)))
	    expect_output(print(x11)$show())
	    invisible(capture.output(expect_error(summary(x11), NA)))
	    expect_output(summary(x11)$show())
	    x11CodeBased <- eval(parse(text = getObjectRCode(x11, stringWrapParagraphWidth = NULL)))
	    expect_equal(x11CodeBased$effect, x11$effect, tolerance = 1e-05)
	    expect_equal(x11CodeBased$iterations, x11$iterations, tolerance = 1e-05)
	    expect_equal(x11CodeBased$overallReject, x11$overallReject, tolerance = 1e-05)
	    expect_equal(x11CodeBased$rejectPerStage, x11$rejectPerStage, tolerance = 1e-05)
	    expect_equal(x11CodeBased$futilityPerStage, x11$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x11CodeBased$earlyStop, x11$earlyStop, tolerance = 1e-05)
	    expect_equal(x11CodeBased$expectedNumberOfSubjects, x11$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x11CodeBased$sampleSizes, x11$sampleSizes, tolerance = 1e-05)
	    expect_equal(x11CodeBased$conditionalPowerAchieved, x11$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x11), "character")
	    df <- as.data.frame(x11)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x11)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x12 <- getSimulationMeans(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.3333, 1)),
	    normalApproximation = FALSE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0.1, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = FALSE, meanRatio = TRUE,
	    maxNumberOfIterations = 100, thetaH0 = 0.8
	)

	## Comparison of the results of SimulationResultsMeans object 'x12' with expected results
	expect_equal(x12$effect, c(-0.7, -0.4, 0), tolerance = 1e-07)
	expect_equal(x12$iterations[1, ], c(100, 100, 100))
	expect_equal(x12$iterations[2, ], c(99, 100, 100))
	expect_equal(x12$overallReject, c(0.57, 0.35, 0.01), tolerance = 1e-07)
	expect_equal(x12$rejectPerStage[1, ], c(0.01, 0, 0), tolerance = 1e-07)
	expect_equal(x12$rejectPerStage[2, ], c(0.56, 0.35, 0.01), tolerance = 1e-07)
	expect_equal(x12$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x12$earlyStop, c(0.01, 0, 0), tolerance = 1e-07)
	expect_equal(x12$expectedNumberOfSubjects, c(65.632546, 86.865451, 105.50507), tolerance = 1e-07)
	expect_equal(x12$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x12$sampleSizes[2, ], c(56.194491, 76.865451, 95.50507), tolerance = 1e-07)
	expect_equal(x12$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x12$conditionalPowerAchieved[2, ], c(0.52087291, 0.38731069, 0.15906003), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x12), NA)))
	    expect_output(print(x12)$show())
	    invisible(capture.output(expect_error(summary(x12), NA)))
	    expect_output(summary(x12)$show())
	    x12CodeBased <- eval(parse(text = getObjectRCode(x12, stringWrapParagraphWidth = NULL)))
	    expect_equal(x12CodeBased$effect, x12$effect, tolerance = 1e-05)
	    expect_equal(x12CodeBased$iterations, x12$iterations, tolerance = 1e-05)
	    expect_equal(x12CodeBased$overallReject, x12$overallReject, tolerance = 1e-05)
	    expect_equal(x12CodeBased$rejectPerStage, x12$rejectPerStage, tolerance = 1e-05)
	    expect_equal(x12CodeBased$futilityPerStage, x12$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x12CodeBased$earlyStop, x12$earlyStop, tolerance = 1e-05)
	    expect_equal(x12CodeBased$expectedNumberOfSubjects, x12$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x12CodeBased$sampleSizes, x12$sampleSizes, tolerance = 1e-05)
	    expect_equal(x12CodeBased$conditionalPowerAchieved, x12$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x12), "character")
	    df <- as.data.frame(x12)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x12)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getSimulationMeans': group sequential design with several configurations", {

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
	# @refFS[Tab.]{fs:tab:output:getSimulationMeans}
	# @refFS[Formula]{fs:simulationOneArmMeansTestStatistics}
	# @refFS[Formula]{fs:simulationTwoArmMeansTestStatisticsDiff}
	# @refFS[Formula]{fs:simulationTwoArmMeansTestStatisticsRatio}
	# @refFS[Formula]{fs:testStatisticGroupSequentialWeightedAverage}
	x1 <- getSimulationMeans(
	    seed = 1234, getDesignGroupSequential(informationRates = c(0.3333, 1)),
	    normalApproximation = TRUE, groups = 1, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = TRUE,
	    maxNumberOfIterations = 100, thetaH0 = 0.05
	)

	## Comparison of the results of SimulationResultsMeans object 'x1' with expected results
	expect_equal(x1$effect, c(-0.05, 0.35, 0.75), tolerance = 1e-07)
	expect_equal(x1$iterations[1, ], c(100, 100, 100))
	expect_equal(x1$iterations[2, ], c(100, 99, 93))
	expect_equal(x1$overallReject, c(0.02, 0.71, 0.93), tolerance = 1e-07)
	expect_equal(x1$rejectPerStage[1, ], c(0, 0.01, 0.07), tolerance = 1e-07)
	expect_equal(x1$rejectPerStage[2, ], c(0.02, 0.7, 0.86), tolerance = 1e-07)
	expect_equal(x1$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x1$earlyStop, c(0, 0.01, 0.07), tolerance = 1e-07)
	expect_equal(x1$expectedNumberOfSubjects, c(97.726214, 61.386317, 35.456429), tolerance = 1e-07)
	expect_equal(x1$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x1$sampleSizes[2, ], c(87.726214, 51.905371, 27.372504), tolerance = 1e-07)
	expect_equal(x1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x1$conditionalPowerAchieved[2, ], c(0.21948944, 0.59945542, 0.74761937), tolerance = 1e-07)
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

	.skipTestIfDisabled()

	x2 <- getSimulationMeans(
	    seed = 1234, getDesignGroupSequential(informationRates = c(0.3333, 1)),
	    normalApproximation = TRUE, groups = 1, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = FALSE,
	    maxNumberOfIterations = 100, thetaH0 = 0.8
	)

	## Comparison of the results of SimulationResultsMeans object 'x2' with expected results
	expect_equal(x2$effect, c(-0.8, -0.4, 0), tolerance = 1e-07)
	expect_equal(x2$iterations[1, ], c(100, 100, 100))
	expect_equal(x2$iterations[2, ], c(92, 98, 100))
	expect_equal(x2$overallReject, c(0.94, 0.81, 0.07), tolerance = 1e-07)
	expect_equal(x2$rejectPerStage[1, ], c(0.08, 0.02, 0), tolerance = 1e-07)
	expect_equal(x2$rejectPerStage[2, ], c(0.86, 0.79, 0.07), tolerance = 1e-07)
	expect_equal(x2$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x2$earlyStop, c(0.08, 0.02, 0), tolerance = 1e-07)
	expect_equal(x2$expectedNumberOfSubjects, c(30.529806, 74.585778, 94.761842), tolerance = 1e-07)
	expect_equal(x2$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x2$sampleSizes[2, ], c(22.315007, 65.903855, 84.761842), tolerance = 1e-07)
	expect_equal(x2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x2$conditionalPowerAchieved[2, ], c(0.78002751, 0.51035441, 0.27866093), tolerance = 1e-07)
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

	x3 <- getSimulationMeans(
	    seed = 1234, getDesignGroupSequential(informationRates = c(0.3333, 1)),
	    normalApproximation = FALSE, groups = 1, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = TRUE,
	    maxNumberOfIterations = 100, thetaH0 = 0.05
	)

	## Comparison of the results of SimulationResultsMeans object 'x3' with expected results
	expect_equal(x3$effect, c(-0.05, 0.35, 0.75), tolerance = 1e-07)
	expect_equal(x3$iterations[1, ], c(100, 100, 100))
	expect_equal(x3$iterations[2, ], c(100, 100, 98))
	expect_equal(x3$overallReject, c(0.01, 0.68, 0.94), tolerance = 1e-07)
	expect_equal(x3$rejectPerStage[1, ], c(0, 0, 0.02), tolerance = 1e-07)
	expect_equal(x3$rejectPerStage[2, ], c(0.01, 0.68, 0.92), tolerance = 1e-07)
	expect_equal(x3$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x3$earlyStop, c(0, 0, 0.02), tolerance = 1e-07)
	expect_equal(x3$expectedNumberOfSubjects, c(99.571933, 69.623473, 35.859349), tolerance = 1e-07)
	expect_equal(x3$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x3$sampleSizes[2, ], c(89.571933, 59.623473, 26.38709), tolerance = 1e-07)
	expect_equal(x3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x3$conditionalPowerAchieved[2, ], c(0.17531256, 0.54483038, 0.79325539), tolerance = 1e-07)
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

	x4 <- getSimulationMeans(
	    seed = 1234, getDesignGroupSequential(informationRates = c(0.3333, 1)),
	    normalApproximation = FALSE, groups = 1, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = FALSE,
	    maxNumberOfIterations = 100, thetaH0 = 0.8
	)

	## Comparison of the results of SimulationResultsMeans object 'x4' with expected results
	expect_equal(x4$effect, c(-0.8, -0.4, 0), tolerance = 1e-07)
	expect_equal(x4$iterations[1, ], c(100, 100, 100))
	expect_equal(x4$iterations[2, ], c(97, 100, 100))
	expect_equal(x4$overallReject, c(0.92, 0.78, 0.02), tolerance = 1e-07)
	expect_equal(x4$rejectPerStage[1, ], c(0.03, 0, 0), tolerance = 1e-07)
	expect_equal(x4$rejectPerStage[2, ], c(0.89, 0.78, 0.02), tolerance = 1e-07)
	expect_equal(x4$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x4$earlyStop, c(0.03, 0, 0), tolerance = 1e-07)
	expect_equal(x4$expectedNumberOfSubjects, c(34.808208, 66.656932, 104.30185), tolerance = 1e-07)
	expect_equal(x4$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x4$sampleSizes[2, ], c(25.575472, 56.656932, 94.301853), tolerance = 1e-07)
	expect_equal(x4$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x4$conditionalPowerAchieved[2, ], c(0.78184392, 0.58902983, 0.17869551), tolerance = 1e-07)
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

	x5 <- getSimulationMeans(
	    seed = 1234, getDesignGroupSequential(informationRates = c(0.3333, 1)),
	    normalApproximation = TRUE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = TRUE, meanRatio = FALSE,
	    maxNumberOfIterations = 100, thetaH0 = 0.05
	)

	## Comparison of the results of SimulationResultsMeans object 'x5' with expected results
	expect_equal(x5$effect, c(-0.05, 0.35, 0.75), tolerance = 1e-07)
	expect_equal(x5$iterations[1, ], c(100, 100, 100))
	expect_equal(x5$iterations[2, ], c(100, 100, 100))
	expect_equal(x5$overallReject, c(0.03, 0.36, 0.74), tolerance = 1e-07)
	expect_equal(x5$rejectPerStage[1, ], c(0, 0, 0))
	expect_equal(x5$rejectPerStage[2, ], c(0.03, 0.36, 0.74), tolerance = 1e-07)
	expect_equal(x5$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x5$earlyStop, c(0, 0, 0))
	expect_equal(x5$expectedNumberOfSubjects, c(96.372889, 89.619156, 71.907268), tolerance = 1e-07)
	expect_equal(x5$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x5$sampleSizes[2, ], c(86.372889, 79.619156, 61.907268), tolerance = 1e-07)
	expect_equal(x5$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x5$conditionalPowerAchieved[2, ], c(0.23254121, 0.37156759, 0.50696796), tolerance = 1e-07)
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

	x6 <- getSimulationMeans(
	    seed = 1234, getDesignGroupSequential(informationRates = c(0.3333, 1)),
	    normalApproximation = TRUE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = FALSE, meanRatio = FALSE,
	    maxNumberOfIterations = 100, thetaH0 = 0.8
	)

	## Comparison of the results of SimulationResultsMeans object 'x6' with expected results
	expect_equal(x6$effect, c(-0.8, -0.4, 0), tolerance = 1e-07)
	expect_equal(x6$iterations[1, ], c(100, 100, 100))
	expect_equal(x6$iterations[2, ], c(98, 98, 100))
	expect_equal(x6$overallReject, c(0.79, 0.36, 0.06), tolerance = 1e-07)
	expect_equal(x6$rejectPerStage[1, ], c(0.02, 0.02, 0), tolerance = 1e-07)
	expect_equal(x6$rejectPerStage[2, ], c(0.77, 0.34, 0.06), tolerance = 1e-07)
	expect_equal(x6$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x6$earlyStop, c(0.02, 0.02, 0), tolerance = 1e-07)
	expect_equal(x6$expectedNumberOfSubjects, c(61.262488, 89.754099, 94.761842), tolerance = 1e-07)
	expect_equal(x6$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x6$sampleSizes[2, ], c(52.308661, 81.381734, 84.761842), tolerance = 1e-07)
	expect_equal(x6$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x6$conditionalPowerAchieved[2, ], c(0.57015973, 0.32347024, 0.27139992), tolerance = 1e-07)
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

	x7 <- getSimulationMeans(
	    seed = 1234, getDesignGroupSequential(informationRates = c(0.3333, 1)),
	    normalApproximation = FALSE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = TRUE, meanRatio = FALSE,
	    maxNumberOfIterations = 100, thetaH0 = 0.05
	)

	## Comparison of the results of SimulationResultsMeans object 'x7' with expected results
	expect_equal(x7$effect, c(-0.05, 0.35, 0.75), tolerance = 1e-07)
	expect_equal(x7$iterations[1, ], c(100, 100, 100))
	expect_equal(x7$iterations[2, ], c(100, 100, 99))
	expect_equal(x7$overallReject, c(0.01, 0.23, 0.83), tolerance = 1e-07)
	expect_equal(x7$rejectPerStage[1, ], c(0, 0, 0.01), tolerance = 1e-07)
	expect_equal(x7$rejectPerStage[2, ], c(0.01, 0.23, 0.82), tolerance = 1e-07)
	expect_equal(x7$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x7$earlyStop, c(0, 0, 0.01), tolerance = 1e-07)
	expect_equal(x7$expectedNumberOfSubjects, c(99.874349, 85.385224, 62.337209), tolerance = 1e-07)
	expect_equal(x7$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x7$sampleSizes[2, ], c(89.874349, 75.385224, 52.865867), tolerance = 1e-07)
	expect_equal(x7$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x7$conditionalPowerAchieved[2, ], c(0.19979349, 0.40152955, 0.59344307), tolerance = 1e-07)
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

	x8 <- getSimulationMeans(
	    seed = 1234, getDesignGroupSequential(informationRates = c(0.3333, 1)),
	    normalApproximation = FALSE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0.1, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = FALSE, meanRatio = TRUE,
	    maxNumberOfIterations = 100, thetaH0 = 0.8
	)

	## Comparison of the results of SimulationResultsMeans object 'x8' with expected results
	expect_equal(x8$effect, c(-0.7, -0.4, 0), tolerance = 1e-07)
	expect_equal(x8$iterations[1, ], c(100, 100, 100))
	expect_equal(x8$iterations[2, ], c(99, 100, 100))
	expect_equal(x8$overallReject, c(0.72, 0.45, 0.01), tolerance = 1e-07)
	expect_equal(x8$rejectPerStage[1, ], c(0.01, 0, 0), tolerance = 1e-07)
	expect_equal(x8$rejectPerStage[2, ], c(0.71, 0.45, 0.01), tolerance = 1e-07)
	expect_equal(x8$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x8$earlyStop, c(0.01, 0, 0), tolerance = 1e-07)
	expect_equal(x8$expectedNumberOfSubjects, c(65.632546, 86.865451, 105.50507), tolerance = 1e-07)
	expect_equal(x8$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x8$sampleSizes[2, ], c(56.194491, 76.865451, 95.50507), tolerance = 1e-07)
	expect_equal(x8$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x8$conditionalPowerAchieved[2, ], c(0.52087291, 0.38731069, 0.15906003), tolerance = 1e-07)
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

	x9 <- getSimulationMeans(
	    seed = 1234, getDesignGroupSequential(informationRates = c(0.3333, 1)),
	    normalApproximation = TRUE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0.1, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = TRUE, meanRatio = TRUE,
	    maxNumberOfIterations = 100, thetaH0 = 0.05
	)

	## Comparison of the results of SimulationResultsMeans object 'x9' with expected results
	expect_equal(x9$effect, c(0.05, 0.35, 0.75), tolerance = 1e-07)
	expect_equal(x9$iterations[1, ], c(100, 100, 100))
	expect_equal(x9$iterations[2, ], c(100, 99, 98))
	expect_equal(x9$overallReject, c(0.09, 0.44, 0.85), tolerance = 1e-07)
	expect_equal(x9$rejectPerStage[1, ], c(0, 0.01, 0.02), tolerance = 1e-07)
	expect_equal(x9$rejectPerStage[2, ], c(0.09, 0.43, 0.83), tolerance = 1e-07)
	expect_equal(x9$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x9$earlyStop, c(0, 0.01, 0.02), tolerance = 1e-07)
	expect_equal(x9$expectedNumberOfSubjects, c(93.166381, 72.993336, 56.443486), tolerance = 1e-07)
	expect_equal(x9$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x9$sampleSizes[2, ], c(83.166381, 63.629633, 47.391312), tolerance = 1e-07)
	expect_equal(x9$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x9$conditionalPowerAchieved[2, ], c(0.26023971, 0.52016331, 0.61018937), tolerance = 1e-07)
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

	x10 <- getSimulationMeans(
	    seed = 1234, getDesignGroupSequential(informationRates = c(0.3333, 1)),
	    normalApproximation = TRUE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0.1, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = FALSE, meanRatio = TRUE,
	    maxNumberOfIterations = 100, thetaH0 = 0.8
	)

	## Comparison of the results of SimulationResultsMeans object 'x10' with expected results
	expect_equal(x10$effect, c(-0.7, -0.4, 0), tolerance = 1e-07)
	expect_equal(x10$iterations[1, ], c(100, 100, 100))
	expect_equal(x10$iterations[2, ], c(98, 98, 100))
	expect_equal(x10$overallReject, c(0.76, 0.42, 0.06), tolerance = 1e-07)
	expect_equal(x10$rejectPerStage[1, ], c(0.02, 0.02, 0), tolerance = 1e-07)
	expect_equal(x10$rejectPerStage[2, ], c(0.74, 0.4, 0.06), tolerance = 1e-07)
	expect_equal(x10$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x10$earlyStop, c(0.02, 0.02, 0), tolerance = 1e-07)
	expect_equal(x10$expectedNumberOfSubjects, c(62.435526, 88.169977, 94.761842), tolerance = 1e-07)
	expect_equal(x10$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x10$sampleSizes[2, ], c(53.505639, 79.765282, 84.761842), tolerance = 1e-07)
	expect_equal(x10$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x10$conditionalPowerAchieved[2, ], c(0.54108822, 0.32455187, 0.25936079), tolerance = 1e-07)
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

	x11 <- getSimulationMeans(
	    seed = 1234, getDesignGroupSequential(informationRates = c(0.3333, 1)),
	    normalApproximation = FALSE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0.1, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = TRUE, meanRatio = TRUE,
	    maxNumberOfIterations = 100, thetaH0 = 0.05
	)

	## Comparison of the results of SimulationResultsMeans object 'x11' with expected results
	expect_equal(x11$effect, c(0.05, 0.35, 0.75), tolerance = 1e-07)
	expect_equal(x11$iterations[1, ], c(100, 100, 100))
	expect_equal(x11$iterations[2, ], c(100, 100, 98))
	expect_equal(x11$overallReject, c(0.12, 0.39, 0.87), tolerance = 1e-07)
	expect_equal(x11$rejectPerStage[1, ], c(0, 0, 0.02), tolerance = 1e-07)
	expect_equal(x11$rejectPerStage[2, ], c(0.12, 0.39, 0.85), tolerance = 1e-07)
	expect_equal(x11$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x11$earlyStop, c(0, 0, 0.02), tolerance = 1e-07)
	expect_equal(x11$expectedNumberOfSubjects, c(97.820553, 79.30135, 45.942964), tolerance = 1e-07)
	expect_equal(x11$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x11$sampleSizes[2, ], c(87.820553, 69.30135, 36.676494), tolerance = 1e-07)
	expect_equal(x11$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x11$conditionalPowerAchieved[2, ], c(0.24110629, 0.45389272, 0.70091861), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x11), NA)))
	    expect_output(print(x11)$show())
	    invisible(capture.output(expect_error(summary(x11), NA)))
	    expect_output(summary(x11)$show())
	    x11CodeBased <- eval(parse(text = getObjectRCode(x11, stringWrapParagraphWidth = NULL)))
	    expect_equal(x11CodeBased$effect, x11$effect, tolerance = 1e-05)
	    expect_equal(x11CodeBased$iterations, x11$iterations, tolerance = 1e-05)
	    expect_equal(x11CodeBased$overallReject, x11$overallReject, tolerance = 1e-05)
	    expect_equal(x11CodeBased$rejectPerStage, x11$rejectPerStage, tolerance = 1e-05)
	    expect_equal(x11CodeBased$futilityPerStage, x11$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x11CodeBased$earlyStop, x11$earlyStop, tolerance = 1e-05)
	    expect_equal(x11CodeBased$expectedNumberOfSubjects, x11$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x11CodeBased$sampleSizes, x11$sampleSizes, tolerance = 1e-05)
	    expect_equal(x11CodeBased$conditionalPowerAchieved, x11$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x11), "character")
	    df <- as.data.frame(x11)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x11)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x12 <- getSimulationMeans(
	    seed = 1234, getDesignGroupSequential(informationRates = c(0.3333, 1)),
	    normalApproximation = FALSE, groups = 2, plannedSubjects = c(10, 30), alternative = c(0.1, 0.4, 0.8),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4), maxNumberOfSubjectsPerStage = c(10, 100),
	    stDev = 1.2, directionUpper = FALSE, meanRatio = TRUE,
	    maxNumberOfIterations = 100, thetaH0 = 0.8
	)

	## Comparison of the results of SimulationResultsMeans object 'x12' with expected results
	expect_equal(x12$effect, c(-0.7, -0.4, 0), tolerance = 1e-07)
	expect_equal(x12$iterations[1, ], c(100, 100, 100))
	expect_equal(x12$iterations[2, ], c(99, 100, 100))
	expect_equal(x12$overallReject, c(0.72, 0.45, 0.01), tolerance = 1e-07)
	expect_equal(x12$rejectPerStage[1, ], c(0.01, 0, 0), tolerance = 1e-07)
	expect_equal(x12$rejectPerStage[2, ], c(0.71, 0.45, 0.01), tolerance = 1e-07)
	expect_equal(x12$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x12$earlyStop, c(0.01, 0, 0), tolerance = 1e-07)
	expect_equal(x12$expectedNumberOfSubjects, c(65.632546, 86.865451, 105.50507), tolerance = 1e-07)
	expect_equal(x12$sampleSizes[1, ], c(10, 10, 10))
	expect_equal(x12$sampleSizes[2, ], c(56.194491, 76.865451, 95.50507), tolerance = 1e-07)
	expect_equal(x12$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x12$conditionalPowerAchieved[2, ], c(0.52087291, 0.38731069, 0.15906003), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x12), NA)))
	    expect_output(print(x12)$show())
	    invisible(capture.output(expect_error(summary(x12), NA)))
	    expect_output(summary(x12)$show())
	    x12CodeBased <- eval(parse(text = getObjectRCode(x12, stringWrapParagraphWidth = NULL)))
	    expect_equal(x12CodeBased$effect, x12$effect, tolerance = 1e-05)
	    expect_equal(x12CodeBased$iterations, x12$iterations, tolerance = 1e-05)
	    expect_equal(x12CodeBased$overallReject, x12$overallReject, tolerance = 1e-05)
	    expect_equal(x12CodeBased$rejectPerStage, x12$rejectPerStage, tolerance = 1e-05)
	    expect_equal(x12CodeBased$futilityPerStage, x12$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x12CodeBased$earlyStop, x12$earlyStop, tolerance = 1e-05)
	    expect_equal(x12CodeBased$expectedNumberOfSubjects, x12$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x12CodeBased$sampleSizes, x12$sampleSizes, tolerance = 1e-05)
	    expect_equal(x12CodeBased$conditionalPowerAchieved, x12$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x12), "character")
	    df <- as.data.frame(x12)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x12)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getSimulationMeans': comparison with getPowerMeans() results", {

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingTestingOneHypothesis}
	# @refFS[Tab.]{fs:tab:output:getSimulationMeans}
	# @refFS[Formula]{fs:simulationOneArmMeansTestStatistics}
	# @refFS[Formula]{fs:simulationTwoArmMeansTestStatisticsDiff}
	# @refFS[Formula]{fs:simulationTwoArmMeansTestStatisticsRatio}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	.skipTestIfDisabled()

	x1 <- getSimulationMeans(
	    seed = 1234, design = getDesignInverseNormal(futilityBounds = c(-0.5, 0.5), informationRates = c(0.2, 0.5, 1)), groups = 2, meanRatio = TRUE, thetaH0 = 0.4,
	    plannedSubjects = c(40, 100, 200), maxNumberOfIterations = 1000, allocationRatioPlanned = 3, stDev = 1.5
	)
	y1 <- getPowerMeans(
	    design = getDesignInverseNormal(futilityBounds = c(-0.5, 0.5), informationRates = c(0.2, 0.5, 1)), groups = 2, meanRatio = TRUE, thetaH0 = 0.4,
	    maxNumberOfSubjects = 200, allocationRatioPlanned = 3, stDev = 1.5, normalApproximation = TRUE
	)

	expectedNumberOfSubjectsDiff <- round((x1$expectedNumberOfSubjects - y1$expectedNumberOfSubjects) / 200, 4)

	## Comparison of the results of numeric object 'expectedNumberOfSubjectsDiff' with expected results
	expect_equal(expectedNumberOfSubjectsDiff, c(0.0027, 0.0092, 0.0016, -0.0071, 0.0018, 0.0013), tolerance = 1e-07)

	overallRejectDiff1 <- round(x1$overallReject - y1$overallReject, 4)

	## Comparison of the results of numeric object 'overallRejectDiff1' with expected results
	expect_equal(overallRejectDiff1, c(-0.0018, 0.0015, 2e-04, 0, 0, 0), tolerance = 1e-07)

	futilityStopDiff1 <- round(x1$futilityStop - y1$futilityStop, 4)

	## Comparison of the results of numeric object 'futilityStopDiff1' with expected results
	expect_equal(futilityStopDiff1, c(0.003, -0.0012, -2e-04, 0, 0, 0), tolerance = 1e-07)

	x2 <- getSimulationMeans(
	    seed = 1234, design = getDesignInverseNormal(futilityBounds = c(-0.5, 0.5), informationRates = c(0.2, 0.5, 1)), groups = 2, meanRatio = FALSE, thetaH0 = 0.2,
	    plannedSubjects = c(40, 100, 200), maxNumberOfIterations = 1000, allocationRatioPlanned = 3, stDev = 1.5
	)
	y2 <- getPowerMeans(
	    design = getDesignInverseNormal(futilityBounds = c(-0.5, 0.5), informationRates = c(0.2, 0.5, 1)), groups = 2, meanRatio = FALSE, thetaH0 = 0.2,
	    maxNumberOfSubjects = 200, allocationRatioPlanned = 3, stDev = 1.5, normalApproximation = TRUE
	)
	expectedNumberOfSubjectsDiff <- round((x2$expectedNumberOfSubjects - y2$expectedNumberOfSubjects) / 200, 4)

	## Comparison of the results of numeric object 'expectedNumberOfSubjectsDiff' with expected results
	expect_equal(expectedNumberOfSubjectsDiff, c(-0.0117, 0.0015, -4e-04, 4e-04, -0.0018, 0.0065), tolerance = 1e-07)

	overallRejectDiff2 <- round(x2$overallReject - y2$overallReject, 4)

	## Comparison of the results of numeric object 'overallRejectDiff2' with expected results
	expect_equal(overallRejectDiff2, c(-0.0016, 0.0111, 0.0023, 0.0198, 0.0107, -0.0071), tolerance = 1e-07)

	futilityStopDiff2 <- round(x2$futilityStop - y2$futilityStop, 4)

	## Comparison of the results of numeric object 'futilityStopDiff2' with expected results
	expect_equal(futilityStopDiff2, c(0.0132, -0.0034, 0.0147, -3e-04, 0.0035, 0.0013), tolerance = 1e-07)

	x4 <- getSimulationMeans(
	    seed = 1234, design = getDesignInverseNormal(futilityBounds = c(-0.5, 0.5), informationRates = c(0.2, 0.5, 1)), groups = 1, thetaH0 = 0.2,
	    plannedSubjects = c(40, 100, 200), maxNumberOfIterations = 1000, stDev = 1.5
	)
	y4 <- getPowerMeans(
	    design = getDesignInverseNormal(futilityBounds = c(-0.5, 0.5), informationRates = c(0.2, 0.5, 1)), groups = 1, thetaH0 = 0.2,
	    maxNumberOfSubjects = 200, stDev = 1.5, normalApproximation = TRUE
	)
	expectedNumberOfSubjectsDiff <- round((x4$expectedNumberOfSubjects - y4$expectedNumberOfSubjects) / 200, 4)

	## Comparison of the results of numeric object 'expectedNumberOfSubjectsDiff' with expected results
	expect_equal(expectedNumberOfSubjectsDiff, c(-0.0038, 0.0042, 0.0102, -0.0074, -0.002, -0.0036), tolerance = 1e-07)

	overallRejectDiff4 <- round(x4$overallReject - y4$overallReject, 4)

	## Comparison of the results of numeric object 'overallRejectDiff4' with expected results
	expect_equal(overallRejectDiff4, c(-1e-04, 0.0121, -0.0064, 0.0131, -0.0015, 1e-04), tolerance = 1e-07)

	futilityStopDiff4 <- round(x4$futilityStop - y4$futilityStop, 4)

	## Comparison of the results of numeric object 'futilityStopDiff4' with expected results
	expect_equal(futilityStopDiff4, c(0.0013, -0.0094, -0.0191, -0.007, 0.0016, -1e-04), tolerance = 1e-07)

	x5 <- getSimulationMeans(
	    seed = 1234, design = getDesignInverseNormal(futilityBounds = c(-0.5, 0.5), informationRates = c(0.2, 0.5, 1)), groups = 2, meanRatio = TRUE, thetaH0 = 1.1,
	    plannedSubjects = c(40, 100, 200), maxNumberOfIterations = 1000, allocationRatioPlanned = 3, stDev = 1.5, directionUpper = FALSE
	)
	y5 <- getPowerMeans(
	    design = getDesignInverseNormal(futilityBounds = c(-0.5, 0.5), informationRates = c(0.2, 0.5, 1)), groups = 2, meanRatio = TRUE, thetaH0 = 1.1,
	    maxNumberOfSubjects = 200, allocationRatioPlanned = 3, stDev = 1.5, normalApproximation = TRUE, directionUpper = FALSE
	)
	expectedNumberOfSubjectsDiff <- round((x5$expectedNumberOfSubjects - y5$expectedNumberOfSubjects) / 200, 4)

	## Comparison of the results of numeric object 'expectedNumberOfSubjectsDiff' with expected results
	expect_equal(expectedNumberOfSubjectsDiff, c(0.008, -0.0088, 0.0023, -0.001, -0.0062, -0.0039), tolerance = 1e-07)

	overallRejectDiff5 <- round(x5$overallReject - y5$overallReject, 4)

	## Comparison of the results of numeric object 'overallRejectDiff5' with expected results
	expect_equal(overallRejectDiff5, c(0, -0.0019, -9e-04, -1e-04, 0, 0), tolerance = 1e-07)

	futilityStopDiff5 <- round(x5$futilityStop - y5$futilityStop, 4)

	## Comparison of the results of numeric object 'futilityStopDiff5' with expected results
	expect_equal(futilityStopDiff5, c(-0.0164, 0.0103, 0.0038, 0.0057, 0.0018, 6e-04), tolerance = 1e-07)

	x6 <- getSimulationMeans(
	    seed = 1234, design = getDesignInverseNormal(futilityBounds = c(-0.5, 0.5), informationRates = c(0.2, 0.5, 1)), groups = 2, meanRatio = FALSE, thetaH0 = 1.1,
	    plannedSubjects = c(40, 100, 200), maxNumberOfIterations = 1000, allocationRatioPlanned = 3, stDev = 1.5, directionUpper = FALSE
	)
	y6 <- getPowerMeans(
	    design = getDesignInverseNormal(futilityBounds = c(-0.5, 0.5), informationRates = c(0.2, 0.5, 1)), groups = 2, meanRatio = FALSE, thetaH0 = 1.1,
	    maxNumberOfSubjects = 200, allocationRatioPlanned = 3, stDev = 1.5, normalApproximation = TRUE, directionUpper = FALSE
	)
	expectedNumberOfSubjectsDiff <- round((x6$expectedNumberOfSubjects - y6$expectedNumberOfSubjects) / 200, 4)

	## Comparison of the results of numeric object 'expectedNumberOfSubjectsDiff' with expected results
	expect_equal(expectedNumberOfSubjectsDiff, c(0.0029, -0.0013, 0.0079, 0.023, -0.003, -0.0132), tolerance = 1e-07)

	overallRejectDiff6 <- round(x6$overallReject - y6$overallReject, 4)

	## Comparison of the results of numeric object 'overallRejectDiff6' with expected results
	expect_equal(overallRejectDiff6, c(0.0036, 0.003, -0.0112, -0.0033, -0.0108, -0.0031), tolerance = 1e-07)

	futilityStopDiff6 <- round(x6$futilityStop - y6$futilityStop, 4)

	## Comparison of the results of numeric object 'futilityStopDiff6' with expected results
	expect_equal(futilityStopDiff6, c(-0.004, 2e-04, 0.0083, -0.0213, -4e-04, 0.0232), tolerance = 1e-07)

	x7 <- getSimulationMeans(
	    seed = 1234, design = getDesignInverseNormal(futilityBounds = c(-0.5, 0.5), informationRates = c(0.2, 0.5, 1)), groups = 1, thetaH0 = 0.8,
	    plannedSubjects = c(40, 100, 200), maxNumberOfIterations = 1000, stDev = 1.5, directionUpper = FALSE
	)
	y7 <- getPowerMeans(
	    design = getDesignInverseNormal(futilityBounds = c(-0.5, 0.5), informationRates = c(0.2, 0.5, 1)), groups = 1, thetaH0 = 0.8,
	    maxNumberOfSubjects = 200, stDev = 1.5, normalApproximation = TRUE, directionUpper = FALSE
	)
	expectedNumberOfSubjectsDiff <- round((x7$expectedNumberOfSubjects - y7$expectedNumberOfSubjects) / 200, 4)

	## Comparison of the results of numeric object 'expectedNumberOfSubjectsDiff' with expected results
	expect_equal(expectedNumberOfSubjectsDiff, c(0.0012, 6e-04, -0.0061, -3e-04, 0.0091, 0.0036), tolerance = 1e-07)

	overallRejectDiff7 <- round(x7$overallReject - y7$overallReject, 4)

	## Comparison of the results of numeric object 'overallRejectDiff7' with expected results
	expect_equal(overallRejectDiff7, c(1e-04, 5e-04, -9e-04, -0.0224, -9e-04, -1e-04), tolerance = 1e-07)

	futilityStopDiff7 <- round(x7$futilityStop - y7$futilityStop, 4)

	## Comparison of the results of numeric object 'futilityStopDiff7' with expected results
	expect_equal(futilityStopDiff7, c(-1e-04, -4e-04, -0.003, 0.0059, -4e-04, 0.0033), tolerance = 1e-07)
})

test_that("Internal simulation base means functions throw errors when arguments are missing or wrong", {
    expect_error(.getSimulationMeansStageSubjects())
})
