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
## |  File name: test-f_simulation_multiarm_rates.R
## |  Creation date: 12 August 2022, 09:12:28
## |  File version: $Revision: 6658 $
## |  Last changed: $Date: 2022-11-04 10:30:20 +0100 (Fr, 04 Nov 2022) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing Simulation Multi-Arm Rates Function")


test_that("'getSimulationMultiArmRates': several configurations", {
        
    .skipTestIfDisabled()
    .skipTestIfNotX64()
    
	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
	# @refFS[Tab.]{fs:tab:output:getSimulationMultiArmRates}
	# @refFS[Formula]{fs:simulationMultiArmDoseResponse}
	# @refFS[Formula]{fs:simulationMultiArmRatesGenerate}
	# @refFS[Formula]{fs:simulationMultiArmRatesTestStatistics}
	# @refFS[Formula]{fs:simulationMultiArmSelections}
	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	x1 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
	    typeOfShape = "linear", activeArms = 4,
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x1' with expected results
	expect_equal(x1$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x1$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x1$iterations[3, ], c(10, 10, 9, 7))
	expect_equal(x1$rejectAtLeastOne, c(0, 0.1, 0.6, 0.8), tolerance = 1e-07)
	expect_equal(unlist(as.list(x1$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.2, 0, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x1$futilityStop, c(0, 0, 0, 0))
	expect_equal(x1$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x1$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x1$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x1$earlyStop[2, ], c(0, 0, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x1$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x1$successPerStage[2, ], c(0, 0, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x1$successPerStage[3, ], c(0, 0.1, 0.5, 0.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(x1$selectedArms)), c(1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0, 0, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0, 0, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.3, 0.2, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.4, 0.3, 1, 0.5, 0.3, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.7), tolerance = 1e-07)
	expect_equal(x1$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x1$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x1$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(x1$expectedNumberOfSubjects, c(334.8, 445, 331.8, 179.8), tolerance = 1e-07)
	expect_equal(unlist(as.list(x1$sampleSizes)), c(10, 8, 10.4, 10, 10, 10, 10, 11.3, 15.333333, 10, 0, 0, 10, 10, 10, 10, 17.5, 20, 10, 0, 0, 10, 13, 19.142857, 10, 22.4, 22.5, 10, 40, 40, 10, 37.5, 36.555556, 10, 4.4, 8.5714286, 10, 20.4, 38.7, 10, 30, 30, 10, 28.2, 19.111111, 10, 17.1, 15.714286, 10, 60.8, 81.6, 10, 97.5, 100, 10, 77, 71, 10, 34.5, 43.428571), tolerance = 1e-07)
	expect_equal(x1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$conditionalPowerAchieved[2, ], c(0.032197948, 0.00019444487, 0.052129075, 0.12394528), tolerance = 1e-07)
	expect_equal(x1$conditionalPowerAchieved[3, ], c(0.33607045, 0.04525892, 0.4023749, 0.68738904), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	    x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x1CodeBased$iterations, x1$iterations, tolerance = 1e-05)
	    expect_equal(x1CodeBased$rejectAtLeastOne, x1$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x1CodeBased$rejectedArmsPerStage, x1$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x1CodeBased$futilityStop, x1$futilityStop, tolerance = 1e-05)
	    expect_equal(x1CodeBased$futilityPerStage, x1$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x1CodeBased$earlyStop, x1$earlyStop, tolerance = 1e-05)
	    expect_equal(x1CodeBased$successPerStage, x1$successPerStage, tolerance = 1e-05)
	    expect_equal(x1CodeBased$selectedArms, x1$selectedArms, tolerance = 1e-05)
	    expect_equal(x1CodeBased$numberOfActiveArms, x1$numberOfActiveArms, tolerance = 1e-05)
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
    
	x2 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
	    typeOfShape = "userDefined", activeArms = 4,
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, adaptations = rep(TRUE, 2),
	    effectMatrix = matrix(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.5), ncol = 4),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x2' with expected results
	expect_equal(x2$iterations[1, ], c(10, 10))
	expect_equal(x2$iterations[2, ], c(10, 10))
	expect_equal(x2$iterations[3, ], c(10, 8))
	expect_equal(x2$rejectAtLeastOne, c(0.2, 0.7), tolerance = 1e-07)
	expect_equal(unlist(as.list(x2$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0.2, 0.5), tolerance = 1e-07)
	expect_equal(x2$futilityStop, c(0, 0))
	expect_equal(x2$futilityPerStage[1, ], c(0, 0))
	expect_equal(x2$futilityPerStage[2, ], c(0, 0))
	expect_equal(x2$earlyStop[1, ], c(0, 0))
	expect_equal(x2$earlyStop[2, ], c(0, 0.2), tolerance = 1e-07)
	expect_equal(x2$successPerStage[1, ], c(0, 0))
	expect_equal(x2$successPerStage[2, ], c(0, 0.2), tolerance = 1e-07)
	expect_equal(x2$successPerStage[3, ], c(0.2, 0.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(x2$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0, 0, 1, 0.5, 0.5, 1, 0.7, 0.5, 1, 1, 1, 1, 1, 0.8), tolerance = 1e-07)
	expect_equal(x2$numberOfActiveArms[1, ], c(4, 4))
	expect_equal(x2$numberOfActiveArms[2, ], c(1, 1))
	expect_equal(x2$numberOfActiveArms[3, ], c(1, 1))
	expect_equal(x2$expectedNumberOfSubjects, c(397.2, 312.8), tolerance = 1e-07)
	expect_equal(unlist(as.list(x2$sampleSizes)), c(10, 0, 0, 10, 0, 0, 10, 30, 30, 10, 22.4, 37.5, 10, 13, 20, 10, 0, 0, 10, 38.8, 41.8, 10, 52.8, 32.75, 10, 81.8, 91.8, 10, 75.2, 70.25), tolerance = 1e-07)
	expect_equal(x2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_))
	expect_equal(x2$conditionalPowerAchieved[2, ], c(0.0097327907, 0.021741893), tolerance = 1e-07)
	expect_equal(x2$conditionalPowerAchieved[3, ], c(0.14656813, 0.35197865), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	    x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x2CodeBased$iterations, x2$iterations, tolerance = 1e-05)
	    expect_equal(x2CodeBased$rejectAtLeastOne, x2$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x2CodeBased$rejectedArmsPerStage, x2$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x2CodeBased$futilityStop, x2$futilityStop, tolerance = 1e-05)
	    expect_equal(x2CodeBased$futilityPerStage, x2$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x2CodeBased$earlyStop, x2$earlyStop, tolerance = 1e-05)
	    expect_equal(x2CodeBased$successPerStage, x2$successPerStage, tolerance = 1e-05)
	    expect_equal(x2CodeBased$selectedArms, x2$selectedArms, tolerance = 1e-05)
	    expect_equal(x2CodeBased$numberOfActiveArms, x2$numberOfActiveArms, tolerance = 1e-05)
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

	x3 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
	    typeOfShape = "sigmoidEmax", gED50 = 2, slope = 0.5, activeArms = 4,
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x3' with expected results
	expect_equal(x3$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x3$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x3$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(x3$rejectAtLeastOne, c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x3$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x3$futilityStop, c(0, 0, 0, 0))
	expect_equal(x3$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x3$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x3$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x3$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x3$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x3$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x3$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x3$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), tolerance = 1e-07)
	expect_equal(x3$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x3$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x3$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(x3$expectedNumberOfSubjects, c(434.8, 402, 440, 425), tolerance = 1e-07)
	expect_equal(unlist(as.list(x3$sampleSizes)), c(10, 0, 0, 10, 0, 0, 10, 15, 20, 10, 10, 10, 10, 30, 30, 10, 12.7, 20, 10, 40, 40, 10, 20, 20, 10, 30, 30, 10, 29.1, 34.2, 10, 30, 30, 10, 33.4, 40, 10, 32.4, 40, 10, 40, 40, 10, 10, 10, 10, 26.7, 27.4, 10, 92.4, 100, 10, 81.8, 94.2, 10, 95, 100, 10, 90.1, 97.4), tolerance = 1e-07)
	expect_equal(x3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$conditionalPowerAchieved[2, ], c(0.0098526063, 0.0022619481, 0.010226943, 0.0071111057), tolerance = 1e-07)
	expect_equal(x3$conditionalPowerAchieved[3, ], c(0.00025317548, 0.089328639, 4.5501958e-05, 0.12015791), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	    x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x3CodeBased$iterations, x3$iterations, tolerance = 1e-05)
	    expect_equal(x3CodeBased$rejectAtLeastOne, x3$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x3CodeBased$rejectedArmsPerStage, x3$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x3CodeBased$futilityStop, x3$futilityStop, tolerance = 1e-05)
	    expect_equal(x3CodeBased$futilityPerStage, x3$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x3CodeBased$earlyStop, x3$earlyStop, tolerance = 1e-05)
	    expect_equal(x3CodeBased$successPerStage, x3$successPerStage, tolerance = 1e-05)
	    expect_equal(x3CodeBased$selectedArms, x3$selectedArms, tolerance = 1e-05)
	    expect_equal(x3CodeBased$numberOfActiveArms, x3$numberOfActiveArms, tolerance = 1e-05)
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

	x4 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
	    activeArms = 4, typeOfSelection = "all",
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x4' with expected results
	expect_equal(x4$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x4$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x4$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(x4$rejectAtLeastOne, c(0, 0.3, 0.7, 1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x4$rejectedArmsPerStage)), c(0, 0, 0, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.3, 0, 0.3, 0.4, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.3, 0, 0.6, 0.2, 0, 0, 0, 0, 0, 0.2, 0, 0.4, 0.3, 0, 0.8, 0.2), tolerance = 1e-07)
	expect_equal(x4$futilityStop, c(0, 0, 0, 0))
	expect_equal(x4$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x4$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x4$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x4$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x4$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x4$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x4$successPerStage[3, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x4$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
	expect_equal(x4$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x4$numberOfActiveArms[2, ], c(4, 4, 4, 4))
	expect_equal(x4$numberOfActiveArms[3, ], c(4, 4, 4, 4))
	expect_equal(x4$expectedNumberOfSubjects, c(1026, 1002, 924.5, 714.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(x4$sampleSizes)), c(10, 95.2, 100, 10, 100, 90.4, 10, 91.8, 83.1, 10, 100, 32.9, 10, 95.2, 100, 10, 100, 90.4, 10, 91.8, 83.1, 10, 100, 32.9, 10, 95.2, 100, 10, 100, 90.4, 10, 91.8, 83.1, 10, 100, 32.9, 10, 95.2, 100, 10, 100, 90.4, 10, 91.8, 83.1, 10, 100, 32.9, 10, 95.2, 100, 10, 100, 90.4, 10, 91.8, 83.1, 10, 100, 32.9), tolerance = 1e-07)
	expect_equal(x4$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$conditionalPowerAchieved[2, ], c(0.16336896, 3.7379108e-06, 0.18421481, 0.069788183), tolerance = 1e-07)
	expect_equal(x4$conditionalPowerAchieved[3, ], c(0.00052547754, 0.089531131, 0.32040425, 0.67566016), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	    x4CodeBased <- eval(parse(text = getObjectRCode(x4, stringWrapParagraphWidth = NULL)))
	    expect_equal(x4CodeBased$iterations, x4$iterations, tolerance = 1e-05)
	    expect_equal(x4CodeBased$rejectAtLeastOne, x4$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x4CodeBased$rejectedArmsPerStage, x4$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x4CodeBased$futilityStop, x4$futilityStop, tolerance = 1e-05)
	    expect_equal(x4CodeBased$futilityPerStage, x4$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x4CodeBased$earlyStop, x4$earlyStop, tolerance = 1e-05)
	    expect_equal(x4CodeBased$successPerStage, x4$successPerStage, tolerance = 1e-05)
	    expect_equal(x4CodeBased$selectedArms, x4$selectedArms, tolerance = 1e-05)
	    expect_equal(x4CodeBased$numberOfActiveArms, x4$numberOfActiveArms, tolerance = 1e-05)
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

	x5 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
	    activeArms = 4, typeOfSelection = "rBest", rValue = 2,
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x5' with expected results
	expect_equal(x5$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x5$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x5$iterations[3, ], c(10, 10, 8, 6))
	expect_equal(x5$rejectAtLeastOne, c(0, 0.3, 0.9, 1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x5$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0.1, 0, 0.2, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0.2, 0, 0.3, 0.1, 0, 0, 0, 0, 0, 0.2, 0, 0.3, 0.4, 0, 0.8, 0.1), tolerance = 1e-07)
	expect_equal(x5$futilityStop, c(0, 0, 0, 0))
	expect_equal(x5$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x5$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x5$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x5$earlyStop[2, ], c(0, 0, 0.2, 0.4), tolerance = 1e-07)
	expect_equal(x5$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x5$successPerStage[2, ], c(0, 0, 0.2, 0.4), tolerance = 1e-07)
	expect_equal(x5$successPerStage[3, ], c(0, 0, 0.3, 0.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x5$selectedArms)), c(1, 0.8, 0.8, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.4, 0.2, 1, 0.3, 0.1, 1, 0.6, 0.6, 1, 0.5, 0.5, 1, 0.6, 0.5, 1, 0.6, 0.3, 1, 0.5, 0.5, 1, 0.8, 0.8, 1, 0.8, 0.7, 1, 0.9, 0.6, 1, 1, 1, 1, 1, 1, 1, 1, 0.8, 1, 1, 0.6), tolerance = 1e-07)
	expect_equal(x5$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x5$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x5$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(x5$expectedNumberOfSubjects, c(642.8, 566.9, 399.8, 265.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x5$sampleSizes)), c(10, 77.6, 80, 10, 23.9, 30, 10, 20, 1, 10, 12.3, 1.3333333, 10, 10, 10, 10, 30.2, 28.6, 10, 28.6, 25, 10, 20, 3.1666667, 10, 60, 60, 10, 49.7, 41.1, 10, 37.4, 28.25, 10, 40.8, 9.8333333, 10, 47.6, 50, 10, 63.8, 77.3, 10, 61.2, 53.25, 10, 53.1, 14.333333, 10, 97.6, 100, 10, 83.8, 88.5, 10, 73.6, 53.75, 10, 63.1, 14.333333), tolerance = 1e-07)
	expect_equal(x5$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$conditionalPowerAchieved[2, ], c(0.080486965, 0.12759682, 0.10458054, 0.065420449), tolerance = 1e-07)
	expect_equal(x5$conditionalPowerAchieved[3, ], c(0.022470074, 0.31122739, 0.58569198, 0.85520318), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x5), NA)))
	    expect_output(print(x5)$show())
	    invisible(capture.output(expect_error(summary(x5), NA)))
	    expect_output(summary(x5)$show())
	    x5CodeBased <- eval(parse(text = getObjectRCode(x5, stringWrapParagraphWidth = NULL)))
	    expect_equal(x5CodeBased$iterations, x5$iterations, tolerance = 1e-05)
	    expect_equal(x5CodeBased$rejectAtLeastOne, x5$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x5CodeBased$rejectedArmsPerStage, x5$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x5CodeBased$futilityStop, x5$futilityStop, tolerance = 1e-05)
	    expect_equal(x5CodeBased$futilityPerStage, x5$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x5CodeBased$earlyStop, x5$earlyStop, tolerance = 1e-05)
	    expect_equal(x5CodeBased$successPerStage, x5$successPerStage, tolerance = 1e-05)
	    expect_equal(x5CodeBased$selectedArms, x5$selectedArms, tolerance = 1e-05)
	    expect_equal(x5CodeBased$numberOfActiveArms, x5$numberOfActiveArms, tolerance = 1e-05)
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

	x6 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
	    activeArms = 4, typeOfSelection = "epsilon", epsilonValue = 0.1,
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x6' with expected results
	expect_equal(x6$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x6$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x6$iterations[3, ], c(10, 10, 8, 7))
	expect_equal(x6$rejectAtLeastOne, c(0, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x6$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.2, 0, 0.3, 0.4, 0, 0, 0, 0, 0, 0.4, 0, 0.2, 0.5, 0, 0.4, 0.5), tolerance = 1e-07)
	expect_equal(x6$futilityStop, c(0, 0, 0, 0))
	expect_equal(x6$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x6$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x6$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x6$earlyStop[2, ], c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x6$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x6$successPerStage[2, ], c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x6$successPerStage[3, ], c(0, 0.4, 0.6, 0.6), tolerance = 1e-07)
	expect_equal(unlist(as.list(x6$selectedArms)), c(1, 0.2, 0.2, 1, 0.4, 0.1, 1, 0.3, 0.1, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.1, 0, 1, 0.2, 0.1, 1, 0.2, 0, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.3, 0.2, 1, 0.9, 0.4, 1, 0.3, 0.3, 1, 0.7, 0.7, 1, 0.9, 0.7, 1, 0.9, 0.6, 1, 1, 1, 1, 1, 1, 1, 1, 0.8, 1, 1, 0.7), tolerance = 1e-07)
	expect_equal(x6$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x6$numberOfActiveArms[2, ], c(1.3, 1.6, 1.7, 2.1), tolerance = 1e-07)
	expect_equal(x6$numberOfActiveArms[3, ], c(1.3, 1.2, 1.375, 1.5714286), tolerance = 1e-07)
	expect_equal(x6$expectedNumberOfSubjects, c(436.4, 438.6, 346.7, 372.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(x6$sampleSizes)), c(10, 16.7, 20, 10, 27.9, 10, 10, 9.1, 12.5, 10, 1.2, 14.285714, 10, 37.5, 40, 10, 1.2, 0, 10, 12.3, 11.625, 10, 7.9, 0, 10, 32.4, 32.5, 10, 31.2, 40, 10, 21.5, 13.375, 10, 63.2, 50.142857, 10, 15.4, 28.7, 10, 56.2, 59, 10, 60.4, 63, 10, 58, 51.714286, 10, 72, 91.2, 10, 74.1, 89, 10, 61.9, 63.875, 10, 64.7, 66), tolerance = 1e-07)
	expect_equal(x6$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$conditionalPowerAchieved[2, ], c(0.031688257, 0.035836944, 0.12967885, 0.10427074), tolerance = 1e-07)
	expect_equal(x6$conditionalPowerAchieved[3, ], c(0.2491354, 0.21222327, 0.47711159, 0.3978836), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x6), NA)))
	    expect_output(print(x6)$show())
	    invisible(capture.output(expect_error(summary(x6), NA)))
	    expect_output(summary(x6)$show())
	    x6CodeBased <- eval(parse(text = getObjectRCode(x6, stringWrapParagraphWidth = NULL)))
	    expect_equal(x6CodeBased$iterations, x6$iterations, tolerance = 1e-05)
	    expect_equal(x6CodeBased$rejectAtLeastOne, x6$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x6CodeBased$rejectedArmsPerStage, x6$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x6CodeBased$futilityStop, x6$futilityStop, tolerance = 1e-05)
	    expect_equal(x6CodeBased$futilityPerStage, x6$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x6CodeBased$earlyStop, x6$earlyStop, tolerance = 1e-05)
	    expect_equal(x6CodeBased$successPerStage, x6$successPerStage, tolerance = 1e-05)
	    expect_equal(x6CodeBased$selectedArms, x6$selectedArms, tolerance = 1e-05)
	    expect_equal(x6CodeBased$numberOfActiveArms, x6$numberOfActiveArms, tolerance = 1e-05)
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

	x7 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)),
	    activeArms = 4,
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x7' with expected results
	expect_equal(x7$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x7$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x7$iterations[3, ], c(10, 9, 8, 5))
	expect_equal(x7$rejectAtLeastOne, c(0, 0.4, 0.5, 1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x7$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0, 0.1, 0.2, 0, 0, 0, 0, 0.1, 0.2, 0, 0.2, 0, 0, 0.4, 0.3), tolerance = 1e-07)
	expect_equal(x7$futilityStop, c(0, 0, 0, 0))
	expect_equal(x7$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x7$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x7$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x7$earlyStop[2, ], c(0, 0.1, 0.2, 0.5), tolerance = 1e-07)
	expect_equal(x7$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x7$successPerStage[2, ], c(0, 0.1, 0.2, 0.5), tolerance = 1e-07)
	expect_equal(x7$successPerStage[3, ], c(0, 0.3, 0.3, 0.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(x7$selectedArms)), c(1, 0.2, 0.2, 1, 0, 0, 1, 0.2, 0.2, 1, 0, 0, 1, 0.3, 0.3, 1, 0, 0, 1, 0.2, 0.2, 1, 0, 0, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.3, 0.2, 1, 0.3, 0.3, 1, 0.6, 0.5, 1, 0.4, 0.2, 1, 0.7, 0.3, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 0.5), tolerance = 1e-07)
	expect_equal(x7$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x7$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x7$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(x7$expectedNumberOfSubjects, c(355.2, 334, 233, 193.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x7$sampleSizes)), c(10, 20, 20, 10, 0, 0, 10, 20, 25, 10, 0, 0, 10, 30, 30, 10, 0, 0, 10, 3, 3.75, 10, 0, 0, 10, 12.4, 12.4, 10, 20.7, 22.777778, 10, 15, 18.75, 10, 16.2, 26.4, 10, 13.9, 13.9, 10, 54.2, 51.777778, 10, 13.1, 3, 10, 30.3, 24, 10, 76.3, 76.3, 10, 74.9, 74.555556, 10, 51.1, 50.5, 10, 46.5, 50.4), tolerance = 1e-07)
	expect_equal(x7$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$conditionalPowerAchieved[2, ], c(0.035427106, 0.012436575, 0.08338715, 0.046283385), tolerance = 1e-07)
	expect_equal(x7$conditionalPowerAchieved[3, ], c(0.076058567, 0.27636533, 0.46741694, 0.70493817), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x7), NA)))
	    expect_output(print(x7)$show())
	    invisible(capture.output(expect_error(summary(x7), NA)))
	    expect_output(summary(x7)$show())
	    x7CodeBased <- eval(parse(text = getObjectRCode(x7, stringWrapParagraphWidth = NULL)))
	    expect_equal(x7CodeBased$iterations, x7$iterations, tolerance = 1e-05)
	    expect_equal(x7CodeBased$rejectAtLeastOne, x7$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x7CodeBased$rejectedArmsPerStage, x7$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x7CodeBased$futilityStop, x7$futilityStop, tolerance = 1e-05)
	    expect_equal(x7CodeBased$futilityPerStage, x7$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x7CodeBased$earlyStop, x7$earlyStop, tolerance = 1e-05)
	    expect_equal(x7CodeBased$successPerStage, x7$successPerStage, tolerance = 1e-05)
	    expect_equal(x7CodeBased$selectedArms, x7$selectedArms, tolerance = 1e-05)
	    expect_equal(x7CodeBased$numberOfActiveArms, x7$numberOfActiveArms, tolerance = 1e-05)
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

	x8 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
	    activeArms = 4, typeOfSelection = "all",
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x8' with expected results
	expect_equal(x8$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x8$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x8$iterations[3, ], c(10, 10, 9, 8))
	expect_equal(x8$rejectAtLeastOne, c(0, 0.2, 0.9, 1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x8$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0.2, 0, 0.1, 0.3, 0, 0.2, 0.4, 0, 0, 0, 0, 0, 0.1, 0, 0.3, 0.3, 0.1, 0.4, 0.5, 0, 0, 0, 0, 0, 0.1, 0, 0.4, 0.5, 0.1, 0.8, 0.1), tolerance = 1e-07)
	expect_equal(x8$futilityStop, c(0, 0, 0, 0))
	expect_equal(x8$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x8$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x8$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x8$earlyStop[2, ], c(0, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x8$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x8$successPerStage[2, ], c(0, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x8$successPerStage[3, ], c(0, 0, 0.1, 0), tolerance = 1e-07)
	expect_equal(unlist(as.list(x8$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.8), tolerance = 1e-07)
	expect_equal(x8$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x8$numberOfActiveArms[2, ], c(4, 4, 4, 4))
	expect_equal(x8$numberOfActiveArms[3, ], c(4, 4, 4, 4))
	expect_equal(x8$expectedNumberOfSubjects, c(952, 1050, 909.5, 860), tolerance = 1e-07)
	expect_equal(unlist(as.list(x8$sampleSizes)), c(10, 90.2, 90.2, 10, 100, 100, 10, 91, 89.888889, 10, 91, 88.75, 10, 90.2, 90.2, 10, 100, 100, 10, 91, 89.888889, 10, 91, 88.75, 10, 90.2, 90.2, 10, 100, 100, 10, 91, 89.888889, 10, 91, 88.75, 10, 90.2, 90.2, 10, 100, 100, 10, 91, 89.888889, 10, 91, 88.75, 10, 90.2, 90.2, 10, 100, 100, 10, 91, 89.888889, 10, 91, 88.75), tolerance = 1e-07)
	expect_equal(x8$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$conditionalPowerAchieved[2, ], c(0.16068828, 0.022112719, 0.21849189, 0.19646842), tolerance = 1e-07)
	expect_equal(x8$conditionalPowerAchieved[3, ], c(0.0018216452, 0.044801331, 0.47086458, 0.69046124), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x8), NA)))
	    expect_output(print(x8)$show())
	    invisible(capture.output(expect_error(summary(x8), NA)))
	    expect_output(summary(x8)$show())
	    x8CodeBased <- eval(parse(text = getObjectRCode(x8, stringWrapParagraphWidth = NULL)))
	    expect_equal(x8CodeBased$iterations, x8$iterations, tolerance = 1e-05)
	    expect_equal(x8CodeBased$rejectAtLeastOne, x8$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x8CodeBased$rejectedArmsPerStage, x8$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x8CodeBased$futilityStop, x8$futilityStop, tolerance = 1e-05)
	    expect_equal(x8CodeBased$futilityPerStage, x8$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x8CodeBased$earlyStop, x8$earlyStop, tolerance = 1e-05)
	    expect_equal(x8CodeBased$successPerStage, x8$successPerStage, tolerance = 1e-05)
	    expect_equal(x8CodeBased$selectedArms, x8$selectedArms, tolerance = 1e-05)
	    expect_equal(x8CodeBased$numberOfActiveArms, x8$numberOfActiveArms, tolerance = 1e-05)
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

	x9 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
	    activeArms = 4, typeOfSelection = "rBest", rValue = 2,
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x9' with expected results
	expect_equal(x9$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x9$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x9$iterations[3, ], c(10, 10, 10, 5))
	expect_equal(x9$rejectAtLeastOne, c(0, 0.2, 0.7, 1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x9$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0.1, 0.1, 0.1, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0.2, 0, 0.4, 0.2, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.3, 0.1, 0.6, 0.1), tolerance = 1e-07)
	expect_equal(x9$futilityStop, c(0, 0, 0, 0))
	expect_equal(x9$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x9$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x9$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x9$earlyStop[2, ], c(0, 0, 0, 0.5), tolerance = 1e-07)
	expect_equal(x9$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x9$successPerStage[2, ], c(0, 0, 0, 0.5), tolerance = 1e-07)
	expect_equal(x9$successPerStage[3, ], c(0, 0.1, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x9$selectedArms)), c(1, 0.6, 0.6, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.2, 0.1, 1, 0.7, 0.7, 1, 0.2, 0.2, 1, 0.5, 0.5, 1, 0.3, 0.2, 1, 0.5, 0.5, 1, 0.7, 0.7, 1, 0.6, 0.6, 1, 0.6, 0.3, 1, 0.2, 0.2, 1, 0.7, 0.7, 1, 0.6, 0.6, 1, 0.9, 0.4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5), tolerance = 1e-07)
	expect_equal(x9$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x9$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x9$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(x9$expectedNumberOfSubjects, c(603.2, 605.9, 453.2, 361.7), tolerance = 1e-07)
	expect_equal(unlist(as.list(x9$sampleSizes)), c(10, 52.2, 52.2, 10, 33.6, 33.5, 10, 21.2, 21.2, 10, 9.2, 17.6, 10, 70, 70, 10, 20, 20, 10, 35.6, 35.3, 10, 19.7, 21.4, 10, 45.3, 45.3, 10, 62.7, 62.6, 10, 36.2, 35.8, 10, 52.8, 45.4, 10, 16.9, 16.9, 10, 69.1, 69.1, 10, 41.8, 41.7, 10, 61.7, 44.4, 10, 92.2, 92.2, 10, 92.7, 92.6, 10, 67.4, 67, 10, 71.7, 64.4), tolerance = 1e-07)
	expect_equal(x9$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x9$conditionalPowerAchieved[2, ], c(0.083443128, 0.076003514, 0.14647721, 0.085145955), tolerance = 1e-07)
	expect_equal(x9$conditionalPowerAchieved[3, ], c(0.043093175, 0.13127607, 0.3479275, 0.64693149), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x9), NA)))
	    expect_output(print(x9)$show())
	    invisible(capture.output(expect_error(summary(x9), NA)))
	    expect_output(summary(x9)$show())
	    x9CodeBased <- eval(parse(text = getObjectRCode(x9, stringWrapParagraphWidth = NULL)))
	    expect_equal(x9CodeBased$iterations, x9$iterations, tolerance = 1e-05)
	    expect_equal(x9CodeBased$rejectAtLeastOne, x9$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x9CodeBased$rejectedArmsPerStage, x9$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x9CodeBased$futilityStop, x9$futilityStop, tolerance = 1e-05)
	    expect_equal(x9CodeBased$futilityPerStage, x9$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x9CodeBased$earlyStop, x9$earlyStop, tolerance = 1e-05)
	    expect_equal(x9CodeBased$successPerStage, x9$successPerStage, tolerance = 1e-05)
	    expect_equal(x9CodeBased$selectedArms, x9$selectedArms, tolerance = 1e-05)
	    expect_equal(x9CodeBased$numberOfActiveArms, x9$numberOfActiveArms, tolerance = 1e-05)
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

	x10 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
	    activeArms = 4, typeOfSelection = "epsilon", epsilonValue = 0.1,
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x10' with expected results
	expect_equal(x10$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x10$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x10$iterations[3, ], c(10, 9, 7, 6))
	expect_equal(x10$rejectAtLeastOne, c(0, 0.2, 0.6, 0.6), tolerance = 1e-07)
	expect_equal(unlist(as.list(x10$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0.1, 0, 0.1, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0.1, 0, 0.3, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.3, 0, 0, 0.3, 0.1), tolerance = 1e-07)
	expect_equal(x10$futilityStop, c(0, 0, 0, 0))
	expect_equal(x10$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x10$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x10$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x10$earlyStop[2, ], c(0, 0.1, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x10$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x10$successPerStage[2, ], c(0, 0.1, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x10$successPerStage[3, ], c(0, 0, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x10$selectedArms)), c(1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.3, 0.2, 1, 0.3, 0.2, 1, 0.5, 0.4, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.5, 0.3, 1, 0.4, 0.4, 1, 0.7, 0.6, 1, 0.5, 0.3, 1, 0.5, 0.3, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.7, 1, 1, 0.6), tolerance = 1e-07)
	expect_equal(x10$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x10$numberOfActiveArms[2, ], c(1.2, 1.8, 1.5, 1.6), tolerance = 1e-07)
	expect_equal(x10$numberOfActiveArms[3, ], c(1.2, 1.7777778, 1.7142857, 1.8333333), tolerance = 1e-07)
	expect_equal(x10$expectedNumberOfSubjects, c(313.2, 474, 363.7, 263.7), tolerance = 1e-07)
	expect_equal(unlist(as.list(x10$sampleSizes)), c(10, 15.9, 15.8, 10, 35.9, 39.777778, 10, 12.7, 18, 10, 2.8, 4.6666667, 10, 22.2, 22.2, 10, 30, 22.222222, 10, 22.7, 28.571429, 10, 27.4, 43.166667, 10, 18.1, 18, 10, 32.8, 36.444444, 10, 38.6, 54.857143, 10, 26.7, 26.5, 10, 15.8, 15.8, 10, 54.9, 49.777778, 10, 37.3, 24.571429, 10, 24.9, 23.666667, 10, 59.8, 59.6, 10, 73.6, 70.444444, 10, 68.6, 65.142857, 10, 43, 50.166667), tolerance = 1e-07)
	expect_equal(x10$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x10$conditionalPowerAchieved[2, ], c(0.067103341, 0.011749166, 0.024807536, 0.13720867), tolerance = 1e-07)
	expect_equal(x10$conditionalPowerAchieved[3, ], c(0.10265269, 0.46661697, 0.4198773, 0.2422132), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x10), NA)))
	    expect_output(print(x10)$show())
	    invisible(capture.output(expect_error(summary(x10), NA)))
	    expect_output(summary(x10)$show())
	    x10CodeBased <- eval(parse(text = getObjectRCode(x10, stringWrapParagraphWidth = NULL)))
	    expect_equal(x10CodeBased$iterations, x10$iterations, tolerance = 1e-05)
	    expect_equal(x10CodeBased$rejectAtLeastOne, x10$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x10CodeBased$rejectedArmsPerStage, x10$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x10CodeBased$futilityStop, x10$futilityStop, tolerance = 1e-05)
	    expect_equal(x10CodeBased$futilityPerStage, x10$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x10CodeBased$earlyStop, x10$earlyStop, tolerance = 1e-05)
	    expect_equal(x10CodeBased$successPerStage, x10$successPerStage, tolerance = 1e-05)
	    expect_equal(x10CodeBased$selectedArms, x10$selectedArms, tolerance = 1e-05)
	    expect_equal(x10CodeBased$numberOfActiveArms, x10$numberOfActiveArms, tolerance = 1e-05)
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

	x11 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
	    activeArms = 4, threshold = 0,
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.1, 0.3, 0.1), intersectionTest = "Bonferroni",
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100), directionUpper = FALSE,
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x11' with expected results
	expect_equal(x11$iterations[1, ], c(10, 10, 10))
	expect_equal(x11$iterations[2, ], c(8, 5, 9))
	expect_equal(x11$iterations[3, ], c(4, 4, 6))
	expect_equal(x11$rejectAtLeastOne, c(0.4, 0, 0), tolerance = 1e-07)
	expect_equal(unlist(as.list(x11$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x11$futilityStop, c(0.2, 0.6, 0.4), tolerance = 1e-07)
	expect_equal(x11$futilityPerStage[1, ], c(0.2, 0.5, 0.1), tolerance = 1e-07)
	expect_equal(x11$futilityPerStage[2, ], c(0, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x11$earlyStop[1, ], c(0.2, 0.5, 0.1), tolerance = 1e-07)
	expect_equal(x11$earlyStop[2, ], c(0.4, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x11$successPerStage[1, ], c(0, 0, 0))
	expect_equal(x11$successPerStage[2, ], c(0.4, 0, 0), tolerance = 1e-07)
	expect_equal(x11$successPerStage[3, ], c(0, 0, 0))
	expect_equal(unlist(as.list(x11$selectedArms)), c(1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.3, 0.1, 1, 0.2, 0.2, 1, 0.3, 0.2, 1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0, 0, 1, 0.2, 0.2, 1, 0.3, 0, 1, 0.1, 0.1, 1, 0.3, 0.2, 1, 0.8, 0.4, 1, 0.5, 0.4, 1, 0.9, 0.6), tolerance = 1e-07)
	expect_equal(x11$numberOfActiveArms[1, ], c(4, 4, 4))
	expect_equal(x11$numberOfActiveArms[2, ], c(1, 1, 1))
	expect_equal(x11$numberOfActiveArms[3, ], c(1, 1, 1))
	expect_equal(x11$expectedNumberOfSubjects, c(200.6, 150, 279), tolerance = 1e-07)
	expect_equal(unlist(as.list(x11$sampleSizes)), c(10, 1, 3.25, 10, 2.8, 25, 10, 17.333333, 15.666667, 10, 6.25, 50, 10, 23.2, 32, 10, 11.111111, 16.666667, 10, 14.5, 21, 10, 0, 0, 10, 15.777778, 33.333333, 10, 35.25, 0, 10, 8.4, 25, 10, 17, 33.333333, 10, 57, 74.25, 10, 34.4, 82, 10, 61.222222, 99), tolerance = 1e-07)
	expect_equal(x11$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x11$conditionalPowerAchieved[2, ], c(0.10402635, 0.15240707, 0.070533409), tolerance = 1e-07)
	expect_equal(x11$conditionalPowerAchieved[3, ], c(0.68219789, 0.38677479, 0.34246832), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x11), NA)))
	    expect_output(print(x11)$show())
	    invisible(capture.output(expect_error(summary(x11), NA)))
	    expect_output(summary(x11)$show())
	    x11CodeBased <- eval(parse(text = getObjectRCode(x11, stringWrapParagraphWidth = NULL)))
	    expect_equal(x11CodeBased$iterations, x11$iterations, tolerance = 1e-05)
	    expect_equal(x11CodeBased$rejectAtLeastOne, x11$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x11CodeBased$rejectedArmsPerStage, x11$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x11CodeBased$futilityStop, x11$futilityStop, tolerance = 1e-05)
	    expect_equal(x11CodeBased$futilityPerStage, x11$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x11CodeBased$earlyStop, x11$earlyStop, tolerance = 1e-05)
	    expect_equal(x11CodeBased$successPerStage, x11$successPerStage, tolerance = 1e-05)
	    expect_equal(x11CodeBased$selectedArms, x11$selectedArms, tolerance = 1e-05)
	    expect_equal(x11CodeBased$numberOfActiveArms, x11$numberOfActiveArms, tolerance = 1e-05)
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

	x12 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
	    typeOfShape = "linear", activeArms = 4, threshold = 0,
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2), intersectionTest = "Bonferroni",
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x12' with expected results
	expect_equal(x12$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x12$iterations[2, ], c(6, 6, 7, 9))
	expect_equal(x12$iterations[3, ], c(3, 4, 5, 4))
	expect_equal(x12$rejectAtLeastOne, c(0, 0, 0.5, 0.8), tolerance = 1e-07)
	expect_equal(unlist(as.list(x12$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.3, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.2, 0.2, 0, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x12$futilityStop, c(0.7, 0.6, 0.3, 0.1), tolerance = 1e-07)
	expect_equal(x12$futilityPerStage[1, ], c(0.4, 0.4, 0.3, 0.1), tolerance = 1e-07)
	expect_equal(x12$futilityPerStage[2, ], c(0.3, 0.2, 0, 0), tolerance = 1e-07)
	expect_equal(x12$earlyStop[1, ], c(0.4, 0.4, 0.3, 0.1), tolerance = 1e-07)
	expect_equal(x12$earlyStop[2, ], c(0.3, 0.2, 0.2, 0.5), tolerance = 1e-07)
	expect_equal(x12$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x12$successPerStage[2, ], c(0, 0, 0.2, 0.5), tolerance = 1e-07)
	expect_equal(x12$successPerStage[3, ], c(0, 0, 0.3, 0.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x12$selectedArms)), c(1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.2, 0, 1, 0.2, 0.1, 1, 0.1, 0.1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0.1, 0.1, 1, 0.4, 0.1, 1, 0.3, 0.2, 1, 0.2, 0.1, 1, 0.4, 0.2, 1, 0.4, 0.2, 1, 0.6, 0.3, 1, 0.6, 0.4, 1, 0.7, 0.5, 1, 0.9, 0.4), tolerance = 1e-07)
	expect_equal(x12$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x12$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x12$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(x12$expectedNumberOfSubjects, c(188, 175, 176, 185))
	expect_equal(unlist(as.list(x12$sampleSizes)), c(10, 9.8333333, 32, 10, 10.833333, 27.5, 10, 14.285714, 20, 10, 5.8888889, 25, 10, 18.833333, 0, 10, 33.333333, 25, 10, 2, 20, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 2, 2, 10, 13.333333, 22.75, 10, 37, 66.666667, 10, 8.3333333, 25, 10, 20.857143, 29.2, 10, 18.444444, 36.25, 10, 65.666667, 98.666667, 10, 52.5, 77.5, 10, 39.142857, 71.2, 10, 37.666667, 84), tolerance = 1e-07)
	expect_equal(x12$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x12$conditionalPowerAchieved[2, ], c(0.1067614, 0.028335233, 0.15675994, 0.029094411), tolerance = 1e-07)
	expect_equal(x12$conditionalPowerAchieved[3, ], c(0.43970154, 0.38730712, 0.69132205, 0.60200615), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x12), NA)))
	    expect_output(print(x12)$show())
	    invisible(capture.output(expect_error(summary(x12), NA)))
	    expect_output(summary(x12)$show())
	    x12CodeBased <- eval(parse(text = getObjectRCode(x12, stringWrapParagraphWidth = NULL)))
	    expect_equal(x12CodeBased$iterations, x12$iterations, tolerance = 1e-05)
	    expect_equal(x12CodeBased$rejectAtLeastOne, x12$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x12CodeBased$rejectedArmsPerStage, x12$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x12CodeBased$futilityStop, x12$futilityStop, tolerance = 1e-05)
	    expect_equal(x12CodeBased$futilityPerStage, x12$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x12CodeBased$earlyStop, x12$earlyStop, tolerance = 1e-05)
	    expect_equal(x12CodeBased$successPerStage, x12$successPerStage, tolerance = 1e-05)
	    expect_equal(x12CodeBased$selectedArms, x12$selectedArms, tolerance = 1e-05)
	    expect_equal(x12CodeBased$numberOfActiveArms, x12$numberOfActiveArms, tolerance = 1e-05)
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

	x13 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
	    typeOfShape = "userDefined", activeArms = 4, threshold = 0,
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, adaptations = rep(TRUE, 2),
	    effectMatrix = matrix(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.5), ncol = 4), intersectionTest = "Bonferroni",
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x13' with expected results
	expect_equal(x13$iterations[1, ], c(10, 10))
	expect_equal(x13$iterations[2, ], c(6, 5))
	expect_equal(x13$iterations[3, ], c(6, 3))
	expect_equal(x13$rejectAtLeastOne, c(0.2, 0.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x13$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x13$futilityStop, c(0.4, 0.5), tolerance = 1e-07)
	expect_equal(x13$futilityPerStage[1, ], c(0.4, 0.5), tolerance = 1e-07)
	expect_equal(x13$futilityPerStage[2, ], c(0, 0))
	expect_equal(x13$earlyStop[1, ], c(0.4, 0.5), tolerance = 1e-07)
	expect_equal(x13$earlyStop[2, ], c(0, 0.2), tolerance = 1e-07)
	expect_equal(x13$successPerStage[1, ], c(0, 0))
	expect_equal(x13$successPerStage[2, ], c(0, 0.2), tolerance = 1e-07)
	expect_equal(x13$successPerStage[3, ], c(0.2, 0.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x13$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0, 0, 1, 0, 0, 1, 0.5, 0.5, 1, 0.4, 0.2, 1, 0.6, 0.6, 1, 0.5, 0.3), tolerance = 1e-07)
	expect_equal(x13$numberOfActiveArms[1, ], c(4, 4))
	expect_equal(x13$numberOfActiveArms[2, ], c(1, 1))
	expect_equal(x13$numberOfActiveArms[3, ], c(1, 1))
	expect_equal(x13$expectedNumberOfSubjects, c(203, 169.6), tolerance = 1e-07)
	expect_equal(unlist(as.list(x13$sampleSizes)), c(10, 0, 0, 10, 0, 0, 10, 9.8333333, 16.666667, 10, 11.8, 10.666667, 10, 0, 0, 10, 0, 0, 10, 34.5, 66.5, 10, 63.6, 63, 10, 44.333333, 83.166667, 10, 75.4, 73.666667), tolerance = 1e-07)
	expect_equal(x13$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_))
	expect_equal(x13$conditionalPowerAchieved[2, ], c(0.045209815, 0.0014148507), tolerance = 1e-07)
	expect_equal(x13$conditionalPowerAchieved[3, ], c(0.60681086, 0.72002567), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x13), NA)))
	    expect_output(print(x13)$show())
	    invisible(capture.output(expect_error(summary(x13), NA)))
	    expect_output(summary(x13)$show())
	    x13CodeBased <- eval(parse(text = getObjectRCode(x13, stringWrapParagraphWidth = NULL)))
	    expect_equal(x13CodeBased$iterations, x13$iterations, tolerance = 1e-05)
	    expect_equal(x13CodeBased$rejectAtLeastOne, x13$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x13CodeBased$rejectedArmsPerStage, x13$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x13CodeBased$futilityStop, x13$futilityStop, tolerance = 1e-05)
	    expect_equal(x13CodeBased$futilityPerStage, x13$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x13CodeBased$earlyStop, x13$earlyStop, tolerance = 1e-05)
	    expect_equal(x13CodeBased$successPerStage, x13$successPerStage, tolerance = 1e-05)
	    expect_equal(x13CodeBased$selectedArms, x13$selectedArms, tolerance = 1e-05)
	    expect_equal(x13CodeBased$numberOfActiveArms, x13$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x13CodeBased$expectedNumberOfSubjects, x13$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x13CodeBased$sampleSizes, x13$sampleSizes, tolerance = 1e-05)
	    expect_equal(x13CodeBased$conditionalPowerAchieved, x13$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x13), "character")
	    df <- as.data.frame(x13)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x13)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x14 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
	    typeOfShape = "sigmoidEmax", gED50 = 2, slope = 0.5, activeArms = 4, threshold = 0,
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1),
	    adaptations = rep(TRUE, 2), intersectionTest = "Sidak",
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x14' with expected results
	expect_equal(x14$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x14$iterations[2, ], c(5, 6, 9, 9))
	expect_equal(x14$iterations[3, ], c(0, 1, 5, 9))
	expect_equal(x14$rejectAtLeastOne, c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x14$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x14$futilityStop, c(1, 0.9, 0.5, 0.1), tolerance = 1e-07)
	expect_equal(x14$futilityPerStage[1, ], c(0.5, 0.4, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x14$futilityPerStage[2, ], c(0.5, 0.5, 0.4, 0), tolerance = 1e-07)
	expect_equal(x14$earlyStop[1, ], c(0.5, 0.4, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x14$earlyStop[2, ], c(0.5, 0.5, 0.4, 0), tolerance = 1e-07)
	expect_equal(x14$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x14$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x14$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x14$selectedArms)), c(1, 0.1, 0, 1, 0.1, 0, 1, 0.2, 0, 1, 0.1, 0.1, 1, 0.2, 0, 1, 0, 0, 1, 0.2, 0.1, 1, 0.1, 0.1, 1, 0, 0, 1, 0.4, 0.1, 1, 0.2, 0.1, 1, 0.3, 0.3, 1, 0.2, 0, 1, 0.1, 0, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.5, 0, 1, 0.6, 0.1, 1, 0.9, 0.5, 1, 0.9, 0.9), tolerance = 1e-07)
	expect_equal(x14$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x14$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x14$numberOfActiveArms[3, ], c(NaN, 1, 1, 1))
	expect_equal(x14$expectedNumberOfSubjects, c(NaN, 171.2, 271.2, 368.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x14$sampleSizes)), c(10, 20, 0, 10, 8.8333333, 0, 10, 17, 0, 10, 5.8888889, 11.111111, 10, 40, 0, 10, 0, 0, 10, 8.8888889, 20, 10, 3, 11.111111, 10, 0, 0, 10, 58.833333, 100, 10, 8.1111111, 20, 10, 28.111111, 33.333333, 10, 38.2, 0, 10, 16.666667, 0, 10, 33.333333, 60, 10, 39.888889, 44.444444, 10, 98.2, 0, 10, 84.333333, 100, 10, 67.333333, 100, 10, 76.888889, 100), tolerance = 1e-07)
	expect_equal(x14$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x14$conditionalPowerAchieved[2, ], c(0.0010701396, 1.0749986e-05, 0.015009054, 0.019936014), tolerance = 1e-07)
	expect_equal(x14$conditionalPowerAchieved[3, ], c(NaN, 0.062530095, 0.19373785, 0.13543053), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x14), NA)))
	    expect_output(print(x14)$show())
	    invisible(capture.output(expect_error(summary(x14), NA)))
	    expect_output(summary(x14)$show())
	    x14CodeBased <- eval(parse(text = getObjectRCode(x14, stringWrapParagraphWidth = NULL)))
	    expect_equal(x14CodeBased$iterations, x14$iterations, tolerance = 1e-05)
	    expect_equal(x14CodeBased$rejectAtLeastOne, x14$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x14CodeBased$rejectedArmsPerStage, x14$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x14CodeBased$futilityStop, x14$futilityStop, tolerance = 1e-05)
	    expect_equal(x14CodeBased$futilityPerStage, x14$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x14CodeBased$earlyStop, x14$earlyStop, tolerance = 1e-05)
	    expect_equal(x14CodeBased$successPerStage, x14$successPerStage, tolerance = 1e-05)
	    expect_equal(x14CodeBased$selectedArms, x14$selectedArms, tolerance = 1e-05)
	    expect_equal(x14CodeBased$numberOfActiveArms, x14$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x14CodeBased$expectedNumberOfSubjects, x14$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x14CodeBased$sampleSizes, x14$sampleSizes, tolerance = 1e-05)
	    expect_equal(x14CodeBased$conditionalPowerAchieved, x14$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x14), "character")
	    df <- as.data.frame(x14)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x14)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x15 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
	    activeArms = 4, threshold = 0, typeOfSelection = "all",
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1),
	    adaptations = rep(TRUE, 2), intersectionTest = "Sidak",
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x15' with expected results
	expect_equal(x15$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x15$iterations[2, ], c(8, 9, 10, 9))
	expect_equal(x15$iterations[3, ], c(4, 9, 8, 5))
	expect_equal(x15$rejectAtLeastOne, c(0, 0.2, 0.7, 0.9), tolerance = 1e-07)
	expect_equal(unlist(as.list(x15$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0.1, 0, 0, 0, 0, 0.2, 0, 0, 0.3, 0, 0, 0.4, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.4, 0.3, 0, 0.6, 0.1), tolerance = 1e-07)
	expect_equal(x15$futilityStop, c(0.6, 0.1, 0, 0.1), tolerance = 1e-07)
	expect_equal(x15$futilityPerStage[1, ], c(0.2, 0.1, 0, 0.1), tolerance = 1e-07)
	expect_equal(x15$futilityPerStage[2, ], c(0.4, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x15$earlyStop[1, ], c(0.2, 0.1, 0, 0.1), tolerance = 1e-07)
	expect_equal(x15$earlyStop[2, ], c(0.4, 0, 0.2, 0.4), tolerance = 1e-07)
	expect_equal(x15$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x15$successPerStage[2, ], c(0, 0, 0.2, 0.4), tolerance = 1e-07)
	expect_equal(x15$successPerStage[3, ], c(0, 0, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x15$selectedArms)), c(1, 0.7, 0.2, 1, 0.5, 0.2, 1, 0.7, 0.5, 1, 0.2, 0.2, 1, 0.5, 0.3, 1, 0.5, 0.4, 1, 0.3, 0.3, 1, 0.7, 0.4, 1, 0.6, 0.1, 1, 0.5, 0.4, 1, 0.7, 0.6, 1, 0.8, 0.4, 1, 0.4, 0.4, 1, 0.8, 0.8, 1, 0.8, 0.6, 1, 0.7, 0.5, 1, 0.8, 0.4, 1, 0.9, 0.9, 1, 1, 0.8, 1, 0.9, 0.5), tolerance = 1e-07)
	expect_equal(x15$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x15$numberOfActiveArms[2, ], c(2.75, 2.5555556, 2.5, 2.6666667), tolerance = 1e-07)
	expect_equal(x15$numberOfActiveArms[3, ], c(2.5, 2, 2.5, 3), tolerance = 1e-07)
	expect_equal(x15$expectedNumberOfSubjects, c(460, 640, 571.4, 381.6), tolerance = 1e-07)
	expect_equal(unlist(as.list(x15$sampleSizes)), c(10, 80, 50, 10, 55.555556, 22.222222, 10, 66.8, 38.5, 10, 22.222222, 1.6, 10, 55, 75, 10, 55.555556, 44.444444, 10, 27.8, 25.5, 10, 69.777778, 14.4, 10, 67.5, 25, 10, 55.555556, 44.444444, 10, 66.8, 48, 10, 80.888889, 14.4, 10, 42.5, 100, 10, 88.888889, 88.888889, 10, 76.8, 48, 10, 69.777778, 15.2, 10, 92.5, 100, 10, 100, 100, 10, 96.8, 73, 10, 92, 15.2), tolerance = 1e-07)
	expect_equal(x15$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x15$conditionalPowerAchieved[2, ], c(0.26433659, 0.055206819, 0.10369686, 0.046653519), tolerance = 1e-07)
	expect_equal(x15$conditionalPowerAchieved[3, ], c(0.023182671, 0.15953762, 0.43788092, 0.96046919), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x15), NA)))
	    expect_output(print(x15)$show())
	    invisible(capture.output(expect_error(summary(x15), NA)))
	    expect_output(summary(x15)$show())
	    x15CodeBased <- eval(parse(text = getObjectRCode(x15, stringWrapParagraphWidth = NULL)))
	    expect_equal(x15CodeBased$iterations, x15$iterations, tolerance = 1e-05)
	    expect_equal(x15CodeBased$rejectAtLeastOne, x15$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x15CodeBased$rejectedArmsPerStage, x15$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x15CodeBased$futilityStop, x15$futilityStop, tolerance = 1e-05)
	    expect_equal(x15CodeBased$futilityPerStage, x15$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x15CodeBased$earlyStop, x15$earlyStop, tolerance = 1e-05)
	    expect_equal(x15CodeBased$successPerStage, x15$successPerStage, tolerance = 1e-05)
	    expect_equal(x15CodeBased$selectedArms, x15$selectedArms, tolerance = 1e-05)
	    expect_equal(x15CodeBased$numberOfActiveArms, x15$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x15CodeBased$expectedNumberOfSubjects, x15$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x15CodeBased$sampleSizes, x15$sampleSizes, tolerance = 1e-05)
	    expect_equal(x15CodeBased$conditionalPowerAchieved, x15$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x15), "character")
	    df <- as.data.frame(x15)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x15)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x16 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
	    activeArms = 4, threshold = 0, typeOfSelection = "rBest", rValue = 2,
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1),
	    adaptations = rep(TRUE, 2), intersectionTest = "Sidak",
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x16' with expected results
	expect_equal(x16$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x16$iterations[2, ], c(9, 9, 10, 10))
	expect_equal(x16$iterations[3, ], c(7, 9, 10, 8))
	expect_equal(x16$rejectAtLeastOne, c(0, 0.2, 0.6, 1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x16$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0.2, 0, 0.2, 0.4, 0, 0, 0, 0, 0.1, 0.1, 0, 0.1, 0.1, 0, 0.7, 0.1), tolerance = 1e-07)
	expect_equal(x16$futilityStop, c(0.3, 0.1, 0, 0), tolerance = 1e-07)
	expect_equal(x16$futilityPerStage[1, ], c(0.1, 0.1, 0, 0), tolerance = 1e-07)
	expect_equal(x16$futilityPerStage[2, ], c(0.2, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x16$earlyStop[1, ], c(0.1, 0.1, 0, 0), tolerance = 1e-07)
	expect_equal(x16$earlyStop[2, ], c(0.2, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x16$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x16$successPerStage[2, ], c(0, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x16$successPerStage[3, ], c(0, 0.1, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x16$selectedArms)), c(1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.6, 0.4, 1, 0.5, 0.5, 1, 0.2, 0.2, 1, 0.2, 0.1, 1, 0.4, 0.2, 1, 0.2, 0.2, 1, 0.6, 0.6, 1, 0.8, 0.6, 1, 0.4, 0.3, 1, 0.8, 0.8, 1, 0.8, 0.8, 1, 0.9, 0.8, 1, 0.9, 0.7, 1, 0.9, 0.9, 1, 1, 1, 1, 1, 0.8), tolerance = 1e-07)
	expect_equal(x16$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x16$numberOfActiveArms[2, ], c(1.8888889, 1.8888889, 1.9, 2), tolerance = 1e-07)
	expect_equal(x16$numberOfActiveArms[3, ], c(1.7142857, 1.8888889, 1.9, 2), tolerance = 1e-07)
	expect_equal(x16$expectedNumberOfSubjects, c(465.5, 426.3, 413.1, 244.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x16$sampleSizes)), c(10, 20.555556, 42.857143, 10, 6.5555556, 22.222222, 10, 30, 1.2, 10, 2.2, 0.5, 10, 66.666667, 57.142857, 10, 42.111111, 55.555556, 10, 4.9, 13.9, 10, 14.1, 5, 10, 35.111111, 28.571429, 10, 11.777778, 1.5555556, 10, 44.7, 33.8, 10, 36.6, 21.75, 10, 41, 42.857143, 10, 63.333333, 68.222222, 10, 49.6, 57.3, 10, 32.9, 27.25, 10, 87.222222, 100, 10, 67.444444, 79.333333, 10, 69.6, 58.1, 10, 42.9, 27.25), tolerance = 1e-07)
	expect_equal(x16$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x16$conditionalPowerAchieved[2, ], c(0.096913955, 0.09039929, 0.11243241, 0.1746525), tolerance = 1e-07)
	expect_equal(x16$conditionalPowerAchieved[3, ], c(0.093425176, 0.41153932, 0.67843506, 0.87119979), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x16), NA)))
	    expect_output(print(x16)$show())
	    invisible(capture.output(expect_error(summary(x16), NA)))
	    expect_output(summary(x16)$show())
	    x16CodeBased <- eval(parse(text = getObjectRCode(x16, stringWrapParagraphWidth = NULL)))
	    expect_equal(x16CodeBased$iterations, x16$iterations, tolerance = 1e-05)
	    expect_equal(x16CodeBased$rejectAtLeastOne, x16$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x16CodeBased$rejectedArmsPerStage, x16$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x16CodeBased$futilityStop, x16$futilityStop, tolerance = 1e-05)
	    expect_equal(x16CodeBased$futilityPerStage, x16$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x16CodeBased$earlyStop, x16$earlyStop, tolerance = 1e-05)
	    expect_equal(x16CodeBased$successPerStage, x16$successPerStage, tolerance = 1e-05)
	    expect_equal(x16CodeBased$selectedArms, x16$selectedArms, tolerance = 1e-05)
	    expect_equal(x16CodeBased$numberOfActiveArms, x16$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x16CodeBased$expectedNumberOfSubjects, x16$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x16CodeBased$sampleSizes, x16$sampleSizes, tolerance = 1e-05)
	    expect_equal(x16CodeBased$conditionalPowerAchieved, x16$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x16), "character")
	    df <- as.data.frame(x16)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x16)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x17 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
	    activeArms = 4, threshold = 0, typeOfSelection = "epsilon", epsilonValue = 0.1,
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1),
	    adaptations = rep(TRUE, 2), intersectionTest = "Simes",
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x17' with expected results
	expect_equal(x17$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x17$iterations[2, ], c(9, 9, 8, 10))
	expect_equal(x17$iterations[3, ], c(7, 8, 6, 5))
	expect_equal(x17$rejectAtLeastOne, c(0, 0.3, 0.4, 0.8), tolerance = 1e-07)
	expect_equal(unlist(as.list(x17$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0.1, 0.1, 0, 0.1, 0.4, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.3, 0, 0.1, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(x17$futilityStop, c(0.3, 0.1, 0.2, 0), tolerance = 1e-07)
	expect_equal(x17$futilityPerStage[1, ], c(0.1, 0.1, 0.2, 0), tolerance = 1e-07)
	expect_equal(x17$futilityPerStage[2, ], c(0.2, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x17$earlyStop[1, ], c(0.1, 0.1, 0.2, 0), tolerance = 1e-07)
	expect_equal(x17$earlyStop[2, ], c(0.2, 0.1, 0.2, 0.5), tolerance = 1e-07)
	expect_equal(x17$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x17$successPerStage[2, ], c(0, 0.1, 0.2, 0.5), tolerance = 1e-07)
	expect_equal(x17$successPerStage[3, ], c(0, 0.1, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x17$selectedArms)), c(1, 0.3, 0.1, 1, 0.2, 0.1, 1, 0.1, 0, 1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0.6, 0.4, 1, 0.3, 0.1, 1, 0.3, 0.1, 1, 0.3, 0.2, 1, 0.4, 0.4, 1, 0.4, 0.2, 1, 0.7, 0.3, 1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.7, 0.5, 1, 0.6, 0.2, 1, 0.9, 0.7, 1, 0.9, 0.8, 1, 0.8, 0.6, 1, 1, 0.5), tolerance = 1e-07)
	expect_equal(x17$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x17$numberOfActiveArms[2, ], c(1.3333333, 1.4444444, 1.875, 1.7), tolerance = 1e-07)
	expect_equal(x17$numberOfActiveArms[3, ], c(1.1428571, 1.25, 1.3333333, 1.4), tolerance = 1e-07)
	expect_equal(x17$expectedNumberOfSubjects, c(339.9, 359.2, 222.7, 176), tolerance = 1e-07)
	expect_equal(unlist(as.list(x17$sampleSizes)), c(10, 31.333333, 14.285714, 10, 17.666667, 8.125, 10, 12.5, 0, 10, 1.2, 7.4, 10, 21.222222, 14.285714, 10, 35.888889, 50, 10, 25.625, 16.666667, 10, 13.6, 7.4, 10, 24.666667, 21.142857, 10, 33.222222, 50, 10, 31.5, 17.333333, 10, 26.2, 9.8, 10, 22.444444, 57.142857, 10, 5.1111111, 12.5, 10, 33, 19.833333, 10, 21.2, 12.2, 10, 67.333333, 92.571429, 10, 59.444444, 95.625, 10, 45.5, 36.5, 10, 34.4, 22), tolerance = 1e-07)
	expect_equal(x17$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x17$conditionalPowerAchieved[2, ], c(0.039329058, 0.14668797, 0.16576057, 0.14296603), tolerance = 1e-07)
	expect_equal(x17$conditionalPowerAchieved[3, ], c(0.28763166, 0.40839298, 0.6012117, 0.84313531), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x17), NA)))
	    expect_output(print(x17)$show())
	    invisible(capture.output(expect_error(summary(x17), NA)))
	    expect_output(summary(x17)$show())
	    x17CodeBased <- eval(parse(text = getObjectRCode(x17, stringWrapParagraphWidth = NULL)))
	    expect_equal(x17CodeBased$iterations, x17$iterations, tolerance = 1e-05)
	    expect_equal(x17CodeBased$rejectAtLeastOne, x17$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x17CodeBased$rejectedArmsPerStage, x17$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x17CodeBased$futilityStop, x17$futilityStop, tolerance = 1e-05)
	    expect_equal(x17CodeBased$futilityPerStage, x17$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x17CodeBased$earlyStop, x17$earlyStop, tolerance = 1e-05)
	    expect_equal(x17CodeBased$successPerStage, x17$successPerStage, tolerance = 1e-05)
	    expect_equal(x17CodeBased$selectedArms, x17$selectedArms, tolerance = 1e-05)
	    expect_equal(x17CodeBased$numberOfActiveArms, x17$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x17CodeBased$expectedNumberOfSubjects, x17$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x17CodeBased$sampleSizes, x17$sampleSizes, tolerance = 1e-05)
	    expect_equal(x17CodeBased$conditionalPowerAchieved, x17$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x17), "character")
	    df <- as.data.frame(x17)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x17)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x18 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
	    activeArms = 4, threshold = 0,
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1),
	    adaptations = c(TRUE, FALSE), intersectionTest = "Simes",
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x18' with expected results
	expect_equal(x18$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x18$iterations[2, ], c(7, 8, 8, 10))
	expect_equal(x18$iterations[3, ], c(7, 8, 7, 5))
	expect_equal(x18$rejectAtLeastOne, c(0.1, 0.1, 0.3, 0.7), tolerance = 1e-07)
	expect_equal(unlist(as.list(x18$rejectedArmsPerStage)), c(0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0.2, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.1, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x18$futilityStop, c(0.3, 0.2, 0.2, 0), tolerance = 1e-07)
	expect_equal(x18$futilityPerStage[1, ], c(0.3, 0.2, 0.2, 0), tolerance = 1e-07)
	expect_equal(x18$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x18$earlyStop[1, ], c(0.3, 0.2, 0.2, 0), tolerance = 1e-07)
	expect_equal(x18$earlyStop[2, ], c(0, 0, 0.1, 0.5), tolerance = 1e-07)
	expect_equal(x18$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x18$successPerStage[2, ], c(0, 0, 0.1, 0.5), tolerance = 1e-07)
	expect_equal(x18$successPerStage[3, ], c(0.1, 0.1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x18$selectedArms)), c(1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0, 0, 1, 0.2, 0.1, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.4, 0.1, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.2, 0.1, 1, 0.3, 0.2, 1, 0.7, 0.7, 1, 0.8, 0.8, 1, 0.8, 0.7, 1, 1, 0.5), tolerance = 1e-07)
	expect_equal(x18$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x18$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x18$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(x18$expectedNumberOfSubjects, c(241.6, 306.8, 235.2, 156), tolerance = 1e-07)
	expect_equal(unlist(as.list(x18$sampleSizes)), c(10, 27.285714, 27.285714, 10, 25, 25, 10, 0, 0, 10, 2.6, 4.4, 10, 16.142857, 16.142857, 10, 16, 16, 10, 3.5, 4, 10, 1.4, 2.8, 10, 14.285714, 14.285714, 10, 12.5, 12.5, 10, 40.875, 46.571429, 10, 15.8, 5.2, 10, 10.714286, 10.714286, 10, 26.75, 26.75, 10, 19.875, 8.2857143, 10, 18.6, 16.8, 10, 68.428571, 68.428571, 10, 80.25, 80.25, 10, 64.25, 58.857143, 10, 38.4, 29.2), tolerance = 1e-07)
	expect_equal(x18$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x18$conditionalPowerAchieved[2, ], c(0.064400041, 0.012818439, 0.075196936, 0.13824332), tolerance = 1e-07)
	expect_equal(x18$conditionalPowerAchieved[3, ], c(0.066989319, 0.23112098, 0.45267281, 0.52012057), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x18), NA)))
	    expect_output(print(x18)$show())
	    invisible(capture.output(expect_error(summary(x18), NA)))
	    expect_output(summary(x18)$show())
	    x18CodeBased <- eval(parse(text = getObjectRCode(x18, stringWrapParagraphWidth = NULL)))
	    expect_equal(x18CodeBased$iterations, x18$iterations, tolerance = 1e-05)
	    expect_equal(x18CodeBased$rejectAtLeastOne, x18$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x18CodeBased$rejectedArmsPerStage, x18$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x18CodeBased$futilityStop, x18$futilityStop, tolerance = 1e-05)
	    expect_equal(x18CodeBased$futilityPerStage, x18$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x18CodeBased$earlyStop, x18$earlyStop, tolerance = 1e-05)
	    expect_equal(x18CodeBased$successPerStage, x18$successPerStage, tolerance = 1e-05)
	    expect_equal(x18CodeBased$selectedArms, x18$selectedArms, tolerance = 1e-05)
	    expect_equal(x18CodeBased$numberOfActiveArms, x18$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x18CodeBased$expectedNumberOfSubjects, x18$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x18CodeBased$sampleSizes, x18$sampleSizes, tolerance = 1e-05)
	    expect_equal(x18CodeBased$conditionalPowerAchieved, x18$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x18), "character")
	    df <- as.data.frame(x18)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x18)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x19 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
	    activeArms = 4, threshold = 0, typeOfSelection = "all",
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1),
	    adaptations = c(TRUE, FALSE), intersectionTest = "Simes",
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x19' with expected results
	expect_equal(x19$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x19$iterations[2, ], c(8, 8, 10, 10))
	expect_equal(x19$iterations[3, ], c(8, 8, 9, 9))
	expect_equal(x19$rejectAtLeastOne, c(0, 0, 0.9, 1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x19$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0.4, 0.2, 0, 0, 0, 0, 0, 0, 0, 0.5, 0.1, 0, 0.6, 0.2, 0, 0, 0, 0, 0, 0, 0, 0.8, 0.1, 0, 0.7, 0), tolerance = 1e-07)
	expect_equal(x19$futilityStop, c(0.2, 0.2, 0, 0), tolerance = 1e-07)
	expect_equal(x19$futilityPerStage[1, ], c(0.2, 0.2, 0, 0), tolerance = 1e-07)
	expect_equal(x19$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x19$earlyStop[1, ], c(0.2, 0.2, 0, 0), tolerance = 1e-07)
	expect_equal(x19$earlyStop[2, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x19$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x19$successPerStage[2, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x19$successPerStage[3, ], c(0, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x19$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.8, 0.8, 1, 0.6, 0.6, 1, 0.5, 0.5, 1, 0.6, 0.6, 1, 0.7, 0.6, 1, 0.9, 0.8, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.9, 0.8, 1, 0.8, 0.7, 1, 0.6, 0.6, 1, 0.5, 0.5, 1, 0.9, 0.8, 1, 0.8, 0.7, 1, 0.8, 0.8, 1, 0.8, 0.8, 1, 1, 0.9, 1, 1, 0.9), tolerance = 1e-07)
	expect_equal(x19$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x19$numberOfActiveArms[2, ], c(2.375, 2.375, 3.3, 3.1), tolerance = 1e-07)
	expect_equal(x19$numberOfActiveArms[3, ], c(2.375, 2.375, 3.3333333, 3.1111111), tolerance = 1e-07)
	expect_equal(x19$expectedNumberOfSubjects, c(523.8, 590, 818.4, 765.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x19$sampleSizes)), c(10, 28.125, 28.125, 10, 37.5, 37.5, 10, 73.6, 81.666667, 10, 55.2, 61.333333, 10, 58.625, 58.625, 10, 75, 75, 10, 70, 66.666667, 10, 85.2, 83.555556, 10, 53.125, 53.125, 10, 62.5, 62.5, 10, 83.6, 81.666667, 10, 71.1, 67.777778, 10, 65.625, 65.625, 10, 62.5, 62.5, 10, 83.6, 81.666667, 10, 75.2, 72.444444, 10, 90.625, 90.625, 10, 100, 100, 10, 93.6, 92.777778, 10, 91.1, 90), tolerance = 1e-07)
	expect_equal(x19$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x19$conditionalPowerAchieved[2, ], c(0.10081958, 0.049714416, 0.18629752, 0.24626925), tolerance = 1e-07)
	expect_equal(x19$conditionalPowerAchieved[3, ], c(0.088506618, 0.13049081, 0.60815392, 0.85577973), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x19), NA)))
	    expect_output(print(x19)$show())
	    invisible(capture.output(expect_error(summary(x19), NA)))
	    expect_output(summary(x19)$show())
	    x19CodeBased <- eval(parse(text = getObjectRCode(x19, stringWrapParagraphWidth = NULL)))
	    expect_equal(x19CodeBased$iterations, x19$iterations, tolerance = 1e-05)
	    expect_equal(x19CodeBased$rejectAtLeastOne, x19$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x19CodeBased$rejectedArmsPerStage, x19$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x19CodeBased$futilityStop, x19$futilityStop, tolerance = 1e-05)
	    expect_equal(x19CodeBased$futilityPerStage, x19$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x19CodeBased$earlyStop, x19$earlyStop, tolerance = 1e-05)
	    expect_equal(x19CodeBased$successPerStage, x19$successPerStage, tolerance = 1e-05)
	    expect_equal(x19CodeBased$selectedArms, x19$selectedArms, tolerance = 1e-05)
	    expect_equal(x19CodeBased$numberOfActiveArms, x19$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x19CodeBased$expectedNumberOfSubjects, x19$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x19CodeBased$sampleSizes, x19$sampleSizes, tolerance = 1e-05)
	    expect_equal(x19CodeBased$conditionalPowerAchieved, x19$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x19), "character")
	    df <- as.data.frame(x19)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x19)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x20 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
	    activeArms = 4, threshold = 0, typeOfSelection = "rBest", rValue = 2,
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1),
	    adaptations = c(TRUE, FALSE), intersectionTest = "Hierarchical",
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x20' with expected results
	expect_equal(x20$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x20$iterations[2, ], c(7, 7, 9, 10))
	expect_equal(x20$iterations[3, ], c(2, 5, 3, 1))
	expect_equal(x20$rejectAtLeastOne, c(0, 0, 0.2, 0), tolerance = 1e-07)
	expect_equal(unlist(as.list(x20$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x20$futilityStop, c(0.8, 0.5, 0.6, 0.9), tolerance = 1e-07)
	expect_equal(x20$futilityPerStage[1, ], c(0.3, 0.3, 0.1, 0), tolerance = 1e-07)
	expect_equal(x20$futilityPerStage[2, ], c(0.5, 0.2, 0.5, 0.9), tolerance = 1e-07)
	expect_equal(x20$earlyStop[1, ], c(0.3, 0.3, 0.1, 0), tolerance = 1e-07)
	expect_equal(x20$earlyStop[2, ], c(0.5, 0.2, 0.6, 0.9), tolerance = 1e-07)
	expect_equal(x20$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x20$successPerStage[2, ], c(0, 0, 0.1, 0), tolerance = 1e-07)
	expect_equal(x20$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x20$selectedArms)), c(1, 0.2, 0.2, 1, 0.5, 0.5, 1, 0.4, 0.3, 1, 0.1, 0.1, 1, 0.3, 0.1, 1, 0.2, 0, 1, 0.1, 0, 1, 0.4, 0, 1, 0.2, 0, 1, 0.3, 0.3, 1, 0.5, 0.1, 1, 0.8, 0, 1, 0.5, 0.1, 1, 0.4, 0.2, 1, 0.6, 0.2, 1, 0.7, 0.1, 1, 0.7, 0.2, 1, 0.7, 0.5, 1, 0.9, 0.3, 1, 1, 0.1), tolerance = 1e-07)
	expect_equal(x20$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x20$numberOfActiveArms[2, ], c(1.7142857, 2, 1.7777778, 2), tolerance = 1e-07)
	expect_equal(x20$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(x20$expectedNumberOfSubjects, c(267.3, 301.1, 325.2, 315.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(x20$sampleSizes)), c(10, 24.142857, 84.5, 10, 51.714286, 72.2, 10, 39, 83.333333, 10, 8.8, 88, 10, 36.142857, 50, 10, 16.285714, 0, 10, 4.1111111, 0, 10, 28.2, 0, 10, 28.571429, 0, 10, 30.142857, 42.2, 10, 42.555556, 33.333333, 10, 60.9, 0, 10, 60.285714, 34.5, 10, 37.857143, 30, 10, 55.222222, 50, 10, 61.5, 88, 10, 88.857143, 84.5, 10, 68, 72.2, 10, 81.555556, 83.333333, 10, 79.7, 88), tolerance = 1e-07)
	expect_equal(x20$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x20$conditionalPowerAchieved[2, ], c(0.14688077, 0.19244817, 0.083030211, 0.1268121), tolerance = 1e-07)
	expect_equal(x20$conditionalPowerAchieved[3, ], c(0.021357961, 0.35341345, 0.67128636, 1), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x20), NA)))
	    expect_output(print(x20)$show())
	    invisible(capture.output(expect_error(summary(x20), NA)))
	    expect_output(summary(x20)$show())
	    x20CodeBased <- eval(parse(text = getObjectRCode(x20, stringWrapParagraphWidth = NULL)))
	    expect_equal(x20CodeBased$iterations, x20$iterations, tolerance = 1e-05)
	    expect_equal(x20CodeBased$rejectAtLeastOne, x20$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x20CodeBased$rejectedArmsPerStage, x20$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x20CodeBased$futilityStop, x20$futilityStop, tolerance = 1e-05)
	    expect_equal(x20CodeBased$futilityPerStage, x20$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x20CodeBased$earlyStop, x20$earlyStop, tolerance = 1e-05)
	    expect_equal(x20CodeBased$successPerStage, x20$successPerStage, tolerance = 1e-05)
	    expect_equal(x20CodeBased$selectedArms, x20$selectedArms, tolerance = 1e-05)
	    expect_equal(x20CodeBased$numberOfActiveArms, x20$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x20CodeBased$expectedNumberOfSubjects, x20$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x20CodeBased$sampleSizes, x20$sampleSizes, tolerance = 1e-05)
	    expect_equal(x20CodeBased$conditionalPowerAchieved, x20$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x20), "character")
	    df <- as.data.frame(x20)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x20)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x21 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
	    activeArms = 4, threshold = 0, typeOfSelection = "epsilon", epsilonValue = 0.1,
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1),
	    adaptations = c(TRUE, FALSE), intersectionTest = "Hierarchical",
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x21' with expected results
	expect_equal(x21$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x21$iterations[2, ], c(9, 9, 9, 10))
	expect_equal(x21$iterations[3, ], c(2, 4, 4, 2))
	expect_equal(x21$rejectAtLeastOne, c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x21$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x21$futilityStop, c(0.8, 0.6, 0.6, 0.8), tolerance = 1e-07)
	expect_equal(x21$futilityPerStage[1, ], c(0.1, 0.1, 0.1, 0), tolerance = 1e-07)
	expect_equal(x21$futilityPerStage[2, ], c(0.7, 0.5, 0.5, 0.8), tolerance = 1e-07)
	expect_equal(x21$earlyStop[1, ], c(0.1, 0.1, 0.1, 0), tolerance = 1e-07)
	expect_equal(x21$earlyStop[2, ], c(0.7, 0.5, 0.5, 0.8), tolerance = 1e-07)
	expect_equal(x21$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x21$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x21$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x21$selectedArms)), c(1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.2, 0, 1, 0.2, 0.1, 1, 0.3, 0.1, 1, 0.4, 0.1, 1, 0.5, 0.1, 1, 0.5, 0.2, 1, 0.2, 0.2, 1, 0.7, 0.1, 1, 0.4, 0, 1, 0.2, 0, 1, 0.7, 0.3, 1, 0.9, 0.2, 1, 0.9, 0.2, 1, 0.9, 0.4, 1, 0.9, 0.4, 1, 1, 0.2), tolerance = 1e-07)
	expect_equal(x21$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x21$numberOfActiveArms[2, ], c(1.4444444, 1.4444444, 1.7777778, 2.2), tolerance = 1e-07)
	expect_equal(x21$numberOfActiveArms[3, ], c(1.5, 1.75, 2.5, 3), tolerance = 1e-07)
	expect_equal(x21$expectedNumberOfSubjects, c(240.6, 332.2, 346.2, 256.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(x21$sampleSizes)), c(10, 17.666667, 79, 10, 39.222222, 88.25, 10, 35.777778, 80.25, 10, 7.9, 39, 10, 13.555556, 0, 10, 22.222222, 25, 10, 24.666667, 5.25, 10, 17.7, 25, 10, 42.333333, 50, 10, 47.555556, 50, 10, 22.222222, 50, 10, 44.9, 25, 10, 27.111111, 0, 10, 14.111111, 0, 10, 51.888889, 55.25, 10, 50.7, 39, 10, 64.888889, 79, 10, 78.666667, 88.25, 10, 74.111111, 80.25, 10, 51.9, 39), tolerance = 1e-07)
	expect_equal(x21$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x21$conditionalPowerAchieved[2, ], c(0.071382822, 0.0014758747, 0.067299064, 0.14413714), tolerance = 1e-07)
	expect_equal(x21$conditionalPowerAchieved[3, ], c(0.29927137, 0.0060466075, 0.55383829, 0.59417789), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x21), NA)))
	    expect_output(print(x21)$show())
	    invisible(capture.output(expect_error(summary(x21), NA)))
	    expect_output(summary(x21)$show())
	    x21CodeBased <- eval(parse(text = getObjectRCode(x21, stringWrapParagraphWidth = NULL)))
	    expect_equal(x21CodeBased$iterations, x21$iterations, tolerance = 1e-05)
	    expect_equal(x21CodeBased$rejectAtLeastOne, x21$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x21CodeBased$rejectedArmsPerStage, x21$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x21CodeBased$futilityStop, x21$futilityStop, tolerance = 1e-05)
	    expect_equal(x21CodeBased$futilityPerStage, x21$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x21CodeBased$earlyStop, x21$earlyStop, tolerance = 1e-05)
	    expect_equal(x21CodeBased$successPerStage, x21$successPerStage, tolerance = 1e-05)
	    expect_equal(x21CodeBased$selectedArms, x21$selectedArms, tolerance = 1e-05)
	    expect_equal(x21CodeBased$numberOfActiveArms, x21$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x21CodeBased$expectedNumberOfSubjects, x21$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x21CodeBased$sampleSizes, x21$sampleSizes, tolerance = 1e-05)
	    expect_equal(x21CodeBased$conditionalPowerAchieved, x21$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x21), "character")
	    df <- as.data.frame(x21)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x21)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x22 <- getSimulationMultiArmRates(
	    seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
	    activeArms = 4, threshold = 0.1, plannedSubjects = c(10, 30, 50), piControl = 0.3,
	    piMaxVector = seq(0.1, 0.3, 0.1), intersectionTest = "Hierarchical",
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4),
	    maxNumberOfSubjectsPerStage = c(10, 100, 100), directionUpper = FALSE,
	    maxNumberOfIterations = 1
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x22' with expected results
	expect_equal(x22$iterations[1, ], c(1, 1, 1))
	expect_equal(x22$iterations[2, ], c(0, 1, 0))
	expect_equal(x22$iterations[3, ], c(0, 0, 0))
	expect_equal(x22$rejectAtLeastOne, c(0, 0, 0))
	expect_equal(unlist(as.list(x22$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x22$futilityStop, c(1, 1, 1))
	expect_equal(x22$futilityPerStage[1, ], c(1, 0, 1))
	expect_equal(x22$futilityPerStage[2, ], c(0, 1, 0))
	expect_equal(x22$earlyStop[1, ], c(1, 0, 1))
	expect_equal(x22$earlyStop[2, ], c(0, 1, 0))
	expect_equal(x22$successPerStage[1, ], c(0, 0, 0))
	expect_equal(x22$successPerStage[2, ], c(0, 0, 0))
	expect_equal(x22$successPerStage[3, ], c(0, 0, 0))
	expect_equal(unlist(as.list(x22$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0))
	expect_equal(x22$numberOfActiveArms[1, ], c(4, 4, 4))
	expect_equal(x22$numberOfActiveArms[2, ], c(NaN, 1, NaN))
	expect_equal(x22$numberOfActiveArms[3, ], c(NaN, NaN, NaN))
	expect_equal(x22$expectedNumberOfSubjects, c(NaN, NaN, NaN))
	expect_equal(unlist(as.list(x22$sampleSizes)), c(10, 0, 0, 10, 0, 0, 10, NaN, NaN, 10, 0, 0, 10, 0, 0, 10, NaN, NaN, 10, 0, 0, 10, 91, 0, 10, NaN, NaN, 10, 0, 0, 10, 0, 0, 10, NaN, NaN, 10, 0, 0, 10, 91, 0, 10, NaN, NaN))
	expect_equal(x22$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x22$conditionalPowerAchieved[2, ], c(NaN, 3.7427402e-05, NaN), tolerance = 1e-07)
	expect_equal(x22$conditionalPowerAchieved[3, ], c(NaN, NaN, NaN))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x22), NA)))
	    expect_output(print(x22)$show())
	    invisible(capture.output(expect_error(summary(x22), NA)))
	    expect_output(summary(x22)$show())
	    x22CodeBased <- eval(parse(text = getObjectRCode(x22, stringWrapParagraphWidth = NULL)))
	    expect_equal(x22CodeBased$iterations, x22$iterations, tolerance = 1e-05)
	    expect_equal(x22CodeBased$rejectAtLeastOne, x22$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x22CodeBased$rejectedArmsPerStage, x22$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x22CodeBased$futilityStop, x22$futilityStop, tolerance = 1e-05)
	    expect_equal(x22CodeBased$futilityPerStage, x22$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x22CodeBased$earlyStop, x22$earlyStop, tolerance = 1e-05)
	    expect_equal(x22CodeBased$successPerStage, x22$successPerStage, tolerance = 1e-05)
	    expect_equal(x22CodeBased$selectedArms, x22$selectedArms, tolerance = 1e-05)
	    expect_equal(x22CodeBased$numberOfActiveArms, x22$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x22CodeBased$expectedNumberOfSubjects, x22$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(x22CodeBased$sampleSizes, x22$sampleSizes, tolerance = 1e-05)
	    expect_equal(x22CodeBased$conditionalPowerAchieved, x22$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x22), "character")
	    df <- as.data.frame(x22)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x22)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getSimulationMultiArmRates': using calcSubjectsFunction", {

	.skipTestIfDisabled()
	.skipTestIfNotX64()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
	# @refFS[Tab.]{fs:tab:output:getSimulationMultiArmRates}
	# @refFS[Formula]{fs:simulationMultiArmDoseResponse}
	# @refFS[Formula]{fs:simulationMultiArmRatesGenerate}
	# @refFS[Formula]{fs:simulationMultiArmRatesTestStatistics}
	# @refFS[Formula]{fs:simulationMultiArmSelections}
	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	calcSubjectsFunctionSimulationMultiArmRates <- function(..., stage, minNumberOfSubjectsPerStage) {
	    return(ifelse(stage == 3, 33, minNumberOfSubjectsPerStage[stage]))
	}

	x <- getSimulationMultiArmRates(
	    seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
	    typeOfShape = "linear", activeArms = 4,
	    plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
	    minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),
	    maxNumberOfIterations = 10, calcSubjectsFunction = calcSubjectsFunctionSimulationMultiArmRates
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 9))
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.2, 0.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0, 0, 1, 0.1, 0.1, 1, 0, 0, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.6, 0.6, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.6, 0.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.9), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(x$expectedNumberOfSubjects, c(124, 124, 124, 117.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 0.4, 3.3, 10, 1.6, 13.2, 10, 0, 0, 10, 0.4, 3.6666667, 10, 0, 0, 10, 0.4, 3.3, 10, 1.6, 13.2, 10, 0.8, 7.3333333, 10, 1.2, 9.9, 10, 0.8, 6.6, 10, 0.8, 6.6, 10, 0.4, 3.6666667, 10, 2.4, 19.8, 10, 1.2, 9.9, 10, 1.6, 13.2, 10, 2.4, 18.333333, 10, 4, 33, 10, 4, 33, 10, 4, 33, 10, 4, 33), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.012189382, 0.016190277, 0.020380353, 0.11925746), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.32488024, 0.34652134, 0.40081174, 0.68872913), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL)))
	    expect_equal(xCodeBased$iterations, x$iterations, tolerance = 1e-05)
	    expect_equal(xCodeBased$rejectAtLeastOne, x$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(xCodeBased$rejectedArmsPerStage, x$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityStop, x$futilityStop, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityPerStage, x$futilityPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$earlyStop, x$earlyStop, tolerance = 1e-05)
	    expect_equal(xCodeBased$successPerStage, x$successPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$selectedArms, x$selectedArms, tolerance = 1e-05)
	    expect_equal(xCodeBased$numberOfActiveArms, x$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(xCodeBased$expectedNumberOfSubjects, x$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(xCodeBased$sampleSizes, x$sampleSizes, tolerance = 1e-05)
	    expect_equal(xCodeBased$conditionalPowerAchieved, x$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getSimulationMultiArmRates': using selectArmsFunction", {

	.skipTestIfDisabled()
	.skipTestIfNotX64()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
	# @refFS[Tab.]{fs:tab:output:getSimulationMultiArmRates}
	# @refFS[Formula]{fs:simulationMultiArmDoseResponse}
	# @refFS[Formula]{fs:simulationMultiArmRatesGenerate}
	# @refFS[Formula]{fs:simulationMultiArmRatesTestStatistics}
	# @refFS[Formula]{fs:simulationMultiArmSelections}
	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	selectArmsFunctionSimulationMultiArmRates <- function(effectSizes) {
	    return(c(TRUE, FALSE, FALSE, FALSE))
	}

	x <- getSimulationMultiArmRates(
	    seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)),
	    typeOfShape = "linear", activeArms = 4,
	    plannedSubjects = c(10, 30, 50), piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
	    maxNumberOfIterations = 10, selectArmsFunction = selectArmsFunctionSimulationMultiArmRates, typeOfSelection = "userDefined"
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 9))
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.1, 0.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.4, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.1, 0), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.9), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(x$expectedNumberOfSubjects, c(130, 130, 130, 126))
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 20, 20, 10, 20, 20, 10, 20, 20, 10, 20, 20, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 20, 20, 10, 20, 20, 10, 20, 20, 10, 20, 20))
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.044616119, 0.11264062, 0.1248477, 0.43958255), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.087582974, 0.1172724, 0.15105487, 0.4331775), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL)))
	    expect_equal(xCodeBased$iterations, x$iterations, tolerance = 1e-05)
	    expect_equal(xCodeBased$rejectAtLeastOne, x$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(xCodeBased$rejectedArmsPerStage, x$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityStop, x$futilityStop, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityPerStage, x$futilityPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$earlyStop, x$earlyStop, tolerance = 1e-05)
	    expect_equal(xCodeBased$successPerStage, x$successPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$selectedArms, x$selectedArms, tolerance = 1e-05)
	    expect_equal(xCodeBased$numberOfActiveArms, x$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(xCodeBased$expectedNumberOfSubjects, x$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(xCodeBased$sampleSizes, x$sampleSizes, tolerance = 1e-05)
	    expect_equal(xCodeBased$conditionalPowerAchieved, x$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getSimulationMultiArmRates': typeOfShape = sigmoidEmax", {

	.skipTestIfDisabled()
	.skipTestIfNotX64()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
	# @refFS[Tab.]{fs:tab:output:getSimulationMultiArmRates}
	# @refFS[Formula]{fs:simulationMultiArmDoseResponse}
	# @refFS[Formula]{fs:simulationMultiArmRatesGenerate}
	# @refFS[Formula]{fs:simulationMultiArmRatesTestStatistics}
	# @refFS[Formula]{fs:simulationMultiArmSelections}
	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	designIN <- getDesignInverseNormal(typeOfDesign = "P", kMax = 3, futilityBounds = c(0, 0))
	x <- getSimulationMultiArmRates(designIN,
	    activeArms = 3, typeOfShape = "sigmoidEmax",
	    piMaxVector = seq(0.1, 0.9, 0.2), gED50 = 2, plannedSubjects = cumsum(rep(20, 3)), piControl = 0.1,
	    intersectionTest = "Sidak", typeOfSelection = "rBest", rValue = 2, threshold = -Inf,
	    successCriterion = "all", maxNumberOfIterations = 100, seed = 3456
	)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x$iterations[2, ], c(20, 60, 88, 84, 81))
	expect_equal(x$iterations[3, ], c(4, 45, 70, 38, 20))
	expect_equal(x$rejectAtLeastOne, c(0, 0.07, 0.55, 0.89, 0.99), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0.03, 0.02, 0.01, 0.11, 0.11, 0.05, 0.19, 0.06, 0.03, 0, 0, 0, 0, 0.01, 0.03, 0.07, 0.1, 0.13, 0.3, 0.22, 0.14, 0.45, 0.3, 0.12, 0, 0, 0, 0.01, 0.03, 0.01, 0.11, 0.23, 0.18, 0.41, 0.32, 0.09, 0.62, 0.31, 0.04), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.96, 0.54, 0.13, 0.05, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.8, 0.4, 0.11, 0.05, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.16, 0.14, 0.02, 0, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop[1, ], c(0.8, 0.4, 0.12, 0.16, 0.19), tolerance = 1e-07)
	expect_equal(x$earlyStop[2, ], c(0.16, 0.15, 0.18, 0.46, 0.61), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0.01, 0.11, 0.19), tolerance = 1e-07)
	expect_equal(x$successPerStage[2, ], c(0, 0.01, 0.16, 0.46, 0.61), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.01, 0.15, 0.18, 0.14), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.11, 0.01, 1, 0.24, 0.17, 1, 0.26, 0.2, 1, 0.24, 0.14, 1, 0.14, 0.08, 1, 0.13, 0.03, 1, 0.44, 0.34, 1, 0.7, 0.55, 1, 0.69, 0.31, 1, 0.69, 0.13, 1, 0.16, 0.04, 1, 0.52, 0.39, 1, 0.8, 0.65, 1, 0.75, 0.31, 1, 0.79, 0.19, 1, 0.2, 0.04, 1, 0.6, 0.45, 1, 0.88, 0.7, 1, 0.84, 0.38, 1, 0.81, 0.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(3, 3, 3, 3, 3))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2, 2))
	expect_equal(x$expectedNumberOfSubjects, c(94.4, 143, 174.8, 153.2, 140.6), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(20, 11, 5, 20, 8, 7.5555556, 20, 5.9090909, 5.7142857, 20, 5.7142857, 7.3684211, 20, 3.4567901, 8, 20, 13, 15, 20, 14.666667, 15.111111, 20, 15.909091, 15.714286, 20, 16.428571, 16.315789, 20, 17.037037, 13, 20, 16, 20, 20, 17.333333, 17.333333, 20, 18.181818, 18.571429, 20, 17.857143, 16.315789, 20, 19.506173, 19, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.011866207, 0.085418744, 0.23090361, 0.47460917, 0.65183497), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.02497337, 0.151524, 0.4525101, 0.68922536, 0.80573911), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL)))
	    expect_equal(xCodeBased$iterations, x$iterations, tolerance = 1e-05)
	    expect_equal(xCodeBased$rejectAtLeastOne, x$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(xCodeBased$rejectedArmsPerStage, x$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityStop, x$futilityStop, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityPerStage, x$futilityPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$earlyStop, x$earlyStop, tolerance = 1e-05)
	    expect_equal(xCodeBased$successPerStage, x$successPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$selectedArms, x$selectedArms, tolerance = 1e-05)
	    expect_equal(xCodeBased$numberOfActiveArms, x$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(xCodeBased$expectedNumberOfSubjects, x$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(xCodeBased$sampleSizes, x$sampleSizes, tolerance = 1e-05)
	    expect_equal(xCodeBased$conditionalPowerAchieved, x$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getSimulationMultiArmRates': comparison of base and multi-arm", {

	.skipTestIfDisabled()
	.skipTestIfNotX64()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
	# @refFS[Tab.]{fs:tab:output:getSimulationMultiArmRates}
	# @refFS[Formula]{fs:simulationMultiArmDoseResponse}
	# @refFS[Formula]{fs:simulationMultiArmRatesGenerate}
	# @refFS[Formula]{fs:simulationMultiArmRatesTestStatistics}
	# @refFS[Formula]{fs:simulationMultiArmSelections}
	# @refFS[Formula]{fs:multiarmRejectionRule}
	allocationRatioPlanned <- 2
	design <- getDesignInverseNormal(
	    typeOfDesign = "WT", deltaWT = 0.15,
	    futilityBounds = c(-0.5, 0.5), informationRates = c(0.2, 0.8, 1)
	)

	x <- getSimulationMultiArmRates(design,
	    activeArms = 1, plannedSubjects = c(20, 40, 60),
	    directionUpper = FALSE, piControl = 0.6, piMaxVector = seq(0.3, 0.6, 0.1),
	    conditionalPower = 0.6, minNumberOfSubjectsPerStage = c(NA, 20, 20), maxNumberOfSubjectsPerStage = c(NA, 80, 80),
	    piControlH1 = 0.4,
	    piH1 = 0.3,
	    maxNumberOfIterations = 100, allocationRatioPlanned = allocationRatioPlanned, seed = 1234
	)

	y <- getSimulationRates(design,
	    plannedSubjects = round((1 + 1 / allocationRatioPlanned) * c(20, 40, 60)),
	    normalApproximation = TRUE, pi2 = 0.6, pi1 = seq(0.3, 0.6, 0.1), directionUpper = FALSE,
	    conditionalPower = 0.6,
	    pi2H1 = 0.4,
	    pi1H1 = 0.3,
	    minNumberOfSubjectsPerStage = round((1 + 1 / allocationRatioPlanned) * c(NA, 20, 20)),
	    maxNumberOfSubjectsPerStage = round((1 + 1 / allocationRatioPlanned) * c(NA, 80, 80)),
	    maxNumberOfIterations = 100, allocationRatioPlanned = allocationRatioPlanned, seed = 1234
	)

	comp1 <- y$overallReject - x$rejectAtLeastOne

	## Comparison of the results of numeric object 'comp1' with expected results
	expect_equal(comp1, c(-0.03, -0.02, 0.09, 0.03), tolerance = 1e-07)

	comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

	## Comparison of the results of matrixarray object 'comp2' with expected results
	expect_equal(comp2[1, ], c(0, 0, 0, 0))
	expect_equal(comp2[2, ], c(0.09, -0.01, 0.06, 0.02), tolerance = 1e-07)
	expect_equal(comp2[3, ], c(-0.12, -0.01, 0.03, 0.01), tolerance = 1e-07)

	comp3 <- y$futilityPerStage - x$futilityPerStage

	## Comparison of the results of matrixarray object 'comp3' with expected results
	expect_equal(comp3[1, ], c(0.04, 0.04, -0.12, -0.03), tolerance = 1e-07)
	expect_equal(comp3[2, ], c(0.01, 0.02, -0.05, 0.03), tolerance = 1e-07)

	comp4 <- round(y$sampleSizes - (x$sampleSizes[, , 1] + x$sampleSizes[, , 2]), 1)

	## Comparison of the results of matrixarray object 'comp4' with expected results
	expect_equal(comp4[1, ], c(0, 0, 0, 0))
	expect_equal(comp4[2, ], c(1.1, 0.3, 0, 0), tolerance = 1e-07)
	expect_equal(comp4[3, ], c(-44.7, 9.7, 1.3, -3.2), tolerance = 1e-07)

	comp5 <- round(y$expectedNumberOfSubjects - x$expectedNumberOfSubjects, 1)

	## Comparison of the results of numeric object 'comp5' with expected results
	expect_equal(comp5, c(-14.6, -6.6, 26.9, 0.4), tolerance = 1e-07)

	comp6 <- x$earlyStop - y$earlyStop

	## Comparison of the results of matrixarray object 'comp6' with expected results
	expect_equal(comp6[1, ], c(-0.96, -0.39, -0.75, -0.06), tolerance = 1e-07)
	expect_equal(comp6[2, ], c(0.1, -0.16, -0.38, -0.43), tolerance = 1e-07)

})

test_that("'getSimulationMultiArmRates': comparison of base and multi-arm, Fisher design", {

	.skipTestIfDisabled()
	.skipTestIfNotX64()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
	# @refFS[Tab.]{fs:tab:output:getSimulationMultiArmRates}
	# @refFS[Formula]{fs:simulationMultiArmDoseResponse}
	# @refFS[Formula]{fs:simulationMultiArmRatesGenerate}
	# @refFS[Formula]{fs:simulationMultiArmRatesTestStatistics}
	# @refFS[Formula]{fs:simulationMultiArmSelections}
	# @refFS[Formula]{fs:multiarmRejectionRule}
	allocationRatioPlanned <- 1
	design <- getDesignFisher(alpha0Vec = c(0.3, 0.4), informationRates = c(0.5, 0.7, 1))

	x <- getSimulationMultiArmRates(design,
	    activeArms = 1, plannedSubjects = c(20, 40, 60),
	    directionUpper = FALSE, piControl = 0.6, piMaxVector = seq(0.3, 0.6, 0.1),
	    conditionalPower = 0.6, minNumberOfSubjectsPerStage = c(NA, 20, 20), maxNumberOfSubjectsPerStage = c(NA, 80, 80),
	    maxNumberOfIterations = 100, allocationRatioPlanned = allocationRatioPlanned, seed = -1008239793
	)

	y <- getSimulationRates(design,
	    plannedSubjects = round((1 + 1 / allocationRatioPlanned) * c(20, 40, 60)),
	    normalApproximation = TRUE, pi2 = 0.6, pi1 = seq(0.3, 0.6, 0.1), directionUpper = FALSE,
	    conditionalPower = 0.6, minNumberOfSubjectsPerStage = round((1 + 1 / allocationRatioPlanned) * c(NA, 20, 20)),
	    maxNumberOfSubjectsPerStage = round((1 + 1 / allocationRatioPlanned) * c(NA, 80, 80)),
	    maxNumberOfIterations = 100, allocationRatioPlanned = allocationRatioPlanned, seed = -2039707705
	)

	comp1 <- y$overallReject - x$rejectAtLeastOne

	## Comparison of the results of numeric object 'comp1' with expected results
	expect_equal(comp1, c(0.05, 0.1, 0.07, 0.02), tolerance = 1e-07)

	comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

	## Comparison of the results of matrixarray object 'comp2' with expected results
	expect_equal(comp2[1, ], c(0.05, 0.01, 0.02, 0.03), tolerance = 1e-07)
	expect_equal(comp2[2, ], c(-0.03, 0.04, -0.01, -0.01), tolerance = 1e-07)
	expect_equal(comp2[3, ], c(0.03, 0.05, 0.06, 0), tolerance = 1e-07)

	comp3 <- y$futilityPerStage - x$futilityPerStage

	## Comparison of the results of matrixarray object 'comp3' with expected results
	expect_equal(comp3[1, ], c(-0.05, -0.09, 0, 0), tolerance = 1e-07)
	expect_equal(comp3[2, ], c(0, 0, -0.05, 0.01), tolerance = 1e-07)

	comp4 <- round(y$sampleSizes - (x$sampleSizes[, , 1] + x$sampleSizes[, , 2]), 1)

	## Comparison of the results of matrixarray object 'comp4' with expected results
	expect_equal(comp4[1, ], c(0, 0, 0, 0))
	expect_equal(comp4[2, ], c(7.4, 3.6, -6.3, 6.6), tolerance = 1e-07)
	expect_equal(comp4[3, ], c(0.5, 12.9, -5, 26), tolerance = 1e-07)

	comp5 <- round(y$expectedNumberOfSubjects - x$expectedNumberOfSubjects, 1)

	## Comparison of the results of numeric object 'comp5' with expected results
	expect_equal(comp5, c(6.1, 19.9, -2, -3.9), tolerance = 1e-07)

	comp6 <- x$earlyStop - y$earlyStop

	## Comparison of the results of matrixarray object 'comp6' with expected results
	expect_equal(comp6[1, ], c(-0.38, -0.17, -0.41, 0.14), tolerance = 1e-07)
	expect_equal(comp6[2, ], c(-0.29, -0.61, -0.52, -0.78), tolerance = 1e-07)
})

