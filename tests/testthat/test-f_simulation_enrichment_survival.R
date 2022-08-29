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
## |  File name: test-f_simulation_enrichment_survival.R
## |  Creation date: 12 August 2022, 09:12:10
## |  File version: $Revision: 6485 $
## |  Last changed: $Date: 2022-08-12 13:20:22 +0200 (Fr, 12 Aug 2022) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing Simulation Enrichment Survival Function")


test_that("'getSimulationEnrichmentSurvival': gMax = 2", {
	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:enrichmentDesigns}
	# @refFS[Sec.]{fs:subsec:intersectionTestsEnrichment}
	# @refFS[Sec.]{fs:subsec:adaptiveClosedTestProcedureEnrichment}
	# @refFS[Sec.]{fs:subsec:intersectionTestsEnrichment}
	# @refFS[Sec.]{fs:sec:simulationFunctions}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichment}
	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichmentSurvival}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalEvents}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalLogRanks}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalAdjustedPrevalances}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	hazardRatios <- matrix(c(
	    1.000000, 1.207775, 1.432188, 1.676140, 1.943358, 2.238755, 2.568980,
	    1.432188, 1.432188, 1.432188, 1.432188, 1.432188, 1.432188, 1.432188
	), ncol = 2)

	effectList <- list(subGroups = c("S", "R"), prevalences = c(0.4, 0.6), hazardRatios = hazardRatios)

	design <- getDesignInverseNormal(informationRates = c(0.3, 1), typeOfDesign = "asUser", userAlphaSpending = c(0.01, 0.025))

	suppressWarnings(simResult1 <- getSimulationEnrichmentSurvival(design,
	    plannedEvents = c(40, 120),
	    effectList = effectList,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    typeOfSelection = "rbest",
	    rValue = 2,
	    intersectionTest = "SpiessensDebois",
	    directionUpper = TRUE,
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult1' with expected results
	expect_equal(simResult1$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100))
	expect_equal(simResult1$iterations[2, ], c(100, 99, 97, 95, 90, 85, 72))
	expect_equal(simResult1$rejectAtLeastOne, c(0.16, 0.22, 0.45, 0.6, 0.78, 0.86, 0.92), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$rejectedPopulationsPerStage)), c(0, 0.02, 0.02, 0.07, 0.05, 0.16, 0.05, 0.3, 0.17, 0.49, 0.21, 0.62, 0.35, 0.54, 0.05, 0.1, 0.02, 0.17, 0.1, 0.32, 0.16, 0.42, 0.17, 0.53, 0.22, 0.57, 0.33, 0.53), tolerance = 1e-07)
	expect_equal(simResult1$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0))
	expect_equal(simResult1$earlyStop[1, ], c(0, 0.01, 0.03, 0.05, 0.1, 0.15, 0.28), tolerance = 1e-07)
	expect_equal(simResult1$successPerStage[1, ], c(0, 0.01, 0.03, 0.05, 0.1, 0.15, 0.28), tolerance = 1e-07)
	expect_equal(simResult1$successPerStage[2, ], c(0.01, 0.05, 0.15, 0.28, 0.48, 0.61, 0.55), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$selectedPopulations)), c(1, 1, 1, 0.99, 1, 0.97, 1, 0.95, 1, 0.9, 1, 0.85, 1, 0.72, 1, 1, 1, 0.99, 1, 0.97, 1, 0.95, 1, 0.9, 1, 0.85, 1, 0.72), tolerance = 1e-07)
	expect_equal(simResult1$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2))
	expect_equal(simResult1$numberOfPopulations[2, ], c(2, 2, 2, 2, 2, 2, 2))
	expect_equal(simResult1$expectedNumberOfEvents, c(120, 119.2, 117.6, 116, 112, 108, 97.6), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$singleNumberOfEventsPerStage)), c(14.163599, 28.327198, 15.080284, 30.160567, 16, 32, 16.925752, 33.851505, 17.861157, 35.722315, 18.810731, 37.621462, 19.780244, 39.560487, 25.836401, 51.672802, 24.919716, 49.839433, 24, 48, 23.074248, 46.148495, 22.138843, 44.277685, 21.189269, 42.378538, 20.219756, 40.439513), tolerance = 1e-07)
	expect_equal(simResult1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult1$conditionalPowerAchieved[2, ], c(0.13647132, 0.29139937, 0.31543636, 0.4252398, 0.58131066, 0.58903037, 0.64693196), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult1), NA)))
	    expect_output(print(simResult1)$show())
	    invisible(capture.output(expect_error(summary(simResult1), NA)))
	    expect_output(summary(simResult1)$show())
	    suppressWarnings(simResult1CodeBased <- eval(parse(text = getObjectRCode(simResult1, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult1CodeBased$eventsPerStage, simResult1$eventsPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$iterations, simResult1$iterations, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$rejectAtLeastOne, simResult1$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$rejectedPopulationsPerStage, simResult1$rejectedPopulationsPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$futilityPerStage, simResult1$futilityPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$earlyStop, simResult1$earlyStop, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$successPerStage, simResult1$successPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$selectedPopulations, simResult1$selectedPopulations, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$numberOfPopulations, simResult1$numberOfPopulations, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$expectedNumberOfEvents, simResult1$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$singleNumberOfEventsPerStage, simResult1$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$conditionalPowerAchieved, simResult1$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(simResult1), "character")
	    df <- as.data.frame(simResult1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

	suppressWarnings(simResult2 <- getSimulationEnrichmentSurvival(design,
	    populations = 2,
	    plannedEvents = c(40, 120),
	    effectList = effectList,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    typeOfSelection = "rbest",
	    rValue = 2,
	    intersectionTest = "Simes",
	    directionUpper = TRUE,
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult2' with expected results
	expect_equal(simResult2$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100))
	expect_equal(simResult2$iterations[2, ], c(100, 99, 97, 95, 90, 85, 70))
	expect_equal(simResult2$rejectAtLeastOne, c(0.13, 0.19, 0.43, 0.57, 0.8, 0.86, 0.94), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$rejectedPopulationsPerStage)), c(0, 0.02, 0.02, 0.07, 0.05, 0.15, 0.05, 0.28, 0.16, 0.51, 0.21, 0.62, 0.37, 0.54, 0.05, 0.07, 0.01, 0.15, 0.1, 0.31, 0.15, 0.4, 0.16, 0.56, 0.22, 0.57, 0.35, 0.54), tolerance = 1e-07)
	expect_equal(simResult2$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0))
	expect_equal(simResult2$earlyStop[1, ], c(0, 0.01, 0.03, 0.05, 0.1, 0.15, 0.3), tolerance = 1e-07)
	expect_equal(simResult2$successPerStage[1, ], c(0, 0.01, 0.03, 0.05, 0.1, 0.15, 0.3), tolerance = 1e-07)
	expect_equal(simResult2$successPerStage[2, ], c(0.01, 0.05, 0.15, 0.26, 0.49, 0.61, 0.56), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$selectedPopulations)), c(1, 1, 1, 0.99, 1, 0.97, 1, 0.95, 1, 0.9, 1, 0.85, 1, 0.7, 1, 1, 1, 0.99, 1, 0.97, 1, 0.95, 1, 0.9, 1, 0.85, 1, 0.7), tolerance = 1e-07)
	expect_equal(simResult2$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2))
	expect_equal(simResult2$numberOfPopulations[2, ], c(2, 2, 2, 2, 2, 2, 2))
	expect_equal(simResult2$expectedNumberOfEvents, c(120, 119.2, 117.6, 116, 112, 108, 96), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$singleNumberOfEventsPerStage)), c(14.163599, 28.327198, 15.080284, 30.160567, 16, 32, 16.925752, 33.851505, 17.861157, 35.722315, 18.810731, 37.621462, 19.780244, 39.560487, 25.836401, 51.672802, 24.919716, 49.839433, 24, 48, 23.074248, 46.148495, 22.138843, 44.277685, 21.189269, 42.378538, 20.219756, 40.439513), tolerance = 1e-07)
	expect_equal(simResult2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult2$conditionalPowerAchieved[2, ], c(0.13647132, 0.29139937, 0.31543636, 0.4252398, 0.58131066, 0.58903037, 0.63717343), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult2), NA)))
	    expect_output(print(simResult2)$show())
	    invisible(capture.output(expect_error(summary(simResult2), NA)))
	    expect_output(summary(simResult2)$show())
	    suppressWarnings(simResult2CodeBased <- eval(parse(text = getObjectRCode(simResult2, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult2CodeBased$eventsPerStage, simResult2$eventsPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$iterations, simResult2$iterations, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$rejectAtLeastOne, simResult2$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$rejectedPopulationsPerStage, simResult2$rejectedPopulationsPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$futilityPerStage, simResult2$futilityPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$earlyStop, simResult2$earlyStop, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$successPerStage, simResult2$successPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$selectedPopulations, simResult2$selectedPopulations, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$numberOfPopulations, simResult2$numberOfPopulations, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$expectedNumberOfEvents, simResult2$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$singleNumberOfEventsPerStage, simResult2$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$conditionalPowerAchieved, simResult2$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(simResult2), "character")
	    df <- as.data.frame(simResult2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	suppressWarnings(simResult3 <- getSimulationEnrichmentSurvival(design,
	    populations = 2,
	    plannedEvents = c(40, 120),
	    effectList = effectList,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    typeOfSelection = "best",
	    intersectionTest = "Sidak",
	    directionUpper = TRUE,
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult3' with expected results
	expect_equal(simResult3$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100))
	expect_equal(simResult3$iterations[2, ], c(100, 99, 97, 96, 90, 85, 72))
	expect_equal(simResult3$rejectAtLeastOne, c(0.14, 0.16, 0.4, 0.67, 0.84, 0.92, 0.95), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult3$rejectedPopulationsPerStage)), c(0, 0.01, 0.02, 0.04, 0.05, 0.15, 0.04, 0.36, 0.16, 0.46, 0.21, 0.58, 0.35, 0.45, 0.05, 0.08, 0.01, 0.1, 0.1, 0.13, 0.14, 0.23, 0.16, 0.17, 0.22, 0.09, 0.33, 0.11), tolerance = 1e-07)
	expect_equal(simResult3$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0))
	expect_equal(simResult3$earlyStop[1, ], c(0, 0.01, 0.03, 0.04, 0.1, 0.15, 0.28), tolerance = 1e-07)
	expect_equal(simResult3$successPerStage[1, ], c(0, 0.01, 0.03, 0.04, 0.1, 0.15, 0.28), tolerance = 1e-07)
	expect_equal(simResult3$successPerStage[2, ], c(0.11, 0.15, 0.37, 0.62, 0.74, 0.77, 0.67), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult3$selectedPopulations)), c(1, 0.32, 1, 0.41, 1, 0.48, 1, 0.49, 1, 0.56, 1, 0.68, 1, 0.52, 1, 0.68, 1, 0.58, 1, 0.49, 1, 0.47, 1, 0.34, 1, 0.17, 1, 0.2), tolerance = 1e-07)
	expect_equal(simResult3$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2))
	expect_equal(simResult3$numberOfPopulations[2, ], c(1, 1, 1, 1, 1, 1, 1))
	expect_equal(simResult3$expectedNumberOfEvents, c(120, 119.2, 117.6, 116.8, 112, 108, 97.6), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult3$singleNumberOfEventsPerStage)), c(14.163599, 44.862494, 15.080284, 50.80114, 16, 55.752577, 16.925752, 57.406466, 17.861157, 63.272875, 18.810731, 71.524292, 19.780244, 68.766802, 25.836401, 35.137506, 24.919716, 29.19886, 24, 24.247423, 23.074248, 22.593534, 22.138843, 16.727125, 21.189269, 8.4757076, 20.219756, 11.233198), tolerance = 1e-07)
	expect_equal(simResult3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult3$conditionalPowerAchieved[2, ], c(0.13647132, 0.29139937, 0.31543636, 0.43117712, 0.58131066, 0.58903037, 0.64693196), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult3), NA)))
	    expect_output(print(simResult3)$show())
	    invisible(capture.output(expect_error(summary(simResult3), NA)))
	    expect_output(summary(simResult3)$show())
	    suppressWarnings(simResult3CodeBased <- eval(parse(text = getObjectRCode(simResult3, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult3CodeBased$eventsPerStage, simResult3$eventsPerStage, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$iterations, simResult3$iterations, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$rejectAtLeastOne, simResult3$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$rejectedPopulationsPerStage, simResult3$rejectedPopulationsPerStage, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$futilityPerStage, simResult3$futilityPerStage, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$earlyStop, simResult3$earlyStop, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$successPerStage, simResult3$successPerStage, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$selectedPopulations, simResult3$selectedPopulations, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$numberOfPopulations, simResult3$numberOfPopulations, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$expectedNumberOfEvents, simResult3$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$singleNumberOfEventsPerStage, simResult3$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$conditionalPowerAchieved, simResult3$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(simResult3), "character")
	    df <- as.data.frame(simResult3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	suppressWarnings(simResult4 <- getSimulationEnrichmentSurvival(design,
	    populations = 2,
	    plannedEvents = c(40, 120),
	    effectList = effectList,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    typeOfSelection = "epsilon",
	    epsilonValue = 0.1,
	    intersectionTest = "Bonferroni",
	    directionUpper = TRUE,
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult4' with expected results
	expect_equal(simResult4$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100))
	expect_equal(simResult4$iterations[2, ], c(87, 88, 91, 93, 89, 83, 71))
	expect_equal(simResult4$rejectAtLeastOne, c(0.12, 0.15, 0.39, 0.63, 0.83, 0.91, 0.94), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult4$rejectedPopulationsPerStage)), c(0, 0.01, 0.02, 0.04, 0.05, 0.15, 0.04, 0.32, 0.16, 0.48, 0.21, 0.62, 0.35, 0.46, 0.05, 0.06, 0.01, 0.09, 0.1, 0.14, 0.14, 0.23, 0.16, 0.17, 0.22, 0.15, 0.33, 0.15), tolerance = 1e-07)
	expect_equal(simResult4$futilityPerStage[1, ], c(0.13, 0.11, 0.06, 0.03, 0.01, 0.02, 0.01), tolerance = 1e-07)
	expect_equal(simResult4$earlyStop[1, ], c(0.13, 0.12, 0.09, 0.07, 0.11, 0.17, 0.29), tolerance = 1e-07)
	expect_equal(simResult4$successPerStage[1, ], c(0, 0.01, 0.03, 0.04, 0.1, 0.15, 0.28), tolerance = 1e-07)
	expect_equal(simResult4$successPerStage[2, ], c(0.09, 0.14, 0.34, 0.55, 0.71, 0.75, 0.65), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult4$selectedPopulations)), c(1, 0.33, 1, 0.46, 1, 0.55, 1, 0.54, 1, 0.61, 1, 0.75, 1, 0.56, 1, 0.64, 1, 0.51, 1, 0.5, 1, 0.52, 1, 0.34, 1, 0.23, 1, 0.24), tolerance = 1e-07)
	expect_equal(simResult4$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2))
	expect_equal(simResult4$numberOfPopulations[2, ], c(1.1149425, 1.1022727, 1.1538462, 1.1397849, 1.0674157, 1.1807229, 1.1267606), tolerance = 1e-07)
	expect_equal(simResult4$expectedNumberOfEvents, c(109.6, 110.4, 112.8, 114.4, 111.2, 106.4, 96.8), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult4$singleNumberOfEventsPerStage)), c(14.163599, 41.987823, 15.080284, 51.115783, 16, 53.626374, 16.925752, 54.19654, 17.861157, 63.084929, 18.810731, 68.25655, 19.780244, 66.330305, 25.836401, 38.012177, 24.919716, 28.884217, 24, 26.373626, 23.074248, 25.80346, 22.138843, 16.915071, 21.189269, 11.74345, 20.219756, 13.669695), tolerance = 1e-07)
	expect_equal(simResult4$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult4$conditionalPowerAchieved[2, ], c(0.15686358, 0.32782429, 0.33623436, 0.44508606, 0.58784224, 0.60322388, 0.65604368), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult4), NA)))
	    expect_output(print(simResult4)$show())
	    invisible(capture.output(expect_error(summary(simResult4), NA)))
	    expect_output(summary(simResult4)$show())
	    suppressWarnings(simResult4CodeBased <- eval(parse(text = getObjectRCode(simResult4, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult4CodeBased$eventsPerStage, simResult4$eventsPerStage, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$iterations, simResult4$iterations, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$rejectAtLeastOne, simResult4$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$rejectedPopulationsPerStage, simResult4$rejectedPopulationsPerStage, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$futilityPerStage, simResult4$futilityPerStage, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$earlyStop, simResult4$earlyStop, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$successPerStage, simResult4$successPerStage, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$selectedPopulations, simResult4$selectedPopulations, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$numberOfPopulations, simResult4$numberOfPopulations, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$expectedNumberOfEvents, simResult4$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$singleNumberOfEventsPerStage, simResult4$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$conditionalPowerAchieved, simResult4$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(simResult4), "character")
	    df <- as.data.frame(simResult4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getSimulationEnrichmentSurvival': gMax = 3", {

	.skipTestIfDisabled()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:enrichmentDesigns}
	# @refFS[Sec.]{fs:subsec:intersectionTestsEnrichment}
	# @refFS[Sec.]{fs:subsec:adaptiveClosedTestProcedureEnrichment}
	# @refFS[Sec.]{fs:subsec:intersectionTestsEnrichment}
	# @refFS[Sec.]{fs:sec:simulationFunctions}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalEvents}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalLogRanks}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalAdjustedPrevalances}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	subGroups <- c("S1", "S12", "S2", "R")
	prevalences <- c(0.20, 0.30, 0.40, 0.1)
	hazardRatios <- matrix(c(1.432, 1.432, 1.943, 1.943, 1.432, 1.432, 1.432, 1.432, 1.943, 1.943, 1.943, 1.943, 1.943, 2.569, 1.943, 2.569), ncol = 4)
	effectList <- list(subGroups = subGroups, prevalences = prevalences, hazardRatios = hazardRatios)

	design <- getDesignInverseNormal(informationRates = c(0.4, 0.8, 1), typeOfDesign = "noEarlyEfficacy")

	suppressWarnings(simResult1 <- getSimulationEnrichmentSurvival(design,
	    plannedEvents = c(20, 40, 50),
	    effectList = effectList,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    typeOfSelection = "best",
	    intersectionTest = "Sidak",
	    directionUpper = TRUE,
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult1' with expected results
	expect_equal(simResult1$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(simResult1$iterations[2, ], c(100, 100, 100, 100))
	expect_equal(simResult1$iterations[3, ], c(100, 100, 100, 100))
	expect_equal(simResult1$rejectAtLeastOne, c(0.37, 0.35, 0.41, 0.35), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$rejectedPopulationsPerStage)), c(0, 0, 0.06, 0, 0, 0.06, 0, 0, 0.12, 0, 0, 0.15, 0, 0, 0.27, 0, 0, 0.12, 0, 0, 0.17, 0, 0, 0.12, 0, 0, 0.04, 0, 0, 0.17, 0, 0, 0.12, 0, 0, 0.08), tolerance = 1e-07)
	expect_equal(simResult1$futilityStop, c(0, 0, 0, 0))
	expect_equal(simResult1$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(simResult1$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(simResult1$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(simResult1$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(simResult1$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(simResult1$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(simResult1$successPerStage[3, ], c(0.37, 0.35, 0.41, 0.35), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$selectedPopulations)), c(1, 0.33, 0.33, 1, 0.21, 0.21, 1, 0.29, 0.29, 1, 0.48, 0.48, 1, 0.49, 0.49, 1, 0.34, 0.34, 1, 0.42, 0.42, 1, 0.29, 0.29, 1, 0.18, 0.18, 1, 0.45, 0.45, 1, 0.29, 0.29, 1, 0.23, 0.23), tolerance = 1e-07)
	expect_equal(simResult1$numberOfPopulations[1, ], c(3, 3, 3, 3))
	expect_equal(simResult1$numberOfPopulations[2, ], c(1, 1, 1, 1))
	expect_equal(simResult1$numberOfPopulations[3, ], c(1, 1, 1, 1))
	expect_equal(simResult1$expectedNumberOfEvents, c(50, 50, 50, 50))
	expect_equal(unlist(as.list(simResult1$singleNumberOfEventsPerStage)), c(3.6197209, 3.2915498, 1.6457749, 3.5373259, 3.2717967, 1.6358983, 4.2198086, 3.8135488, 1.9067744, 4.1271956, 5.2358276, 2.6179138, 8.7605581, 7.6271207, 3.8135604, 8.5611432, 8.0506265, 4.0253132, 8.4396172, 7.6333921, 3.816696, 8.2543912, 5.4792526, 2.7396263, 5.4295814, 8.6871044, 4.3435522, 5.3059889, 7.509583, 3.7547915, 5.23067, 7.9411869, 3.9705935, 5.1158714, 8.7093352, 4.3546676, 2.1901395, 0.39422512, 0.19711256, 2.595542, 1.1679939, 0.58399695, 2.1099043, 0.61187224, 0.30593612, 2.5025418, 0.57558462, 0.28779231), tolerance = 1e-07)
	expect_equal(simResult1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult1$conditionalPowerAchieved[2, ], c(0, 0, 0, 0))
	expect_equal(simResult1$conditionalPowerAchieved[3, ], c(0.40782052, 0.41157037, 0.44953877, 0.471601), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult1), NA)))
	    expect_output(print(simResult1)$show())
	    invisible(capture.output(expect_error(summary(simResult1), NA)))
	    expect_output(summary(simResult1)$show())
	    suppressWarnings(simResult1CodeBased <- eval(parse(text = getObjectRCode(simResult1, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult1CodeBased$eventsPerStage, simResult1$eventsPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$iterations, simResult1$iterations, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$rejectAtLeastOne, simResult1$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$rejectedPopulationsPerStage, simResult1$rejectedPopulationsPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$futilityStop, simResult1$futilityStop, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$futilityPerStage, simResult1$futilityPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$earlyStop, simResult1$earlyStop, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$successPerStage, simResult1$successPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$selectedPopulations, simResult1$selectedPopulations, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$numberOfPopulations, simResult1$numberOfPopulations, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$expectedNumberOfEvents, simResult1$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$singleNumberOfEventsPerStage, simResult1$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$conditionalPowerAchieved, simResult1$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(simResult1), "character")
	    df <- as.data.frame(simResult1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	suppressWarnings(simResult2 <- getSimulationEnrichmentSurvival(design,
	    plannedEvents = c(20, 40, 50),
	    effectList = effectList,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    typeOfSelection = "rbest",
	    rValue = 2,
	    intersectionTest = "Bonferroni",
	    directionUpper = TRUE,
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult2' with expected results
	expect_equal(simResult2$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(simResult2$iterations[2, ], c(85, 86, 87, 88))
	expect_equal(simResult2$iterations[3, ], c(79, 81, 84, 85))
	expect_equal(simResult2$rejectAtLeastOne, c(0.26, 0.26, 0.31, 0.28), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$rejectedPopulationsPerStage)), c(0, 0, 0.04, 0, 0, 0.06, 0, 0, 0.08, 0, 0, 0.07, 0, 0, 0.22, 0, 0, 0.16, 0, 0, 0.18, 0, 0, 0.13, 0, 0, 0.17, 0, 0, 0.21, 0, 0, 0.29, 0, 0, 0.25), tolerance = 1e-07)
	expect_equal(simResult2$futilityStop, c(0.21, 0.19, 0.16, 0.15), tolerance = 1e-07)
	expect_equal(simResult2$futilityPerStage[1, ], c(0.15, 0.14, 0.13, 0.12), tolerance = 1e-07)
	expect_equal(simResult2$futilityPerStage[2, ], c(0.06, 0.05, 0.03, 0.03), tolerance = 1e-07)
	expect_equal(simResult2$earlyStop[1, ], c(0.15, 0.14, 0.13, 0.12), tolerance = 1e-07)
	expect_equal(simResult2$earlyStop[2, ], c(0.06, 0.05, 0.03, 0.03), tolerance = 1e-07)
	expect_equal(simResult2$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(simResult2$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(simResult2$successPerStage[3, ], c(0.17, 0.17, 0.24, 0.17), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$selectedPopulations)), c(1, 0.43, 0.4, 1, 0.41, 0.38, 1, 0.4, 0.38, 1, 0.54, 0.51, 1, 0.67, 0.61, 1, 0.61, 0.57, 1, 0.58, 0.56, 1, 0.5, 0.49, 1, 0.6, 0.57, 1, 0.7, 0.67, 1, 0.76, 0.74, 1, 0.72, 0.7), tolerance = 1e-07)
	expect_equal(simResult2$numberOfPopulations[1, ], c(3, 3, 3, 3))
	expect_equal(simResult2$numberOfPopulations[2, ], c(2, 2, 2, 2))
	expect_equal(simResult2$numberOfPopulations[3, ], c(2, 2, 2, 2))
	expect_equal(simResult2$expectedNumberOfEvents, c(44.9, 45.3, 45.8, 46.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$singleNumberOfEventsPerStage)), c(3.6197209, 3.7506414, 1.8718405, 3.5373259, 3.63547, 1.8142515, 4.2198086, 4.2827326, 2.1395276, 4.1271956, 4.2345201, 2.1156817, 8.7605581, 9.0774157, 4.5302851, 8.5611432, 8.7986745, 4.3909064, 8.4396172, 8.5654651, 4.2790552, 8.2543912, 8.4690401, 4.2313635, 5.4295814, 5.625962, 2.8077608, 5.3059889, 5.453205, 2.7213772, 5.23067, 5.3086675, 2.6520546, 5.1158714, 5.2489056, 2.6224964, 2.1901395, 1.5459808, 0.79011363, 2.595542, 2.1126504, 1.0734649, 2.1099043, 1.8431348, 0.9293626, 2.5025418, 2.0475342, 1.0304584), tolerance = 1e-07)
	expect_equal(simResult2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult2$conditionalPowerAchieved[2, ], c(0, 0, 0, 0))
	expect_equal(simResult2$conditionalPowerAchieved[3, ], c(0.41272973, 0.44447643, 0.46315477, 0.46991584), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult2), NA)))
	    expect_output(print(simResult2)$show())
	    invisible(capture.output(expect_error(summary(simResult2), NA)))
	    expect_output(summary(simResult2)$show())
	    suppressWarnings(simResult2CodeBased <- eval(parse(text = getObjectRCode(simResult2, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult2CodeBased$eventsPerStage, simResult2$eventsPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$iterations, simResult2$iterations, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$rejectAtLeastOne, simResult2$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$rejectedPopulationsPerStage, simResult2$rejectedPopulationsPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$futilityStop, simResult2$futilityStop, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$futilityPerStage, simResult2$futilityPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$earlyStop, simResult2$earlyStop, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$successPerStage, simResult2$successPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$selectedPopulations, simResult2$selectedPopulations, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$numberOfPopulations, simResult2$numberOfPopulations, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$expectedNumberOfEvents, simResult2$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$singleNumberOfEventsPerStage, simResult2$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$conditionalPowerAchieved, simResult2$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(simResult2), "character")
	    df <- as.data.frame(simResult2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	suppressWarnings(simResult3 <- getSimulationEnrichmentSurvival(design,
	    plannedEvents = c(20, 40, 50),
	    effectList = effectList,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    typeOfSelection = "epsilon",
	    epsilonValue = 0.2,
	    intersectionTest = "Simes",
	    directionUpper = TRUE,
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult3' with expected results
	expect_equal(simResult3$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(simResult3$iterations[2, ], c(100, 100, 100, 100))
	expect_equal(simResult3$iterations[3, ], c(100, 100, 100, 100))
	expect_equal(simResult3$rejectAtLeastOne, c(0.36, 0.35, 0.44, 0.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult3$rejectedPopulationsPerStage)), c(0, 0, 0.07, 0, 0, 0.06, 0, 0, 0.13, 0, 0, 0.17, 0, 0, 0.27, 0, 0, 0.18, 0, 0, 0.17, 0, 0, 0.12, 0, 0, 0.05, 0, 0, 0.17, 0, 0, 0.16, 0, 0, 0.12), tolerance = 1e-07)
	expect_equal(simResult3$futilityStop, c(0, 0, 0, 0))
	expect_equal(simResult3$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(simResult3$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(simResult3$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(simResult3$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(simResult3$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(simResult3$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(simResult3$successPerStage[3, ], c(0.36, 0.34, 0.41, 0.38), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult3$selectedPopulations)), c(1, 0.42, 0.32, 1, 0.32, 0.22, 1, 0.36, 0.32, 1, 0.54, 0.5, 1, 0.59, 0.53, 1, 0.52, 0.48, 1, 0.55, 0.48, 1, 0.36, 0.31, 1, 0.38, 0.27, 1, 0.5, 0.45, 1, 0.48, 0.42, 1, 0.36, 0.33), tolerance = 1e-07)
	expect_equal(simResult3$numberOfPopulations[1, ], c(3, 3, 3, 3))
	expect_equal(simResult3$numberOfPopulations[2, ], c(1.39, 1.34, 1.39, 1.26), tolerance = 1e-07)
	expect_equal(simResult3$numberOfPopulations[3, ], c(1.12, 1.15, 1.22, 1.14), tolerance = 1e-07)
	expect_equal(simResult3$expectedNumberOfEvents, c(50, 50, 50, 50))
	expect_equal(unlist(as.list(simResult3$singleNumberOfEventsPerStage)), c(3.6197209, 3.780682, 1.6893108, 3.5373259, 3.2506085, 1.5558983, 4.2198086, 4.1738404, 2.0706939, 4.1271956, 5.1472378, 2.6010146, 8.7605581, 7.5733107, 3.8740056, 8.5611432, 8.2799243, 4.1487871, 8.4396172, 7.5735103, 3.7333396, 8.2543912, 5.8114801, 2.8436611, 5.4295814, 7.8137543, 4.1410147, 5.3059889, 7.1716962, 3.7113176, 5.23067, 7.2398952, 3.7528866, 5.1158714, 8.140367, 4.1424049, 2.1901395, 0.83225302, 0.29566884, 2.595542, 1.297771, 0.58399695, 2.1099043, 1.0127541, 0.4430799, 2.5025418, 0.90091505, 0.4129194), tolerance = 1e-07)
	expect_equal(simResult3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult3$conditionalPowerAchieved[2, ], c(0, 0, 0, 0))
	expect_equal(simResult3$conditionalPowerAchieved[3, ], c(0.38601776, 0.39399786, 0.42333822, 0.45516082), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult3), NA)))
	    expect_output(print(simResult3)$show())
	    invisible(capture.output(expect_error(summary(simResult3), NA)))
	    expect_output(summary(simResult3)$show())
	    suppressWarnings(simResult3CodeBased <- eval(parse(text = getObjectRCode(simResult3, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult3CodeBased$eventsPerStage, simResult3$eventsPerStage, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$iterations, simResult3$iterations, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$rejectAtLeastOne, simResult3$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$rejectedPopulationsPerStage, simResult3$rejectedPopulationsPerStage, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$futilityStop, simResult3$futilityStop, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$futilityPerStage, simResult3$futilityPerStage, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$earlyStop, simResult3$earlyStop, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$successPerStage, simResult3$successPerStage, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$selectedPopulations, simResult3$selectedPopulations, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$numberOfPopulations, simResult3$numberOfPopulations, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$expectedNumberOfEvents, simResult3$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$singleNumberOfEventsPerStage, simResult3$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$conditionalPowerAchieved, simResult3$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(simResult3), "character")
	    df <- as.data.frame(simResult3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getSimulationEnrichmentSurvival': gMax = 4", {

	.skipTestIfDisabled()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:enrichmentDesigns}
	# @refFS[Sec.]{fs:subsec:intersectionTestsEnrichment}
	# @refFS[Sec.]{fs:subsec:adaptiveClosedTestProcedureEnrichment}
	# @refFS[Sec.]{fs:subsec:intersectionTestsEnrichment}
	# @refFS[Sec.]{fs:sec:simulationFunctions}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalEvents}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalLogRanks}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalAdjustedPrevalances}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	subGroups <- c("S1", "S2", "S3", "S12", "S13", "S23", "S123", "R")
	prevalences <- c(0.1, 0.05, 0.1, 0.15, 0.1, 0.15, 0.3, 0.05)
	hazardRatios <- matrix(c(seq(1, 1.75, 0.25), seq(1, 1.75, 0.25)), ncol = 8)

	effectList <- list(subGroups = subGroups, prevalences = prevalences, hazardRatios = hazardRatios)

	design <- getDesignInverseNormal(informationRates = c(0.4, 1), typeOfDesign = "noEarlyEfficacy")

	suppressWarnings(simResult1 <- getSimulationEnrichmentSurvival(design,
	    plannedEvents = c(100, 200),
	    effectList = effectList,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 2,
	    typeOfSelection = "epsilon",
	    epsilonValue = 0.15,
	    adaptations = c(T),
	    intersectionTest = "Sidak",
	    seed = 123
	))


	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult1' with expected results
	expect_equal(simResult1$iterations[1, ], 100)
	expect_equal(simResult1$iterations[2, ], 100)
	expect_equal(simResult1$rejectAtLeastOne, 0.57, tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$rejectedPopulationsPerStage)), c(0, 0.16, 0, 0.3, 0, 0.2, 0, 0.12), tolerance = 1e-07)
	expect_equal(simResult1$futilityPerStage[1, ], 0)
	expect_equal(simResult1$earlyStop[1, ], 0)
	expect_equal(simResult1$successPerStage[1, ], 0)
	expect_equal(simResult1$successPerStage[2, ], 0.51, tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$selectedPopulations)), c(1, 0.45, 1, 0.58, 1, 0.44, 1, 0.44), tolerance = 1e-07)
	expect_equal(simResult1$numberOfPopulations[1, ], 4)
	expect_equal(simResult1$numberOfPopulations[2, ], 1.91, tolerance = 1e-07)
	expect_equal(simResult1$expectedNumberOfEvents, 200)
	expect_equal(unlist(as.list(simResult1$singleNumberOfEventsPerStage)), c(7.8947368, 5.6940341, 4.6052632, 3.8122818, 10.526316, 7.8604058, 17.763158, 18.4485, 7.8947368, 7.9435029, 13.815789, 14.40569, 31.578947, 39.230322, 5.9210526, 2.6052632), tolerance = 1e-07)
	expect_equal(simResult1$conditionalPowerAchieved[1, ], NA_real_)
	expect_equal(simResult1$conditionalPowerAchieved[2, ], 0.42595791, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult1), NA)))
	    expect_output(print(simResult1)$show())
	    invisible(capture.output(expect_error(summary(simResult1), NA)))
	    expect_output(summary(simResult1)$show())
	    suppressWarnings(simResult1CodeBased <- eval(parse(text = getObjectRCode(simResult1, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult1CodeBased$eventsPerStage, simResult1$eventsPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$iterations, simResult1$iterations, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$rejectAtLeastOne, simResult1$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$rejectedPopulationsPerStage, simResult1$rejectedPopulationsPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$futilityPerStage, simResult1$futilityPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$earlyStop, simResult1$earlyStop, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$successPerStage, simResult1$successPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$selectedPopulations, simResult1$selectedPopulations, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$numberOfPopulations, simResult1$numberOfPopulations, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$expectedNumberOfEvents, simResult1$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$singleNumberOfEventsPerStage, simResult1$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$conditionalPowerAchieved, simResult1$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(simResult1), "character")
	    df <- as.data.frame(simResult1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
	suppressWarnings(simResult2 <- getSimulationEnrichmentSurvival(design,
	    plannedEvents = c(100, 200),
	    effectList = effectList,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 2,
	    typeOfSelection = "rBest",
	    rValue = 2,
	    adaptations = c(T),
	    intersectionTest = "Bonferroni",
	    seed = 123
	))


	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult2' with expected results
	expect_equal(simResult2$iterations[1, ], 100)
	expect_equal(simResult2$iterations[2, ], 90)
	expect_equal(simResult2$rejectAtLeastOne, 0.51, tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$rejectedPopulationsPerStage)), c(0, 0.19, 0, 0.3, 0, 0.22, 0, 0.17), tolerance = 1e-07)
	expect_equal(simResult2$futilityPerStage[1, ], 0.1, tolerance = 1e-07)
	expect_equal(simResult2$earlyStop[1, ], 0.1, tolerance = 1e-07)
	expect_equal(simResult2$successPerStage[1, ], 0)
	expect_equal(simResult2$successPerStage[2, ], 0.37, tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$selectedPopulations)), c(1, 0.41, 1, 0.59, 1, 0.39, 1, 0.41), tolerance = 1e-07)
	expect_equal(simResult2$numberOfPopulations[1, ], 4)
	expect_equal(simResult2$numberOfPopulations[2, ], 2)
	expect_equal(simResult2$expectedNumberOfEvents, 190)
	expect_equal(unlist(as.list(simResult2$singleNumberOfEventsPerStage)), c(7.8947368, 6.4805425, 4.6052632, 4.5696194, 10.526316, 8.690946, 17.763158, 19.390381, 7.8947368, 8.6179471, 13.815789, 15.081407, 31.578947, 34.471788, 5.9210526, 2.6973684), tolerance = 1e-07)
	expect_equal(simResult2$conditionalPowerAchieved[1, ], NA_real_)
	expect_equal(simResult2$conditionalPowerAchieved[2, ], 0.47328657, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult2), NA)))
	    expect_output(print(simResult2)$show())
	    invisible(capture.output(expect_error(summary(simResult2), NA)))
	    expect_output(summary(simResult2)$show())
	    suppressWarnings(simResult2CodeBased <- eval(parse(text = getObjectRCode(simResult2, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult2CodeBased$eventsPerStage, simResult2$eventsPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$iterations, simResult2$iterations, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$rejectAtLeastOne, simResult2$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$rejectedPopulationsPerStage, simResult2$rejectedPopulationsPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$futilityPerStage, simResult2$futilityPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$earlyStop, simResult2$earlyStop, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$successPerStage, simResult2$successPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$selectedPopulations, simResult2$selectedPopulations, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$numberOfPopulations, simResult2$numberOfPopulations, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$expectedNumberOfEvents, simResult2$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$singleNumberOfEventsPerStage, simResult2$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$conditionalPowerAchieved, simResult2$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(simResult2), "character")
	    df <- as.data.frame(simResult2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
})

test_that("'getSimulationEnrichmentSurvival': comparison of base and enrichment for inverse normal", {

	.skipTestIfDisabled()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:enrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulationFunctions}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalEvents}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalLogRanks}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	effectList <- list(subGroups = "F", prevalences = 1, stDevs = 1.3, hazardRatios = matrix(seq(0.6, 1, 0.05), ncol = 1))

	design <- getDesignInverseNormal(informationRates = c(0.3, 0.7, 1), typeOfDesign = "asKD", gammaA = 2.4)

	suppressWarnings(x1 <- getSimulationEnrichmentSurvival(design,
	    populations = 1,
	    plannedEvents = c(50, 100, 180), effectList = effectList,
	    maxNumberOfIterations = 100, allocationRatioPlanned = 1,
	    directionUpper = FALSE,
	    conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA, 10, 10), maxNumberOfEventsPerStage = c(NA, 100, 100),
	    seed = 123
	))

	x2 <- getSimulationSurvival(design,
	    plannedEvents = c(50, 100, 180), hazardRatio = seq(0.6, 1, 0.05),
	    directionUpper = FALSE,
	    maxNumberOfSubjects = 1500, maxNumberOfIterations = 100,
	    allocation1 = 1, allocation2 = 1, longTimeSimulationAllowed = TRUE,
	    conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA, 10, 10), maxNumberOfEventsPerStage = c(NA, 100, 100),
	    seed = 123
	)

	comp1 <- x2$overallReject - x1$rejectAtLeastOne

	## Comparison of the results of numeric object 'comp1' with expected results
	expect_equal(comp1, c(-0.05, 0.03, 0.03, 0.01, 0.07, 0.08, 0.05, -0.05, -0.02), tolerance = 1e-07)

	comp2 <- x2$conditionalPowerAchieved - x1$conditionalPowerAchieved

	## Comparison of the results of matrixarray object 'comp2' with expected results
	expect_equal(comp2[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(comp2[2, ], c(-0.022566213, -0.0056751237, 0.047207778, 0.035251356, 0.033740719, -0.051453144, 0.039406427, 0.0072692294, -0.022722897), tolerance = 1e-07)
	expect_equal(comp2[3, ], c(0.025359011, -0.021253382, 0.092581664, -0.080566447, 0.087298305, -0.050787114, 0.070673698, 0.019777739, -0.019114098), tolerance = 1e-07)

	comp3 <- x2$expectedNumberOfEvents - x1$expectedNumberOfEvents

	## Comparison of the results of numeric object 'comp3' with expected results
	expect_equal(comp3, c(5.6713987, 8.8976119, -9.7670181, -2.0326559, -2.7081522, -0.88153519, -5.5780096, 3.3199537, 1.2334371), tolerance = 1e-07)

})

test_that("'getSimulationEnrichmentSurvival': comparison of base and enrichment for Fisher combination", {

	.skipTestIfDisabled()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:enrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulationFunctions}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalEvents}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalLogRanks}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	effectList <- list(subGroups = "F", prevalences = 1, stDevs = 1.3, hazardRatios = matrix(seq(0.6, 1, 0.05), ncol = 1))

	design <- getDesignFisher(informationRates = c(0.3, 0.6, 1))

	suppressWarnings(x1 <- getSimulationEnrichmentSurvival(design,
	    populations = 1,
	    plannedEvents = c(50, 100, 180), effectList = effectList,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    directionUpper = FALSE,
	    conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA, 10, 10), maxNumberOfEventsPerStage = c(NA, 100, 100),
	    seed = 123
	))

	x2 <- getSimulationSurvival(design,
	    plannedEvents = c(50, 100, 180), hazardRatio = seq(0.6, 1, 0.05),
	    directionUpper = FALSE,
	    maxNumberOfSubjects = 1500, maxNumberOfIterations = 100,
	    allocation1 = 1, allocation2 = 1,
	    conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA, 10, 10), maxNumberOfEventsPerStage = c(NA, 100, 100),
	    seed = 123
	)

	comp4 <- x2$overallReject - x1$rejectAtLeastOne

	## Comparison of the results of numeric object 'comp4' with expected results
	expect_equal(comp4, c(-0.08, 0.02, 0.12, 0.02, 0.04, 0.04, 0.04, -0.03, 0), tolerance = 1e-07)

	comp5 <- x2$conditionalPowerAchieved - x1$conditionalPowerAchieved

	## Comparison of the results of matrixarray object 'comp5' with expected results
	expect_equal(comp5[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(comp5[2, ], c(-0.067329229, 0.0040653837, 0.025600632, 0.024680224, 0.025189093, -0.043591198, 0.033525993, -0.0055417344, -0.031790612), tolerance = 1e-07)
	expect_equal(comp5[3, ], c(0.012384997, 0.030980232, 0.047012202, -0.035304718, 0.068468504, 0.00374058, 0.042913189, -0.015210788, -0.017776302), tolerance = 1e-07)

	comp6 <- x2$expectedNumberOfEvents - x1$expectedNumberOfEvents

	## Comparison of the results of numeric object 'comp6' with expected results
	expect_equal(comp6, c(5.1347448, 9.1286427, -16.823834, -1.3136156, 0.71128925, 1.9694657, -7.1208497, -0.94699441, -0.085337992), tolerance = 1e-07)

})

