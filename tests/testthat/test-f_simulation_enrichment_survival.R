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
## |  Creation date: 23 February 2022, 14:07:08
## |  File version: $Revision: 6279 $
## |  Last changed: $Date: 2022-06-09 17:48:13 +0200 (Thu, 09 Jun 2022) $
## |  Last changed by: $Author: pahlke $
## |  

context("Testing Simulation Enrichment Survival Function")


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
	expect_equal(simResult1$iterations[2, ], c(100, 100, 100, 100, 96, 92, 89))
	expect_equal(simResult1$rejectAtLeastOne, c(0.15, 0.25, 0.57, 0.69, 0.93, 0.97, 1), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$rejectedPopulationsPerStage)), c(0, 0.01, 0.02, 0.06, 0.05, 0.16, 0.03, 0.3, 0.17, 0.52, 0.19, 0.66, 0.35, 0.58, 0.04, 0.1, 0.01, 0.18, 0.12, 0.34, 0.12, 0.47, 0.23, 0.54, 0.18, 0.71, 0.29, 0.6), tolerance = 1e-07)
	expect_equal(simResult1$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0))
	expect_equal(simResult1$earlyStop[1, ], c(0, 0, 0, 0, 0.04, 0.08, 0.11), tolerance = 1e-07)
	expect_equal(simResult1$successPerStage[1, ], c(0, 0, 0, 0, 0.04, 0.08, 0.11), tolerance = 1e-07)
	expect_equal(simResult1$successPerStage[2, ], c(0, 0.02, 0.1, 0.23, 0.49, 0.69, 0.71), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$selectedPopulations)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0.96, 1, 0.92, 1, 0.89, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.96, 1, 0.92, 1, 0.89), tolerance = 1e-07)
	expect_equal(simResult1$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2))
	expect_equal(simResult1$numberOfPopulations[2, ], c(2, 2, 2, 2, 2, 2, 2))
	expect_equal(simResult1$expectedNumberOfEvents, c(120, 120, 120, 120, 116.8, 113.6, 111.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$singleNumberOfEventsPerStage)), c(14.163599, 28.327198, 15.080284, 30.160567, 16, 32, 16.925752, 33.851505, 17.861157, 35.722315, 18.810731, 37.621462, 19.780244, 39.560487, 25.836401, 51.672802, 24.919716, 49.839433, 24, 48, 23.074248, 46.148495, 22.138843, 44.277685, 21.189269, 42.378538, 20.219756, 40.439513), tolerance = 1e-07)
	expect_equal(simResult1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult1$conditionalPowerAchieved[2, ], c(0.089064808, 0.19781497, 0.26401795, 0.43542559, 0.54534009, 0.59195655, 0.7433686), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult1), NA)))
	    expect_output(print(simResult1)$show())
	    invisible(capture.output(expect_error(summary(simResult1), NA)))
	    expect_output(summary(simResult1)$show())
	    suppressWarnings(simResult1CodeBased <- eval(parse(text = getObjectRCode(simResult1, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult1CodeBased$iterations, simResult1$iterations, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$rejectAtLeastOne, simResult1$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$rejectedPopulationsPerStage, simResult1$rejectedPopulationsPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$futilityPerStage, simResult1$futilityPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$earlyStop, simResult1$earlyStop, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$successPerStage, simResult1$successPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$selectedPopulations, simResult1$selectedPopulations, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$numberOfPopulations, simResult1$numberOfPopulations, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$expectedNumberOfEvents, simResult1$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$eventsPerStage, simResult1$eventsPerStage, tolerance = 1e-05)
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
	expect_equal(simResult2$iterations[2, ], c(100, 100, 100, 100, 96, 91, 89))
	expect_equal(simResult2$rejectAtLeastOne, c(0.13, 0.23, 0.56, 0.66, 0.93, 0.95, 1), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$rejectedPopulationsPerStage)), c(0, 0, 0.02, 0.05, 0.05, 0.14, 0.03, 0.29, 0.16, 0.53, 0.2, 0.64, 0.35, 0.58, 0.04, 0.09, 0.01, 0.17, 0.12, 0.35, 0.12, 0.45, 0.23, 0.54, 0.18, 0.69, 0.28, 0.61), tolerance = 1e-07)
	expect_equal(simResult2$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0))
	expect_equal(simResult2$earlyStop[1, ], c(0, 0, 0, 0, 0.04, 0.09, 0.11), tolerance = 1e-07)
	expect_equal(simResult2$successPerStage[1, ], c(0, 0, 0, 0, 0.04, 0.09, 0.11), tolerance = 1e-07)
	expect_equal(simResult2$successPerStage[2, ], c(0, 0.02, 0.1, 0.23, 0.49, 0.67, 0.71), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$selectedPopulations)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0.96, 1, 0.91, 1, 0.89, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.96, 1, 0.91, 1, 0.89), tolerance = 1e-07)
	expect_equal(simResult2$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2))
	expect_equal(simResult2$numberOfPopulations[2, ], c(2, 2, 2, 2, 2, 2, 2))
	expect_equal(simResult2$expectedNumberOfEvents, c(120, 120, 120, 120, 116.8, 112.8, 111.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$singleNumberOfEventsPerStage)), c(14.163599, 28.327198, 15.080284, 30.160567, 16, 32, 16.925752, 33.851505, 17.861157, 35.722315, 18.810731, 37.621462, 19.780244, 39.560487, 25.836401, 51.672802, 24.919716, 49.839433, 24, 48, 23.074248, 46.148495, 22.138843, 44.277685, 21.189269, 42.378538, 20.219756, 40.439513), tolerance = 1e-07)
	expect_equal(simResult2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult2$conditionalPowerAchieved[2, ], c(0.089064808, 0.19781497, 0.26401795, 0.43542559, 0.54534009, 0.58757942, 0.7433686), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult2), NA)))
	    expect_output(print(simResult2)$show())
	    invisible(capture.output(expect_error(summary(simResult2), NA)))
	    expect_output(summary(simResult2)$show())
	    suppressWarnings(simResult2CodeBased <- eval(parse(text = getObjectRCode(simResult2, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult2CodeBased$iterations, simResult2$iterations, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$rejectAtLeastOne, simResult2$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$rejectedPopulationsPerStage, simResult2$rejectedPopulationsPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$futilityPerStage, simResult2$futilityPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$earlyStop, simResult2$earlyStop, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$successPerStage, simResult2$successPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$selectedPopulations, simResult2$selectedPopulations, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$numberOfPopulations, simResult2$numberOfPopulations, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$expectedNumberOfEvents, simResult2$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$eventsPerStage, simResult2$eventsPerStage, tolerance = 1e-05)
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
	expect_equal(simResult3$iterations[2, ], c(100, 100, 100, 100, 96, 92, 89))
	expect_equal(simResult3$rejectAtLeastOne, c(0.14, 0.18, 0.55, 0.72, 0.89, 0.96, 0.99), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult3$rejectedPopulationsPerStage)), c(0, 0.01, 0.02, 0.04, 0.05, 0.16, 0.03, 0.36, 0.16, 0.45, 0.19, 0.48, 0.35, 0.36, 0.04, 0.09, 0.01, 0.11, 0.12, 0.22, 0.12, 0.24, 0.23, 0.09, 0.17, 0.21, 0.28, 0.13), tolerance = 1e-07)
	expect_equal(simResult3$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0))
	expect_equal(simResult3$earlyStop[1, ], c(0, 0, 0, 0, 0.04, 0.08, 0.11), tolerance = 1e-07)
	expect_equal(simResult3$successPerStage[1, ], c(0, 0, 0, 0, 0.04, 0.08, 0.11), tolerance = 1e-07)
	expect_equal(simResult3$successPerStage[2, ], c(0.14, 0.18, 0.55, 0.72, 0.85, 0.88, 0.88), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult3$selectedPopulations)), c(1, 0.36, 1, 0.46, 1, 0.52, 1, 0.51, 1, 0.61, 1, 0.62, 1, 0.6, 1, 0.64, 1, 0.54, 1, 0.48, 1, 0.49, 1, 0.35, 1, 0.3, 1, 0.29), tolerance = 1e-07)
	expect_equal(simResult3$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2))
	expect_equal(simResult3$numberOfPopulations[2, ], c(1, 1, 1, 1, 1, 1, 1))
	expect_equal(simResult3$expectedNumberOfEvents, c(120, 120, 120, 120, 116.8, 113.6, 111.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult3$singleNumberOfEventsPerStage)), c(14.163599, 46.929406, 15.080284, 53.086706, 16, 56.96, 16.925752, 57.387237, 17.861157, 63.857094, 18.810731, 66.180911, 19.780244, 66.82308, 25.836401, 33.070594, 24.919716, 26.913294, 24, 23.04, 23.074248, 22.612763, 22.138843, 16.142906, 21.189269, 13.819089, 20.219756, 13.17692), tolerance = 1e-07)
	expect_equal(simResult3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult3$conditionalPowerAchieved[2, ], c(0.089064808, 0.19781497, 0.26401795, 0.43542559, 0.54534009, 0.59195655, 0.7433686), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult3), NA)))
	    expect_output(print(simResult3)$show())
	    invisible(capture.output(expect_error(summary(simResult3), NA)))
	    expect_output(summary(simResult3)$show())
	    suppressWarnings(simResult3CodeBased <- eval(parse(text = getObjectRCode(simResult3, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult3CodeBased$iterations, simResult3$iterations, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$rejectAtLeastOne, simResult3$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$rejectedPopulationsPerStage, simResult3$rejectedPopulationsPerStage, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$futilityPerStage, simResult3$futilityPerStage, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$earlyStop, simResult3$earlyStop, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$successPerStage, simResult3$successPerStage, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$selectedPopulations, simResult3$selectedPopulations, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$numberOfPopulations, simResult3$numberOfPopulations, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$expectedNumberOfEvents, simResult3$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$eventsPerStage, simResult3$eventsPerStage, tolerance = 1e-05)
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
	expect_equal(simResult4$iterations[2, ], c(95, 95, 99, 99, 96, 92, 89))
	expect_equal(simResult4$rejectAtLeastOne, c(0.13, 0.17, 0.54, 0.71, 0.87, 0.96, 0.99), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult4$rejectedPopulationsPerStage)), c(0, 0.01, 0.02, 0.05, 0.05, 0.15, 0.03, 0.35, 0.16, 0.43, 0.19, 0.5, 0.35, 0.39, 0.04, 0.08, 0.01, 0.1, 0.12, 0.23, 0.12, 0.24, 0.23, 0.11, 0.17, 0.24, 0.28, 0.15), tolerance = 1e-07)
	expect_equal(simResult4$futilityPerStage[1, ], c(0.05, 0.05, 0.01, 0.01, 0, 0, 0), tolerance = 1e-07)
	expect_equal(simResult4$earlyStop[1, ], c(0.05, 0.05, 0.01, 0.01, 0.04, 0.08, 0.11), tolerance = 1e-07)
	expect_equal(simResult4$successPerStage[1, ], c(0, 0, 0, 0, 0.04, 0.08, 0.11), tolerance = 1e-07)
	expect_equal(simResult4$successPerStage[2, ], c(0.11, 0.17, 0.5, 0.67, 0.82, 0.87, 0.87), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult4$selectedPopulations)), c(1, 0.38, 1, 0.51, 1, 0.57, 1, 0.54, 1, 0.62, 1, 0.65, 1, 0.63, 1, 0.63, 1, 0.54, 1, 0.52, 1, 0.5, 1, 0.38, 1, 0.34, 1, 0.32), tolerance = 1e-07)
	expect_equal(simResult4$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2))
	expect_equal(simResult4$numberOfPopulations[2, ], c(1.0631579, 1.1052632, 1.1010101, 1.0505051, 1.0416667, 1.076087, 1.0674157), tolerance = 1e-07)
	expect_equal(simResult4$expectedNumberOfEvents, c(116, 116, 119.2, 119.2, 116.8, 113.6, 111.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult4$singleNumberOfEventsPerStage)), c(14.163599, 45.732773, 15.080284, 51.670217, 16, 54.787879, 16.925752, 56.692679, 17.861157, 62.473416, 18.810731, 64.338366, 19.780244, 65.45995, 25.836401, 34.267227, 24.919716, 28.329783, 24, 25.212121, 23.074248, 23.307321, 22.138843, 17.526584, 21.189269, 15.661634, 20.219756, 14.54005), tolerance = 1e-07)
	expect_equal(simResult4$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult4$conditionalPowerAchieved[2, ], c(0.093752429, 0.20822629, 0.2666848, 0.43982382, 0.54534009, 0.59195655, 0.7433686), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult4), NA)))
	    expect_output(print(simResult4)$show())
	    invisible(capture.output(expect_error(summary(simResult4), NA)))
	    expect_output(summary(simResult4)$show())
	    suppressWarnings(simResult4CodeBased <- eval(parse(text = getObjectRCode(simResult4, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult4CodeBased$iterations, simResult4$iterations, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$rejectAtLeastOne, simResult4$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$rejectedPopulationsPerStage, simResult4$rejectedPopulationsPerStage, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$futilityPerStage, simResult4$futilityPerStage, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$earlyStop, simResult4$earlyStop, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$successPerStage, simResult4$successPerStage, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$selectedPopulations, simResult4$selectedPopulations, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$numberOfPopulations, simResult4$numberOfPopulations, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$expectedNumberOfEvents, simResult4$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$eventsPerStage, simResult4$eventsPerStage, tolerance = 1e-05)
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
	expect_equal(simResult1$rejectAtLeastOne, c(0.54, 0.47, 0.52, 0.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$rejectedPopulationsPerStage)), c(0, 0, 0.06, 0, 0, 0.09, 0, 0, 0.1, 0, 0, 0.14, 0, 0, 0.23, 0, 0, 0.23, 0, 0, 0.22, 0, 0, 0.16, 0, 0, 0.25, 0, 0, 0.15, 0, 0, 0.2, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(simResult1$futilityStop, c(0, 0, 0, 0))
	expect_equal(simResult1$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(simResult1$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(simResult1$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(simResult1$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(simResult1$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(simResult1$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(simResult1$successPerStage[3, ], c(0.54, 0.47, 0.52, 0.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$selectedPopulations)), c(1, 0.2, 0.2, 1, 0.31, 0.31, 1, 0.22, 0.22, 1, 0.32, 0.32, 1, 0.39, 0.39, 1, 0.38, 0.38, 1, 0.44, 0.44, 1, 0.36, 0.36, 1, 0.41, 0.41, 1, 0.31, 0.31, 1, 0.34, 0.34, 1, 0.32, 0.32), tolerance = 1e-07)
	expect_equal(simResult1$numberOfPopulations[1, ], c(3, 3, 3, 3))
	expect_equal(simResult1$numberOfPopulations[2, ], c(1, 1, 1, 1))
	expect_equal(simResult1$numberOfPopulations[3, ], c(1, 1, 1, 1))
	expect_equal(simResult1$expectedNumberOfEvents, c(50, 50, 50, 50))
	expect_equal(unlist(as.list(simResult1$singleNumberOfEventsPerStage)), c(3.6197209, 3.0840856, 1.5420428, 3.5373259, 3.576571, 1.7882855, 4.2198086, 3.399414, 1.699707, 4.1271956, 4.1784177, 2.0892088, 8.7605581, 8.4073103, 4.2036551, 8.5611432, 7.345962, 3.672981, 8.4396172, 8.3023207, 4.1511603, 8.2543912, 7.086465, 3.5432325, 5.4295814, 7.6106469, 3.8053235, 5.3059889, 8.272849, 4.1364245, 5.23067, 7.5808978, 3.7904489, 5.1158714, 7.934304, 3.967152, 2.1901395, 0.89795721, 0.4489786, 2.595542, 0.80461801, 0.40230901, 2.1099043, 0.71736746, 0.35868373, 2.5025418, 0.80081338, 0.40040669), tolerance = 1e-07)
	expect_equal(simResult1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult1$conditionalPowerAchieved[2, ], c(0, 0, 0, 0))
	expect_equal(simResult1$conditionalPowerAchieved[3, ], c(0.55391579, 0.55749656, 0.54642294, 0.56972787), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult1), NA)))
	    expect_output(print(simResult1)$show())
	    invisible(capture.output(expect_error(summary(simResult1), NA)))
	    expect_output(summary(simResult1)$show())
	    suppressWarnings(simResult1CodeBased <- eval(parse(text = getObjectRCode(simResult1, stringWrapParagraphWidth = NULL))))
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
	    expect_equal(simResult1CodeBased$eventsPerStage, simResult1$eventsPerStage, tolerance = 1e-05)
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
	expect_equal(simResult2$iterations[2, ], c(96, 98, 99, 99))
	expect_equal(simResult2$iterations[3, ], c(95, 95, 94, 97))
	expect_equal(simResult2$rejectAtLeastOne, c(0.41, 0.43, 0.42, 0.44), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$rejectedPopulationsPerStage)), c(0, 0, 0.03, 0, 0, 0.06, 0, 0, 0.09, 0, 0, 0.11, 0, 0, 0.19, 0, 0, 0.22, 0, 0, 0.18, 0, 0, 0.18, 0, 0, 0.29, 0, 0, 0.21, 0, 0, 0.27, 0, 0, 0.28), tolerance = 1e-07)
	expect_equal(simResult2$futilityStop, c(0.05, 0.05, 0.06, 0.03), tolerance = 1e-07)
	expect_equal(simResult2$futilityPerStage[1, ], c(0.04, 0.02, 0.01, 0.01), tolerance = 1e-07)
	expect_equal(simResult2$futilityPerStage[2, ], c(0.01, 0.03, 0.05, 0.02), tolerance = 1e-07)
	expect_equal(simResult2$earlyStop[1, ], c(0.04, 0.02, 0.01, 0.01), tolerance = 1e-07)
	expect_equal(simResult2$earlyStop[2, ], c(0.01, 0.03, 0.05, 0.02), tolerance = 1e-07)
	expect_equal(simResult2$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(simResult2$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(simResult2$successPerStage[3, ], c(0.1, 0.06, 0.12, 0.13), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$selectedPopulations)), c(1, 0.46, 0.46, 1, 0.56, 0.53, 1, 0.53, 0.49, 1, 0.61, 0.59, 1, 0.73, 0.72, 1, 0.71, 0.7, 1, 0.73, 0.69, 1, 0.64, 0.63, 1, 0.73, 0.72, 1, 0.69, 0.67, 1, 0.72, 0.7, 1, 0.73, 0.72), tolerance = 1e-07)
	expect_equal(simResult2$numberOfPopulations[1, ], c(3, 3, 3, 3))
	expect_equal(simResult2$numberOfPopulations[2, ], c(2, 2, 2, 2))
	expect_equal(simResult2$numberOfPopulations[3, ], c(2, 2, 2, 2))
	expect_equal(simResult2$expectedNumberOfEvents, c(48.7, 49.1, 49.2, 49.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$singleNumberOfEventsPerStage)), c(3.6197209, 3.7263665, 1.8637446, 3.5373259, 3.6934301, 1.8464034, 4.2198086, 4.3555372, 2.1734368, 4.1271956, 4.2822198, 2.1396654, 8.7605581, 9.0186651, 4.510691, 8.5611432, 8.9389513, 4.4687214, 8.4396172, 8.7110744, 4.3468737, 8.2543912, 8.5644397, 4.2793308, 5.4295814, 5.5895498, 2.7956168, 5.3059889, 5.5401452, 2.7696051, 5.23067, 5.3989126, 2.6940868, 5.1158714, 5.3080319, 2.6522254, 2.1901395, 1.6654186, 0.82994761, 2.595542, 1.8274734, 0.91527007, 2.1099043, 1.5344758, 0.78560266, 2.5025418, 1.8453086, 0.9287784), tolerance = 1e-07)
	expect_equal(simResult2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult2$conditionalPowerAchieved[2, ], c(0, 0, 0, 0))
	expect_equal(simResult2$conditionalPowerAchieved[3, ], c(0.50063521, 0.54643719, 0.52718298, 0.50178869), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult2), NA)))
	    expect_output(print(simResult2)$show())
	    invisible(capture.output(expect_error(summary(simResult2), NA)))
	    expect_output(summary(simResult2)$show())
	    suppressWarnings(simResult2CodeBased <- eval(parse(text = getObjectRCode(simResult2, stringWrapParagraphWidth = NULL))))
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
	    expect_equal(simResult2CodeBased$eventsPerStage, simResult2$eventsPerStage, tolerance = 1e-05)
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
	expect_equal(simResult3$rejectAtLeastOne, c(0.58, 0.5, 0.54, 0.59), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult3$rejectedPopulationsPerStage)), c(0, 0, 0.08, 0, 0, 0.1, 0, 0, 0.12, 0, 0, 0.17, 0, 0, 0.25, 0, 0, 0.24, 0, 0, 0.22, 0, 0, 0.19, 0, 0, 0.26, 0, 0, 0.16, 0, 0, 0.2, 0, 0, 0.23), tolerance = 1e-07)
	expect_equal(simResult3$futilityStop, c(0, 0, 0, 0))
	expect_equal(simResult3$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(simResult3$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(simResult3$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(simResult3$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(simResult3$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(simResult3$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(simResult3$successPerStage[3, ], c(0.58, 0.49, 0.54, 0.59), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult3$selectedPopulations)), c(1, 0.23, 0.19, 1, 0.34, 0.29, 1, 0.29, 0.25, 1, 0.36, 0.3, 1, 0.43, 0.4, 1, 0.44, 0.38, 1, 0.46, 0.44, 1, 0.39, 0.37, 1, 0.49, 0.45, 1, 0.42, 0.34, 1, 0.38, 0.34, 1, 0.39, 0.35), tolerance = 1e-07)
	expect_equal(simResult3$numberOfPopulations[1, ], c(3, 3, 3, 3))
	expect_equal(simResult3$numberOfPopulations[2, ], c(1.15, 1.2, 1.13, 1.14), tolerance = 1e-07)
	expect_equal(simResult3$numberOfPopulations[3, ], c(1.04, 1.01, 1.03, 1.02), tolerance = 1e-07)
	expect_equal(simResult3$expectedNumberOfEvents, c(50, 50, 50, 50))
	expect_equal(unlist(as.list(simResult3$singleNumberOfEventsPerStage)), c(3.6197209, 3.2949603, 1.5344372, 3.5373259, 3.5669739, 1.7213454, 4.2198086, 3.446866, 1.7468818, 4.1271956, 4.1623278, 2.0171614, 8.7605581, 8.3171213, 4.2553924, 8.5611432, 7.620128, 3.8013981, 8.4396172, 8.494284, 4.1220361, 8.2543912, 7.3299517, 3.6670484, 5.4295814, 7.31475, 3.717389, 5.3059889, 7.7227705, 4.0360143, 5.23067, 7.2570864, 3.7723984, 5.1158714, 7.5317292, 3.8778455, 2.1901395, 1.0731684, 0.4927814, 2.595542, 1.0901276, 0.44124214, 2.1099043, 0.80176363, 0.35868373, 2.5025418, 0.97599131, 0.43794482), tolerance = 1e-07)
	expect_equal(simResult3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult3$conditionalPowerAchieved[2, ], c(0, 0, 0, 0))
	expect_equal(simResult3$conditionalPowerAchieved[3, ], c(0.55208933, 0.54329872, 0.55485058, 0.59570674), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult3), NA)))
	    expect_output(print(simResult3)$show())
	    invisible(capture.output(expect_error(summary(simResult3), NA)))
	    expect_output(summary(simResult3)$show())
	    suppressWarnings(simResult3CodeBased <- eval(parse(text = getObjectRCode(simResult3, stringWrapParagraphWidth = NULL))))
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
	    expect_equal(simResult3CodeBased$eventsPerStage, simResult3$eventsPerStage, tolerance = 1e-05)
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
	expect_equal(simResult1$rejectAtLeastOne, 0.78, tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$rejectedPopulationsPerStage)), c(0, 0.25, 0, 0.27, 0, 0.2, 0, 0.2), tolerance = 1e-07)
	expect_equal(simResult1$futilityPerStage[1, ], 0)
	expect_equal(simResult1$earlyStop[1, ], 0)
	expect_equal(simResult1$successPerStage[1, ], 0)
	expect_equal(simResult1$successPerStage[2, ], 0.62, tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$selectedPopulations)), c(1, 0.4, 1, 0.42, 1, 0.38, 1, 0.26), tolerance = 1e-07)
	expect_equal(simResult1$numberOfPopulations[1, ], 4)
	expect_equal(simResult1$numberOfPopulations[2, ], 1.46, tolerance = 1e-07)
	expect_equal(simResult1$expectedNumberOfEvents, 200)
	expect_equal(unlist(as.list(simResult1$singleNumberOfEventsPerStage)), c(7.8947368, 5.6875992, 4.6052632, 3.0762436, 10.526316, 7.5333457, 17.763158, 17.965778, 7.8947368, 8.587733, 13.815789, 14.268603, 31.578947, 41.341223, 5.9210526, 1.5394737), tolerance = 1e-07)
	expect_equal(simResult1$conditionalPowerAchieved[1, ], NA_real_)
	expect_equal(simResult1$conditionalPowerAchieved[2, ], 0.30293141, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult1), NA)))
	    expect_output(print(simResult1)$show())
	    invisible(capture.output(expect_error(summary(simResult1), NA)))
	    expect_output(summary(simResult1)$show())
	    suppressWarnings(simResult1CodeBased <- eval(parse(text = getObjectRCode(simResult1, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult1CodeBased$iterations, simResult1$iterations, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$rejectAtLeastOne, simResult1$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$rejectedPopulationsPerStage, simResult1$rejectedPopulationsPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$futilityPerStage, simResult1$futilityPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$earlyStop, simResult1$earlyStop, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$successPerStage, simResult1$successPerStage, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$selectedPopulations, simResult1$selectedPopulations, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$numberOfPopulations, simResult1$numberOfPopulations, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$expectedNumberOfEvents, simResult1$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$eventsPerStage, simResult1$eventsPerStage, tolerance = 1e-05)
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
	expect_equal(simResult2$iterations[2, ], 100)
	expect_equal(simResult2$rejectAtLeastOne, 0.72, tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$rejectedPopulationsPerStage)), c(0, 0.24, 0, 0.35, 0, 0.19, 0, 0.26), tolerance = 1e-07)
	expect_equal(simResult2$futilityPerStage[1, ], 0)
	expect_equal(simResult2$earlyStop[1, ], 0)
	expect_equal(simResult2$successPerStage[1, ], 0)
	expect_equal(simResult2$successPerStage[2, ], 0.32, tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$selectedPopulations)), c(1, 0.52, 1, 0.6, 1, 0.5, 1, 0.38), tolerance = 1e-07)
	expect_equal(simResult2$numberOfPopulations[1, ], 4)
	expect_equal(simResult2$numberOfPopulations[2, ], 2)
	expect_equal(simResult2$expectedNumberOfEvents, 200)
	expect_equal(unlist(as.list(simResult2$singleNumberOfEventsPerStage)), c(7.8947368, 6.6732283, 4.6052632, 4.1932891, 10.526316, 8.6870229, 17.763158, 19.549115, 7.8947368, 8.6884955, 13.815789, 15.204867, 31.578947, 34.753982, 5.9210526, 2.25), tolerance = 1e-07)
	expect_equal(simResult2$conditionalPowerAchieved[1, ], NA_real_)
	expect_equal(simResult2$conditionalPowerAchieved[2, ], 0.30293141, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult2), NA)))
	    expect_output(print(simResult2)$show())
	    invisible(capture.output(expect_error(summary(simResult2), NA)))
	    expect_output(summary(simResult2)$show())
	    suppressWarnings(simResult2CodeBased <- eval(parse(text = getObjectRCode(simResult2, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult2CodeBased$iterations, simResult2$iterations, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$rejectAtLeastOne, simResult2$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$rejectedPopulationsPerStage, simResult2$rejectedPopulationsPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$futilityPerStage, simResult2$futilityPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$earlyStop, simResult2$earlyStop, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$successPerStage, simResult2$successPerStage, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$selectedPopulations, simResult2$selectedPopulations, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$numberOfPopulations, simResult2$numberOfPopulations, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$expectedNumberOfEvents, simResult2$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$eventsPerStage, simResult2$eventsPerStage, tolerance = 1e-05)
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

