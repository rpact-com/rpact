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
## |  Creation date: 08 November 2023, 09:11:32
## |  File version: $Revision$
## |  Last changed: $Date$
## |  Last changed by: $Author$
## |  

test_plan_section("Testing Simulation Enrichment Survival Function")


test_that("'getSimulationEnrichmentSurvival': gMax = 2", {
	.skipTestIfDisabled()

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
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalEventsWithControls}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalLogRanks}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalAdjustedPrevalances}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	hazardRatios <- matrix(c(
	    1.000000, 1.207775, 1.432188, 1.676140, 1.943358, 2.238755, 2.568980,
	    1.432188, 1.432188, 1.432188, 1.432188, 1.432188, 1.432188, 1.432188
	), ncol = 2)

	effectList1 <- list(
	    subGroups = c("S", "R"), prevalences = c(0.4, 0.6),
	    hazardRatios = hazardRatios, piControls = c(0.1, 0.3)
	)

	design <- getDesignInverseNormal(
	    informationRates = c(0.3, 1),
	    typeOfDesign = "asUser", userAlphaSpending = c(0.01, 0.025)
	)

	suppressWarnings(simResult1 <- getSimulationEnrichmentSurvival(design,
	    plannedEvents = c(40, 120),
	    effectList = effectList1,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    typeOfSelection = "rbest",
	    rValue = 2,
	    intersectionTest = "SpiessensDebois",
	    directionUpper = TRUE,
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult1' with expected results
	expect_equal(simResult1$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100), label = paste0("c(", paste0(simResult1$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$iterations[2, ], c(100, 99, 99, 98, 95, 98, 91), label = paste0("c(", paste0(simResult1$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult1$rejectAtLeastOne, c(0.25, 0.26, 0.44, 0.49, 0.63, 0.52, 0.78), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult1$rejectedPopulationsPerStage)), c(0, 0, 0.02, 0.05, 0.04, 0.07, 0.03, 0.14, 0.09, 0.24, 0.06, 0.36, 0.2, 0.47, 0.08, 0.17, 0.02, 0.21, 0.09, 0.33, 0.1, 0.37, 0.12, 0.47, 0.09, 0.42, 0.15, 0.52), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult1$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult1$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0), label = paste0("c(", paste0(simResult1$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$earlyStop[1, ], c(0, 0.01, 0.01, 0.02, 0.05, 0.02, 0.09), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$successPerStage[1, ], c(0, 0.01, 0.01, 0.02, 0.05, 0.02, 0.09), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$successPerStage[2, ], c(0, 0.03, 0.08, 0.13, 0.24, 0.39, 0.47), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult1$selectedPopulations)), c(1, 1, 1, 0.99, 1, 0.99, 1, 0.98, 1, 0.95, 1, 0.98, 1, 0.91, 1, 1, 1, 0.99, 1, 0.99, 1, 0.98, 1, 0.95, 1, 0.98, 1, 0.91), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult1$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult1$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2), label = paste0("c(", paste0(simResult1$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$numberOfPopulations[2, ], c(2, 2, 2, 2, 2, 2, 2), label = paste0("c(", paste0(simResult1$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult1$expectedNumberOfEvents, c(120, 119.2, 119.2, 118.4, 116, 118.4, 112.8), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$expectedNumberOfEvents, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult1$singleNumberOfEventsPerStage)), c(6.4000002, 12.8, 6.9157979, 13.831596, 7.4434489, 14.886898, 7.9849789, 15.969958, 8.5428984, 17.085797, 9.1204104, 18.240821, 9.7216794, 19.443359, 33.6, 67.2, 33.084202, 66.168404, 32.556551, 65.113102, 32.015021, 64.030042, 31.457102, 62.914203, 30.87959, 61.759179, 30.278321, 60.556641), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult1$singleNumberOfEventsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(simResult1$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$conditionalPowerAchieved[2, ], c(0.15269538, 0.28989266, 0.28620537, 0.37651752, 0.47283912, 0.47668973, 0.531968), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult1), NA)))
	    expect_output(print(simResult1)$show())
	    invisible(capture.output(expect_error(summary(simResult1), NA)))
	    expect_output(summary(simResult1)$show())
	    suppressWarnings(simResult1CodeBased <- eval(parse(text = getObjectRCode(simResult1, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult1CodeBased$eventsPerStage, simResult1$eventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$iterations, simResult1$iterations, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$rejectAtLeastOne, simResult1$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$rejectedPopulationsPerStage, simResult1$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$futilityPerStage, simResult1$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$earlyStop, simResult1$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$successPerStage, simResult1$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$selectedPopulations, simResult1$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$numberOfPopulations, simResult1$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$expectedNumberOfEvents, simResult1$expectedNumberOfEvents, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$singleNumberOfEventsPerStage, simResult1$singleNumberOfEventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$conditionalPowerAchieved, simResult1$conditionalPowerAchieved, tolerance = 1e-07)
	    expect_type(names(simResult1), "character")
	    df <- as.data.frame(simResult1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	suppressWarnings(simResult2 <- getSimulationEnrichmentSurvival(design,
	    plannedEvents = c(40, 120),
	    effectList = effectList1,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    typeOfSelection = "rbest",
	    rValue = 2,
	    intersectionTest = "Simes",
	    directionUpper = TRUE,
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult2' with expected results
	expect_equal(simResult2$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100), label = paste0("c(", paste0(simResult2$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$iterations[2, ], c(100, 99, 99, 98, 93, 98, 91), label = paste0("c(", paste0(simResult2$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult2$rejectAtLeastOne, c(0.25, 0.24, 0.4, 0.47, 0.62, 0.52, 0.79), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult2$rejectedPopulationsPerStage)), c(0, 0.02, 0.02, 0.05, 0.04, 0.07, 0.03, 0.14, 0.11, 0.22, 0.06, 0.36, 0.2, 0.48, 0.08, 0.16, 0.02, 0.19, 0.09, 0.29, 0.1, 0.35, 0.14, 0.44, 0.09, 0.41, 0.15, 0.53), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult2$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult2$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0), label = paste0("c(", paste0(simResult2$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$earlyStop[1, ], c(0, 0.01, 0.01, 0.02, 0.07, 0.02, 0.09), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$successPerStage[1, ], c(0, 0.01, 0.01, 0.02, 0.07, 0.02, 0.09), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$successPerStage[2, ], c(0.01, 0.03, 0.08, 0.13, 0.22, 0.38, 0.48), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult2$selectedPopulations)), c(1, 1, 1, 0.99, 1, 0.99, 1, 0.98, 1, 0.93, 1, 0.98, 1, 0.91, 1, 1, 1, 0.99, 1, 0.99, 1, 0.98, 1, 0.93, 1, 0.98, 1, 0.91), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult2$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult2$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2), label = paste0("c(", paste0(simResult2$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$numberOfPopulations[2, ], c(2, 2, 2, 2, 2, 2, 2), label = paste0("c(", paste0(simResult2$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult2$expectedNumberOfEvents, c(120, 119.2, 119.2, 118.4, 114.4, 118.4, 112.8), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$expectedNumberOfEvents, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult2$singleNumberOfEventsPerStage)), c(6.4000002, 12.8, 6.9157979, 13.831596, 7.4434489, 14.886898, 7.9849789, 15.969958, 8.5428984, 17.085797, 9.1204104, 18.240821, 9.7216794, 19.443359, 33.6, 67.2, 33.084202, 66.168404, 32.556551, 65.113102, 32.015021, 64.030042, 31.457102, 62.914203, 30.87959, 61.759179, 30.278321, 60.556641), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult2$singleNumberOfEventsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(simResult2$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$conditionalPowerAchieved[2, ], c(0.15269538, 0.28989266, 0.28620537, 0.37651752, 0.46167737, 0.47668973, 0.531968), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult2), NA)))
	    expect_output(print(simResult2)$show())
	    invisible(capture.output(expect_error(summary(simResult2), NA)))
	    expect_output(summary(simResult2)$show())
	    suppressWarnings(simResult2CodeBased <- eval(parse(text = getObjectRCode(simResult2, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult2CodeBased$eventsPerStage, simResult2$eventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$iterations, simResult2$iterations, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$rejectAtLeastOne, simResult2$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$rejectedPopulationsPerStage, simResult2$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$futilityPerStage, simResult2$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$earlyStop, simResult2$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$successPerStage, simResult2$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$selectedPopulations, simResult2$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$numberOfPopulations, simResult2$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$expectedNumberOfEvents, simResult2$expectedNumberOfEvents, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$singleNumberOfEventsPerStage, simResult2$singleNumberOfEventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$conditionalPowerAchieved, simResult2$conditionalPowerAchieved, tolerance = 1e-07)
	    expect_type(names(simResult2), "character")
	    df <- as.data.frame(simResult2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	effectList2 <- list(
	    subGroups = c("S", "R"), prevalences = c(0.4, 0.6),
	    hazardRatios = hazardRatios
	)

	suppressWarnings(simResult3 <- getSimulationEnrichmentSurvival(design,
	    plannedEvents = c(40, 120),
	    effectList = effectList2,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    typeOfSelection = "best",
	    intersectionTest = "Sidak",
	    directionUpper = TRUE,
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult3' with expected results
	expect_equal(simResult3$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100), label = paste0("c(", paste0(simResult3$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$iterations[2, ], c(100, 99, 97, 96, 90, 85, 72), label = paste0("c(", paste0(simResult3$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult3$rejectAtLeastOne, c(0.14, 0.16, 0.4, 0.67, 0.84, 0.92, 0.95), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult3$rejectedPopulationsPerStage)), c(0, 0.01, 0.02, 0.04, 0.05, 0.15, 0.04, 0.36, 0.16, 0.46, 0.21, 0.58, 0.35, 0.45, 0.05, 0.08, 0.01, 0.1, 0.1, 0.13, 0.14, 0.23, 0.16, 0.17, 0.22, 0.09, 0.33, 0.11), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult3$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult3$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0), label = paste0("c(", paste0(simResult3$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$earlyStop[1, ], c(0, 0.01, 0.03, 0.04, 0.1, 0.15, 0.28), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$successPerStage[1, ], c(0, 0.01, 0.03, 0.04, 0.1, 0.15, 0.28), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$successPerStage[2, ], c(0.11, 0.15, 0.37, 0.62, 0.74, 0.77, 0.67), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult3$selectedPopulations)), c(1, 0.32, 1, 0.41, 1, 0.48, 1, 0.49, 1, 0.56, 1, 0.68, 1, 0.52, 1, 0.68, 1, 0.58, 1, 0.49, 1, 0.47, 1, 0.34, 1, 0.17, 1, 0.2), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult3$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult3$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2), label = paste0("c(", paste0(simResult3$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$numberOfPopulations[2, ], c(1, 1, 1, 1, 1, 1, 1), label = paste0("c(", paste0(simResult3$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult3$expectedNumberOfEvents, c(120, 119.2, 117.6, 116.8, 112, 108, 97.6), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$expectedNumberOfEvents, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult3$singleNumberOfEventsPerStage)), c(14.163599, 44.862494, 15.080284, 50.80114, 16, 55.752577, 16.925752, 57.406466, 17.861157, 63.272875, 18.810731, 71.524292, 19.780244, 68.766802, 25.836401, 35.137506, 24.919716, 29.19886, 24, 24.247423, 23.074248, 22.593534, 22.138843, 16.727125, 21.189269, 8.4757076, 20.219756, 11.233198), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult3$singleNumberOfEventsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(simResult3$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$conditionalPowerAchieved[2, ], c(0.13647132, 0.29139937, 0.31543636, 0.43117712, 0.58131066, 0.58903037, 0.64693196), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult3), NA)))
	    expect_output(print(simResult3)$show())
	    invisible(capture.output(expect_error(summary(simResult3), NA)))
	    expect_output(summary(simResult3)$show())
	    suppressWarnings(simResult3CodeBased <- eval(parse(text = getObjectRCode(simResult3, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult3CodeBased$eventsPerStage, simResult3$eventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$iterations, simResult3$iterations, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$rejectAtLeastOne, simResult3$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$rejectedPopulationsPerStage, simResult3$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$futilityPerStage, simResult3$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$earlyStop, simResult3$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$successPerStage, simResult3$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$selectedPopulations, simResult3$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$numberOfPopulations, simResult3$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$expectedNumberOfEvents, simResult3$expectedNumberOfEvents, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$singleNumberOfEventsPerStage, simResult3$singleNumberOfEventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$conditionalPowerAchieved, simResult3$conditionalPowerAchieved, tolerance = 1e-07)
	    expect_type(names(simResult3), "character")
	    df <- as.data.frame(simResult3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	suppressWarnings(simResult4 <- getSimulationEnrichmentSurvival(design,
	    plannedEvents = c(40, 120),
	    effectList = effectList2,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    typeOfSelection = "epsilon",
	    epsilonValue = 0.1,
	    intersectionTest = "Bonferroni",
	    directionUpper = TRUE,
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult4' with expected results
	expect_equal(simResult4$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100), label = paste0("c(", paste0(simResult4$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$iterations[2, ], c(87, 88, 91, 93, 89, 83, 71), label = paste0("c(", paste0(simResult4$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult4$rejectAtLeastOne, c(0.12, 0.15, 0.39, 0.63, 0.83, 0.91, 0.94), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult4$rejectedPopulationsPerStage)), c(0, 0.01, 0.02, 0.04, 0.05, 0.15, 0.04, 0.32, 0.16, 0.48, 0.21, 0.62, 0.35, 0.46, 0.05, 0.06, 0.01, 0.09, 0.1, 0.14, 0.14, 0.23, 0.16, 0.17, 0.22, 0.15, 0.33, 0.15), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult4$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult4$futilityPerStage[1, ], c(0.13, 0.11, 0.06, 0.03, 0.01, 0.02, 0.01), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$earlyStop[1, ], c(0.13, 0.12, 0.09, 0.07, 0.11, 0.17, 0.29), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$successPerStage[1, ], c(0, 0.01, 0.03, 0.04, 0.1, 0.15, 0.28), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$successPerStage[2, ], c(0.09, 0.14, 0.34, 0.55, 0.71, 0.75, 0.65), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult4$selectedPopulations)), c(1, 0.33, 1, 0.46, 1, 0.55, 1, 0.54, 1, 0.61, 1, 0.75, 1, 0.56, 1, 0.64, 1, 0.51, 1, 0.5, 1, 0.52, 1, 0.34, 1, 0.23, 1, 0.24), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult4$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult4$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2), label = paste0("c(", paste0(simResult4$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$numberOfPopulations[2, ], c(1.1149425, 1.1022727, 1.1538462, 1.1397849, 1.0674157, 1.1807229, 1.1267606), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult4$expectedNumberOfEvents, c(109.6, 110.4, 112.8, 114.4, 111.2, 106.4, 96.8), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$expectedNumberOfEvents, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult4$singleNumberOfEventsPerStage)), c(14.163599, 41.987823, 15.080284, 51.115783, 16, 53.626374, 16.925752, 54.19654, 17.861157, 63.084929, 18.810731, 68.25655, 19.780244, 66.330305, 25.836401, 38.012177, 24.919716, 28.884217, 24, 26.373626, 23.074248, 25.80346, 22.138843, 16.915071, 21.189269, 11.74345, 20.219756, 13.669695), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult4$singleNumberOfEventsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult4$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(simResult4$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$conditionalPowerAchieved[2, ], c(0.15686358, 0.32782429, 0.33623436, 0.44508606, 0.58784224, 0.60322388, 0.65604368), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult4), NA)))
	    expect_output(print(simResult4)$show())
	    invisible(capture.output(expect_error(summary(simResult4), NA)))
	    expect_output(summary(simResult4)$show())
	    suppressWarnings(simResult4CodeBased <- eval(parse(text = getObjectRCode(simResult4, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult4CodeBased$eventsPerStage, simResult4$eventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$iterations, simResult4$iterations, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$rejectAtLeastOne, simResult4$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$rejectedPopulationsPerStage, simResult4$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$futilityPerStage, simResult4$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$earlyStop, simResult4$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$successPerStage, simResult4$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$selectedPopulations, simResult4$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$numberOfPopulations, simResult4$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$expectedNumberOfEvents, simResult4$expectedNumberOfEvents, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$singleNumberOfEventsPerStage, simResult4$singleNumberOfEventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$conditionalPowerAchieved, simResult4$conditionalPowerAchieved, tolerance = 1e-07)
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
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalEventsWithControls}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalLogRanks}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalAdjustedPrevalances}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	subGroups <- c("S1", "S12", "S2", "R")
	prevalences <- c(0.20, 0.30, 0.40, 0.1)
	piControls <- rep(0.2, 4)
	hazardRatios <- matrix(c(
	    1.432, 1.432, 1.943, 1.943, 1.432, 1.432, 1.432, 1.432, 1.943,
	    1.943, 1.943, 1.943, 1.943, 2.569, 1.943, 2.569
	), ncol = 4)
	effectList1 <- list(subGroups = subGroups, prevalences = prevalences, hazardRatios = hazardRatios)

	design <- getDesignInverseNormal(informationRates = c(0.4, 0.8, 1), typeOfDesign = "noEarlyEfficacy")

	suppressWarnings(simResult1 <- getSimulationEnrichmentSurvival(design,
	    plannedEvents = c(20, 40, 50),
	    effectList = effectList1,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    typeOfSelection = "best",
	    intersectionTest = "Sidak",
	    directionUpper = TRUE,
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult1' with expected results
	expect_equal(simResult1$iterations[1, ], c(100, 100, 100, 100), label = paste0("c(", paste0(simResult1$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$iterations[2, ], c(100, 100, 100, 100), label = paste0("c(", paste0(simResult1$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult1$iterations[3, ], c(100, 100, 100, 100), label = paste0("c(", paste0(simResult1$iterations[3, ], collapse = ", "), ")"))
	expect_equal(simResult1$rejectAtLeastOne, c(0.37, 0.35, 0.41, 0.35), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult1$rejectedPopulationsPerStage)), c(0, 0, 0.06, 0, 0, 0.06, 0, 0, 0.12, 0, 0, 0.15, 0, 0, 0.27, 0, 0, 0.12, 0, 0, 0.17, 0, 0, 0.12, 0, 0, 0.04, 0, 0, 0.17, 0, 0, 0.12, 0, 0, 0.08), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult1$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult1$futilityStop, c(0, 0, 0, 0), label = paste0("c(", paste0(simResult1$futilityStop, collapse = ", "), ")"))
	expect_equal(simResult1$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult1$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult1$futilityPerStage[2, ], collapse = ", "), ")"))
	expect_equal(simResult1$earlyStop[1, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult1$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$earlyStop[2, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult1$earlyStop[2, ], collapse = ", "), ")"))
	expect_equal(simResult1$successPerStage[1, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult1$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$successPerStage[2, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult1$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(simResult1$successPerStage[3, ], c(0.37, 0.35, 0.41, 0.35), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$successPerStage[3, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult1$selectedPopulations)), c(1, 0.33, 0.33, 1, 0.21, 0.21, 1, 0.29, 0.29, 1, 0.48, 0.48, 1, 0.49, 0.49, 1, 0.34, 0.34, 1, 0.42, 0.42, 1, 0.29, 0.29, 1, 0.18, 0.18, 1, 0.45, 0.45, 1, 0.29, 0.29, 1, 0.23, 0.23), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult1$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult1$numberOfPopulations[1, ], c(3, 3, 3, 3), label = paste0("c(", paste0(simResult1$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$numberOfPopulations[2, ], c(1, 1, 1, 1), label = paste0("c(", paste0(simResult1$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult1$numberOfPopulations[3, ], c(1, 1, 1, 1), label = paste0("c(", paste0(simResult1$numberOfPopulations[3, ], collapse = ", "), ")"))
	expect_equal(simResult1$expectedNumberOfEvents, c(50, 50, 50, 50), label = paste0("c(", paste0(simResult1$expectedNumberOfEvents, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult1$singleNumberOfEventsPerStage)), c(3.6197209, 3.2915498, 1.6457749, 3.5373259, 3.2717967, 1.6358983, 4.2198086, 3.8135488, 1.9067744, 4.1271956, 5.2358276, 2.6179138, 8.7605581, 7.6271207, 3.8135604, 8.5611432, 8.0506265, 4.0253132, 8.4396172, 7.6333921, 3.816696, 8.2543912, 5.4792526, 2.7396263, 5.4295814, 8.6871044, 4.3435522, 5.3059889, 7.509583, 3.7547915, 5.23067, 7.9411869, 3.9705935, 5.1158714, 8.7093352, 4.3546676, 2.1901395, 0.39422512, 0.19711256, 2.595542, 1.1679939, 0.58399695, 2.1099043, 0.61187224, 0.30593612, 2.5025418, 0.57558462, 0.28779231), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult1$singleNumberOfEventsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(simResult1$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$conditionalPowerAchieved[2, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult1$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	expect_equal(simResult1$conditionalPowerAchieved[3, ], c(0.40782052, 0.41157037, 0.44953877, 0.471601), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$conditionalPowerAchieved[3, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult1), NA)))
	    expect_output(print(simResult1)$show())
	    invisible(capture.output(expect_error(summary(simResult1), NA)))
	    expect_output(summary(simResult1)$show())
	    suppressWarnings(simResult1CodeBased <- eval(parse(text = getObjectRCode(simResult1, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult1CodeBased$eventsPerStage, simResult1$eventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$iterations, simResult1$iterations, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$rejectAtLeastOne, simResult1$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$rejectedPopulationsPerStage, simResult1$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$futilityStop, simResult1$futilityStop, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$futilityPerStage, simResult1$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$earlyStop, simResult1$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$successPerStage, simResult1$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$selectedPopulations, simResult1$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$numberOfPopulations, simResult1$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$expectedNumberOfEvents, simResult1$expectedNumberOfEvents, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$singleNumberOfEventsPerStage, simResult1$singleNumberOfEventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$conditionalPowerAchieved, simResult1$conditionalPowerAchieved, tolerance = 1e-07)
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
	    effectList = effectList1,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    typeOfSelection = "rbest",
	    rValue = 2,
	    intersectionTest = "Bonferroni",
	    directionUpper = TRUE,
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult2' with expected results
	expect_equal(simResult2$iterations[1, ], c(100, 100, 100, 100), label = paste0("c(", paste0(simResult2$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$iterations[2, ], c(85, 86, 87, 88), label = paste0("c(", paste0(simResult2$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult2$iterations[3, ], c(79, 81, 84, 85), label = paste0("c(", paste0(simResult2$iterations[3, ], collapse = ", "), ")"))
	expect_equal(simResult2$rejectAtLeastOne, c(0.26, 0.26, 0.31, 0.28), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult2$rejectedPopulationsPerStage)), c(0, 0, 0.04, 0, 0, 0.06, 0, 0, 0.08, 0, 0, 0.07, 0, 0, 0.22, 0, 0, 0.16, 0, 0, 0.18, 0, 0, 0.13, 0, 0, 0.17, 0, 0, 0.21, 0, 0, 0.29, 0, 0, 0.25), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult2$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult2$futilityStop, c(0.21, 0.19, 0.16, 0.15), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$futilityStop, collapse = ", "), ")"))
	expect_equal(simResult2$futilityPerStage[1, ], c(0.15, 0.14, 0.13, 0.12), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$futilityPerStage[2, ], c(0.06, 0.05, 0.03, 0.03), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$futilityPerStage[2, ], collapse = ", "), ")"))
	expect_equal(simResult2$earlyStop[1, ], c(0.15, 0.14, 0.13, 0.12), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$earlyStop[2, ], c(0.06, 0.05, 0.03, 0.03), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$earlyStop[2, ], collapse = ", "), ")"))
	expect_equal(simResult2$successPerStage[1, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult2$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$successPerStage[2, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult2$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(simResult2$successPerStage[3, ], c(0.17, 0.17, 0.24, 0.17), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$successPerStage[3, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult2$selectedPopulations)), c(1, 0.43, 0.4, 1, 0.41, 0.38, 1, 0.4, 0.38, 1, 0.54, 0.51, 1, 0.67, 0.61, 1, 0.61, 0.57, 1, 0.58, 0.56, 1, 0.5, 0.49, 1, 0.6, 0.57, 1, 0.7, 0.67, 1, 0.76, 0.74, 1, 0.72, 0.7), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult2$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult2$numberOfPopulations[1, ], c(3, 3, 3, 3), label = paste0("c(", paste0(simResult2$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$numberOfPopulations[2, ], c(2, 2, 2, 2), label = paste0("c(", paste0(simResult2$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult2$numberOfPopulations[3, ], c(2, 2, 2, 2), label = paste0("c(", paste0(simResult2$numberOfPopulations[3, ], collapse = ", "), ")"))
	expect_equal(simResult2$expectedNumberOfEvents, c(44.9, 45.3, 45.8, 46.1), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$expectedNumberOfEvents, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult2$singleNumberOfEventsPerStage)), c(3.6197209, 3.7506414, 1.8718405, 3.5373259, 3.63547, 1.8142515, 4.2198086, 4.2827326, 2.1395276, 4.1271956, 4.2345201, 2.1156817, 8.7605581, 9.0774157, 4.5302851, 8.5611432, 8.7986745, 4.3909064, 8.4396172, 8.5654651, 4.2790552, 8.2543912, 8.4690401, 4.2313635, 5.4295814, 5.625962, 2.8077608, 5.3059889, 5.453205, 2.7213772, 5.23067, 5.3086675, 2.6520546, 5.1158714, 5.2489056, 2.6224964, 2.1901395, 1.5459808, 0.79011363, 2.595542, 2.1126504, 1.0734649, 2.1099043, 1.8431348, 0.9293626, 2.5025418, 2.0475342, 1.0304584), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult2$singleNumberOfEventsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(simResult2$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$conditionalPowerAchieved[2, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult2$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	expect_equal(simResult2$conditionalPowerAchieved[3, ], c(0.41272973, 0.44447643, 0.46315477, 0.46991584), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$conditionalPowerAchieved[3, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult2), NA)))
	    expect_output(print(simResult2)$show())
	    invisible(capture.output(expect_error(summary(simResult2), NA)))
	    expect_output(summary(simResult2)$show())
	    suppressWarnings(simResult2CodeBased <- eval(parse(text = getObjectRCode(simResult2, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult2CodeBased$eventsPerStage, simResult2$eventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$iterations, simResult2$iterations, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$rejectAtLeastOne, simResult2$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$rejectedPopulationsPerStage, simResult2$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$futilityStop, simResult2$futilityStop, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$futilityPerStage, simResult2$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$earlyStop, simResult2$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$successPerStage, simResult2$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$selectedPopulations, simResult2$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$numberOfPopulations, simResult2$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$expectedNumberOfEvents, simResult2$expectedNumberOfEvents, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$singleNumberOfEventsPerStage, simResult2$singleNumberOfEventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$conditionalPowerAchieved, simResult2$conditionalPowerAchieved, tolerance = 1e-07)
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
	    effectList = effectList1,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    typeOfSelection = "epsilon",
	    epsilonValue = 0.2,
	    intersectionTest = "Simes",
	    directionUpper = TRUE,
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult3' with expected results
	expect_equal(simResult3$iterations[1, ], c(100, 100, 100, 100), label = paste0("c(", paste0(simResult3$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$iterations[2, ], c(100, 100, 100, 100), label = paste0("c(", paste0(simResult3$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult3$iterations[3, ], c(100, 100, 100, 100), label = paste0("c(", paste0(simResult3$iterations[3, ], collapse = ", "), ")"))
	expect_equal(simResult3$rejectAtLeastOne, c(0.36, 0.35, 0.44, 0.4), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult3$rejectedPopulationsPerStage)), c(0, 0, 0.07, 0, 0, 0.06, 0, 0, 0.13, 0, 0, 0.17, 0, 0, 0.27, 0, 0, 0.18, 0, 0, 0.17, 0, 0, 0.12, 0, 0, 0.05, 0, 0, 0.17, 0, 0, 0.16, 0, 0, 0.12), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult3$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult3$futilityStop, c(0, 0, 0, 0), label = paste0("c(", paste0(simResult3$futilityStop, collapse = ", "), ")"))
	expect_equal(simResult3$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult3$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult3$futilityPerStage[2, ], collapse = ", "), ")"))
	expect_equal(simResult3$earlyStop[1, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult3$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$earlyStop[2, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult3$earlyStop[2, ], collapse = ", "), ")"))
	expect_equal(simResult3$successPerStage[1, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult3$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$successPerStage[2, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult3$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(simResult3$successPerStage[3, ], c(0.36, 0.34, 0.41, 0.38), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$successPerStage[3, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult3$selectedPopulations)), c(1, 0.42, 0.32, 1, 0.32, 0.22, 1, 0.36, 0.32, 1, 0.54, 0.5, 1, 0.59, 0.53, 1, 0.52, 0.48, 1, 0.55, 0.48, 1, 0.36, 0.31, 1, 0.38, 0.27, 1, 0.5, 0.45, 1, 0.48, 0.42, 1, 0.36, 0.33), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult3$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult3$numberOfPopulations[1, ], c(3, 3, 3, 3), label = paste0("c(", paste0(simResult3$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$numberOfPopulations[2, ], c(1.39, 1.34, 1.39, 1.26), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult3$numberOfPopulations[3, ], c(1.12, 1.15, 1.22, 1.14), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$numberOfPopulations[3, ], collapse = ", "), ")"))
	expect_equal(simResult3$expectedNumberOfEvents, c(50, 50, 50, 50), label = paste0("c(", paste0(simResult3$expectedNumberOfEvents, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult3$singleNumberOfEventsPerStage)), c(3.6197209, 3.780682, 1.6893108, 3.5373259, 3.2506085, 1.5558983, 4.2198086, 4.1738404, 2.0706939, 4.1271956, 5.1472378, 2.6010146, 8.7605581, 7.5733107, 3.8740056, 8.5611432, 8.2799243, 4.1487871, 8.4396172, 7.5735103, 3.7333396, 8.2543912, 5.8114801, 2.8436611, 5.4295814, 7.8137543, 4.1410147, 5.3059889, 7.1716962, 3.7113176, 5.23067, 7.2398952, 3.7528866, 5.1158714, 8.140367, 4.1424049, 2.1901395, 0.83225302, 0.29566884, 2.595542, 1.297771, 0.58399695, 2.1099043, 1.0127541, 0.4430799, 2.5025418, 0.90091505, 0.4129194), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult3$singleNumberOfEventsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(simResult3$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$conditionalPowerAchieved[2, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult3$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	expect_equal(simResult3$conditionalPowerAchieved[3, ], c(0.38601776, 0.39399786, 0.42333822, 0.45516082), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$conditionalPowerAchieved[3, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult3), NA)))
	    expect_output(print(simResult3)$show())
	    invisible(capture.output(expect_error(summary(simResult3), NA)))
	    expect_output(summary(simResult3)$show())
	    suppressWarnings(simResult3CodeBased <- eval(parse(text = getObjectRCode(simResult3, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult3CodeBased$eventsPerStage, simResult3$eventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$iterations, simResult3$iterations, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$rejectAtLeastOne, simResult3$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$rejectedPopulationsPerStage, simResult3$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$futilityStop, simResult3$futilityStop, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$futilityPerStage, simResult3$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$earlyStop, simResult3$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$successPerStage, simResult3$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$selectedPopulations, simResult3$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$numberOfPopulations, simResult3$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$expectedNumberOfEvents, simResult3$expectedNumberOfEvents, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$singleNumberOfEventsPerStage, simResult3$singleNumberOfEventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$conditionalPowerAchieved, simResult3$conditionalPowerAchieved, tolerance = 1e-07)
	    expect_type(names(simResult3), "character")
	    df <- as.data.frame(simResult3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	effectList2 <- list(
	    subGroups = subGroups, prevalences = prevalences,
	    piControls = piControls, hazardRatios = hazardRatios
	)

	suppressWarnings(simResult5 <- getSimulationEnrichmentSurvival(design,
	    plannedEvents = c(20, 40, 50),
	    effectList = effectList2,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    typeOfSelection = "epsilon",
	    epsilonValue = 0.2,
	    intersectionTest = "Simes",
	    directionUpper = TRUE,
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult5' with expected results
	expect_equal(simResult5$iterations[1, ], c(100, 100, 100, 100), label = paste0("c(", paste0(simResult5$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult5$iterations[2, ], c(100, 100, 100, 100), label = paste0("c(", paste0(simResult5$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult5$iterations[3, ], c(100, 100, 100, 100), label = paste0("c(", paste0(simResult5$iterations[3, ], collapse = ", "), ")"))
	expect_equal(simResult5$rejectAtLeastOne, c(0.36, 0.33, 0.44, 0.4), tolerance = 1e-07, label = paste0("c(", paste0(simResult5$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult5$rejectedPopulationsPerStage)), c(0, 0, 0.06, 0, 0, 0.07, 0, 0, 0.13, 0, 0, 0.16, 0, 0, 0.27, 0, 0, 0.19, 0, 0, 0.17, 0, 0, 0.12, 0, 0, 0.05, 0, 0, 0.13, 0, 0, 0.16, 0, 0, 0.13), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult5$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult5$futilityStop, c(0, 0, 0, 0), label = paste0("c(", paste0(simResult5$futilityStop, collapse = ", "), ")"))
	expect_equal(simResult5$futilityPerStage[1, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult5$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult5$futilityPerStage[2, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult5$futilityPerStage[2, ], collapse = ", "), ")"))
	expect_equal(simResult5$earlyStop[1, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult5$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult5$earlyStop[2, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult5$earlyStop[2, ], collapse = ", "), ")"))
	expect_equal(simResult5$successPerStage[1, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult5$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult5$successPerStage[2, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult5$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(simResult5$successPerStage[3, ], c(0.36, 0.33, 0.41, 0.38), tolerance = 1e-07, label = paste0("c(", paste0(simResult5$successPerStage[3, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult5$selectedPopulations)), c(1, 0.4, 0.31, 1, 0.32, 0.22, 1, 0.36, 0.32, 1, 0.54, 0.49, 1, 0.59, 0.52, 1, 0.54, 0.5, 1, 0.55, 0.48, 1, 0.37, 0.32, 1, 0.38, 0.27, 1, 0.51, 0.45, 1, 0.48, 0.42, 1, 0.38, 0.35), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult5$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult5$numberOfPopulations[1, ], c(3, 3, 3, 3), label = paste0("c(", paste0(simResult5$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult5$numberOfPopulations[2, ], c(1.37, 1.37, 1.39, 1.29), tolerance = 1e-07, label = paste0("c(", paste0(simResult5$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult5$numberOfPopulations[3, ], c(1.1, 1.17, 1.22, 1.16), tolerance = 1e-07, label = paste0("c(", paste0(simResult5$numberOfPopulations[3, ], collapse = ", "), ")"))
	expect_equal(simResult5$expectedNumberOfEvents, c(50, 50, 50, 50), label = paste0("c(", paste0(simResult5$expectedNumberOfEvents, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult5$singleNumberOfEventsPerStage)), c(3.6945761, 3.7323515, 1.6794688, 3.6346639, 3.3378849, 1.5777994, 4.1778232, 4.109865, 2.0374383, 4.1120508, 5.0603333, 2.5559297, 8.6108478, 7.5016338, 3.8269345, 8.4712121, 8.1386608, 4.0963486, 8.3556464, 7.4828019, 3.6874504, 8.2241016, 5.8022236, 2.8385927, 5.5418642, 7.9479841, 4.2029806, 5.4519959, 7.2779689, 3.7763732, 5.3776189, 7.4046555, 3.8364399, 5.2929579, 8.236505, 4.1905719, 2.1527119, 0.81803054, 0.29061611, 2.4421281, 1.2454853, 0.54947882, 2.0889116, 1.0026776, 0.43867143, 2.3708897, 0.90093809, 0.4149057), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult5$singleNumberOfEventsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult5$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(simResult5$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult5$conditionalPowerAchieved[2, ], c(0, 0, 0, 0), label = paste0("c(", paste0(simResult5$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	expect_equal(simResult5$conditionalPowerAchieved[3, ], c(0.38413966, 0.38845505, 0.4197785, 0.45252776), tolerance = 1e-07, label = paste0("c(", paste0(simResult5$conditionalPowerAchieved[3, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult5), NA)))
	    expect_output(print(simResult5)$show())
	    invisible(capture.output(expect_error(summary(simResult5), NA)))
	    expect_output(summary(simResult5)$show())
	    suppressWarnings(simResult5CodeBased <- eval(parse(text = getObjectRCode(simResult5, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult5CodeBased$eventsPerStage, simResult5$eventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult5CodeBased$iterations, simResult5$iterations, tolerance = 1e-07)
	    expect_equal(simResult5CodeBased$rejectAtLeastOne, simResult5$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult5CodeBased$rejectedPopulationsPerStage, simResult5$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult5CodeBased$futilityStop, simResult5$futilityStop, tolerance = 1e-07)
	    expect_equal(simResult5CodeBased$futilityPerStage, simResult5$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult5CodeBased$earlyStop, simResult5$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult5CodeBased$successPerStage, simResult5$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult5CodeBased$selectedPopulations, simResult5$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult5CodeBased$numberOfPopulations, simResult5$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult5CodeBased$expectedNumberOfEvents, simResult5$expectedNumberOfEvents, tolerance = 1e-07)
	    expect_equal(simResult5CodeBased$singleNumberOfEventsPerStage, simResult5$singleNumberOfEventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult5CodeBased$conditionalPowerAchieved, simResult5$conditionalPowerAchieved, tolerance = 1e-07)
	    expect_type(names(simResult5), "character")
	    df <- as.data.frame(simResult5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult5)
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
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalEventsWithControls}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalLogRanks}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalAdjustedPrevalances}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	subGroups <- c("S1", "S2", "S3", "S12", "S13", "S23", "S123", "R")
	prevalences <- c(0.1, 0.05, 0.1, 0.15, 0.1, 0.15, 0.3, 0.05)
	hazardRatios <- matrix(c(seq(1, 1.75, 0.25), seq(1, 1.75, 0.25)), ncol = 8)

	effectList1 <- list(subGroups = subGroups, prevalences = prevalences, hazardRatios = hazardRatios)

	design <- getDesignInverseNormal(informationRates = c(0.4, 1), typeOfDesign = "noEarlyEfficacy")

	suppressWarnings(simResult1 <- getSimulationEnrichmentSurvival(design,
	    plannedEvents = c(100, 200),
	    effectList = effectList1,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 2,
	    typeOfSelection = "epsilon",
	    epsilonValue = 0.15,
	    adaptations = c(T),
	    intersectionTest = "Sidak",
	    seed = 123
	))


	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult1' with expected results
	expect_equal(simResult1$iterations[1, ], 100, label = paste0("c(", paste0(simResult1$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$iterations[2, ], 100, label = paste0("c(", paste0(simResult1$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult1$rejectAtLeastOne, 0.57, tolerance = 1e-07, label = paste0("c(", paste0(simResult1$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult1$rejectedPopulationsPerStage)), c(0, 0.16, 0, 0.3, 0, 0.2, 0, 0.12), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult1$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult1$futilityPerStage[1, ], 0, label = paste0("c(", paste0(simResult1$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$earlyStop[1, ], 0, label = paste0("c(", paste0(simResult1$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$successPerStage[1, ], 0, label = paste0("c(", paste0(simResult1$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$successPerStage[2, ], 0.51, tolerance = 1e-07, label = paste0("c(", paste0(simResult1$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult1$selectedPopulations)), c(1, 0.45, 1, 0.58, 1, 0.44, 1, 0.44), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult1$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult1$numberOfPopulations[1, ], 4, label = paste0("c(", paste0(simResult1$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$numberOfPopulations[2, ], 1.91, tolerance = 1e-07, label = paste0("c(", paste0(simResult1$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult1$expectedNumberOfEvents, 200, label = paste0("c(", paste0(simResult1$expectedNumberOfEvents, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult1$singleNumberOfEventsPerStage)), c(7.8947368, 5.6940341, 4.6052632, 3.8122818, 10.526316, 7.8604058, 17.763158, 18.4485, 7.8947368, 7.9435029, 13.815789, 14.40569, 31.578947, 39.230322, 5.9210526, 2.6052632), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult1$singleNumberOfEventsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult1$conditionalPowerAchieved[1, ], NA_real_, label = paste0("c(", paste0(simResult1$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$conditionalPowerAchieved[2, ], 0.42595791, tolerance = 1e-07, label = paste0("c(", paste0(simResult1$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult1), NA)))
	    expect_output(print(simResult1)$show())
	    invisible(capture.output(expect_error(summary(simResult1), NA)))
	    expect_output(summary(simResult1)$show())
	    suppressWarnings(simResult1CodeBased <- eval(parse(text = getObjectRCode(simResult1, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult1CodeBased$eventsPerStage, simResult1$eventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$iterations, simResult1$iterations, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$rejectAtLeastOne, simResult1$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$rejectedPopulationsPerStage, simResult1$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$futilityPerStage, simResult1$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$earlyStop, simResult1$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$successPerStage, simResult1$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$selectedPopulations, simResult1$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$numberOfPopulations, simResult1$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$expectedNumberOfEvents, simResult1$expectedNumberOfEvents, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$singleNumberOfEventsPerStage, simResult1$singleNumberOfEventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$conditionalPowerAchieved, simResult1$conditionalPowerAchieved, tolerance = 1e-07)
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
	    effectList = effectList1,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 2,
	    typeOfSelection = "rBest",
	    rValue = 2,
	    adaptations = c(T),
	    intersectionTest = "Bonferroni",
	    seed = 123
	))


	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult2' with expected results
	expect_equal(simResult2$iterations[1, ], 100, label = paste0("c(", paste0(simResult2$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$iterations[2, ], 90, label = paste0("c(", paste0(simResult2$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult2$rejectAtLeastOne, 0.51, tolerance = 1e-07, label = paste0("c(", paste0(simResult2$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult2$rejectedPopulationsPerStage)), c(0, 0.19, 0, 0.3, 0, 0.22, 0, 0.17), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult2$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult2$futilityPerStage[1, ], 0.1, tolerance = 1e-07, label = paste0("c(", paste0(simResult2$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$earlyStop[1, ], 0.1, tolerance = 1e-07, label = paste0("c(", paste0(simResult2$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$successPerStage[1, ], 0, label = paste0("c(", paste0(simResult2$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$successPerStage[2, ], 0.37, tolerance = 1e-07, label = paste0("c(", paste0(simResult2$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult2$selectedPopulations)), c(1, 0.41, 1, 0.59, 1, 0.39, 1, 0.41), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult2$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult2$numberOfPopulations[1, ], 4, label = paste0("c(", paste0(simResult2$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$numberOfPopulations[2, ], 2, label = paste0("c(", paste0(simResult2$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult2$expectedNumberOfEvents, 190, label = paste0("c(", paste0(simResult2$expectedNumberOfEvents, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult2$singleNumberOfEventsPerStage)), c(7.8947368, 6.4805425, 4.6052632, 4.5696194, 10.526316, 8.690946, 17.763158, 19.390381, 7.8947368, 8.6179471, 13.815789, 15.081407, 31.578947, 34.471788, 5.9210526, 2.6973684), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult2$singleNumberOfEventsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult2$conditionalPowerAchieved[1, ], NA_real_, label = paste0("c(", paste0(simResult2$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$conditionalPowerAchieved[2, ], 0.47328657, tolerance = 1e-07, label = paste0("c(", paste0(simResult2$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult2), NA)))
	    expect_output(print(simResult2)$show())
	    invisible(capture.output(expect_error(summary(simResult2), NA)))
	    expect_output(summary(simResult2)$show())
	    suppressWarnings(simResult2CodeBased <- eval(parse(text = getObjectRCode(simResult2, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult2CodeBased$eventsPerStage, simResult2$eventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$iterations, simResult2$iterations, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$rejectAtLeastOne, simResult2$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$rejectedPopulationsPerStage, simResult2$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$futilityPerStage, simResult2$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$earlyStop, simResult2$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$successPerStage, simResult2$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$selectedPopulations, simResult2$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$numberOfPopulations, simResult2$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$expectedNumberOfEvents, simResult2$expectedNumberOfEvents, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$singleNumberOfEventsPerStage, simResult2$singleNumberOfEventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$conditionalPowerAchieved, simResult2$conditionalPowerAchieved, tolerance = 1e-07)
	    expect_type(names(simResult2), "character")
	    df <- as.data.frame(simResult2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
	piControls <- (1:8) / 10
	effectList2 <- list(
	    subGroups = subGroups, prevalences = prevalences,
	    piControls = piControls, hazardRatios = hazardRatios
	)

	suppressWarnings(simResult3 <- getSimulationEnrichmentSurvival(design,
	    plannedEvents = c(100, 200),
	    effectList = effectList2,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 2,
	    typeOfSelection = "rBest",
	    rValue = 2,
	    adaptations = c(T),
	    intersectionTest = "Bonferroni",
	    seed = 123
	))


	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult3' with expected results
	expect_equal(simResult3$iterations[1, ], 100, label = paste0("c(", paste0(simResult3$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$iterations[2, ], 88, label = paste0("c(", paste0(simResult3$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult3$rejectAtLeastOne, 0.47, tolerance = 1e-07, label = paste0("c(", paste0(simResult3$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult3$rejectedPopulationsPerStage)), c(0, 0.2, 0, 0.29, 0, 0.16, 0, 0.21), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult3$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult3$futilityPerStage[1, ], 0.12, tolerance = 1e-07, label = paste0("c(", paste0(simResult3$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$earlyStop[1, ], 0.12, tolerance = 1e-07, label = paste0("c(", paste0(simResult3$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$successPerStage[1, ], 0, label = paste0("c(", paste0(simResult3$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$successPerStage[2, ], 0.39, tolerance = 1e-07, label = paste0("c(", paste0(simResult3$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult3$selectedPopulations)), c(1, 0.44, 1, 0.56, 1, 0.31, 1, 0.45), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult3$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult3$numberOfPopulations[1, ], 4, label = paste0("c(", paste0(simResult3$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$numberOfPopulations[2, ], 2, label = paste0("c(", paste0(simResult3$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult3$expectedNumberOfEvents, 188, label = paste0("c(", paste0(simResult3$expectedNumberOfEvents, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult3$singleNumberOfEventsPerStage)), c(1.7600545, 1.5465305, 2.0147151, 1.9724525, 6.6217708, 5.219482, 13.921345, 14.858066, 8.8002727, 9.3924133, 17.28183, 18.444665, 41.737343, 44.545708, 7.8626685, 4.0206827), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult3$singleNumberOfEventsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult3$conditionalPowerAchieved[1, ], NA_real_, label = paste0("c(", paste0(simResult3$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$conditionalPowerAchieved[2, ], 0.50609156, tolerance = 1e-07, label = paste0("c(", paste0(simResult3$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult3), NA)))
	    expect_output(print(simResult3)$show())
	    invisible(capture.output(expect_error(summary(simResult3), NA)))
	    expect_output(summary(simResult3)$show())
	    suppressWarnings(simResult3CodeBased <- eval(parse(text = getObjectRCode(simResult3, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult3CodeBased$eventsPerStage, simResult3$eventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$iterations, simResult3$iterations, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$rejectAtLeastOne, simResult3$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$rejectedPopulationsPerStage, simResult3$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$futilityPerStage, simResult3$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$earlyStop, simResult3$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$successPerStage, simResult3$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$selectedPopulations, simResult3$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$numberOfPopulations, simResult3$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$expectedNumberOfEvents, simResult3$expectedNumberOfEvents, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$singleNumberOfEventsPerStage, simResult3$singleNumberOfEventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$conditionalPowerAchieved, simResult3$conditionalPowerAchieved, tolerance = 1e-07)
	    expect_type(names(simResult3), "character")
	    df <- as.data.frame(simResult3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
	suppressWarnings(simResult4 <- getSimulationEnrichmentSurvival(design,
	    plannedEvents = c(100, 200),
	    effectList = effectList2,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 2,
	    typeOfSelection = "rBest",
	    rValue = 2,
	    adaptations = c(T),
	    intersectionTest = "Bonferroni",
	    seed = 123
	))


	## Comparison of the results of SimulationResultsEnrichmentSurvival object 'simResult4' with expected results
	expect_equal(simResult4$iterations[1, ], 100, label = paste0("c(", paste0(simResult4$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$iterations[2, ], 88, label = paste0("c(", paste0(simResult4$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult4$rejectAtLeastOne, 0.47, tolerance = 1e-07, label = paste0("c(", paste0(simResult4$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult4$rejectedPopulationsPerStage)), c(0, 0.2, 0, 0.29, 0, 0.16, 0, 0.21), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult4$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult4$futilityPerStage[1, ], 0.12, tolerance = 1e-07, label = paste0("c(", paste0(simResult4$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$earlyStop[1, ], 0.12, tolerance = 1e-07, label = paste0("c(", paste0(simResult4$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$successPerStage[1, ], 0, label = paste0("c(", paste0(simResult4$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$successPerStage[2, ], 0.39, tolerance = 1e-07, label = paste0("c(", paste0(simResult4$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult4$selectedPopulations)), c(1, 0.44, 1, 0.56, 1, 0.31, 1, 0.45), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult4$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult4$numberOfPopulations[1, ], 4, label = paste0("c(", paste0(simResult4$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$numberOfPopulations[2, ], 2, label = paste0("c(", paste0(simResult4$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult4$expectedNumberOfEvents, 188, label = paste0("c(", paste0(simResult4$expectedNumberOfEvents, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult4$singleNumberOfEventsPerStage)), c(1.7600545, 1.5465305, 2.0147151, 1.9724525, 6.6217708, 5.219482, 13.921345, 14.858066, 8.8002727, 9.3924133, 17.28183, 18.444665, 41.737343, 44.545708, 7.8626685, 4.0206827), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult4$singleNumberOfEventsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult4$conditionalPowerAchieved[1, ], NA_real_, label = paste0("c(", paste0(simResult4$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$conditionalPowerAchieved[2, ], 0.50609156, tolerance = 1e-07, label = paste0("c(", paste0(simResult4$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult4), NA)))
	    expect_output(print(simResult4)$show())
	    invisible(capture.output(expect_error(summary(simResult4), NA)))
	    expect_output(summary(simResult4)$show())
	    suppressWarnings(simResult4CodeBased <- eval(parse(text = getObjectRCode(simResult4, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult4CodeBased$eventsPerStage, simResult4$eventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$iterations, simResult4$iterations, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$rejectAtLeastOne, simResult4$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$rejectedPopulationsPerStage, simResult4$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$futilityPerStage, simResult4$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$earlyStop, simResult4$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$successPerStage, simResult4$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$selectedPopulations, simResult4$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$numberOfPopulations, simResult4$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$expectedNumberOfEvents, simResult4$expectedNumberOfEvents, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$singleNumberOfEventsPerStage, simResult4$singleNumberOfEventsPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$conditionalPowerAchieved, simResult4$conditionalPowerAchieved, tolerance = 1e-07)
	    expect_type(names(simResult4), "character")
	    df <- as.data.frame(simResult4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult4)
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
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalEventsWithControls}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalLogRanks}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	effectList <- list(
	    subGroups = "F", prevalences = 1, stDevs = 1.3,
	    hazardRatios = matrix(seq(0.6, 1, 0.05), ncol = 1)
	)

	design <- getDesignInverseNormal(
	    informationRates = c(0.3, 0.7, 1),
	    typeOfDesign = "asKD", gammaA = 2.4
	)

	suppressWarnings(x1 <- getSimulationEnrichmentSurvival(design,
	    plannedEvents = c(50, 100, 180), effectList = effectList,
	    maxNumberOfIterations = 100, allocationRatioPlanned = 1,
	    directionUpper = FALSE,
	    conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA, 10, 10),
	    maxNumberOfEventsPerStage = c(NA, 100, 100),
	    seed = 123
	))

	x2 <- getSimulationSurvival(design,
	    plannedEvents = c(50, 100, 180), hazardRatio = seq(0.6, 1, 0.05),
	    directionUpper = FALSE,
	    maxNumberOfSubjects = 1500, maxNumberOfIterations = 100,
	    allocation1 = 1, allocation2 = 1, longTimeSimulationAllowed = TRUE,
	    conditionalPower = 0.8, minNumberOfEventsPerStage = c(NA, 10, 10),
	    maxNumberOfEventsPerStage = c(NA, 100, 100),
	    seed = 123
	)

	comp1 <- x2$overallReject - x1$rejectAtLeastOne

	## Comparison of the results of numeric object 'comp1' with expected results
	expect_equal(comp1, c(-0.05, 0.03, 0.03, 0.01, 0.07, 0.08, 0.05, -0.05, -0.02), tolerance = 1e-07, label = paste0("c(", paste0(comp1, collapse = ", "), ")"))

	comp2 <- x2$conditionalPowerAchieved - x1$conditionalPowerAchieved

	## Comparison of the results of matrixarray object 'comp2' with expected results
	expect_equal(comp2[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(comp2[1, ], collapse = ", "), ")"))
	expect_equal(comp2[2, ], c(-0.022566213, -0.0056751237, 0.047207778, 0.035251356, 0.033740719, -0.051453144, 0.039406427, 0.0072692294, -0.022722897), tolerance = 1e-07, label = paste0("c(", paste0(comp2[2, ], collapse = ", "), ")"))
	expect_equal(comp2[3, ], c(0.025359011, -0.021253382, 0.092581664, -0.080566447, 0.087298305, -0.050787114, 0.070673698, 0.019777739, -0.019114098), tolerance = 1e-07, label = paste0("c(", paste0(comp2[3, ], collapse = ", "), ")"))

	comp3 <- x2$expectedNumberOfEvents - x1$expectedNumberOfEvents

	## Comparison of the results of numeric object 'comp3' with expected results
	expect_equal(comp3, c(5.6713987, 8.8976119, -9.7670181, -2.0326559, -2.7081522, -0.88153519, -5.5780096, 3.3199537, 1.2334371), tolerance = 1e-07, label = paste0("c(", paste0(comp3, collapse = ", "), ")"))

})

test_that("'getSimulationEnrichmentSurvival': comparison of base and enrichment for Fisher combination", {

	.skipTestIfDisabled()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:enrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulationFunctions}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalEvents}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalEventsWithControls}
	# @refFS[Formula]{fs:simulationEnrichmentSurvivalLogRanks}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	effectList <- list(
	    subGroups = "F", prevalences = 1, stDevs = 1.3,
	    hazardRatios = matrix(seq(0.6, 1, 0.05), ncol = 1)
	)

	design <- getDesignFisher(informationRates = c(0.3, 0.6, 1))

	suppressWarnings(x1 <- getSimulationEnrichmentSurvival(design,
	    plannedEvents = c(50, 100, 180), effectList = effectList,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    directionUpper = FALSE,
	    conditionalPower = 0.8,
	    minNumberOfEventsPerStage = c(NA, 10, 10),
	    maxNumberOfEventsPerStage = c(NA, 100, 100),
	    seed = 123
	))

	x2 <- getSimulationSurvival(design,
	    plannedEvents = c(50, 100, 180), hazardRatio = seq(0.6, 1, 0.05),
	    directionUpper = FALSE,
	    maxNumberOfSubjects = 1500, maxNumberOfIterations = 100,
	    allocation1 = 1, allocation2 = 1,
	    conditionalPower = 0.8,
	    minNumberOfEventsPerStage = c(NA, 10, 10),
	    maxNumberOfEventsPerStage = c(NA, 100, 100),
	    seed = 123
	)

	comp4 <- x2$overallReject - x1$rejectAtLeastOne

	## Comparison of the results of numeric object 'comp4' with expected results
	expect_equal(comp4, c(-0.08, 0.02, 0.12, 0.02, 0.04, 0.04, 0.04, -0.03, 0), tolerance = 1e-07, label = paste0("c(", paste0(comp4, collapse = ", "), ")"))

	comp5 <- x2$conditionalPowerAchieved - x1$conditionalPowerAchieved

	## Comparison of the results of matrixarray object 'comp5' with expected results
	expect_equal(comp5[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(comp5[1, ], collapse = ", "), ")"))
	expect_equal(comp5[2, ], c(-0.067329229, 0.0040653837, 0.025600632, 0.024680224, 0.025189093, -0.043591198, 0.033525993, -0.0055417344, -0.031790612), tolerance = 1e-07, label = paste0("c(", paste0(comp5[2, ], collapse = ", "), ")"))
	expect_equal(comp5[3, ], c(0.012384997, 0.030980232, 0.047012202, -0.035304718, 0.068468504, 0.00374058, 0.042913189, -0.015210788, -0.017776302), tolerance = 1e-07, label = paste0("c(", paste0(comp5[3, ], collapse = ", "), ")"))

	comp6 <- x2$expectedNumberOfEvents - x1$expectedNumberOfEvents

	## Comparison of the results of numeric object 'comp6' with expected results
	expect_equal(comp6, c(5.1347448, 9.1286427, -16.823834, -1.3136156, 0.71128925, 1.9694657, -7.1208497, -0.94699441, -0.085337992), tolerance = 1e-07, label = paste0("c(", paste0(comp6, collapse = ", "), ")"))
})

