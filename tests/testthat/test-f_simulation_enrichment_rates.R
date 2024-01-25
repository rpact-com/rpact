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
## |  File name: test-f_simulation_enrichment_rates.R
## |  Creation date: 08 November 2023, 09:11:16
## |  File version: $Revision: 7560 $
## |  Last changed: $Date: 2024-01-15 14:20:32 +0100 (Mo, 15 Jan 2024) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing Simulation Enrichment Rates Function")


test_that("'getSimulationEnrichmentRates': gMax = 2", {
	.skipTestIfDisabled()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:enrichmentDesigns}
	# @refFS[Sec.]{fs:subsec:intersectionTestsEnrichment}
	# @refFS[Sec.]{fs:subsec:adaptiveClosedTestProcedureEnrichment}
	# @refFS[Sec.]{fs:subsec:intersectionTestsEnrichment}
	# @refFS[Sec.]{fs:sec:simulationFunctions}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:testStatisticEnrichmentRates}
	# @refFS[Formula]{fs:stratifiedTestEnrichmentRates}
	# @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichment}
	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichmentRates}
	# @refFS[Formula]{fs:simulationEnrichmentRatesGenerate}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	piInput <- c(0.3, 0.5, 0.3, 0.6, 0.3, 0.7, 0.3, 0.8, 0.4, 0.5, 0.4, 0.6, 0.4, 0.7, 0.4, 0.8, 0.5, 0.5, 0.5, 0.6, 0.5, 0.7, 0.5, 0.8)

	effectList <- list(
	    subGroups = c("S", "R"),
	    prevalences = c(0.74, 0.26),
	    piControl = c(0.3, 0.5),
	    piTreatments = matrix(piInput, byrow = TRUE, ncol = 2)
	)

	design <- getDesignInverseNormal(informationRates = c(0.4, 1), typeOfDesign = "WT", deltaWT = 0.1)

	suppressWarnings(simResult1 <- getSimulationEnrichmentRates(design,
	    plannedSubjects = c(150, 300), effectList = effectList,
	    maxNumberOfIterations = 100, effectMeasure = "effectEstimate", stratifiedAnalysis = TRUE,
	    allocationRatioPlanned = 2, directionUpper = TRUE,
	    successCriterion = "atLeastOne",
	    typeOfSelection = "rbest", rValue = 2,
	    intersectionTest = "SpiessensDebois", seed = 123
	))


	## Comparison of the results of SimulationResultsEnrichmentRates object 'simResult1' with expected results
	expect_equal(simResult1$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100), label = paste0("c(", paste0(simResult1$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$iterations[2, ], c(100, 100, 99, 96, 98, 97, 93, 89, 86, 88, 65, 59), label = paste0("c(", paste0(simResult1$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult1$rejectAtLeastOne, c(0.03, 0.03, 0.17, 0.17, 0.26, 0.41, 0.47, 0.63, 0.8, 0.84, 0.86, 0.99), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult1$rejectedPopulationsPerStage)), c(0, 0.02, 0, 0.02, 0, 0.05, 0, 0.02, 0.01, 0.23, 0.03, 0.28, 0.01, 0.27, 0.02, 0.19, 0.11, 0.65, 0.09, 0.68, 0.22, 0.44, 0.18, 0.41, 0, 0.02, 0, 0.03, 0.01, 0.16, 0.04, 0.13, 0.02, 0.2, 0.03, 0.35, 0.07, 0.4, 0.11, 0.52, 0.1, 0.57, 0.1, 0.7, 0.32, 0.49, 0.39, 0.58), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult1$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult1$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0("c(", paste0(simResult1$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$earlyStop[1, ], c(0, 0, 0.01, 0.04, 0.02, 0.03, 0.07, 0.11, 0.14, 0.12, 0.35, 0.41), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$successPerStage[1, ], c(0, 0, 0.01, 0.04, 0.02, 0.03, 0.07, 0.11, 0.14, 0.12, 0.35, 0.41), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$successPerStage[2, ], c(0.03, 0.03, 0.16, 0.13, 0.24, 0.38, 0.4, 0.52, 0.66, 0.72, 0.51, 0.58), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult1$selectedPopulations)), c(1, 1, 1, 1, 1, 0.99, 1, 0.96, 1, 0.98, 1, 0.97, 1, 0.93, 1, 0.89, 1, 0.86, 1, 0.88, 1, 0.65, 1, 0.59, 1, 1, 1, 1, 1, 0.99, 1, 0.96, 1, 0.98, 1, 0.97, 1, 0.93, 1, 0.89, 1, 0.86, 1, 0.88, 1, 0.65, 1, 0.59), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult1$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult1$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), label = paste0("c(", paste0(simResult1$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$numberOfPopulations[2, ], c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), label = paste0("c(", paste0(simResult1$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult1$expectedNumberOfSubjects, c(300, 300, 298.5, 294, 297, 295.5, 289.5, 283.5, 279, 282, 247.5, 238.5), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult1$sampleSizes)), c(111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39), label = paste0("c(", paste0(unlist(as.list(simResult1$sampleSizes)), collapse = ", "), ")"))
	expect_equal(simResult1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(simResult1$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$conditionalPowerAchieved[2, ], c(0.057886457, 0.11722504, 0.17374263, 0.14254287, 0.24091794, 0.35196657, 0.39807899, 0.36830797, 0.54596748, 0.63396607, 0.61766608, 0.68903084), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult1), NA)))
	    expect_output(print(simResult1)$show())
	    invisible(capture.output(expect_error(summary(simResult1), NA)))
	    expect_output(summary(simResult1)$show())
	    suppressWarnings(simResult1CodeBased <- eval(parse(text = getObjectRCode(simResult1, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult1CodeBased$iterations, simResult1$iterations, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$rejectAtLeastOne, simResult1$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$rejectedPopulationsPerStage, simResult1$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$futilityPerStage, simResult1$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$earlyStop, simResult1$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$successPerStage, simResult1$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$selectedPopulations, simResult1$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$numberOfPopulations, simResult1$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$expectedNumberOfSubjects, simResult1$expectedNumberOfSubjects, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$sampleSizes, simResult1$sampleSizes, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$conditionalPowerAchieved, simResult1$conditionalPowerAchieved, tolerance = 1e-07)
	    expect_type(names(simResult1), "character")
	    df <- as.data.frame(simResult1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
	.skipTestIfNotX64()

	suppressWarnings(simResult2 <- getSimulationEnrichmentRates(design,
	    plannedSubjects = c(150, 300), effectList = effectList,
	    maxNumberOfIterations = 100, effectMeasure = "effectEstimate", stratifiedAnalysis = TRUE,
	    piTreatmentH1 = 0.6, piControlH1 = 0.45,
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(NA, 150), maxNumberOfSubjectsPerStage = c(NA, 600),
	    allocationRatioPlanned = 2, directionUpper = TRUE,
	    successCriterion = "atLeastOne",
	    typeOfSelection = "epsilon", epsilonValue = 0.025,
	    intersectionTest = "Simes", seed = 123
	))


	## Comparison of the results of SimulationResultsEnrichmentRates object 'simResult2' with expected results
	expect_equal(simResult2$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100), label = paste0("c(", paste0(simResult2$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$iterations[2, ], c(99, 100, 99, 96, 100, 95, 91, 91, 83, 79, 79, 63), label = paste0("c(", paste0(simResult2$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult2$rejectAtLeastOne, c(0.04, 0.04, 0.15, 0.36, 0.41, 0.54, 0.7, 0.92, 0.94, 0.93, 0.97, 0.98), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult2$rejectedPopulationsPerStage)), c(0.01, 0.03, 0, 0.01, 0.01, 0.01, 0, 0, 0, 0.36, 0.03, 0.31, 0.03, 0.22, 0.02, 0.12, 0.16, 0.73, 0.18, 0.55, 0.16, 0.45, 0.23, 0.29, 0, 0.01, 0, 0.03, 0.01, 0.14, 0.04, 0.32, 0, 0.14, 0.04, 0.38, 0.09, 0.52, 0.09, 0.8, 0.11, 0.23, 0.2, 0.44, 0.21, 0.57, 0.37, 0.51), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult2$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult2$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0("c(", paste0(simResult2$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$earlyStop[1, ], c(0.01, 0, 0.01, 0.04, 0, 0.05, 0.09, 0.09, 0.17, 0.21, 0.21, 0.37), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$successPerStage[1, ], c(0.01, 0, 0.01, 0.04, 0, 0.05, 0.09, 0.09, 0.17, 0.21, 0.21, 0.37), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$successPerStage[2, ], c(0.03, 0.04, 0.14, 0.32, 0.41, 0.49, 0.61, 0.83, 0.77, 0.72, 0.76, 0.61), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult2$selectedPopulations)), c(1, 0.64, 1, 0.55, 1, 0.3, 1, 0.13, 1, 0.87, 1, 0.63, 1, 0.44, 1, 0.28, 1, 0.77, 1, 0.62, 1, 0.49, 1, 0.32, 1, 0.71, 1, 0.78, 1, 0.93, 1, 0.95, 1, 0.44, 1, 0.74, 1, 0.79, 1, 0.87, 1, 0.31, 1, 0.49, 1, 0.59, 1, 0.51), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult2$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult2$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), label = paste0("c(", paste0(simResult2$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$numberOfPopulations[2, ], c(1.3636364, 1.33, 1.2424242, 1.125, 1.31, 1.4421053, 1.3516484, 1.2637363, 1.3012048, 1.4050633, 1.3670886, 1.3174603), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult2$expectedNumberOfSubjects, c(669.59264, 671.53258, 620.0907, 573.83864, 556.82907, 514.33552, 439.19492, 418.05629, 385.4022, 357.09909, 335.03201, 280.36711), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult2$sampleSizes)), c(111, 424.65084, 111, 412.59405, 111, 358.73817, 111, 327.31465, 111, 356.03736, 111, 302.73985, 111, 244.22837, 111, 220.27012, 111, 253.59117, 111, 214.59387, 111, 187.06662, 111, 161.81648, 39, 100.19021, 39, 108.93853, 39, 116.10091, 39, 114.18394, 39, 50.79171, 39, 80.771228, 39, 73.56825, 39, 74.297232, 39, 30.025946, 39, 47.556877, 39, 47.151107, 39, 45.115446), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult2$sampleSizes)), collapse = ", "), ")"))
	expect_equal(simResult2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(simResult2$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$conditionalPowerAchieved[2, ], c(0.47580257, 0.49594366, 0.54143038, 0.56498304, 0.65590031, 0.69185697, 0.74958231, 0.78227803, 0.78802696, 0.82212774, 0.82750537, 0.8268688), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult2), NA)))
	    expect_output(print(simResult2)$show())
	    invisible(capture.output(expect_error(summary(simResult2), NA)))
	    expect_output(summary(simResult2)$show())
	    suppressWarnings(simResult2CodeBased <- eval(parse(text = getObjectRCode(simResult2, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult2CodeBased$iterations, simResult2$iterations, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$rejectAtLeastOne, simResult2$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$rejectedPopulationsPerStage, simResult2$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$futilityPerStage, simResult2$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$earlyStop, simResult2$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$successPerStage, simResult2$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$selectedPopulations, simResult2$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$numberOfPopulations, simResult2$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$expectedNumberOfSubjects, simResult2$expectedNumberOfSubjects, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$sampleSizes, simResult2$sampleSizes, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$conditionalPowerAchieved, simResult2$conditionalPowerAchieved, tolerance = 1e-07)
	    expect_type(names(simResult2), "character")
	    df <- as.data.frame(simResult2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
	suppressWarnings(simResult3 <- getSimulationEnrichmentRates(design,
	    plannedSubjects = c(150, 300), effectList = effectList,
	    maxNumberOfIterations = 100, effectMeasure = "effectEstimate", stratifiedAnalysis = TRUE,
	    allocationRatioPlanned = 2, directionUpper = TRUE,
	    successCriterion = "atLeastOne",
	    typeOfSelection = "best",
	    intersectionTest = "Sidak", seed = 123
	))


	## Comparison of the results of SimulationResultsEnrichmentRates object 'simResult3' with expected results
	expect_equal(simResult3$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100), label = paste0("c(", paste0(simResult3$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$iterations[2, ], c(100, 100, 98, 98, 100, 97, 94, 87, 84, 89, 77, 61), label = paste0("c(", paste0(simResult3$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult3$rejectAtLeastOne, c(0.01, 0.03, 0.15, 0.21, 0.19, 0.31, 0.51, 0.62, 0.85, 0.78, 0.91, 0.96), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult3$rejectedPopulationsPerStage)), c(0, 0.01, 0, 0.01, 0, 0, 0, 0, 0, 0.17, 0.03, 0.16, 0.03, 0.1, 0.02, 0.03, 0.12, 0.58, 0.1, 0.49, 0.19, 0.44, 0.19, 0.11, 0, 0, 0, 0.02, 0.02, 0.13, 0.02, 0.19, 0, 0.02, 0.03, 0.12, 0.06, 0.35, 0.13, 0.46, 0.13, 0.11, 0.1, 0.18, 0.21, 0.24, 0.36, 0.46), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult3$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult3$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0("c(", paste0(simResult3$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$earlyStop[1, ], c(0, 0, 0.02, 0.02, 0, 0.03, 0.06, 0.13, 0.16, 0.11, 0.23, 0.39), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$successPerStage[1, ], c(0, 0, 0.02, 0.02, 0, 0.03, 0.06, 0.13, 0.16, 0.11, 0.23, 0.39), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$successPerStage[2, ], c(0.01, 0.03, 0.13, 0.19, 0.19, 0.28, 0.45, 0.49, 0.69, 0.67, 0.68, 0.57), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult3$selectedPopulations)), c(1, 0.44, 1, 0.31, 1, 0.13, 1, 0.09, 1, 0.71, 1, 0.45, 1, 0.24, 1, 0.14, 1, 0.68, 1, 0.59, 1, 0.48, 1, 0.12, 1, 0.56, 1, 0.69, 1, 0.85, 1, 0.89, 1, 0.29, 1, 0.52, 1, 0.7, 1, 0.73, 1, 0.16, 1, 0.3, 1, 0.29, 1, 0.49), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult3$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult3$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), label = paste0("c(", paste0(simResult3$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$numberOfPopulations[2, ], c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), label = paste0("c(", paste0(simResult3$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult3$expectedNumberOfSubjects, c(300, 300, 297, 297, 300, 295.5, 291, 280.5, 276, 283.5, 265.5, 241.5), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult3$sampleSizes)), c(111, 128.16, 111, 123.09, 111, 116.17347, 111, 114.58163, 111, 138.69, 111, 129.09278, 111, 120.95745, 111, 117.27586, 111, 142.57143, 111, 136.85393, 111, 135.31169, 111, 118.67213, 39, 21.84, 39, 26.91, 39, 33.826531, 39, 35.418367, 39, 11.31, 39, 20.907216, 39, 29.042553, 39, 32.724138, 39, 7.4285714, 39, 13.146067, 39, 14.688312, 39, 31.327869), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult3$sampleSizes)), collapse = ", "), ")"))
	expect_equal(simResult3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(simResult3$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$conditionalPowerAchieved[2, ], c(0.083063533, 0.12244222, 0.16903461, 0.19341855, 0.20869939, 0.28782427, 0.42698224, 0.4072498, 0.57493889, 0.6368279, 0.70412178, 0.6855194), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult3), NA)))
	    expect_output(print(simResult3)$show())
	    invisible(capture.output(expect_error(summary(simResult3), NA)))
	    expect_output(summary(simResult3)$show())
	    suppressWarnings(simResult3CodeBased <- eval(parse(text = getObjectRCode(simResult3, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult3CodeBased$iterations, simResult3$iterations, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$rejectAtLeastOne, simResult3$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$rejectedPopulationsPerStage, simResult3$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$futilityPerStage, simResult3$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$earlyStop, simResult3$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$successPerStage, simResult3$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$selectedPopulations, simResult3$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$numberOfPopulations, simResult3$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$expectedNumberOfSubjects, simResult3$expectedNumberOfSubjects, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$sampleSizes, simResult3$sampleSizes, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$conditionalPowerAchieved, simResult3$conditionalPowerAchieved, tolerance = 1e-07)
	    expect_type(names(simResult3), "character")
	    df <- as.data.frame(simResult3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
	suppressWarnings(simResult4 <- getSimulationEnrichmentRates(design,
	    plannedSubjects = c(150, 300), effectList = effectList,
	    maxNumberOfIterations = 100, effectMeasure = "effectEstimate", stratifiedAnalysis = TRUE,
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(NA, 150), maxNumberOfSubjectsPerStage = c(NA, 600),
	    allocationRatioPlanned = 2, directionUpper = TRUE,
	    successCriterion = "atLeastOne",
	    typeOfSelection = "epsilon", epsilonValue = 0.025,
	    intersectionTest = "Bonferroni", seed = 123
	))


	## Comparison of the results of SimulationResultsEnrichmentRates object 'simResult4' with expected results
	expect_equal(simResult4$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100), label = paste0("c(", paste0(simResult4$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$iterations[2, ], c(57, 60, 73, 78, 91, 90, 86, 84, 80, 79, 75, 63), label = paste0("c(", paste0(simResult4$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult4$rejectAtLeastOne, c(0.02, 0.02, 0.13, 0.38, 0.43, 0.49, 0.63, 0.82, 0.84, 0.87, 0.95, 0.97), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult4$rejectedPopulationsPerStage)), c(0, 0.01, 0, 0, 0, 0, 0, 0.02, 0, 0.38, 0.02, 0.31, 0.03, 0.16, 0.01, 0.11, 0.16, 0.63, 0.17, 0.53, 0.18, 0.49, 0.18, 0.33, 0, 0.01, 0, 0.02, 0.01, 0.12, 0.04, 0.34, 0, 0.15, 0.02, 0.31, 0.09, 0.5, 0.12, 0.66, 0.09, 0.23, 0.16, 0.37, 0.24, 0.49, 0.37, 0.48), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult4$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult4$futilityPerStage[1, ], c(0.43, 0.4, 0.26, 0.18, 0.09, 0.08, 0.05, 0.04, 0.03, 0.01, 0.01, 0), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$earlyStop[1, ], c(0.43, 0.4, 0.27, 0.22, 0.09, 0.1, 0.14, 0.16, 0.2, 0.21, 0.25, 0.37), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$successPerStage[1, ], c(0, 0, 0.01, 0.04, 0, 0.02, 0.09, 0.12, 0.17, 0.2, 0.24, 0.37), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$successPerStage[2, ], c(0.02, 0.02, 0.12, 0.34, 0.43, 0.47, 0.54, 0.7, 0.67, 0.67, 0.71, 0.6), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult4$selectedPopulations)), c(1, 0.47, 1, 0.28, 1, 0.21, 1, 0.15, 1, 0.79, 1, 0.64, 1, 0.43, 1, 0.23, 1, 0.73, 1, 0.61, 1, 0.53, 1, 0.36, 1, 0.3, 1, 0.46, 1, 0.67, 1, 0.76, 1, 0.43, 1, 0.62, 1, 0.77, 1, 0.8, 1, 0.31, 1, 0.47, 1, 0.52, 1, 0.5), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult4$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult4$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), label = paste0("c(", paste0(simResult4$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$numberOfPopulations[2, ], c(1.3508772, 1.2333333, 1.2054795, 1.1666667, 1.3406593, 1.4, 1.3953488, 1.2261905, 1.3, 1.3670886, 1.4, 1.3650794), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult4$expectedNumberOfSubjects, c(453.37572, 447.96694, 541.237, 483.73584, 535.2315, 511.41354, 448.69764, 410.71972, 362.5422, 354.38041, 338.45385, 285.45619), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult4$sampleSizes)), c(111, 459.21285, 111, 397.21147, 111, 406.83057, 111, 317.9318, 111, 364.75188, 111, 324.0256, 111, 263.10197, 111, 232.55273, 111, 230.38271, 111, 215.59706, 111, 201.103, 111, 168.7985, 39, 73.025259, 39, 99.400093, 39, 129.11053, 39, 109.93466, 39, 58.579437, 39, 77.545002, 39, 84.220859, 39, 77.827889, 39, 35.295041, 39, 43.112321, 39, 50.1688, 39, 46.211325), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult4$sampleSizes)), collapse = ", "), ")"))
	expect_equal(simResult4$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(simResult4$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$conditionalPowerAchieved[2, ], c(0.17722261, 0.20630429, 0.22165392, 0.29435606, 0.38613941, 0.45798394, 0.53716481, 0.50557573, 0.59360581, 0.71535155, 0.72089862, 0.74669086), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult4), NA)))
	    expect_output(print(simResult4)$show())
	    invisible(capture.output(expect_error(summary(simResult4), NA)))
	    expect_output(summary(simResult4)$show())
	    suppressWarnings(simResult4CodeBased <- eval(parse(text = getObjectRCode(simResult4, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult4CodeBased$iterations, simResult4$iterations, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$rejectAtLeastOne, simResult4$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$rejectedPopulationsPerStage, simResult4$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$futilityPerStage, simResult4$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$earlyStop, simResult4$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$successPerStage, simResult4$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$selectedPopulations, simResult4$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$numberOfPopulations, simResult4$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$expectedNumberOfSubjects, simResult4$expectedNumberOfSubjects, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$sampleSizes, simResult4$sampleSizes, tolerance = 1e-07)
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

test_that("'getSimulationEnrichmentRates': gMax = 3", {

	.skipTestIfDisabled()
	.skipTestIfNotX64()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:enrichmentDesigns}
	# @refFS[Sec.]{fs:subsec:intersectionTestsEnrichment}
	# @refFS[Sec.]{fs:subsec:adaptiveClosedTestProcedureEnrichment}
	# @refFS[Sec.]{fs:subsec:intersectionTestsEnrichment}
	# @refFS[Sec.]{fs:sec:simulationFunctions}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:testStatisticEnrichmentRates}
	# @refFS[Formula]{fs:stratifiedTestEnrichmentRates}
	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:simulationEnrichmentRatesGenerate}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	piTreatments <- c(
	    0.30, 0.40, 0.30, 0.55, 0.30, 0.40, 0.30, 0.75, 0.30, 0.40, 0.50, 0.55, 0.30, 0.40, 0.50,
	    0.75, 0.30, 0.60, 0.30, 0.55, 0.30, 0.60, 0.30, 0.75, 0.30, 0.60, 0.50, 0.55, 0.30, 0.60,
	    0.50, 0.75, 0.50, 0.40, 0.30, 0.55, 0.50, 0.40, 0.30, 0.75, 0.50, 0.40, 0.50, 0.55, 0.50,
	    0.40, 0.50, 0.75, 0.50, 0.60, 0.30, 0.55, 0.50, 0.60, 0.30, 0.75, 0.50, 0.60, 0.50, 0.55,
	    0.50, 0.60, 0.50, 0.75
	)

	effectList <- list(
	    subGroups = c("S1", "S2", "S12", "R"),
	    prevalences = c(0.1, 0.4, 0.2, 0.3), piControls = c(0.3, 0.4, 0.3, 0.55),
	    piTreatments = matrix(piTreatments, byrow = TRUE, ncol = 4)
	)

	design <- getDesignInverseNormal(informationRates = c(0.5, 1), typeOfDesign = "noEarlyEfficacy")

	suppressWarnings(simResult1 <- getSimulationEnrichmentRates(design,
	    plannedSubjects = c(150, 300),
	    effectList = effectList,
	    maxNumberOfIterations = 100,
	    effectMeasure = "effectEstimate", stratifiedAnalysis = TRUE,
	    allocationRatioPlanned = 1.5, directionUpper = TRUE,
	    successCriterion = "atLeastOne",
	    typeOfSelection = "epsilon", epsilonValue = 0.025,
	    intersectionTest = "Sidak",
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentRates object 'simResult1' with expected results
	expect_equal(simResult1$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100), label = paste0("c(", paste0(simResult1$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$iterations[2, ], c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100), label = paste0("c(", paste0(simResult1$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult1$rejectAtLeastOne, c(0.01, 0.11, 0.34, 0.41, 0.43, 0.52, 0.64, 0.76, 0.1, 0.13, 0.58, 0.58, 0.37, 0.63, 0.8, 0.88), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult1$rejectedPopulationsPerStage)), c(0, 0.01, 0, 0.01, 0, 0.29, 0, 0.31, 0, 0, 0, 0.02, 0, 0.16, 0, 0.1, 0, 0.08, 0, 0.08, 0, 0.51, 0, 0.42, 0, 0.08, 0, 0.06, 0, 0.43, 0, 0.4, 0, 0, 0, 0.01, 0, 0.06, 0, 0.03, 0, 0.4, 0, 0.26, 0, 0.48, 0, 0.51, 0, 0.01, 0, 0, 0, 0.05, 0, 0.03, 0, 0.26, 0, 0.19, 0, 0.42, 0, 0.37, 0, 0, 0, 0.1, 0, 0.02, 0, 0.09, 0, 0.03, 0, 0.35, 0, 0.02, 0, 0.31, 0, 0.01, 0, 0.05, 0, 0.03, 0, 0.16, 0, 0.09, 0, 0.5, 0, 0.1, 0, 0.35), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult1$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult1$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0("c(", paste0(simResult1$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$earlyStop[1, ], c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0("c(", paste0(simResult1$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$successPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0("c(", paste0(simResult1$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$successPerStage[2, ], c(0.01, 0.11, 0.34, 0.41, 0.43, 0.52, 0.64, 0.76, 0.1, 0.13, 0.58, 0.58, 0.37, 0.63, 0.8, 0.88), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult1$selectedPopulations)), c(1, 0.45, 1, 0.41, 1, 0.76, 1, 0.65, 1, 0.14, 1, 0.09, 1, 0.39, 1, 0.24, 1, 0.6, 1, 0.56, 1, 0.81, 1, 0.74, 1, 0.31, 1, 0.17, 1, 0.59, 1, 0.55, 1, 0.38, 1, 0.15, 1, 0.3, 1, 0.18, 1, 0.81, 1, 0.62, 1, 0.66, 1, 0.63, 1, 0.28, 1, 0.17, 1, 0.22, 1, 0.14, 1, 0.73, 1, 0.35, 1, 0.54, 1, 0.42, 1, 0.41, 1, 0.64, 1, 0.19, 1, 0.35, 1, 0.23, 1, 0.67, 1, 0.16, 1, 0.46, 1, 0.32, 1, 0.59, 1, 0.14, 1, 0.38, 1, 0.32, 1, 0.78, 1, 0.14, 1, 0.4), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult1$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult1$numberOfPopulations[1, ], c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3), label = paste0("c(", paste0(simResult1$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$numberOfPopulations[2, ], c(1.24, 1.2, 1.25, 1.18, 1.18, 1.38, 1.21, 1.33, 1.2, 1.32, 1.17, 1.26, 1.36, 1.3, 1.27, 1.37), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult1$expectedNumberOfSubjects, c(300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult1$sampleSizes)), c(15, 23.007143, 15, 24.6, 15, 35.778571, 15, 33.607143, 15, 8.0928571, 15, 12.764286, 15, 17.042857, 15, 13.757143, 15, 29.442857, 15, 27.278571, 15, 37.6, 15, 34.414286, 15, 15.3, 15, 16.7, 15, 25.742857, 15, 25.071429, 60, 51.028571, 60, 44.4, 60, 29.114286, 60, 30.428571, 60, 82.371429, 60, 68.057143, 60, 67.171429, 60, 69.028571, 60, 38.771429, 60, 40.114286, 60, 25.4, 60, 27.657143, 60, 68.2, 60, 58.8, 60, 51.971429, 60, 47.285714, 30, 57.514286, 30, 52.2, 30, 76.557143, 30, 70.214286, 30, 49.185714, 30, 39.028571, 30, 58.585714, 30, 46.514286, 30, 67.385714, 30, 56.057143, 30, 80.7, 30, 70.828571, 30, 52.1, 30, 39.4, 30, 65.985714, 30, 59.642857, 45, 18.45, 45, 28.8, 45, 8.55, 45, 15.75, 45, 10.35, 45, 30.15, 45, 7.2, 45, 20.7, 45, 14.4, 45, 26.55, 45, 6.3, 45, 17.1, 45, 14.4, 45, 35.1, 45, 6.3, 45, 18), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult1$sampleSizes)), collapse = ", "), ")"))
	expect_equal(simResult1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(simResult1$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$conditionalPowerAchieved[2, ], c(0.052366398, 0.10235816, 0.23768651, 0.28614763, 0.25721791, 0.27114584, 0.42018555, 0.53367483, 0.094822282, 0.17558915, 0.27651135, 0.31521608, 0.31906941, 0.3984128, 0.57056973, 0.7055787), tolerance = 1e-07, label = paste0("c(", paste0(simResult1$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult1), NA)))
	    expect_output(print(simResult1)$show())
	    invisible(capture.output(expect_error(summary(simResult1), NA)))
	    expect_output(summary(simResult1)$show())
	    suppressWarnings(simResult1CodeBased <- eval(parse(text = getObjectRCode(simResult1, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult1CodeBased$iterations, simResult1$iterations, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$rejectAtLeastOne, simResult1$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$rejectedPopulationsPerStage, simResult1$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$futilityPerStage, simResult1$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$earlyStop, simResult1$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$successPerStage, simResult1$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$selectedPopulations, simResult1$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$numberOfPopulations, simResult1$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$expectedNumberOfSubjects, simResult1$expectedNumberOfSubjects, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$sampleSizes, simResult1$sampleSizes, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$conditionalPowerAchieved, simResult1$conditionalPowerAchieved, tolerance = 1e-07)
	    expect_type(names(simResult1), "character")
	    df <- as.data.frame(simResult1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	suppressWarnings(simResult2 <- getSimulationEnrichmentRates(design,
	    plannedSubjects = c(150, 300),
	    effectList = effectList,
	    maxNumberOfIterations = 100,
	    effectMeasure = "effectEstimate", stratifiedAnalysis = TRUE,
	    allocationRatioPlanned = 1.5, directionUpper = TRUE,
	    successCriterion = "atLeastOne",
	    typeOfSelection = "rbest", rValue = 2,
	    intersectionTest = "Bonferroni",
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentRates object 'simResult2' with expected results
	expect_equal(simResult2$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100), label = paste0("c(", paste0(simResult2$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$iterations[2, ], c(63, 72, 78, 88, 91, 93, 97, 97, 67, 72, 91, 96, 91, 97, 99, 99), label = paste0("c(", paste0(simResult2$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult2$rejectAtLeastOne, c(0.03, 0.1, 0.11, 0.34, 0.28, 0.42, 0.7, 0.76, 0.04, 0.18, 0.38, 0.29, 0.27, 0.62, 0.79, 0.82), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult2$rejectedPopulationsPerStage)), c(0, 0.01, 0, 0.01, 0, 0.09, 0, 0.21, 0, 0.03, 0, 0.01, 0, 0.18, 0, 0.16, 0, 0.02, 0, 0.07, 0, 0.36, 0, 0.19, 0, 0.06, 0, 0.09, 0, 0.53, 0, 0.45, 0, 0.03, 0, 0.01, 0, 0.04, 0, 0.05, 0, 0.25, 0, 0.29, 0, 0.64, 0, 0.57, 0, 0, 0, 0.01, 0, 0.12, 0, 0.06, 0, 0.24, 0, 0.31, 0, 0.7, 0, 0.5, 0, 0.02, 0, 0.09, 0, 0.03, 0, 0.28, 0, 0.09, 0, 0.37, 0, 0.22, 0, 0.6, 0, 0.02, 0, 0.15, 0, 0.06, 0, 0.17, 0, 0.16, 0, 0.54, 0, 0.18, 0, 0.52), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult2$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult2$futilityPerStage[1, ], c(0.37, 0.28, 0.22, 0.12, 0.09, 0.07, 0.03, 0.03, 0.33, 0.28, 0.09, 0.04, 0.09, 0.03, 0.01, 0.01), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$earlyStop[1, ], c(0.37, 0.28, 0.22, 0.12, 0.09, 0.07, 0.03, 0.03, 0.33, 0.28, 0.09, 0.04, 0.09, 0.03, 0.01, 0.01), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$successPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0("c(", paste0(simResult2$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$successPerStage[2, ], c(0.03, 0.1, 0.11, 0.34, 0.28, 0.42, 0.7, 0.76, 0.04, 0.18, 0.38, 0.29, 0.27, 0.62, 0.79, 0.82), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult2$selectedPopulations)), c(1, 0.37, 1, 0.43, 1, 0.66, 1, 0.66, 1, 0.23, 1, 0.14, 1, 0.55, 1, 0.37, 1, 0.54, 1, 0.54, 1, 0.87, 1, 0.87, 1, 0.42, 1, 0.35, 1, 0.82, 1, 0.66, 1, 0.46, 1, 0.33, 1, 0.59, 1, 0.38, 1, 0.88, 1, 0.86, 1, 0.9, 1, 0.8, 1, 0.25, 1, 0.24, 1, 0.52, 1, 0.26, 1, 0.79, 1, 0.73, 1, 0.85, 1, 0.68, 1, 0.43, 1, 0.68, 1, 0.31, 1, 0.72, 1, 0.71, 1, 0.86, 1, 0.49, 1, 0.77, 1, 0.55, 1, 0.66, 1, 0.43, 1, 0.79, 1, 0.61, 1, 0.86, 1, 0.31, 1, 0.64), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult2$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult2$numberOfPopulations[1, ], c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3), label = paste0("c(", paste0(simResult2$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$numberOfPopulations[2, ], c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), label = paste0("c(", paste0(simResult2$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult2$expectedNumberOfSubjects, c(244.5, 258, 267, 282, 286.5, 289.5, 295.5, 295.5, 250.5, 258, 286.5, 294, 286.5, 295.5, 298.5, 298.5), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult2$sampleSizes)), c(15, 17.040816, 15, 15.357143, 15, 18.873626, 15, 16.168831, 15, 16.412873, 15, 15.483871, 15, 18.181149, 15, 16.325479, 15, 16.151386, 15, 15.535714, 15, 18.390895, 15, 16.138393, 15, 17.119309, 15, 15.729013, 15, 19.415584, 15, 17.272727, 60, 68.163265, 60, 61.428571, 60, 75.494505, 60, 64.675325, 60, 65.651491, 60, 61.935484, 60, 72.724595, 60, 65.301915, 60, 64.605544, 60, 62.142857, 60, 73.563579, 60, 64.553571, 60, 68.477237, 60, 62.916053, 60, 77.662338, 60, 69.090909, 30, 34.081633, 30, 30.714286, 30, 37.747253, 30, 32.337662, 30, 32.825746, 30, 30.967742, 30, 36.362297, 30, 32.650957, 30, 32.302772, 30, 31.071429, 30, 36.78179, 30, 32.276786, 30, 34.238619, 30, 31.458027, 30, 38.831169, 30, 34.545455, 45, 30.714286, 45, 42.5, 45, 17.884615, 45, 36.818182, 45, 35.10989, 45, 41.612903, 45, 22.731959, 45, 35.721649, 45, 36.940299, 45, 41.25, 45, 21.263736, 45, 37.03125, 45, 30.164835, 45, 39.896907, 45, 14.090909, 45, 29.090909), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult2$sampleSizes)), collapse = ", "), ")"))
	expect_equal(simResult2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(simResult2$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$conditionalPowerAchieved[2, ], c(0.098541448, 0.1603324, 0.18848191, 0.33019209, 0.1726177, 0.23217693, 0.48938782, 0.5528132, 0.15183095, 0.21072686, 0.29316228, 0.34756908, 0.32894823, 0.41694547, 0.62874091, 0.68601647), tolerance = 1e-07, label = paste0("c(", paste0(simResult2$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult2), NA)))
	    expect_output(print(simResult2)$show())
	    invisible(capture.output(expect_error(summary(simResult2), NA)))
	    expect_output(summary(simResult2)$show())
	    suppressWarnings(simResult2CodeBased <- eval(parse(text = getObjectRCode(simResult2, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult2CodeBased$iterations, simResult2$iterations, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$rejectAtLeastOne, simResult2$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$rejectedPopulationsPerStage, simResult2$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$futilityPerStage, simResult2$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$earlyStop, simResult2$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$successPerStage, simResult2$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$selectedPopulations, simResult2$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$numberOfPopulations, simResult2$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$expectedNumberOfSubjects, simResult2$expectedNumberOfSubjects, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$sampleSizes, simResult2$sampleSizes, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$conditionalPowerAchieved, simResult2$conditionalPowerAchieved, tolerance = 1e-07)
	    expect_type(names(simResult2), "character")
	    df <- as.data.frame(simResult2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	suppressWarnings(simResult3 <- getSimulationEnrichmentRates(design,
	    plannedSubjects = c(150, 300),
	    effectList = effectList,
	    maxNumberOfIterations = 100,
	    effectMeasure = "effectEstimate", stratifiedAnalysis = FALSE,
	    allocationRatioPlanned = 1.5, directionUpper = TRUE,
	    successCriterion = "atLeastOne",
	    typeOfSelection = "epsilon", epsilonValue = 0.025,
	    intersectionTest = "Simes",
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentRates object 'simResult3' with expected results
	expect_equal(simResult3$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100), label = paste0("c(", paste0(simResult3$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$iterations[2, ], c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100), label = paste0("c(", paste0(simResult3$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult3$rejectAtLeastOne, c(0.01, 0.09, 0.33, 0.41, 0.43, 0.49, 0.64, 0.74, 0.09, 0.13, 0.6, 0.55, 0.37, 0.59, 0.82, 0.87), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult3$rejectedPopulationsPerStage)), c(0, 0.01, 0, 0.01, 0, 0.28, 0, 0.32, 0, 0, 0, 0.02, 0, 0.15, 0, 0.11, 0, 0.07, 0, 0.08, 0, 0.53, 0, 0.42, 0, 0.08, 0, 0.05, 0, 0.45, 0, 0.42, 0, 0, 0, 0.01, 0, 0.05, 0, 0.03, 0, 0.4, 0, 0.25, 0, 0.5, 0, 0.5, 0, 0.01, 0, 0.01, 0, 0.05, 0, 0.03, 0, 0.25, 0, 0.17, 0, 0.44, 0, 0.37, 0, 0, 0, 0.08, 0, 0.02, 0, 0.08, 0, 0.03, 0, 0.31, 0, 0.01, 0, 0.28, 0, 0.01, 0, 0.04, 0, 0.03, 0, 0.14, 0, 0.11, 0, 0.46, 0, 0.1, 0, 0.34), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult3$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult3$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0("c(", paste0(simResult3$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$earlyStop[1, ], c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0("c(", paste0(simResult3$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$successPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0("c(", paste0(simResult3$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$successPerStage[2, ], c(0.01, 0.09, 0.33, 0.41, 0.43, 0.49, 0.64, 0.74, 0.09, 0.13, 0.6, 0.55, 0.37, 0.59, 0.82, 0.87), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult3$selectedPopulations)), c(1, 0.45, 1, 0.41, 1, 0.76, 1, 0.65, 1, 0.14, 1, 0.09, 1, 0.39, 1, 0.24, 1, 0.6, 1, 0.56, 1, 0.81, 1, 0.74, 1, 0.31, 1, 0.17, 1, 0.59, 1, 0.55, 1, 0.38, 1, 0.15, 1, 0.3, 1, 0.18, 1, 0.81, 1, 0.62, 1, 0.66, 1, 0.63, 1, 0.28, 1, 0.17, 1, 0.22, 1, 0.14, 1, 0.73, 1, 0.35, 1, 0.54, 1, 0.42, 1, 0.41, 1, 0.64, 1, 0.19, 1, 0.35, 1, 0.23, 1, 0.67, 1, 0.16, 1, 0.46, 1, 0.32, 1, 0.59, 1, 0.14, 1, 0.38, 1, 0.32, 1, 0.78, 1, 0.14, 1, 0.4), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult3$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult3$numberOfPopulations[1, ], c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3), label = paste0("c(", paste0(simResult3$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$numberOfPopulations[2, ], c(1.24, 1.2, 1.25, 1.18, 1.18, 1.38, 1.21, 1.33, 1.2, 1.32, 1.17, 1.26, 1.36, 1.3, 1.27, 1.37), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult3$expectedNumberOfSubjects, c(300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult3$sampleSizes)), c(15, 23.007143, 15, 24.6, 15, 35.778571, 15, 33.607143, 15, 8.0928571, 15, 12.764286, 15, 17.042857, 15, 13.757143, 15, 29.442857, 15, 27.278571, 15, 37.6, 15, 34.414286, 15, 15.3, 15, 16.7, 15, 25.742857, 15, 25.071429, 60, 51.028571, 60, 44.4, 60, 29.114286, 60, 30.428571, 60, 82.371429, 60, 68.057143, 60, 67.171429, 60, 69.028571, 60, 38.771429, 60, 40.114286, 60, 25.4, 60, 27.657143, 60, 68.2, 60, 58.8, 60, 51.971429, 60, 47.285714, 30, 57.514286, 30, 52.2, 30, 76.557143, 30, 70.214286, 30, 49.185714, 30, 39.028571, 30, 58.585714, 30, 46.514286, 30, 67.385714, 30, 56.057143, 30, 80.7, 30, 70.828571, 30, 52.1, 30, 39.4, 30, 65.985714, 30, 59.642857, 45, 18.45, 45, 28.8, 45, 8.55, 45, 15.75, 45, 10.35, 45, 30.15, 45, 7.2, 45, 20.7, 45, 14.4, 45, 26.55, 45, 6.3, 45, 17.1, 45, 14.4, 45, 35.1, 45, 6.3, 45, 18), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult3$sampleSizes)), collapse = ", "), ")"))
	expect_equal(simResult3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(simResult3$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$conditionalPowerAchieved[2, ], c(0.049725876, 0.09761839, 0.23332728, 0.27699949, 0.24938469, 0.25259324, 0.41341769, 0.52195003, 0.091306519, 0.16413348, 0.27352495, 0.30455767, 0.309829, 0.37988667, 0.56618897, 0.69760011), tolerance = 1e-07, label = paste0("c(", paste0(simResult3$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult3), NA)))
	    expect_output(print(simResult3)$show())
	    invisible(capture.output(expect_error(summary(simResult3), NA)))
	    expect_output(summary(simResult3)$show())
	    suppressWarnings(simResult3CodeBased <- eval(parse(text = getObjectRCode(simResult3, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult3CodeBased$iterations, simResult3$iterations, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$rejectAtLeastOne, simResult3$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$rejectedPopulationsPerStage, simResult3$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$futilityPerStage, simResult3$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$earlyStop, simResult3$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$successPerStage, simResult3$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$selectedPopulations, simResult3$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$numberOfPopulations, simResult3$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$expectedNumberOfSubjects, simResult3$expectedNumberOfSubjects, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$sampleSizes, simResult3$sampleSizes, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$conditionalPowerAchieved, simResult3$conditionalPowerAchieved, tolerance = 1e-07)
	    expect_type(names(simResult3), "character")
	    df <- as.data.frame(simResult3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	suppressWarnings(simResult4 <- getSimulationEnrichmentRates(design,
	    plannedSubjects = c(150, 300),
	    effectList = effectList,
	    maxNumberOfIterations = 100,
	    effectMeasure = "effectEstimate", stratifiedAnalysis = FALSE,
	    allocationRatioPlanned = 1.5, directionUpper = TRUE,
	    successCriterion = "atLeastOne",
	    typeOfSelection = "best",
	    intersectionTest = "Sidak",
	    seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentRates object 'simResult4' with expected results
	expect_equal(simResult4$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100), label = paste0("c(", paste0(simResult4$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$iterations[2, ], c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100), label = paste0("c(", paste0(simResult4$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult4$rejectAtLeastOne, c(0.01, 0.05, 0.27, 0.38, 0.42, 0.54, 0.58, 0.77, 0.07, 0.18, 0.52, 0.64, 0.39, 0.53, 0.83, 0.94), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult4$rejectedPopulationsPerStage)), c(0, 0, 0, 0.01, 0, 0.25, 0, 0.26, 0, 0, 0, 0, 0, 0.15, 0, 0.15, 0, 0.05, 0, 0.1, 0, 0.49, 0, 0.58, 0, 0.06, 0, 0.1, 0, 0.37, 0, 0.3, 0, 0.01, 0, 0, 0, 0.02, 0, 0.02, 0, 0.39, 0, 0.25, 0, 0.42, 0, 0.45, 0, 0.01, 0, 0.01, 0, 0.02, 0, 0, 0, 0.26, 0, 0.21, 0, 0.43, 0, 0.35, 0, 0, 0, 0.04, 0, 0, 0, 0.1, 0, 0.03, 0, 0.29, 0, 0.01, 0, 0.17, 0, 0.01, 0, 0.07, 0, 0.01, 0, 0.06, 0, 0.07, 0, 0.22, 0, 0.03, 0, 0.29), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult4$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult4$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0("c(", paste0(simResult4$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$earlyStop[1, ], c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0("c(", paste0(simResult4$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$successPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = paste0("c(", paste0(simResult4$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$successPerStage[2, ], c(0.01, 0.05, 0.27, 0.38, 0.42, 0.54, 0.58, 0.77, 0.07, 0.18, 0.52, 0.64, 0.39, 0.53, 0.83, 0.94), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult4$selectedPopulations)), c(1, 0.36, 1, 0.29, 1, 0.72, 1, 0.61, 1, 0.12, 1, 0.07, 1, 0.3, 1, 0.28, 1, 0.6, 1, 0.42, 1, 0.77, 1, 0.75, 1, 0.19, 1, 0.27, 1, 0.42, 1, 0.34, 1, 0.4, 1, 0.21, 1, 0.21, 1, 0.11, 1, 0.72, 1, 0.43, 1, 0.61, 1, 0.5, 1, 0.22, 1, 0.1, 1, 0.12, 1, 0.03, 1, 0.6, 1, 0.33, 1, 0.49, 1, 0.36, 1, 0.24, 1, 0.5, 1, 0.07, 1, 0.28, 1, 0.16, 1, 0.5, 1, 0.09, 1, 0.22, 1, 0.18, 1, 0.48, 1, 0.11, 1, 0.22, 1, 0.21, 1, 0.4, 1, 0.09, 1, 0.3), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult4$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult4$numberOfPopulations[1, ], c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3), label = paste0("c(", paste0(simResult4$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$numberOfPopulations[2, ], c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), label = paste0("c(", paste0(simResult4$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult4$expectedNumberOfSubjects, c(300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300), label = paste0("c(", paste0(simResult4$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult4$sampleSizes)), c(15, 21.6, 15, 22, 15, 37.05, 15, 34.7, 15, 8.4, 15, 11, 15, 16.35, 15, 17.3, 15, 32.7, 15, 28.2, 15, 40.15, 15, 40.8, 15, 12.65, 15, 19.5, 15, 22.35, 15, 21.5, 60, 54.4, 60, 51, 60, 25.2, 60, 27.8, 60, 81.6, 60, 73, 60, 66.4, 60, 63.2, 60, 32.8, 60, 38.8, 60, 18.6, 60, 16.2, 60, 72.6, 60, 57, 60, 54.4, 60, 54, 30, 63.2, 30, 54.5, 30, 84.6, 30, 74.9, 30, 52.8, 30, 43.5, 30, 63.2, 30, 59.6, 30, 76.4, 30, 61.4, 30, 86.3, 30, 83.1, 30, 55.3, 30, 55.5, 30, 69.2, 30, 61, 45, 10.8, 45, 22.5, 45, 3.15, 45, 12.6, 45, 7.2, 45, 22.5, 45, 4.05, 45, 9.9, 45, 8.1, 45, 21.6, 45, 4.95, 45, 9.9, 45, 9.45, 45, 18, 45, 4.05, 45, 13.5), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult4$sampleSizes)), collapse = ", "), ")"))
	expect_equal(simResult4$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(simResult4$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$conditionalPowerAchieved[2, ], c(0.049846768, 0.13642814, 0.19933025, 0.24691696, 0.23422702, 0.31462001, 0.42177681, 0.55370896, 0.056314813, 0.13292646, 0.2493284, 0.31063163, 0.27530592, 0.41566754, 0.59151016, 0.69993156), tolerance = 1e-07, label = paste0("c(", paste0(simResult4$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult4), NA)))
	    expect_output(print(simResult4)$show())
	    invisible(capture.output(expect_error(summary(simResult4), NA)))
	    expect_output(summary(simResult4)$show())
	    suppressWarnings(simResult4CodeBased <- eval(parse(text = getObjectRCode(simResult4, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult4CodeBased$iterations, simResult4$iterations, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$rejectAtLeastOne, simResult4$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$rejectedPopulationsPerStage, simResult4$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$futilityPerStage, simResult4$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$earlyStop, simResult4$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$successPerStage, simResult4$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$selectedPopulations, simResult4$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$numberOfPopulations, simResult4$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$expectedNumberOfSubjects, simResult4$expectedNumberOfSubjects, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$sampleSizes, simResult4$sampleSizes, tolerance = 1e-07)
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

test_that("'getSimulationEnrichmentRates': gMax = 4", {

	.skipTestIfDisabled()
	.skipTestIfNotX64()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:enrichmentDesigns}
	# @refFS[Sec.]{fs:subsec:intersectionTestsEnrichment}
	# @refFS[Sec.]{fs:subsec:adaptiveClosedTestProcedureEnrichment}
	# @refFS[Sec.]{fs:subsec:intersectionTestsEnrichment}
	# @refFS[Sec.]{fs:sec:simulationFunctions}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:testStatisticEnrichmentRates}
	# @refFS[Formula]{fs:stratifiedTestEnrichmentRates}
	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:simulationEnrichmentRatesGenerate}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	effectList <- list(
	    subGroups = c("S1", "S2", "S3", "S12", "S13", "S23", "S123", "R"),
	    prevalences = c(0.1, 0.15, 0.2, 0.1, 0, 0.18, 0.1, 0.17),
	    piControl = rep(0.2, 8),
	    piTreatments = matrix(rep(0.2, 8) + c(0.1, 0.025, 0.15, 0.075, 0.03, 0.125, 0.15, 0.025), byrow = TRUE, ncol = 8)
	)

	design <- getDesignInverseNormal(
	    informationRates = c(0.4, 1),
	    typeOfDesign = "noEarlyEfficacy"
	)

	suppressWarnings(simResult1 <- getSimulationEnrichmentRates(design,
	    plannedSubjects = c(320, 640),
	    effectList = effectList,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    directionUpper = TRUE,
	    typeOfSelection = "best",
	    adaptations = c(T),
	    intersectionTest = "Sidak",
	    stratifiedAnalysis = TRUE,
	    seed = 123
	))


	## Comparison of the results of SimulationResultsEnrichmentRates object 'simResult1' with expected results
	expect_equal(simResult1$iterations[1, ], 100, label = paste0("c(", paste0(simResult1$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$iterations[2, ], 100, label = paste0("c(", paste0(simResult1$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult1$rejectAtLeastOne, 0.89, tolerance = 1e-07, label = paste0("c(", paste0(simResult1$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult1$rejectedPopulationsPerStage)), c(0, 0.33, 0, 0.08, 0, 0.46, 0, 0.02), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult1$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult1$futilityPerStage[1, ], 0, label = paste0("c(", paste0(simResult1$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$earlyStop[1, ], 0, label = paste0("c(", paste0(simResult1$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$successPerStage[1, ], 0, label = paste0("c(", paste0(simResult1$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$successPerStage[2, ], 0.89, tolerance = 1e-07, label = paste0("c(", paste0(simResult1$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult1$selectedPopulations)), c(1, 0.36, 1, 0.11, 1, 0.5, 1, 0.03), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult1$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult1$numberOfPopulations[1, ], 4, label = paste0("c(", paste0(simResult1$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$numberOfPopulations[2, ], 1, label = paste0("c(", paste0(simResult1$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult1$expectedNumberOfSubjects, 640, tolerance = 1e-07, label = paste0("c(", paste0(simResult1$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult1$sampleSizes)), c(32, 39.36, 48, 11.402264, 64, 68.586667, 32, 46.001509, 0, 0, 57.6, 73.682717, 32, 79.334843, 54.4, 1.632), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult1$sampleSizes)), collapse = ", "), ")"))
	expect_equal(simResult1$conditionalPowerAchieved[1, ], NA_real_, label = paste0("c(", paste0(simResult1$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult1$conditionalPowerAchieved[2, ], 0.52086641, tolerance = 1e-07, label = paste0("c(", paste0(simResult1$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult1), NA)))
	    expect_output(print(simResult1)$show())
	    invisible(capture.output(expect_error(summary(simResult1), NA)))
	    expect_output(summary(simResult1)$show())
	    suppressWarnings(simResult1CodeBased <- eval(parse(text = getObjectRCode(simResult1, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult1CodeBased$iterations, simResult1$iterations, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$rejectAtLeastOne, simResult1$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$rejectedPopulationsPerStage, simResult1$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$futilityPerStage, simResult1$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$earlyStop, simResult1$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$successPerStage, simResult1$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$selectedPopulations, simResult1$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$numberOfPopulations, simResult1$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$expectedNumberOfSubjects, simResult1$expectedNumberOfSubjects, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$sampleSizes, simResult1$sampleSizes, tolerance = 1e-07)
	    expect_equal(simResult1CodeBased$conditionalPowerAchieved, simResult1$conditionalPowerAchieved, tolerance = 1e-07)
	    expect_type(names(simResult1), "character")
	    df <- as.data.frame(simResult1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
	suppressWarnings(simResult2 <- getSimulationEnrichmentRates(design,
	    plannedSubjects = c(320, 640),
	    effectList = effectList,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 1,
	    directionUpper = TRUE,
	    typeOfSelection = "rbest", rValue = 2,
	    adaptations = c(T),
	    intersectionTest = "Simes",
	    stratifiedAnalysis = TRUE,
	    seed = 123
	))


	## Comparison of the results of SimulationResultsEnrichmentRates object 'simResult2' with expected results
	expect_equal(simResult2$iterations[1, ], 100, label = paste0("c(", paste0(simResult2$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$iterations[2, ], 100, label = paste0("c(", paste0(simResult2$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult2$rejectAtLeastOne, 0.72, tolerance = 1e-07, label = paste0("c(", paste0(simResult2$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult2$rejectedPopulationsPerStage)), c(0, 0.28, 0, 0.23, 0, 0.58, 0, 0.18), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult2$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult2$futilityPerStage[1, ], 0, label = paste0("c(", paste0(simResult2$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$earlyStop[1, ], 0, label = paste0("c(", paste0(simResult2$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$successPerStage[1, ], 0, label = paste0("c(", paste0(simResult2$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$successPerStage[2, ], 0.55, tolerance = 1e-07, label = paste0("c(", paste0(simResult2$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult2$selectedPopulations)), c(1, 0.54, 1, 0.35, 1, 0.77, 1, 0.34), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult2$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult2$numberOfPopulations[1, ], 4, label = paste0("c(", paste0(simResult2$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$numberOfPopulations[2, ], 2, label = paste0("c(", paste0(simResult2$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult2$expectedNumberOfSubjects, 640, label = paste0("c(", paste0(simResult2$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult2$sampleSizes)), c(32, 33.520523, 48, 39.479817, 64, 69.476358, 32, 41.84929, 0, 0, 57.6, 75.328722, 32, 41.84929, 54.4, 18.496), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult2$sampleSizes)), collapse = ", "), ")"))
	expect_equal(simResult2$conditionalPowerAchieved[1, ], NA_real_, label = paste0("c(", paste0(simResult2$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult2$conditionalPowerAchieved[2, ], 0.53747471, tolerance = 1e-07, label = paste0("c(", paste0(simResult2$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult2), NA)))
	    expect_output(print(simResult2)$show())
	    invisible(capture.output(expect_error(summary(simResult2), NA)))
	    expect_output(summary(simResult2)$show())
	    suppressWarnings(simResult2CodeBased <- eval(parse(text = getObjectRCode(simResult2, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult2CodeBased$iterations, simResult2$iterations, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$rejectAtLeastOne, simResult2$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$rejectedPopulationsPerStage, simResult2$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$futilityPerStage, simResult2$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$earlyStop, simResult2$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$successPerStage, simResult2$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$selectedPopulations, simResult2$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$numberOfPopulations, simResult2$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$expectedNumberOfSubjects, simResult2$expectedNumberOfSubjects, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$sampleSizes, simResult2$sampleSizes, tolerance = 1e-07)
	    expect_equal(simResult2CodeBased$conditionalPowerAchieved, simResult2$conditionalPowerAchieved, tolerance = 1e-07)
	    expect_type(names(simResult2), "character")
	    df <- as.data.frame(simResult2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
	suppressWarnings(simResult3 <- getSimulationEnrichmentRates(design,
	    plannedSubjects = c(320, 640),
	    effectList = effectList,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 2,
	    directionUpper = TRUE,
	    typeOfSelection = "epsilon", epsilonValue = 0.025,
	    adaptations = c(T),
	    intersectionTest = "Sidak",
	    stratifiedAnalysis = FALSE,
	    seed = 123
	))


	## Comparison of the results of SimulationResultsEnrichmentRates object 'simResult3' with expected results
	expect_equal(simResult3$iterations[1, ], 100, label = paste0("c(", paste0(simResult3$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$iterations[2, ], 100, label = paste0("c(", paste0(simResult3$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult3$rejectAtLeastOne, 0.73, tolerance = 1e-07, label = paste0("c(", paste0(simResult3$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult3$rejectedPopulationsPerStage)), c(0, 0.17, 0, 0.07, 0, 0.56, 0, 0.05), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult3$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult3$futilityPerStage[1, ], 0, label = paste0("c(", paste0(simResult3$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$earlyStop[1, ], 0, label = paste0("c(", paste0(simResult3$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$successPerStage[1, ], 0, label = paste0("c(", paste0(simResult3$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$successPerStage[2, ], 0.63, tolerance = 1e-07, label = paste0("c(", paste0(simResult3$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult3$selectedPopulations)), c(1, 0.36, 1, 0.21, 1, 0.77, 1, 0.1), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult3$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult3$numberOfPopulations[1, ], 4, label = paste0("c(", paste0(simResult3$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$numberOfPopulations[2, ], 1.44, tolerance = 1e-07, label = paste0("c(", paste0(simResult3$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult3$expectedNumberOfSubjects, 640, label = paste0("c(", paste0(simResult3$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult3$sampleSizes)), c(32, 28.456504, 48, 16.155061, 64, 87.139927, 32, 33.609257, 0, 0, 57.6, 84.256662, 32, 64.94259, 54.4, 5.44), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult3$sampleSizes)), collapse = ", "), ")"))
	expect_equal(simResult3$conditionalPowerAchieved[1, ], NA_real_, label = paste0("c(", paste0(simResult3$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult3$conditionalPowerAchieved[2, ], 0.37873435, tolerance = 1e-07, label = paste0("c(", paste0(simResult3$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult3), NA)))
	    expect_output(print(simResult3)$show())
	    invisible(capture.output(expect_error(summary(simResult3), NA)))
	    expect_output(summary(simResult3)$show())
	    suppressWarnings(simResult3CodeBased <- eval(parse(text = getObjectRCode(simResult3, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult3CodeBased$iterations, simResult3$iterations, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$rejectAtLeastOne, simResult3$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$rejectedPopulationsPerStage, simResult3$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$futilityPerStage, simResult3$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$earlyStop, simResult3$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$successPerStage, simResult3$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$selectedPopulations, simResult3$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$numberOfPopulations, simResult3$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$expectedNumberOfSubjects, simResult3$expectedNumberOfSubjects, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$sampleSizes, simResult3$sampleSizes, tolerance = 1e-07)
	    expect_equal(simResult3CodeBased$conditionalPowerAchieved, simResult3$conditionalPowerAchieved, tolerance = 1e-07)
	    expect_type(names(simResult3), "character")
	    df <- as.data.frame(simResult3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
	suppressWarnings(simResult4 <- getSimulationEnrichmentRates(design,
	    plannedSubjects = c(320, 640),
	    effectList = effectList,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 0.5,
	    directionUpper = TRUE,
	    typeOfSelection = "rbest", rValue = 1,
	    adaptations = c(T),
	    intersectionTest = "Simes",
	    stratifiedAnalysis = FALSE,
	    seed = 123
	))


	## Comparison of the results of SimulationResultsEnrichmentRates object 'simResult4' with expected results
	expect_equal(simResult4$iterations[1, ], 100, label = paste0("c(", paste0(simResult4$iterations[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$iterations[2, ], 100, label = paste0("c(", paste0(simResult4$iterations[2, ], collapse = ", "), ")"))
	expect_equal(simResult4$rejectAtLeastOne, 0.91, tolerance = 1e-07, label = paste0("c(", paste0(simResult4$rejectAtLeastOne, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult4$rejectedPopulationsPerStage)), c(0, 0.36, 0, 0.11, 0, 0.43, 0, 0.01), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult4$rejectedPopulationsPerStage)), collapse = ", "), ")"))
	expect_equal(simResult4$futilityPerStage[1, ], 0, label = paste0("c(", paste0(simResult4$futilityPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$earlyStop[1, ], 0, label = paste0("c(", paste0(simResult4$earlyStop[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$successPerStage[1, ], 0, label = paste0("c(", paste0(simResult4$successPerStage[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$successPerStage[2, ], 0.91, tolerance = 1e-07, label = paste0("c(", paste0(simResult4$successPerStage[2, ], collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult4$selectedPopulations)), c(1, 0.39, 1, 0.13, 1, 0.44, 1, 0.04), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult4$selectedPopulations)), collapse = ", "), ")"))
	expect_equal(simResult4$numberOfPopulations[1, ], 4, label = paste0("c(", paste0(simResult4$numberOfPopulations[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$numberOfPopulations[2, ], 1, label = paste0("c(", paste0(simResult4$numberOfPopulations[2, ], collapse = ", "), ")"))
	expect_equal(simResult4$expectedNumberOfSubjects, 640, label = paste0("c(", paste0(simResult4$expectedNumberOfSubjects, collapse = ", "), ")"))
	expect_equal(unlist(as.list(simResult4$sampleSizes)), c(32, 42.88, 48, 13.693585, 64, 61.226667, 32, 50.729057, 0, 0, 57.6, 69.232302, 32, 80.06239, 54.4, 2.176), tolerance = 1e-07, label = paste0("c(", paste0(unlist(as.list(simResult4$sampleSizes)), collapse = ", "), ")"))
	expect_equal(simResult4$conditionalPowerAchieved[1, ], NA_real_, label = paste0("c(", paste0(simResult4$conditionalPowerAchieved[1, ], collapse = ", "), ")"))
	expect_equal(simResult4$conditionalPowerAchieved[2, ], 0.50602278, tolerance = 1e-07, label = paste0("c(", paste0(simResult4$conditionalPowerAchieved[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(simResult4), NA)))
	    expect_output(print(simResult4)$show())
	    invisible(capture.output(expect_error(summary(simResult4), NA)))
	    expect_output(summary(simResult4)$show())
	    suppressWarnings(simResult4CodeBased <- eval(parse(text = getObjectRCode(simResult4, stringWrapParagraphWidth = NULL))))
	    expect_equal(simResult4CodeBased$iterations, simResult4$iterations, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$rejectAtLeastOne, simResult4$rejectAtLeastOne, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$rejectedPopulationsPerStage, simResult4$rejectedPopulationsPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$futilityPerStage, simResult4$futilityPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$earlyStop, simResult4$earlyStop, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$successPerStage, simResult4$successPerStage, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$selectedPopulations, simResult4$selectedPopulations, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$numberOfPopulations, simResult4$numberOfPopulations, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$expectedNumberOfSubjects, simResult4$expectedNumberOfSubjects, tolerance = 1e-07)
	    expect_equal(simResult4CodeBased$sampleSizes, simResult4$sampleSizes, tolerance = 1e-07)
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

test_that("'getSimulationEnrichmentRates': comparison of base and enrichment for inverse normal", {

	.skipTestIfDisabled()
	.skipTestIfNotX64()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:enrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulationFunctions}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:simulationEnrichmentRatesGenerate}
	# @refFS[Formula]{fs:testStatisticEnrichmentRates}
	# @refFS[Formula]{fs:stratifiedTestEnrichmentRates}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	effectList <- list(
	    subGroups = "F",
	    prevalences = 1,
	    piTreatments = matrix(seq(0.1, 0.4, 0.05), byrow = TRUE, ncol = 1),
	    piControl = 0.4
	)

	design <- getDesignInverseNormal(
	    informationRates = c(0.3, 0.7, 1), typeOfDesign = "asUser",
	    userAlphaSpending = c(0.001, 0.005, 0.025), futilityBounds = c(0.1, 0.2)
	)

	suppressWarnings(x1 <- getSimulationEnrichmentRates(design,
	    plannedSubjects = c(60, 120, 180),
	    effectList = effectList,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 0.5, directionUpper = FALSE,
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(NA, 10, 10), maxNumberOfSubjectsPerStage = c(NA, 180, 180),
	    seed = 123
	))

	x2 <- getSimulationRates(design,
	    plannedSubjects = c(60, 120, 180), pi1 = seq(0.1, 0.4, 0.05),
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 0.5, pi2 = 0.4, directionUpper = FALSE,
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(NA, 10, 10), maxNumberOfSubjectsPerStage = c(NA, 180, 180),
	    seed = 123
	)

	comp1 <- x2$overallReject - x1$rejectAtLeastOne

	## Comparison of the results of numeric object 'comp1' with expected results
	expect_equal(comp1, c(0.01, -0.02, -0.05, 0.05, -0.04, -0.01, 0), tolerance = 1e-07, label = paste0("c(", paste0(comp1, collapse = ", "), ")"))

	comp2 <- x2$conditionalPowerAchieved - x1$conditionalPowerAchieved

	## Comparison of the results of matrixarray object 'comp2' with expected results
	expect_equal(comp2[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(comp2[1, ], collapse = ", "), ")"))
	expect_equal(comp2[2, ], c(-0.025509072, 0.012775113, 0.070479737, -0.032229481, -0.033954309, -0.00098028605, 0.042084407), tolerance = 1e-07, label = paste0("c(", paste0(comp2[2, ], collapse = ", "), ")"))
	expect_equal(comp2[3, ], c(0.010055798, 0.0013933335, -0.038882763, 0.065726498, -0.078554562, 0.016763805, -0.040918315), tolerance = 1e-07, label = paste0("c(", paste0(comp2[3, ], collapse = ", "), ")"))

	comp3 <- x2$expectedNumberOfSubjects - x1$expectedNumberOfSubjects

	## Comparison of the results of numeric object 'comp3' with expected results
	expect_equal(comp3, c(-12.434592, -4.3689056, -8.2101141, -5.602304, 0.020282466, -29.650036, 15.243032), tolerance = 1e-07, label = paste0("c(", paste0(comp3, collapse = ", "), ")"))

})

test_that("'getSimulationEnrichmentRates': comparison of base and enrichment for Fisher combination", {

	.skipTestIfDisabled()
	.skipTestIfNotX64()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:enrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulationFunctions}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:simulationEnrichmentRatesGenerate}
	# @refFS[Formula]{fs:testStatisticEnrichmentRates}
	# @refFS[Formula]{fs:stratifiedTestEnrichmentRates}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	effectList <- list(
	    subGroups = "F",
	    prevalences = 1,
	    piTreatments = matrix(seq(0.1, 0.4, 0.05), byrow = TRUE, ncol = 1),
	    piControl = 0.4
	)

	design <- getDesignFisher(informationRates = c(0.3, 0.7, 1), method = "fullAlpha", alpha0Vec = c(0.5, 0.4), kMax = 3)

	suppressWarnings(x1 <- getSimulationEnrichmentRates(design,
	    plannedSubjects = c(60, 120, 180), effectList = effectList,
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 0.5, directionUpper = FALSE,
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(NA, 10, 10), maxNumberOfSubjectsPerStage = c(NA, 180, 180),
	    seed = 123
	))

	x2 <- getSimulationRates(design,
	    plannedSubjects = c(60, 120, 180), pi1 = seq(0.1, 0.4, 0.05),
	    maxNumberOfIterations = 100,
	    allocationRatioPlanned = 0.5, pi2 = 0.4, directionUpper = FALSE,
	    conditionalPower = 0.8,
	    minNumberOfSubjectsPerStage = c(NA, 10, 10), maxNumberOfSubjectsPerStage = c(NA, 180, 180),
	    seed = 123
	)

	comp4 <- x2$overallReject - x1$rejectAtLeastOne

	## Comparison of the results of numeric object 'comp4' with expected results
	expect_equal(comp4, c(0, -0.03, -0.07, -0.05, 0.06, 0, 0.03), tolerance = 1e-07, label = paste0("c(", paste0(comp4, collapse = ", "), ")"))

	comp5 <- x2$conditionalPowerAchieved - x1$conditionalPowerAchieved

	## Comparison of the results of matrixarray object 'comp5' with expected results
	expect_equal(comp5[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(comp5[1, ], collapse = ", "), ")"))
	expect_equal(comp5[2, ], c(0.047883697, -0.012456169, 0.030195535, 0.040269247, -0.012692642, 0.10456209, -0.012774146), tolerance = 1e-07, label = paste0("c(", paste0(comp5[2, ], collapse = ", "), ")"))
	expect_equal(comp5[3, ], c(0.0080078465, -0.026077103, 0.024172504, -0.036648722, -0.028435685, 0.10893012, 0.014935464), tolerance = 1e-07, label = paste0("c(", paste0(comp5[3, ], collapse = ", "), ")"))

	comp6 <- x2$expectedNumberOfSubjects - x1$expectedNumberOfSubjects

	## Comparison of the results of numeric object 'comp6' with expected results
	expect_equal(comp6, c(-15.662688, 11.12334, -9.6084771, 12.680032, -11.581166, -34.68345, -0.36035592), tolerance = 1e-07, label = paste0("c(", paste0(comp6, collapse = ", "), ")"))
})

