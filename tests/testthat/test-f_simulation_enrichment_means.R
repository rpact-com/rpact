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
## |  File name: test-f_simulation_enrichment_means.R
## |  Creation date: 12 August 2022, 09:11:47
## |  File version: $Revision: 6485 $
## |  Last changed: $Date: 2022-08-12 13:20:22 +0200 (Fr, 12 Aug 2022) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing Simulation Enrichment Means Function")


test_that("'getSimulationEnrichmentMeans': gMax = 2", {
        
    .skipTestIfDisabled()
        
	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:enrichmentDesigns}
	# @refFS[Sec.]{fs:subsec:intersectionTestsEnrichment}
	# @refFS[Sec.]{fs:subsec:adaptiveClosedTestProcedureEnrichment}
	# @refFS[Sec.]{fs:subsec:intersectionTestsEnrichment}
	# @refFS[Sec.]{fs:sec:simulationFunctions}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:stratifiedtTestEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichment}
	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:simulationEnrichmentMeansGenerate}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	# do not remove
	# m <- c()
	# for (effect1 in seq(0, 0.5, 0.25)) {
	#    for (effect2 in seq(0, 0.5, 0.25)) {
	#        m <- c(m, effect1, effect2)
	#    }
	# }
	# effects <- matrix(m, byrow = TRUE, ncol = 2)

	effects <- matrix(c(0, 0, 0, 0.25, 0.25, 0.25, 0.5, 0.5, 0.5, 0, 0.25, 0.5, 0, 0.25, 0.5, 0, 0.25, 0.5), ncol = 2)

	effectList <- list(subGroups = c("S", "R"), 
			prevalences = c(0.2, 0.8), 
			stDevs = 0.8, effects = effects)

	design <- getDesignInverseNormal(informationRates = c(0.3, 1), typeOfDesign = "asUser", userAlphaSpending = c(0.01, 0.025))

	suppressWarnings(simResult1 <- getSimulationEnrichmentMeans(design,
	    populations = 2,
	    plannedSubjects = c(60, 160), effectList = effectList,
	    maxNumberOfIterations = 100, allocationRatioPlanned = 0.5,
	    typeOfSelection = "epsilon", epsilonValue = 0.1,
	    successCriterion = "atLeastOne",
	    intersectionTest = "SpiessensDebois", seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentMeans object 'simResult1' with expected results
	expect_equal(simResult1$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100, 100, 100))
	expect_equal(simResult1$iterations[2, ], c(98, 95, 78, 99, 86, 71, 94, 85, 50))
	expect_equal(simResult1$rejectAtLeastOne, c(0.03, 0.16, 0.67, 0.18, 0.42, 0.74, 0.75, 0.75, 0.92), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$rejectedPopulationsPerStage)), c(0.01, 0.01, 0, 0, 0.02, 0.01, 0.01, 0.16, 0.02, 0.15, 0, 0.06, 0.05, 0.67, 0.05, 0.49, 0.08, 0.21, 0.01, 0, 0.05, 0.11, 0.22, 0.44, 0.01, 0.01, 0.13, 0.14, 0.29, 0.39, 0.01, 0.02, 0.13, 0.13, 0.49, 0.24), tolerance = 1e-07)
	expect_equal(simResult1$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(simResult1$earlyStop[1, ], c(0.02, 0.05, 0.22, 0.01, 0.14, 0.29, 0.06, 0.15, 0.5), tolerance = 1e-07)
	expect_equal(simResult1$successPerStage[1, ], c(0.02, 0.05, 0.22, 0.01, 0.14, 0.29, 0.06, 0.15, 0.5), tolerance = 1e-07)
	expect_equal(simResult1$successPerStage[2, ], c(0.01, 0.11, 0.45, 0.17, 0.28, 0.45, 0.69, 0.6, 0.42), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$selectedPopulations)), c(1, 0.52, 1, 0.37, 1, 0.23, 1, 0.7, 1, 0.5, 1, 0.34, 1, 0.82, 1, 0.66, 1, 0.31, 1, 0.65, 1, 0.7, 1, 0.67, 1, 0.41, 1, 0.52, 1, 0.51, 1, 0.18, 1, 0.37, 1, 0.29), tolerance = 1e-07)
	expect_equal(simResult1$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2, 2, 2))
	expect_equal(simResult1$numberOfPopulations[2, ], c(1.1938776, 1.1263158, 1.1538462, 1.1212121, 1.1860465, 1.1971831, 1.0638298, 1.2117647, 1.2), tolerance = 1e-07)
	expect_equal(simResult1$expectedNumberOfSubjects, c(158, 155, 138, 159, 146, 131, 154, 145, 110))
	expect_equal(unlist(as.list(simResult1$sampleSizes)), c(12, 46.938776, 12, 41.052632, 12, 31.282051, 12, 66.868687, 12, 51.627907, 12, 42.535211, 12, 84.680851, 12, 65.176471, 12, 53.6, 48, 53.061224, 48, 58.947368, 48, 68.717949, 48, 33.131313, 48, 48.372093, 48, 57.464789, 48, 15.319149, 48, 34.823529, 48, 46.4), tolerance = 1e-07)
	expect_equal(simResult1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult1$conditionalPowerAchieved[2, ], c(0.047488291, 0.14634991, 0.18288786, 0.12148547, 0.21896362, 0.33298102, 0.17634955, 0.32251361, 0.45476897), tolerance = 1e-07)
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
	    expect_equal(simResult1CodeBased$expectedNumberOfSubjects, simResult1$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$sampleSizes, simResult1$sampleSizes, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$conditionalPowerAchieved, simResult1$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(simResult1), "character")
	    df <- as.data.frame(simResult1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	suppressWarnings(simResult2 <- getSimulationEnrichmentMeans(design,
	    populations = 2,
	    plannedSubjects = c(60, 160), effectList = effectList,
	    maxNumberOfIterations = 100, allocationRatioPlanned = 0.5,
	    typeOfSelection = "rBest", rValue = 2,
	    successCriterion = "atLeastOne",
	    intersectionTest = "Simes", seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentMeans object 'simResult2' with expected results
	expect_equal(simResult2$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100, 100, 100))
	expect_equal(simResult2$iterations[2, ], c(100, 100, 77, 98, 89, 75, 92, 87, 56))
	expect_equal(simResult2$rejectAtLeastOne, c(0.01, 0.13, 0.7, 0.05, 0.41, 0.78, 0.24, 0.49, 0.94), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$rejectedPopulationsPerStage)), c(0, 0, 0, 0.03, 0.01, 0.04, 0.01, 0.02, 0.04, 0.03, 0.02, 0.05, 0.07, 0.15, 0.07, 0.22, 0.04, 0.17, 0, 0.01, 0, 0.13, 0.23, 0.47, 0.01, 0.01, 0.08, 0.29, 0.25, 0.53, 0.01, 0.04, 0.09, 0.33, 0.44, 0.5), tolerance = 1e-07)
	expect_equal(simResult2$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(simResult2$earlyStop[1, ], c(0, 0, 0.23, 0.02, 0.11, 0.25, 0.08, 0.13, 0.44), tolerance = 1e-07)
	expect_equal(simResult2$successPerStage[1, ], c(0, 0, 0.23, 0.02, 0.11, 0.25, 0.08, 0.13, 0.44), tolerance = 1e-07)
	expect_equal(simResult2$successPerStage[2, ], c(0.01, 0.13, 0.47, 0.03, 0.3, 0.53, 0.16, 0.36, 0.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$selectedPopulations)), c(1, 1, 1, 1, 1, 0.77, 1, 0.98, 1, 0.89, 1, 0.75, 1, 0.92, 1, 0.87, 1, 0.56, 1, 1, 1, 1, 1, 0.77, 1, 0.98, 1, 0.89, 1, 0.75, 1, 0.92, 1, 0.87, 1, 0.56), tolerance = 1e-07)
	expect_equal(simResult2$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2, 2, 2))
	expect_equal(simResult2$numberOfPopulations[2, ], c(2, 2, 2, 2, 2, 2, 2, 2, 2))
	expect_equal(simResult2$expectedNumberOfSubjects, c(160, 160, 137, 158, 149, 135, 152, 147, 116), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$sampleSizes)), c(12, 20, 12, 20, 12, 20, 12, 20, 12, 20, 12, 20, 12, 20, 12, 20, 12, 20, 48, 80, 48, 80, 48, 80, 48, 80, 48, 80, 48, 80, 48, 80, 48, 80, 48, 80))
	expect_equal(simResult2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult2$conditionalPowerAchieved[2, ], c(0.068305544, 0.20988473, 0.20468607, 0.13306892, 0.26809268, 0.3042488, 0.16765633, 0.35488797, 0.3840908), tolerance = 1e-07)
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
	    expect_equal(simResult2CodeBased$expectedNumberOfSubjects, simResult2$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$sampleSizes, simResult2$sampleSizes, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$conditionalPowerAchieved, simResult2$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(simResult2), "character")
	    df <- as.data.frame(simResult2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	suppressWarnings(simResult3 <- getSimulationEnrichmentMeans(design,
	    populations = 2,
	    plannedSubjects = c(60, 160), effectList = effectList,
	    maxNumberOfIterations = 100, allocationRatioPlanned = 0.5,
	    typeOfSelection = "all",
	    successCriterion = "atLeastOne",
	    intersectionTest = "Sidak", seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentMeans object 'simResult3' with expected results
	expect_equal(simResult3$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100, 100, 100))
	expect_equal(simResult3$iterations[2, ], c(100, 100, 76, 98, 90, 76, 92, 88, 56))
	expect_equal(simResult3$rejectAtLeastOne, c(0, 0.13, 0.7, 0.05, 0.41, 0.79, 0.24, 0.48, 0.94), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult3$rejectedPopulationsPerStage)), c(0, 0, 0, 0.03, 0.01, 0.04, 0.01, 0.02, 0.03, 0.04, 0.01, 0.06, 0.07, 0.15, 0.06, 0.23, 0.04, 0.17, 0, 0, 0, 0.13, 0.24, 0.46, 0.01, 0.01, 0.07, 0.3, 0.24, 0.55, 0.01, 0.04, 0.08, 0.33, 0.44, 0.5), tolerance = 1e-07)
	expect_equal(simResult3$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(simResult3$earlyStop[1, ], c(0, 0, 0.24, 0.02, 0.1, 0.24, 0.08, 0.12, 0.44), tolerance = 1e-07)
	expect_equal(simResult3$successPerStage[1, ], c(0, 0, 0.24, 0.02, 0.1, 0.24, 0.08, 0.12, 0.44), tolerance = 1e-07)
	expect_equal(simResult3$successPerStage[2, ], c(0, 0.13, 0.46, 0.03, 0.31, 0.55, 0.16, 0.36, 0.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult3$selectedPopulations)), c(1, 1, 1, 1, 1, 0.76, 1, 0.98, 1, 0.9, 1, 0.76, 1, 0.92, 1, 0.88, 1, 0.56, 1, 1, 1, 1, 1, 0.76, 1, 0.98, 1, 0.9, 1, 0.76, 1, 0.92, 1, 0.88, 1, 0.56), tolerance = 1e-07)
	expect_equal(simResult3$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2, 2, 2))
	expect_equal(simResult3$numberOfPopulations[2, ], c(2, 2, 2, 2, 2, 2, 2, 2, 2))
	expect_equal(simResult3$expectedNumberOfSubjects, c(160, 160, 136, 158, 150, 136, 152, 148, 116), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult3$sampleSizes)), c(12, 20, 12, 20, 12, 20, 12, 20, 12, 20, 12, 20, 12, 20, 12, 20, 12, 20, 48, 80, 48, 80, 48, 80, 48, 80, 48, 80, 48, 80, 48, 80, 48, 80, 48, 80))
	expect_equal(simResult3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult3$conditionalPowerAchieved[2, ], c(0.068305544, 0.20988473, 0.2073793, 0.13306892, 0.27600384, 0.31320424, 0.16765633, 0.36196259, 0.3840908), tolerance = 1e-07)
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
	    expect_equal(simResult3CodeBased$expectedNumberOfSubjects, simResult3$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$sampleSizes, simResult3$sampleSizes, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$conditionalPowerAchieved, simResult3$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(simResult3), "character")
	    df <- as.data.frame(simResult3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	suppressWarnings(simResult4 <- getSimulationEnrichmentMeans(design,
	    populations = 2,
	    plannedSubjects = c(60, 160), effectList = effectList,
	    maxNumberOfIterations = 100, allocationRatioPlanned = 0.5,
	    typeOfSelection = "epsilon", epsilonValue = 0.1,
	    successCriterion = "all",
	    intersectionTest = "Bonferroni", seed = 123
	))

	## Comparison of the results of SimulationResultsEnrichmentMeans object 'simResult4' with expected results
	expect_equal(simResult4$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100, 100, 100))
	expect_equal(simResult4$iterations[2, ], c(62, 86, 95, 80, 91, 98, 95, 93, 93))
	expect_equal(simResult4$rejectAtLeastOne, c(0.01, 0.15, 0.63, 0.17, 0.39, 0.71, 0.69, 0.73, 0.9), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult4$rejectedPopulationsPerStage)), c(0, 0, 0, 0, 0.02, 0, 0.01, 0.16, 0.02, 0.19, 0, 0.07, 0.05, 0.62, 0.05, 0.52, 0.08, 0.36, 0.01, 0, 0.05, 0.1, 0.22, 0.41, 0.01, 0, 0.12, 0.11, 0.29, 0.37, 0.01, 0.02, 0.13, 0.12, 0.49, 0.23), tolerance = 1e-07)
	expect_equal(simResult4$futilityPerStage[1, ], c(0.38, 0.14, 0.03, 0.19, 0.08, 0.02, 0.05, 0.04, 0), tolerance = 1e-07)
	expect_equal(simResult4$earlyStop[1, ], c(0.38, 0.14, 0.05, 0.2, 0.09, 0.02, 0.05, 0.07, 0.07), tolerance = 1e-07)
	expect_equal(simResult4$successPerStage[1, ], c(0, 0, 0.02, 0.01, 0.01, 0, 0, 0.03, 0.07), tolerance = 1e-07)
	expect_equal(simResult4$successPerStage[2, ], c(0, 0.13, 0.53, 0.16, 0.3, 0.57, 0.68, 0.61, 0.76), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult4$selectedPopulations)), c(1, 0.43, 1, 0.34, 1, 0.26, 1, 0.65, 1, 0.55, 1, 0.41, 1, 0.84, 1, 0.75, 1, 0.51, 1, 0.3, 1, 0.62, 1, 0.8, 1, 0.23, 1, 0.54, 1, 0.75, 1, 0.15, 1, 0.4, 1, 0.59), tolerance = 1e-07)
	expect_equal(simResult4$numberOfPopulations[1, ], c(2, 2, 2, 2, 2, 2, 2, 2, 2))
	expect_equal(simResult4$numberOfPopulations[2, ], c(1.1774194, 1.1162791, 1.1157895, 1.1, 1.1978022, 1.1836735, 1.0421053, 1.2365591, 1.1827957), tolerance = 1e-07)
	expect_equal(simResult4$expectedNumberOfSubjects, c(122, 146, 155, 140, 151, 158, 155, 153, 153))
	expect_equal(unlist(as.list(simResult4$sampleSizes)), c(12, 61.290323, 12, 42.325581, 12, 32.631579, 12, 77, 12, 52.527473, 12, 38.77551, 12, 87.368421, 12, 65.591398, 12, 49.247312, 48, 38.709677, 48, 57.674419, 48, 67.368421, 48, 23, 48, 47.472527, 48, 61.22449, 48, 12.631579, 48, 34.408602, 48, 50.752688), tolerance = 1e-07)
	expect_equal(simResult4$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult4$conditionalPowerAchieved[2, ], c(0.10066083, 0.19572583, 0.27485551, 0.15033827, 0.32882422, 0.47317914, 0.22494724, 0.41529639, 0.62724251), tolerance = 1e-07)
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
	    expect_equal(simResult4CodeBased$expectedNumberOfSubjects, simResult4$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(simResult4CodeBased$sampleSizes, simResult4$sampleSizes, tolerance = 1e-05)
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

test_that("'getSimulationEnrichmentMeans': gMax = 3", {

	.skipTestIfDisabled()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:enrichmentDesigns}
	# @refFS[Sec.]{fs:subsec:intersectionTestsEnrichment}
	# @refFS[Sec.]{fs:subsec:adaptiveClosedTestProcedureEnrichment}
	# @refFS[Sec.]{fs:subsec:intersectionTestsEnrichment}
	# @refFS[Sec.]{fs:sec:simulationFunctions}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:stratifiedtTestEnrichment}
	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:simulationEnrichmentMeansGenerate}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	effectList <- list(
	    subGroups = c("S1", "S2", "S12", "R"),
	    prevalences = c(0.05, 0.35, 0.15, 0.45),
	    stDevs = c(2.2, 2.2, 2.2, 2.2),
	    effects = matrix(c(
	        0.3, 1.1, 0.2, 1.2,
	        2.3, 3.1, 0.9, 1.2,
	        3.1, 3.4, 0.3, 0.2,
	        1.2, 2.4, 3.7, 2.1
	    ), byrow = TRUE, ncol = 4)
	)

	design <- getDesignInverseNormal(informationRates = c(0.4, 0.8, 1), typeOfDesign = "noEarlyEfficacy")

	suppressWarnings(simResult1 <- getSimulationEnrichmentMeans(design,
	    populations = 3,
	    plannedSubjects = c(20, 40, 50), effectList = effectList,
	    maxNumberOfIterations = 100, allocationRatioPlanned = 2,
	    typeOfSelection = "rBest", rValue = 2,
	    adaptations = c(TRUE, FALSE), intersectionTest = "Sidak", seed = 123
	))


	## Comparison of the results of SimulationResultsEnrichmentMeans object 'simResult1' with expected results
	expect_equal(simResult1$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(simResult1$iterations[2, ], c(100, 100, 100, 100))
	expect_equal(simResult1$iterations[3, ], c(100, 100, 100, 100))
	expect_equal(simResult1$rejectAtLeastOne, c(0.13, 0.74, 0.67, 0.93), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$rejectedPopulationsPerStage)), c(0, 0, 0.01, 0, 0, 0.09, 0, 0, 0.09, 0, 0, 0.5, 0, 0, 0.04, 0, 0, 0.59, 0, 0, 0.59, 0, 0, 0.66, 0, 0, 0.11, 0, 0, 0.47, 0, 0, 0.31, 0, 0, 0.55), tolerance = 1e-07)
	expect_equal(simResult1$futilityStop, c(0, 0, 0, 0))
	expect_equal(simResult1$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(simResult1$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(simResult1$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(simResult1$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(simResult1$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(simResult1$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(simResult1$successPerStage[3, ], c(0.03, 0.41, 0.32, 0.78), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$selectedPopulations)), c(1, 0.54, 0.54, 1, 0.41, 0.41, 1, 0.43, 0.43, 1, 0.64, 0.64, 1, 0.68, 0.68, 1, 0.91, 0.91, 1, 0.94, 0.94, 1, 0.74, 0.74, 1, 0.78, 0.78, 1, 0.68, 0.68, 1, 0.63, 0.63, 1, 0.62, 0.62), tolerance = 1e-07)
	expect_equal(simResult1$numberOfPopulations[1, ], c(3, 3, 3, 3))
	expect_equal(simResult1$numberOfPopulations[2, ], c(2, 2, 2, 2))
	expect_equal(simResult1$numberOfPopulations[3, ], c(2, 2, 2, 2))
	expect_equal(simResult1$expectedNumberOfSubjects, c(50, 50, 50, 50), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$sampleSizes)), c(1, 1.18, 0.59, 1, 1.2618182, 0.63090909, 1, 1.3027273, 0.65136364, 1, 1.3109091, 0.65545455, 7, 8.26, 4.13, 7, 8.8327273, 4.4163636, 7, 9.1190909, 4.5595455, 7, 9.1763636, 4.5881818, 3, 3.54, 1.77, 3, 3.7854545, 1.8927273, 3, 3.9081818, 1.9540909, 3, 3.9327273, 1.9663636, 9, 7.02, 3.51, 9, 6.12, 3.06, 9, 5.67, 2.835, 9, 5.58, 2.79), tolerance = 1e-07)
	expect_equal(simResult1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult1$conditionalPowerAchieved[2, ], c(0, 0, 0, 0))
	expect_equal(simResult1$conditionalPowerAchieved[3, ], c(0.141872, 0.6853746, 0.62195245, 0.87969243), tolerance = 1e-07)
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
	    expect_equal(simResult1CodeBased$expectedNumberOfSubjects, simResult1$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$sampleSizes, simResult1$sampleSizes, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$conditionalPowerAchieved, simResult1$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(simResult1), "character")
	    df <- as.data.frame(simResult1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
	suppressWarnings(simResult2 <- getSimulationEnrichmentMeans(design,
	    populations = 3,
	    plannedSubjects = c(20, 40, 50), effectList = effectList,
	    maxNumberOfIterations = 100, allocationRatioPlanned = 2,
	    typeOfSelection = "best",
	    intersectionTest = "Simes", seed = 123
	))


	## Comparison of the results of SimulationResultsEnrichmentMeans object 'simResult2' with expected results
	expect_equal(simResult2$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(simResult2$iterations[2, ], c(100, 100, 100, 100))
	expect_equal(simResult2$iterations[3, ], c(100, 100, 100, 100))
	expect_equal(simResult2$rejectAtLeastOne, c(0.1, 0.86, 0.64, 0.95), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$rejectedPopulationsPerStage)), c(0, 0, 0, 0, 0, 0.11, 0, 0, 0.09, 0, 0, 0.45, 0, 0, 0.07, 0, 0, 0.52, 0, 0, 0.5, 0, 0, 0.36, 0, 0, 0.03, 0, 0, 0.23, 0, 0, 0.05, 0, 0, 0.14), tolerance = 1e-07)
	expect_equal(simResult2$futilityStop, c(0, 0, 0, 0))
	expect_equal(simResult2$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(simResult2$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(simResult2$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(simResult2$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(simResult2$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(simResult2$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(simResult2$successPerStage[3, ], c(0.1, 0.86, 0.64, 0.95), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$selectedPopulations)), c(1, 0.38, 0.38, 1, 0.15, 0.15, 1, 0.28, 0.28, 1, 0.46, 0.46, 1, 0.32, 0.32, 1, 0.6, 0.6, 1, 0.59, 0.59, 1, 0.37, 0.37, 1, 0.3, 0.3, 1, 0.25, 0.25, 1, 0.13, 0.13, 1, 0.17, 0.17), tolerance = 1e-07)
	expect_equal(simResult2$numberOfPopulations[1, ], c(3, 3, 3, 3))
	expect_equal(simResult2$numberOfPopulations[2, ], c(1, 1, 1, 1))
	expect_equal(simResult2$numberOfPopulations[3, ], c(1, 1, 1, 1))
	expect_equal(simResult2$expectedNumberOfSubjects, c(50, 50, 50, 50))
	expect_equal(unlist(as.list(simResult2$sampleSizes)), c(1, 2.2, 1.1, 1, 1, 0.5, 1, 1.53, 0.765, 1, 2.47, 1.235, 7, 6.58, 3.29, 7, 10.15, 5.075, 7, 9.17, 4.585, 7, 6.37, 3.185, 3, 8.52, 4.26, 3, 6.6, 3.3, 3, 8.13, 4.065, 3, 9.63, 4.815, 9, 2.7, 1.35, 9, 2.25, 1.125, 9, 1.17, 0.585, 9, 1.53, 0.765), tolerance = 1e-07)
	expect_equal(simResult2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult2$conditionalPowerAchieved[2, ], c(0, 0, 0, 0))
	expect_equal(simResult2$conditionalPowerAchieved[3, ], c(0.17206636, 0.78936816, 0.62458725, 0.9248007), tolerance = 1e-07)
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
	    expect_equal(simResult2CodeBased$expectedNumberOfSubjects, simResult2$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$sampleSizes, simResult2$sampleSizes, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$conditionalPowerAchieved, simResult2$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(simResult2), "character")
	    df <- as.data.frame(simResult2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
	suppressWarnings(simResult3 <- getSimulationEnrichmentMeans(design,
	    populations = 3,
	    plannedSubjects = c(20, 40, 50), effectList = effectList,
	    maxNumberOfIterations = 100, allocationRatioPlanned = 2,
	    typeOfSelection = "epsilon", epsilonValue = 0.1,
	    intersectionTest = "Bonferroni", seed = 123
	))


	## Comparison of the results of SimulationResultsEnrichmentMeans object 'simResult3' with expected results
	expect_equal(simResult3$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(simResult3$iterations[2, ], c(75, 96, 91, 99))
	expect_equal(simResult3$iterations[3, ], c(74, 96, 91, 99))
	expect_equal(simResult3$rejectAtLeastOne, c(0.17, 0.71, 0.67, 0.96), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult3$rejectedPopulationsPerStage)), c(0, 0, 0.04, 0, 0, 0.07, 0, 0, 0.06, 0, 0, 0.49, 0, 0, 0.07, 0, 0, 0.44, 0, 0, 0.57, 0, 0, 0.32, 0, 0, 0.06, 0, 0, 0.2, 0, 0, 0.04, 0, 0, 0.16), tolerance = 1e-07)
	expect_equal(simResult3$futilityStop, c(0.26, 0.04, 0.09, 0.01), tolerance = 1e-07)
	expect_equal(simResult3$futilityPerStage[1, ], c(0.25, 0.04, 0.09, 0.01), tolerance = 1e-07)
	expect_equal(simResult3$futilityPerStage[2, ], c(0.01, 0, 0, 0), tolerance = 1e-07)
	expect_equal(simResult3$earlyStop[1, ], c(0.25, 0.04, 0.09, 0.01), tolerance = 1e-07)
	expect_equal(simResult3$earlyStop[2, ], c(0.01, 0, 0, 0), tolerance = 1e-07)
	expect_equal(simResult3$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(simResult3$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(simResult3$successPerStage[3, ], c(0.17, 0.71, 0.67, 0.96), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult3$selectedPopulations)), c(1, 0.26, 0.25, 1, 0.2, 0.19, 1, 0.21, 0.17, 1, 0.51, 0.5, 1, 0.27, 0.26, 1, 0.54, 0.51, 1, 0.65, 0.65, 1, 0.33, 0.33, 1, 0.28, 0.23, 1, 0.28, 0.26, 1, 0.13, 0.09, 1, 0.19, 0.17), tolerance = 1e-07)
	expect_equal(simResult3$numberOfPopulations[1, ], c(3, 3, 3, 3))
	expect_equal(simResult3$numberOfPopulations[2, ], c(1.08, 1.0625, 1.0879121, 1.040404), tolerance = 1e-07)
	expect_equal(simResult3$numberOfPopulations[3, ], c(1, 1, 1, 1.010101), tolerance = 1e-07)
	expect_equal(simResult3$expectedNumberOfSubjects, c(42.4, 48.8, 47.3, 49.7), tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult3$sampleSizes)), c(1, 2.0642424, 1, 1, 1.2481061, 0.63020833, 1, 1.1368631, 0.51648352, 1, 2.6023875, 1.3324151, 7, 6.7030303, 3.5472973, 7, 9.3200758, 4.6666667, 7, 10.342657, 5.3461538, 7, 5.9843893, 2.9279155, 3, 7.8727273, 4.0540541, 3, 6.8068182, 3.484375, 3, 7.2347652, 3.6923077, 3, 9.6859504, 4.9669421, 9, 3.36, 1.3986486, 9, 2.625, 1.21875, 9, 1.2857143, 0.44505495, 9, 1.7272727, 0.77272727), tolerance = 1e-07)
	expect_equal(simResult3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(simResult3$conditionalPowerAchieved[2, ], c(0, 0, 0, 0))
	expect_equal(simResult3$conditionalPowerAchieved[3, ], c(0.31528596, 0.78554895, 0.74702653, 0.96322984), tolerance = 1e-07)
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
	    expect_equal(simResult3CodeBased$expectedNumberOfSubjects, simResult3$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(simResult3CodeBased$sampleSizes, simResult3$sampleSizes, tolerance = 1e-05)
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

test_that("'getSimulationEnrichmentMeans': gMax = 4", {

	.skipTestIfDisabled()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:enrichmentDesigns}
	# @refFS[Sec.]{fs:subsec:intersectionTestsEnrichment}
	# @refFS[Sec.]{fs:subsec:adaptiveClosedTestProcedureEnrichment}
	# @refFS[Sec.]{fs:subsec:intersectionTestsEnrichment}
	# @refFS[Sec.]{fs:sec:simulationFunctions}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:stratifiedtTestEnrichment}
	# @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
	# @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
	# @refFS[Formula]{fs:simulationEnrichmentMeansGenerate}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	effects <- matrix(c(2.3, 3.1, 0.9, 1.2, 2.1, 3.4, 0.9, 0.2), byrow = TRUE, ncol = 8)
	effectList <- list(subGroups = c("S1", "S2", "S3", "S12", "S13", "S23", "S123", "R"),
			prevalences = c(0.1, 0.05, 0.1, 0.15, 0.1, 0.15, 0.3, 0.05), effects = effects, 
			stDevs = c(rep(3.5, 4), rep(4.5, 4)))

	design <- getDesignInverseNormal(informationRates = c(0.4, 1), typeOfDesign = "noEarlyEfficacy")

	suppressWarnings(simResult1 <- getSimulationEnrichmentMeans(design,
	    populations = 4, 
		plannedSubjects = c(100, 200), 
		effectList = effectList,
	    maxNumberOfIterations = 100, allocationRatioPlanned = 2, seed = 123,
	    typeOfSelection = "epsilon", epsilonValue = 0.15, adaptations = c(T), 
		intersectionTest = "Bonferroni", stratifiedAnalysis = TRUE,
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(NA, 50), maxNumberOfSubjectsPerStage = c(NA, 200), 
		thetaH1 = 2, stDevH1 = 3
	))


	## Comparison of the results of SimulationResultsEnrichmentMeans object 'simResult1' with expected results
	expect_equal(simResult1$iterations[1, ], 100)
	expect_equal(simResult1$iterations[2, ], 97)
	expect_equal(simResult1$rejectAtLeastOne, 0.54, tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$rejectedPopulationsPerStage)), c(0, 0.12, 0, 0.21, 0, 0.19, 0, 0.08), tolerance = 1e-07)
	expect_equal(simResult1$futilityPerStage[1, ], 0.03, tolerance = 1e-07)
	expect_equal(simResult1$earlyStop[1, ], 0.03, tolerance = 1e-07)
	expect_equal(simResult1$successPerStage[1, ], 0)
	expect_equal(simResult1$successPerStage[2, ], 0.5, tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$selectedPopulations)), c(1, 0.28, 1, 0.37, 1, 0.37, 1, 0.22), tolerance = 1e-07)
	expect_equal(simResult1$numberOfPopulations[1, ], 4)
	expect_equal(simResult1$numberOfPopulations[2, ], 1.2783505, tolerance = 1e-07)
	expect_equal(simResult1$expectedNumberOfSubjects, 165.08824, tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult1$sampleSizes)), c(10, 3.9329768, 5, 2.3736653, 10, 5.1412409, 15, 9.6615922, 10, 6.744047, 15, 11.418006, 30, 26.843598, 5, 0.98615092), tolerance = 1e-07)
	expect_equal(simResult1$conditionalPowerAchieved[1, ], NA_real_)
	expect_equal(simResult1$conditionalPowerAchieved[2, ], 0.87059965, tolerance = 1e-07)
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
	    expect_equal(simResult1CodeBased$expectedNumberOfSubjects, simResult1$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$sampleSizes, simResult1$sampleSizes, tolerance = 1e-05)
	    expect_equal(simResult1CodeBased$conditionalPowerAchieved, simResult1$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(simResult1), "character")
	    df <- as.data.frame(simResult1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(simResult1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}
	suppressWarnings(simResult2 <- getSimulationEnrichmentMeans(design,
	    populations = 4, 
		plannedSubjects = c(100, 200), 
		effectList = effectList,
	    maxNumberOfIterations = 100, allocationRatioPlanned = 2, seed = 123,
	    typeOfSelection = "rbest", rValue = 2, adaptations = c(T), 
		intersectionTest = "Sidak", stratifiedAnalysis = TRUE,
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(NA, 50), maxNumberOfSubjectsPerStage = c(NA, 200), 
		thetaH1 = 2, stDevH1 = 3
	))


	## Comparison of the results of SimulationResultsEnrichmentMeans object 'simResult2' with expected results
	expect_equal(simResult2$iterations[1, ], 100)
	expect_equal(simResult2$iterations[2, ], 100)
	expect_equal(simResult2$rejectAtLeastOne, 0.55, tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$rejectedPopulationsPerStage)), c(0, 0.19, 0, 0.2, 0, 0.25, 0, 0.28), tolerance = 1e-07)
	expect_equal(simResult2$futilityPerStage[1, ], 0)
	expect_equal(simResult2$earlyStop[1, ], 0)
	expect_equal(simResult2$successPerStage[1, ], 0)
	expect_equal(simResult2$successPerStage[2, ], 0.37, tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$selectedPopulations)), c(1, 0.41, 1, 0.49, 1, 0.54, 1, 0.56), tolerance = 1e-07)
	expect_equal(simResult2$numberOfPopulations[1, ], 4)
	expect_equal(simResult2$numberOfPopulations[2, ], 2)
	expect_equal(simResult2$expectedNumberOfSubjects, 174.9744, tolerance = 1e-07)
	expect_equal(unlist(as.list(simResult2$sampleSizes)), c(10, 6.8133223, 5, 3.3795954, 10, 6.9887063, 15, 11.878041, 10, 7.9186938, 15, 11.878041, 30, 23.756082, 5, 2.3619159), tolerance = 1e-07)
	expect_equal(simResult2$conditionalPowerAchieved[1, ], NA_real_)
	expect_equal(simResult2$conditionalPowerAchieved[2, ], 0.81461286, tolerance = 1e-07)
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
	    expect_equal(simResult2CodeBased$expectedNumberOfSubjects, simResult2$expectedNumberOfSubjects, tolerance = 1e-05)
	    expect_equal(simResult2CodeBased$sampleSizes, simResult2$sampleSizes, tolerance = 1e-05)
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

test_that("'getSimulationEnrichmentMeans': comparison of base and enrichment for inverse normal", {

	.skipTestIfDisabled()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:enrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulationFunctions}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:stratifiedtTestEnrichment}
	# @refFS[Formula]{fs:simulationEnrichmentMeansGenerate}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	effectSeq <- seq(0, 0.7, 0.1)
	effects <- matrix(effectSeq, byrow = TRUE, ncol = 1)
	effectList <- list(subGroups = "F", prevalences = 1, stDevs = 1.3, effects = effects)

	design <- getDesignInverseNormal(informationRates = c(0.3, 1), typeOfDesign = "OF", futilityBounds = c(0.1))

	suppressWarnings(x1 <- getSimulationEnrichmentMeans(design,
	    populations = 1, 
		plannedSubjects = c(60, 180), effectList = effectList,
	    maxNumberOfIterations = 100, allocationRatioPlanned = 2,
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(NA, 10), maxNumberOfSubjectsPerStage = c(NA, 180), thetaH1 = 0.5,
	    seed = 123
	))

	x2 <- getSimulationMeans(design,
	    plannedSubjects = c(60, 180), alternative = effectSeq, maxNumberOfIterations = 100,
	    allocationRatioPlanned = 2, stDev = 1.3, conditionalPower = 0.8,
	    minNumberOfSubjectsPerStage = c(NA, 10), maxNumberOfSubjectsPerStage = c(NA, 180), thetaH1 = 0.5,
	    seed = 123
	)

	comp1 <- x2$overallReject - x1$rejectAtLeastOne

	## Comparison of the results of numeric object 'comp1' with expected results
	expect_equal(comp1, c(0.02, -0.01, 0.01, -0.03, 0.08, -0.05, 0.04, -0.03), tolerance = 1e-07)

	comp2 <- x2$conditionalPowerAchieved - x1$conditionalPowerAchieved

	## Comparison of the results of matrixarray object 'comp2' with expected results
	expect_equal(comp2[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(comp2[2, ], c(0.0070281375, -0.0046190664, -0.020739941, -0.011327634, -0.0046695544, 0.0025709653, 0.0032941476, 0.0045055727), tolerance = 1e-07)

	comp3 <- x2$expectedNumberOfSubjects - x1$expectedNumberOfSubjects

	## Comparison of the results of numeric object 'comp3' with expected results
	expect_equal(comp3, c(-5.9383973, -5.0998562, -5.4120322, 1.2304065, -6.6264122, -15.289639, -4.6069346, -0.41855064), tolerance = 1e-07)

})

test_that("'getSimulationEnrichmentMeans': comparison of base and enrichment for Fisher combination", {

	.skipTestIfDisabled()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:enrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulationFunctions}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentDesigns}
	# @refFS[Sec.]{fs:sec:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:stratifiedtTestEnrichment}
	# @refFS[Formula]{fs:simulationEnrichmentMeansGenerate}
	# @refFS[Formula]{fs:simulationEnrichmentSelections}
	# @refFS[Formula]{fs:simulatingEnrichmentEffectSpecification}
	# @refFS[Formula]{fs:enrichmentRejectionRule}
	effectSeq <- seq(0, 0.7, 0.1)
	effects <- matrix(effectSeq, byrow = TRUE, ncol = 1)
	effectList <- list(subGroups = "F", prevalences = 1, stDevs = 1.3, effects = effects)

	design <- getDesignFisher(informationRates = c(0.3, 1), kMax = 2)

	suppressWarnings(x1 <- getSimulationEnrichmentMeans(design,
	    populations = 1, plannedSubjects = c(60, 180), effectList = effectList,
	    maxNumberOfIterations = 100, allocationRatioPlanned = 2,
	    conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(NA, 10), maxNumberOfSubjectsPerStage = c(NA, 180), thetaH1 = 0.5,
	    seed = 123
	))

	x2 <- getSimulationMeans(design,
	    plannedSubjects = c(60, 180), alternative = effectSeq, maxNumberOfIterations = 100,
	    allocationRatioPlanned = 2, stDev = 1.3, conditionalPower = 0.8,
	    minNumberOfSubjectsPerStage = c(NA, 10), maxNumberOfSubjectsPerStage = c(NA, 180), thetaH1 = 0.5,
	    seed = 123
	)

	comp4 <- x2$overallReject - x1$rejectAtLeastOne

	## Comparison of the results of numeric object 'comp4' with expected results
	expect_equal(comp4, c(0, 0, 0.02, -0.01, 0.04, 0.03, 0.07, -0.01), tolerance = 1e-07)

	comp5 <- x2$conditionalPowerAchieved - x1$conditionalPowerAchieved

	## Comparison of the results of matrixarray object 'comp5' with expected results
	expect_equal(comp5[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(comp5[2, ], c(0, -0.0026724666, -0.020506475, -0.0095136176, -0.01871572, -0.0085381669, -0.0011844682, -0.023030147), tolerance = 1e-07)

	comp6 <- x2$expectedNumberOfSubjects - x1$expectedNumberOfSubjects

	## Comparison of the results of numeric object 'comp6' with expected results
	expect_equal(comp6, c(0, 3.5569071, 9.4761962, -1.6191689, -3.0007806, -12.622314, 2.072784, -19.12106), tolerance = 1e-07)
})

