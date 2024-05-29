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
## |  File name: test-f_analysis_multiarm_rates.R
## |  Creation date: 08 November 2023, 09:07:05
## |  File version: $Revision$
## |  Last changed: $Date$
## |  Last changed by: $Author$
## |  

test_plan_section("Testing the Analysis Rates Functionality for Three or More Treatments")

test_that("'getAnalysisResultsMultiArm' with dataset of rates", {

	design1 <- getDesignInverseNormal(
	        kMax = 4, alpha = 0.02, futilityBounds = c(-0.5, 0, 0.5),
	        bindingFutility = FALSE, typeOfDesign = "asKD", gammaA = 1.2, informationRates = c(0.15, 0.4, 0.7, 1)
	    )
	
	    design2 <- getDesignFisher(
	        kMax = 4, alpha = 0.02, alpha0Vec = c(0.7, 0.5, 0.3), method = "equalAlpha",
	        bindingFutility = TRUE, informationRates = c(0.15, 0.4, 0.7, 1)
	    )
	
	    design3 <- getDesignConditionalDunnett(alpha = 0.02, informationAtInterim = 0.4, secondStageConditioning = TRUE)
	
	    # directionUpper = TRUE
	    dataExample1 <- getDataset(
	        n1 = c(23, 25),
	        n2 = c(25, NA),
	        n3 = c(22, 29),
	        events1 = c(15, 12),
	        events2 = c(19, NA),
	        events3 = c(12, 13)
	    )
	
	    # directionUpper = FALSE
	    dataExample2 <- getDataset(
	        n1 = c(23, 25),
	        n2 = c(25, NA),
	        n3 = c(22, 29),
	        events1 = c(15, 12),
	        events2 = c(19, NA),
	        events3 = c(21, 25)
	    )


 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results1 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Simes", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results1' with expected results
	expect_equal(results1$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results1$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results1$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results1$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results1$piControl[1, ], 0.49019608, tolerance = 1e-06, label = paste0("c(", paste0(results1$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results1$conditionalRejectionProbabilities[1, ], c(0.022712676, 0.0087985227, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results1$conditionalRejectionProbabilities[2, ], c(0.038810537, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results1$conditionalPower[1, ], c(NA_real_, NA_real_, 0.004624756, 0.026737358), tolerance = 1e-06, label = paste0("c(", paste0(results1$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(results1$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results1$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(results1$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.32184855, -0.20584893, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results1$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.20645613, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results1$repeatedConfidenceIntervalUpperBounds[1, ], c(0.5011587, 0.32866179, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results1$repeatedConfidenceIntervalUpperBounds[2, ], c(0.57764375, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results1$repeatedPValues[1, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results1$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results1), NA)))
	    expect_output(print(results1)$show())
	    invisible(capture.output(expect_error(summary(results1), NA)))
	    expect_output(summary(results1)$show())
	    results1CodeBased <- eval(parse(text = getObjectRCode(results1, stringWrapParagraphWidth = NULL)))
	    expect_equal(results1CodeBased$piTreatments, results1$piTreatments, tolerance = 1e-06)
	    expect_equal(results1CodeBased$piControl, results1$piControl, tolerance = 1e-06)
	    expect_equal(results1CodeBased$conditionalRejectionProbabilities, results1$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results1CodeBased$conditionalPower, results1$conditionalPower, tolerance = 1e-06)
	    expect_equal(results1CodeBased$repeatedConfidenceIntervalLowerBounds, results1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results1CodeBased$repeatedConfidenceIntervalUpperBounds, results1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results1CodeBased$repeatedPValues, results1$repeatedPValues, tolerance = 1e-06)
	    expect_type(names(results1), "character")
	    df <- as.data.frame(results1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results2 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Simes", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results2' with expected results
	expect_equal(results2$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results2$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results2$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results2$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results2$piControl[1, ], 0.49019608, tolerance = 1e-06, label = paste0("c(", paste0(results2$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results2$conditionalRejectionProbabilities[1, ], c(0.015420568, 0.003193865, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results2$conditionalRejectionProbabilities[2, ], c(0.024462749, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results2$conditionalPower[1, ], c(NA_real_, NA_real_, 0.0010766875, 0.011284717), tolerance = 1e-06, label = paste0("c(", paste0(results2$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(results2$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results2$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(results2$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.32184855, -0.20584893, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results2$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.20645613, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results2$repeatedConfidenceIntervalUpperBounds[1, ], c(0.5011587, 0.32866179, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results2$repeatedConfidenceIntervalUpperBounds[2, ], c(0.57764375, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results2$repeatedPValues[1, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results2$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results2), NA)))
	    expect_output(print(results2)$show())
	    invisible(capture.output(expect_error(summary(results2), NA)))
	    expect_output(summary(results2)$show())
	    results2CodeBased <- eval(parse(text = getObjectRCode(results2, stringWrapParagraphWidth = NULL)))
	    expect_equal(results2CodeBased$piTreatments, results2$piTreatments, tolerance = 1e-06)
	    expect_equal(results2CodeBased$piControl, results2$piControl, tolerance = 1e-06)
	    expect_equal(results2CodeBased$conditionalRejectionProbabilities, results2$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results2CodeBased$conditionalPower, results2$conditionalPower, tolerance = 1e-06)
	    expect_equal(results2CodeBased$repeatedConfidenceIntervalLowerBounds, results2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results2CodeBased$repeatedConfidenceIntervalUpperBounds, results2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results2CodeBased$repeatedPValues, results2$repeatedPValues, tolerance = 1e-06)
	    expect_type(names(results2), "character")
	    df <- as.data.frame(results2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results3 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Sidak", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results3' with expected results
	expect_equal(results3$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results3$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results3$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results3$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results3$piControl[1, ], 0.49019608, tolerance = 1e-06, label = paste0("c(", paste0(results3$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results3$conditionalRejectionProbabilities[1, ], c(0.022712676, 0.0087985227, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results3$conditionalRejectionProbabilities[2, ], c(0.039678147, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results3$conditionalPower[1, ], c(NA_real_, NA_real_, 0.004624756, 0.026737358), tolerance = 1e-06, label = paste0("c(", paste0(results3$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(results3$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results3$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(results3$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.32182995, -0.20567448, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results3$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.20643657, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results3$repeatedConfidenceIntervalUpperBounds[1, ], c(0.50114307, 0.32784635, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results3$repeatedConfidenceIntervalUpperBounds[2, ], c(0.57762974, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results3$repeatedPValues[1, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results3$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results3), NA)))
	    expect_output(print(results3)$show())
	    invisible(capture.output(expect_error(summary(results3), NA)))
	    expect_output(summary(results3)$show())
	    results3CodeBased <- eval(parse(text = getObjectRCode(results3, stringWrapParagraphWidth = NULL)))
	    expect_equal(results3CodeBased$piTreatments, results3$piTreatments, tolerance = 1e-06)
	    expect_equal(results3CodeBased$piControl, results3$piControl, tolerance = 1e-06)
	    expect_equal(results3CodeBased$conditionalRejectionProbabilities, results3$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results3CodeBased$conditionalPower, results3$conditionalPower, tolerance = 1e-06)
	    expect_equal(results3CodeBased$repeatedConfidenceIntervalLowerBounds, results3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results3CodeBased$repeatedConfidenceIntervalUpperBounds, results3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results3CodeBased$repeatedPValues, results3$repeatedPValues, tolerance = 1e-06)
	    expect_type(names(results3), "character")
	    df <- as.data.frame(results3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results4 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Sidak", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results4' with expected results
	expect_equal(results4$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results4$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results4$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results4$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results4$piControl[1, ], 0.49019608, tolerance = 1e-06, label = paste0("c(", paste0(results4$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results4$conditionalRejectionProbabilities[1, ], c(0.015420568, 0.003193865, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results4$conditionalRejectionProbabilities[2, ], c(0.025712435, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results4$conditionalPower[1, ], c(NA_real_, NA_real_, 0.0010766875, 0.011284717), tolerance = 1e-06, label = paste0("c(", paste0(results4$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(results4$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results4$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(results4$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.32182995, -0.20567448, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results4$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.20643657, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results4$repeatedConfidenceIntervalUpperBounds[1, ], c(0.50114307, 0.32784635, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results4$repeatedConfidenceIntervalUpperBounds[2, ], c(0.57762974, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results4$repeatedPValues[1, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results4$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results4), NA)))
	    expect_output(print(results4)$show())
	    invisible(capture.output(expect_error(summary(results4), NA)))
	    expect_output(summary(results4)$show())
	    results4CodeBased <- eval(parse(text = getObjectRCode(results4, stringWrapParagraphWidth = NULL)))
	    expect_equal(results4CodeBased$piTreatments, results4$piTreatments, tolerance = 1e-06)
	    expect_equal(results4CodeBased$piControl, results4$piControl, tolerance = 1e-06)
	    expect_equal(results4CodeBased$conditionalRejectionProbabilities, results4$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results4CodeBased$conditionalPower, results4$conditionalPower, tolerance = 1e-06)
	    expect_equal(results4CodeBased$repeatedConfidenceIntervalLowerBounds, results4$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results4CodeBased$repeatedConfidenceIntervalUpperBounds, results4$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results4CodeBased$repeatedPValues, results4$repeatedPValues, tolerance = 1e-06)
	    expect_type(names(results4), "character")
	    df <- as.data.frame(results4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results5 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Bonferroni", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results5' with expected results
	expect_equal(results5$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results5$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results5$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results5$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results5$piControl[1, ], 0.49019608, tolerance = 1e-06, label = paste0("c(", paste0(results5$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results5$conditionalRejectionProbabilities[1, ], c(0.022712676, 0.0087985227, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results5$conditionalRejectionProbabilities[2, ], c(0.038810537, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results5$conditionalPower[1, ], c(NA_real_, NA_real_, 0.004624756, 0.026737358), tolerance = 1e-06, label = paste0("c(", paste0(results5$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(results5$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results5$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(results5$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.32184855, -0.20584893, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results5$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.20645613, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results5$repeatedConfidenceIntervalUpperBounds[1, ], c(0.5011587, 0.32866179, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results5$repeatedConfidenceIntervalUpperBounds[2, ], c(0.57764375, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results5$repeatedPValues[1, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results5$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results5), NA)))
	    expect_output(print(results5)$show())
	    invisible(capture.output(expect_error(summary(results5), NA)))
	    expect_output(summary(results5)$show())
	    results5CodeBased <- eval(parse(text = getObjectRCode(results5, stringWrapParagraphWidth = NULL)))
	    expect_equal(results5CodeBased$piTreatments, results5$piTreatments, tolerance = 1e-06)
	    expect_equal(results5CodeBased$piControl, results5$piControl, tolerance = 1e-06)
	    expect_equal(results5CodeBased$conditionalRejectionProbabilities, results5$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results5CodeBased$conditionalPower, results5$conditionalPower, tolerance = 1e-06)
	    expect_equal(results5CodeBased$repeatedConfidenceIntervalLowerBounds, results5$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results5CodeBased$repeatedConfidenceIntervalUpperBounds, results5$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results5CodeBased$repeatedPValues, results5$repeatedPValues, tolerance = 1e-06)
	    expect_type(names(results5), "character")
	    df <- as.data.frame(results5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results5)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results6 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Bonferroni", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results6' with expected results
	expect_equal(results6$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results6$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results6$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results6$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results6$piControl[1, ], 0.49019608, tolerance = 1e-06, label = paste0("c(", paste0(results6$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results6$conditionalRejectionProbabilities[1, ], c(0.015420568, 0.003193865, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results6$conditionalRejectionProbabilities[2, ], c(0.024462749, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results6$conditionalPower[1, ], c(NA_real_, NA_real_, 0.0010766875, 0.011284717), tolerance = 1e-06, label = paste0("c(", paste0(results6$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(results6$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results6$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(results6$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.32184855, -0.20584893, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results6$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.20645613, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results6$repeatedConfidenceIntervalUpperBounds[1, ], c(0.5011587, 0.32866179, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results6$repeatedConfidenceIntervalUpperBounds[2, ], c(0.57764375, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results6$repeatedPValues[1, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results6$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results6), NA)))
	    expect_output(print(results6)$show())
	    invisible(capture.output(expect_error(summary(results6), NA)))
	    expect_output(summary(results6)$show())
	    results6CodeBased <- eval(parse(text = getObjectRCode(results6, stringWrapParagraphWidth = NULL)))
	    expect_equal(results6CodeBased$piTreatments, results6$piTreatments, tolerance = 1e-06)
	    expect_equal(results6CodeBased$piControl, results6$piControl, tolerance = 1e-06)
	    expect_equal(results6CodeBased$conditionalRejectionProbabilities, results6$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results6CodeBased$conditionalPower, results6$conditionalPower, tolerance = 1e-06)
	    expect_equal(results6CodeBased$repeatedConfidenceIntervalLowerBounds, results6$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results6CodeBased$repeatedConfidenceIntervalUpperBounds, results6$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results6CodeBased$repeatedPValues, results6$repeatedPValues, tolerance = 1e-06)
	    expect_type(names(results6), "character")
	    df <- as.data.frame(results6)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results6)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results7 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Dunnett", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results7' with expected results
	expect_equal(results7$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results7$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results7$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results7$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results7$piControl[1, ], 0.49019608, tolerance = 1e-06, label = paste0("c(", paste0(results7$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results7$conditionalRejectionProbabilities[1, ], c(0.022712676, 0.0087985227, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results7$conditionalRejectionProbabilities[2, ], c(0.043097833, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results7$conditionalPower[1, ], c(NA_real_, NA_real_, 0.004624756, 0.026737358), tolerance = 1e-06, label = paste0("c(", paste0(results7$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(results7$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results7$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(results7$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.32069275, -0.20381959, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results7$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.20524097, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results7$repeatedConfidenceIntervalUpperBounds[1, ], c(0.50018706, 0.32441791, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results7$repeatedConfidenceIntervalUpperBounds[2, ], c(0.5767715, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results7$repeatedPValues[1, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results7$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results7), NA)))
	    expect_output(print(results7)$show())
	    invisible(capture.output(expect_error(summary(results7), NA)))
	    expect_output(summary(results7)$show())
	    results7CodeBased <- eval(parse(text = getObjectRCode(results7, stringWrapParagraphWidth = NULL)))
	    expect_equal(results7CodeBased$piTreatments, results7$piTreatments, tolerance = 1e-06)
	    expect_equal(results7CodeBased$piControl, results7$piControl, tolerance = 1e-06)
	    expect_equal(results7CodeBased$conditionalRejectionProbabilities, results7$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results7CodeBased$conditionalPower, results7$conditionalPower, tolerance = 1e-06)
	    expect_equal(results7CodeBased$repeatedConfidenceIntervalLowerBounds, results7$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results7CodeBased$repeatedConfidenceIntervalUpperBounds, results7$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results7CodeBased$repeatedPValues, results7$repeatedPValues, tolerance = 1e-06)
	    expect_type(names(results7), "character")
	    df <- as.data.frame(results7)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results7)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results8 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Simes", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results8' with expected results
	expect_equal(results8$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results8$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results8$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results8$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results8$piControl[1, ], 0.49019608, tolerance = 1e-06, label = paste0("c(", paste0(results8$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results8$conditionalRejectionProbabilities[1, ], c(0.014541388, 0.0059378141, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results8$conditionalRejectionProbabilities[2, ], c(0.022055696, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results8$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.26319109, -0.20678373, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results8$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.14541584, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results8$repeatedConfidenceIntervalUpperBounds[1, ], c(0.45121457, 0.32319296, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results8$repeatedConfidenceIntervalUpperBounds[2, ], c(0.53261778, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results8$repeatedPValues[1, ], c(0.3372539, 0.3372539, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results8$repeatedPValues[2, ], c(0.20120204, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(results8$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.011, 0.018), tolerance = 1e-06, label = paste0("c(", paste0(results8$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
	expect_equal(results8$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results8$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results8), NA)))
	    expect_output(print(results8)$show())
	    invisible(capture.output(expect_error(summary(results8), NA)))
	    expect_output(summary(results8)$show())
	    results8CodeBased <- eval(parse(text = getObjectRCode(results8, stringWrapParagraphWidth = NULL)))
	    expect_equal(results8CodeBased$piTreatments, results8$piTreatments, tolerance = 1e-06)
	    expect_equal(results8CodeBased$piControl, results8$piControl, tolerance = 1e-06)
	    expect_equal(results8CodeBased$conditionalRejectionProbabilities, results8$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results8CodeBased$repeatedConfidenceIntervalLowerBounds, results8$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results8CodeBased$repeatedConfidenceIntervalUpperBounds, results8$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results8CodeBased$repeatedPValues, results8$repeatedPValues, tolerance = 1e-06)
	    expect_equal(results8CodeBased$conditionalPowerSimulated, results8$conditionalPowerSimulated, tolerance = 1e-06)
	    expect_type(names(results8), "character")
	    df <- as.data.frame(results8)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results8)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results9 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Simes", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results9' with expected results
	expect_equal(results9$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results9$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results9$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results9$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results9$piControl[1, ], 0.49019608, tolerance = 1e-06, label = paste0("c(", paste0(results9$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results9$conditionalRejectionProbabilities[1, ], c(0.011503611, 0, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results9$conditionalRejectionProbabilities[2, ], c(0.015301846, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results9$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.26319109, -0.20678373, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results9$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.14541584, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results9$repeatedConfidenceIntervalUpperBounds[1, ], c(0.45121457, 0.32319296, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results9$repeatedConfidenceIntervalUpperBounds[2, ], c(0.53261778, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results9$repeatedPValues[1, ], c(0.4416362, 0.4416362, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results9$repeatedPValues[2, ], c(0.31730879, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(results9$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0, 0), label = paste0("c(", paste0(results9$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
	expect_equal(results9$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results9$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results9), NA)))
	    expect_output(print(results9)$show())
	    invisible(capture.output(expect_error(summary(results9), NA)))
	    expect_output(summary(results9)$show())
	    results9CodeBased <- eval(parse(text = getObjectRCode(results9, stringWrapParagraphWidth = NULL)))
	    expect_equal(results9CodeBased$piTreatments, results9$piTreatments, tolerance = 1e-06)
	    expect_equal(results9CodeBased$piControl, results9$piControl, tolerance = 1e-06)
	    expect_equal(results9CodeBased$conditionalRejectionProbabilities, results9$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results9CodeBased$repeatedConfidenceIntervalLowerBounds, results9$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results9CodeBased$repeatedConfidenceIntervalUpperBounds, results9$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results9CodeBased$repeatedPValues, results9$repeatedPValues, tolerance = 1e-06)
	    expect_equal(results9CodeBased$conditionalPowerSimulated, results9$conditionalPowerSimulated, tolerance = 1e-06)
	    expect_type(names(results9), "character")
	    df <- as.data.frame(results9)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results9)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results10 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Sidak", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results10' with expected results
	expect_equal(results10$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results10$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results10$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results10$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results10$piControl[1, ], 0.49019608, tolerance = 1e-06, label = paste0("c(", paste0(results10$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results10$conditionalRejectionProbabilities[1, ], c(0.014541388, 0.0059378141, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results10$conditionalRejectionProbabilities[2, ], c(0.022495939, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results10$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.26309239, -0.20660876, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results10$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.14531421, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results10$repeatedConfidenceIntervalUpperBounds[1, ], c(0.45112942, 0.32249413, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results10$repeatedConfidenceIntervalUpperBounds[2, ], c(0.5325407, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results10$repeatedPValues[1, ], c(0.3372539, 0.3372539, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results10$repeatedPValues[2, ], c(0.19615616, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(results10$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.011, 0.018), tolerance = 1e-06, label = paste0("c(", paste0(results10$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
	expect_equal(results10$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results10$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results10), NA)))
	    expect_output(print(results10)$show())
	    invisible(capture.output(expect_error(summary(results10), NA)))
	    expect_output(summary(results10)$show())
	    results10CodeBased <- eval(parse(text = getObjectRCode(results10, stringWrapParagraphWidth = NULL)))
	    expect_equal(results10CodeBased$piTreatments, results10$piTreatments, tolerance = 1e-06)
	    expect_equal(results10CodeBased$piControl, results10$piControl, tolerance = 1e-06)
	    expect_equal(results10CodeBased$conditionalRejectionProbabilities, results10$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results10CodeBased$repeatedConfidenceIntervalLowerBounds, results10$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results10CodeBased$repeatedConfidenceIntervalUpperBounds, results10$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results10CodeBased$repeatedPValues, results10$repeatedPValues, tolerance = 1e-06)
	    expect_equal(results10CodeBased$conditionalPowerSimulated, results10$conditionalPowerSimulated, tolerance = 1e-06)
	    expect_type(names(results10), "character")
	    df <- as.data.frame(results10)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results10)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results11 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Sidak", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results11' with expected results
	expect_equal(results11$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results11$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results11$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results11$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results11$piControl[1, ], 0.49019608, tolerance = 1e-06, label = paste0("c(", paste0(results11$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results11$conditionalRejectionProbabilities[1, ], c(0.011503611, 0, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results11$conditionalRejectionProbabilities[2, ], c(0.015852794, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results11$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.26309239, -0.20660876, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results11$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.14531421, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results11$repeatedConfidenceIntervalUpperBounds[1, ], c(0.45112942, 0.32249413, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results11$repeatedConfidenceIntervalUpperBounds[2, ], c(0.5325407, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results11$repeatedPValues[1, ], c(0.4416362, 0.4416362, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results11$repeatedPValues[2, ], c(0.30403844, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(results11$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0, 0), label = paste0("c(", paste0(results11$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
	expect_equal(results11$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results11$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results11), NA)))
	    expect_output(print(results11)$show())
	    invisible(capture.output(expect_error(summary(results11), NA)))
	    expect_output(summary(results11)$show())
	    results11CodeBased <- eval(parse(text = getObjectRCode(results11, stringWrapParagraphWidth = NULL)))
	    expect_equal(results11CodeBased$piTreatments, results11$piTreatments, tolerance = 1e-06)
	    expect_equal(results11CodeBased$piControl, results11$piControl, tolerance = 1e-06)
	    expect_equal(results11CodeBased$conditionalRejectionProbabilities, results11$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results11CodeBased$repeatedConfidenceIntervalLowerBounds, results11$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results11CodeBased$repeatedConfidenceIntervalUpperBounds, results11$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results11CodeBased$repeatedPValues, results11$repeatedPValues, tolerance = 1e-06)
	    expect_equal(results11CodeBased$conditionalPowerSimulated, results11$conditionalPowerSimulated, tolerance = 1e-06)
	    expect_type(names(results11), "character")
	    df <- as.data.frame(results11)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results11)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results12 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Bonferroni", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results12' with expected results
	expect_equal(results12$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results12$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results12$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results12$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results12$piControl[1, ], 0.49019608, tolerance = 1e-06, label = paste0("c(", paste0(results12$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results12$conditionalRejectionProbabilities[1, ], c(0.014541388, 0.0059378141, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results12$conditionalRejectionProbabilities[2, ], c(0.022055696, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results12$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.26319109, -0.20678373, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results12$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.14541584, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results12$repeatedConfidenceIntervalUpperBounds[1, ], c(0.45121457, 0.32319296, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results12$repeatedConfidenceIntervalUpperBounds[2, ], c(0.53261778, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results12$repeatedPValues[1, ], c(0.3372539, 0.3372539, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results12$repeatedPValues[2, ], c(0.20120204, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(results12$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.011, 0.018), tolerance = 1e-06, label = paste0("c(", paste0(results12$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
	expect_equal(results12$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results12$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results12), NA)))
	    expect_output(print(results12)$show())
	    invisible(capture.output(expect_error(summary(results12), NA)))
	    expect_output(summary(results12)$show())
	    results12CodeBased <- eval(parse(text = getObjectRCode(results12, stringWrapParagraphWidth = NULL)))
	    expect_equal(results12CodeBased$piTreatments, results12$piTreatments, tolerance = 1e-06)
	    expect_equal(results12CodeBased$piControl, results12$piControl, tolerance = 1e-06)
	    expect_equal(results12CodeBased$conditionalRejectionProbabilities, results12$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results12CodeBased$repeatedConfidenceIntervalLowerBounds, results12$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results12CodeBased$repeatedConfidenceIntervalUpperBounds, results12$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results12CodeBased$repeatedPValues, results12$repeatedPValues, tolerance = 1e-06)
	    expect_equal(results12CodeBased$conditionalPowerSimulated, results12$conditionalPowerSimulated, tolerance = 1e-06)
	    expect_type(names(results12), "character")
	    df <- as.data.frame(results12)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results12)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results13 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Bonferroni", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results13' with expected results
	expect_equal(results13$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results13$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results13$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results13$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results13$piControl[1, ], 0.49019608, tolerance = 1e-06, label = paste0("c(", paste0(results13$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results13$conditionalRejectionProbabilities[1, ], c(0.011503611, 0, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results13$conditionalRejectionProbabilities[2, ], c(0.015301846, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results13$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.26319109, -0.20678373, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results13$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.14541584, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results13$repeatedConfidenceIntervalUpperBounds[1, ], c(0.45121457, 0.32319296, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results13$repeatedConfidenceIntervalUpperBounds[2, ], c(0.53261778, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results13$repeatedPValues[1, ], c(0.4416362, 0.4416362, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results13$repeatedPValues[2, ], c(0.31730879, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(results13$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0, 0), label = paste0("c(", paste0(results13$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
	expect_equal(results13$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results13$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results13), NA)))
	    expect_output(print(results13)$show())
	    invisible(capture.output(expect_error(summary(results13), NA)))
	    expect_output(summary(results13)$show())
	    results13CodeBased <- eval(parse(text = getObjectRCode(results13, stringWrapParagraphWidth = NULL)))
	    expect_equal(results13CodeBased$piTreatments, results13$piTreatments, tolerance = 1e-06)
	    expect_equal(results13CodeBased$piControl, results13$piControl, tolerance = 1e-06)
	    expect_equal(results13CodeBased$conditionalRejectionProbabilities, results13$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results13CodeBased$repeatedConfidenceIntervalLowerBounds, results13$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results13CodeBased$repeatedConfidenceIntervalUpperBounds, results13$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results13CodeBased$repeatedPValues, results13$repeatedPValues, tolerance = 1e-06)
	    expect_equal(results13CodeBased$conditionalPowerSimulated, results13$conditionalPowerSimulated, tolerance = 1e-06)
	    expect_type(names(results13), "character")
	    df <- as.data.frame(results13)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results13)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results14 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Dunnett", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results14' with expected results
	expect_equal(results14$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results14$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results14$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results14$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results14$piControl[1, ], 0.49019608, tolerance = 1e-06, label = paste0("c(", paste0(results14$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results14$conditionalRejectionProbabilities[1, ], c(0.014541388, 0.0059378141, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results14$conditionalRejectionProbabilities[2, ], c(0.02426897, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results14$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.26076224, -0.20472006, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results14$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.14291673, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results14$repeatedConfidenceIntervalUpperBounds[1, ], c(0.44911842, 0.31972464, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results14$repeatedConfidenceIntervalUpperBounds[2, ], c(0.53072024, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results14$repeatedPValues[1, ], c(0.3372539, 0.3372539, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results14$repeatedPValues[2, ], c(0.17782371, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(results14$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.011, 0.018), tolerance = 1e-06, label = paste0("c(", paste0(results14$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
	expect_equal(results14$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results14$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results14), NA)))
	    expect_output(print(results14)$show())
	    invisible(capture.output(expect_error(summary(results14), NA)))
	    expect_output(summary(results14)$show())
	    results14CodeBased <- eval(parse(text = getObjectRCode(results14, stringWrapParagraphWidth = NULL)))
	    expect_equal(results14CodeBased$piTreatments, results14$piTreatments, tolerance = 1e-06)
	    expect_equal(results14CodeBased$piControl, results14$piControl, tolerance = 1e-06)
	    expect_equal(results14CodeBased$conditionalRejectionProbabilities, results14$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results14CodeBased$repeatedConfidenceIntervalLowerBounds, results14$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results14CodeBased$repeatedConfidenceIntervalUpperBounds, results14$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results14CodeBased$repeatedPValues, results14$repeatedPValues, tolerance = 1e-06)
	    expect_equal(results14CodeBased$conditionalPowerSimulated, results14$conditionalPowerSimulated, tolerance = 1e-06)
	    expect_type(names(results14), "character")
	    df <- as.data.frame(results14)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results14)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results15 <- getAnalysisResults(design = design3, dataInput = dataExample1,
								intersectionTest = "Dunnett", normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsConditionalDunnett object 'results15' with expected results
	expect_equal(results15$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results15$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results15$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results15$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results15$piControl[1, ], 0.49019608, tolerance = 1e-06, label = paste0("c(", paste0(results15$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results15$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.019942093), tolerance = 1e-06, label = paste0("c(", paste0(results15$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results15$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.049974092), tolerance = 1e-06, label = paste0("c(", paste0(results15$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results15$conditionalPower[1, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results15$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(results15$conditionalPower[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results15$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(results15$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, -0.10423565), tolerance = 1e-06, label = paste0("c(", paste0(results15$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results15$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results15$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results15$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, 0.28064632), tolerance = 1e-06, label = paste0("c(", paste0(results15$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results15$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results15$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results15$repeatedPValues[1, ], c(NA_real_, 0.26025152), tolerance = 1e-06, label = paste0("c(", paste0(results15$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results15$repeatedPValues[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results15$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results15), NA)))
	    expect_output(print(results15)$show())
	    invisible(capture.output(expect_error(summary(results15), NA)))
	    expect_output(summary(results15)$show())
	    results15CodeBased <- eval(parse(text = getObjectRCode(results15, stringWrapParagraphWidth = NULL)))
	    expect_equal(results15CodeBased$piTreatments, results15$piTreatments, tolerance = 1e-06)
	    expect_equal(results15CodeBased$piControl, results15$piControl, tolerance = 1e-06)
	    expect_equal(results15CodeBased$conditionalRejectionProbabilities, results15$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results15CodeBased$conditionalPower, results15$conditionalPower, tolerance = 1e-06)
	    expect_equal(results15CodeBased$repeatedConfidenceIntervalLowerBounds, results15$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results15CodeBased$repeatedConfidenceIntervalUpperBounds, results15$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results15CodeBased$repeatedPValues, results15$repeatedPValues, tolerance = 1e-06)
	    expect_type(names(results15), "character")
	    df <- as.data.frame(results15)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results15)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results16 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Simes", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results16' with expected results
	expect_equal(results16$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results16$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results16$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results16$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results16$piControl[1, ], 0.90196078, tolerance = 1e-06, label = paste0("c(", paste0(results16$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results16$conditionalRejectionProbabilities[1, ], c(0.13434137, 0.80112393, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results16$conditionalRejectionProbabilities[2, ], c(0.086909033, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results16$conditionalPower[1, ], c(NA_real_, NA_real_, 0.99558173, 0.99935678), tolerance = 1e-06, label = paste0("c(", paste0(results16$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(results16$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results16$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(results16$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.62349618, -0.55900271, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results16$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.51524937, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results16$repeatedConfidenceIntervalUpperBounds[1, ], c(0.08041061, -0.10884679, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results16$repeatedConfidenceIntervalUpperBounds[2, ], c(0.16732342, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results16$repeatedPValues[1, ], c(0.10960848, 0.00033097065, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results16$repeatedPValues[2, ], c(0.30001108, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results16), NA)))
	    expect_output(print(results16)$show())
	    invisible(capture.output(expect_error(summary(results16), NA)))
	    expect_output(summary(results16)$show())
	    results16CodeBased <- eval(parse(text = getObjectRCode(results16, stringWrapParagraphWidth = NULL)))
	    expect_equal(results16CodeBased$piTreatments, results16$piTreatments, tolerance = 1e-06)
	    expect_equal(results16CodeBased$piControl, results16$piControl, tolerance = 1e-06)
	    expect_equal(results16CodeBased$conditionalRejectionProbabilities, results16$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results16CodeBased$conditionalPower, results16$conditionalPower, tolerance = 1e-06)
	    expect_equal(results16CodeBased$repeatedConfidenceIntervalLowerBounds, results16$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results16CodeBased$repeatedConfidenceIntervalUpperBounds, results16$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results16CodeBased$repeatedPValues, results16$repeatedPValues, tolerance = 1e-06)
	    expect_type(names(results16), "character")
	    df <- as.data.frame(results16)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results16)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results17 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Simes", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results17' with expected results
	expect_equal(results17$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results17$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results17$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results17$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results17$piControl[1, ], 0.90196078, tolerance = 1e-06, label = paste0("c(", paste0(results17$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results17$conditionalRejectionProbabilities[1, ], c(0.093864952, 0.64895151, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results17$conditionalRejectionProbabilities[2, ], c(0.055973245, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results17$conditionalPower[1, ], c(NA_real_, NA_real_, 0.9824342, 0.99709946), tolerance = 1e-06, label = paste0("c(", paste0(results17$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(results17$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results17$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(results17$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.62349618, -0.55900271, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results17$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.51524937, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results17$repeatedConfidenceIntervalUpperBounds[1, ], c(0.08041061, -0.10884679, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results17$repeatedConfidenceIntervalUpperBounds[2, ], c(0.16732342, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results17$repeatedPValues[1, ], c(0.25516368, 0.0016127064, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results17$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results17), NA)))
	    expect_output(print(results17)$show())
	    invisible(capture.output(expect_error(summary(results17), NA)))
	    expect_output(summary(results17)$show())
	    results17CodeBased <- eval(parse(text = getObjectRCode(results17, stringWrapParagraphWidth = NULL)))
	    expect_equal(results17CodeBased$piTreatments, results17$piTreatments, tolerance = 1e-06)
	    expect_equal(results17CodeBased$piControl, results17$piControl, tolerance = 1e-06)
	    expect_equal(results17CodeBased$conditionalRejectionProbabilities, results17$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results17CodeBased$conditionalPower, results17$conditionalPower, tolerance = 1e-06)
	    expect_equal(results17CodeBased$repeatedConfidenceIntervalLowerBounds, results17$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results17CodeBased$repeatedConfidenceIntervalUpperBounds, results17$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results17CodeBased$repeatedPValues, results17$repeatedPValues, tolerance = 1e-06)
	    expect_type(names(results17), "character")
	    df <- as.data.frame(results17)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results17)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results18 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Sidak", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results18' with expected results
	expect_equal(results18$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results18$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results18$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results18$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results18$piControl[1, ], 0.90196078, tolerance = 1e-06, label = paste0("c(", paste0(results18$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results18$conditionalRejectionProbabilities[1, ], c(0.13448752, 0.80132731, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results18$conditionalRejectionProbabilities[2, ], c(0.086909033, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results18$conditionalPower[1, ], c(NA_real_, NA_real_, 0.99559209, 0.9993584), tolerance = 1e-06, label = paste0("c(", paste0(results18$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(results18$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results18$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(results18$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.62348308, -0.55893143, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results18$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.51523461, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results18$repeatedConfidenceIntervalUpperBounds[1, ], c(0.080387741, -0.10944682, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results18$repeatedConfidenceIntervalUpperBounds[2, ], c(0.16730172, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results18$repeatedPValues[1, ], c(0.10930044, 0.00033001698, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results18$repeatedPValues[2, ], c(0.30001108, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results18), NA)))
	    expect_output(print(results18)$show())
	    invisible(capture.output(expect_error(summary(results18), NA)))
	    expect_output(summary(results18)$show())
	    results18CodeBased <- eval(parse(text = getObjectRCode(results18, stringWrapParagraphWidth = NULL)))
	    expect_equal(results18CodeBased$piTreatments, results18$piTreatments, tolerance = 1e-06)
	    expect_equal(results18CodeBased$piControl, results18$piControl, tolerance = 1e-06)
	    expect_equal(results18CodeBased$conditionalRejectionProbabilities, results18$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results18CodeBased$conditionalPower, results18$conditionalPower, tolerance = 1e-06)
	    expect_equal(results18CodeBased$repeatedConfidenceIntervalLowerBounds, results18$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results18CodeBased$repeatedConfidenceIntervalUpperBounds, results18$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results18CodeBased$repeatedPValues, results18$repeatedPValues, tolerance = 1e-06)
	    expect_type(names(results18), "character")
	    df <- as.data.frame(results18)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results18)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results19 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Sidak", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results19' with expected results
	expect_equal(results19$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results19$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results19$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results19$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results19$piControl[1, ], 0.90196078, tolerance = 1e-06, label = paste0("c(", paste0(results19$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results19$conditionalRejectionProbabilities[1, ], c(0.094152745, 0.64965648, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results19$conditionalRejectionProbabilities[2, ], c(0.055973245, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results19$conditionalPower[1, ], c(NA_real_, NA_real_, 0.98252556, 0.99711613), tolerance = 1e-06, label = paste0("c(", paste0(results19$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(results19$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results19$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(results19$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.62348308, -0.55893143, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results19$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.51523461, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results19$repeatedConfidenceIntervalUpperBounds[1, ], c(0.080387741, -0.10944682, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results19$repeatedConfidenceIntervalUpperBounds[2, ], c(0.16730172, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results19$repeatedPValues[1, ], c(0.25349285, 0.001602216, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results19$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results19), NA)))
	    expect_output(print(results19)$show())
	    invisible(capture.output(expect_error(summary(results19), NA)))
	    expect_output(summary(results19)$show())
	    results19CodeBased <- eval(parse(text = getObjectRCode(results19, stringWrapParagraphWidth = NULL)))
	    expect_equal(results19CodeBased$piTreatments, results19$piTreatments, tolerance = 1e-06)
	    expect_equal(results19CodeBased$piControl, results19$piControl, tolerance = 1e-06)
	    expect_equal(results19CodeBased$conditionalRejectionProbabilities, results19$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results19CodeBased$conditionalPower, results19$conditionalPower, tolerance = 1e-06)
	    expect_equal(results19CodeBased$repeatedConfidenceIntervalLowerBounds, results19$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results19CodeBased$repeatedConfidenceIntervalUpperBounds, results19$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results19CodeBased$repeatedPValues, results19$repeatedPValues, tolerance = 1e-06)
	    expect_type(names(results19), "character")
	    df <- as.data.frame(results19)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results19)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results20 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Bonferroni", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results20' with expected results
	expect_equal(results20$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results20$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results20$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results20$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results20$piControl[1, ], 0.90196078, tolerance = 1e-06, label = paste0("c(", paste0(results20$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results20$conditionalRejectionProbabilities[1, ], c(0.13434137, 0.80112393, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results20$conditionalRejectionProbabilities[2, ], c(0.086909033, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results20$conditionalPower[1, ], c(NA_real_, NA_real_, 0.99558173, 0.99935678), tolerance = 1e-06, label = paste0("c(", paste0(results20$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(results20$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results20$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(results20$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.62349618, -0.55900271, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results20$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.51524937, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results20$repeatedConfidenceIntervalUpperBounds[1, ], c(0.08041061, -0.10884679, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results20$repeatedConfidenceIntervalUpperBounds[2, ], c(0.16732342, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results20$repeatedPValues[1, ], c(0.10960848, 0.00033097065, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results20$repeatedPValues[2, ], c(0.30001108, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results20), NA)))
	    expect_output(print(results20)$show())
	    invisible(capture.output(expect_error(summary(results20), NA)))
	    expect_output(summary(results20)$show())
	    results20CodeBased <- eval(parse(text = getObjectRCode(results20, stringWrapParagraphWidth = NULL)))
	    expect_equal(results20CodeBased$piTreatments, results20$piTreatments, tolerance = 1e-06)
	    expect_equal(results20CodeBased$piControl, results20$piControl, tolerance = 1e-06)
	    expect_equal(results20CodeBased$conditionalRejectionProbabilities, results20$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results20CodeBased$conditionalPower, results20$conditionalPower, tolerance = 1e-06)
	    expect_equal(results20CodeBased$repeatedConfidenceIntervalLowerBounds, results20$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results20CodeBased$repeatedConfidenceIntervalUpperBounds, results20$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results20CodeBased$repeatedPValues, results20$repeatedPValues, tolerance = 1e-06)
	    expect_type(names(results20), "character")
	    df <- as.data.frame(results20)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results20)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results21 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Bonferroni", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results21' with expected results
	expect_equal(results21$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results21$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results21$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results21$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results21$piControl[1, ], 0.90196078, tolerance = 1e-06, label = paste0("c(", paste0(results21$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results21$conditionalRejectionProbabilities[1, ], c(0.093864952, 0.64895151, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results21$conditionalRejectionProbabilities[2, ], c(0.055973245, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results21$conditionalPower[1, ], c(NA_real_, NA_real_, 0.9824342, 0.99709946), tolerance = 1e-06, label = paste0("c(", paste0(results21$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(results21$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results21$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(results21$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.62349618, -0.55900271, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results21$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.51524937, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results21$repeatedConfidenceIntervalUpperBounds[1, ], c(0.08041061, -0.10884679, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results21$repeatedConfidenceIntervalUpperBounds[2, ], c(0.16732342, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results21$repeatedPValues[1, ], c(0.25516368, 0.0016127064, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results21$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results21), NA)))
	    expect_output(print(results21)$show())
	    invisible(capture.output(expect_error(summary(results21), NA)))
	    expect_output(summary(results21)$show())
	    results21CodeBased <- eval(parse(text = getObjectRCode(results21, stringWrapParagraphWidth = NULL)))
	    expect_equal(results21CodeBased$piTreatments, results21$piTreatments, tolerance = 1e-06)
	    expect_equal(results21CodeBased$piControl, results21$piControl, tolerance = 1e-06)
	    expect_equal(results21CodeBased$conditionalRejectionProbabilities, results21$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results21CodeBased$conditionalPower, results21$conditionalPower, tolerance = 1e-06)
	    expect_equal(results21CodeBased$repeatedConfidenceIntervalLowerBounds, results21$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results21CodeBased$repeatedConfidenceIntervalUpperBounds, results21$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results21CodeBased$repeatedPValues, results21$repeatedPValues, tolerance = 1e-06)
	    expect_type(names(results21), "character")
	    df <- as.data.frame(results21)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results21)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results22 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Dunnett", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results22' with expected results
	expect_equal(results22$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results22$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results22$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results22$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results22$piControl[1, ], 0.90196078, tolerance = 1e-06, label = paste0("c(", paste0(results22$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results22$conditionalRejectionProbabilities[1, ], c(0.13739709, 0.80531545, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results22$conditionalRejectionProbabilities[2, ], c(0.086909033, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results22$conditionalPower[1, ], c(NA_real_, NA_real_, 0.9957922, 0.99938978), tolerance = 1e-06, label = paste0("c(", paste0(results22$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(results22$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results22$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(results22$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.62267641, -0.55784922, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results22$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.51432155, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results22$repeatedConfidenceIntervalUpperBounds[1, ], c(0.079005391, -0.11253619, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results22$repeatedConfidenceIntervalUpperBounds[2, ], c(0.16597553, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results22$repeatedPValues[1, ], c(0.10336955, 0.00031285088, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results22$repeatedPValues[2, ], c(0.30001108, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results22), NA)))
	    expect_output(print(results22)$show())
	    invisible(capture.output(expect_error(summary(results22), NA)))
	    expect_output(summary(results22)$show())
	    results22CodeBased <- eval(parse(text = getObjectRCode(results22, stringWrapParagraphWidth = NULL)))
	    expect_equal(results22CodeBased$piTreatments, results22$piTreatments, tolerance = 1e-06)
	    expect_equal(results22CodeBased$piControl, results22$piControl, tolerance = 1e-06)
	    expect_equal(results22CodeBased$conditionalRejectionProbabilities, results22$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results22CodeBased$conditionalPower, results22$conditionalPower, tolerance = 1e-06)
	    expect_equal(results22CodeBased$repeatedConfidenceIntervalLowerBounds, results22$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results22CodeBased$repeatedConfidenceIntervalUpperBounds, results22$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results22CodeBased$repeatedPValues, results22$repeatedPValues, tolerance = 1e-06)
	    expect_type(names(results22), "character")
	    df <- as.data.frame(results22)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results22)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results23 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Simes", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results23' with expected results
	expect_equal(results23$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results23$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results23$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results23$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results23$piControl[1, ], 0.90196078, tolerance = 1e-06, label = paste0("c(", paste0(results23$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results23$conditionalRejectionProbabilities[1, ], c(0.10173644, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results23$conditionalRejectionProbabilities[2, ], c(0.053203298, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results23$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.58125932, -0.55861966, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results23$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.46821261, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results23$repeatedConfidenceIntervalUpperBounds[1, ], c(0.011590857, -0.11157179, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results23$repeatedConfidenceIntervalUpperBounds[2, ], c(0.10089066, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results23$repeatedPValues[1, ], c(0.024755475, 0.00046257745, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results23$repeatedPValues[2, ], c(0.061679763, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(results23$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results23$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
	expect_equal(results23$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results23$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results23), NA)))
	    expect_output(print(results23)$show())
	    invisible(capture.output(expect_error(summary(results23), NA)))
	    expect_output(summary(results23)$show())
	    results23CodeBased <- eval(parse(text = getObjectRCode(results23, stringWrapParagraphWidth = NULL)))
	    expect_equal(results23CodeBased$piTreatments, results23$piTreatments, tolerance = 1e-06)
	    expect_equal(results23CodeBased$piControl, results23$piControl, tolerance = 1e-06)
	    expect_equal(results23CodeBased$conditionalRejectionProbabilities, results23$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results23CodeBased$repeatedConfidenceIntervalLowerBounds, results23$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results23CodeBased$repeatedConfidenceIntervalUpperBounds, results23$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results23CodeBased$repeatedPValues, results23$repeatedPValues, tolerance = 1e-06)
	    expect_equal(results23CodeBased$conditionalPowerSimulated, results23$conditionalPowerSimulated, tolerance = 1e-06)
	    expect_type(names(results23), "character")
	    df <- as.data.frame(results23)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results23)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results24 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Simes", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results24' with expected results
	expect_equal(results24$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results24$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results24$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results24$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results24$piControl[1, ], 0.90196078, tolerance = 1e-06, label = paste0("c(", paste0(results24$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results24$conditionalRejectionProbabilities[1, ], c(0.059039844, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results24$conditionalRejectionProbabilities[2, ], c(0.031516943, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results24$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.58125932, -0.55861966, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results24$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.46821261, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results24$repeatedConfidenceIntervalUpperBounds[1, ], c(0.011590857, -0.11157179, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results24$repeatedConfidenceIntervalUpperBounds[2, ], c(0.10089066, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results24$repeatedPValues[1, ], c(0.053362786, 0.0019131132, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results24$repeatedPValues[2, ], c(0.12598399, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(results24$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results24$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
	expect_equal(results24$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results24$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results24), NA)))
	    expect_output(print(results24)$show())
	    invisible(capture.output(expect_error(summary(results24), NA)))
	    expect_output(summary(results24)$show())
	    results24CodeBased <- eval(parse(text = getObjectRCode(results24, stringWrapParagraphWidth = NULL)))
	    expect_equal(results24CodeBased$piTreatments, results24$piTreatments, tolerance = 1e-06)
	    expect_equal(results24CodeBased$piControl, results24$piControl, tolerance = 1e-06)
	    expect_equal(results24CodeBased$conditionalRejectionProbabilities, results24$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results24CodeBased$repeatedConfidenceIntervalLowerBounds, results24$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results24CodeBased$repeatedConfidenceIntervalUpperBounds, results24$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results24CodeBased$repeatedPValues, results24$repeatedPValues, tolerance = 1e-06)
	    expect_equal(results24CodeBased$conditionalPowerSimulated, results24$conditionalPowerSimulated, tolerance = 1e-06)
	    expect_type(names(results24), "character")
	    df <- as.data.frame(results24)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results24)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results25 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Sidak", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results25' with expected results
	expect_equal(results25$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results25$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results25$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results25$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results25$piControl[1, ], 0.90196078, tolerance = 1e-06, label = paste0("c(", paste0(results25$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results25$conditionalRejectionProbabilities[1, ], c(0.10192145, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results25$conditionalRejectionProbabilities[2, ], c(0.053203298, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results25$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.58118715, -0.55853958, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results25$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.46813361, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results25$repeatedConfidenceIntervalUpperBounds[1, ], c(0.011479679, -0.11210069, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results25$repeatedConfidenceIntervalUpperBounds[2, ], c(0.1007828, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results25$repeatedPValues[1, ], c(0.024691579, 0.00046162377, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results25$repeatedPValues[2, ], c(0.061679763, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(results25$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results25$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
	expect_equal(results25$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results25$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results25), NA)))
	    expect_output(print(results25)$show())
	    invisible(capture.output(expect_error(summary(results25), NA)))
	    expect_output(summary(results25)$show())
	    results25CodeBased <- eval(parse(text = getObjectRCode(results25, stringWrapParagraphWidth = NULL)))
	    expect_equal(results25CodeBased$piTreatments, results25$piTreatments, tolerance = 1e-06)
	    expect_equal(results25CodeBased$piControl, results25$piControl, tolerance = 1e-06)
	    expect_equal(results25CodeBased$conditionalRejectionProbabilities, results25$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results25CodeBased$repeatedConfidenceIntervalLowerBounds, results25$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results25CodeBased$repeatedConfidenceIntervalUpperBounds, results25$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results25CodeBased$repeatedPValues, results25$repeatedPValues, tolerance = 1e-06)
	    expect_equal(results25CodeBased$conditionalPowerSimulated, results25$conditionalPowerSimulated, tolerance = 1e-06)
	    expect_type(names(results25), "character")
	    df <- as.data.frame(results25)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results25)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results26 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Sidak", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results26' with expected results
	expect_equal(results26$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results26$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results26$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results26$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results26$piControl[1, ], 0.90196078, tolerance = 1e-06, label = paste0("c(", paste0(results26$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results26$conditionalRejectionProbabilities[1, ], c(0.059289764, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results26$conditionalRejectionProbabilities[2, ], c(0.031516943, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results26$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.58118715, -0.55853958, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results26$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.46813361, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results26$repeatedConfidenceIntervalUpperBounds[1, ], c(0.011479679, -0.11210069, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results26$repeatedConfidenceIntervalUpperBounds[2, ], c(0.1007828, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results26$repeatedPValues[1, ], c(0.053049028, 0.0019045301, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results26$repeatedPValues[2, ], c(0.12598399, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(results26$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results26$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
	expect_equal(results26$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results26$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results26), NA)))
	    expect_output(print(results26)$show())
	    invisible(capture.output(expect_error(summary(results26), NA)))
	    expect_output(summary(results26)$show())
	    results26CodeBased <- eval(parse(text = getObjectRCode(results26, stringWrapParagraphWidth = NULL)))
	    expect_equal(results26CodeBased$piTreatments, results26$piTreatments, tolerance = 1e-06)
	    expect_equal(results26CodeBased$piControl, results26$piControl, tolerance = 1e-06)
	    expect_equal(results26CodeBased$conditionalRejectionProbabilities, results26$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results26CodeBased$repeatedConfidenceIntervalLowerBounds, results26$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results26CodeBased$repeatedConfidenceIntervalUpperBounds, results26$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results26CodeBased$repeatedPValues, results26$repeatedPValues, tolerance = 1e-06)
	    expect_equal(results26CodeBased$conditionalPowerSimulated, results26$conditionalPowerSimulated, tolerance = 1e-06)
	    expect_type(names(results26), "character")
	    df <- as.data.frame(results26)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results26)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results27 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Bonferroni", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results27' with expected results
	expect_equal(results27$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results27$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results27$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results27$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results27$piControl[1, ], 0.90196078, tolerance = 1e-06, label = paste0("c(", paste0(results27$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results27$conditionalRejectionProbabilities[1, ], c(0.10173644, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results27$conditionalRejectionProbabilities[2, ], c(0.053203298, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results27$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.58125932, -0.55861966, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results27$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.46821261, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results27$repeatedConfidenceIntervalUpperBounds[1, ], c(0.011590857, -0.11157179, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results27$repeatedConfidenceIntervalUpperBounds[2, ], c(0.10089066, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results27$repeatedPValues[1, ], c(0.024755475, 0.00046257745, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results27$repeatedPValues[2, ], c(0.061679763, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(results27$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results27$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
	expect_equal(results27$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results27$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results27), NA)))
	    expect_output(print(results27)$show())
	    invisible(capture.output(expect_error(summary(results27), NA)))
	    expect_output(summary(results27)$show())
	    results27CodeBased <- eval(parse(text = getObjectRCode(results27, stringWrapParagraphWidth = NULL)))
	    expect_equal(results27CodeBased$piTreatments, results27$piTreatments, tolerance = 1e-06)
	    expect_equal(results27CodeBased$piControl, results27$piControl, tolerance = 1e-06)
	    expect_equal(results27CodeBased$conditionalRejectionProbabilities, results27$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results27CodeBased$repeatedConfidenceIntervalLowerBounds, results27$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results27CodeBased$repeatedConfidenceIntervalUpperBounds, results27$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results27CodeBased$repeatedPValues, results27$repeatedPValues, tolerance = 1e-06)
	    expect_equal(results27CodeBased$conditionalPowerSimulated, results27$conditionalPowerSimulated, tolerance = 1e-06)
	    expect_type(names(results27), "character")
	    df <- as.data.frame(results27)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results27)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results28 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Bonferroni", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results28' with expected results
	expect_equal(results28$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results28$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results28$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results28$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results28$piControl[1, ], 0.90196078, tolerance = 1e-06, label = paste0("c(", paste0(results28$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results28$conditionalRejectionProbabilities[1, ], c(0.059039844, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results28$conditionalRejectionProbabilities[2, ], c(0.031516943, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results28$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.58125932, -0.55861966, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results28$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.46821261, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results28$repeatedConfidenceIntervalUpperBounds[1, ], c(0.011590857, -0.11157179, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results28$repeatedConfidenceIntervalUpperBounds[2, ], c(0.10089066, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results28$repeatedPValues[1, ], c(0.053362786, 0.0019131132, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results28$repeatedPValues[2, ], c(0.12598399, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(results28$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results28$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
	expect_equal(results28$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results28$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results28), NA)))
	    expect_output(print(results28)$show())
	    invisible(capture.output(expect_error(summary(results28), NA)))
	    expect_output(summary(results28)$show())
	    results28CodeBased <- eval(parse(text = getObjectRCode(results28, stringWrapParagraphWidth = NULL)))
	    expect_equal(results28CodeBased$piTreatments, results28$piTreatments, tolerance = 1e-06)
	    expect_equal(results28CodeBased$piControl, results28$piControl, tolerance = 1e-06)
	    expect_equal(results28CodeBased$conditionalRejectionProbabilities, results28$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results28CodeBased$repeatedConfidenceIntervalLowerBounds, results28$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results28CodeBased$repeatedConfidenceIntervalUpperBounds, results28$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results28CodeBased$repeatedPValues, results28$repeatedPValues, tolerance = 1e-06)
	    expect_equal(results28CodeBased$conditionalPowerSimulated, results28$conditionalPowerSimulated, tolerance = 1e-06)
	    expect_type(names(results28), "character")
	    df <- as.data.frame(results28)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results28)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results29 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Dunnett", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results29' with expected results
	expect_equal(results29$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results29$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results29$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results29$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results29$piControl[1, ], 0.90196078, tolerance = 1e-06, label = paste0("c(", paste0(results29$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results29$conditionalRejectionProbabilities[1, ], c(0.10565679, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results29$conditionalRejectionProbabilities[2, ], c(0.053203298, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results29$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.57948542, -0.55733023, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results29$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.46626976, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results29$repeatedConfidenceIntervalUpperBounds[1, ], c(0.0088604379, -0.11474633, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results29$repeatedConfidenceIntervalUpperBounds[2, ], c(0.0982386, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results29$repeatedPValues[1, ], c(0.023455619, 0.000443504, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results29$repeatedPValues[2, ], c(0.061679763, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$repeatedPValues[2, ], collapse = ", "), ")"))
	expect_equal(results29$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results29$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
	expect_equal(results29$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results29$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results29), NA)))
	    expect_output(print(results29)$show())
	    invisible(capture.output(expect_error(summary(results29), NA)))
	    expect_output(summary(results29)$show())
	    results29CodeBased <- eval(parse(text = getObjectRCode(results29, stringWrapParagraphWidth = NULL)))
	    expect_equal(results29CodeBased$piTreatments, results29$piTreatments, tolerance = 1e-06)
	    expect_equal(results29CodeBased$piControl, results29$piControl, tolerance = 1e-06)
	    expect_equal(results29CodeBased$conditionalRejectionProbabilities, results29$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results29CodeBased$repeatedConfidenceIntervalLowerBounds, results29$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results29CodeBased$repeatedConfidenceIntervalUpperBounds, results29$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results29CodeBased$repeatedPValues, results29$repeatedPValues, tolerance = 1e-06)
	    expect_equal(results29CodeBased$conditionalPowerSimulated, results29$conditionalPowerSimulated, tolerance = 1e-06)
	    expect_type(names(results29), "character")
	    df <- as.data.frame(results29)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results29)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results30 <- getAnalysisResults(design = design3, dataInput = dataExample2,
								intersectionTest = "Dunnett", normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsConditionalDunnett object 'results30' with expected results
	expect_equal(results30$piTreatments[1, ], 0.5625, tolerance = 1e-06, label = paste0("c(", paste0(results30$piTreatments[1, ], collapse = ", "), ")"))
	expect_equal(results30$piTreatments[2, ], NA_real_, label = paste0("c(", paste0(results30$piTreatments[2, ], collapse = ", "), ")"))
	expect_equal(results30$piControl[1, ], 0.90196078, tolerance = 1e-06, label = paste0("c(", paste0(results30$piControl[1, ], collapse = ", "), ")"))
	expect_equal(results30$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.21935767), tolerance = 1e-06, label = paste0("c(", paste0(results30$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
	expect_equal(results30$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.13026808), tolerance = 1e-06, label = paste0("c(", paste0(results30$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
	expect_equal(results30$conditionalPower[1, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results30$conditionalPower[1, ], collapse = ", "), ")"))
	expect_equal(results30$conditionalPower[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results30$conditionalPower[2, ], collapse = ", "), ")"))
	expect_equal(results30$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, -0.46994305), tolerance = 1e-06, label = paste0("c(", paste0(results30$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
	expect_equal(results30$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results30$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
	expect_equal(results30$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, -0.15490055), tolerance = 1e-06, label = paste0("c(", paste0(results30$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
	expect_equal(results30$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results30$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
	expect_equal(results30$repeatedPValues[1, ], c(NA_real_, 7.2525431e-05), tolerance = 1e-06, label = paste0("c(", paste0(results30$repeatedPValues[1, ], collapse = ", "), ")"))
	expect_equal(results30$repeatedPValues[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results30$repeatedPValues[2, ], collapse = ", "), ")"))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results30), NA)))
	    expect_output(print(results30)$show())
	    invisible(capture.output(expect_error(summary(results30), NA)))
	    expect_output(summary(results30)$show())
	    results30CodeBased <- eval(parse(text = getObjectRCode(results30, stringWrapParagraphWidth = NULL)))
	    expect_equal(results30CodeBased$piTreatments, results30$piTreatments, tolerance = 1e-06)
	    expect_equal(results30CodeBased$piControl, results30$piControl, tolerance = 1e-06)
	    expect_equal(results30CodeBased$conditionalRejectionProbabilities, results30$conditionalRejectionProbabilities, tolerance = 1e-06)
	    expect_equal(results30CodeBased$conditionalPower, results30$conditionalPower, tolerance = 1e-06)
	    expect_equal(results30CodeBased$repeatedConfidenceIntervalLowerBounds, results30$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
	    expect_equal(results30CodeBased$repeatedConfidenceIntervalUpperBounds, results30$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
	    expect_equal(results30CodeBased$repeatedPValues, results30$repeatedPValues, tolerance = 1e-06)
	    expect_type(names(results30), "character")
	    df <- as.data.frame(results30)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results30)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})
