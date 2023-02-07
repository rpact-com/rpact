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
## |  Creation date: 06 February 2023, 12:10:38
## |  File version: $Revision: 6801 $
## |  Last changed: $Date: 2023-02-06 15:29:57 +0100 (Mon, 06 Feb 2023) $
## |  Last changed by: $Author: pahlke $
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
								intersectionTest = "Simes", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results1' with expected results
	expect_equal(results1$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results1$piTreatments[2, ], NA_real_)
	expect_equal(results1$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results1$conditionalRejectionProbabilities[1, ], c(0.015420568, 0.003193865, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$conditionalRejectionProbabilities[2, ], c(0.024462749, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$conditionalPower[1, ], c(NA_real_, NA_real_, 0.0010766875, 0.011284717), tolerance = 1e-05)
	expect_equal(results1$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results1$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.32184855, -0.20584893, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.20645613, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalUpperBounds[1, ], c(0.5011587, 0.32866179, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalUpperBounds[2, ], c(0.57764375, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedPValues[1, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results1), NA)))
	    expect_output(print(results1)$show())
	    invisible(capture.output(expect_error(summary(results1), NA)))
	    expect_output(summary(results1)$show())
	    results1CodeBased <- eval(parse(text = getObjectRCode(results1, stringWrapParagraphWidth = NULL)))
	    expect_equal(results1CodeBased$piTreatments, results1$piTreatments, tolerance = 1e-05)
	    expect_equal(results1CodeBased$piControl, results1$piControl, tolerance = 1e-05)
	    expect_equal(results1CodeBased$conditionalRejectionProbabilities, results1$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(results1CodeBased$conditionalPower, results1$conditionalPower, tolerance = 1e-05)
	    expect_equal(results1CodeBased$repeatedConfidenceIntervalLowerBounds, results1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(results1CodeBased$repeatedConfidenceIntervalUpperBounds, results1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(results1CodeBased$repeatedPValues, results1$repeatedPValues, tolerance = 1e-05)
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
								intersectionTest = "Dunnett", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results2' with expected results
	expect_equal(results2$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results2$piTreatments[2, ], NA_real_)
	expect_equal(results2$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results2$conditionalRejectionProbabilities[1, ], c(0.022712676, 0.0087985227, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$conditionalRejectionProbabilities[2, ], c(0.043097831, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$conditionalPower[1, ], c(NA_real_, NA_real_, 0.004624756, 0.026737358), tolerance = 1e-05)
	expect_equal(results2$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results2$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.3206942, -0.20381953, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.2052416, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalUpperBounds[1, ], c(0.50018786, 0.32441792, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalUpperBounds[2, ], c(0.57677219, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedPValues[1, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results2), NA)))
	    expect_output(print(results2)$show())
	    invisible(capture.output(expect_error(summary(results2), NA)))
	    expect_output(summary(results2)$show())
	    results2CodeBased <- eval(parse(text = getObjectRCode(results2, stringWrapParagraphWidth = NULL)))
	    expect_equal(results2CodeBased$piTreatments, results2$piTreatments, tolerance = 1e-05)
	    expect_equal(results2CodeBased$piControl, results2$piControl, tolerance = 1e-05)
	    expect_equal(results2CodeBased$conditionalRejectionProbabilities, results2$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(results2CodeBased$conditionalPower, results2$conditionalPower, tolerance = 1e-05)
	    expect_equal(results2CodeBased$repeatedConfidenceIntervalLowerBounds, results2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(results2CodeBased$repeatedConfidenceIntervalUpperBounds, results2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(results2CodeBased$repeatedPValues, results2$repeatedPValues, tolerance = 1e-05)
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
	results3 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Simes", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results3' with expected results
	expect_equal(results3$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results3$piTreatments[2, ], NA_real_)
	expect_equal(results3$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results3$conditionalRejectionProbabilities[1, ], c(0.011503611, 0, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$conditionalRejectionProbabilities[2, ], c(0.015301846, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.26319109, -0.20678373, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.14541584, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalUpperBounds[1, ], c(0.45121457, 0.32319296, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalUpperBounds[2, ], c(0.53261778, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedPValues[1, ], c(0.4416362, 0.4416362, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedPValues[2, ], c(0.31730879, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0, 0))
	expect_equal(results3$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results3), NA)))
	    expect_output(print(results3)$show())
	    invisible(capture.output(expect_error(summary(results3), NA)))
	    expect_output(summary(results3)$show())
	    results3CodeBased <- eval(parse(text = getObjectRCode(results3, stringWrapParagraphWidth = NULL)))
	    expect_equal(results3CodeBased$piTreatments, results3$piTreatments, tolerance = 1e-05)
	    expect_equal(results3CodeBased$piControl, results3$piControl, tolerance = 1e-05)
	    expect_equal(results3CodeBased$conditionalRejectionProbabilities, results3$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(results3CodeBased$repeatedConfidenceIntervalLowerBounds, results3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(results3CodeBased$repeatedConfidenceIntervalUpperBounds, results3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(results3CodeBased$repeatedPValues, results3$repeatedPValues, tolerance = 1e-05)
	    expect_equal(results3CodeBased$conditionalPowerSimulated, results3$conditionalPowerSimulated, tolerance = 1e-05)
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
	results4 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Dunnett", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results4' with expected results
	expect_equal(results4$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results4$piTreatments[2, ], NA_real_)
	expect_equal(results4$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results4$conditionalRejectionProbabilities[1, ], c(0.014541388, 0.0059378141, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$conditionalRejectionProbabilities[2, ], c(0.024268969, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.26076213, -0.20472006, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.14291708, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalUpperBounds[1, ], c(0.44911894, 0.31972469, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalUpperBounds[2, ], c(0.53072029, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedPValues[1, ], c(0.3372539, 0.3372539, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedPValues[2, ], c(0.17782371, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.011, 0.018), tolerance = 1e-05)
	expect_equal(results4$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results4), NA)))
	    expect_output(print(results4)$show())
	    invisible(capture.output(expect_error(summary(results4), NA)))
	    expect_output(summary(results4)$show())
	    results4CodeBased <- eval(parse(text = getObjectRCode(results4, stringWrapParagraphWidth = NULL)))
	    expect_equal(results4CodeBased$piTreatments, results4$piTreatments, tolerance = 1e-05)
	    expect_equal(results4CodeBased$piControl, results4$piControl, tolerance = 1e-05)
	    expect_equal(results4CodeBased$conditionalRejectionProbabilities, results4$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(results4CodeBased$repeatedConfidenceIntervalLowerBounds, results4$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(results4CodeBased$repeatedConfidenceIntervalUpperBounds, results4$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(results4CodeBased$repeatedPValues, results4$repeatedPValues, tolerance = 1e-05)
	    expect_equal(results4CodeBased$conditionalPowerSimulated, results4$conditionalPowerSimulated, tolerance = 1e-05)
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
	results5 <- getAnalysisResults(design = design3, dataInput = dataExample1,
								intersectionTest = "Dunnett", normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsConditionalDunnett object 'results5' with expected results
	expect_equal(results5$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results5$piTreatments[2, ], NA_real_)
	expect_equal(results5$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results5$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.019942093), tolerance = 1e-05)
	expect_equal(results5$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.049973806), tolerance = 1e-05)
	expect_equal(results5$conditionalPower[1, ], c(NA_real_, NA_real_))
	expect_equal(results5$conditionalPower[2, ], c(NA_real_, NA_real_))
	expect_equal(results5$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, -0.10423565), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results5$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, 0.28064632), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results5$repeatedPValues[1, ], c(NA_real_, 0.26025152), tolerance = 1e-05)
	expect_equal(results5$repeatedPValues[2, ], c(NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results5), NA)))
	    expect_output(print(results5)$show())
	    invisible(capture.output(expect_error(summary(results5), NA)))
	    expect_output(summary(results5)$show())
	    results5CodeBased <- eval(parse(text = getObjectRCode(results5, stringWrapParagraphWidth = NULL)))
	    expect_equal(results5CodeBased$piTreatments, results5$piTreatments, tolerance = 1e-05)
	    expect_equal(results5CodeBased$piControl, results5$piControl, tolerance = 1e-05)
	    expect_equal(results5CodeBased$conditionalRejectionProbabilities, results5$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(results5CodeBased$conditionalPower, results5$conditionalPower, tolerance = 1e-05)
	    expect_equal(results5CodeBased$repeatedConfidenceIntervalLowerBounds, results5$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(results5CodeBased$repeatedConfidenceIntervalUpperBounds, results5$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(results5CodeBased$repeatedPValues, results5$repeatedPValues, tolerance = 1e-05)
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
	results6 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Simes", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results6' with expected results
	expect_equal(results6$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results6$piTreatments[2, ], NA_real_)
	expect_equal(results6$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results6$conditionalRejectionProbabilities[1, ], c(0.13434137, 0.80112393, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$conditionalRejectionProbabilities[2, ], c(0.086909033, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$conditionalPower[1, ], c(NA_real_, NA_real_, 0.99558173, 0.99935678), tolerance = 1e-05)
	expect_equal(results6$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results6$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.62349618, -0.55900271, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.51524937, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalUpperBounds[1, ], c(0.08041061, -0.10884679, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalUpperBounds[2, ], c(0.16732342, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedPValues[1, ], c(0.10960848, 0.00033097065, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedPValues[2, ], c(0.30001108, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results6), NA)))
	    expect_output(print(results6)$show())
	    invisible(capture.output(expect_error(summary(results6), NA)))
	    expect_output(summary(results6)$show())
	    results6CodeBased <- eval(parse(text = getObjectRCode(results6, stringWrapParagraphWidth = NULL)))
	    expect_equal(results6CodeBased$piTreatments, results6$piTreatments, tolerance = 1e-05)
	    expect_equal(results6CodeBased$piControl, results6$piControl, tolerance = 1e-05)
	    expect_equal(results6CodeBased$conditionalRejectionProbabilities, results6$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(results6CodeBased$conditionalPower, results6$conditionalPower, tolerance = 1e-05)
	    expect_equal(results6CodeBased$repeatedConfidenceIntervalLowerBounds, results6$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(results6CodeBased$repeatedConfidenceIntervalUpperBounds, results6$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(results6CodeBased$repeatedPValues, results6$repeatedPValues, tolerance = 1e-05)
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
	results7 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Dunnett", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results7' with expected results
	expect_equal(results7$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results7$piTreatments[2, ], NA_real_)
	expect_equal(results7$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results7$conditionalRejectionProbabilities[1, ], c(0.13739667, 0.80531488, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$conditionalRejectionProbabilities[2, ], c(0.086909033, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$conditionalPower[1, ], c(NA_real_, NA_real_, 0.99579217, 0.99938978), tolerance = 1e-05)
	expect_equal(results7$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results7$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.62267686, -0.55784951, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.5143226, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalUpperBounds[1, ], c(0.079007072, -0.11253618, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalUpperBounds[2, ], c(0.16597626, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedPValues[1, ], c(0.10337051, 0.00031285088, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedPValues[2, ], c(0.30001108, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results7), NA)))
	    expect_output(print(results7)$show())
	    invisible(capture.output(expect_error(summary(results7), NA)))
	    expect_output(summary(results7)$show())
	    results7CodeBased <- eval(parse(text = getObjectRCode(results7, stringWrapParagraphWidth = NULL)))
	    expect_equal(results7CodeBased$piTreatments, results7$piTreatments, tolerance = 1e-05)
	    expect_equal(results7CodeBased$piControl, results7$piControl, tolerance = 1e-05)
	    expect_equal(results7CodeBased$conditionalRejectionProbabilities, results7$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(results7CodeBased$conditionalPower, results7$conditionalPower, tolerance = 1e-05)
	    expect_equal(results7CodeBased$repeatedConfidenceIntervalLowerBounds, results7$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(results7CodeBased$repeatedConfidenceIntervalUpperBounds, results7$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(results7CodeBased$repeatedPValues, results7$repeatedPValues, tolerance = 1e-05)
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
	results8 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Simes", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results8' with expected results
	expect_equal(results8$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results8$piTreatments[2, ], NA_real_)
	expect_equal(results8$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results8$conditionalRejectionProbabilities[1, ], c(0.10173644, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$conditionalRejectionProbabilities[2, ], c(0.053203298, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.58125932, -0.55861966, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.46821261, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalUpperBounds[1, ], c(0.011590857, -0.11157179, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalUpperBounds[2, ], c(0.10089066, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedPValues[1, ], c(0.024755475, 0.00046257745, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedPValues[2, ], c(0.061679763, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1))
	expect_equal(results8$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results8), NA)))
	    expect_output(print(results8)$show())
	    invisible(capture.output(expect_error(summary(results8), NA)))
	    expect_output(summary(results8)$show())
	    results8CodeBased <- eval(parse(text = getObjectRCode(results8, stringWrapParagraphWidth = NULL)))
	    expect_equal(results8CodeBased$piTreatments, results8$piTreatments, tolerance = 1e-05)
	    expect_equal(results8CodeBased$piControl, results8$piControl, tolerance = 1e-05)
	    expect_equal(results8CodeBased$conditionalRejectionProbabilities, results8$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(results8CodeBased$repeatedConfidenceIntervalLowerBounds, results8$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(results8CodeBased$repeatedConfidenceIntervalUpperBounds, results8$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(results8CodeBased$repeatedPValues, results8$repeatedPValues, tolerance = 1e-05)
	    expect_equal(results8CodeBased$conditionalPowerSimulated, results8$conditionalPowerSimulated, tolerance = 1e-05)
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
	results9 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Dunnett", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results9' with expected results
	expect_equal(results9$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results9$piTreatments[2, ], NA_real_)
	expect_equal(results9$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results9$conditionalRejectionProbabilities[1, ], c(0.10565624, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$conditionalRejectionProbabilities[2, ], c(0.053203298, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.57948552, -0.55733034, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.4662704, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalUpperBounds[1, ], c(0.0088609184, -0.11474637, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalUpperBounds[2, ], c(0.098238963, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedPValues[1, ], c(0.023456573, 0.000443504, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedPValues[2, ], c(0.061679763, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1))
	expect_equal(results9$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results9), NA)))
	    expect_output(print(results9)$show())
	    invisible(capture.output(expect_error(summary(results9), NA)))
	    expect_output(summary(results9)$show())
	    results9CodeBased <- eval(parse(text = getObjectRCode(results9, stringWrapParagraphWidth = NULL)))
	    expect_equal(results9CodeBased$piTreatments, results9$piTreatments, tolerance = 1e-05)
	    expect_equal(results9CodeBased$piControl, results9$piControl, tolerance = 1e-05)
	    expect_equal(results9CodeBased$conditionalRejectionProbabilities, results9$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(results9CodeBased$repeatedConfidenceIntervalLowerBounds, results9$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(results9CodeBased$repeatedConfidenceIntervalUpperBounds, results9$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(results9CodeBased$repeatedPValues, results9$repeatedPValues, tolerance = 1e-05)
	    expect_equal(results9CodeBased$conditionalPowerSimulated, results9$conditionalPowerSimulated, tolerance = 1e-05)
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
	results10 <- getAnalysisResults(design = design3, dataInput = dataExample2,
								intersectionTest = "Dunnett", normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsConditionalDunnett object 'results10' with expected results
	expect_equal(results10$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results10$piTreatments[2, ], NA_real_)
	expect_equal(results10$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results10$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.21935683), tolerance = 1e-05)
	expect_equal(results10$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.13026808), tolerance = 1e-05)
	expect_equal(results10$conditionalPower[1, ], c(NA_real_, NA_real_))
	expect_equal(results10$conditionalPower[2, ], c(NA_real_, NA_real_))
	expect_equal(results10$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, -0.46994305), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results10$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, -0.15490055), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results10$repeatedPValues[1, ], c(NA_real_, 7.2525431e-05), tolerance = 1e-05)
	expect_equal(results10$repeatedPValues[2, ], c(NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results10), NA)))
	    expect_output(print(results10)$show())
	    invisible(capture.output(expect_error(summary(results10), NA)))
	    expect_output(summary(results10)$show())
	    results10CodeBased <- eval(parse(text = getObjectRCode(results10, stringWrapParagraphWidth = NULL)))
	    expect_equal(results10CodeBased$piTreatments, results10$piTreatments, tolerance = 1e-05)
	    expect_equal(results10CodeBased$piControl, results10$piControl, tolerance = 1e-05)
	    expect_equal(results10CodeBased$conditionalRejectionProbabilities, results10$conditionalRejectionProbabilities, tolerance = 1e-05)
	    expect_equal(results10CodeBased$conditionalPower, results10$conditionalPower, tolerance = 1e-05)
	    expect_equal(results10CodeBased$repeatedConfidenceIntervalLowerBounds, results10$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
	    expect_equal(results10CodeBased$repeatedConfidenceIntervalUpperBounds, results10$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
	    expect_equal(results10CodeBased$repeatedPValues, results10$repeatedPValues, tolerance = 1e-05)
	    expect_type(names(results10), "character")
	    df <- as.data.frame(results10)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(results10)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})
