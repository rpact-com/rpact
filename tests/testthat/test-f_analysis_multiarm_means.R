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
## |  File name: test-f_analysis_multiarm_means.R
## |  Creation date: 08 November 2023, 08:56:03
## |  File version: $Revision: 8260 $
## |  Last changed: $Date: 2024-09-25 10:42:43 +0200 (Mi, 25 Sep 2024) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing the Analysis Means Functionality for Three or More Treatments")

test_that("'getAnalysisResultsMultiArm' with dataset of means", {
        
    .skipTestIfDisabled()
        
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
        n1 = c(13, 25),
        n2 = c(15, NA),
        n3 = c(14, 27),
        n4 = c(12, 29),
        means1 = c(24.2, 22.2),
        means2 = c(18.8, NA),
        means3 = c(26.7, 27.7),
        means4 = c(9.2, 12.2),
        stDevs1 = c(24.4, 22.1),
        stDevs2 = c(21.2, NA),
        stDevs3 = c(25.6, 23.2),
        stDevs4 = c(21.5, 22.7)
    )

    # directionUpper = FALSE
    dataExample2 <- getDataset(
        n1 = c(13, 25),
        n2 = c(15, NA),
        n3 = c(14, 27),
        n4 = c(12, 29),
        means1 = -c(24.2, 22.2),
        means2 = -c(18.8, NA),
        means3 = -c(26.7, 27.7),
        means4 = -c(9.2, 12.2),
        stDevs1 = c(24.4, 22.1),
        stDevs2 = c(21.2, NA),
        stDevs3 = c(25.6, 23.2),
        stDevs4 = c(21.5, 22.7)
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
    results1 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Bonferroni", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results1' with expected results
    expect_equal(results1$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results1$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results1$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results1$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results1$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results1$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results1$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results1$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results1$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results1$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results1$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results1$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results1$conditionalRejectionProbabilities[1, ], c(0.043739576, 0.16022367, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results1$conditionalRejectionProbabilities[2, ], c(0.03844608, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results1$conditionalRejectionProbabilities[3, ], c(0.048616927, 0.34001465, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results1$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45838936, 0.70196346), tolerance = 1e-06, label = paste0("c(", paste0(results1$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results1$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results1$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results1$conditionalPower[3, ], c(NA_real_, NA_real_, 0.827255, 0.9465652), tolerance = 1e-06, label = paste0("c(", paste0(results1$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results1$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.395028, -4.0669228, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results1$repeatedConfidenceIntervalLowerBounds[2, ], c(-16.891548, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results1$repeatedConfidenceIntervalLowerBounds[3, ], c(-12.071338, 0.24153969, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results1$repeatedConfidenceIntervalUpperBounds[1, ], c(44.395028, 27.895908, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results1$repeatedConfidenceIntervalUpperBounds[2, ], c(36.091548, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results1$repeatedConfidenceIntervalUpperBounds[3, ], c(47.071339, 32.285152, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results1$repeatedPValues[1, ], c(0.5, 0.072888275, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results1$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results1$repeatedPValues[3, ], c(0.5, 0.017155659, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results1), NA)))
        expect_output(print(results1)$show())
        invisible(capture.output(expect_error(summary(results1), NA)))
        expect_output(summary(results1)$show())
        results1CodeBased <- eval(parse(text = getObjectRCode(results1, stringWrapParagraphWidth = NULL)))
        expect_equal(results1CodeBased$thetaH1, results1$thetaH1, tolerance = 1e-06)
        expect_equal(results1CodeBased$assumedStDevs, results1$assumedStDevs, tolerance = 1e-06)
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
    results2 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Bonferroni", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results2' with expected results
    expect_equal(results2$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results2$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results2$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results2$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results2$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results2$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results2$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results2$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results2$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results2$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results2$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results2$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results2$conditionalRejectionProbabilities[1, ], c(0.040100206, 0.14400686, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results2$conditionalRejectionProbabilities[2, ], c(0.037171319, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results2$conditionalRejectionProbabilities[3, ], c(0.042866371, 0.28890175, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results2$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42767622, 0.67947129), tolerance = 1e-06, label = paste0("c(", paste0(results2$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results2$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results2$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results2$conditionalPower[3, ], c(NA_real_, NA_real_, 0.78417464, 0.93070164), tolerance = 1e-06, label = paste0("c(", paste0(results2$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results2$repeatedConfidenceIntervalLowerBounds[1, ], c(-18.433726, -4.7641755, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results2$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.426743, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results2$repeatedConfidenceIntervalLowerBounds[3, ], c(-15.938808, -0.40329832, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results2$repeatedConfidenceIntervalUpperBounds[1, ], c(48.433726, 28.584393, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results2$repeatedConfidenceIntervalUpperBounds[2, ], c(39.626743, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results2$repeatedConfidenceIntervalUpperBounds[3, ], c(50.938808, 32.927366, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results2$repeatedPValues[1, ], c(0.5, 0.085188742, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results2$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results2$repeatedPValues[3, ], c(0.5, 0.025112148, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results2), NA)))
        expect_output(print(results2)$show())
        invisible(capture.output(expect_error(summary(results2), NA)))
        expect_output(summary(results2)$show())
        results2CodeBased <- eval(parse(text = getObjectRCode(results2, stringWrapParagraphWidth = NULL)))
        expect_equal(results2CodeBased$thetaH1, results2$thetaH1, tolerance = 1e-06)
        expect_equal(results2CodeBased$assumedStDevs, results2$assumedStDevs, tolerance = 1e-06)
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
    results3 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Bonferroni", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results3' with expected results
    expect_equal(results3$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results3$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results3$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results3$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results3$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results3$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results3$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results3$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results3$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results3$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results3$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results3$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results3$conditionalRejectionProbabilities[1, ], c(0.042394596, 0.15198143, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results3$conditionalRejectionProbabilities[2, ], c(0.034321105, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results3$conditionalRejectionProbabilities[3, ], c(0.049947129, 0.35588618, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results3$conditionalPower[1, ], c(NA_real_, NA_real_, 0.44302928, 0.69082025), tolerance = 1e-06, label = paste0("c(", paste0(results3$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results3$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results3$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results3$conditionalPower[3, ], c(NA_real_, NA_real_, 0.83889182, 0.95069292), tolerance = 1e-06, label = paste0("c(", paste0(results3$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results3$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.802158, -4.2854677, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results3$repeatedConfidenceIntervalLowerBounds[2, ], c(-19.232721, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results3$repeatedConfidenceIntervalLowerBounds[3, ], c(-11.786808, 0.41764226, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results3$repeatedConfidenceIntervalUpperBounds[1, ], c(44.802158, 28.113845, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results3$repeatedConfidenceIntervalUpperBounds[2, ], c(38.432721, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results3$repeatedConfidenceIntervalUpperBounds[3, ], c(46.786808, 32.10754, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results3$repeatedPValues[1, ], c(0.5, 0.078823932, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results3$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results3$repeatedPValues[3, ], c(0.5, 0.015272156, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results3), NA)))
        expect_output(print(results3)$show())
        invisible(capture.output(expect_error(summary(results3), NA)))
        expect_output(summary(results3)$show())
        results3CodeBased <- eval(parse(text = getObjectRCode(results3, stringWrapParagraphWidth = NULL)))
        expect_equal(results3CodeBased$thetaH1, results3$thetaH1, tolerance = 1e-06)
        expect_equal(results3CodeBased$assumedStDevs, results3$assumedStDevs, tolerance = 1e-06)
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
    results4 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Bonferroni", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results4' with expected results
    expect_equal(results4$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results4$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results4$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results4$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results4$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results4$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results4$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results4$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results4$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results4$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results4$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results4$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results4$conditionalRejectionProbabilities[1, ], c(0.040740209, 0.14372404, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results4$conditionalRejectionProbabilities[2, ], c(0.033856262, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results4$conditionalRejectionProbabilities[3, ], c(0.046882975, 0.32321322, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results4$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42712247, 0.6790579), tolerance = 1e-06, label = paste0("c(", paste0(results4$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results4$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results4$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results4$conditionalPower[3, ], c(NA_real_, NA_real_, 0.81409137, 0.94181531), tolerance = 1e-06, label = paste0("c(", paste0(results4$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results4$repeatedConfidenceIntervalLowerBounds[1, ], c(-16.567569, -4.662798, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results4$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.940706, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results4$repeatedConfidenceIntervalLowerBounds[3, ], c(-13.521691, 0.049006969, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results4$repeatedConfidenceIntervalUpperBounds[1, ], c(46.567569, 28.528695, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results4$repeatedConfidenceIntervalUpperBounds[2, ], c(40.140706, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results4$repeatedConfidenceIntervalUpperBounds[3, ], c(48.521691, 32.491814, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results4$repeatedPValues[1, ], c(0.5, 0.08542716, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results4$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results4$repeatedPValues[3, ], c(0.5, 0.019420631, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results4), NA)))
        expect_output(print(results4)$show())
        invisible(capture.output(expect_error(summary(results4), NA)))
        expect_output(summary(results4)$show())
        results4CodeBased <- eval(parse(text = getObjectRCode(results4, stringWrapParagraphWidth = NULL)))
        expect_equal(results4CodeBased$thetaH1, results4$thetaH1, tolerance = 1e-06)
        expect_equal(results4CodeBased$assumedStDevs, results4$assumedStDevs, tolerance = 1e-06)
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
    results5 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Bonferroni", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results5' with expected results
    expect_equal(results5$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results5$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results5$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results5$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results5$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results5$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results5$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results5$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results5$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results5$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results5$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results5$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results5$conditionalRejectionProbabilities[1, ], c(0.043219831, 0.15803856, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results5$conditionalRejectionProbabilities[2, ], c(0.038533075, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results5$conditionalRejectionProbabilities[3, ], c(0.046782116, 0.33290332, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results5$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45436587, 0.6990644), tolerance = 1e-06, label = paste0("c(", paste0(results5$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results5$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results5$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results5$conditionalPower[3, ], c(NA_real_, NA_real_, 0.8217936, 0.94460493), tolerance = 1e-06, label = paste0("c(", paste0(results5$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results5$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.549821, -4.1213996, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results5$repeatedConfidenceIntervalLowerBounds[2, ], c(-16.848567, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results5$repeatedConfidenceIntervalLowerBounds[3, ], c(-12.483405, 0.16013976, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results5$repeatedConfidenceIntervalUpperBounds[1, ], c(44.549821, 27.945069, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results5$repeatedConfidenceIntervalUpperBounds[2, ], c(36.048567, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results5$repeatedConfidenceIntervalUpperBounds[3, ], c(47.483405, 32.356999, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results5$repeatedPValues[1, ], c(0.5, 0.07440366, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results5$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results5$repeatedPValues[3, ], c(0.5, 0.018077861, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results5), NA)))
        expect_output(print(results5)$show())
        invisible(capture.output(expect_error(summary(results5), NA)))
        expect_output(summary(results5)$show())
        results5CodeBased <- eval(parse(text = getObjectRCode(results5, stringWrapParagraphWidth = NULL)))
        expect_equal(results5CodeBased$thetaH1, results5$thetaH1, tolerance = 1e-06)
        expect_equal(results5CodeBased$assumedStDevs, results5$assumedStDevs, tolerance = 1e-06)
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
    results6 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Bonferroni", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results6' with expected results
    expect_equal(results6$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results6$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results6$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results6$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results6$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results6$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results6$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results6$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results6$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results6$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results6$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results6$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results6$conditionalRejectionProbabilities[1, ], c(0.039664178, 0.14221619, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results6$conditionalRejectionProbabilities[2, ], c(0.037322005, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results6$conditionalRejectionProbabilities[3, ], c(0.041377736, 0.28315003, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results6$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42415922, 0.67684078), tolerance = 1e-06, label = paste0("c(", paste0(results6$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results6$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results6$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results6$conditionalPower[3, ], c(NA_real_, NA_real_, 0.77871789, 0.92862656), tolerance = 1e-06, label = paste0("c(", paste0(results6$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results6$repeatedConfidenceIntervalLowerBounds[1, ], c(-18.601467, -4.8144337, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results6$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.153637, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results6$repeatedConfidenceIntervalLowerBounds[3, ], c(-16.403927, -0.48327212, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results6$repeatedConfidenceIntervalUpperBounds[1, ], c(48.601467, 28.627869, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results6$repeatedConfidenceIntervalUpperBounds[2, ], c(39.353637, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results6$repeatedConfidenceIntervalUpperBounds[3, ], c(51.403927, 32.999307, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results6$repeatedPValues[1, ], c(0.5, 0.086711756, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results6$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results6$repeatedPValues[3, ], c(0.5, 0.026234621, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results6), NA)))
        expect_output(print(results6)$show())
        invisible(capture.output(expect_error(summary(results6), NA)))
        expect_output(summary(results6)$show())
        results6CodeBased <- eval(parse(text = getObjectRCode(results6, stringWrapParagraphWidth = NULL)))
        expect_equal(results6CodeBased$thetaH1, results6$thetaH1, tolerance = 1e-06)
        expect_equal(results6CodeBased$assumedStDevs, results6$assumedStDevs, tolerance = 1e-06)
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
    results7 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results7' with expected results
    expect_equal(results7$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results7$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results7$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results7$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results7$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results7$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results7$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results7$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results7$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results7$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results7$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results7$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results7$conditionalRejectionProbabilities[1, ], c(0.043739576, 0.16022367, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results7$conditionalRejectionProbabilities[2, ], c(0.03844608, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results7$conditionalRejectionProbabilities[3, ], c(0.052717287, 0.35672949, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results7$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45838936, 0.70196346), tolerance = 1e-06, label = paste0("c(", paste0(results7$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results7$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results7$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results7$conditionalPower[3, ], c(NA_real_, NA_real_, 0.83948961, 0.95090316), tolerance = 1e-06, label = paste0("c(", paste0(results7$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results7$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.395028, -4.0669228, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results7$repeatedConfidenceIntervalLowerBounds[2, ], c(-16.891548, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results7$repeatedConfidenceIntervalLowerBounds[3, ], c(-12.071338, 0.24153969, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results7$repeatedConfidenceIntervalUpperBounds[1, ], c(44.395028, 27.895908, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results7$repeatedConfidenceIntervalUpperBounds[2, ], c(36.091548, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results7$repeatedConfidenceIntervalUpperBounds[3, ], c(47.071339, 32.285152, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results7$repeatedPValues[1, ], c(0.5, 0.072888275, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results7$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results7$repeatedPValues[3, ], c(0.5, 0.015177743, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results7), NA)))
        expect_output(print(results7)$show())
        invisible(capture.output(expect_error(summary(results7), NA)))
        expect_output(summary(results7)$show())
        results7CodeBased <- eval(parse(text = getObjectRCode(results7, stringWrapParagraphWidth = NULL)))
        expect_equal(results7CodeBased$thetaH1, results7$thetaH1, tolerance = 1e-06)
        expect_equal(results7CodeBased$assumedStDevs, results7$assumedStDevs, tolerance = 1e-06)
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
    results8 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results8' with expected results
    expect_equal(results8$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results8$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results8$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results8$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results8$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results8$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results8$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results8$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results8$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results8$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results8$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results8$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results8$conditionalRejectionProbabilities[1, ], c(0.040100206, 0.14400686, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results8$conditionalRejectionProbabilities[2, ], c(0.037171319, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results8$conditionalRejectionProbabilities[3, ], c(0.048708233, 0.3133215, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results8$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42767622, 0.67947129), tolerance = 1e-06, label = paste0("c(", paste0(results8$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results8$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results8$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results8$conditionalPower[3, ], c(NA_real_, NA_real_, 0.80590445, 0.93881804), tolerance = 1e-06, label = paste0("c(", paste0(results8$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results8$repeatedConfidenceIntervalLowerBounds[1, ], c(-18.433726, -4.7641755, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results8$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.426743, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results8$repeatedConfidenceIntervalLowerBounds[3, ], c(-15.938808, -0.40329832, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results8$repeatedConfidenceIntervalUpperBounds[1, ], c(48.433726, 28.584393, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results8$repeatedConfidenceIntervalUpperBounds[2, ], c(39.626743, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results8$repeatedConfidenceIntervalUpperBounds[3, ], c(50.938808, 32.927366, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results8$repeatedPValues[1, ], c(0.5, 0.085188742, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results8$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results8$repeatedPValues[3, ], c(0.5, 0.020901685, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results8), NA)))
        expect_output(print(results8)$show())
        invisible(capture.output(expect_error(summary(results8), NA)))
        expect_output(summary(results8)$show())
        results8CodeBased <- eval(parse(text = getObjectRCode(results8, stringWrapParagraphWidth = NULL)))
        expect_equal(results8CodeBased$thetaH1, results8$thetaH1, tolerance = 1e-06)
        expect_equal(results8CodeBased$assumedStDevs, results8$assumedStDevs, tolerance = 1e-06)
        expect_equal(results8CodeBased$conditionalRejectionProbabilities, results8$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results8CodeBased$conditionalPower, results8$conditionalPower, tolerance = 1e-06)
        expect_equal(results8CodeBased$repeatedConfidenceIntervalLowerBounds, results8$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results8CodeBased$repeatedConfidenceIntervalUpperBounds, results8$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results8CodeBased$repeatedPValues, results8$repeatedPValues, tolerance = 1e-06)
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
    results9 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Simes", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results9' with expected results
    expect_equal(results9$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results9$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results9$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results9$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results9$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results9$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results9$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results9$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results9$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results9$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results9$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results9$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results9$conditionalRejectionProbabilities[1, ], c(0.042394596, 0.15198143, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results9$conditionalRejectionProbabilities[2, ], c(0.034321105, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results9$conditionalRejectionProbabilities[3, ], c(0.051237296, 0.36121246, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results9$conditionalPower[1, ], c(NA_real_, NA_real_, 0.44302928, 0.69082025), tolerance = 1e-06, label = paste0("c(", paste0(results9$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results9$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results9$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results9$conditionalPower[3, ], c(NA_real_, NA_real_, 0.84263383, 0.95200602), tolerance = 1e-06, label = paste0("c(", paste0(results9$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results9$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.802158, -4.2854677, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results9$repeatedConfidenceIntervalLowerBounds[2, ], c(-19.232721, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results9$repeatedConfidenceIntervalLowerBounds[3, ], c(-11.786808, 0.41764226, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results9$repeatedConfidenceIntervalUpperBounds[1, ], c(44.802158, 28.113845, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results9$repeatedConfidenceIntervalUpperBounds[2, ], c(38.432721, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results9$repeatedConfidenceIntervalUpperBounds[3, ], c(46.786808, 32.10754, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results9$repeatedPValues[1, ], c(0.5, 0.078823932, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results9$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results9$repeatedPValues[3, ], c(0.5, 0.014689462, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results9$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results9), NA)))
        expect_output(print(results9)$show())
        invisible(capture.output(expect_error(summary(results9), NA)))
        expect_output(summary(results9)$show())
        results9CodeBased <- eval(parse(text = getObjectRCode(results9, stringWrapParagraphWidth = NULL)))
        expect_equal(results9CodeBased$thetaH1, results9$thetaH1, tolerance = 1e-06)
        expect_equal(results9CodeBased$assumedStDevs, results9$assumedStDevs, tolerance = 1e-06)
        expect_equal(results9CodeBased$conditionalRejectionProbabilities, results9$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results9CodeBased$conditionalPower, results9$conditionalPower, tolerance = 1e-06)
        expect_equal(results9CodeBased$repeatedConfidenceIntervalLowerBounds, results9$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results9CodeBased$repeatedConfidenceIntervalUpperBounds, results9$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results9CodeBased$repeatedPValues, results9$repeatedPValues, tolerance = 1e-06)
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
    results10 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Simes", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results10' with expected results
    expect_equal(results10$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results10$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results10$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results10$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results10$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results10$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results10$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results10$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results10$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results10$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results10$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results10$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results10$conditionalRejectionProbabilities[1, ], c(0.040740209, 0.14372404, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results10$conditionalRejectionProbabilities[2, ], c(0.033856262, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results10$conditionalRejectionProbabilities[3, ], c(0.049414261, 0.33374326, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results10$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42712247, 0.6790579), tolerance = 1e-06, label = paste0("c(", paste0(results10$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results10$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results10$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results10$conditionalPower[3, ], c(NA_real_, NA_real_, 0.82244694, 0.94484021), tolerance = 1e-06, label = paste0("c(", paste0(results10$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results10$repeatedConfidenceIntervalLowerBounds[1, ], c(-16.567569, -4.662798, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results10$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.940706, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results10$repeatedConfidenceIntervalLowerBounds[3, ], c(-13.521691, 0.049006969, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results10$repeatedConfidenceIntervalUpperBounds[1, ], c(46.567569, 28.528695, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results10$repeatedConfidenceIntervalUpperBounds[2, ], c(40.140706, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results10$repeatedConfidenceIntervalUpperBounds[3, ], c(48.521691, 32.491814, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results10$repeatedPValues[1, ], c(0.5, 0.08542716, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results10$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results10$repeatedPValues[3, ], c(0.5, 0.017966281, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results10), NA)))
        expect_output(print(results10)$show())
        invisible(capture.output(expect_error(summary(results10), NA)))
        expect_output(summary(results10)$show())
        results10CodeBased <- eval(parse(text = getObjectRCode(results10, stringWrapParagraphWidth = NULL)))
        expect_equal(results10CodeBased$thetaH1, results10$thetaH1, tolerance = 1e-06)
        expect_equal(results10CodeBased$assumedStDevs, results10$assumedStDevs, tolerance = 1e-06)
        expect_equal(results10CodeBased$conditionalRejectionProbabilities, results10$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results10CodeBased$conditionalPower, results10$conditionalPower, tolerance = 1e-06)
        expect_equal(results10CodeBased$repeatedConfidenceIntervalLowerBounds, results10$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results10CodeBased$repeatedConfidenceIntervalUpperBounds, results10$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results10CodeBased$repeatedPValues, results10$repeatedPValues, tolerance = 1e-06)
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
    results11 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Simes", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results11' with expected results
    expect_equal(results11$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results11$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results11$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results11$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results11$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results11$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results11$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results11$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results11$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results11$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results11$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results11$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results11$conditionalRejectionProbabilities[1, ], c(0.043219831, 0.15803856, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results11$conditionalRejectionProbabilities[2, ], c(0.038533075, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results11$conditionalRejectionProbabilities[3, ], c(0.052145589, 0.35513472, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results11$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45436587, 0.6990644), tolerance = 1e-06, label = paste0("c(", paste0(results11$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results11$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results11$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results11$conditionalPower[3, ], c(NA_real_, NA_real_, 0.83835744, 0.95050484), tolerance = 1e-06, label = paste0("c(", paste0(results11$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results11$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.549821, -4.1213996, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results11$repeatedConfidenceIntervalLowerBounds[2, ], c(-16.848567, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results11$repeatedConfidenceIntervalLowerBounds[3, ], c(-12.483405, 0.16013976, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results11$repeatedConfidenceIntervalUpperBounds[1, ], c(44.549821, 27.945069, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results11$repeatedConfidenceIntervalUpperBounds[2, ], c(36.048567, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results11$repeatedConfidenceIntervalUpperBounds[3, ], c(47.483405, 32.356999, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results11$repeatedPValues[1, ], c(0.5, 0.07440366, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results11$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results11$repeatedPValues[3, ], c(0.5, 0.015356079, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results11), NA)))
        expect_output(print(results11)$show())
        invisible(capture.output(expect_error(summary(results11), NA)))
        expect_output(summary(results11)$show())
        results11CodeBased <- eval(parse(text = getObjectRCode(results11, stringWrapParagraphWidth = NULL)))
        expect_equal(results11CodeBased$thetaH1, results11$thetaH1, tolerance = 1e-06)
        expect_equal(results11CodeBased$assumedStDevs, results11$assumedStDevs, tolerance = 1e-06)
        expect_equal(results11CodeBased$conditionalRejectionProbabilities, results11$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results11CodeBased$conditionalPower, results11$conditionalPower, tolerance = 1e-06)
        expect_equal(results11CodeBased$repeatedConfidenceIntervalLowerBounds, results11$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results11CodeBased$repeatedConfidenceIntervalUpperBounds, results11$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results11CodeBased$repeatedPValues, results11$repeatedPValues, tolerance = 1e-06)
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
    results12 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Simes", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results12' with expected results
    expect_equal(results12$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results12$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results12$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results12$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results12$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results12$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results12$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results12$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results12$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results12$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results12$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results12$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results12$conditionalRejectionProbabilities[1, ], c(0.039664178, 0.14221619, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results12$conditionalRejectionProbabilities[2, ], c(0.037322005, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results12$conditionalRejectionProbabilities[3, ], c(0.048226966, 0.31219358, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results12$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42415922, 0.67684078), tolerance = 1e-06, label = paste0("c(", paste0(results12$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results12$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results12$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results12$conditionalPower[3, ], c(NA_real_, NA_real_, 0.80494934, 0.93846621), tolerance = 1e-06, label = paste0("c(", paste0(results12$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results12$repeatedConfidenceIntervalLowerBounds[1, ], c(-18.601467, -4.8144337, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results12$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.153637, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results12$repeatedConfidenceIntervalLowerBounds[3, ], c(-16.403927, -0.48327212, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results12$repeatedConfidenceIntervalUpperBounds[1, ], c(48.601467, 28.627869, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results12$repeatedConfidenceIntervalUpperBounds[2, ], c(39.353637, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results12$repeatedConfidenceIntervalUpperBounds[3, ], c(51.403927, 32.999307, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results12$repeatedPValues[1, ], c(0.5, 0.086711756, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results12$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results12$repeatedPValues[3, ], c(0.5, 0.021078114, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results12), NA)))
        expect_output(print(results12)$show())
        invisible(capture.output(expect_error(summary(results12), NA)))
        expect_output(summary(results12)$show())
        results12CodeBased <- eval(parse(text = getObjectRCode(results12, stringWrapParagraphWidth = NULL)))
        expect_equal(results12CodeBased$thetaH1, results12$thetaH1, tolerance = 1e-06)
        expect_equal(results12CodeBased$assumedStDevs, results12$assumedStDevs, tolerance = 1e-06)
        expect_equal(results12CodeBased$conditionalRejectionProbabilities, results12$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results12CodeBased$conditionalPower, results12$conditionalPower, tolerance = 1e-06)
        expect_equal(results12CodeBased$repeatedConfidenceIntervalLowerBounds, results12$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results12CodeBased$repeatedConfidenceIntervalUpperBounds, results12$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results12CodeBased$repeatedPValues, results12$repeatedPValues, tolerance = 1e-06)
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
    results13 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Sidak", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results13' with expected results
    expect_equal(results13$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results13$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results13$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results13$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results13$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results13$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results13$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results13$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results13$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results13$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results13$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results13$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results13$conditionalRejectionProbabilities[1, ], c(0.044513617, 0.16250147, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results13$conditionalRejectionProbabilities[2, ], c(0.03844608, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results13$conditionalRejectionProbabilities[3, ], c(0.049538053, 0.34419132, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results13$conditionalPower[1, ], c(NA_real_, NA_real_, 0.46254707, 0.70494473), tolerance = 1e-06, label = paste0("c(", paste0(results13$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results13$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results13$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results13$conditionalPower[3, ], c(NA_real_, NA_real_, 0.83038985, 0.94768376), tolerance = 1e-06, label = paste0("c(", paste0(results13$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results13$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.393216, -4.0328452, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results13$repeatedConfidenceIntervalLowerBounds[2, ], c(-16.889915, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results13$repeatedConfidenceIntervalLowerBounds[3, ], c(-12.069516, 0.29402607, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results13$repeatedConfidenceIntervalUpperBounds[1, ], c(44.393216, 27.725836, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results13$repeatedConfidenceIntervalUpperBounds[2, ], c(36.089915, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results13$repeatedConfidenceIntervalUpperBounds[3, ], c(47.069516, 32.182569, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results13$repeatedPValues[1, ], c(0.5, 0.071351909, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results13$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results13$repeatedPValues[3, ], c(0.5, 0.016637815, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results13), NA)))
        expect_output(print(results13)$show())
        invisible(capture.output(expect_error(summary(results13), NA)))
        expect_output(summary(results13)$show())
        results13CodeBased <- eval(parse(text = getObjectRCode(results13, stringWrapParagraphWidth = NULL)))
        expect_equal(results13CodeBased$thetaH1, results13$thetaH1, tolerance = 1e-06)
        expect_equal(results13CodeBased$assumedStDevs, results13$assumedStDevs, tolerance = 1e-06)
        expect_equal(results13CodeBased$conditionalRejectionProbabilities, results13$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results13CodeBased$conditionalPower, results13$conditionalPower, tolerance = 1e-06)
        expect_equal(results13CodeBased$repeatedConfidenceIntervalLowerBounds, results13$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results13CodeBased$repeatedConfidenceIntervalUpperBounds, results13$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results13CodeBased$repeatedPValues, results13$repeatedPValues, tolerance = 1e-06)
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
    results14 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Sidak", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results14' with expected results
    expect_equal(results14$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results14$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results14$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results14$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results14$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results14$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results14$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results14$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results14$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results14$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results14$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results14$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results14$conditionalRejectionProbabilities[1, ], c(0.040941914, 0.14648989, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results14$conditionalRejectionProbabilities[2, ], c(0.037171319, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results14$conditionalRejectionProbabilities[3, ], c(0.043912863, 0.29382832, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results14$conditionalPower[1, ], c(NA_real_, NA_real_, 0.4325103, 0.68306799), tolerance = 1e-06, label = paste0("c(", paste0(results14$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results14$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results14$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results14$conditionalPower[3, ], c(NA_real_, NA_real_, 0.78874215, 0.93242714), tolerance = 1e-06, label = paste0("c(", paste0(results14$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results14$repeatedConfidenceIntervalLowerBounds[1, ], c(-18.431163, -4.7231897, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results14$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.424453, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results14$repeatedConfidenceIntervalLowerBounds[3, ], c(-15.936268, -0.34247232, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results14$repeatedConfidenceIntervalUpperBounds[1, ], c(48.431163, 28.407231, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results14$repeatedConfidenceIntervalUpperBounds[2, ], c(39.624453, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results14$repeatedConfidenceIntervalUpperBounds[3, ], c(50.936268, 32.815818, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results14$repeatedPValues[1, ], c(0.5, 0.083136439, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results14$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results14$repeatedPValues[3, ], c(0.5, 0.024192808, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results14), NA)))
        expect_output(print(results14)$show())
        invisible(capture.output(expect_error(summary(results14), NA)))
        expect_output(summary(results14)$show())
        results14CodeBased <- eval(parse(text = getObjectRCode(results14, stringWrapParagraphWidth = NULL)))
        expect_equal(results14CodeBased$thetaH1, results14$thetaH1, tolerance = 1e-06)
        expect_equal(results14CodeBased$assumedStDevs, results14$assumedStDevs, tolerance = 1e-06)
        expect_equal(results14CodeBased$conditionalRejectionProbabilities, results14$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results14CodeBased$conditionalPower, results14$conditionalPower, tolerance = 1e-06)
        expect_equal(results14CodeBased$repeatedConfidenceIntervalLowerBounds, results14$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results14CodeBased$repeatedConfidenceIntervalUpperBounds, results14$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results14CodeBased$repeatedPValues, results14$repeatedPValues, tolerance = 1e-06)
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
    results15 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Sidak", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results15' with expected results
    expect_equal(results15$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results15$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results15$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results15$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results15$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results15$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results15$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results15$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results15$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results15$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results15$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results15$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results15$conditionalRejectionProbabilities[1, ], c(0.043192758, 0.15430882, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results15$conditionalRejectionProbabilities[2, ], c(0.034321105, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results15$conditionalRejectionProbabilities[3, ], c(0.050842102, 0.35990794, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results15$conditionalPower[1, ], c(NA_real_, NA_real_, 0.44741759, 0.6940249), tolerance = 1e-06, label = paste0("c(", paste0(results15$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results15$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results15$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results15$conditionalPower[3, ], c(NA_real_, NA_real_, 0.84172467, 0.95168763), tolerance = 1e-06, label = paste0("c(", paste0(results15$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results15$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.800321, -4.2506387, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results15$repeatedConfidenceIntervalLowerBounds[2, ], c(-19.230944, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results15$repeatedConfidenceIntervalLowerBounds[3, ], c(-11.785003, 0.46968016, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results15$repeatedConfidenceIntervalUpperBounds[1, ], c(44.800321, 27.943326, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results15$repeatedConfidenceIntervalUpperBounds[2, ], c(38.430944, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results15$repeatedConfidenceIntervalUpperBounds[3, ], c(46.785003, 32.005071, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results15$repeatedPValues[1, ], c(0.5, 0.077086341, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results15$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results15$repeatedPValues[3, ], c(0.5, 0.014829652, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results15), NA)))
        expect_output(print(results15)$show())
        invisible(capture.output(expect_error(summary(results15), NA)))
        expect_output(summary(results15)$show())
        results15CodeBased <- eval(parse(text = getObjectRCode(results15, stringWrapParagraphWidth = NULL)))
        expect_equal(results15CodeBased$thetaH1, results15$thetaH1, tolerance = 1e-06)
        expect_equal(results15CodeBased$assumedStDevs, results15$assumedStDevs, tolerance = 1e-06)
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
    results16 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Sidak", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results16' with expected results
    expect_equal(results16$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results16$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results16$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results16$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results16$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results16$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results16$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results16$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results16$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results16$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results16$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results16$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results16$conditionalRejectionProbabilities[1, ], c(0.041569453, 0.14613212, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results16$conditionalRejectionProbabilities[2, ], c(0.033856262, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results16$conditionalRejectionProbabilities[3, ], c(0.047839714, 0.32760313, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results16$conditionalPower[1, ], c(NA_real_, NA_real_, 0.43181681, 0.68255335), tolerance = 1e-06, label = paste0("c(", paste0(results16$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results16$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results16$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results16$conditionalPower[3, ], c(NA_real_, NA_real_, 0.81761872, 0.94309649), tolerance = 1e-06, label = paste0("c(", paste0(results16$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results16$repeatedConfidenceIntervalLowerBounds[1, ], c(-16.565416, -4.6248784, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results16$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.938622, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results16$repeatedConfidenceIntervalLowerBounds[3, ], c(-13.519575, 0.10461531, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results16$repeatedConfidenceIntervalUpperBounds[1, ], c(46.565416, 28.357046, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results16$repeatedConfidenceIntervalUpperBounds[2, ], c(40.138622, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results16$repeatedConfidenceIntervalUpperBounds[3, ], c(48.519575, 32.386196, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results16$repeatedPValues[1, ], c(0.5, 0.083428262, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results16$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results16$repeatedPValues[3, ], c(0.5, 0.018799791, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results16), NA)))
        expect_output(print(results16)$show())
        invisible(capture.output(expect_error(summary(results16), NA)))
        expect_output(summary(results16)$show())
        results16CodeBased <- eval(parse(text = getObjectRCode(results16, stringWrapParagraphWidth = NULL)))
        expect_equal(results16CodeBased$thetaH1, results16$thetaH1, tolerance = 1e-06)
        expect_equal(results16CodeBased$assumedStDevs, results16$assumedStDevs, tolerance = 1e-06)
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
    results17 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Sidak", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results17' with expected results
    expect_equal(results17$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results17$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results17$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results17$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results17$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results17$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results17$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results17$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results17$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results17$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results17$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results17$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results17$conditionalRejectionProbabilities[1, ], c(0.044003076, 0.16034604, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results17$conditionalRejectionProbabilities[2, ], c(0.038533075, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results17$conditionalRejectionProbabilities[3, ], c(0.047740982, 0.33733332, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results17$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45861366, 0.70212467), tolerance = 1e-06, label = paste0("c(", paste0(results17$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results17$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results17$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results17$conditionalPower[3, ], c(NA_real_, NA_real_, 0.82521432, 0.94583446), tolerance = 1e-06, label = paste0("c(", paste0(results17$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results17$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.548, -4.0869288, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results17$repeatedConfidenceIntervalLowerBounds[2, ], c(-16.846937, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results17$repeatedConfidenceIntervalLowerBounds[3, ], c(-12.481557, 0.21501802, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results17$repeatedConfidenceIntervalUpperBounds[1, ], c(44.548, 27.773536, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results17$repeatedConfidenceIntervalUpperBounds[2, ], c(36.046937, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results17$repeatedConfidenceIntervalUpperBounds[3, ], c(47.481556, 32.250037, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results17$repeatedPValues[1, ], c(0.5, 0.072804352, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results17$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results17$repeatedPValues[3, ], c(0.5, 0.017498028, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results17), NA)))
        expect_output(print(results17)$show())
        invisible(capture.output(expect_error(summary(results17), NA)))
        expect_output(summary(results17)$show())
        results17CodeBased <- eval(parse(text = getObjectRCode(results17, stringWrapParagraphWidth = NULL)))
        expect_equal(results17CodeBased$thetaH1, results17$thetaH1, tolerance = 1e-06)
        expect_equal(results17CodeBased$assumedStDevs, results17$assumedStDevs, tolerance = 1e-06)
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
    results18 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Sidak", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results18' with expected results
    expect_equal(results18$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results18$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results18$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results18$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results18$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results18$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results18$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results18$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results18$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results18$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results18$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results18$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results18$conditionalRejectionProbabilities[1, ], c(0.040514523, 0.14472681, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results18$conditionalRejectionProbabilities[2, ], c(0.037322005, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results18$conditionalRejectionProbabilities[3, ], c(0.042460333, 0.28832504, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results18$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42908294, 0.68052019), tolerance = 1e-06, label = paste0("c(", paste0(results18$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results18$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results18$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results18$conditionalPower[3, ], c(NA_real_, NA_real_, 0.78363361, 0.93049656), tolerance = 1e-06, label = paste0("c(", paste0(results18$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results18$repeatedConfidenceIntervalLowerBounds[1, ], c(-18.598892, -4.7729883, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results18$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.151395, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results18$repeatedConfidenceIntervalLowerBounds[3, ], c(-16.401351, -0.41981706, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results18$repeatedConfidenceIntervalUpperBounds[1, ], c(48.598892, 28.449073, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results18$repeatedConfidenceIntervalUpperBounds[2, ], c(39.351395, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results18$repeatedConfidenceIntervalUpperBounds[3, ], c(51.401351, 32.883177, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results18$repeatedPValues[1, ], c(0.5, 0.084586974, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results18$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results18$repeatedPValues[3, ], c(0.5, 0.025221821, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results18$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results18), NA)))
        expect_output(print(results18)$show())
        invisible(capture.output(expect_error(summary(results18), NA)))
        expect_output(summary(results18)$show())
        results18CodeBased <- eval(parse(text = getObjectRCode(results18, stringWrapParagraphWidth = NULL)))
        expect_equal(results18CodeBased$thetaH1, results18$thetaH1, tolerance = 1e-06)
        expect_equal(results18CodeBased$assumedStDevs, results18$assumedStDevs, tolerance = 1e-06)
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
    results19 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Dunnett", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results19' with expected results
    expect_equal(results19$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results19$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results19$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results19$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results19$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results19$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results19$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results19$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results19$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results19$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results19$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results19$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results19$conditionalRejectionProbabilities[1, ], c(0.046821821, 0.16471602, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results19$conditionalRejectionProbabilities[2, ], c(0.034321105, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results19$conditionalRejectionProbabilities[3, ], c(0.056787656, 0.38875311, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results19$conditionalPower[1, ], c(NA_real_, NA_real_, 0.46655424, 0.70780427), tolerance = 1e-06, label = paste0("c(", paste0(results19$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results19$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results19$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results19$conditionalPower[3, ], c(NA_real_, NA_real_, 0.8607721, 0.95827226), tolerance = 1e-06, label = paste0("c(", paste0(results19$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results19$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.645333, -3.927683, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results19$repeatedConfidenceIntervalLowerBounds[2, ], c(-19.080998, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results19$repeatedConfidenceIntervalLowerBounds[3, ], c(-11.632695, 0.82950364, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results19$repeatedConfidenceIntervalUpperBounds[1, ], c(44.645334, 27.415422, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results19$repeatedConfidenceIntervalUpperBounds[2, ], c(38.280999, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results19$repeatedConfidenceIntervalUpperBounds[3, ], c(46.632695, 31.563129, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results19$repeatedPValues[1, ], c(0.5, 0.069897558, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results19$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results19$repeatedPValues[3, ], c(0.5, 0.012021087, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results19$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results19), NA)))
        expect_output(print(results19)$show())
        invisible(capture.output(expect_error(summary(results19), NA)))
        expect_output(summary(results19)$show())
        results19CodeBased <- eval(parse(text = getObjectRCode(results19, stringWrapParagraphWidth = NULL)))
        expect_equal(results19CodeBased$thetaH1, results19$thetaH1, tolerance = 1e-06)
        expect_equal(results19CodeBased$assumedStDevs, results19$assumedStDevs, tolerance = 1e-06)
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
    results20 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Dunnett", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results20' with expected results
    expect_equal(results20$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results20$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results20$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results20$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results20$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results20$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results20$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results20$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results20$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results20$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results20$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results20$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results20$conditionalRejectionProbabilities[1, ], c(0.045317687, 0.15683192, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results20$conditionalRejectionProbabilities[2, ], c(0.033856262, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results20$conditionalRejectionProbabilities[3, ], c(0.054085103, 0.3588303, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results20$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45212919, 0.69744676), tolerance = 1e-06, label = paste0("c(", paste0(results20$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results20$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results20$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results20$conditionalPower[3, ], c(NA_real_, NA_real_, 0.84097006, 0.95142305), tolerance = 1e-06, label = paste0("c(", paste0(results20$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results20$repeatedConfidenceIntervalLowerBounds[1, ], c(-16.335113, -4.2557288, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results20$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.71581, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results20$repeatedConfidenceIntervalLowerBounds[3, ], c(-13.293254, 0.50940978, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results20$repeatedConfidenceIntervalUpperBounds[1, ], c(46.335113, 27.786662, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results20$repeatedConfidenceIntervalUpperBounds[2, ], c(39.91581, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results20$repeatedConfidenceIntervalUpperBounds[3, ], c(48.293254, 31.900882, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results20$repeatedPValues[1, ], c(0.5, 0.075258151, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results20$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results20$repeatedPValues[3, ], c(0.5, 0.014946954, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results20$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results20), NA)))
        expect_output(print(results20)$show())
        invisible(capture.output(expect_error(summary(results20), NA)))
        expect_output(summary(results20)$show())
        results20CodeBased <- eval(parse(text = getObjectRCode(results20, stringWrapParagraphWidth = NULL)))
        expect_equal(results20CodeBased$thetaH1, results20$thetaH1, tolerance = 1e-06)
        expect_equal(results20CodeBased$assumedStDevs, results20$assumedStDevs, tolerance = 1e-06)
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
    results21 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Bonferroni", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results21' with expected results
    expect_equal(results21$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results21$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results21$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results21$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results21$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results21$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results21$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results21$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results21$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results21$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results21$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results21$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results21$conditionalRejectionProbabilities[1, ], c(0.024608533, 0.053964296, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results21$conditionalRejectionProbabilities[2, ], c(0.021871903, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results21$conditionalRejectionProbabilities[3, ], c(0.027261939, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results21$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.256183, -4.0576303, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results21$repeatedConfidenceIntervalLowerBounds[2, ], c(-13.161515, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results21$repeatedConfidenceIntervalLowerBounds[3, ], c(-7.9076686, 0.4326836, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results21$repeatedConfidenceIntervalUpperBounds[1, ], c(40.256183, 26.806923, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results21$repeatedConfidenceIntervalUpperBounds[2, ], c(32.361515, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results21$repeatedConfidenceIntervalUpperBounds[3, ], c(42.907669, 31.664999, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results21$repeatedPValues[1, ], c(0.17463845, 0.062131804, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results21$repeatedPValues[2, ], c(0.20337355, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results21$repeatedPValues[3, ], c(0.1527221, 0.015597359, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results21$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results21$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.277, 0.453), tolerance = 1e-06, label = paste0("c(", paste0(results21$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results21$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results21$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results21$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results21$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results21), NA)))
        expect_output(print(results21)$show())
        invisible(capture.output(expect_error(summary(results21), NA)))
        expect_output(summary(results21)$show())
        results21CodeBased <- eval(parse(text = getObjectRCode(results21, stringWrapParagraphWidth = NULL)))
        expect_equal(results21CodeBased$thetaH1, results21$thetaH1, tolerance = 1e-06)
        expect_equal(results21CodeBased$assumedStDevs, results21$assumedStDevs, tolerance = 1e-06)
        expect_equal(results21CodeBased$conditionalRejectionProbabilities, results21$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results21CodeBased$repeatedConfidenceIntervalLowerBounds, results21$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results21CodeBased$repeatedConfidenceIntervalUpperBounds, results21$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results21CodeBased$repeatedPValues, results21$repeatedPValues, tolerance = 1e-06)
        expect_equal(results21CodeBased$conditionalPowerSimulated, results21$conditionalPowerSimulated, tolerance = 1e-06)
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
    results22 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Bonferroni", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results22' with expected results
    expect_equal(results22$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results22$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results22$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results22$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results22$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results22$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results22$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results22$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results22$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results22$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results22$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results22$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results22$conditionalRejectionProbabilities[1, ], c(0.022711489, 0.047669561, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results22$conditionalRejectionProbabilities[2, ], c(0.021234311, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results22$conditionalRejectionProbabilities[3, ], c(0.024147032, 0.14148061, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results22$repeatedConfidenceIntervalLowerBounds[1, ], c(-12.830851, -4.722817, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results22$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.416779, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results22$repeatedConfidenceIntervalLowerBounds[3, ], c(-10.376075, -0.16648466, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results22$repeatedConfidenceIntervalUpperBounds[1, ], c(42.830851, 27.447651, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results22$repeatedConfidenceIntervalUpperBounds[2, ], c(34.616779, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results22$repeatedConfidenceIntervalUpperBounds[3, ], c(45.376075, 32.257244, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results22$repeatedPValues[1, ], c(0.19376148, 0.070634747, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results22$repeatedPValues[2, ], c(0.2112175, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results22$repeatedPValues[3, ], c(0.17899101, 0.021776202, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results22$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results22$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.257, 0.434), tolerance = 1e-06, label = paste0("c(", paste0(results22$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results22$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results22$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results22$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.656, 0.797), tolerance = 1e-06, label = paste0("c(", paste0(results22$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results22), NA)))
        expect_output(print(results22)$show())
        invisible(capture.output(expect_error(summary(results22), NA)))
        expect_output(summary(results22)$show())
        results22CodeBased <- eval(parse(text = getObjectRCode(results22, stringWrapParagraphWidth = NULL)))
        expect_equal(results22CodeBased$thetaH1, results22$thetaH1, tolerance = 1e-06)
        expect_equal(results22CodeBased$assumedStDevs, results22$assumedStDevs, tolerance = 1e-06)
        expect_equal(results22CodeBased$conditionalRejectionProbabilities, results22$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results22CodeBased$repeatedConfidenceIntervalLowerBounds, results22$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results22CodeBased$repeatedConfidenceIntervalUpperBounds, results22$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results22CodeBased$repeatedPValues, results22$repeatedPValues, tolerance = 1e-06)
        expect_equal(results22CodeBased$conditionalPowerSimulated, results22$conditionalPowerSimulated, tolerance = 1e-06)
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
    results23 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Bonferroni", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results23' with expected results
    expect_equal(results23$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results23$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results23$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results23$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results23$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results23$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results23$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results23$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results23$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results23$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results23$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results23$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results23$conditionalRejectionProbabilities[1, ], c(0.02389937, 0.050606752, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results23$conditionalRejectionProbabilities[2, ], c(0.019837849, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results23$conditionalRejectionProbabilities[3, ], c(0.028008383, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results23$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.605988, -4.2731734, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results23$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.173049, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results23$repeatedConfidenceIntervalLowerBounds[3, ], c(-7.6631999, 0.60791563, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results23$repeatedConfidenceIntervalUpperBounds[1, ], c(40.605988, 27.021858, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results23$repeatedConfidenceIntervalUpperBounds[2, ], c(34.373049, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results23$repeatedConfidenceIntervalUpperBounds[3, ], c(42.6632, 31.486561, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results23$repeatedPValues[1, ], c(0.18140284, 0.066412839, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results23$repeatedPValues[2, ], c(0.23027951, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results23$repeatedPValues[3, ], c(0.14737581, 0.014014262, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results23$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results23$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.268, 0.445), tolerance = 1e-06, label = paste0("c(", paste0(results23$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results23$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results23$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results23$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results23$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results23), NA)))
        expect_output(print(results23)$show())
        invisible(capture.output(expect_error(summary(results23), NA)))
        expect_output(summary(results23)$show())
        results23CodeBased <- eval(parse(text = getObjectRCode(results23, stringWrapParagraphWidth = NULL)))
        expect_equal(results23CodeBased$thetaH1, results23$thetaH1, tolerance = 1e-06)
        expect_equal(results23CodeBased$assumedStDevs, results23$assumedStDevs, tolerance = 1e-06)
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
    results24 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Bonferroni", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results24' with expected results
    expect_equal(results24$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results24$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results24$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results24$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results24$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results24$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results24$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results24$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results24$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results24$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results24$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results24$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results24$conditionalRejectionProbabilities[1, ], c(0.023040094, 0.047419024, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results24$conditionalRejectionProbabilities[2, ], c(0.019613852, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results24$conditionalRejectionProbabilities[3, ], c(0.026303733, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results24$repeatedConfidenceIntervalLowerBounds[1, ], c(-11.74771, -4.6401767, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results24$repeatedConfidenceIntervalLowerBounds[2, ], c(-16.277631, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results24$repeatedConfidenceIntervalLowerBounds[3, ], c(-8.7851784, 0.25415362, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results24$repeatedConfidenceIntervalUpperBounds[1, ], c(41.74771, 27.425347, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results24$repeatedConfidenceIntervalUpperBounds[2, ], c(35.477631, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results24$repeatedConfidenceIntervalUpperBounds[3, ], c(43.785178, 31.856528, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results24$repeatedPValues[1, ], c(0.19020524, 0.071018123, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results24$repeatedPValues[2, ], c(0.23360401, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results24$repeatedPValues[3, ], c(0.16007682, 0.01742078, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results24$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results24$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.256, 0.433), tolerance = 1e-06, label = paste0("c(", paste0(results24$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results24$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results24$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results24$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results24$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results24), NA)))
        expect_output(print(results24)$show())
        invisible(capture.output(expect_error(summary(results24), NA)))
        expect_output(summary(results24)$show())
        results24CodeBased <- eval(parse(text = getObjectRCode(results24, stringWrapParagraphWidth = NULL)))
        expect_equal(results24CodeBased$thetaH1, results24$thetaH1, tolerance = 1e-06)
        expect_equal(results24CodeBased$assumedStDevs, results24$assumedStDevs, tolerance = 1e-06)
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
    results25 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Bonferroni", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results25' with expected results
    expect_equal(results25$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results25$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results25$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results25$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results25$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results25$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results25$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results25$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results25$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results25$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results25$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results25$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results25$conditionalRejectionProbabilities[1, ], c(0.024333354, 0.053095357, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results25$conditionalRejectionProbabilities[2, ], c(0.021915713, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results25$conditionalRejectionProbabilities[3, ], c(0.026248507, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results25$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.389181, -4.1091853, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results25$repeatedConfidenceIntervalLowerBounds[2, ], c(-13.124586, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results25$repeatedConfidenceIntervalLowerBounds[3, ], c(-8.2617152, 0.37246523, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results25$repeatedConfidenceIntervalUpperBounds[1, ], c(40.389181, 26.847363, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results25$repeatedConfidenceIntervalUpperBounds[2, ], c(32.324586, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results25$repeatedConfidenceIntervalUpperBounds[3, ], c(43.261715, 31.705217, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results25$repeatedPValues[1, ], c(0.17721241, 0.063189426, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results25$repeatedPValues[2, ], c(0.20285189, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results25$repeatedPValues[3, ], c(0.16051933, 0.01616384, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results25$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results25$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.274, 0.453), tolerance = 1e-06, label = paste0("c(", paste0(results25$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results25$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results25$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results25$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results25$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results25), NA)))
        expect_output(print(results25)$show())
        invisible(capture.output(expect_error(summary(results25), NA)))
        expect_output(summary(results25)$show())
        results25CodeBased <- eval(parse(text = getObjectRCode(results25, stringWrapParagraphWidth = NULL)))
        expect_equal(results25CodeBased$thetaH1, results25$thetaH1, tolerance = 1e-06)
        expect_equal(results25CodeBased$assumedStDevs, results25$assumedStDevs, tolerance = 1e-06)
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
    results26 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Bonferroni", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results26' with expected results
    expect_equal(results26$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results26$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results26$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results26$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results26$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results26$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results26$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results26$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results26$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results26$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results26$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results26$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results26$conditionalRejectionProbabilities[1, ], c(0.02248882, 0.047009108, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results26$conditionalRejectionProbabilities[2, ], c(0.021309255, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results26$conditionalRejectionProbabilities[3, ], c(0.023369532, 0.13794488, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results26$repeatedConfidenceIntervalLowerBounds[1, ], c(-12.972232, -4.7692163, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results26$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.236237, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results26$repeatedConfidenceIntervalLowerBounds[3, ], c(-10.763995, -0.22335705, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results26$repeatedConfidenceIntervalUpperBounds[1, ], c(42.972232, 27.481288, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results26$repeatedConfidenceIntervalUpperBounds[2, ], c(34.436237, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results26$repeatedConfidenceIntervalUpperBounds[3, ], c(45.763994, 32.295837, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results26$repeatedPValues[1, ], c(0.19623626, 0.071653269, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results26$repeatedPValues[2, ], c(0.21026955, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results26$repeatedPValues[3, ], c(0.18674722, 0.022408487, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results26$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results26$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.256, 0.431), tolerance = 1e-06, label = paste0("c(", paste0(results26$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results26$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results26$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results26$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.652, 0.795), tolerance = 1e-06, label = paste0("c(", paste0(results26$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results26), NA)))
        expect_output(print(results26)$show())
        invisible(capture.output(expect_error(summary(results26), NA)))
        expect_output(summary(results26)$show())
        results26CodeBased <- eval(parse(text = getObjectRCode(results26, stringWrapParagraphWidth = NULL)))
        expect_equal(results26CodeBased$thetaH1, results26$thetaH1, tolerance = 1e-06)
        expect_equal(results26CodeBased$assumedStDevs, results26$assumedStDevs, tolerance = 1e-06)
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
    results27 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results27' with expected results
    expect_equal(results27$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results27$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results27$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results27$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results27$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results27$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results27$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results27$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results27$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results27$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results27$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results27$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results27$conditionalRejectionProbabilities[1, ], c(0.024608533, 0.053964296, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results27$conditionalRejectionProbabilities[2, ], c(0.021871903, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results27$conditionalRejectionProbabilities[3, ], c(0.029595078, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results27$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.256183, -4.0576303, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results27$repeatedConfidenceIntervalLowerBounds[2, ], c(-13.161515, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results27$repeatedConfidenceIntervalLowerBounds[3, ], c(-7.9076686, 0.4326836, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results27$repeatedConfidenceIntervalUpperBounds[1, ], c(40.256183, 26.806923, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results27$repeatedConfidenceIntervalUpperBounds[2, ], c(32.361515, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results27$repeatedConfidenceIntervalUpperBounds[3, ], c(42.907669, 31.664999, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results27$repeatedPValues[1, ], c(0.17463845, 0.062131804, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results27$repeatedPValues[2, ], c(0.20337355, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results27$repeatedPValues[3, ], c(0.13700176, 0.014275569, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results27$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results27$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.277, 0.453), tolerance = 1e-06, label = paste0("c(", paste0(results27$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results27$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results27$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results27$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results27$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results27), NA)))
        expect_output(print(results27)$show())
        invisible(capture.output(expect_error(summary(results27), NA)))
        expect_output(summary(results27)$show())
        results27CodeBased <- eval(parse(text = getObjectRCode(results27, stringWrapParagraphWidth = NULL)))
        expect_equal(results27CodeBased$thetaH1, results27$thetaH1, tolerance = 1e-06)
        expect_equal(results27CodeBased$assumedStDevs, results27$assumedStDevs, tolerance = 1e-06)
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
    results28 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results28' with expected results
    expect_equal(results28$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results28$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results28$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results28$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results28$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results28$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results28$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results28$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results28$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results28$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results28$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results28$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results28$conditionalRejectionProbabilities[1, ], c(0.022711489, 0.047669561, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results28$conditionalRejectionProbabilities[2, ], c(0.021234311, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results28$conditionalRejectionProbabilities[3, ], c(0.027312859, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results28$repeatedConfidenceIntervalLowerBounds[1, ], c(-12.830851, -4.722817, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results28$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.416779, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results28$repeatedConfidenceIntervalLowerBounds[3, ], c(-10.376075, -0.16648466, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results28$repeatedConfidenceIntervalUpperBounds[1, ], c(42.830851, 27.447651, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results28$repeatedConfidenceIntervalUpperBounds[2, ], c(34.616779, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results28$repeatedConfidenceIntervalUpperBounds[3, ], c(45.376075, 32.257244, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results28$repeatedPValues[1, ], c(0.19376148, 0.070634747, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results28$repeatedPValues[2, ], c(0.2112175, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results28$repeatedPValues[3, ], c(0.15234731, 0.019097336, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results28$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results28$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.257, 0.434), tolerance = 1e-06, label = paste0("c(", paste0(results28$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results28$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results28$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results28$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results28$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results28), NA)))
        expect_output(print(results28)$show())
        invisible(capture.output(expect_error(summary(results28), NA)))
        expect_output(summary(results28)$show())
        results28CodeBased <- eval(parse(text = getObjectRCode(results28, stringWrapParagraphWidth = NULL)))
        expect_equal(results28CodeBased$thetaH1, results28$thetaH1, tolerance = 1e-06)
        expect_equal(results28CodeBased$assumedStDevs, results28$assumedStDevs, tolerance = 1e-06)
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
    results29 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Simes", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results29' with expected results
    expect_equal(results29$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results29$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results29$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results29$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results29$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results29$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results29$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results29$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results29$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results29$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results29$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results29$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results29$conditionalRejectionProbabilities[1, ], c(0.02389937, 0.050606752, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results29$conditionalRejectionProbabilities[2, ], c(0.019837849, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results29$conditionalRejectionProbabilities[3, ], c(0.028741907, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results29$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.605988, -4.2731734, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results29$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.173049, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results29$repeatedConfidenceIntervalLowerBounds[3, ], c(-7.6631999, 0.60791563, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results29$repeatedConfidenceIntervalUpperBounds[1, ], c(40.605988, 27.021858, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results29$repeatedConfidenceIntervalUpperBounds[2, ], c(34.373049, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results29$repeatedConfidenceIntervalUpperBounds[3, ], c(42.6632, 31.486561, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results29$repeatedPValues[1, ], c(0.18140284, 0.066412839, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results29$repeatedPValues[2, ], c(0.23027951, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results29$repeatedPValues[3, ], c(0.14242148, 0.013628025, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results29$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results29$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.268, 0.445), tolerance = 1e-06, label = paste0("c(", paste0(results29$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results29$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results29$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results29$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results29$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results29), NA)))
        expect_output(print(results29)$show())
        invisible(capture.output(expect_error(summary(results29), NA)))
        expect_output(summary(results29)$show())
        results29CodeBased <- eval(parse(text = getObjectRCode(results29, stringWrapParagraphWidth = NULL)))
        expect_equal(results29CodeBased$thetaH1, results29$thetaH1, tolerance = 1e-06)
        expect_equal(results29CodeBased$assumedStDevs, results29$assumedStDevs, tolerance = 1e-06)
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
    results30 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Simes", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results30' with expected results
    expect_equal(results30$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results30$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results30$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results30$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results30$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results30$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results30$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results30$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results30$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results30$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results30$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results30$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results30$conditionalRejectionProbabilities[1, ], c(0.023040094, 0.047419024, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results30$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results30$conditionalRejectionProbabilities[2, ], c(0.019613852, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results30$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results30$conditionalRejectionProbabilities[3, ], c(0.027708171, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results30$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results30$repeatedConfidenceIntervalLowerBounds[1, ], c(-11.74771, -4.6401767, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results30$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results30$repeatedConfidenceIntervalLowerBounds[2, ], c(-16.277631, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results30$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results30$repeatedConfidenceIntervalLowerBounds[3, ], c(-8.7851784, 0.25415362, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results30$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results30$repeatedConfidenceIntervalUpperBounds[1, ], c(41.74771, 27.425347, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results30$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results30$repeatedConfidenceIntervalUpperBounds[2, ], c(35.477631, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results30$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results30$repeatedConfidenceIntervalUpperBounds[3, ], c(43.785178, 31.856528, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results30$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results30$repeatedPValues[1, ], c(0.19020524, 0.071018123, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results30$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results30$repeatedPValues[2, ], c(0.23360401, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results30$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results30$repeatedPValues[3, ], c(0.1494882, 0.016474737, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results30$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results30$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.256, 0.433), tolerance = 1e-06, label = paste0("c(", paste0(results30$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results30$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results30$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results30$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results30$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results30), NA)))
        expect_output(print(results30)$show())
        invisible(capture.output(expect_error(summary(results30), NA)))
        expect_output(summary(results30)$show())
        results30CodeBased <- eval(parse(text = getObjectRCode(results30, stringWrapParagraphWidth = NULL)))
        expect_equal(results30CodeBased$thetaH1, results30$thetaH1, tolerance = 1e-06)
        expect_equal(results30CodeBased$assumedStDevs, results30$assumedStDevs, tolerance = 1e-06)
        expect_equal(results30CodeBased$conditionalRejectionProbabilities, results30$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results30CodeBased$repeatedConfidenceIntervalLowerBounds, results30$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results30CodeBased$repeatedConfidenceIntervalUpperBounds, results30$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results30CodeBased$repeatedPValues, results30$repeatedPValues, tolerance = 1e-06)
        expect_equal(results30CodeBased$conditionalPowerSimulated, results30$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results30), "character")
        df <- as.data.frame(results30)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results30)
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
    results31 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Simes", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results31' with expected results
    expect_equal(results31$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results31$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results31$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results31$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results31$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results31$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results31$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results31$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results31$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results31$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results31$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results31$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results31$conditionalRejectionProbabilities[1, ], c(0.024333354, 0.053095357, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results31$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results31$conditionalRejectionProbabilities[2, ], c(0.021915713, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results31$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results31$conditionalRejectionProbabilities[3, ], c(0.029264016, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results31$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results31$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.389181, -4.1091853, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results31$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results31$repeatedConfidenceIntervalLowerBounds[2, ], c(-13.124586, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results31$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results31$repeatedConfidenceIntervalLowerBounds[3, ], c(-8.2617152, 0.37246523, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results31$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results31$repeatedConfidenceIntervalUpperBounds[1, ], c(40.389181, 26.847363, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results31$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results31$repeatedConfidenceIntervalUpperBounds[2, ], c(32.324586, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results31$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results31$repeatedConfidenceIntervalUpperBounds[3, ], c(43.261715, 31.705217, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results31$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results31$repeatedPValues[1, ], c(0.17721241, 0.063189426, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results31$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results31$repeatedPValues[2, ], c(0.20285189, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results31$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results31$repeatedPValues[3, ], c(0.13906265, 0.014376658, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results31$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results31$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.274, 0.453), tolerance = 1e-06, label = paste0("c(", paste0(results31$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results31$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results31$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results31$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results31$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results31), NA)))
        expect_output(print(results31)$show())
        invisible(capture.output(expect_error(summary(results31), NA)))
        expect_output(summary(results31)$show())
        results31CodeBased <- eval(parse(text = getObjectRCode(results31, stringWrapParagraphWidth = NULL)))
        expect_equal(results31CodeBased$thetaH1, results31$thetaH1, tolerance = 1e-06)
        expect_equal(results31CodeBased$assumedStDevs, results31$assumedStDevs, tolerance = 1e-06)
        expect_equal(results31CodeBased$conditionalRejectionProbabilities, results31$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results31CodeBased$repeatedConfidenceIntervalLowerBounds, results31$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results31CodeBased$repeatedConfidenceIntervalUpperBounds, results31$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results31CodeBased$repeatedPValues, results31$repeatedPValues, tolerance = 1e-06)
        expect_equal(results31CodeBased$conditionalPowerSimulated, results31$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results31), "character")
        df <- as.data.frame(results31)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results31)
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
    results32 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Simes", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results32' with expected results
    expect_equal(results32$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results32$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results32$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results32$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results32$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results32$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results32$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results32$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results32$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results32$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results32$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results32$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results32$conditionalRejectionProbabilities[1, ], c(0.02248882, 0.047009108, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results32$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results32$conditionalRejectionProbabilities[2, ], c(0.021309255, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results32$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results32$conditionalRejectionProbabilities[3, ], c(0.027044989, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results32$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results32$repeatedConfidenceIntervalLowerBounds[1, ], c(-12.972232, -4.7692163, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results32$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results32$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.236237, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results32$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results32$repeatedConfidenceIntervalLowerBounds[3, ], c(-10.763995, -0.22335705, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results32$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results32$repeatedConfidenceIntervalUpperBounds[1, ], c(42.972232, 27.481288, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results32$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results32$repeatedConfidenceIntervalUpperBounds[2, ], c(34.436237, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results32$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results32$repeatedConfidenceIntervalUpperBounds[3, ], c(45.763994, 32.295837, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results32$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results32$repeatedPValues[1, ], c(0.19623626, 0.071653269, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results32$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results32$repeatedPValues[2, ], c(0.21026955, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results32$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results32$repeatedPValues[3, ], c(0.15433667, 0.019180306, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results32$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results32$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.256, 0.431), tolerance = 1e-06, label = paste0("c(", paste0(results32$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results32$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results32$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results32$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results32$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results32), NA)))
        expect_output(print(results32)$show())
        invisible(capture.output(expect_error(summary(results32), NA)))
        expect_output(summary(results32)$show())
        results32CodeBased <- eval(parse(text = getObjectRCode(results32, stringWrapParagraphWidth = NULL)))
        expect_equal(results32CodeBased$thetaH1, results32$thetaH1, tolerance = 1e-06)
        expect_equal(results32CodeBased$assumedStDevs, results32$assumedStDevs, tolerance = 1e-06)
        expect_equal(results32CodeBased$conditionalRejectionProbabilities, results32$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results32CodeBased$repeatedConfidenceIntervalLowerBounds, results32$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results32CodeBased$repeatedConfidenceIntervalUpperBounds, results32$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results32CodeBased$repeatedPValues, results32$repeatedPValues, tolerance = 1e-06)
        expect_equal(results32CodeBased$conditionalPowerSimulated, results32$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results32), "character")
        df <- as.data.frame(results32)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results32)
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
    results33 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Sidak", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results33' with expected results
    expect_equal(results33$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results33$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results33$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results33$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results33$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results33$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results33$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results33$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results33$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results33$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results33$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results33$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results33$conditionalRejectionProbabilities[1, ], c(0.025021019, 0.054834069, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results33$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results33$conditionalRejectionProbabilities[2, ], c(0.021871903, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results33$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results33$conditionalRejectionProbabilities[3, ], c(0.027777772, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results33$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results33$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.247199, -4.0258193, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results33$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results33$repeatedConfidenceIntervalLowerBounds[2, ], c(-13.153418, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results33$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results33$repeatedConfidenceIntervalLowerBounds[3, ], c(-7.8986307, 0.47811558, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results33$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results33$repeatedConfidenceIntervalUpperBounds[1, ], c(40.247199, 26.680539, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results33$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results33$repeatedConfidenceIntervalUpperBounds[2, ], c(32.353418, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results33$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results33$repeatedConfidenceIntervalUpperBounds[3, ], c(42.898631, 31.584065, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results33$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results33$repeatedPValues[1, ], c(0.17089623, 0.061105652, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results33$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results33$repeatedPValues[2, ], c(0.20337355, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results33$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results33$repeatedPValues[3, ], c(0.14899419, 0.015246407, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results33$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results33$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.283, 0.454), tolerance = 1e-06, label = paste0("c(", paste0(results33$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results33$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results33$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results33$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results33$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results33), NA)))
        expect_output(print(results33)$show())
        invisible(capture.output(expect_error(summary(results33), NA)))
        expect_output(summary(results33)$show())
        results33CodeBased <- eval(parse(text = getObjectRCode(results33, stringWrapParagraphWidth = NULL)))
        expect_equal(results33CodeBased$thetaH1, results33$thetaH1, tolerance = 1e-06)
        expect_equal(results33CodeBased$assumedStDevs, results33$assumedStDevs, tolerance = 1e-06)
        expect_equal(results33CodeBased$conditionalRejectionProbabilities, results33$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results33CodeBased$repeatedConfidenceIntervalLowerBounds, results33$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results33CodeBased$repeatedConfidenceIntervalUpperBounds, results33$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results33CodeBased$repeatedPValues, results33$repeatedPValues, tolerance = 1e-06)
        expect_equal(results33CodeBased$conditionalPowerSimulated, results33$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results33), "character")
        df <- as.data.frame(results33)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results33)
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
    results34 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Sidak", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results34' with expected results
    expect_equal(results34$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results34$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results34$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results34$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results34$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results34$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results34$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results34$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results34$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results34$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results34$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results34$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results34$conditionalRejectionProbabilities[1, ], c(0.023144095, 0.048545015, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results34$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results34$conditionalRejectionProbabilities[2, ], c(0.021234311, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results34$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results34$conditionalRejectionProbabilities[3, ], c(0.0247006, 0.1449328, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results34$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results34$repeatedConfidenceIntervalLowerBounds[1, ], c(-12.8192, -4.6852584, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results34$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results34$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.40635, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results34$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results34$repeatedConfidenceIntervalLowerBounds[3, ], c(-10.364486, -0.1144866, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results34$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results34$repeatedConfidenceIntervalUpperBounds[1, ], c(42.8192, 27.314543, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results34$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results34$repeatedConfidenceIntervalUpperBounds[2, ], c(34.60635, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results34$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results34$repeatedConfidenceIntervalUpperBounds[3, ], c(45.364486, 32.169333, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results34$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results34$repeatedPValues[1, ], c(0.18910184, 0.069324401, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results34$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results34$repeatedPValues[2, ], c(0.2112175, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results34$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results34$repeatedPValues[3, ], c(0.17379158, 0.021189694, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results34$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results34$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.26, 0.437), tolerance = 1e-06, label = paste0("c(", paste0(results34$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results34$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results34$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results34$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.66, 0.799), tolerance = 1e-06, label = paste0("c(", paste0(results34$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results34), NA)))
        expect_output(print(results34)$show())
        invisible(capture.output(expect_error(summary(results34), NA)))
        expect_output(summary(results34)$show())
        results34CodeBased <- eval(parse(text = getObjectRCode(results34, stringWrapParagraphWidth = NULL)))
        expect_equal(results34CodeBased$thetaH1, results34$thetaH1, tolerance = 1e-06)
        expect_equal(results34CodeBased$assumedStDevs, results34$assumedStDevs, tolerance = 1e-06)
        expect_equal(results34CodeBased$conditionalRejectionProbabilities, results34$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results34CodeBased$repeatedConfidenceIntervalLowerBounds, results34$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results34CodeBased$repeatedConfidenceIntervalUpperBounds, results34$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results34CodeBased$repeatedPValues, results34$repeatedPValues, tolerance = 1e-06)
        expect_equal(results34CodeBased$conditionalPowerSimulated, results34$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results34), "character")
        df <- as.data.frame(results34)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results34)
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
    results35 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Sidak", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results35' with expected results
    expect_equal(results35$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results35$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results35$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results35$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results35$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results35$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results35$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results35$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results35$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results35$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results35$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results35$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results35$conditionalRejectionProbabilities[1, ], c(0.024319059, 0.051462476, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results35$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results35$conditionalRejectionProbabilities[2, ], c(0.019837849, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results35$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results35$conditionalRejectionProbabilities[3, ], c(0.028516214, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results35$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results35$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.59688, -4.2407133, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results35$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results35$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.164237, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results35$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results35$repeatedConfidenceIntervalLowerBounds[3, ], c(-7.6542489, 0.6529301, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results35$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results35$repeatedConfidenceIntervalUpperBounds[1, ], c(40.59688, 26.894985, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results35$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results35$repeatedConfidenceIntervalUpperBounds[2, ], c(34.364237, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results35$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results35$repeatedConfidenceIntervalUpperBounds[3, ], c(42.654249, 31.405859, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results35$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results35$repeatedPValues[1, ], c(0.17734783, 0.06527034, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results35$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results35$repeatedPValues[2, ], c(0.23027951, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results35$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results35$repeatedPValues[3, ], c(0.14391589, 0.013711948, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results35$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results35$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.271, 0.447), tolerance = 1e-06, label = paste0("c(", paste0(results35$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results35$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results35$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results35$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results35$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results35), NA)))
        expect_output(print(results35)$show())
        invisible(capture.output(expect_error(summary(results35), NA)))
        expect_output(summary(results35)$show())
        results35CodeBased <- eval(parse(text = getObjectRCode(results35, stringWrapParagraphWidth = NULL)))
        expect_equal(results35CodeBased$thetaH1, results35$thetaH1, tolerance = 1e-06)
        expect_equal(results35CodeBased$assumedStDevs, results35$assumedStDevs, tolerance = 1e-06)
        expect_equal(results35CodeBased$conditionalRejectionProbabilities, results35$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results35CodeBased$repeatedConfidenceIntervalLowerBounds, results35$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results35CodeBased$repeatedConfidenceIntervalUpperBounds, results35$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results35CodeBased$repeatedPValues, results35$repeatedPValues, tolerance = 1e-06)
        expect_equal(results35CodeBased$conditionalPowerSimulated, results35$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results35), "character")
        df <- as.data.frame(results35)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results35)
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
    results36 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Sidak", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results36' with expected results
    expect_equal(results36$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results36$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results36$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results36$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results36$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results36$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results36$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results36$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results36$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results36$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results36$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results36$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results36$conditionalRejectionProbabilities[1, ], c(0.023469013, 0.048270226, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results36$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results36$conditionalRejectionProbabilities[2, ], c(0.019613852, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results36$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results36$conditionalRejectionProbabilities[3, ], c(0.026830382, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results36$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results36$repeatedConfidenceIntervalLowerBounds[1, ], c(-11.737451, -4.6050352, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results36$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results36$repeatedConfidenceIntervalLowerBounds[2, ], c(-16.267707, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results36$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results36$repeatedConfidenceIntervalLowerBounds[3, ], c(-8.7750975, 0.30217392, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results36$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results36$repeatedConfidenceIntervalUpperBounds[1, ], c(41.737451, 27.295819, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results36$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results36$repeatedConfidenceIntervalUpperBounds[2, ], c(35.467707, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results36$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results36$repeatedConfidenceIntervalUpperBounds[3, ], c(43.775098, 31.772829, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results36$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results36$repeatedPValues[1, ], c(0.18572393, 0.069730666, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results36$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results36$repeatedPValues[2, ], c(0.23360401, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results36$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results36$repeatedPValues[3, ], c(0.15596268, 0.017006886, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results36$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results36$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.26, 0.436), tolerance = 1e-06, label = paste0("c(", paste0(results36$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results36$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results36$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results36$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results36$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results36), NA)))
        expect_output(print(results36)$show())
        invisible(capture.output(expect_error(summary(results36), NA)))
        expect_output(summary(results36)$show())
        results36CodeBased <- eval(parse(text = getObjectRCode(results36, stringWrapParagraphWidth = NULL)))
        expect_equal(results36CodeBased$thetaH1, results36$thetaH1, tolerance = 1e-06)
        expect_equal(results36CodeBased$assumedStDevs, results36$assumedStDevs, tolerance = 1e-06)
        expect_equal(results36CodeBased$conditionalRejectionProbabilities, results36$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results36CodeBased$repeatedConfidenceIntervalLowerBounds, results36$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results36CodeBased$repeatedConfidenceIntervalUpperBounds, results36$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results36CodeBased$repeatedPValues, results36$repeatedPValues, tolerance = 1e-06)
        expect_equal(results36CodeBased$conditionalPowerSimulated, results36$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results36), "character")
        df <- as.data.frame(results36)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results36)
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
    results37 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Sidak", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results37' with expected results
    expect_equal(results37$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results37$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results37$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results37$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results37$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results37$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results37$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results37$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results37$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results37$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results37$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results37$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results37$conditionalRejectionProbabilities[1, ], c(0.024748593, 0.053966892, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results37$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results37$conditionalRejectionProbabilities[2, ], c(0.021915713, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results37$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results37$conditionalRejectionProbabilities[3, ], c(0.0267758, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results37$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results37$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.38015, -4.0770639, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results37$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results37$repeatedConfidenceIntervalLowerBounds[2, ], c(-13.116502, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results37$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results37$repeatedConfidenceIntervalLowerBounds[3, ], c(-8.2525514, 0.41959343, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results37$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results37$repeatedConfidenceIntervalUpperBounds[1, ], c(40.38015, 26.720108, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results37$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results37$repeatedConfidenceIntervalUpperBounds[2, ], c(32.316502, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results37$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results37$repeatedConfidenceIntervalUpperBounds[3, ], c(43.252551, 31.62149, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results37$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results37$repeatedPValues[1, ], c(0.17335289, 0.062127989, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results37$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results37$repeatedPValues[2, ], c(0.20285189, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results37$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results37$repeatedPValues[3, ], c(0.15638134, 0.015781417, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results37$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results37$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.277, 0.453), tolerance = 1e-06, label = paste0("c(", paste0(results37$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results37$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results37$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results37$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results37$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results37), NA)))
        expect_output(print(results37)$show())
        invisible(capture.output(expect_error(summary(results37), NA)))
        expect_output(summary(results37)$show())
        results37CodeBased <- eval(parse(text = getObjectRCode(results37, stringWrapParagraphWidth = NULL)))
        expect_equal(results37CodeBased$thetaH1, results37$thetaH1, tolerance = 1e-06)
        expect_equal(results37CodeBased$assumedStDevs, results37$assumedStDevs, tolerance = 1e-06)
        expect_equal(results37CodeBased$conditionalRejectionProbabilities, results37$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results37CodeBased$repeatedConfidenceIntervalLowerBounds, results37$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results37CodeBased$repeatedConfidenceIntervalUpperBounds, results37$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results37CodeBased$repeatedPValues, results37$repeatedPValues, tolerance = 1e-06)
        expect_equal(results37CodeBased$conditionalPowerSimulated, results37$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results37), "character")
        df <- as.data.frame(results37)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results37)
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
    results38 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Sidak", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results38' with expected results
    expect_equal(results38$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results38$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results38$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results38$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results38$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results38$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results38$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results38$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results38$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results38$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results38$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results38$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results38$conditionalRejectionProbabilities[1, ], c(0.022923976, 0.04788638, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results38$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results38$conditionalRejectionProbabilities[2, ], c(0.021309255, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results38$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results38$conditionalRejectionProbabilities[3, ], c(0.023933809, 0.14146912, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results38$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results38$repeatedConfidenceIntervalLowerBounds[1, ], c(-12.960526, -4.7313117, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results38$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results38$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.225975, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results38$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results38$repeatedConfidenceIntervalLowerBounds[3, ], c(-10.752245, -0.16953037, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results38$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results38$repeatedConfidenceIntervalUpperBounds[1, ], c(42.960526, 27.347242, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results38$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results38$repeatedConfidenceIntervalUpperBounds[2, ], c(34.425975, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results38$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results38$repeatedConfidenceIntervalUpperBounds[3, ], c(45.752245, 32.205007, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results38$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results38$repeatedPValues[1, ], c(0.19144883, 0.07030573, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results38$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results38$repeatedPValues[2, ], c(0.21026955, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results38$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results38$repeatedPValues[3, ], c(0.18106429, 0.021778109, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results38$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results38$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.259, 0.434), tolerance = 1e-06, label = paste0("c(", paste0(results38$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results38$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results38$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results38$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.656, 0.797), tolerance = 1e-06, label = paste0("c(", paste0(results38$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results38), NA)))
        expect_output(print(results38)$show())
        invisible(capture.output(expect_error(summary(results38), NA)))
        expect_output(summary(results38)$show())
        results38CodeBased <- eval(parse(text = getObjectRCode(results38, stringWrapParagraphWidth = NULL)))
        expect_equal(results38CodeBased$thetaH1, results38$thetaH1, tolerance = 1e-06)
        expect_equal(results38CodeBased$assumedStDevs, results38$assumedStDevs, tolerance = 1e-06)
        expect_equal(results38CodeBased$conditionalRejectionProbabilities, results38$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results38CodeBased$repeatedConfidenceIntervalLowerBounds, results38$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results38CodeBased$repeatedConfidenceIntervalUpperBounds, results38$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results38CodeBased$repeatedPValues, results38$repeatedPValues, tolerance = 1e-06)
        expect_equal(results38CodeBased$conditionalPowerSimulated, results38$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results38), "character")
        df <- as.data.frame(results38)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results38)
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
    results39 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Dunnett", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results39' with expected results
    expect_equal(results39$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results39$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results39$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results39$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results39$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results39$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results39$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results39$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results39$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results39$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results39$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results39$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results39$conditionalRejectionProbabilities[1, ], c(0.026270241, 0.055429536, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results39$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results39$conditionalRejectionProbabilities[2, ], c(0.019837849, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results39$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results39$conditionalRejectionProbabilities[3, ], c(0.032007473, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results39$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results39$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.308137, -3.9366921, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results39$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results39$repeatedConfidenceIntervalLowerBounds[2, ], c(-14.884887, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results39$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results39$repeatedConfidenceIntervalLowerBounds[3, ], c(-7.3704995, 0.96851041, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results39$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results39$repeatedConfidenceIntervalUpperBounds[1, ], c(40.308137, 26.527814, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results39$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results39$repeatedConfidenceIntervalUpperBounds[2, ], c(34.084887, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results39$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results39$repeatedConfidenceIntervalUpperBounds[3, ], c(42.370499, 31.063081, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results39$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results39$repeatedPValues[1, ], c(0.1603448, 0.060420915, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results39$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results39$repeatedPValues[2, ], c(0.23027951, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results39$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results39$repeatedPValues[3, ], c(0.12340907, 0.011635803, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results39$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results39$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.286, 0.457), tolerance = 1e-06, label = paste0("c(", paste0(results39$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results39$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results39$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results39$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results39$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results39), NA)))
        expect_output(print(results39)$show())
        invisible(capture.output(expect_error(summary(results39), NA)))
        expect_output(summary(results39)$show())
        results39CodeBased <- eval(parse(text = getObjectRCode(results39, stringWrapParagraphWidth = NULL)))
        expect_equal(results39CodeBased$thetaH1, results39$thetaH1, tolerance = 1e-06)
        expect_equal(results39CodeBased$assumedStDevs, results39$assumedStDevs, tolerance = 1e-06)
        expect_equal(results39CodeBased$conditionalRejectionProbabilities, results39$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results39CodeBased$repeatedConfidenceIntervalLowerBounds, results39$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results39CodeBased$repeatedConfidenceIntervalUpperBounds, results39$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results39CodeBased$repeatedPValues, results39$repeatedPValues, tolerance = 1e-06)
        expect_equal(results39CodeBased$conditionalPowerSimulated, results39$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results39), "character")
        df <- as.data.frame(results39)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results39)
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
    results40 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Dunnett", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results40' with expected results
    expect_equal(results40$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results40$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results40$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results40$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results40$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results40$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results40$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results40$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results40$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results40$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results40$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results40$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results40$conditionalRejectionProbabilities[1, ], c(0.025452912, 0.052195908, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results40$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results40$conditionalRejectionProbabilities[2, ], c(0.019613852, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results40$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results40$conditionalRejectionProbabilities[3, ], c(0.030394861, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results40$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results40$repeatedConfidenceIntervalLowerBounds[1, ], c(-11.358826, -4.2590391, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results40$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results40$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.901398, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results40$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results40$repeatedConfidenceIntervalLowerBounds[3, ], c(-8.4030195, 0.65705914, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results40$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results40$repeatedConfidenceIntervalUpperBounds[1, ], c(41.358826, 26.891429, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results40$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results40$repeatedConfidenceIntervalUpperBounds[2, ], c(35.101397, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results40$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results40$repeatedConfidenceIntervalUpperBounds[3, ], c(43.40302, 31.392896, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results40$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results40$repeatedPValues[1, ], c(0.16712065, 0.064319528, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results40$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results40$repeatedPValues[2, ], c(0.23360401, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results40$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results40$repeatedPValues[3, ], c(0.13222768, 0.014210719, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results40$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results40$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.272, 0.449), tolerance = 1e-06, label = paste0("c(", paste0(results40$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results40$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results40$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results40$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results40$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results40), NA)))
        expect_output(print(results40)$show())
        invisible(capture.output(expect_error(summary(results40), NA)))
        expect_output(summary(results40)$show())
        results40CodeBased <- eval(parse(text = getObjectRCode(results40, stringWrapParagraphWidth = NULL)))
        expect_equal(results40CodeBased$thetaH1, results40$thetaH1, tolerance = 1e-06)
        expect_equal(results40CodeBased$assumedStDevs, results40$assumedStDevs, tolerance = 1e-06)
        expect_equal(results40CodeBased$conditionalRejectionProbabilities, results40$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results40CodeBased$repeatedConfidenceIntervalLowerBounds, results40$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results40CodeBased$repeatedConfidenceIntervalUpperBounds, results40$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results40CodeBased$repeatedPValues, results40$repeatedPValues, tolerance = 1e-06)
        expect_equal(results40CodeBased$conditionalPowerSimulated, results40$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results40), "character")
        df <- as.data.frame(results40)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results40)
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
    results41 <- getAnalysisResults(
        design = design3, dataInput = dataExample1,
        intersectionTest = "Dunnett", varianceOption = "overallPooled", normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsConditionalDunnett object 'results41' with expected results
    expect_equal(results41$thetaH1[1, ], 11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results41$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results41$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results41$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results41$thetaH1[3, ], 16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results41$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results41$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results41$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results41$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results41$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results41$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results41$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results41$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.061352937), tolerance = 1e-06, label = paste0("c(", paste0(results41$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results41$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.03744742), tolerance = 1e-06, label = paste0("c(", paste0(results41$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results41$conditionalRejectionProbabilities[3, ], c(NA_real_, 0.086511764), tolerance = 1e-06, label = paste0("c(", paste0(results41$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results41$conditionalPower[1, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results41$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results41$conditionalPower[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results41$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results41$conditionalPower[3, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results41$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results41$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, -0.72441408), tolerance = 1e-06, label = paste0("c(", paste0(results41$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results41$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results41$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results41$repeatedConfidenceIntervalLowerBounds[3, ], c(NA_real_, 3.9389155), tolerance = 1e-06, label = paste0("c(", paste0(results41$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results41$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, 22.538727), tolerance = 1e-06, label = paste0("c(", paste0(results41$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results41$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results41$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results41$repeatedConfidenceIntervalUpperBounds[3, ], c(NA_real_, 26.753532), tolerance = 1e-06, label = paste0("c(", paste0(results41$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results41$repeatedPValues[1, ], c(NA_real_, 0.017445576), tolerance = 1e-06, label = paste0("c(", paste0(results41$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results41$repeatedPValues[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results41$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results41$repeatedPValues[3, ], c(NA_real_, 0.0019493527), tolerance = 1e-06, label = paste0("c(", paste0(results41$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results41), NA)))
        expect_output(print(results41)$show())
        invisible(capture.output(expect_error(summary(results41), NA)))
        expect_output(summary(results41)$show())
        results41CodeBased <- eval(parse(text = getObjectRCode(results41, stringWrapParagraphWidth = NULL)))
        expect_equal(results41CodeBased$thetaH1, results41$thetaH1, tolerance = 1e-06)
        expect_equal(results41CodeBased$assumedStDevs, results41$assumedStDevs, tolerance = 1e-06)
        expect_equal(results41CodeBased$conditionalRejectionProbabilities, results41$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results41CodeBased$conditionalPower, results41$conditionalPower, tolerance = 1e-06)
        expect_equal(results41CodeBased$repeatedConfidenceIntervalLowerBounds, results41$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results41CodeBased$repeatedConfidenceIntervalUpperBounds, results41$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results41CodeBased$repeatedPValues, results41$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results41), "character")
        df <- as.data.frame(results41)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results41)
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
    results42 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Bonferroni", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results42' with expected results
    expect_equal(results42$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results42$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results42$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results42$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results42$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results42$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results42$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results42$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results42$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results42$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results42$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results42$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results42$conditionalRejectionProbabilities[1, ], c(0.043739576, 0.16022367, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results42$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results42$conditionalRejectionProbabilities[2, ], c(0.03844608, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results42$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results42$conditionalRejectionProbabilities[3, ], c(0.048616927, 0.34001465, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results42$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results42$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45838936, 0.70196346), tolerance = 1e-06, label = paste0("c(", paste0(results42$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results42$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results42$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results42$conditionalPower[3, ], c(NA_real_, NA_real_, 0.827255, 0.9465652), tolerance = 1e-06, label = paste0("c(", paste0(results42$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results42$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.395028, -27.895908, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results42$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results42$repeatedConfidenceIntervalLowerBounds[2, ], c(-36.091548, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results42$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results42$repeatedConfidenceIntervalLowerBounds[3, ], c(-47.071339, -32.285152, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results42$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results42$repeatedConfidenceIntervalUpperBounds[1, ], c(14.395028, 4.0669228, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results42$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results42$repeatedConfidenceIntervalUpperBounds[2, ], c(16.891548, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results42$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results42$repeatedConfidenceIntervalUpperBounds[3, ], c(12.071338, -0.24153969, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results42$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results42$repeatedPValues[1, ], c(0.5, 0.072888275, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results42$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results42$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results42$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results42$repeatedPValues[3, ], c(0.5, 0.017155659, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results42$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results42), NA)))
        expect_output(print(results42)$show())
        invisible(capture.output(expect_error(summary(results42), NA)))
        expect_output(summary(results42)$show())
        results42CodeBased <- eval(parse(text = getObjectRCode(results42, stringWrapParagraphWidth = NULL)))
        expect_equal(results42CodeBased$thetaH1, results42$thetaH1, tolerance = 1e-06)
        expect_equal(results42CodeBased$assumedStDevs, results42$assumedStDevs, tolerance = 1e-06)
        expect_equal(results42CodeBased$conditionalRejectionProbabilities, results42$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results42CodeBased$conditionalPower, results42$conditionalPower, tolerance = 1e-06)
        expect_equal(results42CodeBased$repeatedConfidenceIntervalLowerBounds, results42$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results42CodeBased$repeatedConfidenceIntervalUpperBounds, results42$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results42CodeBased$repeatedPValues, results42$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results42), "character")
        df <- as.data.frame(results42)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results42)
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
    results43 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Bonferroni", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results43' with expected results
    expect_equal(results43$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results43$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results43$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results43$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results43$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results43$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results43$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results43$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results43$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results43$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results43$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results43$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results43$conditionalRejectionProbabilities[1, ], c(0.040100206, 0.14400686, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results43$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results43$conditionalRejectionProbabilities[2, ], c(0.037171319, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results43$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results43$conditionalRejectionProbabilities[3, ], c(0.042866371, 0.28890175, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results43$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results43$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42767622, 0.67947129), tolerance = 1e-06, label = paste0("c(", paste0(results43$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results43$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results43$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results43$conditionalPower[3, ], c(NA_real_, NA_real_, 0.78417464, 0.93070164), tolerance = 1e-06, label = paste0("c(", paste0(results43$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results43$repeatedConfidenceIntervalLowerBounds[1, ], c(-48.433726, -28.584393, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results43$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results43$repeatedConfidenceIntervalLowerBounds[2, ], c(-39.626743, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results43$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results43$repeatedConfidenceIntervalLowerBounds[3, ], c(-50.938808, -32.927366, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results43$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results43$repeatedConfidenceIntervalUpperBounds[1, ], c(18.433726, 4.7641755, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results43$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results43$repeatedConfidenceIntervalUpperBounds[2, ], c(20.426743, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results43$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results43$repeatedConfidenceIntervalUpperBounds[3, ], c(15.938808, 0.40329832, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results43$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results43$repeatedPValues[1, ], c(0.5, 0.085188742, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results43$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results43$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results43$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results43$repeatedPValues[3, ], c(0.5, 0.025112148, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results43$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results43), NA)))
        expect_output(print(results43)$show())
        invisible(capture.output(expect_error(summary(results43), NA)))
        expect_output(summary(results43)$show())
        results43CodeBased <- eval(parse(text = getObjectRCode(results43, stringWrapParagraphWidth = NULL)))
        expect_equal(results43CodeBased$thetaH1, results43$thetaH1, tolerance = 1e-06)
        expect_equal(results43CodeBased$assumedStDevs, results43$assumedStDevs, tolerance = 1e-06)
        expect_equal(results43CodeBased$conditionalRejectionProbabilities, results43$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results43CodeBased$conditionalPower, results43$conditionalPower, tolerance = 1e-06)
        expect_equal(results43CodeBased$repeatedConfidenceIntervalLowerBounds, results43$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results43CodeBased$repeatedConfidenceIntervalUpperBounds, results43$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results43CodeBased$repeatedPValues, results43$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results43), "character")
        df <- as.data.frame(results43)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results43)
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
    results44 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Bonferroni", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results44' with expected results
    expect_equal(results44$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results44$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results44$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results44$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results44$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results44$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results44$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results44$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results44$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results44$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results44$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results44$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results44$conditionalRejectionProbabilities[1, ], c(0.042394596, 0.15198143, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results44$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results44$conditionalRejectionProbabilities[2, ], c(0.034321105, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results44$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results44$conditionalRejectionProbabilities[3, ], c(0.049947129, 0.35588618, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results44$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results44$conditionalPower[1, ], c(NA_real_, NA_real_, 0.44302928, 0.69082025), tolerance = 1e-06, label = paste0("c(", paste0(results44$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results44$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results44$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results44$conditionalPower[3, ], c(NA_real_, NA_real_, 0.83889182, 0.95069292), tolerance = 1e-06, label = paste0("c(", paste0(results44$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results44$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.802158, -28.113845, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results44$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results44$repeatedConfidenceIntervalLowerBounds[2, ], c(-38.432721, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results44$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results44$repeatedConfidenceIntervalLowerBounds[3, ], c(-46.786808, -32.10754, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results44$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results44$repeatedConfidenceIntervalUpperBounds[1, ], c(14.802158, 4.2854677, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results44$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results44$repeatedConfidenceIntervalUpperBounds[2, ], c(19.232721, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results44$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results44$repeatedConfidenceIntervalUpperBounds[3, ], c(11.786808, -0.41764226, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results44$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results44$repeatedPValues[1, ], c(0.5, 0.078823932, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results44$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results44$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results44$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results44$repeatedPValues[3, ], c(0.5, 0.015272156, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results44$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results44), NA)))
        expect_output(print(results44)$show())
        invisible(capture.output(expect_error(summary(results44), NA)))
        expect_output(summary(results44)$show())
        results44CodeBased <- eval(parse(text = getObjectRCode(results44, stringWrapParagraphWidth = NULL)))
        expect_equal(results44CodeBased$thetaH1, results44$thetaH1, tolerance = 1e-06)
        expect_equal(results44CodeBased$assumedStDevs, results44$assumedStDevs, tolerance = 1e-06)
        expect_equal(results44CodeBased$conditionalRejectionProbabilities, results44$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results44CodeBased$conditionalPower, results44$conditionalPower, tolerance = 1e-06)
        expect_equal(results44CodeBased$repeatedConfidenceIntervalLowerBounds, results44$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results44CodeBased$repeatedConfidenceIntervalUpperBounds, results44$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results44CodeBased$repeatedPValues, results44$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results44), "character")
        df <- as.data.frame(results44)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results44)
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
    results45 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Bonferroni", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results45' with expected results
    expect_equal(results45$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results45$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results45$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results45$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results45$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results45$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results45$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results45$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results45$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results45$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results45$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results45$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results45$conditionalRejectionProbabilities[1, ], c(0.040740209, 0.14372404, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results45$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results45$conditionalRejectionProbabilities[2, ], c(0.033856262, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results45$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results45$conditionalRejectionProbabilities[3, ], c(0.046882975, 0.32321322, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results45$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results45$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42712247, 0.6790579), tolerance = 1e-06, label = paste0("c(", paste0(results45$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results45$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results45$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results45$conditionalPower[3, ], c(NA_real_, NA_real_, 0.81409137, 0.94181531), tolerance = 1e-06, label = paste0("c(", paste0(results45$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results45$repeatedConfidenceIntervalLowerBounds[1, ], c(-46.567569, -28.528695, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results45$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results45$repeatedConfidenceIntervalLowerBounds[2, ], c(-40.140706, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results45$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results45$repeatedConfidenceIntervalLowerBounds[3, ], c(-48.521691, -32.491814, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results45$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results45$repeatedConfidenceIntervalUpperBounds[1, ], c(16.567569, 4.662798, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results45$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results45$repeatedConfidenceIntervalUpperBounds[2, ], c(20.940706, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results45$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results45$repeatedConfidenceIntervalUpperBounds[3, ], c(13.521691, -0.049006969, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results45$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results45$repeatedPValues[1, ], c(0.5, 0.08542716, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results45$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results45$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results45$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results45$repeatedPValues[3, ], c(0.5, 0.019420631, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results45$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results45), NA)))
        expect_output(print(results45)$show())
        invisible(capture.output(expect_error(summary(results45), NA)))
        expect_output(summary(results45)$show())
        results45CodeBased <- eval(parse(text = getObjectRCode(results45, stringWrapParagraphWidth = NULL)))
        expect_equal(results45CodeBased$thetaH1, results45$thetaH1, tolerance = 1e-06)
        expect_equal(results45CodeBased$assumedStDevs, results45$assumedStDevs, tolerance = 1e-06)
        expect_equal(results45CodeBased$conditionalRejectionProbabilities, results45$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results45CodeBased$conditionalPower, results45$conditionalPower, tolerance = 1e-06)
        expect_equal(results45CodeBased$repeatedConfidenceIntervalLowerBounds, results45$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results45CodeBased$repeatedConfidenceIntervalUpperBounds, results45$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results45CodeBased$repeatedPValues, results45$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results45), "character")
        df <- as.data.frame(results45)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results45)
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
    results46 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Bonferroni", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results46' with expected results
    expect_equal(results46$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results46$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results46$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results46$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results46$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results46$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results46$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results46$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results46$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results46$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results46$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results46$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results46$conditionalRejectionProbabilities[1, ], c(0.043219831, 0.15803856, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results46$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results46$conditionalRejectionProbabilities[2, ], c(0.038533075, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results46$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results46$conditionalRejectionProbabilities[3, ], c(0.046782116, 0.33290332, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results46$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results46$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45436587, 0.6990644), tolerance = 1e-06, label = paste0("c(", paste0(results46$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results46$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results46$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results46$conditionalPower[3, ], c(NA_real_, NA_real_, 0.8217936, 0.94460493), tolerance = 1e-06, label = paste0("c(", paste0(results46$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results46$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.549821, -27.945069, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results46$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results46$repeatedConfidenceIntervalLowerBounds[2, ], c(-36.048567, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results46$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results46$repeatedConfidenceIntervalLowerBounds[3, ], c(-47.483405, -32.356999, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results46$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results46$repeatedConfidenceIntervalUpperBounds[1, ], c(14.549821, 4.1213996, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results46$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results46$repeatedConfidenceIntervalUpperBounds[2, ], c(16.848567, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results46$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results46$repeatedConfidenceIntervalUpperBounds[3, ], c(12.483405, -0.16013976, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results46$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results46$repeatedPValues[1, ], c(0.5, 0.07440366, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results46$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results46$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results46$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results46$repeatedPValues[3, ], c(0.5, 0.018077861, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results46$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results46), NA)))
        expect_output(print(results46)$show())
        invisible(capture.output(expect_error(summary(results46), NA)))
        expect_output(summary(results46)$show())
        results46CodeBased <- eval(parse(text = getObjectRCode(results46, stringWrapParagraphWidth = NULL)))
        expect_equal(results46CodeBased$thetaH1, results46$thetaH1, tolerance = 1e-06)
        expect_equal(results46CodeBased$assumedStDevs, results46$assumedStDevs, tolerance = 1e-06)
        expect_equal(results46CodeBased$conditionalRejectionProbabilities, results46$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results46CodeBased$conditionalPower, results46$conditionalPower, tolerance = 1e-06)
        expect_equal(results46CodeBased$repeatedConfidenceIntervalLowerBounds, results46$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results46CodeBased$repeatedConfidenceIntervalUpperBounds, results46$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results46CodeBased$repeatedPValues, results46$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results46), "character")
        df <- as.data.frame(results46)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results46)
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
    results47 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Bonferroni", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results47' with expected results
    expect_equal(results47$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results47$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results47$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results47$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results47$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results47$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results47$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results47$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results47$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results47$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results47$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results47$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results47$conditionalRejectionProbabilities[1, ], c(0.039664178, 0.14221619, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results47$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results47$conditionalRejectionProbabilities[2, ], c(0.037322005, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results47$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results47$conditionalRejectionProbabilities[3, ], c(0.041377736, 0.28315003, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results47$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results47$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42415922, 0.67684078), tolerance = 1e-06, label = paste0("c(", paste0(results47$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results47$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results47$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results47$conditionalPower[3, ], c(NA_real_, NA_real_, 0.77871789, 0.92862656), tolerance = 1e-06, label = paste0("c(", paste0(results47$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results47$repeatedConfidenceIntervalLowerBounds[1, ], c(-48.601467, -28.627869, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results47$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results47$repeatedConfidenceIntervalLowerBounds[2, ], c(-39.353637, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results47$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results47$repeatedConfidenceIntervalLowerBounds[3, ], c(-51.403927, -32.999307, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results47$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results47$repeatedConfidenceIntervalUpperBounds[1, ], c(18.601467, 4.8144337, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results47$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results47$repeatedConfidenceIntervalUpperBounds[2, ], c(20.153637, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results47$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results47$repeatedConfidenceIntervalUpperBounds[3, ], c(16.403927, 0.48327212, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results47$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results47$repeatedPValues[1, ], c(0.5, 0.086711756, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results47$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results47$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results47$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results47$repeatedPValues[3, ], c(0.5, 0.026234621, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results47$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results47), NA)))
        expect_output(print(results47)$show())
        invisible(capture.output(expect_error(summary(results47), NA)))
        expect_output(summary(results47)$show())
        results47CodeBased <- eval(parse(text = getObjectRCode(results47, stringWrapParagraphWidth = NULL)))
        expect_equal(results47CodeBased$thetaH1, results47$thetaH1, tolerance = 1e-06)
        expect_equal(results47CodeBased$assumedStDevs, results47$assumedStDevs, tolerance = 1e-06)
        expect_equal(results47CodeBased$conditionalRejectionProbabilities, results47$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results47CodeBased$conditionalPower, results47$conditionalPower, tolerance = 1e-06)
        expect_equal(results47CodeBased$repeatedConfidenceIntervalLowerBounds, results47$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results47CodeBased$repeatedConfidenceIntervalUpperBounds, results47$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results47CodeBased$repeatedPValues, results47$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results47), "character")
        df <- as.data.frame(results47)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results47)
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
    results48 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results48' with expected results
    expect_equal(results48$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results48$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results48$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results48$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results48$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results48$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results48$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results48$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results48$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results48$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results48$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results48$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results48$conditionalRejectionProbabilities[1, ], c(0.043739576, 0.16022367, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results48$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results48$conditionalRejectionProbabilities[2, ], c(0.03844608, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results48$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results48$conditionalRejectionProbabilities[3, ], c(0.052717287, 0.35672949, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results48$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results48$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45838936, 0.70196346), tolerance = 1e-06, label = paste0("c(", paste0(results48$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results48$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results48$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results48$conditionalPower[3, ], c(NA_real_, NA_real_, 0.83948961, 0.95090316), tolerance = 1e-06, label = paste0("c(", paste0(results48$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results48$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.395028, -27.895908, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results48$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results48$repeatedConfidenceIntervalLowerBounds[2, ], c(-36.091548, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results48$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results48$repeatedConfidenceIntervalLowerBounds[3, ], c(-47.071339, -32.285152, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results48$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results48$repeatedConfidenceIntervalUpperBounds[1, ], c(14.395028, 4.0669228, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results48$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results48$repeatedConfidenceIntervalUpperBounds[2, ], c(16.891548, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results48$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results48$repeatedConfidenceIntervalUpperBounds[3, ], c(12.071338, -0.24153969, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results48$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results48$repeatedPValues[1, ], c(0.5, 0.072888275, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results48$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results48$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results48$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results48$repeatedPValues[3, ], c(0.5, 0.015177743, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results48$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results48), NA)))
        expect_output(print(results48)$show())
        invisible(capture.output(expect_error(summary(results48), NA)))
        expect_output(summary(results48)$show())
        results48CodeBased <- eval(parse(text = getObjectRCode(results48, stringWrapParagraphWidth = NULL)))
        expect_equal(results48CodeBased$thetaH1, results48$thetaH1, tolerance = 1e-06)
        expect_equal(results48CodeBased$assumedStDevs, results48$assumedStDevs, tolerance = 1e-06)
        expect_equal(results48CodeBased$conditionalRejectionProbabilities, results48$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results48CodeBased$conditionalPower, results48$conditionalPower, tolerance = 1e-06)
        expect_equal(results48CodeBased$repeatedConfidenceIntervalLowerBounds, results48$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results48CodeBased$repeatedConfidenceIntervalUpperBounds, results48$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results48CodeBased$repeatedPValues, results48$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results48), "character")
        df <- as.data.frame(results48)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results48)
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
    results49 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results49' with expected results
    expect_equal(results49$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results49$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results49$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results49$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results49$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results49$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results49$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results49$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results49$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results49$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results49$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results49$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results49$conditionalRejectionProbabilities[1, ], c(0.040100206, 0.14400686, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results49$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results49$conditionalRejectionProbabilities[2, ], c(0.037171319, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results49$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results49$conditionalRejectionProbabilities[3, ], c(0.048708233, 0.3133215, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results49$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results49$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42767622, 0.67947129), tolerance = 1e-06, label = paste0("c(", paste0(results49$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results49$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results49$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results49$conditionalPower[3, ], c(NA_real_, NA_real_, 0.80590445, 0.93881804), tolerance = 1e-06, label = paste0("c(", paste0(results49$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results49$repeatedConfidenceIntervalLowerBounds[1, ], c(-48.433726, -28.584393, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results49$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results49$repeatedConfidenceIntervalLowerBounds[2, ], c(-39.626743, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results49$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results49$repeatedConfidenceIntervalLowerBounds[3, ], c(-50.938808, -32.927366, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results49$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results49$repeatedConfidenceIntervalUpperBounds[1, ], c(18.433726, 4.7641755, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results49$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results49$repeatedConfidenceIntervalUpperBounds[2, ], c(20.426743, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results49$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results49$repeatedConfidenceIntervalUpperBounds[3, ], c(15.938808, 0.40329832, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results49$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results49$repeatedPValues[1, ], c(0.5, 0.085188742, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results49$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results49$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results49$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results49$repeatedPValues[3, ], c(0.5, 0.020901685, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results49$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results49), NA)))
        expect_output(print(results49)$show())
        invisible(capture.output(expect_error(summary(results49), NA)))
        expect_output(summary(results49)$show())
        results49CodeBased <- eval(parse(text = getObjectRCode(results49, stringWrapParagraphWidth = NULL)))
        expect_equal(results49CodeBased$thetaH1, results49$thetaH1, tolerance = 1e-06)
        expect_equal(results49CodeBased$assumedStDevs, results49$assumedStDevs, tolerance = 1e-06)
        expect_equal(results49CodeBased$conditionalRejectionProbabilities, results49$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results49CodeBased$conditionalPower, results49$conditionalPower, tolerance = 1e-06)
        expect_equal(results49CodeBased$repeatedConfidenceIntervalLowerBounds, results49$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results49CodeBased$repeatedConfidenceIntervalUpperBounds, results49$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results49CodeBased$repeatedPValues, results49$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results49), "character")
        df <- as.data.frame(results49)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results49)
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
    results50 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Simes", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results50' with expected results
    expect_equal(results50$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results50$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results50$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results50$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results50$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results50$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results50$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results50$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results50$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results50$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results50$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results50$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results50$conditionalRejectionProbabilities[1, ], c(0.042394596, 0.15198143, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results50$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results50$conditionalRejectionProbabilities[2, ], c(0.034321105, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results50$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results50$conditionalRejectionProbabilities[3, ], c(0.051237296, 0.36121246, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results50$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results50$conditionalPower[1, ], c(NA_real_, NA_real_, 0.44302928, 0.69082025), tolerance = 1e-06, label = paste0("c(", paste0(results50$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results50$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results50$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results50$conditionalPower[3, ], c(NA_real_, NA_real_, 0.84263383, 0.95200602), tolerance = 1e-06, label = paste0("c(", paste0(results50$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results50$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.802158, -28.113845, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results50$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results50$repeatedConfidenceIntervalLowerBounds[2, ], c(-38.432721, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results50$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results50$repeatedConfidenceIntervalLowerBounds[3, ], c(-46.786808, -32.10754, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results50$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results50$repeatedConfidenceIntervalUpperBounds[1, ], c(14.802158, 4.2854677, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results50$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results50$repeatedConfidenceIntervalUpperBounds[2, ], c(19.232721, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results50$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results50$repeatedConfidenceIntervalUpperBounds[3, ], c(11.786808, -0.41764226, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results50$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results50$repeatedPValues[1, ], c(0.5, 0.078823932, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results50$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results50$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results50$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results50$repeatedPValues[3, ], c(0.5, 0.014689462, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results50$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results50), NA)))
        expect_output(print(results50)$show())
        invisible(capture.output(expect_error(summary(results50), NA)))
        expect_output(summary(results50)$show())
        results50CodeBased <- eval(parse(text = getObjectRCode(results50, stringWrapParagraphWidth = NULL)))
        expect_equal(results50CodeBased$thetaH1, results50$thetaH1, tolerance = 1e-06)
        expect_equal(results50CodeBased$assumedStDevs, results50$assumedStDevs, tolerance = 1e-06)
        expect_equal(results50CodeBased$conditionalRejectionProbabilities, results50$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results50CodeBased$conditionalPower, results50$conditionalPower, tolerance = 1e-06)
        expect_equal(results50CodeBased$repeatedConfidenceIntervalLowerBounds, results50$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results50CodeBased$repeatedConfidenceIntervalUpperBounds, results50$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results50CodeBased$repeatedPValues, results50$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results50), "character")
        df <- as.data.frame(results50)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results50)
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
    results51 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Simes", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results51' with expected results
    expect_equal(results51$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results51$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results51$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results51$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results51$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results51$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results51$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results51$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results51$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results51$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results51$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results51$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results51$conditionalRejectionProbabilities[1, ], c(0.040740209, 0.14372404, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results51$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results51$conditionalRejectionProbabilities[2, ], c(0.033856262, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results51$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results51$conditionalRejectionProbabilities[3, ], c(0.049414261, 0.33374326, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results51$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results51$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42712247, 0.6790579), tolerance = 1e-06, label = paste0("c(", paste0(results51$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results51$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results51$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results51$conditionalPower[3, ], c(NA_real_, NA_real_, 0.82244694, 0.94484021), tolerance = 1e-06, label = paste0("c(", paste0(results51$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results51$repeatedConfidenceIntervalLowerBounds[1, ], c(-46.567569, -28.528695, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results51$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results51$repeatedConfidenceIntervalLowerBounds[2, ], c(-40.140706, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results51$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results51$repeatedConfidenceIntervalLowerBounds[3, ], c(-48.521691, -32.491814, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results51$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results51$repeatedConfidenceIntervalUpperBounds[1, ], c(16.567569, 4.662798, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results51$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results51$repeatedConfidenceIntervalUpperBounds[2, ], c(20.940706, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results51$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results51$repeatedConfidenceIntervalUpperBounds[3, ], c(13.521691, -0.049006969, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results51$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results51$repeatedPValues[1, ], c(0.5, 0.08542716, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results51$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results51$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results51$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results51$repeatedPValues[3, ], c(0.5, 0.017966281, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results51$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results51), NA)))
        expect_output(print(results51)$show())
        invisible(capture.output(expect_error(summary(results51), NA)))
        expect_output(summary(results51)$show())
        results51CodeBased <- eval(parse(text = getObjectRCode(results51, stringWrapParagraphWidth = NULL)))
        expect_equal(results51CodeBased$thetaH1, results51$thetaH1, tolerance = 1e-06)
        expect_equal(results51CodeBased$assumedStDevs, results51$assumedStDevs, tolerance = 1e-06)
        expect_equal(results51CodeBased$conditionalRejectionProbabilities, results51$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results51CodeBased$conditionalPower, results51$conditionalPower, tolerance = 1e-06)
        expect_equal(results51CodeBased$repeatedConfidenceIntervalLowerBounds, results51$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results51CodeBased$repeatedConfidenceIntervalUpperBounds, results51$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results51CodeBased$repeatedPValues, results51$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results51), "character")
        df <- as.data.frame(results51)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results51)
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
    results52 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Simes", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results52' with expected results
    expect_equal(results52$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results52$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results52$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results52$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results52$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results52$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results52$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results52$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results52$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results52$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results52$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results52$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results52$conditionalRejectionProbabilities[1, ], c(0.043219831, 0.15803856, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results52$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results52$conditionalRejectionProbabilities[2, ], c(0.038533075, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results52$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results52$conditionalRejectionProbabilities[3, ], c(0.052145589, 0.35513472, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results52$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results52$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45436587, 0.6990644), tolerance = 1e-06, label = paste0("c(", paste0(results52$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results52$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results52$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results52$conditionalPower[3, ], c(NA_real_, NA_real_, 0.83835744, 0.95050484), tolerance = 1e-06, label = paste0("c(", paste0(results52$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results52$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.549821, -27.945069, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results52$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results52$repeatedConfidenceIntervalLowerBounds[2, ], c(-36.048567, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results52$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results52$repeatedConfidenceIntervalLowerBounds[3, ], c(-47.483405, -32.356999, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results52$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results52$repeatedConfidenceIntervalUpperBounds[1, ], c(14.549821, 4.1213996, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results52$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results52$repeatedConfidenceIntervalUpperBounds[2, ], c(16.848567, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results52$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results52$repeatedConfidenceIntervalUpperBounds[3, ], c(12.483405, -0.16013976, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results52$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results52$repeatedPValues[1, ], c(0.5, 0.07440366, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results52$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results52$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results52$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results52$repeatedPValues[3, ], c(0.5, 0.015356079, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results52$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results52), NA)))
        expect_output(print(results52)$show())
        invisible(capture.output(expect_error(summary(results52), NA)))
        expect_output(summary(results52)$show())
        results52CodeBased <- eval(parse(text = getObjectRCode(results52, stringWrapParagraphWidth = NULL)))
        expect_equal(results52CodeBased$thetaH1, results52$thetaH1, tolerance = 1e-06)
        expect_equal(results52CodeBased$assumedStDevs, results52$assumedStDevs, tolerance = 1e-06)
        expect_equal(results52CodeBased$conditionalRejectionProbabilities, results52$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results52CodeBased$conditionalPower, results52$conditionalPower, tolerance = 1e-06)
        expect_equal(results52CodeBased$repeatedConfidenceIntervalLowerBounds, results52$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results52CodeBased$repeatedConfidenceIntervalUpperBounds, results52$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results52CodeBased$repeatedPValues, results52$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results52), "character")
        df <- as.data.frame(results52)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results52)
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
    results53 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Simes", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results53' with expected results
    expect_equal(results53$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results53$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results53$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results53$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results53$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results53$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results53$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results53$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results53$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results53$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results53$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results53$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results53$conditionalRejectionProbabilities[1, ], c(0.039664178, 0.14221619, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results53$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results53$conditionalRejectionProbabilities[2, ], c(0.037322005, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results53$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results53$conditionalRejectionProbabilities[3, ], c(0.048226966, 0.31219358, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results53$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results53$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42415922, 0.67684078), tolerance = 1e-06, label = paste0("c(", paste0(results53$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results53$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results53$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results53$conditionalPower[3, ], c(NA_real_, NA_real_, 0.80494934, 0.93846621), tolerance = 1e-06, label = paste0("c(", paste0(results53$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results53$repeatedConfidenceIntervalLowerBounds[1, ], c(-48.601467, -28.627869, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results53$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results53$repeatedConfidenceIntervalLowerBounds[2, ], c(-39.353637, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results53$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results53$repeatedConfidenceIntervalLowerBounds[3, ], c(-51.403927, -32.999307, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results53$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results53$repeatedConfidenceIntervalUpperBounds[1, ], c(18.601467, 4.8144337, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results53$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results53$repeatedConfidenceIntervalUpperBounds[2, ], c(20.153637, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results53$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results53$repeatedConfidenceIntervalUpperBounds[3, ], c(16.403927, 0.48327212, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results53$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results53$repeatedPValues[1, ], c(0.5, 0.086711756, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results53$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results53$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results53$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results53$repeatedPValues[3, ], c(0.5, 0.021078114, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results53$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results53), NA)))
        expect_output(print(results53)$show())
        invisible(capture.output(expect_error(summary(results53), NA)))
        expect_output(summary(results53)$show())
        results53CodeBased <- eval(parse(text = getObjectRCode(results53, stringWrapParagraphWidth = NULL)))
        expect_equal(results53CodeBased$thetaH1, results53$thetaH1, tolerance = 1e-06)
        expect_equal(results53CodeBased$assumedStDevs, results53$assumedStDevs, tolerance = 1e-06)
        expect_equal(results53CodeBased$conditionalRejectionProbabilities, results53$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results53CodeBased$conditionalPower, results53$conditionalPower, tolerance = 1e-06)
        expect_equal(results53CodeBased$repeatedConfidenceIntervalLowerBounds, results53$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results53CodeBased$repeatedConfidenceIntervalUpperBounds, results53$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results53CodeBased$repeatedPValues, results53$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results53), "character")
        df <- as.data.frame(results53)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results53)
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
    results54 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Sidak", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results54' with expected results
    expect_equal(results54$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results54$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results54$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results54$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results54$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results54$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results54$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results54$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results54$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results54$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results54$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results54$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results54$conditionalRejectionProbabilities[1, ], c(0.044513617, 0.16250147, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results54$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results54$conditionalRejectionProbabilities[2, ], c(0.03844608, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results54$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results54$conditionalRejectionProbabilities[3, ], c(0.049538053, 0.34419132, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results54$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results54$conditionalPower[1, ], c(NA_real_, NA_real_, 0.46254707, 0.70494473), tolerance = 1e-06, label = paste0("c(", paste0(results54$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results54$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results54$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results54$conditionalPower[3, ], c(NA_real_, NA_real_, 0.83038985, 0.94768376), tolerance = 1e-06, label = paste0("c(", paste0(results54$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results54$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.393216, -27.725836, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results54$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results54$repeatedConfidenceIntervalLowerBounds[2, ], c(-36.089915, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results54$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results54$repeatedConfidenceIntervalLowerBounds[3, ], c(-47.069516, -32.182569, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results54$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results54$repeatedConfidenceIntervalUpperBounds[1, ], c(14.393216, 4.0328452, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results54$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results54$repeatedConfidenceIntervalUpperBounds[2, ], c(16.889915, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results54$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results54$repeatedConfidenceIntervalUpperBounds[3, ], c(12.069516, -0.29402607, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results54$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results54$repeatedPValues[1, ], c(0.5, 0.071351909, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results54$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results54$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results54$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results54$repeatedPValues[3, ], c(0.5, 0.016637815, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results54$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results54), NA)))
        expect_output(print(results54)$show())
        invisible(capture.output(expect_error(summary(results54), NA)))
        expect_output(summary(results54)$show())
        results54CodeBased <- eval(parse(text = getObjectRCode(results54, stringWrapParagraphWidth = NULL)))
        expect_equal(results54CodeBased$thetaH1, results54$thetaH1, tolerance = 1e-06)
        expect_equal(results54CodeBased$assumedStDevs, results54$assumedStDevs, tolerance = 1e-06)
        expect_equal(results54CodeBased$conditionalRejectionProbabilities, results54$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results54CodeBased$conditionalPower, results54$conditionalPower, tolerance = 1e-06)
        expect_equal(results54CodeBased$repeatedConfidenceIntervalLowerBounds, results54$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results54CodeBased$repeatedConfidenceIntervalUpperBounds, results54$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results54CodeBased$repeatedPValues, results54$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results54), "character")
        df <- as.data.frame(results54)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results54)
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
    results55 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Sidak", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results55' with expected results
    expect_equal(results55$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results55$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results55$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results55$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results55$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results55$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results55$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results55$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results55$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results55$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results55$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results55$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results55$conditionalRejectionProbabilities[1, ], c(0.040941914, 0.14648989, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results55$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results55$conditionalRejectionProbabilities[2, ], c(0.037171319, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results55$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results55$conditionalRejectionProbabilities[3, ], c(0.043912863, 0.29382832, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results55$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results55$conditionalPower[1, ], c(NA_real_, NA_real_, 0.4325103, 0.68306799), tolerance = 1e-06, label = paste0("c(", paste0(results55$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results55$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results55$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results55$conditionalPower[3, ], c(NA_real_, NA_real_, 0.78874215, 0.93242714), tolerance = 1e-06, label = paste0("c(", paste0(results55$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results55$repeatedConfidenceIntervalLowerBounds[1, ], c(-48.431163, -28.407231, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results55$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results55$repeatedConfidenceIntervalLowerBounds[2, ], c(-39.624453, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results55$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results55$repeatedConfidenceIntervalLowerBounds[3, ], c(-50.936268, -32.815818, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results55$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results55$repeatedConfidenceIntervalUpperBounds[1, ], c(18.431163, 4.7231897, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results55$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results55$repeatedConfidenceIntervalUpperBounds[2, ], c(20.424453, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results55$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results55$repeatedConfidenceIntervalUpperBounds[3, ], c(15.936268, 0.34247232, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results55$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results55$repeatedPValues[1, ], c(0.5, 0.083136439, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results55$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results55$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results55$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results55$repeatedPValues[3, ], c(0.5, 0.024192808, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results55$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results55), NA)))
        expect_output(print(results55)$show())
        invisible(capture.output(expect_error(summary(results55), NA)))
        expect_output(summary(results55)$show())
        results55CodeBased <- eval(parse(text = getObjectRCode(results55, stringWrapParagraphWidth = NULL)))
        expect_equal(results55CodeBased$thetaH1, results55$thetaH1, tolerance = 1e-06)
        expect_equal(results55CodeBased$assumedStDevs, results55$assumedStDevs, tolerance = 1e-06)
        expect_equal(results55CodeBased$conditionalRejectionProbabilities, results55$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results55CodeBased$conditionalPower, results55$conditionalPower, tolerance = 1e-06)
        expect_equal(results55CodeBased$repeatedConfidenceIntervalLowerBounds, results55$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results55CodeBased$repeatedConfidenceIntervalUpperBounds, results55$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results55CodeBased$repeatedPValues, results55$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results55), "character")
        df <- as.data.frame(results55)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results55)
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
    results56 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Sidak", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results56' with expected results
    expect_equal(results56$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results56$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results56$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results56$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results56$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results56$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results56$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results56$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results56$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results56$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results56$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results56$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results56$conditionalRejectionProbabilities[1, ], c(0.043192758, 0.15430882, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results56$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results56$conditionalRejectionProbabilities[2, ], c(0.034321105, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results56$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results56$conditionalRejectionProbabilities[3, ], c(0.050842102, 0.35990794, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results56$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results56$conditionalPower[1, ], c(NA_real_, NA_real_, 0.44741759, 0.6940249), tolerance = 1e-06, label = paste0("c(", paste0(results56$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results56$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results56$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results56$conditionalPower[3, ], c(NA_real_, NA_real_, 0.84172467, 0.95168763), tolerance = 1e-06, label = paste0("c(", paste0(results56$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results56$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.800321, -27.943326, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results56$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results56$repeatedConfidenceIntervalLowerBounds[2, ], c(-38.430944, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results56$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results56$repeatedConfidenceIntervalLowerBounds[3, ], c(-46.785003, -32.005071, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results56$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results56$repeatedConfidenceIntervalUpperBounds[1, ], c(14.800321, 4.2506387, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results56$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results56$repeatedConfidenceIntervalUpperBounds[2, ], c(19.230944, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results56$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results56$repeatedConfidenceIntervalUpperBounds[3, ], c(11.785003, -0.46968016, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results56$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results56$repeatedPValues[1, ], c(0.5, 0.077086341, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results56$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results56$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results56$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results56$repeatedPValues[3, ], c(0.5, 0.014829652, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results56$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results56), NA)))
        expect_output(print(results56)$show())
        invisible(capture.output(expect_error(summary(results56), NA)))
        expect_output(summary(results56)$show())
        results56CodeBased <- eval(parse(text = getObjectRCode(results56, stringWrapParagraphWidth = NULL)))
        expect_equal(results56CodeBased$thetaH1, results56$thetaH1, tolerance = 1e-06)
        expect_equal(results56CodeBased$assumedStDevs, results56$assumedStDevs, tolerance = 1e-06)
        expect_equal(results56CodeBased$conditionalRejectionProbabilities, results56$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results56CodeBased$conditionalPower, results56$conditionalPower, tolerance = 1e-06)
        expect_equal(results56CodeBased$repeatedConfidenceIntervalLowerBounds, results56$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results56CodeBased$repeatedConfidenceIntervalUpperBounds, results56$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results56CodeBased$repeatedPValues, results56$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results56), "character")
        df <- as.data.frame(results56)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results56)
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
    results57 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Sidak", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results57' with expected results
    expect_equal(results57$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results57$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results57$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results57$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results57$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results57$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results57$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results57$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results57$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results57$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results57$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results57$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results57$conditionalRejectionProbabilities[1, ], c(0.041569453, 0.14613212, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results57$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results57$conditionalRejectionProbabilities[2, ], c(0.033856262, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results57$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results57$conditionalRejectionProbabilities[3, ], c(0.047839714, 0.32760313, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results57$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results57$conditionalPower[1, ], c(NA_real_, NA_real_, 0.43181681, 0.68255335), tolerance = 1e-06, label = paste0("c(", paste0(results57$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results57$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results57$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results57$conditionalPower[3, ], c(NA_real_, NA_real_, 0.81761872, 0.94309649), tolerance = 1e-06, label = paste0("c(", paste0(results57$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results57$repeatedConfidenceIntervalLowerBounds[1, ], c(-46.565416, -28.357046, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results57$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results57$repeatedConfidenceIntervalLowerBounds[2, ], c(-40.138622, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results57$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results57$repeatedConfidenceIntervalLowerBounds[3, ], c(-48.519575, -32.386196, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results57$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results57$repeatedConfidenceIntervalUpperBounds[1, ], c(16.565416, 4.6248784, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results57$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results57$repeatedConfidenceIntervalUpperBounds[2, ], c(20.938622, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results57$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results57$repeatedConfidenceIntervalUpperBounds[3, ], c(13.519575, -0.10461531, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results57$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results57$repeatedPValues[1, ], c(0.5, 0.083428262, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results57$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results57$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results57$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results57$repeatedPValues[3, ], c(0.5, 0.018799791, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results57$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results57), NA)))
        expect_output(print(results57)$show())
        invisible(capture.output(expect_error(summary(results57), NA)))
        expect_output(summary(results57)$show())
        results57CodeBased <- eval(parse(text = getObjectRCode(results57, stringWrapParagraphWidth = NULL)))
        expect_equal(results57CodeBased$thetaH1, results57$thetaH1, tolerance = 1e-06)
        expect_equal(results57CodeBased$assumedStDevs, results57$assumedStDevs, tolerance = 1e-06)
        expect_equal(results57CodeBased$conditionalRejectionProbabilities, results57$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results57CodeBased$conditionalPower, results57$conditionalPower, tolerance = 1e-06)
        expect_equal(results57CodeBased$repeatedConfidenceIntervalLowerBounds, results57$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results57CodeBased$repeatedConfidenceIntervalUpperBounds, results57$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results57CodeBased$repeatedPValues, results57$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results57), "character")
        df <- as.data.frame(results57)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results57)
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
    results58 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Sidak", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results58' with expected results
    expect_equal(results58$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results58$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results58$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results58$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results58$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results58$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results58$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results58$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results58$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results58$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results58$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results58$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results58$conditionalRejectionProbabilities[1, ], c(0.044003076, 0.16034604, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results58$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results58$conditionalRejectionProbabilities[2, ], c(0.038533075, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results58$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results58$conditionalRejectionProbabilities[3, ], c(0.047740982, 0.33733332, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results58$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results58$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45861366, 0.70212467), tolerance = 1e-06, label = paste0("c(", paste0(results58$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results58$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results58$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results58$conditionalPower[3, ], c(NA_real_, NA_real_, 0.82521432, 0.94583446), tolerance = 1e-06, label = paste0("c(", paste0(results58$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results58$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.548, -27.773536, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results58$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results58$repeatedConfidenceIntervalLowerBounds[2, ], c(-36.046937, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results58$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results58$repeatedConfidenceIntervalLowerBounds[3, ], c(-47.481556, -32.250037, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results58$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results58$repeatedConfidenceIntervalUpperBounds[1, ], c(14.548, 4.0869288, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results58$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results58$repeatedConfidenceIntervalUpperBounds[2, ], c(16.846937, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results58$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results58$repeatedConfidenceIntervalUpperBounds[3, ], c(12.481557, -0.21501802, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results58$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results58$repeatedPValues[1, ], c(0.5, 0.072804352, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results58$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results58$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results58$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results58$repeatedPValues[3, ], c(0.5, 0.017498028, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results58$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results58), NA)))
        expect_output(print(results58)$show())
        invisible(capture.output(expect_error(summary(results58), NA)))
        expect_output(summary(results58)$show())
        results58CodeBased <- eval(parse(text = getObjectRCode(results58, stringWrapParagraphWidth = NULL)))
        expect_equal(results58CodeBased$thetaH1, results58$thetaH1, tolerance = 1e-06)
        expect_equal(results58CodeBased$assumedStDevs, results58$assumedStDevs, tolerance = 1e-06)
        expect_equal(results58CodeBased$conditionalRejectionProbabilities, results58$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results58CodeBased$conditionalPower, results58$conditionalPower, tolerance = 1e-06)
        expect_equal(results58CodeBased$repeatedConfidenceIntervalLowerBounds, results58$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results58CodeBased$repeatedConfidenceIntervalUpperBounds, results58$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results58CodeBased$repeatedPValues, results58$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results58), "character")
        df <- as.data.frame(results58)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results58)
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
    results59 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Sidak", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results59' with expected results
    expect_equal(results59$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results59$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results59$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results59$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results59$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results59$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results59$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results59$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results59$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results59$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results59$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results59$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results59$conditionalRejectionProbabilities[1, ], c(0.040514523, 0.14472681, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results59$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results59$conditionalRejectionProbabilities[2, ], c(0.037322005, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results59$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results59$conditionalRejectionProbabilities[3, ], c(0.042460333, 0.28832504, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results59$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results59$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42908294, 0.68052019), tolerance = 1e-06, label = paste0("c(", paste0(results59$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results59$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results59$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results59$conditionalPower[3, ], c(NA_real_, NA_real_, 0.78363361, 0.93049656), tolerance = 1e-06, label = paste0("c(", paste0(results59$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results59$repeatedConfidenceIntervalLowerBounds[1, ], c(-48.598892, -28.449073, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results59$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results59$repeatedConfidenceIntervalLowerBounds[2, ], c(-39.351395, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results59$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results59$repeatedConfidenceIntervalLowerBounds[3, ], c(-51.401351, -32.883177, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results59$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results59$repeatedConfidenceIntervalUpperBounds[1, ], c(18.598892, 4.7729883, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results59$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results59$repeatedConfidenceIntervalUpperBounds[2, ], c(20.151395, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results59$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results59$repeatedConfidenceIntervalUpperBounds[3, ], c(16.401351, 0.41981706, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results59$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results59$repeatedPValues[1, ], c(0.5, 0.084586974, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results59$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results59$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results59$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results59$repeatedPValues[3, ], c(0.5, 0.025221821, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results59$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results59), NA)))
        expect_output(print(results59)$show())
        invisible(capture.output(expect_error(summary(results59), NA)))
        expect_output(summary(results59)$show())
        results59CodeBased <- eval(parse(text = getObjectRCode(results59, stringWrapParagraphWidth = NULL)))
        expect_equal(results59CodeBased$thetaH1, results59$thetaH1, tolerance = 1e-06)
        expect_equal(results59CodeBased$assumedStDevs, results59$assumedStDevs, tolerance = 1e-06)
        expect_equal(results59CodeBased$conditionalRejectionProbabilities, results59$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results59CodeBased$conditionalPower, results59$conditionalPower, tolerance = 1e-06)
        expect_equal(results59CodeBased$repeatedConfidenceIntervalLowerBounds, results59$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results59CodeBased$repeatedConfidenceIntervalUpperBounds, results59$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results59CodeBased$repeatedPValues, results59$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results59), "character")
        df <- as.data.frame(results59)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results59)
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
    results60 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Dunnett", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results60' with expected results
    expect_equal(results60$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results60$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results60$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results60$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results60$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results60$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results60$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results60$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results60$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results60$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results60$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results60$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results60$conditionalRejectionProbabilities[1, ], c(0.046821821, 0.16471602, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results60$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results60$conditionalRejectionProbabilities[2, ], c(0.034321105, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results60$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results60$conditionalRejectionProbabilities[3, ], c(0.056787656, 0.38875311, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results60$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results60$conditionalPower[1, ], c(NA_real_, NA_real_, 0.46655424, 0.70780427), tolerance = 1e-06, label = paste0("c(", paste0(results60$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results60$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results60$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results60$conditionalPower[3, ], c(NA_real_, NA_real_, 0.8607721, 0.95827226), tolerance = 1e-06, label = paste0("c(", paste0(results60$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results60$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.645334, -27.415422, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results60$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results60$repeatedConfidenceIntervalLowerBounds[2, ], c(-38.280999, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results60$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results60$repeatedConfidenceIntervalLowerBounds[3, ], c(-46.632695, -31.563129, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results60$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results60$repeatedConfidenceIntervalUpperBounds[1, ], c(14.645333, 3.927683, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results60$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results60$repeatedConfidenceIntervalUpperBounds[2, ], c(19.080998, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results60$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results60$repeatedConfidenceIntervalUpperBounds[3, ], c(11.632695, -0.82950364, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results60$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results60$repeatedPValues[1, ], c(0.5, 0.069897558, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results60$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results60$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results60$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results60$repeatedPValues[3, ], c(0.5, 0.012021087, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results60$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results60), NA)))
        expect_output(print(results60)$show())
        invisible(capture.output(expect_error(summary(results60), NA)))
        expect_output(summary(results60)$show())
        results60CodeBased <- eval(parse(text = getObjectRCode(results60, stringWrapParagraphWidth = NULL)))
        expect_equal(results60CodeBased$thetaH1, results60$thetaH1, tolerance = 1e-06)
        expect_equal(results60CodeBased$assumedStDevs, results60$assumedStDevs, tolerance = 1e-06)
        expect_equal(results60CodeBased$conditionalRejectionProbabilities, results60$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results60CodeBased$conditionalPower, results60$conditionalPower, tolerance = 1e-06)
        expect_equal(results60CodeBased$repeatedConfidenceIntervalLowerBounds, results60$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results60CodeBased$repeatedConfidenceIntervalUpperBounds, results60$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results60CodeBased$repeatedPValues, results60$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results60), "character")
        df <- as.data.frame(results60)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results60)
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
    results61 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Dunnett", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results61' with expected results
    expect_equal(results61$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results61$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results61$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results61$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results61$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results61$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results61$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results61$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results61$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results61$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results61$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results61$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results61$conditionalRejectionProbabilities[1, ], c(0.045317687, 0.15683192, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results61$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results61$conditionalRejectionProbabilities[2, ], c(0.033856262, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results61$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results61$conditionalRejectionProbabilities[3, ], c(0.054085103, 0.3588303, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results61$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results61$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45212919, 0.69744676), tolerance = 1e-06, label = paste0("c(", paste0(results61$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results61$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results61$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results61$conditionalPower[3, ], c(NA_real_, NA_real_, 0.84097006, 0.95142305), tolerance = 1e-06, label = paste0("c(", paste0(results61$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results61$repeatedConfidenceIntervalLowerBounds[1, ], c(-46.335113, -27.786662, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results61$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results61$repeatedConfidenceIntervalLowerBounds[2, ], c(-39.91581, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results61$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results61$repeatedConfidenceIntervalLowerBounds[3, ], c(-48.293254, -31.900882, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results61$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results61$repeatedConfidenceIntervalUpperBounds[1, ], c(16.335113, 4.2557288, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results61$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results61$repeatedConfidenceIntervalUpperBounds[2, ], c(20.71581, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results61$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results61$repeatedConfidenceIntervalUpperBounds[3, ], c(13.293254, -0.50940978, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results61$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results61$repeatedPValues[1, ], c(0.5, 0.075258151, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results61$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results61$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results61$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results61$repeatedPValues[3, ], c(0.5, 0.014946954, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results61$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results61), NA)))
        expect_output(print(results61)$show())
        invisible(capture.output(expect_error(summary(results61), NA)))
        expect_output(summary(results61)$show())
        results61CodeBased <- eval(parse(text = getObjectRCode(results61, stringWrapParagraphWidth = NULL)))
        expect_equal(results61CodeBased$thetaH1, results61$thetaH1, tolerance = 1e-06)
        expect_equal(results61CodeBased$assumedStDevs, results61$assumedStDevs, tolerance = 1e-06)
        expect_equal(results61CodeBased$conditionalRejectionProbabilities, results61$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results61CodeBased$conditionalPower, results61$conditionalPower, tolerance = 1e-06)
        expect_equal(results61CodeBased$repeatedConfidenceIntervalLowerBounds, results61$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results61CodeBased$repeatedConfidenceIntervalUpperBounds, results61$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results61CodeBased$repeatedPValues, results61$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results61), "character")
        df <- as.data.frame(results61)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results61)
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
    results62 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Bonferroni", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results62' with expected results
    expect_equal(results62$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results62$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results62$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results62$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results62$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results62$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results62$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results62$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results62$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results62$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results62$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results62$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results62$conditionalRejectionProbabilities[1, ], c(0.024608533, 0.053964296, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results62$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results62$conditionalRejectionProbabilities[2, ], c(0.021871903, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results62$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results62$conditionalRejectionProbabilities[3, ], c(0.027261939, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results62$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results62$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.256183, -26.806923, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results62$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results62$repeatedConfidenceIntervalLowerBounds[2, ], c(-32.361515, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results62$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results62$repeatedConfidenceIntervalLowerBounds[3, ], c(-42.907669, -31.664999, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results62$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results62$repeatedConfidenceIntervalUpperBounds[1, ], c(10.256183, 4.0576303, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results62$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results62$repeatedConfidenceIntervalUpperBounds[2, ], c(13.161515, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results62$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results62$repeatedConfidenceIntervalUpperBounds[3, ], c(7.9076686, -0.4326836, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results62$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results62$repeatedPValues[1, ], c(0.17463845, 0.062131804, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results62$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results62$repeatedPValues[2, ], c(0.20337355, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results62$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results62$repeatedPValues[3, ], c(0.1527221, 0.015597359, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results62$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results62$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.277, 0.453), tolerance = 1e-06, label = paste0("c(", paste0(results62$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results62$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results62$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results62$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results62$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results62), NA)))
        expect_output(print(results62)$show())
        invisible(capture.output(expect_error(summary(results62), NA)))
        expect_output(summary(results62)$show())
        results62CodeBased <- eval(parse(text = getObjectRCode(results62, stringWrapParagraphWidth = NULL)))
        expect_equal(results62CodeBased$thetaH1, results62$thetaH1, tolerance = 1e-06)
        expect_equal(results62CodeBased$assumedStDevs, results62$assumedStDevs, tolerance = 1e-06)
        expect_equal(results62CodeBased$conditionalRejectionProbabilities, results62$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results62CodeBased$repeatedConfidenceIntervalLowerBounds, results62$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results62CodeBased$repeatedConfidenceIntervalUpperBounds, results62$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results62CodeBased$repeatedPValues, results62$repeatedPValues, tolerance = 1e-06)
        expect_equal(results62CodeBased$conditionalPowerSimulated, results62$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results62), "character")
        df <- as.data.frame(results62)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results62)
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
    results63 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Bonferroni", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results63' with expected results
    expect_equal(results63$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results63$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results63$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results63$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results63$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results63$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results63$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results63$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results63$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results63$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results63$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results63$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results63$conditionalRejectionProbabilities[1, ], c(0.022711489, 0.047669561, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results63$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results63$conditionalRejectionProbabilities[2, ], c(0.021234311, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results63$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results63$conditionalRejectionProbabilities[3, ], c(0.024147032, 0.14148061, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results63$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results63$repeatedConfidenceIntervalLowerBounds[1, ], c(-42.830851, -27.447651, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results63$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results63$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.616779, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results63$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results63$repeatedConfidenceIntervalLowerBounds[3, ], c(-45.376075, -32.257244, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results63$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results63$repeatedConfidenceIntervalUpperBounds[1, ], c(12.830851, 4.722817, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results63$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results63$repeatedConfidenceIntervalUpperBounds[2, ], c(15.416779, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results63$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results63$repeatedConfidenceIntervalUpperBounds[3, ], c(10.376075, 0.16648466, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results63$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results63$repeatedPValues[1, ], c(0.19376148, 0.070634747, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results63$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results63$repeatedPValues[2, ], c(0.2112175, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results63$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results63$repeatedPValues[3, ], c(0.17899101, 0.021776202, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results63$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results63$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.257, 0.434), tolerance = 1e-06, label = paste0("c(", paste0(results63$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results63$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results63$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results63$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.656, 0.797), tolerance = 1e-06, label = paste0("c(", paste0(results63$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results63), NA)))
        expect_output(print(results63)$show())
        invisible(capture.output(expect_error(summary(results63), NA)))
        expect_output(summary(results63)$show())
        results63CodeBased <- eval(parse(text = getObjectRCode(results63, stringWrapParagraphWidth = NULL)))
        expect_equal(results63CodeBased$thetaH1, results63$thetaH1, tolerance = 1e-06)
        expect_equal(results63CodeBased$assumedStDevs, results63$assumedStDevs, tolerance = 1e-06)
        expect_equal(results63CodeBased$conditionalRejectionProbabilities, results63$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results63CodeBased$repeatedConfidenceIntervalLowerBounds, results63$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results63CodeBased$repeatedConfidenceIntervalUpperBounds, results63$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results63CodeBased$repeatedPValues, results63$repeatedPValues, tolerance = 1e-06)
        expect_equal(results63CodeBased$conditionalPowerSimulated, results63$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results63), "character")
        df <- as.data.frame(results63)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results63)
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
    results64 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Bonferroni", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results64' with expected results
    expect_equal(results64$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results64$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results64$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results64$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results64$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results64$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results64$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results64$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results64$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results64$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results64$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results64$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results64$conditionalRejectionProbabilities[1, ], c(0.02389937, 0.050606752, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results64$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results64$conditionalRejectionProbabilities[2, ], c(0.019837849, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results64$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results64$conditionalRejectionProbabilities[3, ], c(0.028008383, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results64$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results64$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.605988, -27.021858, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results64$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results64$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.373049, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results64$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results64$repeatedConfidenceIntervalLowerBounds[3, ], c(-42.6632, -31.486561, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results64$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results64$repeatedConfidenceIntervalUpperBounds[1, ], c(10.605988, 4.2731734, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results64$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results64$repeatedConfidenceIntervalUpperBounds[2, ], c(15.173049, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results64$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results64$repeatedConfidenceIntervalUpperBounds[3, ], c(7.6631999, -0.60791563, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results64$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results64$repeatedPValues[1, ], c(0.18140284, 0.066412839, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results64$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results64$repeatedPValues[2, ], c(0.23027951, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results64$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results64$repeatedPValues[3, ], c(0.14737581, 0.014014262, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results64$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results64$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.268, 0.445), tolerance = 1e-06, label = paste0("c(", paste0(results64$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results64$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results64$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results64$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results64$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results64), NA)))
        expect_output(print(results64)$show())
        invisible(capture.output(expect_error(summary(results64), NA)))
        expect_output(summary(results64)$show())
        results64CodeBased <- eval(parse(text = getObjectRCode(results64, stringWrapParagraphWidth = NULL)))
        expect_equal(results64CodeBased$thetaH1, results64$thetaH1, tolerance = 1e-06)
        expect_equal(results64CodeBased$assumedStDevs, results64$assumedStDevs, tolerance = 1e-06)
        expect_equal(results64CodeBased$conditionalRejectionProbabilities, results64$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results64CodeBased$repeatedConfidenceIntervalLowerBounds, results64$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results64CodeBased$repeatedConfidenceIntervalUpperBounds, results64$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results64CodeBased$repeatedPValues, results64$repeatedPValues, tolerance = 1e-06)
        expect_equal(results64CodeBased$conditionalPowerSimulated, results64$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results64), "character")
        df <- as.data.frame(results64)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results64)
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
    results65 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Bonferroni", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results65' with expected results
    expect_equal(results65$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results65$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results65$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results65$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results65$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results65$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results65$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results65$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results65$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results65$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results65$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results65$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results65$conditionalRejectionProbabilities[1, ], c(0.023040094, 0.047419024, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results65$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results65$conditionalRejectionProbabilities[2, ], c(0.019613852, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results65$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results65$conditionalRejectionProbabilities[3, ], c(0.026303733, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results65$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results65$repeatedConfidenceIntervalLowerBounds[1, ], c(-41.74771, -27.425347, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results65$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results65$repeatedConfidenceIntervalLowerBounds[2, ], c(-35.477631, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results65$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results65$repeatedConfidenceIntervalLowerBounds[3, ], c(-43.785178, -31.856528, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results65$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results65$repeatedConfidenceIntervalUpperBounds[1, ], c(11.74771, 4.6401767, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results65$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results65$repeatedConfidenceIntervalUpperBounds[2, ], c(16.277631, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results65$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results65$repeatedConfidenceIntervalUpperBounds[3, ], c(8.7851784, -0.25415362, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results65$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results65$repeatedPValues[1, ], c(0.19020524, 0.071018123, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results65$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results65$repeatedPValues[2, ], c(0.23360401, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results65$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results65$repeatedPValues[3, ], c(0.16007682, 0.01742078, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results65$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results65$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.256, 0.433), tolerance = 1e-06, label = paste0("c(", paste0(results65$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results65$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results65$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results65$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results65$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results65), NA)))
        expect_output(print(results65)$show())
        invisible(capture.output(expect_error(summary(results65), NA)))
        expect_output(summary(results65)$show())
        results65CodeBased <- eval(parse(text = getObjectRCode(results65, stringWrapParagraphWidth = NULL)))
        expect_equal(results65CodeBased$thetaH1, results65$thetaH1, tolerance = 1e-06)
        expect_equal(results65CodeBased$assumedStDevs, results65$assumedStDevs, tolerance = 1e-06)
        expect_equal(results65CodeBased$conditionalRejectionProbabilities, results65$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results65CodeBased$repeatedConfidenceIntervalLowerBounds, results65$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results65CodeBased$repeatedConfidenceIntervalUpperBounds, results65$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results65CodeBased$repeatedPValues, results65$repeatedPValues, tolerance = 1e-06)
        expect_equal(results65CodeBased$conditionalPowerSimulated, results65$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results65), "character")
        df <- as.data.frame(results65)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results65)
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
    results66 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Bonferroni", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results66' with expected results
    expect_equal(results66$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results66$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results66$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results66$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results66$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results66$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results66$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results66$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results66$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results66$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results66$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results66$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results66$conditionalRejectionProbabilities[1, ], c(0.024333354, 0.053095357, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results66$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results66$conditionalRejectionProbabilities[2, ], c(0.021915713, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results66$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results66$conditionalRejectionProbabilities[3, ], c(0.026248507, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results66$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results66$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.389181, -26.847363, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results66$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results66$repeatedConfidenceIntervalLowerBounds[2, ], c(-32.324586, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results66$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results66$repeatedConfidenceIntervalLowerBounds[3, ], c(-43.261715, -31.705217, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results66$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results66$repeatedConfidenceIntervalUpperBounds[1, ], c(10.389181, 4.1091853, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results66$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results66$repeatedConfidenceIntervalUpperBounds[2, ], c(13.124586, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results66$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results66$repeatedConfidenceIntervalUpperBounds[3, ], c(8.2617152, -0.37246523, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results66$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results66$repeatedPValues[1, ], c(0.17721241, 0.063189426, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results66$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results66$repeatedPValues[2, ], c(0.20285189, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results66$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results66$repeatedPValues[3, ], c(0.16051933, 0.01616384, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results66$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results66$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.274, 0.453), tolerance = 1e-06, label = paste0("c(", paste0(results66$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results66$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results66$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results66$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results66$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results66), NA)))
        expect_output(print(results66)$show())
        invisible(capture.output(expect_error(summary(results66), NA)))
        expect_output(summary(results66)$show())
        results66CodeBased <- eval(parse(text = getObjectRCode(results66, stringWrapParagraphWidth = NULL)))
        expect_equal(results66CodeBased$thetaH1, results66$thetaH1, tolerance = 1e-06)
        expect_equal(results66CodeBased$assumedStDevs, results66$assumedStDevs, tolerance = 1e-06)
        expect_equal(results66CodeBased$conditionalRejectionProbabilities, results66$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results66CodeBased$repeatedConfidenceIntervalLowerBounds, results66$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results66CodeBased$repeatedConfidenceIntervalUpperBounds, results66$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results66CodeBased$repeatedPValues, results66$repeatedPValues, tolerance = 1e-06)
        expect_equal(results66CodeBased$conditionalPowerSimulated, results66$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results66), "character")
        df <- as.data.frame(results66)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results66)
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
    results67 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Bonferroni", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results67' with expected results
    expect_equal(results67$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results67$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results67$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results67$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results67$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results67$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results67$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results67$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results67$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results67$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results67$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results67$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results67$conditionalRejectionProbabilities[1, ], c(0.02248882, 0.047009108, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results67$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results67$conditionalRejectionProbabilities[2, ], c(0.021309255, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results67$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results67$conditionalRejectionProbabilities[3, ], c(0.023369532, 0.13794488, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results67$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results67$repeatedConfidenceIntervalLowerBounds[1, ], c(-42.972232, -27.481288, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results67$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results67$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.436237, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results67$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results67$repeatedConfidenceIntervalLowerBounds[3, ], c(-45.763994, -32.295837, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results67$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results67$repeatedConfidenceIntervalUpperBounds[1, ], c(12.972232, 4.7692163, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results67$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results67$repeatedConfidenceIntervalUpperBounds[2, ], c(15.236237, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results67$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results67$repeatedConfidenceIntervalUpperBounds[3, ], c(10.763995, 0.22335705, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results67$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results67$repeatedPValues[1, ], c(0.19623626, 0.071653269, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results67$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results67$repeatedPValues[2, ], c(0.21026955, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results67$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results67$repeatedPValues[3, ], c(0.18674722, 0.022408487, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results67$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results67$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.256, 0.431), tolerance = 1e-06, label = paste0("c(", paste0(results67$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results67$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results67$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results67$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.652, 0.795), tolerance = 1e-06, label = paste0("c(", paste0(results67$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results67), NA)))
        expect_output(print(results67)$show())
        invisible(capture.output(expect_error(summary(results67), NA)))
        expect_output(summary(results67)$show())
        results67CodeBased <- eval(parse(text = getObjectRCode(results67, stringWrapParagraphWidth = NULL)))
        expect_equal(results67CodeBased$thetaH1, results67$thetaH1, tolerance = 1e-06)
        expect_equal(results67CodeBased$assumedStDevs, results67$assumedStDevs, tolerance = 1e-06)
        expect_equal(results67CodeBased$conditionalRejectionProbabilities, results67$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results67CodeBased$repeatedConfidenceIntervalLowerBounds, results67$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results67CodeBased$repeatedConfidenceIntervalUpperBounds, results67$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results67CodeBased$repeatedPValues, results67$repeatedPValues, tolerance = 1e-06)
        expect_equal(results67CodeBased$conditionalPowerSimulated, results67$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results67), "character")
        df <- as.data.frame(results67)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results67)
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
    results68 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results68' with expected results
    expect_equal(results68$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results68$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results68$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results68$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results68$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results68$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results68$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results68$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results68$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results68$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results68$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results68$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results68$conditionalRejectionProbabilities[1, ], c(0.024608533, 0.053964296, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results68$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results68$conditionalRejectionProbabilities[2, ], c(0.021871903, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results68$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results68$conditionalRejectionProbabilities[3, ], c(0.029595078, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results68$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results68$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.256183, -26.806923, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results68$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results68$repeatedConfidenceIntervalLowerBounds[2, ], c(-32.361515, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results68$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results68$repeatedConfidenceIntervalLowerBounds[3, ], c(-42.907669, -31.664999, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results68$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results68$repeatedConfidenceIntervalUpperBounds[1, ], c(10.256183, 4.0576303, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results68$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results68$repeatedConfidenceIntervalUpperBounds[2, ], c(13.161515, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results68$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results68$repeatedConfidenceIntervalUpperBounds[3, ], c(7.9076686, -0.4326836, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results68$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results68$repeatedPValues[1, ], c(0.17463845, 0.062131804, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results68$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results68$repeatedPValues[2, ], c(0.20337355, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results68$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results68$repeatedPValues[3, ], c(0.13700176, 0.014275569, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results68$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results68$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.277, 0.453), tolerance = 1e-06, label = paste0("c(", paste0(results68$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results68$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results68$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results68$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results68$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results68), NA)))
        expect_output(print(results68)$show())
        invisible(capture.output(expect_error(summary(results68), NA)))
        expect_output(summary(results68)$show())
        results68CodeBased <- eval(parse(text = getObjectRCode(results68, stringWrapParagraphWidth = NULL)))
        expect_equal(results68CodeBased$thetaH1, results68$thetaH1, tolerance = 1e-06)
        expect_equal(results68CodeBased$assumedStDevs, results68$assumedStDevs, tolerance = 1e-06)
        expect_equal(results68CodeBased$conditionalRejectionProbabilities, results68$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results68CodeBased$repeatedConfidenceIntervalLowerBounds, results68$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results68CodeBased$repeatedConfidenceIntervalUpperBounds, results68$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results68CodeBased$repeatedPValues, results68$repeatedPValues, tolerance = 1e-06)
        expect_equal(results68CodeBased$conditionalPowerSimulated, results68$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results68), "character")
        df <- as.data.frame(results68)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results68)
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
    results69 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results69' with expected results
    expect_equal(results69$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results69$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results69$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results69$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results69$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results69$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results69$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results69$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results69$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results69$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results69$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results69$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results69$conditionalRejectionProbabilities[1, ], c(0.022711489, 0.047669561, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results69$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results69$conditionalRejectionProbabilities[2, ], c(0.021234311, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results69$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results69$conditionalRejectionProbabilities[3, ], c(0.027312859, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results69$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results69$repeatedConfidenceIntervalLowerBounds[1, ], c(-42.830851, -27.447651, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results69$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results69$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.616779, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results69$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results69$repeatedConfidenceIntervalLowerBounds[3, ], c(-45.376075, -32.257244, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results69$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results69$repeatedConfidenceIntervalUpperBounds[1, ], c(12.830851, 4.722817, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results69$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results69$repeatedConfidenceIntervalUpperBounds[2, ], c(15.416779, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results69$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results69$repeatedConfidenceIntervalUpperBounds[3, ], c(10.376075, 0.16648466, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results69$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results69$repeatedPValues[1, ], c(0.19376148, 0.070634747, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results69$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results69$repeatedPValues[2, ], c(0.2112175, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results69$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results69$repeatedPValues[3, ], c(0.15234731, 0.019097336, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results69$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results69$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.257, 0.434), tolerance = 1e-06, label = paste0("c(", paste0(results69$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results69$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results69$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results69$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results69$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results69), NA)))
        expect_output(print(results69)$show())
        invisible(capture.output(expect_error(summary(results69), NA)))
        expect_output(summary(results69)$show())
        results69CodeBased <- eval(parse(text = getObjectRCode(results69, stringWrapParagraphWidth = NULL)))
        expect_equal(results69CodeBased$thetaH1, results69$thetaH1, tolerance = 1e-06)
        expect_equal(results69CodeBased$assumedStDevs, results69$assumedStDevs, tolerance = 1e-06)
        expect_equal(results69CodeBased$conditionalRejectionProbabilities, results69$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results69CodeBased$repeatedConfidenceIntervalLowerBounds, results69$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results69CodeBased$repeatedConfidenceIntervalUpperBounds, results69$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results69CodeBased$repeatedPValues, results69$repeatedPValues, tolerance = 1e-06)
        expect_equal(results69CodeBased$conditionalPowerSimulated, results69$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results69), "character")
        df <- as.data.frame(results69)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results69)
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
    results70 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Simes", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results70' with expected results
    expect_equal(results70$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results70$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results70$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results70$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results70$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results70$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results70$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results70$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results70$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results70$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results70$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results70$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results70$conditionalRejectionProbabilities[1, ], c(0.02389937, 0.050606752, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results70$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results70$conditionalRejectionProbabilities[2, ], c(0.019837849, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results70$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results70$conditionalRejectionProbabilities[3, ], c(0.028741907, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results70$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results70$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.605988, -27.021858, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results70$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results70$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.373049, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results70$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results70$repeatedConfidenceIntervalLowerBounds[3, ], c(-42.6632, -31.486561, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results70$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results70$repeatedConfidenceIntervalUpperBounds[1, ], c(10.605988, 4.2731734, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results70$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results70$repeatedConfidenceIntervalUpperBounds[2, ], c(15.173049, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results70$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results70$repeatedConfidenceIntervalUpperBounds[3, ], c(7.6631999, -0.60791563, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results70$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results70$repeatedPValues[1, ], c(0.18140284, 0.066412839, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results70$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results70$repeatedPValues[2, ], c(0.23027951, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results70$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results70$repeatedPValues[3, ], c(0.14242148, 0.013628025, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results70$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results70$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.268, 0.445), tolerance = 1e-06, label = paste0("c(", paste0(results70$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results70$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results70$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results70$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results70$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results70), NA)))
        expect_output(print(results70)$show())
        invisible(capture.output(expect_error(summary(results70), NA)))
        expect_output(summary(results70)$show())
        results70CodeBased <- eval(parse(text = getObjectRCode(results70, stringWrapParagraphWidth = NULL)))
        expect_equal(results70CodeBased$thetaH1, results70$thetaH1, tolerance = 1e-06)
        expect_equal(results70CodeBased$assumedStDevs, results70$assumedStDevs, tolerance = 1e-06)
        expect_equal(results70CodeBased$conditionalRejectionProbabilities, results70$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results70CodeBased$repeatedConfidenceIntervalLowerBounds, results70$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results70CodeBased$repeatedConfidenceIntervalUpperBounds, results70$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results70CodeBased$repeatedPValues, results70$repeatedPValues, tolerance = 1e-06)
        expect_equal(results70CodeBased$conditionalPowerSimulated, results70$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results70), "character")
        df <- as.data.frame(results70)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results70)
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
    results71 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Simes", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results71' with expected results
    expect_equal(results71$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results71$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results71$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results71$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results71$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results71$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results71$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results71$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results71$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results71$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results71$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results71$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results71$conditionalRejectionProbabilities[1, ], c(0.023040094, 0.047419024, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results71$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results71$conditionalRejectionProbabilities[2, ], c(0.019613852, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results71$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results71$conditionalRejectionProbabilities[3, ], c(0.027708171, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results71$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results71$repeatedConfidenceIntervalLowerBounds[1, ], c(-41.74771, -27.425347, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results71$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results71$repeatedConfidenceIntervalLowerBounds[2, ], c(-35.477631, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results71$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results71$repeatedConfidenceIntervalLowerBounds[3, ], c(-43.785178, -31.856528, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results71$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results71$repeatedConfidenceIntervalUpperBounds[1, ], c(11.74771, 4.6401767, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results71$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results71$repeatedConfidenceIntervalUpperBounds[2, ], c(16.277631, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results71$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results71$repeatedConfidenceIntervalUpperBounds[3, ], c(8.7851784, -0.25415362, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results71$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results71$repeatedPValues[1, ], c(0.19020524, 0.071018123, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results71$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results71$repeatedPValues[2, ], c(0.23360401, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results71$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results71$repeatedPValues[3, ], c(0.1494882, 0.016474737, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results71$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results71$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.256, 0.433), tolerance = 1e-06, label = paste0("c(", paste0(results71$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results71$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results71$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results71$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results71$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results71), NA)))
        expect_output(print(results71)$show())
        invisible(capture.output(expect_error(summary(results71), NA)))
        expect_output(summary(results71)$show())
        results71CodeBased <- eval(parse(text = getObjectRCode(results71, stringWrapParagraphWidth = NULL)))
        expect_equal(results71CodeBased$thetaH1, results71$thetaH1, tolerance = 1e-06)
        expect_equal(results71CodeBased$assumedStDevs, results71$assumedStDevs, tolerance = 1e-06)
        expect_equal(results71CodeBased$conditionalRejectionProbabilities, results71$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results71CodeBased$repeatedConfidenceIntervalLowerBounds, results71$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results71CodeBased$repeatedConfidenceIntervalUpperBounds, results71$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results71CodeBased$repeatedPValues, results71$repeatedPValues, tolerance = 1e-06)
        expect_equal(results71CodeBased$conditionalPowerSimulated, results71$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results71), "character")
        df <- as.data.frame(results71)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results71)
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
    results72 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Simes", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results72' with expected results
    expect_equal(results72$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results72$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results72$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results72$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results72$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results72$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results72$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results72$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results72$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results72$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results72$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results72$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results72$conditionalRejectionProbabilities[1, ], c(0.024333354, 0.053095357, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results72$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results72$conditionalRejectionProbabilities[2, ], c(0.021915713, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results72$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results72$conditionalRejectionProbabilities[3, ], c(0.029264016, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results72$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results72$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.389181, -26.847363, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results72$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results72$repeatedConfidenceIntervalLowerBounds[2, ], c(-32.324586, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results72$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results72$repeatedConfidenceIntervalLowerBounds[3, ], c(-43.261715, -31.705217, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results72$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results72$repeatedConfidenceIntervalUpperBounds[1, ], c(10.389181, 4.1091853, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results72$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results72$repeatedConfidenceIntervalUpperBounds[2, ], c(13.124586, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results72$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results72$repeatedConfidenceIntervalUpperBounds[3, ], c(8.2617152, -0.37246523, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results72$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results72$repeatedPValues[1, ], c(0.17721241, 0.063189426, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results72$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results72$repeatedPValues[2, ], c(0.20285189, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results72$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results72$repeatedPValues[3, ], c(0.13906265, 0.014376658, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results72$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results72$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.274, 0.453), tolerance = 1e-06, label = paste0("c(", paste0(results72$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results72$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results72$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results72$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results72$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results72), NA)))
        expect_output(print(results72)$show())
        invisible(capture.output(expect_error(summary(results72), NA)))
        expect_output(summary(results72)$show())
        results72CodeBased <- eval(parse(text = getObjectRCode(results72, stringWrapParagraphWidth = NULL)))
        expect_equal(results72CodeBased$thetaH1, results72$thetaH1, tolerance = 1e-06)
        expect_equal(results72CodeBased$assumedStDevs, results72$assumedStDevs, tolerance = 1e-06)
        expect_equal(results72CodeBased$conditionalRejectionProbabilities, results72$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results72CodeBased$repeatedConfidenceIntervalLowerBounds, results72$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results72CodeBased$repeatedConfidenceIntervalUpperBounds, results72$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results72CodeBased$repeatedPValues, results72$repeatedPValues, tolerance = 1e-06)
        expect_equal(results72CodeBased$conditionalPowerSimulated, results72$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results72), "character")
        df <- as.data.frame(results72)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results72)
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
    results73 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Simes", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results73' with expected results
    expect_equal(results73$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results73$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results73$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results73$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results73$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results73$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results73$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results73$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results73$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results73$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results73$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results73$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results73$conditionalRejectionProbabilities[1, ], c(0.02248882, 0.047009108, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results73$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results73$conditionalRejectionProbabilities[2, ], c(0.021309255, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results73$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results73$conditionalRejectionProbabilities[3, ], c(0.027044989, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results73$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results73$repeatedConfidenceIntervalLowerBounds[1, ], c(-42.972232, -27.481288, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results73$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results73$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.436237, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results73$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results73$repeatedConfidenceIntervalLowerBounds[3, ], c(-45.763994, -32.295837, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results73$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results73$repeatedConfidenceIntervalUpperBounds[1, ], c(12.972232, 4.7692163, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results73$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results73$repeatedConfidenceIntervalUpperBounds[2, ], c(15.236237, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results73$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results73$repeatedConfidenceIntervalUpperBounds[3, ], c(10.763995, 0.22335705, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results73$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results73$repeatedPValues[1, ], c(0.19623626, 0.071653269, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results73$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results73$repeatedPValues[2, ], c(0.21026955, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results73$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results73$repeatedPValues[3, ], c(0.15433667, 0.019180306, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results73$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results73$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.256, 0.431), tolerance = 1e-06, label = paste0("c(", paste0(results73$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results73$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results73$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results73$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results73$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results73), NA)))
        expect_output(print(results73)$show())
        invisible(capture.output(expect_error(summary(results73), NA)))
        expect_output(summary(results73)$show())
        results73CodeBased <- eval(parse(text = getObjectRCode(results73, stringWrapParagraphWidth = NULL)))
        expect_equal(results73CodeBased$thetaH1, results73$thetaH1, tolerance = 1e-06)
        expect_equal(results73CodeBased$assumedStDevs, results73$assumedStDevs, tolerance = 1e-06)
        expect_equal(results73CodeBased$conditionalRejectionProbabilities, results73$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results73CodeBased$repeatedConfidenceIntervalLowerBounds, results73$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results73CodeBased$repeatedConfidenceIntervalUpperBounds, results73$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results73CodeBased$repeatedPValues, results73$repeatedPValues, tolerance = 1e-06)
        expect_equal(results73CodeBased$conditionalPowerSimulated, results73$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results73), "character")
        df <- as.data.frame(results73)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results73)
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
    results74 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Sidak", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results74' with expected results
    expect_equal(results74$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results74$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results74$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results74$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results74$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results74$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results74$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results74$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results74$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results74$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results74$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results74$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results74$conditionalRejectionProbabilities[1, ], c(0.025021019, 0.054834069, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results74$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results74$conditionalRejectionProbabilities[2, ], c(0.021871903, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results74$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results74$conditionalRejectionProbabilities[3, ], c(0.027777772, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results74$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results74$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.247199, -26.680539, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results74$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results74$repeatedConfidenceIntervalLowerBounds[2, ], c(-32.353418, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results74$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results74$repeatedConfidenceIntervalLowerBounds[3, ], c(-42.898631, -31.584065, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results74$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results74$repeatedConfidenceIntervalUpperBounds[1, ], c(10.247199, 4.0258193, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results74$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results74$repeatedConfidenceIntervalUpperBounds[2, ], c(13.153418, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results74$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results74$repeatedConfidenceIntervalUpperBounds[3, ], c(7.8986307, -0.47811558, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results74$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results74$repeatedPValues[1, ], c(0.17089623, 0.061105652, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results74$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results74$repeatedPValues[2, ], c(0.20337355, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results74$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results74$repeatedPValues[3, ], c(0.14899419, 0.015246407, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results74$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results74$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.283, 0.454), tolerance = 1e-06, label = paste0("c(", paste0(results74$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results74$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results74$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results74$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results74$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results74), NA)))
        expect_output(print(results74)$show())
        invisible(capture.output(expect_error(summary(results74), NA)))
        expect_output(summary(results74)$show())
        results74CodeBased <- eval(parse(text = getObjectRCode(results74, stringWrapParagraphWidth = NULL)))
        expect_equal(results74CodeBased$thetaH1, results74$thetaH1, tolerance = 1e-06)
        expect_equal(results74CodeBased$assumedStDevs, results74$assumedStDevs, tolerance = 1e-06)
        expect_equal(results74CodeBased$conditionalRejectionProbabilities, results74$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results74CodeBased$repeatedConfidenceIntervalLowerBounds, results74$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results74CodeBased$repeatedConfidenceIntervalUpperBounds, results74$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results74CodeBased$repeatedPValues, results74$repeatedPValues, tolerance = 1e-06)
        expect_equal(results74CodeBased$conditionalPowerSimulated, results74$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results74), "character")
        df <- as.data.frame(results74)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results74)
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
    results75 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Sidak", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results75' with expected results
    expect_equal(results75$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results75$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results75$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results75$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results75$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results75$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results75$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results75$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results75$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results75$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results75$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results75$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results75$conditionalRejectionProbabilities[1, ], c(0.023144095, 0.048545015, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results75$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results75$conditionalRejectionProbabilities[2, ], c(0.021234311, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results75$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results75$conditionalRejectionProbabilities[3, ], c(0.0247006, 0.1449328, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results75$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results75$repeatedConfidenceIntervalLowerBounds[1, ], c(-42.8192, -27.314543, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results75$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results75$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.60635, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results75$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results75$repeatedConfidenceIntervalLowerBounds[3, ], c(-45.364486, -32.169333, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results75$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results75$repeatedConfidenceIntervalUpperBounds[1, ], c(12.8192, 4.6852584, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results75$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results75$repeatedConfidenceIntervalUpperBounds[2, ], c(15.40635, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results75$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results75$repeatedConfidenceIntervalUpperBounds[3, ], c(10.364486, 0.1144866, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results75$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results75$repeatedPValues[1, ], c(0.18910184, 0.069324401, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results75$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results75$repeatedPValues[2, ], c(0.2112175, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results75$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results75$repeatedPValues[3, ], c(0.17379158, 0.021189694, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results75$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results75$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.26, 0.437), tolerance = 1e-06, label = paste0("c(", paste0(results75$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results75$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results75$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results75$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.66, 0.799), tolerance = 1e-06, label = paste0("c(", paste0(results75$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results75), NA)))
        expect_output(print(results75)$show())
        invisible(capture.output(expect_error(summary(results75), NA)))
        expect_output(summary(results75)$show())
        results75CodeBased <- eval(parse(text = getObjectRCode(results75, stringWrapParagraphWidth = NULL)))
        expect_equal(results75CodeBased$thetaH1, results75$thetaH1, tolerance = 1e-06)
        expect_equal(results75CodeBased$assumedStDevs, results75$assumedStDevs, tolerance = 1e-06)
        expect_equal(results75CodeBased$conditionalRejectionProbabilities, results75$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results75CodeBased$repeatedConfidenceIntervalLowerBounds, results75$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results75CodeBased$repeatedConfidenceIntervalUpperBounds, results75$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results75CodeBased$repeatedPValues, results75$repeatedPValues, tolerance = 1e-06)
        expect_equal(results75CodeBased$conditionalPowerSimulated, results75$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results75), "character")
        df <- as.data.frame(results75)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results75)
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
    results76 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Sidak", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results76' with expected results
    expect_equal(results76$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results76$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results76$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results76$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results76$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results76$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results76$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results76$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results76$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results76$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results76$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results76$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results76$conditionalRejectionProbabilities[1, ], c(0.024319059, 0.051462476, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results76$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results76$conditionalRejectionProbabilities[2, ], c(0.019837849, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results76$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results76$conditionalRejectionProbabilities[3, ], c(0.028516214, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results76$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results76$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.59688, -26.894985, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results76$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results76$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.364237, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results76$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results76$repeatedConfidenceIntervalLowerBounds[3, ], c(-42.654249, -31.405859, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results76$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results76$repeatedConfidenceIntervalUpperBounds[1, ], c(10.59688, 4.2407133, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results76$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results76$repeatedConfidenceIntervalUpperBounds[2, ], c(15.164237, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results76$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results76$repeatedConfidenceIntervalUpperBounds[3, ], c(7.6542489, -0.6529301, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results76$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results76$repeatedPValues[1, ], c(0.17734783, 0.06527034, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results76$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results76$repeatedPValues[2, ], c(0.23027951, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results76$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results76$repeatedPValues[3, ], c(0.14391589, 0.013711948, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results76$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results76$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.271, 0.447), tolerance = 1e-06, label = paste0("c(", paste0(results76$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results76$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results76$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results76$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results76$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results76), NA)))
        expect_output(print(results76)$show())
        invisible(capture.output(expect_error(summary(results76), NA)))
        expect_output(summary(results76)$show())
        results76CodeBased <- eval(parse(text = getObjectRCode(results76, stringWrapParagraphWidth = NULL)))
        expect_equal(results76CodeBased$thetaH1, results76$thetaH1, tolerance = 1e-06)
        expect_equal(results76CodeBased$assumedStDevs, results76$assumedStDevs, tolerance = 1e-06)
        expect_equal(results76CodeBased$conditionalRejectionProbabilities, results76$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results76CodeBased$repeatedConfidenceIntervalLowerBounds, results76$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results76CodeBased$repeatedConfidenceIntervalUpperBounds, results76$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results76CodeBased$repeatedPValues, results76$repeatedPValues, tolerance = 1e-06)
        expect_equal(results76CodeBased$conditionalPowerSimulated, results76$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results76), "character")
        df <- as.data.frame(results76)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results76)
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
    results77 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Sidak", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results77' with expected results
    expect_equal(results77$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results77$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results77$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results77$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results77$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results77$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results77$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results77$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results77$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results77$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results77$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results77$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results77$conditionalRejectionProbabilities[1, ], c(0.023469013, 0.048270226, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results77$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results77$conditionalRejectionProbabilities[2, ], c(0.019613852, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results77$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results77$conditionalRejectionProbabilities[3, ], c(0.026830382, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results77$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results77$repeatedConfidenceIntervalLowerBounds[1, ], c(-41.737451, -27.295819, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results77$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results77$repeatedConfidenceIntervalLowerBounds[2, ], c(-35.467707, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results77$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results77$repeatedConfidenceIntervalLowerBounds[3, ], c(-43.775098, -31.772829, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results77$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results77$repeatedConfidenceIntervalUpperBounds[1, ], c(11.737451, 4.6050352, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results77$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results77$repeatedConfidenceIntervalUpperBounds[2, ], c(16.267707, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results77$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results77$repeatedConfidenceIntervalUpperBounds[3, ], c(8.7750975, -0.30217392, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results77$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results77$repeatedPValues[1, ], c(0.18572393, 0.069730666, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results77$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results77$repeatedPValues[2, ], c(0.23360401, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results77$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results77$repeatedPValues[3, ], c(0.15596268, 0.017006886, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results77$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results77$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.26, 0.436), tolerance = 1e-06, label = paste0("c(", paste0(results77$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results77$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results77$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results77$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results77$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results77), NA)))
        expect_output(print(results77)$show())
        invisible(capture.output(expect_error(summary(results77), NA)))
        expect_output(summary(results77)$show())
        results77CodeBased <- eval(parse(text = getObjectRCode(results77, stringWrapParagraphWidth = NULL)))
        expect_equal(results77CodeBased$thetaH1, results77$thetaH1, tolerance = 1e-06)
        expect_equal(results77CodeBased$assumedStDevs, results77$assumedStDevs, tolerance = 1e-06)
        expect_equal(results77CodeBased$conditionalRejectionProbabilities, results77$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results77CodeBased$repeatedConfidenceIntervalLowerBounds, results77$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results77CodeBased$repeatedConfidenceIntervalUpperBounds, results77$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results77CodeBased$repeatedPValues, results77$repeatedPValues, tolerance = 1e-06)
        expect_equal(results77CodeBased$conditionalPowerSimulated, results77$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results77), "character")
        df <- as.data.frame(results77)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results77)
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
    results78 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Sidak", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results78' with expected results
    expect_equal(results78$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results78$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results78$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results78$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results78$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results78$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results78$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results78$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results78$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results78$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results78$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results78$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results78$conditionalRejectionProbabilities[1, ], c(0.024748593, 0.053966892, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results78$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results78$conditionalRejectionProbabilities[2, ], c(0.021915713, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results78$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results78$conditionalRejectionProbabilities[3, ], c(0.0267758, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results78$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results78$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.38015, -26.720108, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results78$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results78$repeatedConfidenceIntervalLowerBounds[2, ], c(-32.316502, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results78$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results78$repeatedConfidenceIntervalLowerBounds[3, ], c(-43.252551, -31.62149, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results78$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results78$repeatedConfidenceIntervalUpperBounds[1, ], c(10.38015, 4.0770639, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results78$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results78$repeatedConfidenceIntervalUpperBounds[2, ], c(13.116502, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results78$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results78$repeatedConfidenceIntervalUpperBounds[3, ], c(8.2525514, -0.41959343, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results78$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results78$repeatedPValues[1, ], c(0.17335289, 0.062127989, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results78$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results78$repeatedPValues[2, ], c(0.20285189, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results78$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results78$repeatedPValues[3, ], c(0.15638134, 0.015781417, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results78$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results78$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.277, 0.453), tolerance = 1e-06, label = paste0("c(", paste0(results78$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results78$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results78$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results78$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results78$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results78), NA)))
        expect_output(print(results78)$show())
        invisible(capture.output(expect_error(summary(results78), NA)))
        expect_output(summary(results78)$show())
        results78CodeBased <- eval(parse(text = getObjectRCode(results78, stringWrapParagraphWidth = NULL)))
        expect_equal(results78CodeBased$thetaH1, results78$thetaH1, tolerance = 1e-06)
        expect_equal(results78CodeBased$assumedStDevs, results78$assumedStDevs, tolerance = 1e-06)
        expect_equal(results78CodeBased$conditionalRejectionProbabilities, results78$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results78CodeBased$repeatedConfidenceIntervalLowerBounds, results78$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results78CodeBased$repeatedConfidenceIntervalUpperBounds, results78$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results78CodeBased$repeatedPValues, results78$repeatedPValues, tolerance = 1e-06)
        expect_equal(results78CodeBased$conditionalPowerSimulated, results78$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results78), "character")
        df <- as.data.frame(results78)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results78)
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
    results79 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Sidak", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results79' with expected results
    expect_equal(results79$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results79$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results79$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results79$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results79$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results79$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results79$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results79$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results79$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results79$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results79$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results79$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results79$conditionalRejectionProbabilities[1, ], c(0.022923976, 0.04788638, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results79$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results79$conditionalRejectionProbabilities[2, ], c(0.021309255, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results79$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results79$conditionalRejectionProbabilities[3, ], c(0.023933809, 0.14146912, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results79$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results79$repeatedConfidenceIntervalLowerBounds[1, ], c(-42.960526, -27.347242, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results79$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results79$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.425975, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results79$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results79$repeatedConfidenceIntervalLowerBounds[3, ], c(-45.752245, -32.205007, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results79$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results79$repeatedConfidenceIntervalUpperBounds[1, ], c(12.960526, 4.7313117, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results79$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results79$repeatedConfidenceIntervalUpperBounds[2, ], c(15.225975, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results79$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results79$repeatedConfidenceIntervalUpperBounds[3, ], c(10.752245, 0.16953037, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results79$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results79$repeatedPValues[1, ], c(0.19144883, 0.07030573, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results79$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results79$repeatedPValues[2, ], c(0.21026955, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results79$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results79$repeatedPValues[3, ], c(0.18106429, 0.021778109, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results79$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results79$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.259, 0.434), tolerance = 1e-06, label = paste0("c(", paste0(results79$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results79$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results79$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results79$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.656, 0.797), tolerance = 1e-06, label = paste0("c(", paste0(results79$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results79), NA)))
        expect_output(print(results79)$show())
        invisible(capture.output(expect_error(summary(results79), NA)))
        expect_output(summary(results79)$show())
        results79CodeBased <- eval(parse(text = getObjectRCode(results79, stringWrapParagraphWidth = NULL)))
        expect_equal(results79CodeBased$thetaH1, results79$thetaH1, tolerance = 1e-06)
        expect_equal(results79CodeBased$assumedStDevs, results79$assumedStDevs, tolerance = 1e-06)
        expect_equal(results79CodeBased$conditionalRejectionProbabilities, results79$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results79CodeBased$repeatedConfidenceIntervalLowerBounds, results79$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results79CodeBased$repeatedConfidenceIntervalUpperBounds, results79$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results79CodeBased$repeatedPValues, results79$repeatedPValues, tolerance = 1e-06)
        expect_equal(results79CodeBased$conditionalPowerSimulated, results79$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results79), "character")
        df <- as.data.frame(results79)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results79)
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
    results80 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Dunnett", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results80' with expected results
    expect_equal(results80$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results80$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results80$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results80$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results80$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results80$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results80$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results80$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results80$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results80$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results80$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results80$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results80$conditionalRejectionProbabilities[1, ], c(0.026270241, 0.055429536, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results80$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results80$conditionalRejectionProbabilities[2, ], c(0.019837849, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results80$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results80$conditionalRejectionProbabilities[3, ], c(0.032007473, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results80$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results80$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.308137, -26.527814, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results80$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results80$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.084887, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results80$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results80$repeatedConfidenceIntervalLowerBounds[3, ], c(-42.370499, -31.063081, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results80$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results80$repeatedConfidenceIntervalUpperBounds[1, ], c(10.308137, 3.9366921, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results80$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results80$repeatedConfidenceIntervalUpperBounds[2, ], c(14.884887, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results80$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results80$repeatedConfidenceIntervalUpperBounds[3, ], c(7.3704995, -0.96851041, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results80$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results80$repeatedPValues[1, ], c(0.1603448, 0.060420915, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results80$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results80$repeatedPValues[2, ], c(0.23027951, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results80$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results80$repeatedPValues[3, ], c(0.12340907, 0.011635803, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results80$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results80$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.286, 0.457), tolerance = 1e-06, label = paste0("c(", paste0(results80$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results80$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results80$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results80$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results80$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results80), NA)))
        expect_output(print(results80)$show())
        invisible(capture.output(expect_error(summary(results80), NA)))
        expect_output(summary(results80)$show())
        results80CodeBased <- eval(parse(text = getObjectRCode(results80, stringWrapParagraphWidth = NULL)))
        expect_equal(results80CodeBased$thetaH1, results80$thetaH1, tolerance = 1e-06)
        expect_equal(results80CodeBased$assumedStDevs, results80$assumedStDevs, tolerance = 1e-06)
        expect_equal(results80CodeBased$conditionalRejectionProbabilities, results80$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results80CodeBased$repeatedConfidenceIntervalLowerBounds, results80$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results80CodeBased$repeatedConfidenceIntervalUpperBounds, results80$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results80CodeBased$repeatedPValues, results80$repeatedPValues, tolerance = 1e-06)
        expect_equal(results80CodeBased$conditionalPowerSimulated, results80$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results80), "character")
        df <- as.data.frame(results80)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results80)
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
    results81 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Dunnett", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results81' with expected results
    expect_equal(results81$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results81$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results81$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results81$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results81$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results81$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results81$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results81$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results81$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results81$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results81$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results81$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results81$conditionalRejectionProbabilities[1, ], c(0.025452912, 0.052195908, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results81$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results81$conditionalRejectionProbabilities[2, ], c(0.019613852, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results81$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results81$conditionalRejectionProbabilities[3, ], c(0.030394861, 1, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results81$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results81$repeatedConfidenceIntervalLowerBounds[1, ], c(-41.358826, -26.891429, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results81$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results81$repeatedConfidenceIntervalLowerBounds[2, ], c(-35.101397, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results81$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results81$repeatedConfidenceIntervalLowerBounds[3, ], c(-43.40302, -31.392896, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results81$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results81$repeatedConfidenceIntervalUpperBounds[1, ], c(11.358826, 4.2590391, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results81$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results81$repeatedConfidenceIntervalUpperBounds[2, ], c(15.901398, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results81$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results81$repeatedConfidenceIntervalUpperBounds[3, ], c(8.4030195, -0.65705914, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results81$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results81$repeatedPValues[1, ], c(0.16712065, 0.064319528, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results81$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results81$repeatedPValues[2, ], c(0.23360401, NA_real_, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results81$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results81$repeatedPValues[3, ], c(0.13222768, 0.014210719, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results81$repeatedPValues[3, ], collapse = ", "), ")"))
    expect_equal(results81$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.272, 0.449), tolerance = 1e-06, label = paste0("c(", paste0(results81$conditionalPowerSimulated[1, ], collapse = ", "), ")"))
    expect_equal(results81$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results81$conditionalPowerSimulated[2, ], collapse = ", "), ")"))
    expect_equal(results81$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1), label = paste0("c(", paste0(results81$conditionalPowerSimulated[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results81), NA)))
        expect_output(print(results81)$show())
        invisible(capture.output(expect_error(summary(results81), NA)))
        expect_output(summary(results81)$show())
        results81CodeBased <- eval(parse(text = getObjectRCode(results81, stringWrapParagraphWidth = NULL)))
        expect_equal(results81CodeBased$thetaH1, results81$thetaH1, tolerance = 1e-06)
        expect_equal(results81CodeBased$assumedStDevs, results81$assumedStDevs, tolerance = 1e-06)
        expect_equal(results81CodeBased$conditionalRejectionProbabilities, results81$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results81CodeBased$repeatedConfidenceIntervalLowerBounds, results81$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results81CodeBased$repeatedConfidenceIntervalUpperBounds, results81$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results81CodeBased$repeatedPValues, results81$repeatedPValues, tolerance = 1e-06)
        expect_equal(results81CodeBased$conditionalPowerSimulated, results81$conditionalPowerSimulated, tolerance = 1e-06)
        expect_type(names(results81), "character")
        df <- as.data.frame(results81)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results81)
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
    results82 <- getAnalysisResults(
        design = design3, dataInput = dataExample2,
        intersectionTest = "Dunnett", varianceOption = "overallPooled", normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsConditionalDunnett object 'results82' with expected results
    expect_equal(results82$thetaH1[1, ], -11.562259, tolerance = 1e-06, label = paste0("c(", paste0(results82$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results82$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results82$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results82$thetaH1[3, ], -16.036585, tolerance = 1e-06, label = paste0("c(", paste0(results82$thetaH1[3, ], collapse = ", "), ")"))
    expect_equal(results82$assumedStDevs[1, ], 22.357668, tolerance = 1e-06, label = paste0("c(", paste0(results82$assumedStDevs[1, ], collapse = ", "), ")"))
    expect_equal(results82$assumedStDevs[2, ], NA_real_, label = paste0("c(", paste0(results82$assumedStDevs[2, ], collapse = ", "), ")"))
    expect_equal(results82$assumedStDevs[3, ], 22.943518, tolerance = 1e-06, label = paste0("c(", paste0(results82$assumedStDevs[3, ], collapse = ", "), ")"))
    expect_equal(results82$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.061352937), tolerance = 1e-06, label = paste0("c(", paste0(results82$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results82$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.03744742), tolerance = 1e-06, label = paste0("c(", paste0(results82$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results82$conditionalRejectionProbabilities[3, ], c(NA_real_, 0.086511764), tolerance = 1e-06, label = paste0("c(", paste0(results82$conditionalRejectionProbabilities[3, ], collapse = ", "), ")"))
    expect_equal(results82$conditionalPower[1, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results82$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results82$conditionalPower[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results82$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results82$conditionalPower[3, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results82$conditionalPower[3, ], collapse = ", "), ")"))
    expect_equal(results82$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, -22.538727), tolerance = 1e-06, label = paste0("c(", paste0(results82$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results82$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results82$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results82$repeatedConfidenceIntervalLowerBounds[3, ], c(NA_real_, -26.753532), tolerance = 1e-06, label = paste0("c(", paste0(results82$repeatedConfidenceIntervalLowerBounds[3, ], collapse = ", "), ")"))
    expect_equal(results82$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, 0.72441408), tolerance = 1e-06, label = paste0("c(", paste0(results82$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results82$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results82$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results82$repeatedConfidenceIntervalUpperBounds[3, ], c(NA_real_, -3.9389155), tolerance = 1e-06, label = paste0("c(", paste0(results82$repeatedConfidenceIntervalUpperBounds[3, ], collapse = ", "), ")"))
    expect_equal(results82$repeatedPValues[1, ], c(NA_real_, 0.017445576), tolerance = 1e-06, label = paste0("c(", paste0(results82$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results82$repeatedPValues[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results82$repeatedPValues[2, ], collapse = ", "), ")"))
    expect_equal(results82$repeatedPValues[3, ], c(NA_real_, 0.0019493527), tolerance = 1e-06, label = paste0("c(", paste0(results82$repeatedPValues[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results82), NA)))
        expect_output(print(results82)$show())
        invisible(capture.output(expect_error(summary(results82), NA)))
        expect_output(summary(results82)$show())
        results82CodeBased <- eval(parse(text = getObjectRCode(results82, stringWrapParagraphWidth = NULL)))
        expect_equal(results82CodeBased$thetaH1, results82$thetaH1, tolerance = 1e-06)
        expect_equal(results82CodeBased$assumedStDevs, results82$assumedStDevs, tolerance = 1e-06)
        expect_equal(results82CodeBased$conditionalRejectionProbabilities, results82$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(results82CodeBased$conditionalPower, results82$conditionalPower, tolerance = 1e-06)
        expect_equal(results82CodeBased$repeatedConfidenceIntervalLowerBounds, results82$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(results82CodeBased$repeatedConfidenceIntervalUpperBounds, results82$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(results82CodeBased$repeatedPValues, results82$repeatedPValues, tolerance = 1e-06)
        expect_type(names(results82), "character")
        df <- as.data.frame(results82)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(results82)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})
