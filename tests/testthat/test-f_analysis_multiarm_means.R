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
## |  Creation date: 12 August 2022, 09:59:10
## |  File version: $Revision: 6658 $
## |  Last changed: $Date: 2022-11-04 10:30:20 +0100 (Fr, 04 Nov 2022) $
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
        intersectionTest = "Simes", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results1' with expected results
    expect_equal(results1$thetaH1[1, ], 11.562259, tolerance = 1e-05)
    expect_equal(results1$thetaH1[2, ], NA_real_)
    expect_equal(results1$thetaH1[3, ], 16.036585, tolerance = 1e-05)
    expect_equal(results1$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
    expect_equal(results1$assumedStDevs[2, ], NA_real_)
    expect_equal(results1$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
    expect_equal(results1$conditionalRejectionProbabilities[1, ], c(0.040740209, 0.14372404, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results1$conditionalRejectionProbabilities[2, ], c(0.033856262, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results1$conditionalRejectionProbabilities[3, ], c(0.049414261, 0.33374326, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results1$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42712247, 0.6790579), tolerance = 1e-05)
    expect_equal(results1$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
    expect_equal(results1$conditionalPower[3, ], c(NA_real_, NA_real_, 0.82244694, 0.94484021), tolerance = 1e-05)
    expect_equal(results1$repeatedConfidenceIntervalLowerBounds[1, ], c(-16.567569, -4.662798, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results1$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.940706, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results1$repeatedConfidenceIntervalLowerBounds[3, ], c(-13.521691, 0.049006969, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results1$repeatedConfidenceIntervalUpperBounds[1, ], c(46.567569, 28.528695, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results1$repeatedConfidenceIntervalUpperBounds[2, ], c(40.140706, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results1$repeatedConfidenceIntervalUpperBounds[3, ], c(48.521691, 32.491814, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results1$repeatedPValues[1, ], c(0.5, 0.08542716, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results1$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results1$repeatedPValues[3, ], c(0.5, 0.017966281, NA_real_, NA_real_), tolerance = 1e-05)
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results1), NA)))
        expect_output(print(results1)$show())
        invisible(capture.output(expect_error(summary(results1), NA)))
        expect_output(summary(results1)$show())
        results1CodeBased <- eval(parse(text = getObjectRCode(results1, stringWrapParagraphWidth = NULL)))
        expect_equal(results1CodeBased$thetaH1, results1$thetaH1, tolerance = 1e-05)
        expect_equal(results1CodeBased$assumedStDevs, results1$assumedStDevs, tolerance = 1e-05)
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
    results2 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Sidak", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results2' with expected results
    expect_equal(results2$thetaH1[1, ], 11.562259, tolerance = 1e-05)
    expect_equal(results2$thetaH1[2, ], NA_real_)
    expect_equal(results2$thetaH1[3, ], 16.036585, tolerance = 1e-05)
    expect_equal(results2$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
    expect_equal(results2$assumedStDevs[2, ], NA_real_)
    expect_equal(results2$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
    expect_equal(results2$conditionalRejectionProbabilities[1, ], c(0.024748593, 0.053966892, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results2$conditionalRejectionProbabilities[2, ], c(0.021915713, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results2$conditionalRejectionProbabilities[3, ], c(0.0267758, 1, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results2$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.38015, -4.0770639, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results2$repeatedConfidenceIntervalLowerBounds[2, ], c(-13.116502, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results2$repeatedConfidenceIntervalLowerBounds[3, ], c(-8.2525514, 0.41959343, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results2$repeatedConfidenceIntervalUpperBounds[1, ], c(40.38015, 26.720108, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results2$repeatedConfidenceIntervalUpperBounds[2, ], c(32.316502, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results2$repeatedConfidenceIntervalUpperBounds[3, ], c(43.252551, 31.62149, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results2$repeatedPValues[1, ], c(0.17335289, 0.062127989, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results2$repeatedPValues[2, ], c(0.20285189, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results2$repeatedPValues[3, ], c(0.15638134, 0.015781417, NA_real_, NA_real_), tolerance = 1e-05)
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results2), NA)))
        expect_output(print(results2)$show())
        invisible(capture.output(expect_error(summary(results2), NA)))
        expect_output(summary(results2)$show())
        results2CodeBased <- eval(parse(text = getObjectRCode(results2, stringWrapParagraphWidth = NULL)))
        expect_equal(results2CodeBased$thetaH1, results2$thetaH1, tolerance = 1e-05)
        expect_equal(results2CodeBased$assumedStDevs, results2$assumedStDevs, tolerance = 1e-05)
        expect_equal(results2CodeBased$conditionalRejectionProbabilities, results2$conditionalRejectionProbabilities, tolerance = 1e-05)
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
    results3 <- getAnalysisResults(
        design = design3, dataInput = dataExample1,
        intersectionTest = "Dunnett", varianceOption = "overallPooled", normalApproximation = TRUE, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsConditionalDunnett object 'results3' with expected results
    expect_equal(results3$thetaH1[1, ], 11.562259, tolerance = 3e-04)
    expect_equal(results3$thetaH1[2, ], NA_real_)
    expect_equal(results3$thetaH1[3, ], 16.036585, tolerance = 3e-04)
    expect_equal(results3$assumedStDevs[1, ], 22.357668, tolerance = 3e-04)
    expect_equal(results3$assumedStDevs[2, ], NA_real_)
    expect_equal(results3$assumedStDevs[3, ], 22.943518, tolerance = 3e-04)
    expect_equal(results3$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.061352393), tolerance = 3e-04)
    expect_equal(results3$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.037447419), tolerance = 3e-04)
    expect_equal(results3$conditionalRejectionProbabilities[3, ], c(NA_real_, 0.08651207), tolerance = 3e-04)
    expect_equal(results3$conditionalPower[1, ], c(NA_real_, NA_real_))
    expect_equal(results3$conditionalPower[2, ], c(NA_real_, NA_real_))
    expect_equal(results3$conditionalPower[3, ], c(NA_real_, NA_real_))
    expect_equal(results3$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, -0.72440621), tolerance = 3e-04)
    expect_equal(results3$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_))
    expect_equal(results3$repeatedConfidenceIntervalLowerBounds[3, ], c(NA_real_, 3.9389233), tolerance = 3e-04)
    expect_equal(results3$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, 22.538721), tolerance = 3e-04)
    expect_equal(results3$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_))
    expect_equal(results3$repeatedConfidenceIntervalUpperBounds[3, ], c(NA_real_, 26.753524), tolerance = 3e-04)
    expect_equal(results3$repeatedPValues[1, ], c(NA_real_, 0.017445576), tolerance = 3e-04)
    expect_equal(results3$repeatedPValues[2, ], c(NA_real_, NA_real_))
    expect_equal(results3$repeatedPValues[3, ], c(NA_real_, 0.0019493527), tolerance = 3e-04)
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results3), NA)))
        expect_output(print(results3)$show())
        invisible(capture.output(expect_error(summary(results3), NA)))
        expect_output(summary(results3)$show())
        results3CodeBased <- eval(parse(text = getObjectRCode(results3, stringWrapParagraphWidth = NULL)))
        expect_equal(results3CodeBased$thetaH1, results3$thetaH1, tolerance = 1e-05)
        expect_equal(results3CodeBased$assumedStDevs, results3$assumedStDevs, tolerance = 1e-05)
        expect_equal(results3CodeBased$conditionalRejectionProbabilities, results3$conditionalRejectionProbabilities, tolerance = 1e-05)
        expect_equal(results3CodeBased$conditionalPower, results3$conditionalPower, tolerance = 1e-05)
        expect_equal(results3CodeBased$repeatedConfidenceIntervalLowerBounds, results3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
        expect_equal(results3CodeBased$repeatedConfidenceIntervalUpperBounds, results3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
        expect_equal(results3CodeBased$repeatedPValues, results3$repeatedPValues, tolerance = 1e-05)
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
        design = design1, dataInput = dataExample2,
        intersectionTest = "Bonferroni", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results4' with expected results
    expect_equal(results4$thetaH1[1, ], -11.562259, tolerance = 1e-05)
    expect_equal(results4$thetaH1[2, ], NA_real_)
    expect_equal(results4$thetaH1[3, ], -16.036585, tolerance = 1e-05)
    expect_equal(results4$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
    expect_equal(results4$assumedStDevs[2, ], NA_real_)
    expect_equal(results4$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
    expect_equal(results4$conditionalRejectionProbabilities[1, ], c(0.042394596, 0.15198143, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results4$conditionalRejectionProbabilities[2, ], c(0.034321105, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results4$conditionalRejectionProbabilities[3, ], c(0.049947129, 0.35588618, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results4$conditionalPower[1, ], c(NA_real_, NA_real_, 0.44302928, 0.69082025), tolerance = 1e-05)
    expect_equal(results4$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
    expect_equal(results4$conditionalPower[3, ], c(NA_real_, NA_real_, 0.83889182, 0.95069292), tolerance = 1e-05)
    expect_equal(results4$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.802158, -28.113845, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results4$repeatedConfidenceIntervalLowerBounds[2, ], c(-38.432721, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results4$repeatedConfidenceIntervalLowerBounds[3, ], c(-46.786808, -32.10754, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results4$repeatedConfidenceIntervalUpperBounds[1, ], c(14.802158, 4.2854677, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results4$repeatedConfidenceIntervalUpperBounds[2, ], c(19.232721, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results4$repeatedConfidenceIntervalUpperBounds[3, ], c(11.786808, -0.41764226, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results4$repeatedPValues[1, ], c(0.5, 0.078823932, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results4$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results4$repeatedPValues[3, ], c(0.5, 0.015272156, NA_real_, NA_real_), tolerance = 1e-05)
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results4), NA)))
        expect_output(print(results4)$show())
        invisible(capture.output(expect_error(summary(results4), NA)))
        expect_output(summary(results4)$show())
        results4CodeBased <- eval(parse(text = getObjectRCode(results4, stringWrapParagraphWidth = NULL)))
        expect_equal(results4CodeBased$thetaH1, results4$thetaH1, tolerance = 1e-05)
        expect_equal(results4CodeBased$assumedStDevs, results4$assumedStDevs, tolerance = 1e-05)
        expect_equal(results4CodeBased$conditionalRejectionProbabilities, results4$conditionalRejectionProbabilities, tolerance = 1e-05)
        expect_equal(results4CodeBased$conditionalPower, results4$conditionalPower, tolerance = 1e-05)
        expect_equal(results4CodeBased$repeatedConfidenceIntervalLowerBounds, results4$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-05)
        expect_equal(results4CodeBased$repeatedConfidenceIntervalUpperBounds, results4$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-05)
        expect_equal(results4CodeBased$repeatedPValues, results4$repeatedPValues, tolerance = 1e-05)
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
        design = design2, dataInput = dataExample2,
        intersectionTest = "Simes", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 1000, normalApproximation = FALSE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results5' with expected results
    expect_equal(results5$thetaH1[1, ], -11.562259, tolerance = 1e-05)
    expect_equal(results5$thetaH1[2, ], NA_real_)
    expect_equal(results5$thetaH1[3, ], -16.036585, tolerance = 1e-05)
    expect_equal(results5$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
    expect_equal(results5$assumedStDevs[2, ], NA_real_)
    expect_equal(results5$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
    expect_equal(results5$conditionalRejectionProbabilities[1, ], c(0.02248882, 0.047009108, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results5$conditionalRejectionProbabilities[2, ], c(0.021309255, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results5$conditionalRejectionProbabilities[3, ], c(0.027044989, 1, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results5$repeatedConfidenceIntervalLowerBounds[1, ], c(-42.972232, -27.481288, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results5$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.436237, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results5$repeatedConfidenceIntervalLowerBounds[3, ], c(-45.763994, -32.295837, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results5$repeatedConfidenceIntervalUpperBounds[1, ], c(12.972232, 4.7692163, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results5$repeatedConfidenceIntervalUpperBounds[2, ], c(15.236237, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results5$repeatedConfidenceIntervalUpperBounds[3, ], c(10.763995, 0.22335705, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results5$repeatedPValues[1, ], c(0.19623626, 0.071653269, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results5$repeatedPValues[2, ], c(0.21026955, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
    expect_equal(results5$repeatedPValues[3, ], c(0.15433667, 0.019180306, NA_real_, NA_real_), tolerance = 1e-05)
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results5), NA)))
        expect_output(print(results5)$show())
        invisible(capture.output(expect_error(summary(results5), NA)))
        expect_output(summary(results5)$show())
        results5CodeBased <- eval(parse(text = getObjectRCode(results5, stringWrapParagraphWidth = NULL)))
        expect_equal(results5CodeBased$thetaH1, results5$thetaH1, tolerance = 1e-05)
        expect_equal(results5CodeBased$assumedStDevs, results5$assumedStDevs, tolerance = 1e-05)
        expect_equal(results5CodeBased$conditionalRejectionProbabilities, results5$conditionalRejectionProbabilities, tolerance = 1e-05)
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
    results6 <- getAnalysisResults(
        design = design3, dataInput = dataExample2,
        intersectionTest = "Dunnett", varianceOption = "overallPooled", normalApproximation = TRUE, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsConditionalDunnett object 'results6' with expected results
    expect_equal(results6$thetaH1[1, ], -11.562259, tolerance = 3e-04)
    expect_equal(results6$thetaH1[2, ], NA_real_)
    expect_equal(results6$thetaH1[3, ], -16.036585, tolerance = 3e-04)
    expect_equal(results6$assumedStDevs[1, ], 22.357668, tolerance = 3e-04)
    expect_equal(results6$assumedStDevs[2, ], NA_real_)
    expect_equal(results6$assumedStDevs[3, ], 22.943518, tolerance = 3e-04)
    expect_equal(results6$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.061352393), tolerance = 3e-04)
    expect_equal(results6$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.037447419), tolerance = 3e-04)
    expect_equal(results6$conditionalRejectionProbabilities[3, ], c(NA_real_, 0.08651207), tolerance = 3e-04)
    expect_equal(results6$conditionalPower[1, ], c(NA_real_, NA_real_))
    expect_equal(results6$conditionalPower[2, ], c(NA_real_, NA_real_))
    expect_equal(results6$conditionalPower[3, ], c(NA_real_, NA_real_))
    expect_equal(results6$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, -22.538721), tolerance = 3e-04)
    expect_equal(results6$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_))
    expect_equal(results6$repeatedConfidenceIntervalLowerBounds[3, ], c(NA_real_, -26.753524), tolerance = 3e-04)
    expect_equal(results6$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, 0.72440621), tolerance = 3e-04)
    expect_equal(results6$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_))
    expect_equal(results6$repeatedConfidenceIntervalUpperBounds[3, ], c(NA_real_, -3.9389233), tolerance = 3e-04)
    expect_equal(results6$repeatedPValues[1, ], c(NA_real_, 0.017445576), tolerance = 3e-04)
    expect_equal(results6$repeatedPValues[2, ], c(NA_real_, NA_real_))
    expect_equal(results6$repeatedPValues[3, ], c(NA_real_, 0.0019493527), tolerance = 3e-04)
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results6), NA)))
        expect_output(print(results6)$show())
        invisible(capture.output(expect_error(summary(results6), NA)))
        expect_output(summary(results6)$show())
        results6CodeBased <- eval(parse(text = getObjectRCode(results6, stringWrapParagraphWidth = NULL)))
        expect_equal(results6CodeBased$thetaH1, results6$thetaH1, tolerance = 1e-05)
        expect_equal(results6CodeBased$assumedStDevs, results6$assumedStDevs, tolerance = 1e-05)
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
})
