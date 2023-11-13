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
## |  File name: test-f_analysis_multiarm_survival.R
## |  Creation date: 08 November 2023, 09:08:43
## |  File version: $Revision: 7403 $
## |  Last changed: $Date: 2023-11-08 16:12:00 +0100 (Mi, 08 Nov 2023) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing the Analysis Survival Functionality for Three or More Treatments")

test_that("'getAnalysisResultsMultiArm' with survival data and different options", {
    design1 <- getDesignInverseNormal(
        kMax = 3, alpha = 0.025, futilityBounds = c(-0.5, 0),
        bindingFutility = FALSE, typeOfDesign = "asKD", gammaA = 1.2, informationRates = c(0.4, 0.7, 1)
    )

    design2 <- getDesignFisher(
        kMax = 3, alpha = 0.025, alpha0Vec = c(0.7, 0.5), method = "equalAlpha",
        bindingFutility = TRUE, informationRates = c(0.4, 0.7, 1)
    )

    design3 <- getDesignConditionalDunnett(alpha = 0.025, informationAtInterim = 0.4, secondStageConditioning = TRUE)

    # directionUpper = TRUE
    dataExample1 <- getDataset(
        events1   = c(25, 32),
        events2   = c(18, NA),
        logRanks1 = c(2.2, 1.8),
        logRanks2 = c(1.99, NA)
    )

    # directionUpper = FALSE
    dataExample2 <- getDataset(
        events1   =  c(25, 32),
        events2   =  c(18, NA),
        logRanks1 = -c(2.2, 1.8),
        logRanks2 = -c(1.99, NA)
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
    # @refFS[Formula]{fs:testStatisticMultiArmSurvival}
    results1 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Dunnett", nPlanned = c(20), directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results1' with expected results
    expect_equal(results1$thetaH1[1, ], 2.1027372, tolerance = 1e-06, label = paste0("c(", paste0(results1$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results1$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results1$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results1$conditionalRejectionProbabilities[1, ], c(0.16552026, 0.53357256, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results1$conditionalRejectionProbabilities[2, ], c(0.16552026, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results1$conditionalPower[1, ], c(NA_real_, NA_real_, 0.9596109), tolerance = 1e-06, label = paste0("c(", paste0(results1$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results1$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results1$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results1$repeatedConfidenceIntervalLowerBounds[1, ], c(0.84462337, 1.0978924, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results1$repeatedConfidenceIntervalLowerBounds[2, ], c(0.74229856, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results1$repeatedConfidenceIntervalUpperBounds[1, ], c(6.8816904, 4.1951378, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results1$repeatedConfidenceIntervalUpperBounds[2, ], c(8.795088, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results1$repeatedPValues[1, ], c(0.077362906, 0.0096216473, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results1$repeatedPValues[2, ], c(0.077362906, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results1$repeatedPValues[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results1), NA)))
        expect_output(print(results1)$show())
        invisible(capture.output(expect_error(summary(results1), NA)))
        expect_output(summary(results1)$show())
        results1CodeBased <- eval(parse(text = getObjectRCode(results1, stringWrapParagraphWidth = NULL)))
        expect_equal(results1CodeBased$thetaH1, results1$thetaH1, tolerance = 1e-06)
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
    # @refFS[Formula]{fs:testStatisticMultiArmSurvival}
    results2 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Simes", nPlanned = c(20), directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results2' with expected results
    expect_equal(results2$thetaH1[1, ], 2.1027372, tolerance = 1e-06, label = paste0("c(", paste0(results2$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results2$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results2$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results2$conditionalRejectionProbabilities[1, ], c(0.17669226, 0.55323067, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results2$conditionalRejectionProbabilities[2, ], c(0.17669226, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results2$conditionalPower[1, ], c(NA_real_, NA_real_, 0.96373388), tolerance = 1e-06, label = paste0("c(", paste0(results2$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results2$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results2$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results2$repeatedConfidenceIntervalLowerBounds[1, ], c(0.83909619, 1.0883368, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results2$repeatedConfidenceIntervalLowerBounds[2, ], c(0.73657742, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results2$repeatedConfidenceIntervalUpperBounds[1, ], c(6.9270216, 4.2761956, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results2$repeatedConfidenceIntervalUpperBounds[2, ], c(8.863406, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results2$repeatedPValues[1, ], c(0.069951918, 0.0087766935, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results2$repeatedPValues[2, ], c(0.069951918, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results2$repeatedPValues[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results2), NA)))
        expect_output(print(results2)$show())
        invisible(capture.output(expect_error(summary(results2), NA)))
        expect_output(summary(results2)$show())
        results2CodeBased <- eval(parse(text = getObjectRCode(results2, stringWrapParagraphWidth = NULL)))
        expect_equal(results2CodeBased$thetaH1, results2$thetaH1, tolerance = 1e-06)
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
    # @refFS[Formula]{fs:testStatisticMultiArmSurvival}
    results3 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Sidak", nPlanned = c(20), directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results3' with expected results
    expect_equal(results3$thetaH1[1, ], 2.1027372, tolerance = 1e-06, label = paste0("c(", paste0(results3$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results3$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results3$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results3$conditionalRejectionProbabilities[1, ], c(0.15801679, 0.51979239, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results3$conditionalRejectionProbabilities[2, ], c(0.15801679, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results3$conditionalPower[1, ], c(NA_real_, NA_real_, 0.9565118), tolerance = 1e-06, label = paste0("c(", paste0(results3$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results3$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results3$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results3$repeatedConfidenceIntervalLowerBounds[1, ], c(0.83933393, 1.0895056, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results3$repeatedConfidenceIntervalLowerBounds[2, ], c(0.73682316, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results3$repeatedConfidenceIntervalUpperBounds[1, ], c(6.9250602, 4.2563039, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results3$repeatedConfidenceIntervalUpperBounds[2, ], c(8.8604482, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results3$repeatedPValues[1, ], c(0.082919001, 0.010252978, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results3$repeatedPValues[2, ], c(0.082919001, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results3$repeatedPValues[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results3), NA)))
        expect_output(print(results3)$show())
        invisible(capture.output(expect_error(summary(results3), NA)))
        expect_output(summary(results3)$show())
        results3CodeBased <- eval(parse(text = getObjectRCode(results3, stringWrapParagraphWidth = NULL)))
        expect_equal(results3CodeBased$thetaH1, results3$thetaH1, tolerance = 1e-06)
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
    # @refFS[Formula]{fs:testStatisticMultiArmSurvival}
    results4 <- getAnalysisResults(
        design = design1, dataInput = dataExample1,
        intersectionTest = "Bonferroni", nPlanned = c(20), directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results4' with expected results
    expect_equal(results4$thetaH1[1, ], 2.1027372, tolerance = 1e-06, label = paste0("c(", paste0(results4$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results4$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results4$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results4$conditionalRejectionProbabilities[1, ], c(0.15727093, 0.51839597, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results4$conditionalRejectionProbabilities[2, ], c(0.15727093, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results4$conditionalPower[1, ], c(NA_real_, NA_real_, 0.95618769), tolerance = 1e-06, label = paste0("c(", paste0(results4$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results4$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results4$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results4$repeatedConfidenceIntervalLowerBounds[1, ], c(0.83909619, 1.0883368, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results4$repeatedConfidenceIntervalLowerBounds[2, ], c(0.73657742, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results4$repeatedConfidenceIntervalUpperBounds[1, ], c(6.9270216, 4.2761956, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results4$repeatedConfidenceIntervalUpperBounds[2, ], c(8.863406, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results4$repeatedPValues[1, ], c(0.083499788, 0.010318782, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results4$repeatedPValues[2, ], c(0.083499788, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results4$repeatedPValues[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results4), NA)))
        expect_output(print(results4)$show())
        invisible(capture.output(expect_error(summary(results4), NA)))
        expect_output(summary(results4)$show())
        results4CodeBased <- eval(parse(text = getObjectRCode(results4, stringWrapParagraphWidth = NULL)))
        expect_equal(results4CodeBased$thetaH1, results4$thetaH1, tolerance = 1e-06)
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
    # @refFS[Formula]{fs:testStatisticMultiArmSurvival}
    results5 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Dunnett", nPlanned = c(20), seed = 1234, iterations = 1000, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results5' with expected results
    expect_equal(results5$thetaH1[1, ], 2.1027372, tolerance = 1e-06, label = paste0("c(", paste0(results5$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results5$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results5$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results5$conditionalRejectionProbabilities[1, ], c(0.10966406, 1, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results5$conditionalRejectionProbabilities[2, ], c(0.10966406, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results5$conditionalPower[1, ], c(NA_real_, NA_real_, 0.93227723), tolerance = 1e-06, label = paste0("c(", paste0(results5$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results5$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results5$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results5$repeatedConfidenceIntervalLowerBounds[1, ], c(0.91202484, 1.0654055, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results5$repeatedConfidenceIntervalLowerBounds[2, ], c(0.81259567, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results5$repeatedConfidenceIntervalUpperBounds[1, ], c(6.3731121, 4.2132452, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results5$repeatedConfidenceIntervalUpperBounds[2, ], c(8.034233, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results5$repeatedPValues[1, ], c(0.04389568, 0.013378163, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results5$repeatedPValues[2, ], c(0.04389568, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results5$repeatedPValues[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results5), NA)))
        expect_output(print(results5)$show())
        invisible(capture.output(expect_error(summary(results5), NA)))
        expect_output(summary(results5)$show())
        results5CodeBased <- eval(parse(text = getObjectRCode(results5, stringWrapParagraphWidth = NULL)))
        expect_equal(results5CodeBased$thetaH1, results5$thetaH1, tolerance = 1e-06)
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
    # @refFS[Formula]{fs:testStatisticMultiArmSurvival}
    results6 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Simes", nPlanned = c(20), seed = 1234, iterations = 1000, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results6' with expected results
    expect_equal(results6$thetaH1[1, ], 2.1027372, tolerance = 1e-06, label = paste0("c(", paste0(results6$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results6$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results6$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results6$conditionalRejectionProbabilities[1, ], c(0.1211541, 1, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results6$conditionalRejectionProbabilities[2, ], c(0.1211541, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results6$conditionalPower[1, ], c(NA_real_, NA_real_, 0.94819096), tolerance = 1e-06, label = paste0("c(", paste0(results6$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results6$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results6$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results6$repeatedConfidenceIntervalLowerBounds[1, ], c(0.90417824, 1.0568242, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results6$repeatedConfidenceIntervalLowerBounds[2, ], c(0.80436275, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results6$repeatedConfidenceIntervalUpperBounds[1, ], c(6.4284199, 4.2747728, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results6$repeatedConfidenceIntervalUpperBounds[2, ], c(8.1164667, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results6$repeatedPValues[1, ], c(0.039924588, 0.01222708, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results6$repeatedPValues[2, ], c(0.039924588, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results6$repeatedPValues[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results6), NA)))
        expect_output(print(results6)$show())
        invisible(capture.output(expect_error(summary(results6), NA)))
        expect_output(summary(results6)$show())
        results6CodeBased <- eval(parse(text = getObjectRCode(results6, stringWrapParagraphWidth = NULL)))
        expect_equal(results6CodeBased$thetaH1, results6$thetaH1, tolerance = 1e-06)
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
    # @refFS[Formula]{fs:testStatisticMultiArmSurvival}
    results7 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Sidak", nPlanned = c(20), seed = 1234, iterations = 1000, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results7' with expected results
    expect_equal(results7$thetaH1[1, ], 2.1027372, tolerance = 1e-06, label = paste0("c(", paste0(results7$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results7$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results7$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results7$conditionalRejectionProbabilities[1, ], c(0.1023739, 1, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results7$conditionalRejectionProbabilities[2, ], c(0.1023739, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results7$conditionalPower[1, ], c(NA_real_, NA_real_, 0.92036569), tolerance = 1e-06, label = paste0("c(", paste0(results7$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results7$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results7$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results7$repeatedConfidenceIntervalLowerBounds[1, ], c(0.90464342, 1.0577667, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results7$repeatedConfidenceIntervalLowerBounds[2, ], c(0.80485046, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results7$repeatedConfidenceIntervalUpperBounds[1, ], c(6.4251144, 4.2597035, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results7$repeatedConfidenceIntervalUpperBounds[2, ], c(8.1115484, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results7$repeatedPValues[1, ], c(0.046853018, 0.014230746, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results7$repeatedPValues[2, ], c(0.046853018, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results7$repeatedPValues[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results7), NA)))
        expect_output(print(results7)$show())
        invisible(capture.output(expect_error(summary(results7), NA)))
        expect_output(summary(results7)$show())
        results7CodeBased <- eval(parse(text = getObjectRCode(results7, stringWrapParagraphWidth = NULL)))
        expect_equal(results7CodeBased$thetaH1, results7$thetaH1, tolerance = 1e-06)
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
    # @refFS[Formula]{fs:testStatisticMultiArmSurvival}
    results8 <- getAnalysisResults(
        design = design2, dataInput = dataExample1,
        intersectionTest = "Bonferroni", nPlanned = c(20), seed = 1234, iterations = 1000, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results8' with expected results
    expect_equal(results8$thetaH1[1, ], 2.1027372, tolerance = 1e-06, label = paste0("c(", paste0(results8$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results8$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results8$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results8$conditionalRejectionProbabilities[1, ], c(0.10166729, 1, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results8$conditionalRejectionProbabilities[2, ], c(0.10166729, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results8$conditionalPower[1, ], c(NA_real_, NA_real_, 0.91912747), tolerance = 1e-06, label = paste0("c(", paste0(results8$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results8$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results8$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results8$repeatedConfidenceIntervalLowerBounds[1, ], c(0.90417824, 1.0568242, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results8$repeatedConfidenceIntervalLowerBounds[2, ], c(0.80436275, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results8$repeatedConfidenceIntervalUpperBounds[1, ], c(6.4284199, 4.2747728, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results8$repeatedConfidenceIntervalUpperBounds[2, ], c(8.1164667, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results8$repeatedPValues[1, ], c(0.047161054, 0.014319438, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results8$repeatedPValues[2, ], c(0.047161054, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results8$repeatedPValues[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results8), NA)))
        expect_output(print(results8)$show())
        invisible(capture.output(expect_error(summary(results8), NA)))
        expect_output(summary(results8)$show())
        results8CodeBased <- eval(parse(text = getObjectRCode(results8, stringWrapParagraphWidth = NULL)))
        expect_equal(results8CodeBased$thetaH1, results8$thetaH1, tolerance = 1e-06)
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
    # @refFS[Formula]{fs:testStatisticMultiArmSurvival}
    results9 <- getAnalysisResults(
        design = design3, dataInput = dataExample1,
        intersectionTest = "Dunnett", directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsConditionalDunnett object 'results9' with expected results
    expect_equal(results9$thetaH1[1, ], 2.1027372, tolerance = 1e-06, label = paste0("c(", paste0(results9$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results9$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results9$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results9$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.20921311), tolerance = 1e-06, label = paste0("c(", paste0(results9$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results9$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.18260705), tolerance = 1e-06, label = paste0("c(", paste0(results9$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results9$conditionalPower[1, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results9$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results9$conditionalPower[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results9$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results9$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, 1.2250509), tolerance = 1e-06, label = paste0("c(", paste0(results9$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results9$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results9$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results9$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, 3.6401262), tolerance = 1e-06, label = paste0("c(", paste0(results9$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results9$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results9$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results9$repeatedPValues[1, ], c(NA_real_, 0.0032883088), tolerance = 1e-06, label = paste0("c(", paste0(results9$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results9$repeatedPValues[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results9$repeatedPValues[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results9), NA)))
        expect_output(print(results9)$show())
        invisible(capture.output(expect_error(summary(results9), NA)))
        expect_output(summary(results9)$show())
        results9CodeBased <- eval(parse(text = getObjectRCode(results9, stringWrapParagraphWidth = NULL)))
        expect_equal(results9CodeBased$thetaH1, results9$thetaH1, tolerance = 1e-06)
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
    # @refFS[Formula]{fs:testStatisticMultiArmSurvival}
    results10 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Dunnett", nPlanned = c(20), directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results10' with expected results
    expect_equal(results10$thetaH1[1, ], 0.47557061, tolerance = 1e-06, label = paste0("c(", paste0(results10$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results10$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results10$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results10$conditionalRejectionProbabilities[1, ], c(0.16552026, 0.53357256, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results10$conditionalRejectionProbabilities[2, ], c(0.16552026, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results10$conditionalPower[1, ], c(NA_real_, NA_real_, 0.9596109), tolerance = 1e-06, label = paste0("c(", paste0(results10$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results10$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results10$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results10$repeatedConfidenceIntervalLowerBounds[1, ], c(0.14531321, 0.23837118, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results10$repeatedConfidenceIntervalLowerBounds[2, ], c(0.11369998, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results10$repeatedConfidenceIntervalUpperBounds[1, ], c(1.1839595, 0.910836, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results10$repeatedConfidenceIntervalUpperBounds[2, ], c(1.3471665, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results10$repeatedPValues[1, ], c(0.077362906, 0.0096216473, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results10$repeatedPValues[2, ], c(0.077362906, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results10$repeatedPValues[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results10), NA)))
        expect_output(print(results10)$show())
        invisible(capture.output(expect_error(summary(results10), NA)))
        expect_output(summary(results10)$show())
        results10CodeBased <- eval(parse(text = getObjectRCode(results10, stringWrapParagraphWidth = NULL)))
        expect_equal(results10CodeBased$thetaH1, results10$thetaH1, tolerance = 1e-06)
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
    # @refFS[Formula]{fs:testStatisticMultiArmSurvival}
    results11 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Simes", nPlanned = c(20), directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results11' with expected results
    expect_equal(results11$thetaH1[1, ], 0.47557061, tolerance = 1e-06, label = paste0("c(", paste0(results11$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results11$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results11$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results11$conditionalRejectionProbabilities[1, ], c(0.17669226, 0.55323067, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results11$conditionalRejectionProbabilities[2, ], c(0.17669226, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results11$conditionalPower[1, ], c(NA_real_, NA_real_, 0.96373388), tolerance = 1e-06, label = paste0("c(", paste0(results11$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results11$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results11$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results11$repeatedConfidenceIntervalLowerBounds[1, ], c(0.14436219, 0.23385274, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results11$repeatedConfidenceIntervalLowerBounds[2, ], c(0.11282345, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results11$repeatedConfidenceIntervalUpperBounds[1, ], c(1.1917585, 0.91883308, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results11$repeatedConfidenceIntervalUpperBounds[2, ], c(1.3576306, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results11$repeatedPValues[1, ], c(0.069951918, 0.0087766935, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results11$repeatedPValues[2, ], c(0.069951918, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results11$repeatedPValues[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results11), NA)))
        expect_output(print(results11)$show())
        invisible(capture.output(expect_error(summary(results11), NA)))
        expect_output(summary(results11)$show())
        results11CodeBased <- eval(parse(text = getObjectRCode(results11, stringWrapParagraphWidth = NULL)))
        expect_equal(results11CodeBased$thetaH1, results11$thetaH1, tolerance = 1e-06)
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
    # @refFS[Formula]{fs:testStatisticMultiArmSurvival}
    results12 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Sidak", nPlanned = c(20), directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results12' with expected results
    expect_equal(results12$thetaH1[1, ], 0.47557061, tolerance = 1e-06, label = paste0("c(", paste0(results12$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results12$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results12$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results12$conditionalRejectionProbabilities[1, ], c(0.15801679, 0.51979239, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results12$conditionalRejectionProbabilities[2, ], c(0.15801679, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results12$conditionalPower[1, ], c(NA_real_, NA_real_, 0.9565118), tolerance = 1e-06, label = paste0("c(", paste0(results12$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results12$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results12$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results12$repeatedConfidenceIntervalLowerBounds[1, ], c(0.14440308, 0.23494562, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results12$repeatedConfidenceIntervalLowerBounds[2, ], c(0.11286087, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results12$repeatedConfidenceIntervalUpperBounds[1, ], c(1.1914212, 0.91784736, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results12$repeatedConfidenceIntervalUpperBounds[2, ], c(1.3571775, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results12$repeatedPValues[1, ], c(0.082919001, 0.010252978, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results12$repeatedPValues[2, ], c(0.082919001, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results12$repeatedPValues[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results12), NA)))
        expect_output(print(results12)$show())
        invisible(capture.output(expect_error(summary(results12), NA)))
        expect_output(summary(results12)$show())
        results12CodeBased <- eval(parse(text = getObjectRCode(results12, stringWrapParagraphWidth = NULL)))
        expect_equal(results12CodeBased$thetaH1, results12$thetaH1, tolerance = 1e-06)
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
    # @refFS[Formula]{fs:testStatisticMultiArmSurvival}
    results13 <- getAnalysisResults(
        design = design1, dataInput = dataExample2,
        intersectionTest = "Bonferroni", nPlanned = c(20), directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results13' with expected results
    expect_equal(results13$thetaH1[1, ], 0.47557061, tolerance = 1e-06, label = paste0("c(", paste0(results13$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results13$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results13$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results13$conditionalRejectionProbabilities[1, ], c(0.15727093, 0.51839597, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results13$conditionalRejectionProbabilities[2, ], c(0.15727093, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results13$conditionalPower[1, ], c(NA_real_, NA_real_, 0.95618769), tolerance = 1e-06, label = paste0("c(", paste0(results13$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results13$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results13$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results13$repeatedConfidenceIntervalLowerBounds[1, ], c(0.14436219, 0.23385274, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results13$repeatedConfidenceIntervalLowerBounds[2, ], c(0.11282345, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results13$repeatedConfidenceIntervalUpperBounds[1, ], c(1.1917585, 0.91883308, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results13$repeatedConfidenceIntervalUpperBounds[2, ], c(1.3576306, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results13$repeatedPValues[1, ], c(0.083499788, 0.010318782, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results13$repeatedPValues[2, ], c(0.083499788, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results13$repeatedPValues[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results13), NA)))
        expect_output(print(results13)$show())
        invisible(capture.output(expect_error(summary(results13), NA)))
        expect_output(summary(results13)$show())
        results13CodeBased <- eval(parse(text = getObjectRCode(results13, stringWrapParagraphWidth = NULL)))
        expect_equal(results13CodeBased$thetaH1, results13$thetaH1, tolerance = 1e-06)
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
    # @refFS[Formula]{fs:testStatisticMultiArmSurvival}
    results14 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Dunnett", nPlanned = c(20), seed = 1234, iterations = 1000, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results14' with expected results
    expect_equal(results14$thetaH1[1, ], 0.47557061, tolerance = 1e-06, label = paste0("c(", paste0(results14$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results14$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results14$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results14$conditionalRejectionProbabilities[1, ], c(0.10966406, 1, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results14$conditionalRejectionProbabilities[2, ], c(0.10966406, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results14$conditionalPower[1, ], c(NA_real_, NA_real_, 0.93227723), tolerance = 1e-06, label = paste0("c(", paste0(results14$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results14$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results14$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results14$repeatedConfidenceIntervalLowerBounds[1, ], c(0.15690927, 0.23734664, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results14$repeatedConfidenceIntervalLowerBounds[2, ], c(0.12446726, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results14$repeatedConfidenceIntervalUpperBounds[1, ], c(1.0964607, 0.93860984, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results14$repeatedConfidenceIntervalUpperBounds[2, ], c(1.2306243, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results14$repeatedPValues[1, ], c(0.04389568, 0.013378163, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results14$repeatedPValues[2, ], c(0.04389568, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results14$repeatedPValues[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results14), NA)))
        expect_output(print(results14)$show())
        invisible(capture.output(expect_error(summary(results14), NA)))
        expect_output(summary(results14)$show())
        results14CodeBased <- eval(parse(text = getObjectRCode(results14, stringWrapParagraphWidth = NULL)))
        expect_equal(results14CodeBased$thetaH1, results14$thetaH1, tolerance = 1e-06)
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
    # @refFS[Formula]{fs:testStatisticMultiArmSurvival}
    results15 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Simes", nPlanned = c(20), seed = 1234, iterations = 1000, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results15' with expected results
    expect_equal(results15$thetaH1[1, ], 0.47557061, tolerance = 1e-06, label = paste0("c(", paste0(results15$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results15$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results15$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results15$conditionalRejectionProbabilities[1, ], c(0.1211541, 1, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results15$conditionalRejectionProbabilities[2, ], c(0.1211541, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results15$conditionalPower[1, ], c(NA_real_, NA_real_, 0.94819096), tolerance = 1e-06, label = paste0("c(", paste0(results15$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results15$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results15$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results15$repeatedConfidenceIntervalLowerBounds[1, ], c(0.15555937, 0.23393056, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results15$repeatedConfidenceIntervalLowerBounds[2, ], c(0.12320632, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results15$repeatedConfidenceIntervalUpperBounds[1, ], c(1.1059766, 0.94623115, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results15$repeatedConfidenceIntervalUpperBounds[2, ], c(1.2432202, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results15$repeatedPValues[1, ], c(0.039924588, 0.01222708, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results15$repeatedPValues[2, ], c(0.039924588, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results15$repeatedPValues[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results15), NA)))
        expect_output(print(results15)$show())
        invisible(capture.output(expect_error(summary(results15), NA)))
        expect_output(summary(results15)$show())
        results15CodeBased <- eval(parse(text = getObjectRCode(results15, stringWrapParagraphWidth = NULL)))
        expect_equal(results15CodeBased$thetaH1, results15$thetaH1, tolerance = 1e-06)
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
    # @refFS[Formula]{fs:testStatisticMultiArmSurvival}
    results16 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Sidak", nPlanned = c(20), seed = 1234, iterations = 1000, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results16' with expected results
    expect_equal(results16$thetaH1[1, ], 0.47557061, tolerance = 1e-06, label = paste0("c(", paste0(results16$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results16$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results16$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results16$conditionalRejectionProbabilities[1, ], c(0.1023739, 1, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results16$conditionalRejectionProbabilities[2, ], c(0.1023739, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results16$conditionalPower[1, ], c(NA_real_, NA_real_, 0.92036569), tolerance = 1e-06, label = paste0("c(", paste0(results16$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results16$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results16$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results16$repeatedConfidenceIntervalLowerBounds[1, ], c(0.15563938, 0.23475813, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results16$repeatedConfidenceIntervalLowerBounds[2, ], c(0.1232811, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results16$repeatedConfidenceIntervalUpperBounds[1, ], c(1.1054079, 0.94538806, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results16$repeatedConfidenceIntervalUpperBounds[2, ], c(1.2424668, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results16$repeatedPValues[1, ], c(0.046853018, 0.014230746, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results16$repeatedPValues[2, ], c(0.046853018, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results16$repeatedPValues[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results16), NA)))
        expect_output(print(results16)$show())
        invisible(capture.output(expect_error(summary(results16), NA)))
        expect_output(summary(results16)$show())
        results16CodeBased <- eval(parse(text = getObjectRCode(results16, stringWrapParagraphWidth = NULL)))
        expect_equal(results16CodeBased$thetaH1, results16$thetaH1, tolerance = 1e-06)
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
    # @refFS[Formula]{fs:testStatisticMultiArmSurvival}
    results17 <- getAnalysisResults(
        design = design2, dataInput = dataExample2,
        intersectionTest = "Bonferroni", nPlanned = c(20), seed = 1234, iterations = 1000, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsMultiArmFisher object 'results17' with expected results
    expect_equal(results17$thetaH1[1, ], 0.47557061, tolerance = 1e-06, label = paste0("c(", paste0(results17$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results17$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results17$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results17$conditionalRejectionProbabilities[1, ], c(0.10166729, 1, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results17$conditionalRejectionProbabilities[2, ], c(0.10166729, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results17$conditionalPower[1, ], c(NA_real_, NA_real_, 0.91912747), tolerance = 1e-06, label = paste0("c(", paste0(results17$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results17$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(results17$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results17$repeatedConfidenceIntervalLowerBounds[1, ], c(0.15555937, 0.23393056, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results17$repeatedConfidenceIntervalLowerBounds[2, ], c(0.12320632, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results17$repeatedConfidenceIntervalUpperBounds[1, ], c(1.1059766, 0.94623115, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results17$repeatedConfidenceIntervalUpperBounds[2, ], c(1.2432202, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results17$repeatedPValues[1, ], c(0.047161054, 0.014319438, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results17$repeatedPValues[2, ], c(0.047161054, NA_real_, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(results17$repeatedPValues[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results17), NA)))
        expect_output(print(results17)$show())
        invisible(capture.output(expect_error(summary(results17), NA)))
        expect_output(summary(results17)$show())
        results17CodeBased <- eval(parse(text = getObjectRCode(results17, stringWrapParagraphWidth = NULL)))
        expect_equal(results17CodeBased$thetaH1, results17$thetaH1, tolerance = 1e-06)
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
    # @refFS[Formula]{fs:testStatisticMultiArmSurvival}
    results18 <- getAnalysisResults(
        design = design3, dataInput = dataExample2,
        intersectionTest = "Dunnett", directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsConditionalDunnett object 'results18' with expected results
    expect_equal(results18$thetaH1[1, ], 0.47557061, tolerance = 1e-06, label = paste0("c(", paste0(results18$thetaH1[1, ], collapse = ", "), ")"))
    expect_equal(results18$thetaH1[2, ], NA_real_, label = paste0("c(", paste0(results18$thetaH1[2, ], collapse = ", "), ")"))
    expect_equal(results18$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.20921311), tolerance = 1e-06, label = paste0("c(", paste0(results18$conditionalRejectionProbabilities[1, ], collapse = ", "), ")"))
    expect_equal(results18$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.18260705), tolerance = 1e-06, label = paste0("c(", paste0(results18$conditionalRejectionProbabilities[2, ], collapse = ", "), ")"))
    expect_equal(results18$conditionalPower[1, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results18$conditionalPower[1, ], collapse = ", "), ")"))
    expect_equal(results18$conditionalPower[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results18$conditionalPower[2, ], collapse = ", "), ")"))
    expect_equal(results18$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, 0.27471638), tolerance = 1e-06, label = paste0("c(", paste0(results18$repeatedConfidenceIntervalLowerBounds[1, ], collapse = ", "), ")"))
    expect_equal(results18$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results18$repeatedConfidenceIntervalLowerBounds[2, ], collapse = ", "), ")"))
    expect_equal(results18$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, 0.81629276), tolerance = 1e-06, label = paste0("c(", paste0(results18$repeatedConfidenceIntervalUpperBounds[1, ], collapse = ", "), ")"))
    expect_equal(results18$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results18$repeatedConfidenceIntervalUpperBounds[2, ], collapse = ", "), ")"))
    expect_equal(results18$repeatedPValues[1, ], c(NA_real_, 0.0032883088), tolerance = 1e-06, label = paste0("c(", paste0(results18$repeatedPValues[1, ], collapse = ", "), ")"))
    expect_equal(results18$repeatedPValues[2, ], c(NA_real_, NA_real_), label = paste0("c(", paste0(results18$repeatedPValues[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(results18), NA)))
        expect_output(print(results18)$show())
        invisible(capture.output(expect_error(summary(results18), NA)))
        expect_output(summary(results18)$show())
        results18CodeBased <- eval(parse(text = getObjectRCode(results18, stringWrapParagraphWidth = NULL)))
        expect_equal(results18CodeBased$thetaH1, results18$thetaH1, tolerance = 1e-06)
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
})
