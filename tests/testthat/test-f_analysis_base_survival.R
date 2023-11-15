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
## |  File name: test-f_analysis_base_survival.R
## |  Creation date: 08 November 2023, 08:52:36
## |  File version: $Revision: 7403 $
## |  Last changed: $Date: 2023-11-08 16:12:00 +0100 (Mi, 08 Nov 2023) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing the Analysis Survival Functionality for the Group Sequential Design")


test_that("'getAnalysisResults' for a two-stage group sequential design and survival data", {
    .skipTestIfDisabled()

    design0 <- getDesignGroupSequential(
        kMax = 2, alpha = 0.025,
        informationRates = c(0.4, 1), bindingFutility = TRUE,
        typeOfDesign = "WT", deltaWT = 0.25, futilityBounds = 0
    )

    dataExample0 <- getDataset(
        overallEvents = c(8, 20),
        overallLogRanks = c(1.92, 2.1)
    )

    # @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
    # @refFS[Formula]{fs:testStatisticSurvival}
    # @refFS[Formula]{fs:pValuesSurvivalAlternativeGreater}
    # @refFS[Formula]{fs:definitionRCIInverseNormal}
    # @refFS[Formula]{fs:calculationRepeatedpValue}
    # @refFS[Formula]{fs:finalCISurvival}
    # @refFS[Formula]{fs:medianUnbiasedEstimate}
    x0 <- getAnalysisResults(design0, dataExample0, directionUpper = TRUE)

    ## Comparison of the results of AnalysisResultsGroupSequential object 'x0' with expected results
    expect_equal(x0$thetaH1, 2.5578027, tolerance = 1e-06, label = paste0("c(", paste0(x0$thetaH1, collapse = ", "), ")"))
    expect_equal(x0$testActions, c("continue", "reject"), label = paste0("c(", paste0(x0$testActions, collapse = ", "), ")"))
    expect_equal(x0$conditionalRejectionProbabilities, c(0.15200046, NA_real_), tolerance = 1e-06, label = paste0("c(", paste0(x0$conditionalRejectionProbabilities, collapse = ", "), ")"))
    expect_equal(x0$conditionalPower, c(NA_real_, NA_real_), label = paste0("c(", paste0(x0$conditionalPower, collapse = ", "), ")"))
    expect_equal(x0$repeatedConfidenceIntervalLowerBounds, c(0.65051922, 1.04083), tolerance = 1e-06, label = paste0("c(", paste0(x0$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x0$repeatedConfidenceIntervalUpperBounds, c(23.22605, 6.2857086), tolerance = 1e-06, label = paste0("c(", paste0(x0$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x0$repeatedPValues, c(0.074184316, 0.019962317), tolerance = 1e-06, label = paste0("c(", paste0(x0$repeatedPValues, collapse = ", "), ")"))
    expect_equal(x0$finalStage, 2, label = paste0("c(", paste0(x0$finalStage, collapse = ", "), ")"))
    expect_equal(x0$finalPValues, c(NA_real_, 0.021122043), tolerance = 1e-06, label = paste0("c(", paste0(x0$finalPValues, collapse = ", "), ")"))
    expect_equal(x0$finalConfidenceIntervalLowerBounds, c(NA_real_, 1.0341796), tolerance = 1e-06, label = paste0("c(", paste0(x0$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x0$finalConfidenceIntervalUpperBounds, c(NA_real_, 6.2409205), tolerance = 1e-06, label = paste0("c(", paste0(x0$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x0$medianUnbiasedEstimates, c(NA_real_, 2.5476534), tolerance = 1e-06, label = paste0("c(", paste0(x0$medianUnbiasedEstimates, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x0), NA)))
        expect_output(print(x0)$show())
        invisible(capture.output(expect_error(summary(x0), NA)))
        expect_output(summary(x0)$show())
        x0CodeBased <- eval(parse(text = getObjectRCode(x0, stringWrapParagraphWidth = NULL)))
        expect_equal(x0CodeBased$thetaH1, x0$thetaH1, tolerance = 1e-06)
        expect_equal(x0CodeBased$testActions, x0$testActions, tolerance = 1e-06)
        expect_equal(x0CodeBased$conditionalRejectionProbabilities, x0$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(x0CodeBased$conditionalPower, x0$conditionalPower, tolerance = 1e-06)
        expect_equal(x0CodeBased$repeatedConfidenceIntervalLowerBounds, x0$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(x0CodeBased$repeatedConfidenceIntervalUpperBounds, x0$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(x0CodeBased$repeatedPValues, x0$repeatedPValues, tolerance = 1e-06)
        expect_equal(x0CodeBased$finalStage, x0$finalStage, tolerance = 1e-06)
        expect_equal(x0CodeBased$finalPValues, x0$finalPValues, tolerance = 1e-06)
        expect_equal(x0CodeBased$finalConfidenceIntervalLowerBounds, x0$finalConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(x0CodeBased$finalConfidenceIntervalUpperBounds, x0$finalConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(x0CodeBased$medianUnbiasedEstimates, x0$medianUnbiasedEstimates, tolerance = 1e-06)
        expect_type(names(x0), "character")
        df <- as.data.frame(x0)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x0)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getAnalysisResults' for a three-stage group sequential design and survival data", {
    .skipTestIfDisabled()

    design1 <- getDesignGroupSequential(
        kMax = 3, alpha = 0.025,
        informationRates = c(0.2, 0.4, 1), bindingFutility = FALSE,
        typeOfDesign = "WT", deltaWT = 0.25, futilityBounds = c(0, 0)
    )

    dataExample1 <- getDataset(
        overallEvents = c(8, 15, 38),
        overallAllocationRatios = c(1, 1, 1),
        overallLogRanks = c(1.52, 1.38, 2.9)
    )

    # @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
    # @refFS[Formula]{fs:testStatisticSurvival}
    # @refFS[Formula]{fs:pValuesSurvivalAlternativeGreater}
    # @refFS[Formula]{fs:definitionRCIInverseNormal}
    # @refFS[Formula]{fs:calculationRepeatedpValue}
    x1 <- getAnalysisResults(design1, dataExample1, directionUpper = TRUE)

    ## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
    expect_equal(x1$thetaH1, 2.5622461, tolerance = 1e-07, label = paste0("c(", paste0(x1$thetaH1, collapse = ", "), ")"))
    expect_equal(x1$testActions, c("continue", "continue", "reject"), label = paste0("c(", paste0(x1$testActions, collapse = ", "), ")"))
    expect_equal(x1$conditionalRejectionProbabilities, c(0.076909306, 0.067473058, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities, collapse = ", "), ")"))
    expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$conditionalPower, collapse = ", "), ")"))
    expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.34217973, 0.54553509, 1.325822), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(25.078822, 7.6235796, 4.9517237), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x1$repeatedPValues, c(0.22249182, 0.19345822, 0.0019646115), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues, collapse = ", "), ")"))
    expect_equal(x1$finalStage, 3, label = paste0("c(", paste0(x1$finalStage, collapse = ", "), ")"))
    expect_equal(x1$finalPValues, c(NA_real_, NA_real_, 0.0074535505), tolerance = 1e-07, label = paste0("c(", paste0(x1$finalPValues, collapse = ", "), ")"))
    expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 1.222663), tolerance = 1e-07, label = paste0("c(", paste0(x1$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 4.752454), tolerance = 1e-07, label = paste0("c(", paste0(x1$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, 2.4764002), tolerance = 1e-07, label = paste0("c(", paste0(x1$medianUnbiasedEstimates, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x1), NA)))
        expect_output(print(x1)$show())
        invisible(capture.output(expect_error(summary(x1), NA)))
        expect_output(summary(x1)$show())
        x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
        expect_equal(x1CodeBased$thetaH1, x1$thetaH1, tolerance = 1e-07)
        expect_equal(x1CodeBased$testActions, x1$testActions, tolerance = 1e-07)
        expect_equal(x1CodeBased$conditionalRejectionProbabilities, x1$conditionalRejectionProbabilities, tolerance = 1e-07)
        expect_equal(x1CodeBased$conditionalPower, x1$conditionalPower, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedConfidenceIntervalLowerBounds, x1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedConfidenceIntervalUpperBounds, x1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedPValues, x1$repeatedPValues, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalStage, x1$finalStage, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalPValues, x1$finalPValues, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalConfidenceIntervalLowerBounds, x1$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalConfidenceIntervalUpperBounds, x1$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$medianUnbiasedEstimates, x1$medianUnbiasedEstimates, tolerance = 1e-07)
        expect_type(names(x1), "character")
        df <- as.data.frame(x1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
    # @refFS[Formula]{fs:testStatisticSurvival}
    # @refFS[Formula]{fs:pValuesSurvivalAlternativeGreater}
    # @refFS[Formula]{fs:definitionRCIInverseNormal}
    # @refFS[Formula]{fs:calculationRepeatedpValue}
    x2 <- getAnalysisResults(design1, dataExample1,
        stage = 2, nPlanned = 40,
        allocationRatioPlanned = 2, thetaH1 = 2, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsGroupSequential object 'x2' with expected results
    expect_equal(x2$testActions, c("continue", "continue", NA_character_), label = paste0("c(", paste0(x2$testActions, collapse = ", "), ")"))
    expect_equal(x2$conditionalRejectionProbabilities, c(0.076909306, 0.067473058, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities, collapse = ", "), ")"))
    expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, 0.70906065), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalPower, collapse = ", "), ")"))
    expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.34217973, 0.54553509, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(25.078822, 7.6235796, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x2$repeatedPValues, c(0.22249182, 0.19345822, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues, collapse = ", "), ")"))
    expect_equal(x2$finalStage, NA_integer_, label = paste0("c(", paste0(x2$finalStage, collapse = ", "), ")"))
    expect_equal(x2$finalPValues, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalPValues, collapse = ", "), ")"))
    expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$medianUnbiasedEstimates, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x2), NA)))
        expect_output(print(x2)$show())
        invisible(capture.output(expect_error(summary(x2), NA)))
        expect_output(summary(x2)$show())
        x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
        expect_equal(x2CodeBased$testActions, x2$testActions, tolerance = 1e-07)
        expect_equal(x2CodeBased$conditionalRejectionProbabilities, x2$conditionalRejectionProbabilities, tolerance = 1e-07)
        expect_equal(x2CodeBased$conditionalPower, x2$conditionalPower, tolerance = 1e-07)
        expect_equal(x2CodeBased$repeatedConfidenceIntervalLowerBounds, x2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$repeatedConfidenceIntervalUpperBounds, x2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$repeatedPValues, x2$repeatedPValues, tolerance = 1e-07)
        expect_equal(x2CodeBased$finalStage, x2$finalStage, tolerance = 1e-07)
        expect_equal(x2CodeBased$finalPValues, x2$finalPValues, tolerance = 1e-07)
        expect_equal(x2CodeBased$finalConfidenceIntervalLowerBounds, x2$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$finalConfidenceIntervalUpperBounds, x2$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$medianUnbiasedEstimates, x2$medianUnbiasedEstimates, tolerance = 1e-07)
        expect_type(names(x2), "character")
        df <- as.data.frame(x2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    plotData1 <- testGetAnalysisResultsPlotData(x2, thetaRange = seq(1, 2.5, 0.05))

    ## Comparison of the results of list object 'plotData1' with expected results
    expect_equal(plotData1$xValues, c(1, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35, 1.4, 1.45, 1.5, 1.55, 1.6, 1.65, 1.7, 1.75, 1.8, 1.85, 1.9, 1.95, 2, 2.05, 2.1, 2.15, 2.2, 2.25, 2.3, 2.35, 2.4, 2.45, 2.5), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$xValues, collapse = ", "), ")"))
    expect_equal(plotData1$condPowerValues, c(0.06476941, 0.085271856, 0.10901882, 0.13583313, 0.16543943, 0.19748538, 0.23156461, 0.26723929, 0.30406079, 0.34158746, 0.37939899, 0.41710731, 0.45436408, 0.49086519, 0.52635279, 0.5606151, 0.59348472, 0.62483573, 0.65458006, 0.68266335, 0.70906065, 0.73377215, 0.75681902, 0.77823954, 0.79808559, 0.81641944, 0.83331101, 0.84883539, 0.86307085, 0.87609709, 0.88799385), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$condPowerValues, collapse = ", "), ")"))
    expect_equal(plotData1$likelihoodValues, c(0.38589113, 0.43767503, 0.48942229, 0.54046191, 0.59019718, 0.63811141, 0.6837696, 0.72681695, 0.76697506, 0.8040366, 0.83785893, 0.86835727, 0.89549763, 0.91928996, 0.93978142, 0.95705021, 0.97119988, 0.98235405, 0.99065189, 0.99624395, 0.99928862, 0.99994909, 0.9983907, 0.99477877, 0.98927675, 0.98204476, 0.97323838, 0.96300773, 0.95149676, 0.93884271, 0.92517582), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$likelihoodValues, collapse = ", "), ")"))
    expect_equal(plotData1$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData1$main, collapse = ", "), ")"))
    expect_equal(plotData1$xlab, "Hazard ratio", label = paste0("c(", paste0(plotData1$xlab, collapse = ", "), ")"))
    expect_equal(plotData1$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData1$ylab, collapse = ", "), ")"))
    expect_equal(plotData1$sub, "Stage = 2, maximum number of remaining events = 40, allocation ratio = 2", label = paste0("c(", paste0(plotData1$sub, collapse = ", "), ")"))

    # @refFS[Tab.]{fs:tab:output:getAnalysisResultsGroupSequential}
    # @refFS[Formula]{fs:testStatisticSurvival}
    # @refFS[Formula]{fs:pValuesSurvivalAlternativeGreater}
    # @refFS[Formula]{fs:definitionRCIInverseNormal}
    # @refFS[Formula]{fs:calculationRepeatedpValue}
    x3 <- getAnalysisResults(design1, dataExample1,
        thetaH0 = 0.95, stage = 2,
        nPlanned = 40, allocationRatioPlanned = 2, thetaH1 = 2, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsGroupSequential object 'x3' with expected results
    expect_equal(x3$testActions, c("continue", "continue", NA_character_), label = paste0("c(", paste0(x3$testActions, collapse = ", "), ")"))
    expect_equal(x3$conditionalRejectionProbabilities, c(0.083820262, 0.07871372, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities, collapse = ", "), ")"))
    expect_equal(x3$conditionalPower, c(NA_real_, NA_real_, 0.78366367), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalPower, collapse = ", "), ")"))
    expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(0.34217973, 0.54553509, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(25.078822, 7.6235796, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x3$repeatedPValues, c(0.20477831, 0.16773576, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues, collapse = ", "), ")"))
    expect_equal(x3$finalStage, NA_integer_, label = paste0("c(", paste0(x3$finalStage, collapse = ", "), ")"))
    expect_equal(x3$finalPValues, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalPValues, collapse = ", "), ")"))
    expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$medianUnbiasedEstimates, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x3), NA)))
        expect_output(print(x3)$show())
        invisible(capture.output(expect_error(summary(x3), NA)))
        expect_output(summary(x3)$show())
        x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
        expect_equal(x3CodeBased$testActions, x3$testActions, tolerance = 1e-07)
        expect_equal(x3CodeBased$conditionalRejectionProbabilities, x3$conditionalRejectionProbabilities, tolerance = 1e-07)
        expect_equal(x3CodeBased$conditionalPower, x3$conditionalPower, tolerance = 1e-07)
        expect_equal(x3CodeBased$repeatedConfidenceIntervalLowerBounds, x3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x3CodeBased$repeatedConfidenceIntervalUpperBounds, x3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x3CodeBased$repeatedPValues, x3$repeatedPValues, tolerance = 1e-07)
        expect_equal(x3CodeBased$finalStage, x3$finalStage, tolerance = 1e-07)
        expect_equal(x3CodeBased$finalPValues, x3$finalPValues, tolerance = 1e-07)
        expect_equal(x3CodeBased$finalConfidenceIntervalLowerBounds, x3$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x3CodeBased$finalConfidenceIntervalUpperBounds, x3$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x3CodeBased$medianUnbiasedEstimates, x3$medianUnbiasedEstimates, tolerance = 1e-07)
        expect_type(names(x3), "character")
        df <- as.data.frame(x3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    plotData2 <- testGetAnalysisResultsPlotData(x3, thetaRange = seq(1, 2.5, 0.05))

    ## Comparison of the results of list object 'plotData2' with expected results
    expect_equal(plotData2$xValues, c(1, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35, 1.4, 1.45, 1.5, 1.55, 1.6, 1.65, 1.7, 1.75, 1.8, 1.85, 1.9, 1.95, 2, 2.05, 2.1, 2.15, 2.2, 2.25, 2.3, 2.35, 2.4, 2.45, 2.5), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$xValues, collapse = ", "), ")"))
    expect_equal(plotData2$condPowerValues, c(0.099931978, 0.12787889, 0.15919322, 0.19345089, 0.23014743, 0.26873157, 0.30863607, 0.34930399, 0.39020957, 0.43087349, 0.47087287, 0.50984669, 0.54749733, 0.58358921, 0.61794519, 0.65044149, 0.6810018, 0.70959089, 0.73620831, 0.7608822, 0.78366367, 0.80462154, 0.82383789, 0.841404, 0.85741704, 0.87197725, 0.88518567, 0.8971423, 0.90794467, 0.91768682, 0.92645845), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$condPowerValues, collapse = ", "), ")"))
    expect_equal(plotData2$likelihoodValues, c(0.38589113, 0.43767503, 0.48942229, 0.54046191, 0.59019718, 0.63811141, 0.6837696, 0.72681695, 0.76697506, 0.8040366, 0.83785893, 0.86835727, 0.89549763, 0.91928996, 0.93978142, 0.95705021, 0.97119988, 0.98235405, 0.99065189, 0.99624395, 0.99928862, 0.99994909, 0.9983907, 0.99477877, 0.98927675, 0.98204476, 0.97323838, 0.96300773, 0.95149676, 0.93884271, 0.92517582), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$likelihoodValues, collapse = ", "), ")"))
    expect_equal(plotData2$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData2$main, collapse = ", "), ")"))
    expect_equal(plotData2$xlab, "Hazard ratio", label = paste0("c(", paste0(plotData2$xlab, collapse = ", "), ")"))
    expect_equal(plotData2$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData2$ylab, collapse = ", "), ")"))
    expect_equal(plotData2$sub, "Stage = 2, maximum number of remaining events = 40, allocation ratio = 2", label = paste0("c(", paste0(plotData2$sub, collapse = ", "), ")"))
})

test_that("'getAnalysisResults' for a three-stage ggroup sequential design and survival data ('directionUpper' reversed)", {
    .skipTestIfDisabled()

    design2 <- getDesignGroupSequential(
        kMax = 3, alpha = 0.025,
        informationRates = c(0.2, 0.4, 1), bindingFutility = FALSE,
        typeOfDesign = "WT", deltaWT = 0.25, futilityBounds = c(0, 0)
    )

    dataExample2 <- getDataset(
        overallEvents = c(8, 15, 40),
        overallAllocationRatios = c(1, 1, 1),
        overallLogRanks = -c(1.52, 1.38, 2.9)
    )

    # @refFS[Tab.]{fs:tab:output:getAnalysisResultsInverseNormal}
    # @refFS[Formula]{fs:testStatisticSurvival}
    # @refFS[Formula]{fs:pValuesSurvivalAlternativeSmaller}
    # @refFS[Formula]{fs:definitionRCIInverseNormal}
    # @refFS[Formula]{fs:calculationRepeatedpValue}
    x1 <- getAnalysisResults(design2, dataExample2, directionUpper = FALSE)

    ## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
    expect_equal(x1$thetaH1, 0.3996922, tolerance = 1e-07, label = paste0("c(", paste0(x1$thetaH1, collapse = ", "), ")"))
    expect_equal(x1$testActions, c("continue", "continue", "reject"), label = paste0("c(", paste0(x1$testActions, collapse = ", "), ")"))
    expect_equal(x1$conditionalRejectionProbabilities, c(0.076909306, 0.067473058, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities, collapse = ", "), ")"))
    expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$conditionalPower, collapse = ", "), ")"))
    expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.039874281, 0.13117197, 0.21029804), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(2.9224407, 1.8330627, 0.75965452), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x1$repeatedPValues, c(0.22249182, 0.19345822, 0.0019646115), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues, collapse = ", "), ")"))
    expect_equal(x1$finalStage, 3, label = paste0("c(", paste0(x1$finalStage, collapse = ", "), ")"))
    expect_equal(x1$finalPValues, c(NA_real_, NA_real_, 0.0074535505), tolerance = 1e-07, label = paste0("c(", paste0(x1$finalPValues, collapse = ", "), ")"))
    expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.21888803), tolerance = 1e-07, label = paste0("c(", paste0(x1$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.82206073), tolerance = 1e-07, label = paste0("c(", paste0(x1$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.41319107), tolerance = 1e-07, label = paste0("c(", paste0(x1$medianUnbiasedEstimates, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x1), NA)))
        expect_output(print(x1)$show())
        invisible(capture.output(expect_error(summary(x1), NA)))
        expect_output(summary(x1)$show())
        x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
        expect_equal(x1CodeBased$thetaH1, x1$thetaH1, tolerance = 1e-07)
        expect_equal(x1CodeBased$testActions, x1$testActions, tolerance = 1e-07)
        expect_equal(x1CodeBased$conditionalRejectionProbabilities, x1$conditionalRejectionProbabilities, tolerance = 1e-07)
        expect_equal(x1CodeBased$conditionalPower, x1$conditionalPower, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedConfidenceIntervalLowerBounds, x1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedConfidenceIntervalUpperBounds, x1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedPValues, x1$repeatedPValues, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalStage, x1$finalStage, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalPValues, x1$finalPValues, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalConfidenceIntervalLowerBounds, x1$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalConfidenceIntervalUpperBounds, x1$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$medianUnbiasedEstimates, x1$medianUnbiasedEstimates, tolerance = 1e-07)
        expect_type(names(x1), "character")
        df <- as.data.frame(x1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getAnalysisResultsInverseNormal}
    # @refFS[Formula]{fs:testStatisticSurvival}
    # @refFS[Formula]{fs:pValuesSurvivalAlternativeSmaller}
    # @refFS[Formula]{fs:definitionRCIInverseNormal}
    # @refFS[Formula]{fs:calculationRepeatedpValue}
    x2 <- getAnalysisResults(design2, dataExample2,
        thetaH0 = 1.1, stage = 2,
        nPlanned = 40, allocationRatioPlanned = 0.5, thetaH1 = 0.5, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsGroupSequential object 'x2' with expected results
    expect_equal(x2$testActions, c("continue", "continue", NA_character_), label = paste0("c(", paste0(x2$testActions, collapse = ", "), ")"))
    expect_equal(x2$conditionalRejectionProbabilities, c(0.090220506, 0.08944509, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities, collapse = ", "), ")"))
    expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, 0.83779047), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalPower, collapse = ", "), ")"))
    expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.039874281, 0.13117197, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(2.9224407, 1.8330627, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x2$repeatedPValues, c(0.19034734, 0.14768766, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues, collapse = ", "), ")"))
    expect_equal(x2$finalStage, NA_integer_, label = paste0("c(", paste0(x2$finalStage, collapse = ", "), ")"))
    expect_equal(x2$finalPValues, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalPValues, collapse = ", "), ")"))
    expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$medianUnbiasedEstimates, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x2), NA)))
        expect_output(print(x2)$show())
        invisible(capture.output(expect_error(summary(x2), NA)))
        expect_output(summary(x2)$show())
        x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
        expect_equal(x2CodeBased$testActions, x2$testActions, tolerance = 1e-07)
        expect_equal(x2CodeBased$conditionalRejectionProbabilities, x2$conditionalRejectionProbabilities, tolerance = 1e-07)
        expect_equal(x2CodeBased$conditionalPower, x2$conditionalPower, tolerance = 1e-07)
        expect_equal(x2CodeBased$repeatedConfidenceIntervalLowerBounds, x2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$repeatedConfidenceIntervalUpperBounds, x2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$repeatedPValues, x2$repeatedPValues, tolerance = 1e-07)
        expect_equal(x2CodeBased$finalStage, x2$finalStage, tolerance = 1e-07)
        expect_equal(x2CodeBased$finalPValues, x2$finalPValues, tolerance = 1e-07)
        expect_equal(x2CodeBased$finalConfidenceIntervalLowerBounds, x2$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$finalConfidenceIntervalUpperBounds, x2$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$medianUnbiasedEstimates, x2$medianUnbiasedEstimates, tolerance = 1e-07)
        expect_type(names(x2), "character")
        df <- as.data.frame(x2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    plotData1 <- testGetAnalysisResultsPlotData(x2, thetaRange = seq(0.4, 1, 0.05))

    ## Comparison of the results of list object 'plotData1' with expected results
    expect_equal(plotData1$xValues, c(0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$xValues, collapse = ", "), ")"))
    expect_equal(plotData1$condPowerValues, c(0.95060038, 0.90312097, 0.83779047, 0.7584288, 0.67069735, 0.58050999, 0.49291957, 0.41159422, 0.33875526, 0.27538378, 0.22153368, 0.17664644, 0.1398156), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$condPowerValues, collapse = ", "), ")"))
    expect_equal(plotData1$likelihoodValues, c(0.92517582, 0.98626675, 0.99928862, 0.9755955, 0.92648393, 0.86161675, 0.78854281, 0.71277663, 0.63811141, 0.56698955, 0.50084781, 0.44040564, 0.38589113), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$likelihoodValues, collapse = ", "), ")"))
    expect_equal(plotData1$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData1$main, collapse = ", "), ")"))
    expect_equal(plotData1$xlab, "Hazard ratio", label = paste0("c(", paste0(plotData1$xlab, collapse = ", "), ")"))
    expect_equal(plotData1$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData1$ylab, collapse = ", "), ")"))
    expect_equal(plotData1$sub, "Stage = 2, maximum number of remaining events = 40, allocation ratio = 0.5", label = paste0("c(", paste0(plotData1$sub, collapse = ", "), ")"))
})

test_plan_section("Testing the Analysis Survival Functionality for the Inverse Normal Design")


test_that("'getAnalysisResults' for a three-stage inverse normal design and survival data", {
    .skipTestIfDisabled()

    design3 <- getDesignInverseNormal(
        kMax = 3, alpha = 0.025,
        informationRates = c(0.4, 0.6, 1), bindingFutility = FALSE,
        typeOfDesign = "WT", deltaWT = 0.25, futilityBounds = c(0.2, 0.2)
    )

    dataExample3 <- getDataset(
        overallEvents = c(8, 15, 29),
        overallAllocationRatios = c(1, 1, 1),
        overallLogRanks = c(1.52, 1.38, 2.9)
    )

    # @refFS[Tab.]{fs:tab:output:getAnalysisResultsInverseNormal}
    # @refFS[Formula]{fs:testStatisticSurvival}
    # @refFS[Formula]{fs:pValuesSurvivalAlternativeGreater}
    # @refFS[Formula]{fs:definitionRCIInverseNormal}
    # @refFS[Formula]{fs:calculationRepeatedpValue}
    x1 <- getAnalysisResults(design3, dataExample3, directionUpper = TRUE)

    ## Comparison of the results of AnalysisResultsInverseNormal object 'x1' with expected results
    expect_equal(x1$thetaH1, 2.9359555, tolerance = 1e-07, label = paste0("c(", paste0(x1$thetaH1, collapse = ", "), ")"))
    expect_equal(x1$testActions, c("continue", "continue", "reject"), label = paste0("c(", paste0(x1$testActions, collapse = ", "), ")"))
    expect_equal(x1$conditionalRejectionProbabilities, c(0.088442162, 0.068047477, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities, collapse = ", "), ")"))
    expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$conditionalPower, collapse = ", "), ")"))
    expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.46058716, 0.62720212, 1.3462647), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(18.631576, 7.3754243, 6.4004419), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x1$repeatedPValues, c(0.16451426, 0.14162994, 0.0024185596), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues, collapse = ", "), ")"))
    expect_equal(x1$finalStage, 3, label = paste0("c(", paste0(x1$finalStage, collapse = ", "), ")"))
    expect_equal(x1$finalPValues, c(NA_real_, NA_real_, 0.012073682), tolerance = 1e-07, label = paste0("c(", paste0(x1$finalPValues, collapse = ", "), ")"))
    expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 1.1608546), tolerance = 1e-07, label = paste0("c(", paste0(x1$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 5.9479756), tolerance = 1e-07, label = paste0("c(", paste0(x1$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, 2.7535435), tolerance = 1e-07, label = paste0("c(", paste0(x1$medianUnbiasedEstimates, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x1), NA)))
        expect_output(print(x1)$show())
        invisible(capture.output(expect_error(summary(x1), NA)))
        expect_output(summary(x1)$show())
        x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
        expect_equal(x1CodeBased$thetaH1, x1$thetaH1, tolerance = 1e-07)
        expect_equal(x1CodeBased$testActions, x1$testActions, tolerance = 1e-07)
        expect_equal(x1CodeBased$conditionalRejectionProbabilities, x1$conditionalRejectionProbabilities, tolerance = 1e-07)
        expect_equal(x1CodeBased$conditionalPower, x1$conditionalPower, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedConfidenceIntervalLowerBounds, x1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedConfidenceIntervalUpperBounds, x1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedPValues, x1$repeatedPValues, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalStage, x1$finalStage, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalPValues, x1$finalPValues, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalConfidenceIntervalLowerBounds, x1$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalConfidenceIntervalUpperBounds, x1$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$medianUnbiasedEstimates, x1$medianUnbiasedEstimates, tolerance = 1e-07)
        expect_type(names(x1), "character")
        df <- as.data.frame(x1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getAnalysisResultsInverseNormal}
    # @refFS[Formula]{fs:testStatisticSurvival}
    # @refFS[Formula]{fs:pValuesSurvivalAlternativeGreater}
    # @refFS[Formula]{fs:definitionRCIInverseNormal}
    # @refFS[Formula]{fs:calculationRepeatedpValue}
    x2 <- getAnalysisResults(design3,
        stage = 1, nPlanned = c(20, 40),
        allocationRatioPlanned = 2, thetaH1 = 2, dataExample3, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsInverseNormal object 'x2' with expected results
    expect_equal(x2$testActions, c("continue", NA_character_, NA_character_), label = paste0("c(", paste0(x2$testActions, collapse = ", "), ")"))
    expect_equal(x2$conditionalRejectionProbabilities, c(0.088442162, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities, collapse = ", "), ")"))
    expect_equal(x2$conditionalPower, c(NA_real_, 0.31420758, 0.86797577), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalPower, collapse = ", "), ")"))
    expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.46058716, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(18.631576, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x2$repeatedPValues, c(0.16451426, NA_real_, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues, collapse = ", "), ")"))
    expect_equal(x2$finalStage, NA_integer_, label = paste0("c(", paste0(x2$finalStage, collapse = ", "), ")"))
    expect_equal(x2$finalPValues, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalPValues, collapse = ", "), ")"))
    expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$medianUnbiasedEstimates, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x2), NA)))
        expect_output(print(x2)$show())
        invisible(capture.output(expect_error(summary(x2), NA)))
        expect_output(summary(x2)$show())
        x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
        expect_equal(x2CodeBased$testActions, x2$testActions, tolerance = 1e-07)
        expect_equal(x2CodeBased$conditionalRejectionProbabilities, x2$conditionalRejectionProbabilities, tolerance = 1e-07)
        expect_equal(x2CodeBased$conditionalPower, x2$conditionalPower, tolerance = 1e-07)
        expect_equal(x2CodeBased$repeatedConfidenceIntervalLowerBounds, x2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$repeatedConfidenceIntervalUpperBounds, x2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$repeatedPValues, x2$repeatedPValues, tolerance = 1e-07)
        expect_equal(x2CodeBased$finalStage, x2$finalStage, tolerance = 1e-07)
        expect_equal(x2CodeBased$finalPValues, x2$finalPValues, tolerance = 1e-07)
        expect_equal(x2CodeBased$finalConfidenceIntervalLowerBounds, x2$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$finalConfidenceIntervalUpperBounds, x2$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$medianUnbiasedEstimates, x2$medianUnbiasedEstimates, tolerance = 1e-07)
        expect_type(names(x2), "character")
        df <- as.data.frame(x2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    plotData1 <- testGetAnalysisResultsPlotData(x2, thetaRange = seq(1, 2.5, 0.05))

    ## Comparison of the results of list object 'plotData1' with expected results
    expect_equal(plotData1$xValues, c(1, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35, 1.4, 1.45, 1.5, 1.55, 1.6, 1.65, 1.7, 1.75, 1.8, 1.85, 1.9, 1.95, 2, 2.05, 2.1, 2.15, 2.2, 2.25, 2.3, 2.35, 2.4, 2.45, 2.5), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$xValues, collapse = ", "), ")"))
    expect_equal(plotData1$condPowerValues, c(0.088421701, 0.1185973, 0.15385139, 0.19371622, 0.23749985, 0.28435066, 0.33332759, 0.38346727, 0.43384172, 0.48360335, 0.53201578, 0.57847144, 0.62249749, 0.66375267, 0.70201741, 0.73717966, 0.7692185, 0.79818706, 0.82419601, 0.84739829, 0.86797577, 0.88612785, 0.90206209, 0.91598687, 0.92810573, 0.93861331, 0.9476925, 0.95551278, 0.96222928, 0.96798255, 0.97289882), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$condPowerValues, collapse = ", "), ")"))
    expect_equal(plotData1$likelihoodValues, c(0.31499453, 0.34899387, 0.38312084, 0.41715372, 0.4508945, 0.48416833, 0.51682261, 0.54872573, 0.57976566, 0.60984848, 0.63889683, 0.66684839, 0.69365439, 0.71927824, 0.74369416, 0.76688598, 0.78884594, 0.80957369, 0.82907527, 0.84736227, 0.86445102, 0.88036187, 0.89511858, 0.90874767, 0.92127801, 0.93274029, 0.94316663, 0.95259025, 0.96104517, 0.96856586, 0.97518711), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$likelihoodValues, collapse = ", "), ")"))
    expect_equal(plotData1$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData1$main, collapse = ", "), ")"))
    expect_equal(plotData1$xlab, "Hazard ratio", label = paste0("c(", paste0(plotData1$xlab, collapse = ", "), ")"))
    expect_equal(plotData1$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData1$ylab, collapse = ", "), ")"))
    expect_equal(plotData1$sub, "Stage = 1, maximum number of remaining events = 60, allocation ratio = 2", label = paste0("c(", paste0(plotData1$sub, collapse = ", "), ")"))

    # @refFS[Tab.]{fs:tab:output:getAnalysisResultsInverseNormal}
    # @refFS[Formula]{fs:testStatisticSurvival}
    # @refFS[Formula]{fs:pValuesSurvivalAlternativeGreater}
    # @refFS[Formula]{fs:definitionRCIInverseNormal}
    # @refFS[Formula]{fs:calculationRepeatedpValue}
    x3 <- getAnalysisResults(design3, dataExample3,
        thetaH0 = 0.95, stage = 2,
        nPlanned = 40, allocationRatioPlanned = 2, thetaH1 = 2, directionUpper = TRUE
    )

    ## Comparison of the results of AnalysisResultsInverseNormal object 'x3' with expected results
    expect_equal(x3$testActions, c("continue", "continue", NA_character_), label = paste0("c(", paste0(x3$testActions, collapse = ", "), ")"))
    expect_equal(x3$conditionalRejectionProbabilities, c(0.1007598, 0.085347867, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalRejectionProbabilities, collapse = ", "), ")"))
    expect_equal(x3$conditionalPower, c(NA_real_, NA_real_, 0.80220427), tolerance = 1e-07, label = paste0("c(", paste0(x3$conditionalPower, collapse = ", "), ")"))
    expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(0.46058716, 0.62720212, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(18.631576, 7.3754243, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x3$repeatedPValues, c(0.14859365, 0.12054424, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x3$repeatedPValues, collapse = ", "), ")"))
    expect_equal(x3$finalStage, NA_integer_, label = paste0("c(", paste0(x3$finalStage, collapse = ", "), ")"))
    expect_equal(x3$finalPValues, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalPValues, collapse = ", "), ")"))
    expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x3$medianUnbiasedEstimates, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x3), NA)))
        expect_output(print(x3)$show())
        invisible(capture.output(expect_error(summary(x3), NA)))
        expect_output(summary(x3)$show())
        x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
        expect_equal(x3CodeBased$testActions, x3$testActions, tolerance = 1e-07)
        expect_equal(x3CodeBased$conditionalRejectionProbabilities, x3$conditionalRejectionProbabilities, tolerance = 1e-07)
        expect_equal(x3CodeBased$conditionalPower, x3$conditionalPower, tolerance = 1e-07)
        expect_equal(x3CodeBased$repeatedConfidenceIntervalLowerBounds, x3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x3CodeBased$repeatedConfidenceIntervalUpperBounds, x3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x3CodeBased$repeatedPValues, x3$repeatedPValues, tolerance = 1e-07)
        expect_equal(x3CodeBased$finalStage, x3$finalStage, tolerance = 1e-07)
        expect_equal(x3CodeBased$finalPValues, x3$finalPValues, tolerance = 1e-07)
        expect_equal(x3CodeBased$finalConfidenceIntervalLowerBounds, x3$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x3CodeBased$finalConfidenceIntervalUpperBounds, x3$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x3CodeBased$medianUnbiasedEstimates, x3$medianUnbiasedEstimates, tolerance = 1e-07)
        expect_type(names(x3), "character")
        df <- as.data.frame(x3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    plotData2 <- testGetAnalysisResultsPlotData(x3, thetaRange = seq(1, 2.5, 0.05))

    ## Comparison of the results of list object 'plotData2' with expected results
    expect_equal(plotData2$xValues, c(1, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35, 1.4, 1.45, 1.5, 1.55, 1.6, 1.65, 1.7, 1.75, 1.8, 1.85, 1.9, 1.95, 2, 2.05, 2.1, 2.15, 2.2, 2.25, 2.3, 2.35, 2.4, 2.45, 2.5), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$xValues, collapse = ", "), ")"))
    expect_equal(plotData2$condPowerValues, c(0.11179361, 0.14195425, 0.17543978, 0.21175256, 0.25032518, 0.2905567, 0.33184453, 0.37361059, 0.415321, 0.45649966, 0.49673619, 0.53568916, 0.57308562, 0.60871782, 0.642438, 0.67415198, 0.70381218, 0.73141052, 0.75697152, 0.7805458, 0.80220427, 0.8220329, 0.84012825, 0.8565936, 0.87153581, 0.88506274, 0.89728115, 0.90829517, 0.91820505, 0.92710634, 0.93508929), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$condPowerValues, collapse = ", "), ")"))
    expect_equal(plotData2$likelihoodValues, c(0.38589113, 0.43767503, 0.48942229, 0.54046191, 0.59019718, 0.63811141, 0.6837696, 0.72681695, 0.76697506, 0.8040366, 0.83785893, 0.86835727, 0.89549763, 0.91928996, 0.93978142, 0.95705021, 0.97119988, 0.98235405, 0.99065189, 0.99624395, 0.99928862, 0.99994909, 0.9983907, 0.99477877, 0.98927675, 0.98204476, 0.97323838, 0.96300773, 0.95149676, 0.93884271, 0.92517582), tolerance = 1e-07, label = paste0("c(", paste0(plotData2$likelihoodValues, collapse = ", "), ")"))
    expect_equal(plotData2$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData2$main, collapse = ", "), ")"))
    expect_equal(plotData2$xlab, "Hazard ratio", label = paste0("c(", paste0(plotData2$xlab, collapse = ", "), ")"))
    expect_equal(plotData2$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData2$ylab, collapse = ", "), ")"))
    expect_equal(plotData2$sub, "Stage = 2, maximum number of remaining events = 40, allocation ratio = 2", label = paste0("c(", paste0(plotData2$sub, collapse = ", "), ")"))
})

test_that("'getAnalysisResults' for a three-stage inverse normal design and survival data ('directionUpper' reversed)", {
    .skipTestIfDisabled()

    design4 <- getDesignInverseNormal(
        kMax = 3, alpha = 0.025,
        informationRates = c(0.4, 0.6, 1), bindingFutility = FALSE,
        typeOfDesign = "WT", deltaWT = 0.25, futilityBounds = c(0.2, 0.2)
    )

    dataExample4 <- getDataset(
        overallEvents = c(8, 15, 29),
        overallAllocationRatios = c(1, 1, 1),
        overallLogRanks = -c(1.52, 1.38, 2.9)
    )

    # @refFS[Tab.]{fs:tab:output:getAnalysisResultsInverseNormal}
    # @refFS[Formula]{fs:testStatisticSurvival}
    # @refFS[Formula]{fs:pValuesSurvivalAlternativeSmaller}
    # @refFS[Formula]{fs:definitionRCIInverseNormal}
    # @refFS[Formula]{fs:calculationRepeatedpValue}
    x1 <- getAnalysisResults(design4, dataExample4, directionUpper = FALSE)

    ## Comparison of the results of AnalysisResultsInverseNormal object 'x1' with expected results
    expect_equal(x1$thetaH1, 0.34060461, tolerance = 1e-07, label = paste0("c(", paste0(x1$thetaH1, collapse = ", "), ")"))
    expect_equal(x1$testActions, c("continue", "continue", "reject"), label = paste0("c(", paste0(x1$testActions, collapse = ", "), ")"))
    expect_equal(x1$conditionalRejectionProbabilities, c(0.088442162, 0.068047477, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities, collapse = ", "), ")"))
    expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$conditionalPower, collapse = ", "), ")"))
    expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.053672215, 0.13558542, 0.1562393), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(2.1711417, 1.5943825, 0.74279586), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x1$repeatedPValues, c(0.16451426, 0.14162994, 0.0024185596), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues, collapse = ", "), ")"))
    expect_equal(x1$finalStage, 3, label = paste0("c(", paste0(x1$finalStage, collapse = ", "), ")"))
    expect_equal(x1$finalPValues, c(NA_real_, NA_real_, 0.012073682), tolerance = 1e-07, label = paste0("c(", paste0(x1$finalPValues, collapse = ", "), ")"))
    expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.16812443), tolerance = 1e-07, label = paste0("c(", paste0(x1$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.86143434), tolerance = 1e-07, label = paste0("c(", paste0(x1$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.3631684), tolerance = 1e-07, label = paste0("c(", paste0(x1$medianUnbiasedEstimates, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x1), NA)))
        expect_output(print(x1)$show())
        invisible(capture.output(expect_error(summary(x1), NA)))
        expect_output(summary(x1)$show())
        x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
        expect_equal(x1CodeBased$thetaH1, x1$thetaH1, tolerance = 1e-07)
        expect_equal(x1CodeBased$testActions, x1$testActions, tolerance = 1e-07)
        expect_equal(x1CodeBased$conditionalRejectionProbabilities, x1$conditionalRejectionProbabilities, tolerance = 1e-07)
        expect_equal(x1CodeBased$conditionalPower, x1$conditionalPower, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedConfidenceIntervalLowerBounds, x1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedConfidenceIntervalUpperBounds, x1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedPValues, x1$repeatedPValues, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalStage, x1$finalStage, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalPValues, x1$finalPValues, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalConfidenceIntervalLowerBounds, x1$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalConfidenceIntervalUpperBounds, x1$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$medianUnbiasedEstimates, x1$medianUnbiasedEstimates, tolerance = 1e-07)
        expect_type(names(x1), "character")
        df <- as.data.frame(x1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getAnalysisResultsInverseNormal}
    # @refFS[Formula]{fs:testStatisticSurvival}
    # @refFS[Formula]{fs:pValuesSurvivalAlternativeSmaller}
    # @refFS[Formula]{fs:definitionRCIInverseNormal}
    # @refFS[Formula]{fs:calculationRepeatedpValue}
    x2 <- getAnalysisResults(design4, dataExample4,
        thetaH0 = 1.1, stage = 2,
        nPlanned = 40, allocationRatioPlanned = 0.5, thetaH1 = 0.5, directionUpper = FALSE
    )

    ## Comparison of the results of AnalysisResultsInverseNormal object 'x2' with expected results
    expect_equal(x2$testActions, c("continue", "continue", NA_character_), label = paste0("c(", paste0(x2$testActions, collapse = ", "), ")"))
    expect_equal(x2$conditionalRejectionProbabilities, c(0.11248903, 0.10265841, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalRejectionProbabilities, collapse = ", "), ")"))
    expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, 0.8608569), tolerance = 1e-07, label = paste0("c(", paste0(x2$conditionalPower, collapse = ", "), ")"))
    expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.053672215, 0.13558542, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(2.1711417, 1.5943825, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x2$repeatedPValues, c(0.13581063, 0.1043566, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x2$repeatedPValues, collapse = ", "), ")"))
    expect_equal(x2$finalStage, NA_integer_, label = paste0("c(", paste0(x2$finalStage, collapse = ", "), ")"))
    expect_equal(x2$finalPValues, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalPValues, collapse = ", "), ")"))
    expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x2$medianUnbiasedEstimates, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x2), NA)))
        expect_output(print(x2)$show())
        invisible(capture.output(expect_error(summary(x2), NA)))
        expect_output(summary(x2)$show())
        x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
        expect_equal(x2CodeBased$testActions, x2$testActions, tolerance = 1e-07)
        expect_equal(x2CodeBased$conditionalRejectionProbabilities, x2$conditionalRejectionProbabilities, tolerance = 1e-07)
        expect_equal(x2CodeBased$conditionalPower, x2$conditionalPower, tolerance = 1e-07)
        expect_equal(x2CodeBased$repeatedConfidenceIntervalLowerBounds, x2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$repeatedConfidenceIntervalUpperBounds, x2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$repeatedPValues, x2$repeatedPValues, tolerance = 1e-07)
        expect_equal(x2CodeBased$finalStage, x2$finalStage, tolerance = 1e-07)
        expect_equal(x2CodeBased$finalPValues, x2$finalPValues, tolerance = 1e-07)
        expect_equal(x2CodeBased$finalConfidenceIntervalLowerBounds, x2$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$finalConfidenceIntervalUpperBounds, x2$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$medianUnbiasedEstimates, x2$medianUnbiasedEstimates, tolerance = 1e-07)
        expect_type(names(x2), "character")
        df <- as.data.frame(x2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    plotData1 <- testGetAnalysisResultsPlotData(x2, thetaRange = seq(0.4, 1, 0.05))

    ## Comparison of the results of list object 'plotData1' with expected results
    expect_equal(plotData1$xValues, c(0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$xValues, collapse = ", "), ")"))
    expect_equal(plotData1$condPowerValues, c(0.95989447, 0.91898875, 0.8608569, 0.78814959, 0.70560814, 0.61865802, 0.53228335, 0.45038602, 0.37558279, 0.3092947, 0.25198255, 0.20342172, 0.16295428), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$condPowerValues, collapse = ", "), ")"))
    expect_equal(plotData1$likelihoodValues, c(0.92517582, 0.98626675, 0.99928862, 0.9755955, 0.92648393, 0.86161675, 0.78854281, 0.71277663, 0.63811141, 0.56698955, 0.50084781, 0.44040564, 0.38589113), tolerance = 1e-07, label = paste0("c(", paste0(plotData1$likelihoodValues, collapse = ", "), ")"))
    expect_equal(plotData1$main, "Conditional Power with Likelihood", label = paste0("c(", paste0(plotData1$main, collapse = ", "), ")"))
    expect_equal(plotData1$xlab, "Hazard ratio", label = paste0("c(", paste0(plotData1$xlab, collapse = ", "), ")"))
    expect_equal(plotData1$ylab, "Conditional power / Likelihood", label = paste0("c(", paste0(plotData1$ylab, collapse = ", "), ")"))
    expect_equal(plotData1$sub, "Stage = 2, maximum number of remaining events = 40, allocation ratio = 0.5", label = paste0("c(", paste0(plotData1$sub, collapse = ", "), ")"))
})

test_plan_section("Testing the Analysis Survival Functionality for the Fisher Design")


test_that("'getAnalysisResults' for a three-stage Fisher design and 'bindingFutility = TRUE'", {
    .skipTestIfDisabled()

    design5 <- getDesignFisher(
        kMax = 3, alpha = 0.025,
        informationRates = c(0.4, 0.6, 1), alpha0Vec = c(0.5, 0.4), bindingFutility = TRUE
    )

    dataExample5 <- getDataset(
        overallEvents = c(8, 15),
        overallAllocationRatios = c(1, 1),
        overallLogRanks = c(1.52, 2)
    )

    # @refFS[Tab.]{fs:tab:output:getAnalysisResultsFisher}
    # @refFS[Formula]{fs:testStatisticSurvival}
    # @refFS[Formula]{fs:pValuesSurvivalAlternativeGreater}
    # @refFS[Formula]{fs:definitionRCIFisherCombination}
    # @refFS[Formula]{fs:calculationRepeatedpValue}
    x1 <- getAnalysisResults(design5, dataExample5,
        thetaH1 = 2, allocationRatioPlanned = 2,
        nPlanned = 50, directionUpper = TRUE, seed = 123456789
    )

    ## Comparison of the results of AnalysisResultsFisher object 'x1' with expected results
    expect_equal(x1$testActions, c("continue", "continue", NA_character_), label = paste0("c(", paste0(x1$testActions, collapse = ", "), ")"))
    expect_equal(x1$conditionalRejectionProbabilities, c(0.043454839, 0.062873928, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities, collapse = ", "), ")"))
    expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, 0.78212896), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalPower, collapse = ", "), ")"))
    expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.63614226, 0.82191364, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(13.489852, 9.7381024, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x1$repeatedPValues, c(0.094302989, 0.05707734, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues, collapse = ", "), ")"))
    expect_equal(x1$finalStage, NA_integer_, label = paste0("c(", paste0(x1$finalStage, collapse = ", "), ")"))
    expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalPValues, collapse = ", "), ")"))
    expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$medianUnbiasedEstimates, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x1), NA)))
        expect_output(print(x1)$show())
        invisible(capture.output(expect_error(summary(x1), NA)))
        expect_output(summary(x1)$show())
        x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
        expect_equal(x1CodeBased$testActions, x1$testActions, tolerance = 1e-07)
        expect_equal(x1CodeBased$conditionalRejectionProbabilities, x1$conditionalRejectionProbabilities, tolerance = 1e-07)
        expect_equal(x1CodeBased$conditionalPower, x1$conditionalPower, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedConfidenceIntervalLowerBounds, x1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedConfidenceIntervalUpperBounds, x1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedPValues, x1$repeatedPValues, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalStage, x1$finalStage, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalPValues, x1$finalPValues, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalConfidenceIntervalLowerBounds, x1$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalConfidenceIntervalUpperBounds, x1$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$medianUnbiasedEstimates, x1$medianUnbiasedEstimates, tolerance = 1e-07)
        expect_type(names(x1), "character")
        df <- as.data.frame(x1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getAnalysisResults' for a three-stage Fisher design and 'bindingFutility = TRUE' ('directionUpper' reversed)", {
    .skipTestIfDisabled()

    design6 <- getDesignFisher(
        kMax = 3, alpha = 0.025,
        informationRates = c(0.4, 0.6, 1), alpha0Vec = c(0.5, 0.4), bindingFutility = TRUE
    )

    dataExample6 <- getDataset(
        overallEvents = c(8, 15),
        overallAllocationRatios = c(1, 1),
        overallLogRanks = -c(1.52, 2)
    )

    # @refFS[Tab.]{fs:tab:output:getAnalysisResultsFisher}
    # @refFS[Formula]{fs:testStatisticSurvival}
    # @refFS[Formula]{fs:pValuesSurvivalAlternativeSmaller}
    # @refFS[Formula]{fs:definitionRCIFisherCombination}
    # @refFS[Formula]{fs:calculationRepeatedpValue}
    x1 <- getAnalysisResults(design6, dataExample6,
        thetaH1 = 0.5, allocationRatioPlanned = 0.5,
        nPlanned = 50, directionUpper = FALSE, seed = 123456789
    )

    ## Comparison of the results of AnalysisResultsFisher object 'x1' with expected results
    expect_equal(x1$testActions, c("continue", "continue", NA_character_), label = paste0("c(", paste0(x1$testActions, collapse = ", "), ")"))
    expect_equal(x1$conditionalRejectionProbabilities, c(0.043454839, 0.062873928, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalRejectionProbabilities, collapse = ", "), ")"))
    expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, 0.78212896), tolerance = 1e-07, label = paste0("c(", paste0(x1$conditionalPower, collapse = ", "), ")"))
    expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.074129584, 0.10268931, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(1.5719754, 1.2166725, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x1$repeatedPValues, c(0.094302989, 0.05707734, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(x1$repeatedPValues, collapse = ", "), ")"))
    expect_equal(x1$finalStage, NA_integer_, label = paste0("c(", paste0(x1$finalStage, collapse = ", "), ")"))
    expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalPValues, collapse = ", "), ")"))
    expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(x1$medianUnbiasedEstimates, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x1), NA)))
        expect_output(print(x1)$show())
        invisible(capture.output(expect_error(summary(x1), NA)))
        expect_output(summary(x1)$show())
        x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
        expect_equal(x1CodeBased$testActions, x1$testActions, tolerance = 1e-07)
        expect_equal(x1CodeBased$conditionalRejectionProbabilities, x1$conditionalRejectionProbabilities, tolerance = 1e-07)
        expect_equal(x1CodeBased$conditionalPower, x1$conditionalPower, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedConfidenceIntervalLowerBounds, x1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedConfidenceIntervalUpperBounds, x1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedPValues, x1$repeatedPValues, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalStage, x1$finalStage, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalPValues, x1$finalPValues, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalConfidenceIntervalLowerBounds, x1$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$finalConfidenceIntervalUpperBounds, x1$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$medianUnbiasedEstimates, x1$medianUnbiasedEstimates, tolerance = 1e-07)
        expect_type(names(x1), "character")
        df <- as.data.frame(x1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getAnalysisResults' with a dataset of survival data and without defining a design", {
    .skipTestIfDisabled()

    data <- getDataset(
        overallEvents = c(38),
        overallAllocationRatios = c(1),
        overallLogRanks = -c(1.72)
    )
    # @refFS[Formula]{fs:testStatisticSurvival}
    # @refFS[Formula]{fs:pValuesSurvivalAlternativeGreater}

    analysisResults1 <- getAnalysisResults(data, alpha = 0.05, directionUpper = FALSE)

    ## Comparison of the results of AnalysisResultsInverseNormal object 'analysisResults1' with expected results
    expect_equal(analysisResults1$thetaH1, 0.57232877, tolerance = 1e-07, label = paste0("c(", paste0(analysisResults1$thetaH1, collapse = ", "), ")"))
    expect_equal(analysisResults1$testActions, "reject", label = paste0("c(", paste0(analysisResults1$testActions, collapse = ", "), ")"))
    expect_equal(analysisResults1$repeatedConfidenceIntervalLowerBounds, 0.33564434, tolerance = 1e-07, label = paste0("c(", paste0(analysisResults1$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(analysisResults1$repeatedConfidenceIntervalUpperBounds, 0.97591411, tolerance = 1e-07, label = paste0("c(", paste0(analysisResults1$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(analysisResults1$repeatedPValues, 0.042716221, tolerance = 1e-07, label = paste0("c(", paste0(analysisResults1$repeatedPValues, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(analysisResults1), NA)))
        expect_output(print(analysisResults1)$show())
        invisible(capture.output(expect_error(summary(analysisResults1), NA)))
        expect_output(summary(analysisResults1)$show())
        analysisResults1CodeBased <- eval(parse(text = getObjectRCode(analysisResults1, stringWrapParagraphWidth = NULL)))
        expect_equal(analysisResults1CodeBased$thetaH1, analysisResults1$thetaH1, tolerance = 1e-07)
        expect_equal(analysisResults1CodeBased$testActions, analysisResults1$testActions, tolerance = 1e-07)
        expect_equal(analysisResults1CodeBased$repeatedConfidenceIntervalLowerBounds, analysisResults1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(analysisResults1CodeBased$repeatedConfidenceIntervalUpperBounds, analysisResults1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(analysisResults1CodeBased$repeatedPValues, analysisResults1$repeatedPValues, tolerance = 1e-07)
        expect_type(names(analysisResults1), "character")
        df <- as.data.frame(analysisResults1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(analysisResults1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:testStatisticSurvival}
    # @refFS[Formula]{fs:pValuesSurvivalAlternativeGreater}
    analysisResults2 <- getAnalysisResults(data, alpha = 0.05, sided = 2)

    ## Comparison of the results of AnalysisResultsInverseNormal object 'analysisResults2' with expected results
    expect_equal(analysisResults2$thetaH1, 0.57232877, tolerance = 1e-07, label = paste0("c(", paste0(analysisResults2$thetaH1, collapse = ", "), ")"))
    expect_equal(analysisResults2$testActions, "accept", label = paste0("c(", paste0(analysisResults2$testActions, collapse = ", "), ")"))
    expect_equal(analysisResults2$repeatedConfidenceIntervalLowerBounds, 0.3030255, tolerance = 1e-07, label = paste0("c(", paste0(analysisResults2$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(analysisResults2$repeatedConfidenceIntervalUpperBounds, 1.0809654, tolerance = 1e-07, label = paste0("c(", paste0(analysisResults2$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(analysisResults2$repeatedPValues, 0.085432442, tolerance = 1e-07, label = paste0("c(", paste0(analysisResults2$repeatedPValues, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(analysisResults2), NA)))
        expect_output(print(analysisResults2)$show())
        invisible(capture.output(expect_error(summary(analysisResults2), NA)))
        expect_output(summary(analysisResults2)$show())
        analysisResults2CodeBased <- eval(parse(text = getObjectRCode(analysisResults2, stringWrapParagraphWidth = NULL)))
        expect_equal(analysisResults2CodeBased$thetaH1, analysisResults2$thetaH1, tolerance = 1e-07)
        expect_equal(analysisResults2CodeBased$testActions, analysisResults2$testActions, tolerance = 1e-07)
        expect_equal(analysisResults2CodeBased$repeatedConfidenceIntervalLowerBounds, analysisResults2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(analysisResults2CodeBased$repeatedConfidenceIntervalUpperBounds, analysisResults2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(analysisResults2CodeBased$repeatedPValues, analysisResults2$repeatedPValues, tolerance = 1e-07)
        expect_type(names(analysisResults2), "character")
        df <- as.data.frame(analysisResults2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(analysisResults2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getAnalysisResults' with a dataset of survival data and automatic boundary recalculation", {
    .skipTestIfDisabled()

    design <- getDesignGroupSequential(sided = 1, alpha = 0.025, typeOfDesign = "asOF")
    data <- getDataset(
        overallEvents = c(205, 285),
        overallLogRanks = c(1.87, 2.19)
    )
    analysisResults <- getAnalysisResults(
        design = design,
        dataInput = data,
        maxInformation = 387
    )

    ## Comparison of the results of AnalysisResultsGroupSequential object 'analysisResults' with expected results
    expect_equal(analysisResults$thetaH1, 1.2962154, tolerance = 1e-07, label = paste0("c(", paste0(analysisResults$thetaH1, collapse = ", "), ")"))
    expect_equal(analysisResults$testActions, c("continue", "continue", NA_character_), label = paste0("c(", paste0(analysisResults$testActions, collapse = ", "), ")"))
    expect_equal(analysisResults$conditionalRejectionProbabilities, c(0.19266595, 0.39869438, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(analysisResults$conditionalRejectionProbabilities, collapse = ", "), ")"))
    expect_equal(analysisResults$conditionalPower, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(analysisResults$conditionalPower, collapse = ", "), ")"))
    expect_equal(analysisResults$repeatedConfidenceIntervalLowerBounds, c(0.87000803, 0.97623896, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(analysisResults$repeatedConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(analysisResults$repeatedConfidenceIntervalUpperBounds, c(1.9380428, 1.7210688, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(analysisResults$repeatedConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(analysisResults$repeatedPValues, c(0.11586361, 0.037973374, NA_real_), tolerance = 1e-07, label = paste0("c(", paste0(analysisResults$repeatedPValues, collapse = ", "), ")"))
    expect_equal(analysisResults$finalStage, NA_integer_, label = paste0("c(", paste0(analysisResults$finalStage, collapse = ", "), ")"))
    expect_equal(analysisResults$finalPValues, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(analysisResults$finalPValues, collapse = ", "), ")"))
    expect_equal(analysisResults$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(analysisResults$finalConfidenceIntervalLowerBounds, collapse = ", "), ")"))
    expect_equal(analysisResults$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(analysisResults$finalConfidenceIntervalUpperBounds, collapse = ", "), ")"))
    expect_equal(analysisResults$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_), label = paste0("c(", paste0(analysisResults$medianUnbiasedEstimates, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(analysisResults), NA)))
        expect_output(print(analysisResults)$show())
        invisible(capture.output(expect_error(summary(analysisResults), NA)))
        expect_output(summary(analysisResults)$show())
        analysisResultsCodeBased <- eval(parse(text = getObjectRCode(analysisResults, stringWrapParagraphWidth = NULL)))
        expect_equal(analysisResultsCodeBased$thetaH1, analysisResults$thetaH1, tolerance = 1e-07)
        expect_equal(analysisResultsCodeBased$testActions, analysisResults$testActions, tolerance = 1e-07)
        expect_equal(analysisResultsCodeBased$conditionalRejectionProbabilities, analysisResults$conditionalRejectionProbabilities, tolerance = 1e-07)
        expect_equal(analysisResultsCodeBased$conditionalPower, analysisResults$conditionalPower, tolerance = 1e-07)
        expect_equal(analysisResultsCodeBased$repeatedConfidenceIntervalLowerBounds, analysisResults$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(analysisResultsCodeBased$repeatedConfidenceIntervalUpperBounds, analysisResults$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(analysisResultsCodeBased$repeatedPValues, analysisResults$repeatedPValues, tolerance = 1e-07)
        expect_equal(analysisResultsCodeBased$finalStage, analysisResults$finalStage, tolerance = 1e-07)
        expect_equal(analysisResultsCodeBased$finalPValues, analysisResults$finalPValues, tolerance = 1e-07)
        expect_equal(analysisResultsCodeBased$finalConfidenceIntervalLowerBounds, analysisResults$finalConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(analysisResultsCodeBased$finalConfidenceIntervalUpperBounds, analysisResults$finalConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(analysisResultsCodeBased$medianUnbiasedEstimates, analysisResults$medianUnbiasedEstimates, tolerance = 1e-07)
        expect_type(names(analysisResults), "character")
        df <- as.data.frame(analysisResults)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(analysisResults)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})
