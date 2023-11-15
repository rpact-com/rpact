## |
## |  *Analysis of means in multi-arm designs with adaptive test*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |
## |  RPACT company website: https://www.rpact.com
## |  rpact package website: https://www.rpact.org
## |
## |  Contact us for information about our services: info@rpact.com
## |
## |  File version: $Revision: 7206 $
## |  Last changed: $Date: 2023-07-25 14:55:05 +0200 (Tue, 25 Jul 2023) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_logger.R
NULL

.getAnalysisResultsMeansMultiArm <- function(..., design, dataInput) {
    if (.isTrialDesignInverseNormal(design)) {
        return(.getAnalysisResultsMeansInverseNormalMultiArm(design = design, dataInput = dataInput, ...))
    }

    if (.isTrialDesignFisher(design)) {
        return(.getAnalysisResultsMeansFisherMultiArm(design = design, dataInput = dataInput, ...))
    }

    if (.isTrialDesignConditionalDunnett(design)) {
        return(.getAnalysisResultsMeansConditionalDunnettMultiArm(design = design, dataInput = dataInput, ...))
    }

    .stopWithWrongDesignMessage(design)
}

.getAnalysisResultsMeansInverseNormalMultiArm <- function(...,
        design, dataInput,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        varianceOption = C_VARIANCE_OPTION_MULTIARMED_DEFAULT,
        thetaH0 = C_THETA_H0_MEANS_DEFAULT,
        thetaH1 = NA_real_, assumedStDevs = NA_real_,
        nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        calculateSingleStepAdjusted = FALSE,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .assertIsTrialDesignInverseNormal(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsMeansInverseNormalMultiArm",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsMultiArmInverseNormalR6$new(design = design, dataInput = dataInput)

    results <- .getAnalysisResultsMeansMultiArmAll(
        results = results, design = design, dataInput = dataInput,
        intersectionTest = intersectionTest, stage = stage, directionUpper = directionUpper,
        normalApproximation = normalApproximation, varianceOption = varianceOption,
        thetaH0 = thetaH0, thetaH1 = thetaH1, assumedStDevs = assumedStDevs, nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        calculateSingleStepAdjusted = calculateSingleStepAdjusted, tolerance = tolerance
    )

    return(results)
}

.getAnalysisResultsMeansFisherMultiArm <- function(...,
        design, dataInput,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        varianceOption = C_VARIANCE_OPTION_MULTIARMED_DEFAULT,
        thetaH0 = C_THETA_H0_MEANS_DEFAULT,
        thetaH1 = NA_real_, assumedStDevs = NA_real_,
        nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        calculateSingleStepAdjusted = FALSE,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
    .assertIsTrialDesignFisher(design)
    .assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsMeansFisherMultiArm",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsMultiArmFisherR6$new(design = design, dataInput = dataInput)
    results <- .getAnalysisResultsMeansMultiArmAll(
        results = results, design = design, dataInput = dataInput,
        intersectionTest = intersectionTest, stage = stage, directionUpper = directionUpper,
        normalApproximation = normalApproximation, varianceOption = varianceOption,
        thetaH0 = thetaH0, thetaH1 = thetaH1, assumedStDevs = assumedStDevs, nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        calculateSingleStepAdjusted = calculateSingleStepAdjusted,
        tolerance = tolerance, iterations = iterations, seed = seed
    )

    return(results)
}

.getAnalysisResultsMeansConditionalDunnettMultiArm <- function(...,
        design, dataInput,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        varianceOption = C_VARIANCE_OPTION_MULTIARMED_DEFAULT,
        thetaH0 = C_THETA_H0_MEANS_DEFAULT,
        thetaH1 = NA_real_, assumedStDevs = NA_real_,
        nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        calculateSingleStepAdjusted = FALSE,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .assertIsTrialDesignConditionalDunnett(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsMeansConditionalDunnettMultiArm",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsConditionalDunnettR6$new(design = design, dataInput = dataInput)

    results <- .getAnalysisResultsMeansMultiArmAll(
        results = results, design = design,
        dataInput = dataInput, intersectionTest = intersectionTest,
        stage = stage, directionUpper = directionUpper, normalApproximation = normalApproximation,
        varianceOption = varianceOption,
        thetaH0 = thetaH0, thetaH1 = thetaH1, assumedStDevs = assumedStDevs, nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        calculateSingleStepAdjusted = calculateSingleStepAdjusted,
        tolerance = tolerance,
        iterations = iterations, seed = seed
    )

    return(results)
}

.getAnalysisResultsMeansMultiArmAll <- function(..., results, design, dataInput, intersectionTest, stage,
        directionUpper, normalApproximation, varianceOption, thetaH0, thetaH1, assumedStDevs,
        nPlanned, allocationRatioPlanned, calculateSingleStepAdjusted, tolerance,
        iterations, seed) {
    startTime <- Sys.time()

    intersectionTest <- .getCorrectedIntersectionTestMultiArmIfNecessary(design, intersectionTest)

    stageResults <- .getStageResultsMeansMultiArm(
        design = design, dataInput = dataInput,
        intersectionTest = intersectionTest, stage = stage,
        thetaH0 = thetaH0, directionUpper = directionUpper,
        normalApproximation = normalApproximation, varianceOption = varianceOption,
        calculateSingleStepAdjusted = calculateSingleStepAdjusted,
        userFunctionCallEnabled = TRUE
    )
    normalApproximation <- stageResults$normalApproximation
    intersectionTest <- stageResults$intersectionTest

    results$.setStageResults(stageResults)
    .logProgress("Stage results calculated", startTime = startTime)
    numberOfGroups <- dataInput$getNumberOfGroups()

    thetaH1 <- .assertIsValidThetaH1ForMultiArm(thetaH1, stageResults, stage, results = results)
    assumedStDevs <- .assertIsValidAssumedStDevForMultiHypotheses(
        assumedStDevs, stageResults, stage,
        results = results
    )

    .setValueAndParameterType(
        results, "intersectionTest",
        intersectionTest, C_INTERSECTION_TEST_MULTIARMED_DEFAULT
    )
    .setValueAndParameterType(
        results, "directionUpper",
        directionUpper, C_DIRECTION_UPPER_DEFAULT
    )
    .setValueAndParameterType(
        results, "normalApproximation",
        normalApproximation, C_NORMAL_APPROXIMATION_MEANS_DEFAULT
    )
    .setValueAndParameterType(
        results, "varianceOption",
        varianceOption, C_VARIANCE_OPTION_MULTIARMED_DEFAULT
    )
    .setValueAndParameterType(results, "thetaH0", thetaH0, C_THETA_H0_MEANS_DEFAULT)
    .setConditionalPowerArguments(results, dataInput, nPlanned, allocationRatioPlanned)
    .setNPlannedAndThetaH1AndAssumedStDevs(results, nPlanned, thetaH1, assumedStDevs)

    startTime <- Sys.time()
    if (!.isTrialDesignConditionalDunnett(design)) {
        results$.closedTestResults <- getClosedCombinationTestResults(stageResults = stageResults)
    } else {
        results$.closedTestResults <- getClosedConditionalDunnettTestResults(
            stageResults = stageResults, design = design, stage = stage
        )
    }
    .logProgress("Closed test calculated", startTime = startTime)

    results$.setParameterType("seed", C_PARAM_NOT_APPLICABLE)
    results$.setParameterType("iterations", C_PARAM_NOT_APPLICABLE)

    if (design$kMax > 1) {
        # conditional power
        startTime <- Sys.time()
        if (.isTrialDesignFisher(design)) {
            results$.conditionalPowerResults <- .getConditionalPowerMeansMultiArm(
                stageResults = stageResults,
                stage = stage, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
                thetaH1 = thetaH1, assumedStDevs = assumedStDevs, iterations = iterations, seed = seed
            )
            .synchronizeIterationsAndSeed(results)
        } else {
            results$.conditionalPowerResults <- .getConditionalPowerMeansMultiArm(
                stageResults = stageResults,
                stage = stage, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
                thetaH1 = thetaH1, assumedStDevs = assumedStDevs
            )
            results$conditionalPower <- results$.conditionalPowerResults$conditionalPower
            results$.setParameterType("conditionalPower", C_PARAM_GENERATED)
        }
        results$thetaH1 <- matrix(results$.conditionalPowerResults$thetaH1, ncol = 1)
        results$assumedStDevs <- matrix(results$.conditionalPowerResults$assumedStDevs, ncol = 1)
        .logProgress("Conditional power calculated", startTime = startTime)

        # CRP - conditional rejection probabilities
        startTime <- Sys.time()
        results$conditionalRejectionProbabilities <- .getConditionalRejectionProbabilitiesMultiArm(
            stageResults = stageResults, stage = stage
        )
        results$.setParameterType("conditionalRejectionProbabilities", C_PARAM_GENERATED)
        .logProgress("Conditional rejection probabilities (CRP) calculated", startTime = startTime)
    } else {
        results$.setParameterType("conditionalPower", C_PARAM_NOT_APPLICABLE)
        results$.setParameterType("conditionalPowerSimulated", C_PARAM_NOT_APPLICABLE)
        results$.setParameterType("conditionalRejectionProbabilities", C_PARAM_NOT_APPLICABLE)
    }

    # RCI - repeated confidence interval
    repeatedConfidenceIntervals <- .getRepeatedConfidenceIntervalsMeansMultiArm(
        design = design, dataInput = dataInput,
        intersectionTest = intersectionTest, stage = stage,
        normalApproximation = normalApproximation,
        varianceOption = varianceOption,
        tolerance = tolerance
    )

    gMax <- stageResults$getGMax()
    results$repeatedConfidenceIntervalLowerBounds <-
        matrix(rep(NA_real_, gMax * design$kMax), nrow = gMax, ncol = design$kMax)
    results$repeatedConfidenceIntervalUpperBounds <- results$repeatedConfidenceIntervalLowerBounds
    for (k in 1:design$kMax) {
        for (treatmentArm in 1:gMax) {
            results$repeatedConfidenceIntervalLowerBounds[treatmentArm, k] <-
                repeatedConfidenceIntervals[treatmentArm, 1, k]
            results$repeatedConfidenceIntervalUpperBounds[treatmentArm, k] <-
                repeatedConfidenceIntervals[treatmentArm, 2, k]
        }
    }

    results$.setParameterType("repeatedConfidenceIntervalLowerBounds", C_PARAM_GENERATED)
    results$.setParameterType("repeatedConfidenceIntervalUpperBounds", C_PARAM_GENERATED)

    # repeated p-value
    results$repeatedPValues <- .getRepeatedPValuesMultiArm(
        stageResults = stageResults, tolerance = tolerance
    )
    results$.setParameterType("repeatedPValues", C_PARAM_GENERATED)

    return(results)
}

.getStageResultsMeansMultiArm <- function(..., design, dataInput,
        thetaH0 = C_THETA_H0_MEANS_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        varianceOption = C_VARIANCE_OPTION_MULTIARMED_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        calculateSingleStepAdjusted = FALSE,
        userFunctionCallEnabled = FALSE) {
    .assertIsTrialDesign(design)
    .assertIsDatasetMeans(dataInput)
    .assertIsValidThetaH0DataInput(thetaH0, dataInput)
    .assertIsValidDirectionUpper(directionUpper, design$sided)
    .assertIsSingleLogical(normalApproximation, "normalApproximation")
    .assertIsValidVarianceOptionMultiArmed(design, varianceOption)
    .warnInCaseOfUnknownArguments(
        functionName = ".getStageResultsMeansMultiArm",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), "stage"), ...
    )

    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    gMax <- dataInput$getNumberOfGroups() - 1
    kMax <- design$kMax

    if (.isTrialDesignConditionalDunnett(design)) {
        if (!normalApproximation) {
            if (userFunctionCallEnabled) {
                warning("'normalApproximation' was set to TRUE ",
                    "because conditional Dunnett test was specified as design",
                    call. = FALSE
                )
            }
            normalApproximation <- TRUE
        }
    }
    intersectionTest <- .getCorrectedIntersectionTestMultiArmIfNecessary(
        design, intersectionTest, userFunctionCallEnabled
    )
    .assertIsValidIntersectionTestMultiArm(design, intersectionTest)

    if (intersectionTest == "Dunnett" && varianceOption != "overallPooled" &&
            !normalApproximation) {
        stop("Dunnett t test can only be performed with overall variance estimation,
						select 'varianceOption' = \"overallPooled\"", call. = FALSE)
    }

    stageResults <- StageResultsMultiArmMeansR6$new(
        design = design,
        dataInput = dataInput,
        thetaH0 = thetaH0,
        direction = ifelse(directionUpper, C_DIRECTION_UPPER, C_DIRECTION_LOWER),
        normalApproximation = normalApproximation,
        directionUpper = directionUpper,
        varianceOption = varianceOption,
        stage = stage
    )

    .setValueAndParameterType(
        stageResults, "intersectionTest",
        intersectionTest, C_INTERSECTION_TEST_MULTIARMED_DEFAULT
    )
    effectSizes <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallStDevs <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallPooledStDevs <- matrix(rep(NA_real_, kMax), 1, kMax)
    testStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallTestStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    separatePValues <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallPValues <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    dimnames(testStatistics) <- list(paste("arm ", 1:gMax, sep = ""), paste("stage ", (1:kMax), sep = ""))
    dimnames(overallTestStatistics) <- list(
        paste("arm ", 1:gMax, sep = ""),
        paste("stage ", (1:kMax), sep = "")
    )
    dimnames(separatePValues) <- list(paste("arm ", 1:gMax, sep = ""), paste("stage ", (1:kMax), sep = ""))
    dimnames(overallPValues) <- list(paste("arm ", 1:gMax, sep = ""), paste("stage ", (1:kMax), sep = ""))

    for (k in 1:stage) {
        overallPooledStDevs[1, k] <- sqrt(sum((dataInput$getOverallSampleSizes(stage = k) - 1) *
            dataInput$getOverallStDevs(stage = k)^2, na.rm = TRUE) /
            sum(dataInput$getOverallSampleSizes(stage = k) - 1, na.rm = TRUE))

        if (varianceOption == "overallPooled") {
            stDev <- sqrt(sum((dataInput$getSampleSizes(stage = k) - 1) *
                dataInput$getStDevs(stage = k)^2, na.rm = TRUE) /
                sum(dataInput$getSampleSizes(stage = k) - 1, na.rm = TRUE))
            overallStDevForTest <- overallPooledStDevs[1, k]
        }

        for (treatmentArm in 1:gMax) {
            effectSizes[treatmentArm, k] <- dataInput$getOverallMeans(stage = k, group = treatmentArm) -
                dataInput$getOverallMeans(stage = k, group = gMax + 1)

            overallStDevs[treatmentArm, k] <- sqrt(sum((
                dataInput$getOverallSampleSize(stage = k, group = c(treatmentArm, gMax + 1)) - 1) *
                dataInput$getOverallStDev(stage = k, group = c(treatmentArm, gMax + 1))^2, na.rm = TRUE) /
                sum(dataInput$getOverallSampleSize(stage = k, group = c(treatmentArm, gMax + 1)) - 1))

            if (varianceOption == "pairwisePooled") {
                stDev <- sqrt(sum((dataInput$getSampleSizes(stage = k, group = c(treatmentArm, gMax + 1)) - 1) *
                    dataInput$getStDevs(stage = k, group = c(treatmentArm, gMax + 1))^2, na.rm = TRUE) /
                    sum(dataInput$getSampleSizes(stage = k, group = c(treatmentArm, gMax + 1)) - 1))
                overallStDevForTest <- overallStDevs[treatmentArm, k]
            }

            if (varianceOption == "notPooled") {
                testStatistics[treatmentArm, k] <- (dataInput$getMeans(stage = k, group = treatmentArm) -
                    dataInput$getMeans(stage = k, group = gMax + 1) - thetaH0) /
                    sqrt(dataInput$getStDevs(stage = k, group = treatmentArm)^2 /
                        dataInput$getSampleSizes(stage = k, group = treatmentArm) +
                        dataInput$getStDevs(stage = k, group = gMax + 1)^2 /
                            dataInput$getSampleSizes(stage = k, group = gMax + 1))
                overallTestStatistics[treatmentArm, k] <- (
                    dataInput$getOverallMeans(stage = k, group = treatmentArm) -
                        dataInput$getOverallMeans(stage = k, group = gMax + 1) - thetaH0) /
                    sqrt(dataInput$getOverallStDevs(stage = k, group = treatmentArm)^2 /
                        dataInput$getOverallSampleSizes(stage = k, group = treatmentArm) +
                        dataInput$getOverallStDevs(stage = k, group = gMax + 1)^2 /
                            dataInput$getOverallSampleSizes(stage = k, group = gMax + 1))
            } else {
                testStatistics[treatmentArm, k] <- (dataInput$getMeans(stage = k, group = treatmentArm) -
                    dataInput$getMeans(stage = k, group = gMax + 1) - thetaH0) / stDev /
                    sqrt(1 / dataInput$getSampleSizes(stage = k, group = treatmentArm) + 1 /
                        dataInput$getSampleSizes(stage = k, group = gMax + 1))
                overallTestStatistics[treatmentArm, k] <- (
                    dataInput$getOverallMeans(stage = k, group = treatmentArm) -
                        dataInput$getOverallMeans(stage = k, group = gMax + 1) - thetaH0) /
                    overallStDevForTest /
                    sqrt(1 / dataInput$getOverallSampleSizes(stage = k, group = treatmentArm) + 1 /
                        dataInput$getOverallSampleSizes(stage = k, group = gMax + 1))
            }

            if (normalApproximation) {
                separatePValues[treatmentArm, k] <- 1 - stats::pnorm(testStatistics[treatmentArm, k])
                overallPValues[treatmentArm, k] <- 1 - stats::pnorm(overallTestStatistics[treatmentArm, k])
            } else {
                if (varianceOption == "overallPooled") {
                    separatePValues[treatmentArm, k] <- 1 - stats::pt(
                        testStatistics[treatmentArm, k],
                        sum(dataInput$getSampleSizes(stage = k) - 1, na.rm = TRUE)
                    )
                    overallPValues[treatmentArm, k] <- 1 - stats::pt(
                        overallTestStatistics[treatmentArm, k],
                        sum(dataInput$getOverallSampleSizes(stage = k) - 1, na.rm = TRUE)
                    )
                } else if (varianceOption == "pairwisePooled") {
                    separatePValues[treatmentArm, k] <- 1 - stats::pt(
                        testStatistics[treatmentArm, k],
                        sum(dataInput$getSampleSizes(stage = k, group = c(treatmentArm, gMax + 1)) - 1)
                    )
                    overallPValues[treatmentArm, k] <- 1 - stats::pt(
                        overallTestStatistics[treatmentArm, k],
                        sum(dataInput$getOverallSampleSizes(stage = k, group = c(treatmentArm, gMax + 1)) - 1)
                    )
                } else if (varianceOption == "notPooled") {
                    u <- dataInput$getStDevs(stage = k, group = treatmentArm)^2 /
                        dataInput$getSampleSizes(stage = k, group = treatmentArm) /
                        (dataInput$getStDevs(stage = k, group = treatmentArm)^2 /
                            dataInput$getSampleSizes(stage = k, group = treatmentArm) +
                            dataInput$getStDevs(stage = k, group = gMax + 1)^2 /
                                dataInput$getSampleSizes(stage = k, group = gMax + 1))
                    separatePValues[treatmentArm, k] <- 1 - stats::pt(
                        testStatistics[treatmentArm, k],
                        1 / (u^2 / (dataInput$getSampleSizes(stage = k, group = treatmentArm) - 1) +
                            (1 - u)^2 / (dataInput$getSampleSizes(stage = k, group = gMax + 1) - 1))
                    )
                    u <- dataInput$getOverallStDevs(stage = k, group = treatmentArm)^2 /
                        dataInput$getOverallSampleSizes(stage = k, group = treatmentArm) /
                        (dataInput$getOverallStDevs(stage = k, group = treatmentArm)^2 /
                            dataInput$getOverallSampleSizes(stage = k, group = treatmentArm) +
                            dataInput$getOverallStDevs(stage = k, group = gMax + 1)^2 /
                                dataInput$getOverallSampleSizes(stage = k, group = gMax + 1))
                    overallPValues[treatmentArm, k] <- 1 - stats::pt(
                        overallTestStatistics[treatmentArm, k],
                        1 / (u^2 / (dataInput$getOverallSampleSizes(stage = k, group = treatmentArm) - 1) +
                            (1 - u)^2 / (dataInput$getOverallSampleSizes(stage = k, group = gMax + 1) - 1))
                    )
                }
            }
            if (!directionUpper) {
                separatePValues[treatmentArm, k] <- 1 - separatePValues[treatmentArm, k]
                overallPValues[treatmentArm, k] <- 1 - overallPValues[treatmentArm, k]
                # testStatistics[treatmentArm, k] <- -testStatistics[treatmentArm, k]
                # overallTestStatistics[treatmentArm, k] <- -overallTestStatistics[treatmentArm, k]
            }
        }
    }

    .setWeightsToStageResults(design, stageResults)

    # Calculation of single stage adjusted p-Values and overall test statistics
    # for determination of RCIs
    if (calculateSingleStepAdjusted) {
        singleStepAdjustedPValues <- matrix(NA_real_, nrow = gMax, ncol = kMax)
        combInverseNormal <- matrix(NA_real_, nrow = gMax, ncol = kMax)
        combFisher <- matrix(NA_real_, nrow = gMax, ncol = kMax)

        if (.isTrialDesignInverseNormal(design)) {
            weightsInverseNormal <- stageResults$weightsInverseNormal
        } else if (.isTrialDesignFisher(design)) {
            weightsFisher <- stageResults$weightsFisher
        }

        for (k in 1:stage) {
            selected <- sum(!is.na(separatePValues[, k]))
            sampleSizesSelected <- as.numeric(na.omit(
                dataInput$getSampleSizes(stage = k, group = -(gMax + 1))
            ))
            sigma <- sqrt(sampleSizesSelected /
                (sampleSizesSelected + dataInput$getSampleSize(k, gMax + 1))) %*%
                sqrt(t(sampleSizesSelected / (sampleSizesSelected +
                    dataInput$getSampleSize(k, gMax + 1))))
            diag(sigma) <- 1
            for (treatmentArm in 1:gMax) {
                if (intersectionTest == "Bonferroni" || intersectionTest == "Simes") {
                    if (.isTrialDesignGroupSequential(design)) {
                        overallPValues[treatmentArm, k] <- min(1, overallPValues[treatmentArm, k] * selected)
                    } else {
                        singleStepAdjustedPValues[treatmentArm, k] <- min(
                            1,
                            separatePValues[treatmentArm, k] * selected
                        )
                    }
                } else if (intersectionTest == "Sidak") {
                    if (.isTrialDesignGroupSequential(design)) {
                        overallPValues[treatmentArm, k] <- 1 - (1 - overallPValues[treatmentArm, k])^selected
                    } else {
                        singleStepAdjustedPValues[treatmentArm, k] <- 1 - (1 -
                            separatePValues[treatmentArm, k])^selected
                    }
                } else if (intersectionTest == "Dunnett") {
                    if (!is.na(testStatistics[treatmentArm, k])) {
                        df <- NA_real_
                        if (!normalApproximation) {
                            df <- sum(dataInput$getSampleSizes(stage = k) - 1, na.rm = TRUE)
                        }
                        singleStepAdjustedPValues[treatmentArm, k] <- 1 - .getMultivariateDistribution(
                            type = ifelse(normalApproximation, "normal", "t"),
                            upper = ifelse(directionUpper,
                                testStatistics[treatmentArm, k], -testStatistics[treatmentArm, k]
                            ),
                            sigma = sigma, df = df
                        )
                    }
                }
                if (.isTrialDesignInverseNormal(design)) {
                    combInverseNormal[treatmentArm, k] <- (weightsInverseNormal[1:k] %*%
                        .getOneMinusQNorm(singleStepAdjustedPValues[treatmentArm, 1:k])) /
                        sqrt(sum(weightsInverseNormal[1:k]^2))
                } else if (.isTrialDesignFisher(design)) {
                    combFisher[treatmentArm, k] <- prod(
                        singleStepAdjustedPValues[treatmentArm, 1:k]^weightsFisher[1:k]
                    )
                }
            }
        }

        stageResults$overallTestStatistics <- overallTestStatistics
        stageResults$overallPValues <- overallPValues
        stageResults$effectSizes <- effectSizes
        stageResults$overallStDevs <- overallStDevs
        stageResults$testStatistics <- testStatistics
        stageResults$separatePValues <- separatePValues
        stageResults$singleStepAdjustedPValues <- singleStepAdjustedPValues
        stageResults$.setParameterType("singleStepAdjustedPValues", C_PARAM_GENERATED)

        if (.isTrialDesignFisher(design)) {
            stageResults$combFisher <- combFisher
            stageResults$.setParameterType("combFisher", C_PARAM_GENERATED)
        } else if (.isTrialDesignInverseNormal(design)) {
            stageResults$combInverseNormal <- combInverseNormal
            stageResults$.setParameterType("combInverseNormal", C_PARAM_GENERATED)
        }
    } else {
        stageResults$overallTestStatistics <- overallTestStatistics
        stageResults$overallPValues <- overallPValues
        stageResults$effectSizes <- effectSizes
        stageResults$overallStDevs <- overallStDevs
        stageResults$overallPooledStDevs <- overallPooledStDevs
        stageResults$testStatistics <- testStatistics
        stageResults$separatePValues <- separatePValues
    }

    return(stageResults)
}

.getRootThetaMeansMultiArm <- function(..., design, dataInput, treatmentArm, stage,
        directionUpper, normalApproximation, varianceOption, intersectionTest,
        thetaLow, thetaUp, firstParameterName, secondValue, tolerance) {
    result <- .getOneDimensionalRoot(
        function(theta) {
            stageResults <- .getStageResultsMeansMultiArm(
                design = design, dataInput = dataInput,
                stage = stage, thetaH0 = theta, directionUpper = directionUpper,
                intersectionTest = intersectionTest, normalApproximation = normalApproximation,
                varianceOption = varianceOption, calculateSingleStepAdjusted = TRUE
            )
            firstValue <- stageResults[[firstParameterName]][treatmentArm, stage]
            if (.isTrialDesignGroupSequential(design)) {
                firstValue <- .getOneMinusQNorm(firstValue)
            }
            return(firstValue - secondValue)
        },
        lower = thetaLow, upper = thetaUp, tolerance = tolerance,
        callingFunctionInformation = ".getRootThetaMeansMultiArm"
    )
    return(result)
}

.getUpperLowerThetaMeansMultiArm <- function(..., design, dataInput, theta, treatmentArm, stage,
        directionUpper, normalApproximation, varianceOption, conditionFunction, intersectionTest,
        firstParameterName, secondValue) {
    stageResults <- .getStageResultsMeansMultiArm(
        design = design, dataInput = dataInput,
        stage = stage, thetaH0 = theta, directionUpper = directionUpper,
        intersectionTest = intersectionTest, normalApproximation = normalApproximation,
        varianceOption = varianceOption, calculateSingleStepAdjusted = TRUE
    )

    firstValue <- stageResults[[firstParameterName]][treatmentArm, stage]
    maxSearchIterations <- 30
    while (conditionFunction(secondValue, firstValue)) {
        theta <- 2 * theta
        stageResults <- .getStageResultsMeansMultiArm(
            design = design, dataInput = dataInput,
            stage = stage, thetaH0 = theta, directionUpper = directionUpper,
            intersectionTest = intersectionTest, normalApproximation = normalApproximation,
            varianceOption = varianceOption, calculateSingleStepAdjusted = TRUE
        )

        firstValue <- stageResults[[firstParameterName]][treatmentArm, stage]
        maxSearchIterations <- maxSearchIterations - 1
        if (maxSearchIterations < 0) {
            stop(
                C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                sprintf(
                    paste0(
                        "failed to find theta (k = %s, firstValue = %s, ",
                        "secondValue = %s, levels(firstValue) = %s, theta = %s)"
                    ),
                    stage, stageResults[[firstParameterName]][treatmentArm, stage], secondValue,
                    firstValue, theta
                )
            )
        }
    }

    return(theta)
}

.getRepeatedConfidenceIntervalsMeansMultiArmAll <- function(...,
        design, dataInput,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        varianceOption = C_VARIANCE_OPTION_MULTIARMED_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        firstParameterName) {
    .assertIsValidIntersectionTestMultiArm(design, intersectionTest)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    stageResults <- .getStageResultsMeansMultiArm(
        design = design, dataInput = dataInput,
        stage = stage, thetaH0 = 0, directionUpper = directionUpper,
        intersectionTest = intersectionTest, normalApproximation = normalApproximation,
        varianceOption = varianceOption, calculateSingleStepAdjusted = FALSE
    )

    gMax <- dataInput$getNumberOfGroups() - 1
    repeatedConfidenceIntervals <- array(NA_real_, dim = c(gMax, 2, design$kMax))

    # Confidence interval for second stage when using conditional Dunnett test
    if (.isTrialDesignConditionalDunnett(design)) {
        startTime <- Sys.time()
        for (treatmentArm in 1:gMax) {
            if (!is.na(stageResults$testStatistics[treatmentArm, 2])) {
                thetaLowLimit <- -1
                iteration <- 30
                rejected <- FALSE
                while (!rejected && iteration >= 0) {
                    stageResults <- .getStageResultsMeansMultiArm(
                        design = design, dataInput = dataInput,
                        stage = stage, thetaH0 = thetaLowLimit, directionUpper = TRUE,
                        intersectionTest = intersectionTest, normalApproximation = normalApproximation,
                        varianceOption = varianceOption, calculateSingleStepAdjusted = FALSE
                    )
                    rejected <- .getConditionalDunnettTestForCI(
                        design = design,
                        stageResults = stageResults, treatmentArm = treatmentArm
                    )
                    iteration <- iteration - 1
                    thetaLowLimit <- 2 * thetaLowLimit
                }

                iteration <- 30
                thetaUpLimit <- 1
                rejected <- FALSE
                while (!rejected && iteration >= 0) {
                    stageResults <- .getStageResultsMeansMultiArm(
                        design = design, dataInput = dataInput,
                        stage = stage, thetaH0 = thetaUpLimit, directionUpper = FALSE,
                        intersectionTest = intersectionTest, normalApproximation = normalApproximation,
                        varianceOption = varianceOption, calculateSingleStepAdjusted = FALSE
                    )
                    rejected <- .getConditionalDunnettTestForCI(
                        design = design,
                        stageResults = stageResults, treatmentArm = treatmentArm
                    )
                    iteration <- iteration - 1
                    thetaUpLimit <- 2 * thetaUpLimit
                }

                thetaLow <- thetaLowLimit
                thetaUp <- thetaUpLimit
                iteration <- 30
                prec <- 1
                while (prec > tolerance) {
                    theta <- (thetaLow + thetaUp) / 2
                    stageResults <- .getStageResultsMeansMultiArm(
                        design = design, dataInput = dataInput,
                        stage = stage, thetaH0 = theta, directionUpper = TRUE,
                        intersectionTest = intersectionTest, normalApproximation = normalApproximation,
                        varianceOption = varianceOption, calculateSingleStepAdjusted = FALSE
                    )
                    conditionalDunnettSingleStepRejected <- .getConditionalDunnettTestForCI(
                        design = design, stageResults = stageResults, treatmentArm = treatmentArm
                    )
                    ifelse(conditionalDunnettSingleStepRejected, thetaLow <- theta, thetaUp <- theta)
                    ifelse(iteration > 0, prec <- thetaUp - thetaLow, prec <- 0)
                    iteration <- iteration - 1
                }
                repeatedConfidenceIntervals[treatmentArm, 1, 2] <- theta

                thetaLow <- thetaLowLimit
                thetaUp <- thetaUpLimit
                iteration <- 30
                prec <- 1
                while (prec > tolerance) {
                    theta <- (thetaLow + thetaUp) / 2
                    stageResults <- .getStageResultsMeansMultiArm(
                        design = design, dataInput = dataInput,
                        stage = stage, thetaH0 = theta, directionUpper = FALSE,
                        intersectionTest = intersectionTest, normalApproximation = normalApproximation,
                        varianceOption = varianceOption, calculateSingleStepAdjusted = FALSE
                    )
                    conditionalDunnettSingleStepRejected <- .getConditionalDunnettTestForCI(
                        design = design, stageResults = stageResults, treatmentArm = treatmentArm
                    )
                    ifelse(conditionalDunnettSingleStepRejected, thetaUp <- theta, thetaLow <- theta)
                    ifelse(iteration > 0, prec <- thetaUp - thetaLow, prec <- 0)
                    iteration <- iteration - 1
                }
                repeatedConfidenceIntervals[treatmentArm, 2, 2] <- theta

                if (!is.na(repeatedConfidenceIntervals[treatmentArm, 1, 2]) &&
                        !is.na(repeatedConfidenceIntervals[treatmentArm, 2, 2]) &&
                        repeatedConfidenceIntervals[treatmentArm, 1, 2] >
                            repeatedConfidenceIntervals[treatmentArm, 2, 2]) {
                    repeatedConfidenceIntervals[treatmentArm, , 2] <- rep(NA_real_, 2)
                }
            }
        }
        .logProgress("Confidence intervals for final stage calculated", startTime = startTime)
    } else {
        # Repeated onfidence intervals when using combination tests
        if (intersectionTest == "Hierarchical") {
            warning("Repeated confidence intervals not available for ",
                "'intersectionTest' = \"Hierarchical\"",
                call. = FALSE
            )
            return(repeatedConfidenceIntervals)
        }

        if (.isTrialDesignFisher(design)) {
            bounds <- design$alpha0Vec
            border <- C_ALPHA_0_VEC_DEFAULT
            criticalValues <- design$criticalValues
            conditionFunction <- .isFirstValueSmallerThanSecondValue
        } else if (.isTrialDesignInverseNormal(design)) {
            bounds <- design$futilityBounds
            border <- C_FUTILITY_BOUNDS_DEFAULT
            criticalValues <- design$criticalValues
            criticalValues[is.infinite(criticalValues) & criticalValues > 0] <- C_QNORM_MAXIMUM
            criticalValues[is.infinite(criticalValues) & criticalValues < 0] <- C_QNORM_MINIMUM
            conditionFunction <- .isFirstValueGreaterThanSecondValue
        }

        # Necessary for adjustment for binding futility boundaries
        futilityCorr <- rep(NA_real_, design$kMax)

        stages <- (1:stage)
        for (k in stages) {
            startTime <- Sys.time()
            for (treatmentArm in 1:gMax) {
                if (!is.na(stageResults$testStatistics[treatmentArm, k]) && criticalValues[k] < C_QNORM_MAXIMUM) {
                    # finding maximum upper and minimum lower bounds for RCIs
                    thetaLow <- .getUpperLowerThetaMeansMultiArm(
                        design = design, dataInput = dataInput,
                        theta = -1, treatmentArm = treatmentArm, stage = k, directionUpper = TRUE,
                        normalApproximation = normalApproximation, varianceOption = varianceOption,
                        conditionFunction = conditionFunction,
                        intersectionTest = intersectionTest, firstParameterName = firstParameterName,
                        secondValue = criticalValues[k]
                    )

                    thetaUp <- .getUpperLowerThetaMeansMultiArm(
                        design = design, dataInput = dataInput,
                        theta = 1, treatmentArm = treatmentArm, stage = k, directionUpper = FALSE,
                        normalApproximation = normalApproximation, varianceOption = varianceOption,
                        conditionFunction = conditionFunction,
                        intersectionTest = intersectionTest, firstParameterName = firstParameterName,
                        secondValue = criticalValues[k]
                    )

                    # finding upper and lower RCI limits through root function
                    repeatedConfidenceIntervals[treatmentArm, 1, k] <- .getRootThetaMeansMultiArm(
                        design = design,
                        dataInput = dataInput, treatmentArm = treatmentArm, stage = k, directionUpper = TRUE,
                        normalApproximation = normalApproximation, varianceOption = varianceOption,
                        thetaLow = thetaLow, thetaUp = thetaUp,
                        intersectionTest = intersectionTest, firstParameterName = firstParameterName,
                        secondValue = criticalValues[k], tolerance = tolerance
                    )

                    repeatedConfidenceIntervals[treatmentArm, 2, k] <- .getRootThetaMeansMultiArm(
                        design = design,
                        dataInput = dataInput, treatmentArm = treatmentArm, stage = k, directionUpper = FALSE,
                        normalApproximation = normalApproximation, varianceOption = varianceOption,
                        thetaLow = thetaLow, thetaUp = thetaUp,
                        intersectionTest = intersectionTest, firstParameterName = firstParameterName,
                        secondValue = criticalValues[k], tolerance = tolerance
                    )

                    # adjustment for binding futility bounds
                    if (k > 1 && !is.na(bounds[k - 1]) && conditionFunction(bounds[k - 1], border) && design$bindingFutility) {
                        parameterName <- ifelse(.isTrialDesignFisher(design),
                            "singleStepAdjustedPValues", firstParameterName
                        )

                        #  Calculate new lower and upper bounds
                        if (directionUpper) {
                            thetaLow <- .getUpperLowerThetaMeansMultiArm(
                                design = design,
                                dataInput = dataInput,
                                theta = -1, treatmentArm = treatmentArm, stage = k - 1, directionUpper = TRUE,
                                normalApproximation = normalApproximation, varianceOption = varianceOption,
                                conditionFunction = conditionFunction,
                                intersectionTest = intersectionTest, firstParameterName = parameterName,
                                secondValue = bounds[k - 1]
                            )
                        } else {
                            thetaUp <- .getUpperLowerThetaMeansMultiArm(
                                design = design,
                                dataInput = dataInput,
                                theta = 1, treatmentArm = treatmentArm, stage = k - 1, directionUpper = FALSE,
                                normalApproximation = normalApproximation, varianceOption = varianceOption,
                                conditionFunction = conditionFunction,
                                intersectionTest = intersectionTest, firstParameterName = parameterName,
                                secondValue = bounds[k - 1]
                            )
                        }

                        futilityCorr[k] <- .getRootThetaMeansMultiArm(
                            design = design, dataInput = dataInput,
                            treatmentArm = treatmentArm, stage = k - 1, directionUpper = directionUpper,
                            normalApproximation = normalApproximation, varianceOption = varianceOption,
                            thetaLow = thetaLow, thetaUp = thetaUp,
                            intersectionTest = intersectionTest, firstParameterName = parameterName,
                            secondValue = bounds[k - 1], tolerance = tolerance
                        )

                        if (directionUpper) {
                            repeatedConfidenceIntervals[treatmentArm, 1, k] <- min(
                                min(futilityCorr[2:k]),
                                repeatedConfidenceIntervals[treatmentArm, 1, k]
                            )
                        } else {
                            repeatedConfidenceIntervals[treatmentArm, 2, k] <- max(
                                max(futilityCorr[2:k]),
                                repeatedConfidenceIntervals[treatmentArm, 2, k]
                            )
                        }
                    }

                    if (!is.na(repeatedConfidenceIntervals[treatmentArm, 1, k]) &&
                            !is.na(repeatedConfidenceIntervals[treatmentArm, 2, k]) &&
                            repeatedConfidenceIntervals[treatmentArm, 1, k] >
                                repeatedConfidenceIntervals[treatmentArm, 2, k]) {
                        repeatedConfidenceIntervals[treatmentArm, , k] <- rep(NA_real_, 2)
                    }
                }
            }
            .logProgress("Repeated confidence intervals for stage %s calculated", startTime = startTime, k)
        }
    }

    return(repeatedConfidenceIntervals)
}

#'
#' RCIs based on inverse normal combination test
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsMeansMultiArmInverseNormal <- function(...,
        design, dataInput,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        varianceOption = C_VARIANCE_OPTION_MULTIARMED_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsMeansMultiArmInverseNormal",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsMeansMultiArmAll(
        design = design, dataInput = dataInput,
        normalApproximation = normalApproximation, varianceOption = varianceOption,
        directionUpper = directionUpper, intersectionTest = intersectionTest,
        tolerance = tolerance, firstParameterName = "combInverseNormal", ...
    ))
}

#'
#' RCIs based on Fisher's combination test
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsMeansMultiArmFisher <- function(...,
        design, dataInput,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        varianceOption = C_VARIANCE_OPTION_MULTIARMED_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsMeansMultiArmFisher",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsMeansMultiArmAll(
        design = design, dataInput = dataInput,
        normalApproximation = normalApproximation, varianceOption = varianceOption,
        directionUpper = directionUpper, intersectionTest = intersectionTest,
        tolerance = tolerance, firstParameterName = "combFisher", ...
    ))
}

#'
#' CIs based on conditional Dunnett test
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsMeansMultiArmConditionalDunnett <- function(...,
        design, dataInput,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        varianceOption = C_VARIANCE_OPTION_MULTIARMED_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsMeansMultiArmConditionalDunnett",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsMeansMultiArmAll(
        design = design, dataInput = dataInput,
        normalApproximation = normalApproximation, varianceOption = varianceOption,
        directionUpper = directionUpper, intersectionTest = intersectionTest,
        tolerance = tolerance, firstParameterName = NA, ...
    ))
}

#'
#' Calculation of lower and upper limits of repeated confidence intervals (RCIs) for Means
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsMeansMultiArm <- function(..., design) {
    if (.isTrialDesignInverseNormal(design)) {
        return(.getRepeatedConfidenceIntervalsMeansMultiArmInverseNormal(design = design, ...))
    }
    if (.isTrialDesignFisher(design)) {
        return(.getRepeatedConfidenceIntervalsMeansMultiArmFisher(design = design, ...))
    }
    if (.isTrialDesignConditionalDunnett(design)) {
        return(.getRepeatedConfidenceIntervalsMeansMultiArmConditionalDunnett(design = design, ...))
    }
    .stopWithWrongDesignMessage(design)
}

#'
#' Calculation of conditional power for Means
#'
#' @noRd
#'
.getConditionalPowerMeansMultiArm <- function(..., stageResults, stage = stageResults$stage,
        nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        thetaH1 = NA_real_, assumedStDevs = NA_real_,
        iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
    stDevsH1 <- .getOptionalArgument("stDevsH1", ...)
    if (!is.null(stDevsH1) && !is.na(stDevsH1)) {
        if (!is.na(assumedStDevs)) {
            warning(sQuote("assumedStDevs"), " will be ignored because ",
                sQuote("stDevsH1"), " is defined",
                call. = FALSE
            )
        }
        assumedStDevs <- stDevsH1
    }

    design <- stageResults$.design
    gMax <- stageResults$getGMax()
    kMax <- design$kMax

    results <- ConditionalPowerResultsMultiArmMeansR6$new(
        .design = design,
        .stageResults = stageResults,
        thetaH1 = thetaH1,
        assumedStDevs = assumedStDevs,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned
    )

    if (any(is.na(nPlanned))) {
        return(results)
    }

    .assertIsValidStage(stage, kMax)
    if (stage == kMax) {
        .logDebug(
            "Conditional power will be calculated only for subsequent stages ",
            "(stage = ", stage, ", kMax = ", kMax, ")"
        )
        return(results)
    }

    if (!.isValidNPlanned(nPlanned = nPlanned, kMax = kMax, stage = stage)) {
        return(results)
    }

    .assertIsValidNPlanned(nPlanned, kMax, stage)
    .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
    .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM)
    .setValueAndParameterType(results, "allocationRatioPlanned", allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT)
    assumedStDevs <- .assertIsValidAssumedStDevForMultiHypotheses(
        assumedStDevs, stageResults, stage,
        results = results
    )
    .assertIsValidAssumedStDevs(assumedStDevs, gMax)
    thetaH1 <- .assertIsValidThetaH1ForMultiArm(thetaH1, stageResults, stage, results = results)
    results$.setParameterType("nPlanned", C_PARAM_USER_DEFINED)
    if (length(thetaH1) != 1 && length(thetaH1) != gMax) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            sprintf(paste0(
                "length of 'thetaH1' (%s) ",
                "must be equal to 'gMax' (%s) or 1"
            ), .arrayToString(thetaH1), gMax)
        )
    }

    if (length(assumedStDevs) == 1) {
        results$assumedStDevs <- rep(assumedStDevs, gMax)
        results$.setParameterType("assumedStDevs", C_PARAM_GENERATED)
    }

    if (.isTrialDesignInverseNormal(design)) {
        return(.getConditionalPowerMeansMultiArmInverseNormal(
            results = results, stageResults = stageResults, stage = stage,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            thetaH1 = thetaH1,
            assumedStDevs = assumedStDevs, ...
        ))
    } else if (.isTrialDesignFisher(design)) {
        return(.getConditionalPowerMeansMultiArmFisher(
            results = results, stageResults = stageResults, stage = stage,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            thetaH1 = thetaH1,
            assumedStDevs = assumedStDevs,
            iterations = iterations, seed = seed, ...
        ))
    } else if (.isTrialDesignConditionalDunnett(design)) {
        return(.getConditionalPowerMeansMultiArmConditionalDunnett(
            results = results, stageResults = stageResults, stage = stage,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            thetaH1 = thetaH1,
            assumedStDevs = assumedStDevs, ...
        ))
    }

    stop(
        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
        "'design' must be an instance of TrialDesignInverseNormal, TrialDesignFisher, or ",
        "TrialDesignConditionalDunnett"
    )
}

#'
#' Calculation of conditional power based on inverse normal method
#'
#' @noRd
#'
.getConditionalPowerMeansMultiArmInverseNormal <- function(..., results, stageResults, stage,
        allocationRatioPlanned, nPlanned, thetaH1, assumedStDevs) {
    design <- stageResults$.design
    .assertIsTrialDesignInverseNormal(design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerMeansMultiArmInverseNormal",
        ignore = c("stage", "design", "stDevsH1"), ...
    )

    kMax <- design$kMax
    gMax <- stageResults$getGMax()
    # results$conditionalPower <- matrix(NA_real_, nrow = gMax, ncol = kMax)

    weights <- .getWeightsInverseNormal(design)
    informationRates <- design$informationRates
    nPlanned <- c(rep(NA_real_, stage), nPlanned)
    nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned

    if (length(thetaH1) == 1) {
        thetaH1 <- rep(thetaH1, gMax)
        results$.setParameterType("thetaH1", C_PARAM_GENERATED)
    } else {
        results$.setParameterType("thetaH1", C_PARAM_DEFAULT_VALUE)
    }
    results$.setParameterType("assumedStDevs", C_PARAM_DEFAULT_VALUE)

    if (stageResults$directionUpper) {
        standardizedEffect <- (thetaH1 - stageResults$thetaH0) / assumedStDevs
    } else {
        standardizedEffect <- -(thetaH1 - stageResults$thetaH0) / assumedStDevs
    }
    ctr <- .performClosedCombinationTest(stageResults = stageResults)
    criticalValues <- design$criticalValues

    for (treatmentArm in 1:gMax) {
        if (!is.na(ctr$separatePValues[treatmentArm, stage])) {
            # shifted decision region for use in getGroupSeqProbs
            # Inverse Normal Method
            shiftedDecisionRegionUpper <- criticalValues[(stage + 1):kMax] *
                sqrt(sum(weights[1:stage]^2) + cumsum(weights[(stage + 1):kMax]^2)) /
                sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
                min(ctr$overallAdjustedTestStatistics[ctr$indices[, treatmentArm] == 1, stage], na.rm = TRUE) *
                    sqrt(sum(weights[1:stage]^2)) /
                    sqrt(cumsum(weights[(stage + 1):kMax]^2)) - standardizedEffect[treatmentArm] *
                    cumsum(sqrt(nPlanned[(stage + 1):kMax]) * weights[(stage + 1):kMax]) /
                    sqrt(cumsum(weights[(stage + 1):kMax]^2))
            if (stage == kMax - 1) {
                shiftedFutilityBounds <- c()
            } else {
                shiftedFutilityBounds <- design$futilityBounds[(stage + 1):(kMax - 1)] *
                    sqrt(sum(weights[1:stage]^2) + cumsum(weights[(stage + 1):(kMax - 1)]^2)) /
                    sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2)) -
                    min(ctr$overallAdjustedTestStatistics[ctr$indices[, treatmentArm] == 1, stage], na.rm = TRUE) *
                        sqrt(sum(weights[1:stage]^2)) /
                        sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2)) - standardizedEffect[treatmentArm] *
                        cumsum(sqrt(nPlanned[(stage + 1):(kMax - 1)]) * weights[(stage + 1):(kMax - 1)]) /
                        sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2))
            }

            # scaled information for use in getGroupSeqProbs
            scaledInformation <- (informationRates[(stage + 1):kMax] - informationRates[stage]) /
                (1 - informationRates[stage])

            decisionMatrix <- matrix(c(
                shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT,
                shiftedDecisionRegionUpper
            ), nrow = 2, byrow = TRUE)

            probs <- .getGroupSequentialProbabilities(
                decisionMatrix = decisionMatrix,
                informationRates = scaledInformation
            )

            results$conditionalPower[treatmentArm, (stage + 1):kMax] <- cumsum(probs[3, ] - probs[2, ])
        }
    }
    nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned

    results$nPlanned <- nPlanned
    results$.setParameterType("nPlanned", C_PARAM_GENERATED)

    results$.setParameterType("conditionalPower", C_PARAM_GENERATED)

    results$thetaH1 <- thetaH1
    results$assumedStDevs <- assumedStDevs
    return(results)
}

#'
#' Calculation of conditional power based on Fisher's combination test
#'
#' @noRd
#'
.getConditionalPowerMeansMultiArmFisher <- function(..., results, stageResults, stage,
        allocationRatioPlanned, nPlanned, thetaH1, assumedStDevs,
        iterations, seed) {
    design <- stageResults$.design
    .assertIsTrialDesignFisher(design)
    .assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerMeansMultiArmFisher",
        ignore = c("stage", "design", "stDevsH1"), ...
    )
    kMax <- design$kMax
    gMax <- stageResults$getGMax()
    criticalValues <- design$criticalValues
    weightsFisher <- .getWeightsFisher(design)

    # results$conditionalPower <- matrix(NA_real_, nrow = gMax, ncol = kMax)

    results$iterations <- as.integer(iterations)
    results$.setParameterType("iterations", C_PARAM_USER_DEFINED)
    results$.setParameterType("seed", ifelse(is.na(seed), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
    results$seed <- .setSeed(seed)
    results$simulated <- FALSE
    results$.setParameterType("simulated", C_PARAM_DEFAULT_VALUE)

    if (length(thetaH1) == 1) {
        thetaH1 <- rep(thetaH1, gMax)
        results$.setParameterType("thetaH1", C_PARAM_GENERATED)
    } else {
        results$.setParameterType("thetaH1", C_PARAM_DEFAULT_VALUE)
    }
    results$.setParameterType("assumedStDevs", C_PARAM_DEFAULT_VALUE)

    if (stageResults$directionUpper) {
        standardizedEffect <- (thetaH1 - stageResults$thetaH0) / assumedStDevs
    } else {
        standardizedEffect <- -(thetaH1 - stageResults$thetaH0) / assumedStDevs
    }
    nPlanned <- c(rep(NA_real_, stage), nPlanned)
    nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned
    ctr <- .performClosedCombinationTest(stageResults = stageResults)

    for (treatmentArm in 1:gMax) {
        if (!is.na(ctr$separatePValues[treatmentArm, stage])) {
            if (gMax == 1) {
                pValues <- ctr$adjustedStageWisePValues[ctr$indices[, treatmentArm] == 1, ][1:stage]
            } else {
                pValues <- ctr$adjustedStageWisePValues[ctr$indices[, treatmentArm] == 1, ][which.max(
                    ctr$overallAdjustedTestStatistics[ctr$indices[, treatmentArm] == 1, stage]
                ), 1:stage]
            }
            if (stage < kMax - 1) {
                for (k in (stage + 1):kMax) {
                    reject <- 0
                    for (i in 1:iterations) {
                        reject <- reject + .getRejectValueConditionalPowerFisher(
                            kMax = kMax, alpha0Vec = design$alpha0Vec,
                            criticalValues = criticalValues, weightsFisher = weightsFisher,
                            pValues = pValues, currentKMax = k, thetaH1 = standardizedEffect[treatmentArm],
                            stage = stage, nPlanned = nPlanned
                        )
                    }
                    results$conditionalPower[treatmentArm, k] <- reject / iterations
                }
                results$simulated <- TRUE
                results$.setParameterType("simulated", C_PARAM_GENERATED)
            } else if (stage == kMax - 1) {
                divisor <- prod(pValues[1:(kMax - 1)]^weightsFisher[1:(kMax - 1)])
                result <- 1 - (criticalValues[kMax] / divisor)^(1 / weightsFisher[kMax])

                if (result <= 0 || result >= 1) {
                    warning("Calculation not possible: could not calculate ",
                        "conditional power for stage ", kMax,
                        call. = FALSE
                    )
                    results$conditionalPower[treatmentArm, kMax] <- NA_real_
                } else {
                    results$conditionalPower[treatmentArm, kMax] <- 1 - stats::pnorm(.getQNorm(result) -
                        standardizedEffect[treatmentArm] * sqrt(nPlanned[kMax]))
                }
            }
        }
    }
    nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
    results$nPlanned <- nPlanned
    results$.setParameterType("nPlanned", C_PARAM_GENERATED)

    results$.setParameterType("conditionalPower", C_PARAM_GENERATED)

    results$thetaH1 <- thetaH1
    results$assumedStDevs <- assumedStDevs

    if (!results$simulated) {
        results$iterations <- NA_integer_
        results$seed <- NA_real_
        results$.setParameterType("iterations", C_PARAM_NOT_APPLICABLE)
        results$.setParameterType("seed", C_PARAM_NOT_APPLICABLE)
    }

    return(results)
}

#'
#' Calculation of conditional power based on conditional Dunnett test
#'
#' @noRd
#'
.getConditionalPowerMeansMultiArmConditionalDunnett <- function(..., results, stageResults, stage,
        allocationRatioPlanned, nPlanned, thetaH1, assumedStDevs) {
    design <- stageResults$.design
    .assertIsTrialDesignConditionalDunnett(design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerMeansMultiArmConditionalDunnett",
        ignore = c("stage", "intersectionTest", "design", "stDevsH1"), ...
    )

    if (stage > 1) {
        warning("Conditional power is only calculated for the first (interim) stage", call. = FALSE)
    }

    kMax <- 2
    gMax <- stageResults$getGMax()

    nPlanned <- c(rep(NA_real_, stage), nPlanned)
    nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned

    if (length(thetaH1) == 1) {
        thetaH1 <- rep(thetaH1, gMax)
        results$.setParameterType("thetaH1", C_PARAM_GENERATED)
    } else {
        results$.setParameterType("thetaH1", C_PARAM_DEFAULT_VALUE)
    }
    results$.setParameterType("assumedStDevs", C_PARAM_DEFAULT_VALUE)

    if (stageResults$directionUpper) {
        standardizedEffect <- (thetaH1 - stageResults$thetaH0) / assumedStDevs
    } else {
        standardizedEffect <- -(thetaH1 - stageResults$thetaH0) / assumedStDevs
    }
    ctr <- .getClosedConditionalDunnettTestResults(stageResults = stageResults, design = design, stage = stage)

    for (treatmentArm in 1:gMax) {
        if (!is.na(ctr$separatePValues[treatmentArm, stage])) {
            results$conditionalPower[treatmentArm, 2] <- 1 -
                stats::pnorm(.getOneMinusQNorm(min(ctr$conditionalErrorRate[
                    ctr$indices[, treatmentArm] == 1,
                    stage
                ], na.rm = TRUE)) - standardizedEffect[treatmentArm] * sqrt(nPlanned[2]))
        }
    }
    nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
    results$nPlanned <- nPlanned
    results$.setParameterType("nPlanned", C_PARAM_GENERATED)

    results$.setParameterType("conditionalPower", C_PARAM_GENERATED)

    results$thetaH1 <- thetaH1
    results$assumedStDevs <- assumedStDevs
    return(results)
}

#'
#' Calculation of conditional power and likelihood values for plotting the graph
#'
#' @noRd
#'
.getConditionalPowerLikelihoodMeansMultiArm <- function(..., stageResults, stage,
        nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        thetaRange, assumedStDevs = NA_real_,
        iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
    .associatedArgumentsAreDefined(nPlanned = nPlanned, thetaRange = thetaRange)
    .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
    .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM)

    design <- stageResults$.design
    kMax <- design$kMax
    gMax <- stageResults$getGMax()
    intersectionTest <- stageResults$intersectionTest

    assumedStDevs <- .assertIsValidAssumedStDevForMultiHypotheses(assumedStDevs, stageResults, stage)

    if (length(assumedStDevs) == 1) {
        assumedStDevs <- rep(assumedStDevs, gMax)
    }

    thetaRange <- .assertIsValidThetaRange(thetaRange = thetaRange)

    treatmentArms <- numeric(gMax * length(thetaRange))
    effectValues <- numeric(gMax * length(thetaRange))
    condPowerValues <- numeric(gMax * length(thetaRange))
    likelihoodValues <- numeric(gMax * length(thetaRange))

    stdErr <- stageResults$overallStDevs[, stage] *
        sqrt(1 / stageResults$.dataInput$getOverallSampleSizes(stage = stage, group = gMax + 1) +
            1 / stageResults$.dataInput$getOverallSampleSizes(stage = stage, group = (1:gMax)))

    results <- ConditionalPowerResultsMultiArmMeansR6$new(
        .design = design,
        .stageResults = stageResults,
        assumedStDevs = assumedStDevs,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned
    )

    j <- 1
    for (i in seq(along = thetaRange)) {
        for (treatmentArm in 1:gMax) {
            treatmentArms[j] <- treatmentArm
            effectValues[j] <- thetaRange[i]

            if (.isTrialDesignInverseNormal(design)) {
                condPowerValues[j] <- .getConditionalPowerMeansMultiArmInverseNormal(
                    results = results,
                    stageResults = stageResults, stage = stage, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned,
                    thetaH1 = thetaRange[i], assumedStDevs = assumedStDevs
                )$conditionalPower[treatmentArm, kMax]
            } else if (.isTrialDesignFisher(design)) {
                condPowerValues[j] <- .getConditionalPowerMeansMultiArmFisher(
                    results = results,
                    stageResults = stageResults, stage = stage, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned,
                    thetaH1 = thetaRange[i], assumedStDevs = assumedStDevs,
                    iterations = iterations, seed = seed
                )$conditionalPower[treatmentArm, kMax]
            } else if (.isTrialDesignConditionalDunnett(design)) {
                condPowerValues[j] <- .getConditionalPowerMeansMultiArmConditionalDunnett(
                    results = results,
                    stageResults = stageResults, stage = stage, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned,
                    thetaH1 = thetaRange[i], assumedStDevs = assumedStDevs
                )$conditionalPower[treatmentArm, 2]
            }

            likelihoodValues[j] <- stats::dnorm(
                thetaRange[i],
                stageResults$effectSizes[treatmentArm, stage], stdErr[treatmentArm]
            ) /
                stats::dnorm(0, 0, stdErr[treatmentArm])
            j <- j + 1
        }
    }

    subtitle <- paste0(
        "Intersection test = ", intersectionTest,
        ", stage = ", stage, ", # of remaining subjects = ",
        sum(nPlanned), ", sd = ", .formatSubTitleValue(assumedStDevs, "assumedStDevs"),
        ", allocation ratio = ", .formatSubTitleValue(allocationRatioPlanned, "allocationRatioPlanned")
    )

    return(list(
        treatmentArms = treatmentArms,
        xValues = effectValues,
        condPowerValues = condPowerValues,
        likelihoodValues = likelihoodValues,
        main = C_PLOT_MAIN_CONDITIONAL_POWER_WITH_LIKELIHOOD,
        xlab = "Effect size",
        ylab = C_PLOT_YLAB_CONDITIONAL_POWER_WITH_LIKELIHOOD,
        sub = subtitle
    ))
}
