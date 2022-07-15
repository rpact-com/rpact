## |
## |  *Analysis of survival in enrichment designs with adaptive test*
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
## |  File version: $Revision: 6015 $
## |  Last changed: $Date: 2022-04-08 14:23:17 +0200 (Fr, 08 Apr 2022) $
## |  Last changed by: $Author: wassmer $
## |
## |

# @title
# Get Analysis Results Survival
#
# @description
# Returns an analysis result object.
#
# @param design The trial design.
#
# @return Returns a \code{AnalysisResultsSurvival} object.
#
# @keywords internal
#
.calcSurvivalTestStatistics <- function(dataInput, subset, stage, thetaH0,
        stratifiedAnalysis, directionUpper = TRUE) {
    overallEvents <- NA_real_
    testStatistics <- NA_real_
    separatePValues <- NA_real_
    overallAllocationRatios <- NA_real_
    overallTestStatistics <- NA_real_

    if (!all(is.na(dataInput$getOverallEvents(stage = stage, subset = subset)))) {
        overallEvents <- sum(dataInput$getOverallEvents(stage = stage, subset = subset), na.rm = TRUE)

        if (dataInput$isStratified()) {
            overallAllocationRatios <- sum(dataInput$getOverallAllocationRatios(stage = stage, subset = subset) *
                dataInput$getOverallEvents(stage = stage, subset = subset), na.rm = TRUE) /
                sum(dataInput$getOverallEvents(stage = stage, subset = subset), na.rm = TRUE)
            overallTestStatistics <- (sum(dataInput$getOverallEvents(stage = stage, subset = subset), na.rm = TRUE) -
                sum(dataInput$getOverallExpectedEvents(stage = stage, subset = subset), na.rm = TRUE)) /
                sqrt(sum(dataInput$getOverallVarianceEvents(stage = stage, subset = subset), na.rm = TRUE)) -
                sqrt(sum(dataInput$getOverallEvents(stage = stage, subset = subset), na.rm = TRUE)) *
                    sqrt(overallAllocationRatios) / (1 + overallAllocationRatios) * log(thetaH0)

            if (stage == 1) {
                testStatistics <- overallTestStatistics
            } else {
                testStatistics <- (sqrt(sum(dataInput$getOverallEvents(stage = stage, subset = subset), na.rm = TRUE)) *
                    (sum(dataInput$getOverallEvents(stage = stage, subset = subset), na.rm = TRUE) -
                        sum(dataInput$getOverallExpectedEvents(stage = stage, subset = subset), na.rm = TRUE)) /
                    sqrt(sum(dataInput$getOverallVarianceEvents(stage = stage, subset = subset), na.rm = TRUE)) -
                    sqrt(sum(dataInput$getOverallEvents(stage = stage - 1, subset = subset), na.rm = TRUE)) *
                        (sum(dataInput$getOverallEvents(stage = stage - 1, subset = subset) -
                            dataInput$getOverallExpectedEvents(stage = stage - 1, subset = subset), na.rm = TRUE)) /
                        sqrt(sum(dataInput$getOverallVarianceEvents(stage = stage - 1, subset = subset), na.rm = TRUE))) /
                    sqrt(sum(dataInput$getOverallEvents(stage = stage, subset = subset) -
                        dataInput$getOverallEvents(stage = stage - 1, subset = subset), na.rm = TRUE)) -
                    sqrt(sum(dataInput$getOverallEvents(stage = stage, subset = subset) -
                        dataInput$getOverallEvents(stage = stage - 1, subset = subset), na.rm = TRUE)) *
                        sqrt(overallAllocationRatios) / (1 + overallAllocationRatios) * log(thetaH0)
            }
        }

        # non-stratified data input
        else {
            overallTestStatistics <- dataInput$getOverallLogRanks(stage = stage, subset = subset) -
                sqrt(dataInput$getOverallEvents(stage = stage, subset = subset)) *
                    sqrt(dataInput$getOverallAllocationRatios(stage = stage, subset = subset)) /
                    (1 + dataInput$getOverallAllocationRatios(stage = stage, subset = subset)) * log(thetaH0)

            testStatistics <- dataInput$getLogRanks(stage = stage, subset = subset) -
                sqrt(dataInput$getEvents(stage = stage, subset = subset)) *
                    sqrt(dataInput$getAllocationRatios(stage = stage, subset = subset)) /
                    (1 + dataInput$getAllocationRatios(stage = stage, subset = subset)) * log(thetaH0)

            overallAllocationRatios <- dataInput$getOverallAllocationRatios(stage = stage, subset = subset)
        }

        if (directionUpper) {
            separatePValues <- 1 - stats::pnorm(testStatistics)
        } else {
            separatePValues <- stats::pnorm(testStatistics)
        }
    }

    if (("R" %in% subset) && is.na(dataInput$getOverallEvents(stage = stage, subset = "R")) ||
            ("S1" %in% subset) && is.na(dataInput$getOverallEvents(stage = stage, subset = "S1")) ||
            ("S2" %in% subset) && is.na(dataInput$getOverallEvents(stage = stage, subset = "S2")) ||
            ("S3" %in% subset) && is.na(dataInput$getOverallEvents(stage = stage, subset = "S3")) ||
            ("S4" %in% subset) && is.na(dataInput$getOverallEvents(stage = stage, subset = "S4"))
        ) {
        overallEvents <- NA_real_
        separatePValues <- NA_real_
        testStatistics <- NA_real_
        overallAllocationRatios <- NA_real_
        overallTestStatistics <- NA_real_
    }

    return(list(
        overallEvents = overallEvents,
        separatePValues = separatePValues,
        testStatistics = testStatistics,
        overallAllocationRatios = overallAllocationRatios,
        overallTestStatistics = overallTestStatistics
    ))
}

.getStageResultsSurvivalEnrichment <- function(..., design, dataInput,
        thetaH0 = C_THETA_H0_SURVIVAL_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        stratifiedAnalysis = C_STRATIFIED_ANALYSIS_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        calculateSingleStepAdjusted = FALSE,
        userFunctionCallEnabled = FALSE) {
    .assertIsTrialDesign(design)
    .assertIsDatasetSurvival(dataInput)
    .assertIsValidThetaH0DataInput(thetaH0, dataInput)
    .assertIsValidDirectionUpper(directionUpper, design$sided)
    .assertIsSingleLogical(calculateSingleStepAdjusted, "calculateSingleStepAdjusted")
    .warnInCaseOfUnknownArguments(
        functionName = ".getStageResultsSurvivalEnrichment",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    kMax <- design$kMax

    if (dataInput$isStratified()) {
        gMax <- log(length(levels(factor(dataInput$subsets))), 2) + 1
    } else {
        gMax <- length(levels(factor(dataInput$subsets)))
    }

    .assertIsValidIntersectionTestEnrichment(design, intersectionTest)

    if (gMax > 2 && intersectionTest == "SpiessensDebois") {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "gMax (", gMax,
            ") > 2: Spiessens & Debois intersection test test can only be used for one subset"
        )
    }

    if (!stratifiedAnalysis) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "only stratified analysis can be performed for enrichment survival designs"
        )
    }

    if (dataInput$isStratified() && gMax > 4) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "gMax (", gMax,
            ") > 4: Stratified analysis not implemented"
        )
    }

    stageResults <- StageResultsEnrichmentSurvival(
        design = design,
        dataInput = dataInput,
        intersectionTest = intersectionTest,
        thetaH0 = thetaH0,
        direction = ifelse(directionUpper, C_DIRECTION_UPPER, C_DIRECTION_LOWER),
        directionUpper = directionUpper,
        stratifiedAnalysis = stratifiedAnalysis,
        stage = stage
    )

    .setValueAndParameterType(
        stageResults, "stratifiedAnalysis",
        stratifiedAnalysis, C_STRATIFIED_ANALYSIS_DEFAULT
    )
    .setValueAndParameterType(
        stageResults, "intersectionTest",
        intersectionTest, C_INTERSECTION_TEST_ENRICHMENT_DEFAULT
    )

    effectSizes <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    testStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    separatePValues <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallTestStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallEvents <- matrix(NA_real_, nrow = gMax, ncol = kMax)

    dimnames(testStatistics) <- list(paste("population ", 1:gMax, sep = ""), paste("stage ", (1:kMax), sep = ""))
    dimnames(separatePValues) <- list(paste("population ", 1:gMax, sep = ""), paste("stage ", (1:kMax), sep = ""))

    subsets <- .createSubsetsByGMax(gMax = gMax, stratifiedInput = dataInput$isStratified(), subsetIdPrefix = "S")
    for (k in 1:stage) {
        for (population in 1:gMax) {
            subset <- subsets[[population]]
            results <- .calcSurvivalTestStatistics(
                dataInput, subset, k,
                thetaH0, stratifiedAnalysis, directionUpper
            )

            effectSizes[population, k] <- thetaH0 * exp(results$overallTestStatistics *
                (1 + results$overallAllocationRatios) /
                sqrt(results$overallAllocationRatios * results$overallEvents))

            overallTestStatistics[population, k] <- results$overallTestStatistics
            testStatistics[population, k] <- results$testStatistics
            separatePValues[population, k] <- results$separatePValues
            overallEvents[population, k] <- results$overallEvents
        }
    }

    .setWeightsToStageResults(design, stageResults)

    # calculation of single stage adjusted p-Values and overall test statistics for determination of RCIs
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
            for (population in 1:gMax) {
                if ((intersectionTest == "Bonferroni") || (intersectionTest == "Simes")) {
                    singleStepAdjustedPValues[population, k] <- min(1, separatePValues[population, k] * selected)
                } else if (intersectionTest == "Sidak") {
                    singleStepAdjustedPValues[population, k] <- 1 - (1 - separatePValues[population, k])^selected
                } else if (intersectionTest == "SpiessensDebois") {
                    if (!is.na(testStatistics[population, k])) {
                        df <- NA_real_
                        sigma <- 1
                        if (selected == 2) {
                            if (dataInput$isStratified()) {
                                sigma <- matrix(rep(sqrt(dataInput$getEvents(stage = k, subset = "S1") /
                                    sum(dataInput$getEvents(stage = k))), 4), nrow = 2)
                            } else {
                                sigma <- matrix(rep(sqrt(dataInput$getEvents(stage = k, subset = "S1") /
                                    dataInput$getEvents(stage = k, subset = "F")), 4), nrow = 2)
                            }
                            diag(sigma) <- 1
                        }
                        singleStepAdjustedPValues[population, k] <- 1 - .getMultivariateDistribution(
                            type = "normal",
                            upper = ifelse(directionUpper, testStatistics[population, k], -testStatistics[population, k]),
                            sigma = sigma, df = NA
                        )
                    }
                }
                if (.isTrialDesignInverseNormal(design)) {
                    combInverseNormal[population, k] <- (weightsInverseNormal[1:k] %*%
                        .getOneMinusQNorm(singleStepAdjustedPValues[population, 1:k])) /
                        sqrt(sum(weightsInverseNormal[1:k]^2))
                } else if (.isTrialDesignFisher(design)) {
                    combFisher[population, k] <- prod(singleStepAdjustedPValues[population, 1:k]^weightsFisher[1:k])
                }
            }
        }

        stageResults$overallTestStatistics <- overallTestStatistics
        stageResults$effectSizes <- effectSizes
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
        stageResults$.overallEvents <- overallEvents
        stageResults$effectSizes <- effectSizes
        stageResults$testStatistics <- testStatistics
        stageResults$separatePValues <- separatePValues
    }

    return(stageResults)
}

.getAnalysisResultsSurvivalEnrichment <- function(..., design, dataInput) {
    if (.isTrialDesignInverseNormal(design)) {
        return(.getAnalysisResultsSurvivalInverseNormalEnrichment(
            design = design, dataInput = dataInput, ...
        ))
    }

    if (.isTrialDesignFisher(design)) {
        return(.getAnalysisResultsSurvivalFisherEnrichment(
            design = design, dataInput = dataInput, ...
        ))
    }

    .stopWithWrongDesignMessage(design)
}

.getAnalysisResultsSurvivalInverseNormalEnrichment <- function(...,
        design, dataInput,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        stratifiedAnalysis = C_STRATIFIED_ANALYSIS_DEFAULT,
        thetaH0 = C_THETA_H0_SURVIVAL_DEFAULT,
        thetaH1 = NA_real_,
        nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .assertIsTrialDesignInverseNormal(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsSurvivalInverseNormalEnrichment",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsEnrichmentInverseNormal(design = design, dataInput = dataInput)

    results <- .getAnalysisResultsSurvivalEnrichmentAll(
        results = results, design = design, dataInput = dataInput,
        intersectionTest = intersectionTest, stage = stage, directionUpper = directionUpper,
        stratifiedAnalysis = stratifiedAnalysis,
        thetaH0 = thetaH0, thetaH1 = thetaH1, nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        tolerance = tolerance
    )

    return(results)
}

.getAnalysisResultsSurvivalFisherEnrichment <- function(...,
        design, dataInput,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        stratifiedAnalysis = C_STRATIFIED_ANALYSIS_DEFAULT,
        thetaH0 = C_THETA_H0_SURVIVAL_DEFAULT,
        thetaH1 = NA_real_,
        nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
    .assertIsTrialDesignFisher(design)
    .assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)

    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)

    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsSurvivalFisherEnrichment",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsEnrichmentFisher(design = design, dataInput = dataInput)
    results <- .getAnalysisResultsSurvivalEnrichmentAll(
        results = results, design = design, dataInput = dataInput,
        intersectionTest = intersectionTest, stage = stage, directionUpper = directionUpper,
        stratifiedAnalysis = stratifiedAnalysis,
        thetaH0 = thetaH0, thetaH1 = thetaH1, nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        tolerance = tolerance,
        iterations = iterations, seed = seed
    )

    return(results)
}

.getAnalysisResultsSurvivalEnrichmentAll <- function(..., results,
        design, dataInput, intersectionTest, stage,
        directionUpper, stratifiedAnalysis, thetaH0, thetaH1, nPlanned,
        allocationRatioPlanned, tolerance, iterations, seed) {
    startTime <- Sys.time()

    stageResults <- .getStageResultsSurvivalEnrichment(
        design = design, dataInput = dataInput,
        intersectionTest = intersectionTest, stage = stage,
        thetaH0 = thetaH0, directionUpper = directionUpper,
        stratifiedAnalysis = stratifiedAnalysis
    )

    results$.setStageResults(stageResults)
    .logProgress("Stage results calculated", startTime = startTime)

    thetaH1 <- .assertIsValidThetaH1ForEnrichment(thetaH1, stageResults, stage, results = results)

    .setValueAndParameterType(results, "intersectionTest", intersectionTest, C_INTERSECTION_TEST_ENRICHMENT_DEFAULT)
    .setValueAndParameterType(results, "directionUpper", directionUpper, C_DIRECTION_UPPER_DEFAULT)
    .setValueAndParameterType(results, "stratifiedAnalysis", stratifiedAnalysis, C_STRATIFIED_ANALYSIS_DEFAULT)
    .setValueAndParameterType(results, "thetaH0", thetaH0, C_THETA_H0_MEANS_DEFAULT)
    .setConditionalPowerArguments(results, dataInput, nPlanned, allocationRatioPlanned)
    .setNPlannedAndThetaH1(results, nPlanned, thetaH1)

    startTime <- Sys.time()

    results$.closedTestResults <- getClosedCombinationTestResults(stageResults = stageResults)

    .logProgress("Closed test calculated", startTime = startTime)

    if (design$kMax > 1) {

        # conditional power
        startTime <- Sys.time()
        if (.isTrialDesignFisher(design)) {
            conditionalPowerResults <- .getConditionalPowerSurvivalEnrichment(
                stageResults = stageResults,
                stage = stage, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
                thetaH1 = thetaH1, iterations = iterations, seed = seed
            )
            if (conditionalPowerResults$simulated) {
                results$conditionalPowerSimulated <- conditionalPowerResults$conditionalPower
                results$.setParameterType("conditionalPower", C_PARAM_NOT_APPLICABLE)
                results$.setParameterType("conditionalPowerSimulated", C_PARAM_GENERATED)
            } else {
                results$conditionalPower <- conditionalPowerResults$conditionalPower
                results$conditionalPowerSimulated <- matrix(numeric(0))
                results$.setParameterType("conditionalPower", C_PARAM_GENERATED)
                results$.setParameterType("conditionalPowerSimulated", C_PARAM_NOT_APPLICABLE)
            }
        } else {
            conditionalPowerResults <- .getConditionalPowerSurvivalEnrichment(
                stageResults = stageResults,
                stage = stage, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
                thetaH1 = thetaH1
            )
            results$conditionalPower <- conditionalPowerResults$conditionalPower
            results$.setParameterType("conditionalPower", C_PARAM_GENERATED)
        }
        results$thetaH1 <- matrix(conditionalPowerResults$thetaH1, ncol = 1)
        results$.conditionalPowerResults <- conditionalPowerResults
        .logProgress("Conditional power calculated", startTime = startTime)

        # CRP - conditional rejection probabilities
        startTime <- Sys.time()
        results$conditionalRejectionProbabilities <- .getConditionalRejectionProbabilitiesEnrichment(
            stageResults = stageResults, stage = stage, iterations = iterations, seed = seed
        )
        results$.setParameterType("conditionalRejectionProbabilities", C_PARAM_GENERATED)
        .logProgress("Conditional rejection probabilities (CRP) calculated", startTime = startTime)
    } else {
        results$.setParameterType("conditionalPower", C_PARAM_NOT_APPLICABLE)
        results$.setParameterType("conditionalPowerSimulated", C_PARAM_NOT_APPLICABLE)
        results$.setParameterType("conditionalRejectionProbabilities", C_PARAM_NOT_APPLICABLE)
    }

    # RCI - repeated confidence interval
    repeatedConfidenceIntervalLowerBounds <- numeric(0)
    repeatedConfidenceIntervalUpperBounds <- numeric(0)
    startTime <- Sys.time()
    repeatedConfidenceIntervals <- .getRepeatedConfidenceIntervalsSurvivalEnrichment(
        design = design, dataInput = dataInput,
        stratifiedAnalysis = stratifiedAnalysis,
        intersectionTest = intersectionTest,
        stage = stage,
        tolerance = tolerance
    )
    gMax <- stageResults$getGMax()
    results$repeatedConfidenceIntervalLowerBounds <-
        matrix(rep(NA_real_, gMax * design$kMax), nrow = gMax, ncol = design$kMax)
    results$repeatedConfidenceIntervalUpperBounds <- results$repeatedConfidenceIntervalLowerBounds

    for (k in 1:design$kMax) {
        for (population in 1:gMax) {
            results$repeatedConfidenceIntervalLowerBounds[population, k] <-
                repeatedConfidenceIntervals[population, 1, k]
            results$repeatedConfidenceIntervalUpperBounds[population, k] <-
                repeatedConfidenceIntervals[population, 2, k]
        }
    }

    results$.setParameterType("repeatedConfidenceIntervalLowerBounds", C_PARAM_GENERATED)
    results$.setParameterType("repeatedConfidenceIntervalUpperBounds", C_PARAM_GENERATED)

    # repeated p-value
    results$repeatedPValues <- .getRepeatedPValuesEnrichment(stageResults = stageResults, tolerance = tolerance)
    results$.setParameterType("repeatedPValues", C_PARAM_GENERATED)

    message("Test statistics from full (and sub-populations) need to be stratified log-rank tests")

    return(results)
}

.getRootThetaSurvivalEnrichment <- function(..., design, dataInput, treatmentArm, stage,
        directionUpper, stratifiedAnalysis, intersectionTest, thetaLow, thetaUp,
        firstParameterName, secondValue, tolerance) {
    result <- .getOneDimensionalRoot(
        function(theta) {
            stageResults <- .getStageResultsSurvivalEnrichment(
                design = design, dataInput = dataInput,
                stage = stage, thetaH0 = theta, directionUpper = directionUpper,
                stratifiedAnalysis = stratifiedAnalysis,
                intersectionTest = intersectionTest, calculateSingleStepAdjusted = TRUE
            )
            firstValue <- stageResults[[firstParameterName]][treatmentArm, stage]
            return(firstValue - secondValue)
        },
        lower = thetaLow, upper = thetaUp, tolerance = tolerance,
        callingFunctionInformation = ".getRootThetaSurvivalEnrichment"
    )
    return(result)
}

.getUpperLowerThetaSurvivalEnrichment <- function(...,
        design, dataInput, theta, treatmentArm, stage,
        directionUpper, conditionFunction, stratifiedAnalysis,
        intersectionTest, firstParameterName, secondValue) {
    stageResults <- .getStageResultsSurvivalEnrichment(
        design = design, dataInput = dataInput,
        stage = stage, thetaH0 = exp(theta), directionUpper = directionUpper,
        stratifiedAnalysis = stratifiedAnalysis,
        intersectionTest = intersectionTest, calculateSingleStepAdjusted = TRUE
    )

    firstValue <- stageResults[[firstParameterName]][treatmentArm, stage]
    maxSearchIterations <- 30

    while (conditionFunction(secondValue, firstValue)) {
        theta <- 2 * theta
        stageResults <- .getStageResultsSurvivalEnrichment(
            design = design, dataInput = dataInput,
            stage = stage, thetaH0 = exp(theta), directionUpper = directionUpper,
            stratifiedAnalysis = stratifiedAnalysis,
            intersectionTest = intersectionTest, calculateSingleStepAdjusted = TRUE
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

.getRepeatedConfidenceIntervalsSurvivalEnrichmentAll <- function(...,
        design, dataInput,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        stratifiedAnalysis = C_STRATIFIED_ANALYSIS_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        firstParameterName) {
    .assertIsValidIntersectionTestEnrichment(design, intersectionTest)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)

    stageResults <- .getStageResultsSurvivalEnrichment(
        design = design, dataInput = dataInput,
        stage = stage, thetaH0 = 1, directionUpper = directionUpper,
        stratifiedAnalysis = stratifiedAnalysis,
        intersectionTest = intersectionTest, calculateSingleStepAdjusted = FALSE
    )

    gMax <- stageResults$getGMax()
    repeatedConfidenceIntervals <- array(NA_real_, dim = c(gMax, 2, design$kMax))

    # Repeated onfidence intervals when using combination tests
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

    if (any(is.na(criticalValues[1:stage]))) {
        warning("Repeated confidence intervals not because ", sum(is.na(criticalValues)),
            " critical values are NA (", .arrayToString(criticalValues), ")",
            call. = FALSE
        )
        return(repeatedConfidenceIntervals)
    }

    # necessary for adjustment for binding futility boundaries
    futilityCorr <- rep(NA_real_, design$kMax)

    stages <- (1:stage)
    for (k in stages) {
        startTime <- Sys.time()
        for (population in 1:gMax) {
			if (!is.na(stageResults$testStatistics[population, k]) && criticalValues[k] < C_QNORM_MAXIMUM) {
				
                # Finding maximum upper and minimum lower bounds for RCIs
                thetaLow <- exp(.getUpperLowerThetaSurvivalEnrichment(
                    design = design, dataInput = dataInput,
                    theta = -1, treatmentArm = population, stage = k, directionUpper = TRUE,
                    stratifiedAnalysis = stratifiedAnalysis, intersectionTest = intersectionTest,
                    conditionFunction = conditionFunction, firstParameterName = firstParameterName,
                    secondValue = criticalValues[k]
                ))

                thetaUp <- exp(.getUpperLowerThetaSurvivalEnrichment(
                    design = design, dataInput = dataInput,
                    theta = 1, treatmentArm = population, stage = k, directionUpper = FALSE,
                    stratifiedAnalysis = stratifiedAnalysis, intersectionTest = intersectionTest,
                    conditionFunction = conditionFunction, firstParameterName = firstParameterName,
                    secondValue = criticalValues[k]
                ))

                # finding upper and lower RCI limits through root function
                repeatedConfidenceIntervals[population, 1, k] <- .getRootThetaSurvivalEnrichment(
                    design = design,
                    dataInput = dataInput, treatmentArm = population, stage = k, directionUpper = TRUE,
                    thetaLow = thetaLow, thetaUp = thetaUp, stratifiedAnalysis = stratifiedAnalysis,
                    intersectionTest = intersectionTest, firstParameterName = firstParameterName,
                    secondValue = criticalValues[k], tolerance = tolerance
                )

                repeatedConfidenceIntervals[population, 2, k] <- .getRootThetaSurvivalEnrichment(
                    design = design,
                    dataInput = dataInput, treatmentArm = population, stage = k, directionUpper = FALSE,
                    thetaLow = thetaLow, thetaUp = thetaUp, stratifiedAnalysis = stratifiedAnalysis,
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
                        thetaLow <- tolerance
                    } else {
                        thetaUp <- .getUpperLowerThetaSurvivalEnrichment(
                            design = design,
                            dataInput = dataInput,
                            theta = 1, treatmentArm = population, stage = k - 1, directionUpper = FALSE,
                            conditionFunction = conditionFunction, stratifiedAnalysis = stratifiedAnalysis,
                            intersectionTest = intersectionTest, firstParameterName = parameterName,
                            secondValue = bounds[k - 1]
                        )
                    }

                    futilityCorr[k] <- .getRootThetaSurvivalEnrichment(
                        design = design, dataInput = dataInput,
                        treatmentArm = population, stage = k - 1, directionUpper = directionUpper,
                        thetaLow = thetaLow, thetaUp = thetaUp, stratifiedAnalysis = stratifiedAnalysis,
                        intersectionTest = intersectionTest, firstParameterName = parameterName,
                        secondValue = bounds[k - 1], tolerance = tolerance
                    )

                    if (directionUpper) {
                        repeatedConfidenceIntervals[population, 1, k] <- min(
                            min(futilityCorr[2:k]),
                            repeatedConfidenceIntervals[population, 1, k]
                        )
                    } else {
                        repeatedConfidenceIntervals[population, 2, k] <- max(
                            max(futilityCorr[2:k]),
                            repeatedConfidenceIntervals[population, 2, k]
                        )
                    }
                }

                if (!is.na(repeatedConfidenceIntervals[population, 1, k]) &&
                        !is.na(repeatedConfidenceIntervals[population, 2, k]) &&
                        repeatedConfidenceIntervals[population, 1, k] > repeatedConfidenceIntervals[population, 2, k]) {
                    repeatedConfidenceIntervals[population, , k] <- rep(NA_real_, 2)
                }
            }
        }
        .logProgress("Repeated confidence intervals for stage %s calculated", startTime = startTime, k)
    }

    return(repeatedConfidenceIntervals)
}

#
# RCIs based on inverse normal combination test
#
.getRepeatedConfidenceIntervalsSurvivalEnrichmentInverseNormal <- function(...,
        design, dataInput,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        stratifiedAnalysis = C_STRATIFIED_ANALYSIS_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsSurvivalEnrichmentInverseNormal",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsSurvivalEnrichmentAll(
        design = design, dataInput = dataInput,
        directionUpper = directionUpper,
        stratifiedAnalysis = stratifiedAnalysis, intersectionTest = intersectionTest,
        tolerance = tolerance, firstParameterName = "combInverseNormal", ...
    ))
}

#
# RCIs based on Fisher's combination test
#
.getRepeatedConfidenceIntervalsSurvivalEnrichmentFisher <- function(...,
        design, dataInput,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        stratifiedAnalysis = C_STRATIFIED_ANALYSIS_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsSurvivalEnrichmentFisher",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsSurvivalEnrichmentAll(
        design = design, dataInput = dataInput,
        directionUpper = directionUpper,
        stratifiedAnalysis = stratifiedAnalysis, intersectionTest = intersectionTest,
        tolerance = tolerance, firstParameterName = "combFisher", ...
    ))
}

#
#  Calculation of lower and upper limits of repeated confidence intervals (RCIs) for Survival
#
.getRepeatedConfidenceIntervalsSurvivalEnrichment <- function(..., design) {
    if (.isTrialDesignInverseNormal(design)) {
        return(.getRepeatedConfidenceIntervalsSurvivalEnrichmentInverseNormal(design = design, ...))
    }

    if (.isTrialDesignFisher(design)) {
        return(.getRepeatedConfidenceIntervalsSurvivalEnrichmentFisher(design = design, ...))
    }

    .stopWithWrongDesignMessage(design)
}

#
#  Calculation of conditional power for Survival
#
.getConditionalPowerSurvivalEnrichment <- function(..., stageResults, stage = stageResults$stage,
        nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        thetaH1 = NA_real_,
        iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
    design <- stageResults$.design
    gMax <- stageResults$getGMax()
    kMax <- design$kMax

    results <- ConditionalPowerResultsEnrichmentSurvival(
        .design = design,
        .stageResults = stageResults,
        thetaH1 = thetaH1,
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
    results$.setParameterType("nPlanned", C_PARAM_USER_DEFINED)
    results$.setParameterType(
        "allocationRatioPlanned",
        ifelse(allocationRatioPlanned == C_ALLOCATION_RATIO_DEFAULT, C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED)
    )

    thetaH1 <- .assertIsValidThetaH1ForEnrichment(thetaH1, stageResults, stage, results = results)

    if (any(thetaH1 <= 0, na.rm = TRUE)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'thetaH1' (", thetaH1, ") must be > 0")
    }
    if ((length(thetaH1) != 1) && (length(thetaH1) != gMax)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            sprintf(paste0(
                "length of 'thetaH1' (%s) must be ",
                "equal to 'gMax' (%s) or 1"
            ), .arrayToString(thetaH1), gMax)
        )
    }

    if (.isTrialDesignInverseNormal(design)) {
        return(.getConditionalPowerSurvivalEnrichmentInverseNormal(
            results = results,
            design = design, stageResults = stageResults, stage = stage,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
            thetaH1 = thetaH1, ...
        ))
    } else if (.isTrialDesignFisher(design)) {
        return(.getConditionalPowerSurvivalEnrichmentFisher(
            results = results,
            design = design, stageResults = stageResults, stage = stage,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
            thetaH1 = thetaH1,
            iterations = iterations, seed = seed, ...
        ))
    }

    stop(
        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
        "'design' must be an instance of TrialDesignInverseNormal or TrialDesignFisher"
    )
}

#
# Calculation of conditional power based on inverse normal method
#
.getConditionalPowerSurvivalEnrichmentInverseNormal <- function(..., results, design, stageResults, stage,
        allocationRatioPlanned, nPlanned, thetaH1) {
    .assertIsTrialDesignInverseNormal(design)
    .warnInCaseOfUnknownArguments(functionName = ".getConditionalPowerSurvivalEnrichmentInverseNormal", ...)

    kMax <- design$kMax
    gMax <- stageResults$getGMax()
    weights <- .getWeightsInverseNormal(design)
    informationRates <- design$informationRates
    nPlanned <- c(rep(NA_real_, stage), nPlanned)
    nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned

    .setValueAndParameterType(results, "allocationRatioPlanned", allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT)
    if (length(thetaH1) == 1) {
        thetaH1 <- rep(thetaH1, gMax)
        results$.setParameterType("thetaH1", C_PARAM_GENERATED)
    } else {
        results$.setParameterType("thetaH1", C_PARAM_DEFAULT_VALUE)
    }

    if (stageResults$directionUpper) {
        standardizedEffect <- log(thetaH1 / stageResults$thetaH0)
    } else {
        standardizedEffect <- -log(thetaH1 / stageResults$thetaH0)
    }
    ctr <- .performClosedCombinationTest(stageResults = stageResults)
    criticalValues <- design$criticalValues

    for (population in 1:gMax) {
        if (!is.na(ctr$separatePValues[population, stage])) {
            # shifted decision region for use in getGroupSeqProbs
            # Inverse Normal Method
            shiftedDecisionRegionUpper <- criticalValues[(stage + 1):kMax] *
                sqrt(sum(weights[1:stage]^2) + cumsum(weights[(stage + 1):kMax]^2)) /
                sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
                min(ctr$overallAdjustedTestStatistics[ctr$indices[, population] == 1, stage], na.rm = TRUE) *
                    sqrt(sum(weights[1:stage]^2)) /
                    sqrt(cumsum(weights[(stage + 1):kMax]^2)) - standardizedEffect[population] *
                    cumsum(sqrt(nPlanned[(stage + 1):kMax]) * weights[(stage + 1):kMax]) /
                    sqrt(cumsum(weights[(stage + 1):kMax]^2))
            if (stage == kMax - 1) {
                shiftedFutilityBounds <- c()
            } else {
                shiftedFutilityBounds <- design$futilityBounds[(stage + 1):(kMax - 1)] *
                    sqrt(sum(weights[1:stage]^2) + cumsum(weights[(stage + 1):(kMax - 1)]^2)) /
                    sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2)) -
                    min(ctr$overallAdjustedTestStatistics[ctr$indices[, population] == 1, stage], na.rm = TRUE) *
                        sqrt(sum(weights[1:stage]^2)) /
                        sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2)) - standardizedEffect[population] *
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

            results$conditionalPower[population, (stage + 1):kMax] <- cumsum(probs[3, ] - probs[2, ])
        }
    }

    nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
    results$nPlanned <- nPlanned
    results$.setParameterType("nPlanned", C_PARAM_GENERATED)

    results$.setParameterType("conditionalPower", C_PARAM_GENERATED)

    results$thetaH1 <- thetaH1
    return(results)
}

#
# Calculation of conditional power based on Fisher's combination test
#
.getConditionalPowerSurvivalEnrichmentFisher <- function(..., results, design, stageResults, stage,
        allocationRatioPlanned, nPlanned, thetaH1, iterations, seed) {
    .assertIsTrialDesignFisher(design)
    .assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
    .warnInCaseOfUnknownArguments(functionName = ".getConditionalPowerSurvivalEnrichmentFisher", ...)
    kMax <- design$kMax
    gMax <- stageResults$getGMax()
    criticalValues <- design$criticalValues
    weightsFisher <- .getWeightsFisher(design)

    results$iterations <- as.integer(iterations)
    results$.setParameterType("iterations", C_PARAM_USER_DEFINED)
    results$.setParameterType("seed", ifelse(is.na(seed), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
    results$seed <- .setSeed(seed)
    results$simulated <- FALSE
    results$.setParameterType("simulated", C_PARAM_DEFAULT_VALUE)

    .setValueAndParameterType(results, "allocationRatioPlanned", allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT)
    if (length(thetaH1) == 1) {
        thetaH1 <- rep(thetaH1, gMax)
        results$.setParameterType("thetaH1", C_PARAM_GENERATED)
    } else {
        results$.setParameterType("thetaH1", C_PARAM_DEFAULT_VALUE)
    }

    if (stageResults$directionUpper) {
        standardizedEffect <- log(thetaH1 / stageResults$thetaH0)
    } else {
        standardizedEffect <- -log(thetaH1 / stageResults$thetaH0)
    }
    nPlanned <- c(rep(NA_real_, stage), nPlanned)
    nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned
    ctr <- .performClosedCombinationTest(stageResults = stageResults)
    for (population in 1:gMax) {
        if (!is.na(ctr$separatePValues[population, stage])) {
            if (gMax == 1) {
                pValues <- ctr$adjustedStageWisePValues[ctr$indices[, population] == 1, ][1:stage]
            } else {
                pValues <- ctr$adjustedStageWisePValues[ctr$indices[, population] == 1, ][which.max(
                    ctr$overallAdjustedTestStatistics[ctr$indices[, population] == 1, stage]
                ), 1:stage]
            }
            if (stage < kMax - 1) {
                for (k in (stage + 1):kMax) {
                    reject <- 0
                    for (i in 1:iterations) {
                        reject <- reject + .getRejectValueConditionalPowerFisher(
                            kMax = kMax, alpha0Vec = design$alpha0Vec,
                            criticalValues = criticalValues, weightsFisher = weightsFisher,
                            pValues = pValues, currentKMax = k, thetaH1 = standardizedEffect[population],
                            stage = stage, nPlanned = nPlanned
                        )
                    }
                    results$conditionalPower[population, k] <- reject / iterations
                }
                results$simulated <- TRUE
                results$.setParameterType("simulated", C_PARAM_GENERATED)
            } else if (stage == kMax - 1) {
                divisor <- prod(pValues[1:(kMax - 1)]^weightsFisher[1:(kMax - 1)])
                result <- 1 - (criticalValues[kMax] / divisor)^(1 / weightsFisher[kMax])

                if (result <= 0 || result >= 1) {
                    warning("Calculation not possible: could not calculate conditional power for stage ", kMax, call. = FALSE)
                    results$conditionalPower[population, kMax] <- NA_real_
                } else {
                    results$conditionalPower[population, kMax] <- 1 - stats::pnorm(.getQNorm(result) -
                        standardizedEffect[population] * sqrt(nPlanned[kMax]))
                }
            }
        }
    }
    nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
    results$nPlanned <- nPlanned
    results$.setParameterType("nPlanned", C_PARAM_GENERATED)

    results$.setParameterType("conditionalPower", C_PARAM_GENERATED)

    results$thetaH1 <- thetaH1
    return(results)
}

#
# Calculation of conditional power and likelihood values for plotting the graph
#
.getConditionalPowerLikelihoodSurvivalEnrichment <- function(..., stageResults, stage,
        nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        thetaRange, iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
    .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
    .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM)
    .associatedArgumentsAreDefined(nPlanned = nPlanned, thetaRange = thetaRange)

    design <- stageResults$.design
    kMax <- design$kMax
    gMax <- stageResults$getGMax()
    intersectionTest <- stageResults$intersectionTest

    thetaRange <- .assertIsValidThetaH1ForEnrichment(thetaH1 = thetaRange)

    if (length(thetaRange) == 1) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "length of 'thetaRange' (", .arrayToString(thetaRange), ") must be at least 2"
        )
    }

    populations <- numeric(gMax * length(thetaRange))
    effectValues <- numeric(gMax * length(thetaRange))
    condPowerValues <- numeric(gMax * length(thetaRange))
    likelihoodValues <- numeric(gMax * length(thetaRange))

    stdErr <- 2 / sqrt(stageResults$.overallEvents[, stage])

    results <- ConditionalPowerResultsEnrichmentSurvival(
        .design = design,
        .stageResults = stageResults,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned
    )

    j <- 1
    for (i in seq(along = thetaRange)) {
        for (population in (1:gMax)) {
            populations[j] <- population
            effectValues[j] <- thetaRange[i]

            if (.isTrialDesignInverseNormal(design)) {
                condPowerValues[j] <- .getConditionalPowerSurvivalEnrichmentInverseNormal(
                    results = results,
                    design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned,
                    thetaH1 = thetaRange[i], ...
                )$conditionalPower[population, kMax]
            } else if (.isTrialDesignFisher(design)) {
                condPowerValues[j] <- .getConditionalPowerSurvivalEnrichmentFisher(
                    results = results,
                    design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned,
                    thetaH1 = thetaRange[i],
                    iterations = iterations, seed = seed, ...
                )$conditionalPower[population, kMax]
            }
            likelihoodValues[j] <- stats::dnorm(
                log(thetaRange[i]), log(stageResults$effectSizes[population, stage]),
                stdErr[population]
            ) / stats::dnorm(0, 0, stdErr[population])
            j <- j + 1
        }
    }

    subtitle <- paste0(
        "Intersection test = ", intersectionTest,
        ", Stage = ", stage, ", # of remaining events = ", sum(nPlanned),
        ", allocation ratio = ", .formatSubTitleValue(allocationRatioPlanned, "allocationRatioPlanned")
    )

    return(list(
        populations = populations,
        xValues = effectValues,
        condPowerValues = condPowerValues,
        likelihoodValues = likelihoodValues,
        main = C_PLOT_MAIN_CONDITIONAL_POWER_WITH_LIKELIHOOD,
        xlab = "Hazard ratio",
        ylab = C_PLOT_YLAB_CONDITIONAL_POWER_WITH_LIKELIHOOD,
        sub = subtitle
    ))
}
