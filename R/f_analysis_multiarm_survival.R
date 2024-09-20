## |
## |  *Analysis of survival in multi-arm designs with adaptive test*
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
## |  File version: $Revision: 8225 $
## |  Last changed: $Date: 2024-09-18 09:38:40 +0200 (Mi, 18 Sep 2024) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_logger.R
NULL

#' @title
#' Get Analysis Results Survival
#'
#' @description
#' Returns an analysis result object.
#'
#' @param design The trial design.
#'
#' @return Returns a \code{AnalysisResultsSurvival} object.
#'
#' @keywords internal
#'
#' @noRd
#'
.getAnalysisResultsSurvivalMultiArm <- function(..., design, dataInput) {
    if (.isTrialDesignInverseNormal(design)) {
        return(.getAnalysisResultsSurvivalInverseNormalMultiArm(
            design = design,
            dataInput = dataInput,
            ...
        ))
    }

    if (.isTrialDesignFisher(design)) {
        return(.getAnalysisResultsSurvivalFisherMultiArm(
            design = design,
            dataInput = dataInput,
            ...
        ))
    }

    if (.isTrialDesignConditionalDunnett(design)) {
        return(.getAnalysisResultsSurvivalConditionalDunnettMultiArm(
            design = design,
            dataInput = dataInput,
            ...
        ))
    }

    .stopWithWrongDesignMessage(design)
}

.getAnalysisResultsSurvivalInverseNormalMultiArm <- function(...,
        design, dataInput,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        directionUpper = NA,
        thetaH0 = C_THETA_H0_SURVIVAL_DEFAULT,
        thetaH1 = NA_real_,
        nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .assertIsTrialDesignInverseNormal(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsSurvivalInverseNormalMultiArm",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsMultiArmInverseNormal$new(design = design, dataInput = dataInput)

    results <- .getAnalysisResultsSurvivalMultiArmAll(
        results = results,
        design = design,
        dataInput = dataInput,
        intersectionTest = intersectionTest,
        stage = stage,
        directionUpper = directionUpper,
        thetaH0 = thetaH0,
        thetaH1 = thetaH1,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        tolerance = tolerance
    )

    return(results)
}

.getAnalysisResultsSurvivalFisherMultiArm <- function(...,
        design,
        dataInput,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        directionUpper = NA,
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
        functionName = ".getAnalysisResultsSurvivalFisherMultiArm",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsMultiArmFisher$new(design = design, dataInput = dataInput)
    results <- .getAnalysisResultsSurvivalMultiArmAll(
        results = results,
        design = design,
        dataInput = dataInput,
        intersectionTest = intersectionTest,
        stage = stage,
        directionUpper = directionUpper,
        thetaH0 = thetaH0,
        thetaH1 = thetaH1,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        tolerance = tolerance,
        iterations = iterations,
        seed = seed
    )

    return(results)
}

.getAnalysisResultsSurvivalConditionalDunnettMultiArm <- function(...,
        design,
        dataInput,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        directionUpper = NA,
        thetaH0 = C_THETA_H0_SURVIVAL_DEFAULT,
        thetaH1 = NA_real_,
        nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        iterations = C_ITERATIONS_DEFAULT,
        seed = NA_real_) {
    .assertIsTrialDesignConditionalDunnett(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsSurvivalConditionalDunnettMultiArm",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsConditionalDunnett$new(design = design, dataInput = dataInput)

    results <- .getAnalysisResultsSurvivalMultiArmAll(
        results = results,
        design = design,
        dataInput = dataInput,
        intersectionTest = intersectionTest,
        stage = stage,
        directionUpper = directionUpper,
        thetaH0 = thetaH0,
        thetaH1 = thetaH1,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        tolerance = tolerance,
        iterations = iterations,
        seed = seed
    )

    return(results)
}

.getAnalysisResultsSurvivalMultiArmAll <- function(...,
        results,
        design,
        dataInput,
        intersectionTest,
        stage,
        directionUpper,
        thetaH0,
        thetaH1,
        nPlanned,
        allocationRatioPlanned,
        tolerance,
        iterations,
        seed) {
    startTime <- Sys.time()

    intersectionTest <- .getCorrectedIntersectionTestMultiArmIfNecessary(design, intersectionTest)

    stageResults <- .getStageResultsSurvivalMultiArm(
        design = design, dataInput = dataInput,
        intersectionTest = intersectionTest, stage = stage,
        thetaH0 = thetaH0, directionUpper = directionUpper
    )
    results$.setStageResults(stageResults)
    .logProgress("Stage results calculated", startTime = startTime)
    gMax <- stageResults$getGMax()

    thetaH1 <- .assertIsValidThetaH1ForMultiArm(thetaH1, stageResults, stage, results = results)

    .setValueAndParameterType(results, "intersectionTest", intersectionTest, C_INTERSECTION_TEST_MULTIARMED_DEFAULT)
    .setValueAndParameterType(results, "directionUpper", directionUpper, C_DIRECTION_UPPER_DEFAULT)
    .setValueAndParameterType(results, "thetaH0", thetaH0, C_THETA_H0_MEANS_DEFAULT)
    .setConditionalPowerArguments(results, dataInput, nPlanned, allocationRatioPlanned)
    .setNPlannedAndThetaH1(results, nPlanned, thetaH1)

    startTime <- Sys.time()
    if (!.isTrialDesignConditionalDunnett(design)) {
        results$.closedTestResults <- getClosedCombinationTestResults(stageResults = stageResults)
    } else {
        results$.closedTestResults <- getClosedConditionalDunnettTestResults(
            stageResults = stageResults, design = design, stage = stage
        )
    }
    .logProgress("Closed test calculated", startTime = startTime)

    if (design$kMax > 1) {
        # conditional power
        startTime <- Sys.time()
        if (.isTrialDesignFisher(design)) {
            results$.conditionalPowerResults <- .getConditionalPowerSurvivalMultiArm(
                stageResults = stageResults,
                stage = stage, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
                thetaH1 = thetaH1, iterations = iterations, seed = seed
            )
            .synchronizeIterationsAndSeed(results)
        } else {
            results$.conditionalPowerResults <- .getConditionalPowerSurvivalMultiArm(
                stageResults = stageResults,
                stage = stage,
                nPlanned = nPlanned,
                allocationRatioPlanned = allocationRatioPlanned,
                thetaH1 = thetaH1
            )
            results$conditionalPower <- results$.conditionalPowerResults$conditionalPower
            results$.setParameterType("conditionalPower", C_PARAM_GENERATED)
        }
        results$thetaH1 <- matrix(results$.conditionalPowerResults$thetaH1, ncol = 1)
        .logProgress("Conditional power calculated", startTime = startTime)

        # CRP - conditional rejection probabilities
        startTime <- Sys.time()
        results$conditionalRejectionProbabilities <- .getConditionalRejectionProbabilitiesMultiArm(
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
    repeatedConfidenceIntervals <- .getRepeatedConfidenceIntervalsSurvivalMultiArm(
        design = design,
        dataInput = dataInput,
        intersectionTest = intersectionTest,
        stage = stage,
        directionUpper = directionUpper,
        tolerance = tolerance
    )
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
    results$repeatedPValues <- .getRepeatedPValuesMultiArm(stageResults = stageResults, tolerance = tolerance)
    results$.setParameterType("repeatedPValues", C_PARAM_GENERATED)

    return(results)
}

.getStageResultsSurvivalMultiArm <- function(...,
        design,
        dataInput,
        thetaH0 = C_THETA_H0_SURVIVAL_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        calculateSingleStepAdjusted = FALSE,
        userFunctionCallEnabled = FALSE) {
    .assertIsTrialDesign(design)
    .assertIsDatasetSurvival(dataInput)
    .assertIsValidThetaH0DataInput(thetaH0, dataInput)
    .assertIsSingleLogical(calculateSingleStepAdjusted, "calculateSingleStepAdjusted")
    .warnInCaseOfUnknownArguments(
        functionName = ".getStageResultsSurvivalMultiArm",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    gMax <- dataInput$getNumberOfGroups() - 1
    kMax <- design$kMax

    intersectionTest <- .getCorrectedIntersectionTestMultiArmIfNecessary(
        design, intersectionTest, userFunctionCallEnabled
    )
    .assertIsValidIntersectionTestMultiArm(design, intersectionTest)

    stageResults <- StageResultsMultiArmSurvival$new(
        design = design,
        dataInput = dataInput,
        intersectionTest = intersectionTest,
        thetaH0 = thetaH0,
        direction = ifelse(directionUpper, C_DIRECTION_UPPER, C_DIRECTION_LOWER),
        directionUpper = directionUpper,
        stage = stage
    )

    effectSizes <- matrix(NA_real_, nrow = gMax, ncol = kMax)
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
        for (treatmentArm in 1:gMax) {
            effectSizes[treatmentArm, k] <- exp(dataInput$getOverallLogRanks(stage = k, group = treatmentArm) *
                (1 + dataInput$getOverallAllocationRatios(stage = k, group = treatmentArm)) /
                sqrt(dataInput$getOverallAllocationRatios(stage = k, group = treatmentArm) *
                    dataInput$getOverallEvents(stage = k, group = treatmentArm)))

            testStatistics[treatmentArm, k] <- dataInput$getLogRanks(stage = k, group = treatmentArm) -
                sqrt(dataInput$getEvents(stage = k, group = treatmentArm)) *
                    sqrt(dataInput$getAllocationRatios(stage = k, group = treatmentArm)) /
                    (1 + dataInput$getAllocationRatios(stage = k, group = treatmentArm)) * log(thetaH0)

            overallTestStatistics[treatmentArm, k] <- dataInput$getOverallLogRanks(stage = k, group = treatmentArm) -
                sqrt(dataInput$getOverallEvents(stage = k, group = treatmentArm)) *
                    sqrt(dataInput$getOverallAllocationRatios(stage = k, group = treatmentArm)) /
                    (1 + dataInput$getOverallAllocationRatios(stage = k, group = treatmentArm)) * log(thetaH0)

            separatePValues[treatmentArm, k] <- .applyDirectionOfAlternative(
                stats::pnorm(testStatistics[treatmentArm, k]), directionUpper,
                type = "oneMinusValue", phase = "analysis"
            )
            overallPValues[treatmentArm, k] <- .applyDirectionOfAlternative(
                stats::pnorm(overallTestStatistics[treatmentArm, k]), directionUpper,
                type = "oneMinusValue", phase = "analysis"
            )
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
            allocationRatiosSelected <- as.numeric(na.omit(
                dataInput$getAllocationRatios(stage = k, group = (1:gMax))
            ))
            sigma <- sqrt(allocationRatiosSelected / (1 + allocationRatiosSelected)) %*%
                sqrt(t(allocationRatiosSelected / (1 + allocationRatiosSelected)))
            diag(sigma) <- 1
            for (treatmentArm in 1:gMax) {
                if ((intersectionTest == "Bonferroni") || (intersectionTest == "Simes")) {
                    if (.isTrialDesignGroupSequential(design)) {
                        overallPValues[treatmentArm, k] <- min(1, overallPValues[treatmentArm, k] * selected)
                    } else {
                        singleStepAdjustedPValues[treatmentArm, k] <- min(1, separatePValues[treatmentArm, k] * selected)
                    }
                } else if (intersectionTest == "Sidak") {
                    if (.isTrialDesignGroupSequential(design)) {
                        overallPValues[treatmentArm, k] <- 1 - (1 - overallPValues[treatmentArm, k])^selected
                    } else {
                        singleStepAdjustedPValues[treatmentArm, k] <- 1 - (1 - separatePValues[treatmentArm, k])^selected
                    }
                } else if (intersectionTest == "Dunnett") {
                    if (!is.na(testStatistics[treatmentArm, k])) {
                        df <- NA_real_
                        singleStepAdjustedPValues[treatmentArm, k] <- 1 - .getMultivariateDistribution(
                            type = "normal",
                            upper = ifelse(directionUpper, testStatistics[treatmentArm, k], -testStatistics[treatmentArm, k]),
                            sigma = sigma, df = df
                        )
                    }
                }
                if (.isTrialDesignInverseNormal(design)) {
                    combInverseNormal[treatmentArm, k] <- (weightsInverseNormal[1:k] %*%
                        .getOneMinusQNorm(singleStepAdjustedPValues[treatmentArm, 1:k])) /
                        sqrt(sum(weightsInverseNormal[1:k]^2))
                } else if (.isTrialDesignFisher(design)) {
                    combFisher[treatmentArm, k] <- prod(singleStepAdjustedPValues[treatmentArm, 1:k]^weightsFisher[1:k])
                }
            }
        }

        stageResults$overallTestStatistics <- overallTestStatistics
        stageResults$overallPValues <- overallPValues
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
        stageResults$overallPValues <- overallPValues
        stageResults$effectSizes <- effectSizes
        stageResults$testStatistics <- testStatistics
        stageResults$separatePValues <- separatePValues
    }

    return(stageResults)
}

.getRootThetaSurvivalMultiArm <- function(...,
        design,
        dataInput,
        treatmentArm,
        stage,
        directionUpper,
        intersectionTest,
        thetaLow,
        thetaUp,
        firstParameterName,
        secondValue,
        tolerance) {
    result <- .getOneDimensionalRoot(
        function(theta) {
            stageResults <- .getStageResultsSurvivalMultiArm(
                design = design,
                dataInput = dataInput,
                stage = stage,
                thetaH0 = theta,
                directionUpper = directionUpper,
                intersectionTest = intersectionTest,
                calculateSingleStepAdjusted = TRUE
            )
            firstValue <- stageResults[[firstParameterName]][treatmentArm, stage]
            return(firstValue - secondValue)
        },
        lower = thetaLow, upper = thetaUp, tolerance = tolerance,
        callingFunctionInformation = ".getRootThetaSurvivalMultiArm"
    )
    return(result)
}

.getUpperLowerThetaSurvivalMultiArm <- function(...,
        design,
        dataInput,
        theta,
        treatmentArm,
        stage,
        directionUpper,
        conditionFunction,
        intersectionTest,
        firstParameterName,
        secondValue) {
    stageResults <- .getStageResultsSurvivalMultiArm(
        design = design,
        dataInput = dataInput,
        stage = stage,
        thetaH0 = exp(theta),
        directionUpper = directionUpper,
        intersectionTest = intersectionTest,
        calculateSingleStepAdjusted = TRUE
    )

    firstValue <- stageResults[[firstParameterName]][treatmentArm, stage]
    maxSearchIterations <- 30

    while (conditionFunction(secondValue, firstValue)) {
        theta <- 2 * theta
        stageResults <- .getStageResultsSurvivalMultiArm(
            design = design, dataInput = dataInput,
            stage = stage, thetaH0 = exp(theta), directionUpper = directionUpper,
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

.getRepeatedConfidenceIntervalsSurvivalMultiArmAll <- function(...,
        design,
        dataInput,
        directionUpper = NA,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        firstParameterName) {
    .assertIsValidIntersectionTestMultiArm(design, intersectionTest)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)

    stageResults <- .getStageResultsSurvivalMultiArm(
        design = design,
        dataInput = dataInput,
        stage = stage,
        thetaH0 = 1,
        directionUpper = directionUpper,
        intersectionTest = intersectionTest,
        calculateSingleStepAdjusted = FALSE
    )

    gMax <- dataInput$getNumberOfGroups() - 1
    repeatedConfidenceIntervals <- array(NA_real_, dim = c(gMax, 2, design$kMax))

    # Confidence interval for second stage when using conditional Dunnett test
    if (.isTrialDesignConditionalDunnett(design)) {
        startTime <- Sys.time()
        for (treatmentArm in 1:gMax) {
            if (!is.na(stageResults$testStatistics[treatmentArm, 2])) {
                iteration <- 30
                thetaUpLimit <- 1
                repeat{
                    stageResults <- .getStageResultsSurvivalMultiArm(
                        design = design,
                        dataInput = dataInput,
                        stage = stage,
                        thetaH0 = thetaUpLimit,
                        directionUpper = FALSE,
                        intersectionTest = intersectionTest,
                        calculateSingleStepAdjusted = FALSE
                    )
                    rejected <- .getConditionalDunnettTestForCI(
                        design = design, stageResults = stageResults, treatmentArm = treatmentArm
                    )
                    iteration <- iteration - 1
                    if (rejected || iteration == 0) break
                    thetaUpLimit <- 2 * thetaUpLimit
                }

                thetaLow <- 0
                thetaUp <- thetaUpLimit

                iteration <- 30
                prec <- 1
                while (prec > tolerance) {
                    theta <- (thetaLow + thetaUp) / 2
                    stageResults <- .getStageResultsSurvivalMultiArm(
                        design = design,
                        dataInput = dataInput,
                        stage = stage,
                        thetaH0 = theta,
                        directionUpper = TRUE,
                        intersectionTest = intersectionTest,
                        calculateSingleStepAdjusted = FALSE
                    )
                    conditionalDunnettSingleStepRejected <- .getConditionalDunnettTestForCI(
                        design = design,
                        stageResults = stageResults,
                        treatmentArm = treatmentArm
                    )
                    ifelse(conditionalDunnettSingleStepRejected, thetaLow <- theta, thetaUp <- theta)
                    ifelse(iteration > 0, prec <- thetaUp - thetaLow, prec <- 0)
                    iteration <- iteration - 1
                }
                repeatedConfidenceIntervals[treatmentArm, 1, 2] <- theta

                thetaLow <- 0
                thetaUp <- thetaUpLimit

                iteration <- 30
                prec <- 1
                while (prec > tolerance) {
                    theta <- (thetaLow + thetaUp) / 2
                    stageResults <- .getStageResultsSurvivalMultiArm(
                        design = design,
                        dataInput = dataInput,
                        stage = stage,
                        thetaH0 = theta,
                        directionUpper = FALSE,
                        intersectionTest = intersectionTest,
                        calculateSingleStepAdjusted = FALSE
                    )
                    conditionalDunnettSingleStepRejected <- .getConditionalDunnettTestForCI(
                        design = design,
                        stageResults = stageResults,
                        treatmentArm = treatmentArm
                    )
                    ifelse(conditionalDunnettSingleStepRejected, thetaUp <- theta, thetaLow <- theta)
                    ifelse(iteration > 0, prec <- thetaUp - thetaLow, prec <- 0)
                    iteration <- iteration - 1
                }
                repeatedConfidenceIntervals[treatmentArm, 2, 2] <- theta

                if (!is.na(repeatedConfidenceIntervals[treatmentArm, 1, 2]) &&
                        !is.na(repeatedConfidenceIntervals[treatmentArm, 2, 2]) &&
                        repeatedConfidenceIntervals[treatmentArm, 1, 2] > repeatedConfidenceIntervals[treatmentArm, 2, 2]) {
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
            criticalValues <- .getCriticalValues(design)
            conditionFunction <- .isFirstValueSmallerThanSecondValue
        } else if (.isTrialDesignInverseNormal(design)) {
            bounds <- design$futilityBounds
            border <- C_FUTILITY_BOUNDS_DEFAULT
            criticalValues <- .getCriticalValues(design)
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
            for (treatmentArm in 1:gMax) {
                if (!is.na(stageResults$testStatistics[treatmentArm, k]) && criticalValues[k] < C_QNORM_MAXIMUM) {
                    # Finding maximum upper and minimum lower bounds for RCIs
                    thetaLow <- exp(.getUpperLowerThetaSurvivalMultiArm(
                        design = design,
                        dataInput = dataInput,
                        theta = -1,
                        treatmentArm = treatmentArm,
                        stage = k,
                        directionUpper = TRUE,
                        intersectionTest = intersectionTest,
                        conditionFunction = conditionFunction,
                        firstParameterName = firstParameterName,
                        secondValue = criticalValues[k]
                    ))

                    thetaUp <- exp(.getUpperLowerThetaSurvivalMultiArm(
                        design = design,
                        dataInput = dataInput,
                        theta = 1,
                        treatmentArm = treatmentArm,
                        stage = k,
                        directionUpper = FALSE,
                        intersectionTest = intersectionTest,
                        conditionFunction = conditionFunction,
                        firstParameterName = firstParameterName,
                        secondValue = criticalValues[k]
                    ))

                    # finding upper and lower RCI limits through root function
                    repeatedConfidenceIntervals[treatmentArm, 1, k] <- .getRootThetaSurvivalMultiArm(
                        design = design,
                        dataInput = dataInput,
                        treatmentArm = treatmentArm,
                        stage = k,
                        directionUpper = TRUE,
                        thetaLow = thetaLow,
                        thetaUp = thetaUp,
                        intersectionTest = intersectionTest,
                        firstParameterName = firstParameterName,
                        secondValue = criticalValues[k],
                        tolerance = tolerance
                    )

                    repeatedConfidenceIntervals[treatmentArm, 2, k] <- .getRootThetaSurvivalMultiArm(
                        design = design,
                        dataInput = dataInput,
                        treatmentArm = treatmentArm,
                        stage = k,
                        directionUpper = FALSE,
                        thetaLow = thetaLow,
                        thetaUp = thetaUp,
                        intersectionTest = intersectionTest,
                        firstParameterName = firstParameterName,
                        secondValue = criticalValues[k],
                        tolerance = tolerance
                    )

                    # adjustment for binding futility bounds
                    if (k > 1 && !is.na(bounds[k - 1]) && conditionFunction(bounds[k - 1], border) && design$bindingFutility) {
                        parameterName <- ifelse(.isTrialDesignFisher(design),
                            "singleStepAdjustedPValues", firstParameterName
                        )

                        #  Calculate new lower and upper bounds
                        if (is.na(directionUpper) || isTRUE(directionUpper)) {
                            thetaLow <- tolerance
                        } else {
                            thetaUp <- .getUpperLowerThetaSurvivalMultiArm(
                                design = design,
                                dataInput = dataInput,
                                theta = 1,
                                treatmentArm = treatmentArm,
                                stage = k - 1,
                                directionUpper = FALSE,
                                conditionFunction = conditionFunction,
                                intersectionTest = intersectionTest,
                                firstParameterName = parameterName,
                                secondValue = bounds[k - 1]
                            )
                        }

                        futilityCorr[k] <- .getRootThetaSurvivalMultiArm(
                            design = design,
                            dataInput = dataInput,
                            treatmentArm = treatmentArm,
                            stage = k - 1,
                            directionUpper = directionUpper,
                            thetaLow = thetaLow,
                            thetaUp = thetaUp,
                            intersectionTest = intersectionTest,
                            firstParameterName = parameterName,
                            secondValue = bounds[k - 1],
                            tolerance = tolerance
                        )

                        if (is.na(directionUpper) || isTRUE(directionUpper)) {
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
                            repeatedConfidenceIntervals[treatmentArm, 1, k] > repeatedConfidenceIntervals[treatmentArm, 2, k]) {
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
.getRepeatedConfidenceIntervalsSurvivalMultiArmInverseNormal <- function(...,
        design,
        dataInput,
        directionUpper = NA,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsSurvivalMultiArmInverseNormal",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsSurvivalMultiArmAll(
        design = design,
        dataInput = dataInput,
        directionUpper = directionUpper,
        intersectionTest = intersectionTest,
        tolerance = tolerance,
        firstParameterName = "combInverseNormal",
        ...
    ))
}

#'
#' RCIs based on Fisher's combination test
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsSurvivalMultiArmFisher <- function(...,
        design,
        dataInput,
        directionUpper = NA,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsSurvivalMultiArmFisher",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsSurvivalMultiArmAll(
        design = design,
        dataInput = dataInput,
        directionUpper = directionUpper,
        intersectionTest = intersectionTest,
        tolerance = tolerance,
        firstParameterName = "combFisher",
        ...
    ))
}

#'
#' CIs based on conditional Dunnett test
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsSurvivalMultiArmConditionalDunnett <- function(...,
        design,
        dataInput,
        directionUpper = NA,
        intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsSurvivalMultiArmConditionalDunnett",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsSurvivalMultiArmAll(
        design = design,
        dataInput = dataInput,
        directionUpper = directionUpper,
        intersectionTest = intersectionTest,
        tolerance = tolerance,
        firstParameterName = "condDunnett",
        ...
    ))
}

#'
#' Calculation of lower and upper limits of repeated confidence intervals (RCIs) for Survival
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsSurvivalMultiArm <- function(..., design) {
    if (.isTrialDesignInverseNormal(design)) {
        return(.getRepeatedConfidenceIntervalsSurvivalMultiArmInverseNormal(design = design, ...))
    }
    if (.isTrialDesignFisher(design)) {
        return(.getRepeatedConfidenceIntervalsSurvivalMultiArmFisher(design = design, ...))
    }
    if (.isTrialDesignConditionalDunnett(design)) {
        return(.getRepeatedConfidenceIntervalsSurvivalMultiArmConditionalDunnett(design = design, ...))
    }
    .stopWithWrongDesignMessage(design)
}

#'
#' Calculation of conditional power for Survival
#'
#' @noRd
#'
.getConditionalPowerSurvivalMultiArm <- function(..., stageResults, stage = stageResults$stage,
        nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        thetaH1 = NA_real_,
        iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
    design <- stageResults$.design
    gMax <- stageResults$getGMax()
    kMax <- design$kMax

    results <- ConditionalPowerResultsMultiArmSurvival$new(
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
    .setValueAndParameterType(results, "allocationRatioPlanned", allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT)
    thetaH1 <- .assertIsValidThetaH1ForMultiArm(thetaH1, stageResults, stage, results = results)
    results$.setParameterType("nPlanned", C_PARAM_USER_DEFINED)

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
        return(.getConditionalPowerSurvivalMultiArmInverseNormal(
            results = results,
            design = design,
            stageResults = stageResults,
            stage = stage,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            thetaH1 = thetaH1,
            ...
        ))
    } else if (.isTrialDesignFisher(design)) {
        return(.getConditionalPowerSurvivalMultiArmFisher(
            results = results,
            design = design,
            stageResults = stageResults,
            stage = stage,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            thetaH1 = thetaH1,
            iterations = iterations,
            seed = seed,
            ...
        ))
    } else if (.isTrialDesignConditionalDunnett(design)) {
        return(.getConditionalPowerSurvivalMultiArmConditionalDunnett(
            results = results,
            design = design,
            stageResults = stageResults,
            stage = stage,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            thetaH1 = thetaH1,
            ...
        ))
    }

    stop(
        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
        "'design' must be an instance of TrialDesignInverseNormal, TrialDesignFisher, ",
        "or TrialDesignConditionalDunnett"
    )
}

#'
#' Calculation of conditional power based on inverse normal method
#'
#' @noRd
#'
.getConditionalPowerSurvivalMultiArmInverseNormal <- function(...,
        results,
        design,
        stageResults,
        stage,
        allocationRatioPlanned,
        nPlanned,
        thetaH1) {
    .assertIsTrialDesignInverseNormal(design)
    .warnInCaseOfUnknownArguments(functionName = ".getConditionalPowerSurvivalMultiArmInverseNormal", ...)

    kMax <- design$kMax
    gMax <- stageResults$getGMax()
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

    if (stageResults$directionUpper) {
        standardizedEffect <- log(thetaH1 / stageResults$thetaH0)
    } else {
        standardizedEffect <- -log(thetaH1 / stageResults$thetaH0)
    }
    ctr <- .performClosedCombinationTest(stageResults = stageResults)
    criticalValues <- .getCriticalValues(design)

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
    return(results)
}

#'
#' Calculation of conditional power based on Fisher's combination test
#'
#' @noRd
#'
.getConditionalPowerSurvivalMultiArmFisher <- function(...,
        results,
        design,
        stageResults,
        stage,
        allocationRatioPlanned,
        nPlanned,
        thetaH1,
        iterations,
        seed) {
    .assertIsTrialDesignFisher(design)
    .assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
    .warnInCaseOfUnknownArguments(functionName = ".getConditionalPowerSurvivalMultiArmFisher", ...)
    kMax <- design$kMax
    gMax <- stageResults$getGMax()
    criticalValues <- .getCriticalValues(design)
    weightsFisher <- .getWeightsFisher(design)

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

    if (stageResults$directionUpper) {
        standardizedEffect <- log(thetaH1 / stageResults$thetaH0)
    } else {
        standardizedEffect <- -log(thetaH1 / stageResults$thetaH0)
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
                    warning("Calculation not possible: ",
                        "could not calculate conditional power for stage ", kMax,
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
.getConditionalPowerSurvivalMultiArmConditionalDunnett <- function(...,
        results,
        design,
        stageResults,
        stage,
        allocationRatioPlanned,
        nPlanned,
        thetaH1) {
    .assertIsTrialDesignConditionalDunnett(design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerSurvivalMultiArmConditionalDunnett",
        ignore = c("intersectionTest"), ...
    )

    if (stage > 1) {
        warning("Conditional power is only calculated for the first (interim) stage", call. = FALSE)
    }

    gMax <- stageResults$getGMax()
    nPlanned <- c(rep(NA_real_, stage), nPlanned)
    nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned

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
    return(results)
}

#'
#' Calculation of conditional power and likelihood values for plotting the graph
#'
#' @noRd
#'
.getConditionalPowerLikelihoodSurvivalMultiArm <- function(...,
        stageResults,
        stage,
        nPlanned,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        thetaRange,
        iterations = C_ITERATIONS_DEFAULT,
        seed = NA_real_) {
    .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
    .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM)
    .associatedArgumentsAreDefined(nPlanned = nPlanned, thetaRange = thetaRange)

    design <- stageResults$.design
    kMax <- design$kMax
    gMax <- stageResults$getGMax()
    intersectionTest <- stageResults$intersectionTest

    thetaRange <- .assertIsValidThetaH1ForMultiArm(thetaH1 = thetaRange)

    if (length(thetaRange) == 1) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "length of 'thetaRange' (", .arrayToString(thetaRange), ") must be at least 2"
        )
    }

    treatmentArms <- numeric(gMax * length(thetaRange))
    effectValues <- numeric(gMax * length(thetaRange))
    condPowerValues <- numeric(gMax * length(thetaRange))
    likelihoodValues <- numeric(gMax * length(thetaRange))

    stdErr <- 2 / sqrt(stageResults$.dataInput$getOverallEvents(stage = stage, group = (1:gMax)))

    results <- ConditionalPowerResultsMultiArmSurvival$new(
        .design = design,
        .stageResults = stageResults,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned
    )

    j <- 1
    for (i in seq(along = thetaRange)) {
        for (treatmentArm in (1:gMax)) {
            treatmentArms[j] <- treatmentArm
            effectValues[j] <- thetaRange[i]

            if (.isTrialDesignInverseNormal(design)) {
                condPowerValues[j] <- .getConditionalPowerSurvivalMultiArmInverseNormal(
                    results = results,
                    design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned,
                    thetaH1 = thetaRange[i], ...
                )$conditionalPower[treatmentArm, kMax]
            } else if (.isTrialDesignFisher(design)) {
                condPowerValues[j] <- .getConditionalPowerSurvivalMultiArmFisher(
                    results = results,
                    design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned,
                    thetaH1 = thetaRange[i],
                    iterations = iterations, seed = seed, ...
                )$conditionalPower[treatmentArm, kMax]
            } else if (.isTrialDesignConditionalDunnett(design)) {
                condPowerValues[j] <- .getConditionalPowerSurvivalMultiArmConditionalDunnett(
                    results = results,
                    design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned,
                    thetaH1 = thetaRange[i], ...
                )$conditionalPower[treatmentArm, 2]
            }
            likelihoodValues[j] <- stats::dnorm(
                log(thetaRange[i]), log(stageResults$effectSizes[treatmentArm, stage]),
                stdErr[treatmentArm]
            ) / stats::dnorm(0, 0, stdErr[treatmentArm])
            j <- j + 1
        }
    }

    subtitle <- paste0(
        "Intersection test = ", intersectionTest,
        ", Stage = ", stage, ", # of remaining events = ", sum(nPlanned),
        ", allocation ratio = ", .formatSubTitleValue(allocationRatioPlanned, "allocationRatioPlanned")
    )

    return(list(
        treatmentArms = treatmentArms,
        xValues = effectValues,
        condPowerValues = condPowerValues,
        likelihoodValues = likelihoodValues,
        main = C_PLOT_MAIN_CONDITIONAL_POWER_WITH_LIKELIHOOD,
        xlab = "Hazard ratio",
        ylab = C_PLOT_YLAB_CONDITIONAL_POWER_WITH_LIKELIHOOD,
        sub = subtitle
    ))
}
