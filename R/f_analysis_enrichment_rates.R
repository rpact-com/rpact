## |
## |  *Analysis of rates in enrichment designs with adaptive test*
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

#' @include f_logger.R
NULL

.calcRatesTestStatistics <- function(dataInput,
        subset,
        stage,
        thetaH0,
        stratifiedAnalysis,
        normalApproximation,
        directionUpper) {
    n <- rep(NA_real_, 2)
    on <- rep(NA_real_, 2)
    e <- rep(NA_real_, 2)
    oe <- rep(NA_real_, 2)
    testStatistics <- NA_real_
    separatePValues <- NA_real_

    if (!all(is.na(dataInput$getSampleSizes(stage = stage, subset = subset)))) {
        for (i in 1:2) {
            # calculation of sample size and events for overall data
            on[i] <- sum(dataInput$getOverallSampleSizes(
                stage = stage, subset = subset, group = i
            ), na.rm = TRUE)
            oe[i] <- sum(dataInput$getOverallEvents(
                stage = stage, subset = subset, group = i
            ), na.rm = TRUE)
        }

        if (stratifiedAnalysis) {
            actEv <- dataInput$getEvents(stage = stage, subset = subset, group = 1)
            ctrEv <- dataInput$getEvents(stage = stage, subset = subset, group = 2)
            actN <- dataInput$getSampleSize(stage = stage, subset = subset, group = 1)
            ctrN <- dataInput$getSampleSize(stage = stage, subset = subset, group = 2)
            weights <- actN * ctrN / (actN + ctrN)

            if (thetaH0 == 0) {
                if (sum(actEv + ctrEv, na.rm = TRUE) == 0 ||
                        sum(actEv + ctrEv, na.rm = TRUE) == sum(actN + ctrN, na.rm = TRUE)) {
                    testStatistics <- 0
                } else {
                    rateH0 <- (actEv + ctrEv) / (actN + ctrN)
                    testStatistics <- sum((actEv / actN - ctrEv / ctrN - thetaH0) * weights, na.rm = TRUE) /
                        sqrt(sum(rateH0 * (1 - rateH0) * weights, na.rm = TRUE))
                }
            } else {
                actMl <- rep(NA_real_, length(subset))
                ctrMl <- rep(NA_real_, length(subset))
                for (population in seq_len(length(subset))) {
                    y <- .getFarringtonManningValues(
                        rate1 = actEv[population] / actN[population],
                        rate2 = ctrEv[population] / ctrN[population], theta = thetaH0,
                        allocation = actN[population] / ctrN[population], method = "diff"
                    )
                    actMl[population] <- y$ml1
                    ctrMl[population] <- y$ml2
                }
                testStatistics <- sum((actEv / actN - ctrEv / ctrN - thetaH0) * weights, na.rm = TRUE) /
                    sqrt(sum((actMl * (1 - actMl) / actN + ctrMl * (1 - ctrMl) / ctrN) * weights^2, na.rm = TRUE))
            }

            separatePValues <- .applyDirectionOfAlternative(stats::pnorm(testStatistics),
                directionUpper,
                type = "oneMinusValue", phase = "analysis"
            )
        }

        # non-stratified analysis
        else {
            for (i in 1:2) {
                n[i] <- sum(dataInput$getSampleSizes(
                    stage = stage,
                    subset = subset, group = i
                ), na.rm = TRUE)
                e[i] <- sum(dataInput$getEvents(
                    stage = stage,
                    subset = subset, group = i
                ), na.rm = TRUE)
            }

            if (normalApproximation) {
                if (thetaH0 == 0) {
                    if (!is.na(e[1])) {
                        if ((e[1] + e[2] == 0) ||
                                (e[1] + e[2] == n[1] + n[2])) {
                            testStatistics <- 0
                        } else {
                            rateH0 <- (e[1] + e[2]) / (n[1] + n[2])
                            testStatistics <- (e[1] / n[1] - e[2] / n[2] - thetaH0) /
                                sqrt(rateH0 * (1 - rateH0) * (1 / n[1] + 1 / n[2]))
                        }
                    } else {
                        testStatistics <- NA_real_
                    }
                } else {
                    y <- .getFarringtonManningValues(
                        rate1 = e[1] / n[1],
                        rate2 = e[2] / n[2], theta = thetaH0, allocation = n[1] / n[2], method = "diff"
                    )
                    testStatistics <- (e[1] / n[1] - e[2] / n[2] - thetaH0) /
                        sqrt(y$ml1 * (1 - y$ml1) / n[1] + y$ml2 * (1 - y$ml2) / n[2])
                }

                separatePValues <- .applyDirectionOfAlternative(stats::pnorm(testStatistics),
                    directionUpper,
                    type = "oneMinusValue", phase = "analysis"
                )
            } else {
                if (thetaH0 != 0) {
                    stop(
                        C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                        "'thetaH0' (", thetaH0, ") must be 0 to perform Fisher's exact test"
                    )
                }

                separatePValues <- stats::phyper(
                    .applyDirectionOfAlternative(e[1], directionUpper, type = "valueMinusOne"),
                    e[1] + e[2],
                    n[1] + n[2] - e[1] - e[2],
                    n[1],
                    lower.tail = .applyDirectionOfAlternative(FALSE,
                        directionUpper,
                        type = "negateIfLower", phase = "analysis"
                    )
                )

                testStatistics <- .applyDirectionOfAlternative(
                    .getOneMinusQNorm(separatePValues),
                    directionUpper,
                    type = "negateIfLower", phase = "analysis"
                )
            }
        }
    }

    if ("R" %in% subset && is.na(dataInput$getSampleSizes(stage = stage, subset = "R", group = 1)) ||
            ("S1" %in% subset) && is.na(dataInput$getSampleSizes(stage = stage, subset = "S1", group = 1)) ||
            ("S2" %in% subset) && is.na(dataInput$getSampleSizes(stage = stage, subset = "S2", group = 1)) ||
            ("S3" %in% subset) && is.na(dataInput$getSampleSizes(stage = stage, subset = "S3", group = 1)) ||
            ("S4" %in% subset) && is.na(dataInput$getSampleSizes(stage = stage, subset = "S4", group = 1))
        ) {
        n <- rep(NA_real_, 2)
        e <- rep(NA_real_, 2)
        on <- rep(NA_real_, 2)
        oe <- rep(NA_real_, 2)
        separatePValues <- NA_real_
        testStatistics <- NA_real_
    }

    return(list(
        populationNs = n,
        populationEvents = e,
        overallRates1 = oe[1] / on[1],
        overallSampleSizes1 = on[1],
        overallRates2 = oe[2] / on[2],
        overallSampleSizes2 = on[2],
        separatePValues = separatePValues,
        testStatistics = testStatistics
    ))
}

.getStageResultsRatesEnrichment <- function(...,
        design,
        dataInput,
        thetaH0 = NA_real_,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        stratifiedAnalysis = C_STRATIFIED_ANALYSIS_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        calculateSingleStepAdjusted = FALSE,
        userFunctionCallEnabled = FALSE) {
    .assertIsTrialDesign(design)
    .assertIsDatasetRates(dataInput)
    .assertIsValidThetaH0DataInput(thetaH0, dataInput)
    thetaH0 <- .getDefaultThetaH0(dataInput, thetaH0)
    .assertIsSingleLogical(normalApproximation, "normalApproximation")
    .assertIsValidIntersectionTestEnrichment(design, intersectionTest)
    .warnInCaseOfUnknownArguments(
        functionName = ".getStageResultsRatesEnrichment",
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

    if ((gMax > 2) && intersectionTest == "SpiessensDebois") {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "gMax (", gMax,
            ") > 2: Spiessens & Debois intersection test test can only be used for one subset"
        )
    }

    if (intersectionTest == "SpiessensDebois" && !normalApproximation) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "Spiessens & Debois test cannot be used with Fisher's ",
            "exact test (normalApproximation = FALSE)",
            call. = FALSE
        )
    }

    if (stratifiedAnalysis && !normalApproximation) {
        stop(
            C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
            "stratified version is not available for Fisher's exact test"
        )
    }

    if (stratifiedAnalysis && !dataInput$isStratified()) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "stratified analysis is only possible for stratified data input"
        )
    }

    if (dataInput$isStratified() && (gMax > 4)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "gMax (", gMax,
            ") > 4: stratified analysis not implemented"
        )
    }

    stageResults <- StageResultsEnrichmentRates$new(
        design = design,
        dataInput = dataInput,
        thetaH0 = thetaH0,
        direction = ifelse(!isFALSE(directionUpper), C_DIRECTION_UPPER, C_DIRECTION_LOWER),
        normalApproximation = normalApproximation,
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

    overallSampleSizes1 <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallSampleSizes2 <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallRates1 <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallRates2 <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallEvents <- rep(NA_real_, kMax)
    testStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallTestStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    separatePValues <- matrix(NA_real_, nrow = gMax, ncol = kMax)

    dimnames(testStatistics) <- list(paste("population ", 1:gMax, sep = ""), paste("stage ", (1:kMax), sep = ""))
    dimnames(separatePValues) <- list(paste("population ", 1:gMax, sep = ""), paste("stage ", (1:kMax), sep = ""))

    subsets <- .createSubsetsByGMax(gMax = gMax, stratifiedInput = dataInput$isStratified(), subsetIdPrefix = "S")
    for (k in 1:stage) {
        for (population in (1:gMax)) {
            subset <- subsets[[population]]
            results <- .calcRatesTestStatistics(
                dataInput, subset, k, thetaH0,
                stratifiedAnalysis, normalApproximation, directionUpper
            )
            testStatistics[population, k] <- results$testStatistics
            separatePValues[population, k] <- results$separatePValues
            overallSampleSizes1[population, k] <- results$overallSampleSizes1
            overallSampleSizes2[population, k] <- results$overallSampleSizes2
            overallRates1[population, k] <- results$overallRates1
            overallRates2[population, k] <- results$overallRates2
        }
    }

    stageResults$overallTestStatistics <- overallTestStatistics
    stageResults$overallPisTreatment <- overallRates1
    stageResults$overallPisControl <- overallRates2
    stageResults$.overallSampleSizes1 <- overallSampleSizes1
    stageResults$.overallSampleSizes2 <- overallSampleSizes2
    stageResults$testStatistics <- testStatistics
    stageResults$separatePValues <- separatePValues

    stageResults$effectSizes <- overallRates1 - overallRates2
    stageResults$.setParameterType("effectSizes", C_PARAM_GENERATED)

    .setWeightsToStageResults(design, stageResults)

    if (!calculateSingleStepAdjusted) {
        return(stageResults)
    }

    # Calculation of single stage adjusted p-Values and overall test statistics
    # for determination of RCIs
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
                            sigma <- matrix(rep(sqrt(sum(dataInput$getSampleSizes(stage = k, subset = "S1")) /
                                sum(dataInput$getSampleSizes(stage = k))), 4), nrow = 2)
                        } else {
                            sigma <- matrix(rep(sqrt(sum(dataInput$getSampleSizes(stage = k, subset = "S1")) /
                                sum(dataInput$getSampleSizes(stage = k, subset = "F"))), 4), nrow = 2)
                        }
                        diag(sigma) <- 1
                    }
                    singleStepAdjustedPValues[population, k] <- 1 - .getMultivariateDistribution(
                        type = "normal",
                        upper = ifelse(!isFALSE(directionUpper),
                            testStatistics[population, k],
                            -testStatistics[population, k]
                        ),
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

    stageResults$singleStepAdjustedPValues <- singleStepAdjustedPValues
    stageResults$.setParameterType("singleStepAdjustedPValues", C_PARAM_GENERATED)

    if (.isTrialDesignFisher(design)) {
        stageResults$combFisher <- combFisher
        stageResults$.setParameterType("combFisher", C_PARAM_GENERATED)
    } else if (.isTrialDesignInverseNormal(design)) {
        stageResults$combInverseNormal <- combInverseNormal
        stageResults$.setParameterType("combInverseNormal", C_PARAM_GENERATED)
    }

    return(stageResults)
}


.getAnalysisResultsRatesEnrichment <- function(..., design, dataInput) {
    if (.isTrialDesignInverseNormal(design)) {
        return(.getAnalysisResultsRatesInverseNormalEnrichment(design = design, dataInput = dataInput, ...))
    }

    if (.isTrialDesignFisher(design)) {
        return(.getAnalysisResultsRatesFisherEnrichment(design = design, dataInput = dataInput, ...))
    }

    .stopWithWrongDesignMessageEnrichment(design)
}

.getAnalysisResultsRatesInverseNormalEnrichment <- function(...,
        design,
        dataInput,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        directionUpper = NA,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        stratifiedAnalysis = C_STRATIFIED_ANALYSIS_DEFAULT,
        thetaH0 = C_THETA_H0_RATES_DEFAULT,
        piTreatments = NA_real_,
        piControls = NA_real_,
        nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .assertIsTrialDesignInverseNormal(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsRatesInverseNormalEnrichment",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsEnrichmentInverseNormal$new(design = design, dataInput = dataInput)

    results <- .getAnalysisResultsRatesEnrichmentAll(
        results = results,
        design = design,
        dataInput = dataInput,
        intersectionTest = intersectionTest,
        stage = stage,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        stratifiedAnalysis = stratifiedAnalysis,
        thetaH0 = thetaH0,
        piTreatments = piTreatments,
        piControls = piControls,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        tolerance = tolerance
    )

    return(results)
}

.getAnalysisResultsRatesFisherEnrichment <- function(...,
        design,
        dataInput,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        directionUpper = NA,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        stratifiedAnalysis = C_STRATIFIED_ANALYSIS_DEFAULT,
        thetaH0 = C_THETA_H0_RATES_DEFAULT,
        piTreatments = NA_real_,
        piControls = NA_real_,
        nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        iterations = C_ITERATIONS_DEFAULT,
        seed = NA_real_) {
    .assertIsTrialDesignFisher(design)
    .assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsRatesFisherEnrichment",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsEnrichmentFisher$new(design = design, dataInput = dataInput)
    .setValueAndParameterType(results, "iterations", as.integer(iterations), C_ITERATIONS_DEFAULT)
    .setValueAndParameterType(results, "seed", seed, NA_real_)

    results <- .getAnalysisResultsRatesEnrichmentAll(
        results = results,
        design = design,
        dataInput = dataInput,
        intersectionTest = intersectionTest,
        stage = stage,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        stratifiedAnalysis = stratifiedAnalysis,
        thetaH0 = thetaH0,
        piTreatments = piTreatments,
        piControls = piControls,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        tolerance = tolerance,
        iterations = iterations,
        seed = seed
    )

    return(results)
}

.getAnalysisResultsRatesEnrichmentAll <- function(...,
        results,
        design,
        dataInput,
        intersectionTest,
        stage,
        directionUpper,
        normalApproximation,
        stratifiedAnalysis,
        thetaH0,
        piTreatments,
        piControls,
        nPlanned,
        allocationRatioPlanned,
        tolerance,
        iterations,
        seed) {
    startTime <- Sys.time()

    stageResults <- .getStageResultsRatesEnrichment(
        design = design,
        dataInput = dataInput,
        intersectionTest = intersectionTest,
        stage = stage,
        thetaH0 = thetaH0,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        stratifiedAnalysis = stratifiedAnalysis
    )

    results$.setStageResults(stageResults)
    .logProgress("Stage results calculated", startTime = startTime)

    gMax <- stageResults$getGMax()

    piControls <- .assertIsValidPiControlForEnrichment(
        piControls, stageResults, stage,
        results = results
    )
    piTreatments <- .assertIsValidPiTreatmentsForEnrichment(
        piTreatments, stageResults, stage,
        results = results
    )

    .setValueAndParameterType(
        results, "intersectionTest",
        intersectionTest, C_INTERSECTION_TEST_ENRICHMENT_DEFAULT
    )
    .setValueAndParameterType(
        results, "directionUpper",
        directionUpper, C_DIRECTION_UPPER_DEFAULT
    )
    .setValueAndParameterType(
        results, "normalApproximation",
        normalApproximation, C_NORMAL_APPROXIMATION_RATES_DEFAULT
    )
    .setValueAndParameterType(
        results, "stratifiedAnalysis",
        stratifiedAnalysis, C_STRATIFIED_ANALYSIS_DEFAULT
    )
    .setValueAndParameterType(results, "thetaH0", thetaH0, C_THETA_H0_RATES_DEFAULT)
    .setConditionalPowerArguments(results, dataInput, nPlanned, allocationRatioPlanned)
    .setNPlannedAndPi(results, nPlanned, "piControls", piControls, piTreatments)

    if (results$.getParameterType("piControls") %in% c(C_PARAM_TYPE_UNKNOWN, C_PARAM_NOT_APPLICABLE)) {
        .setValueAndParameterType(
            results, "piControls",
            matrix(piControls, ncol = 1), matrix(rep(NA_real_, gMax), ncol = 1)
        )
    } else {
        results$piControls <- matrix(piControls, ncol = 1)
    }
    if (results$.getParameterType("piTreatments") %in% c(C_PARAM_TYPE_UNKNOWN, C_PARAM_NOT_APPLICABLE)) {
        .setValueAndParameterType(
            results, "piTreatments",
            matrix(piTreatments, ncol = 1),
            matrix(rep(NA_real_, gMax), ncol = 1)
        )
    } else {
        if (is.matrix(piTreatments)) {
            results$piTreatments <- piTreatments
        } else {
            results$piTreatments <- matrix(piTreatments, ncol = 1)
        }
    }

    startTime <- Sys.time()

    results$.closedTestResults <- getClosedCombinationTestResults(stageResults = stageResults)

    .logProgress("Closed test calculated", startTime = startTime)

    if (design$kMax > 1) {
        # conditional power
        startTime <- Sys.time()
        if (.isTrialDesignFisher(design)) {
            results$.conditionalPowerResults <- .getConditionalPowerRatesEnrichment(
                stageResults = stageResults,
                stage = stage, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
                piTreatments = piTreatments, piControls = piControls, iterations = iterations, seed = seed
            )
            .synchronizeIterationsAndSeed(results)
        } else {
            results$.conditionalPowerResults <- .getConditionalPowerRatesEnrichment(
                stageResults = stageResults,
                stage = stage, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
                piTreatments = piTreatments, piControls = piControls
            )
            results$conditionalPower <- results$.conditionalPowerResults$conditionalPower
            results$.setParameterType("conditionalPower", C_PARAM_GENERATED)
        }
        .logProgress("Conditional power calculated", startTime = startTime)

        # CRP - conditional rejection probabilities
        startTime <- Sys.time()
        results$conditionalRejectionProbabilities <- .getConditionalRejectionProbabilitiesEnrichment(
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
    repeatedConfidenceIntervals <- .getRepeatedConfidenceIntervalsRatesEnrichment(
        design = design,
        dataInput = dataInput,
        stratifiedAnalysis = stratifiedAnalysis,
        intersectionTest = intersectionTest,
        stage = stage,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        tolerance = tolerance
    )

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

    return(results)
}

.getRootThetaRatesEnrichment <- function(...,
        design,
        dataInput,
        population,
        stage,
        directionUpper,
        normalApproximation,
        stratifiedAnalysis,
        intersectionTest,
        thetaLow,
        thetaUp,
        firstParameterName,
        secondValue,
        tolerance) {
    result <- .getOneDimensionalRoot(
        function(theta) {
            stageResults <- .getStageResultsRatesEnrichment(
                design = design,
                dataInput = dataInput,
                stage = stage,
                thetaH0 = theta,
                directionUpper = directionUpper,
                intersectionTest = intersectionTest,
                normalApproximation = normalApproximation,
                stratifiedAnalysis = stratifiedAnalysis,
                calculateSingleStepAdjusted = TRUE
            )
            firstValue <- stageResults[[firstParameterName]][population, stage]
            if (.isTrialDesignGroupSequential(design)) {
                firstValue <- .getOneMinusQNorm(firstValue)
            }
            return(firstValue - secondValue)
        },
        lower = thetaLow, upper = thetaUp, tolerance = tolerance,
        callingFunctionInformation = ".getRootThetaRatesEnrichment"
    )
    return(result)
}

.getRepeatedConfidenceIntervalsRatesEnrichmentAll <- function(...,
        design, dataInput,
        directionUpper = NA,
        stratifiedAnalysis = C_STRATIFIED_ANALYSIS_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        firstParameterName) {
    .assertIsValidIntersectionTestEnrichment(design, intersectionTest)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)

    stageResults <- .getStageResultsRatesEnrichment(
        design = design,
        dataInput = dataInput,
        stage = stage,
        thetaH0 = 0,
        directionUpper = directionUpper,
        intersectionTest = intersectionTest,
        normalApproximation = normalApproximation,
        stratifiedAnalysis = stratifiedAnalysis,
        calculateSingleStepAdjusted = FALSE
    )

    gMax <- stageResults$getGMax()
    repeatedConfidenceIntervals <- array(NA_real_, dim = c(gMax, 2, design$kMax))

    # Repeated onfidence intervals when using combination tests
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

    # necessary for adjustment for binding futility boundaries
    futilityCorr <- rep(NA_real_, design$kMax)

    stages <- (1:stage)
    for (k in stages) {
        startTime <- Sys.time()
        for (population in 1:gMax) {
            if (!is.na(stageResults$testStatistics[population, k]) && criticalValues[k] < C_QNORM_MAXIMUM) {
                thetaLow <- -1 + tolerance
                thetaUp <- 1 - tolerance
                # finding upper and lower RCI limits through root function
                repeatedConfidenceIntervals[population, 1, k] <- .getRootThetaRatesEnrichment(
                    design = design,
                    dataInput = dataInput,
                    population = population,
                    stage = k,
                    directionUpper = TRUE,
                    normalApproximation = normalApproximation,
                    stratifiedAnalysis = stratifiedAnalysis,
                    thetaLow = thetaLow,
                    thetaUp = thetaUp,
                    intersectionTest = intersectionTest,
                    firstParameterName = firstParameterName,
                    secondValue = criticalValues[k],
                    tolerance = tolerance
                )

                repeatedConfidenceIntervals[population, 2, k] <- .getRootThetaRatesEnrichment(
                    design = design,
                    dataInput = dataInput,
                    population = population,
                    stage = k,
                    directionUpper = FALSE,
                    normalApproximation = normalApproximation,
                    stratifiedAnalysis = stratifiedAnalysis,
                    thetaLow = thetaLow, thetaUp = thetaUp,
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

                    futilityCorr[k] <- .getRootThetaRatesEnrichment(
                        design = design,
                        dataInput = dataInput,
                        population = population,
                        stage = k - 1,
                        directionUpper = directionUpper,
                        normalApproximation = normalApproximation,
                        stratifiedAnalysis = stratifiedAnalysis,
                        thetaLow = thetaLow, thetaUp = thetaUp,
                        intersectionTest = intersectionTest,
                        firstParameterName = parameterName,
                        secondValue = bounds[k - 1],
                        tolerance = tolerance
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

#'
#' RCIs based on inverse normal combination test
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsRatesEnrichmentInverseNormal <- function(...,
        design,
        dataInput,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        stratifiedAnalysis = C_STRATIFIED_ANALYSIS_DEFAULT,
        directionUpper = NA,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    if (!normalApproximation) {
        message("Repeated confidence intervals will be calculated under the normal approximation")
        normalApproximation <- TRUE
    }

    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsRatesEnrichmentInverseNormal",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsRatesEnrichmentAll(
        design = design,
        dataInput = dataInput,
        normalApproximation = normalApproximation,
        stratifiedAnalysis = stratifiedAnalysis,
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
.getRepeatedConfidenceIntervalsRatesEnrichmentFisher <- function(...,
        design,
        dataInput,
        normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
        stratifiedAnalysis = C_STRATIFIED_ANALYSIS_DEFAULT,
        directionUpper = NA,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    if (!normalApproximation) {
        message("Repeated confidence intervals will be calculated under the normal approximation")
        normalApproximation <- TRUE
    }

    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsRatesEnrichmentFisher",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsRatesEnrichmentAll(
        design = design,
        dataInput = dataInput,
        normalApproximation = normalApproximation,
        stratifiedAnalysis = stratifiedAnalysis,
        directionUpper = directionUpper,
        intersectionTest = intersectionTest,
        tolerance = tolerance,
        firstParameterName = "combFisher",
        ...
    ))
}

#'
#' Calculation of repeated confidence intervals (RCIs) for Rates
#'
#' @noRd
#'
.getRepeatedConfidenceIntervalsRatesEnrichment <- function(..., design) {
    if (.isTrialDesignInverseNormal(design)) {
        return(.getRepeatedConfidenceIntervalsRatesEnrichmentInverseNormal(design = design, ...))
    }

    if (.isTrialDesignFisher(design)) {
        return(.getRepeatedConfidenceIntervalsRatesEnrichmentFisher(design = design, ...))
    }

    .stopWithWrongDesignMessageEnrichment(design)
}

#'
#' Calculation of conditional power for Rates
#'
#' @noRd
#'
.getConditionalPowerRatesEnrichment <- function(...,
        stageResults,
        stage = stageResults$stage,
        nPlanned,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        piTreatments = NA_real_,
        piControls = NA_real_,
        useAdjustment = TRUE,
        iterations = C_ITERATIONS_DEFAULT,
        seed = NA_real_) {
    design <- stageResults$.design
    gMax <- stageResults$getGMax()
    kMax <- design$kMax

    piTreatmentsH1 <- .getOptionalArgument("piTreatmentsH1", ...)
    if (!is.null(piTreatmentsH1) && !is.na(piTreatmentsH1)) {
        if (!is.na(piTreatments)) {
            warning(sQuote("piTreatments"), " will be ignored because ",
                sQuote("piTreatmentsH1"), " is defined",
                call. = FALSE
            )
        }
        piTreatments <- piTreatmentsH1
    }

    if (is.matrix(piTreatments)) {
        piTreatments <- as.vector(piTreatments)
    }

    piControlH1 <- .getOptionalArgument("piControlH1", ...)
    if (!is.null(piControlH1) && !is.na(piControlH1)) {
        if (!is.na(piControl)) {
            warning(sQuote("piControl"), " will be ignored because ",
                sQuote("piControlH1"), " is defined",
                call. = FALSE
            )
        }
        piControl <- piControlH1
    }

    results <- ConditionalPowerResultsEnrichmentRates$new(
        .design = design,
        .stageResults = stageResults,
        piControls = piControls,
        piTreatments = piTreatments,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned
    )

    if (anyNA(nPlanned)) {
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
    .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned",
        lower = 0, upper = C_ALLOCATION_RATIO_MAXIMUM
    )
    results$.setParameterType("nPlanned", C_PARAM_USER_DEFINED)
    results$.setParameterType(
        "allocationRatioPlanned",
        ifelse(allocationRatioPlanned == C_ALLOCATION_RATIO_DEFAULT, C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED)
    )

    piControls <- .assertIsValidPiControlForEnrichment(
        piControls,
        stageResults,
        stage,
        results = results
    )
    piTreatments <- .assertIsValidPiTreatmentsForEnrichment(
        piTreatments,
        stageResults,
        stage,
        results = results
    )

    if ((length(piTreatments) != 1) && (length(piTreatments) != gMax)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            sprintf(paste0(
                "length of 'piTreatments' (%s) ",
                "must be equal to 'gMax' (%s) or 1"
            ), .arrayToString(piTreatments), gMax)
        )
    }

    if ((length(piControls) != 1) && (length(piControls) != gMax)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            sprintf(paste0(
                "length of 'piControls' (%s) ",
                "must be equal to 'gMax' (%s) or 1"
            ), .arrayToString(piControls), gMax)
        )
    }

    if (.isTrialDesignInverseNormal(design)) {
        return(.getConditionalPowerRatesEnrichmentInverseNormal(
            results = results,
            design = design,
            stageResults = stageResults,
            stage = stage,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            piControls = piControls,
            piTreatments = piTreatments,
            ...
        ))
    } else if (.isTrialDesignFisher(design)) {
        return(.getConditionalPowerRatesEnrichmentFisher(
            results = results,
            design = design,
            stageResults = stageResults,
            stage = stage,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            useAdjustment = useAdjustment,
            piControls = piControls,
            piTreatments = piTreatments,
            iterations = iterations,
            seed = seed,
            ...
        ))
    }

    stop(
        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
        "'design' must be an instance of TrialDesignInverseNormal or TrialDesignFisher"
    )
}

#'
#' Calculation of conditional power based on inverse normal method
#'
#' @noRd
#'
.getConditionalPowerRatesEnrichmentInverseNormal <- function(...,
        results,
        design,
        stageResults,
        stage,
        allocationRatioPlanned,
        nPlanned,
        piTreatments,
        piControls) {
    .assertIsTrialDesignInverseNormal(design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerRatesEnrichmentInverseNormal",
        ignore = c("piTreatmentsH1", "piControlH1"), ...
    )

    kMax <- design$kMax
    gMax <- stageResults$getGMax()
    weights <- .getWeightsInverseNormal(design)
    informationRates <- design$informationRates

    nPlanned <- c(rep(NA_real_, stage), nPlanned)

    condError <- .getConditionalRejectionProbabilitiesEnrichment(design = design, stageResults = stageResults)[, stage]
    ml <- (allocationRatioPlanned * piTreatments + piControls) / (1 + allocationRatioPlanned)
    adjustment <- .getOneMinusQNorm(condError) * (1 - sqrt(ml * (1 - ml) * (1 + allocationRatioPlanned)) /
        sqrt(piTreatments * (1 - piTreatments) + allocationRatioPlanned * piControls * (1 - piControls))) *
        (1 + allocationRatioPlanned) / sqrt(allocationRatioPlanned * sum(nPlanned[(stage + 1):kMax]))
    adjustment[condError < 1e-12] <- 0

    .setValueAndParameterType(
        results, "allocationRatioPlanned",
        allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT
    )
    results$.setParameterType("piControls", C_PARAM_DEFAULT_VALUE)
    if (length(piTreatments) == 1) {
        piTreatments <- rep(piTreatments, gMax)
        results$.setParameterType("piTreatments", C_PARAM_GENERATED)
    } else {
        results$.setParameterType("piTreatments", C_PARAM_DEFAULT_VALUE)
    }

    if (stageResults$directionUpper) {
        standardizedEffect <- (piTreatments - piControls - stageResults$thetaH0) / sqrt(piTreatments * (1 - piTreatments) +
            allocationRatioPlanned * piControls * (1 - piControls)) * sqrt(1 + allocationRatioPlanned) + adjustment
    } else {
        standardizedEffect <- -(piTreatments - piControls - stageResults$thetaH0) / sqrt(piTreatments * (1 - piTreatments) +
            allocationRatioPlanned * piControls * (1 - piControls)) * sqrt(1 + allocationRatioPlanned) + adjustment
    }

    nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned

    ctr <- .performClosedCombinationTest(stageResults = stageResults)
    criticalValues <- .getCriticalValues(design)

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

    results$piTreatments <- piTreatments
    results$piControls <- piControls
    return(results)
}

#'
#' Calculation of conditional power based on Fisher's combination test
#'
#' @noRd
#'
.getConditionalPowerRatesEnrichmentFisher <- function(...,
        results,
        design,
        stageResults,
        stage,
        allocationRatioPlanned,
        nPlanned,
        piTreatments,
        piControls,
        useAdjustment = TRUE,
        iterations,
        seed) {
    .assertIsTrialDesignFisher(design)
    .assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerRatesEnrichmentFisher",
        ignore = c("piTreatmentsH1", "piControlH1"), ...
    )

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

    nPlanned <- c(rep(NA_real_, stage), nPlanned)

    if (useAdjustment) {
        condError <- .getConditionalRejectionProbabilitiesEnrichment(
            design = design, stageResults = stageResults,
            iterations = iterations, seed = seed
        )[, stage]

        ml <- (allocationRatioPlanned * piTreatments + piControls) / (1 + allocationRatioPlanned)
        adjustment <- .getOneMinusQNorm(condError) * (1 - sqrt(ml * (1 - ml) * (1 + allocationRatioPlanned)) /
            sqrt(piTreatments * (1 - piTreatments) + allocationRatioPlanned * piControls * (1 - piControls))) *
            (1 + allocationRatioPlanned) / sqrt(allocationRatioPlanned * sum(nPlanned[(stage + 1):kMax]))
        adjustment[condError < 1e-12] <- 0
    } else {
        adjustment <- 0
    }

    .setValueAndParameterType(results, "allocationRatioPlanned", allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT)
    if (length(piTreatments) == 1) {
        piTreatments <- rep(piTreatments, gMax)
        results$.setParameterType("piTreatments", C_PARAM_GENERATED)
    } else {
        results$.setParameterType("piTreatments", C_PARAM_DEFAULT_VALUE)
    }

    if (stageResults$directionUpper) {
        standardizedEffect <- (piTreatments - piControls) / sqrt(piTreatments * (1 - piTreatments) +
            allocationRatioPlanned * piControls * (1 - piControls)) * sqrt(1 + allocationRatioPlanned) + adjustment
    } else {
        standardizedEffect <- -(piTreatments - piControls - stageResults$thetaH0) / sqrt(piTreatments * (1 - piTreatments) +
            allocationRatioPlanned * piControls * (1 - piControls)) * sqrt(1 + allocationRatioPlanned) + adjustment
    }

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
                            kMax = kMax,
                            alpha0Vec = design$alpha0Vec,
                            criticalValues = criticalValues,
                            weightsFisher = weightsFisher,
                            pValues = pValues,
                            currentKMax = k,
                            thetaH1 = standardizedEffect[population],
                            stage = stage,
                            nPlanned = nPlanned
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
                    warning("Calculation not possible: could not calculate conditional power for stage ",
                        kMax,
                        call. = FALSE
                    )
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

    results$piTreatments <- piTreatments
    results$piControls <- piControls
    return(results)
}

#'
#' Calculation of conditional power and likelihood values for plotting the graph
#'
#' @noRd
#'
.getConditionalPowerLikelihoodRatesEnrichment <- function(...,
        stageResults,
        stage,
        nPlanned,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        piTreatmentRange,
        piControls = NA_real_,
        iterations = C_ITERATIONS_DEFAULT,
        seed = NA_real_) {
    .associatedArgumentsAreDefined(nPlanned = nPlanned, piTreatmentRange = piTreatmentRange)
    .assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
    .assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned",
        lower = 0, upper = C_ALLOCATION_RATIO_MAXIMUM
    )

    design <- stageResults$.design
    kMax <- design$kMax
    gMax <- stageResults$getGMax()
    intersectionTest <- stageResults$intersectionTest

    piControls <- .assertIsValidPiControlForEnrichment(piControls, stageResults, stage)

    if (length(piControls) == 1) {
        piControls <- rep(piControls, gMax)
    }

    piTreatmentRange <- .assertIsValidPiTreatmentRange(piTreatmentRange = piTreatmentRange)

    populations <- numeric(gMax * length(piTreatmentRange))
    effectValues <- numeric(gMax * length(piTreatmentRange))
    condPowerValues <- numeric(gMax * length(piTreatmentRange))
    likelihoodValues <- numeric(gMax * length(piTreatmentRange))

    stdErr <- sqrt(stageResults$overallPisTreatment[, stage] * (1 - stageResults$overallPisTreatment[, stage])) /
        sqrt(stageResults$.overallSampleSizes2[, stage])

    results <- ConditionalPowerResultsEnrichmentRates$new(
        .design = design,
        .stageResults = stageResults,
        piControls = piControls,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned
    )

    j <- 1
    for (i in seq(along = piTreatmentRange)) {
        for (population in (1:gMax)) {
            populations[j] <- population
            effectValues[j] <- piTreatmentRange[i]

            if (.isTrialDesignInverseNormal(design)) {
                condPowerValues[j] <- .getConditionalPowerRatesEnrichmentInverseNormal(
                    results = results,
                    design = design,
                    stageResults = stageResults,
                    stage = stage,
                    nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned,
                    piControls = piControls,
                    piTreatments = piTreatmentRange[i]
                )$conditionalPower[population, kMax]
            } else if (.isTrialDesignFisher(design)) {
                condPowerValues[j] <- .getConditionalPowerRatesEnrichmentFisher(
                    results = results,
                    design = design,
                    stageResults = stageResults,
                    stage = stage,
                    nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned,
                    useAdjustment = FALSE,
                    piControls = piControls,
                    piTreatments = piTreatmentRange[i],
                    iterations = iterations,
                    seed = seed
                )$conditionalPower[population, kMax]
            }

            likelihoodValues[j] <- stats::dnorm(
                piTreatmentRange[i],
                stageResults$overallPisTreatment[population, stage], stdErr[population]
            ) /
                stats::dnorm(0, 0, stdErr[population])
            j <- j + 1
        }
    }

    subtitle <- paste0(
        "Intersection test = ", intersectionTest,
        ", stage = ", stage, ", # of remaining subjects = ",
        sum(nPlanned), ", control rate = ", .formatSubTitleValue(piControls, "piControls"),
        ", allocation ratio = ", .formatSubTitleValue(allocationRatioPlanned, "allocationRatioPlanned")
    )

    return(list(
        populations = populations,
        xValues = effectValues,
        condPowerValues = condPowerValues,
        likelihoodValues = likelihoodValues,
        main = C_PLOT_MAIN_CONDITIONAL_POWER_WITH_LIKELIHOOD,
        xlab = "Treatment rate",
        ylab = C_PLOT_YLAB_CONDITIONAL_POWER_WITH_LIKELIHOOD,
        sub = subtitle
    ))
}
