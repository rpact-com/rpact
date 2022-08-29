## |
## |  *Analysis of means in enrichment designs with adaptive test*
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
## |  File version: $Revision: 6488 $
## |  Last changed: $Date: 2022-08-15 10:28:13 +0200 (Mon, 15 Aug 2022) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_logger.R
NULL

.calcMeansVariancesTestStatistics <- function(dataInput, subset, stage, thetaH0, stratifiedAnalysis, varianceOption) {
    .assertIsSingleInteger(stage, "stage")
    .assertIsSingleNumber(thetaH0, "thetaH0")
    .assertIsSingleLogical(stratifiedAnalysis, "stratifiedAnalysis")
    .assertIsSingleCharacter(varianceOption, "varianceOption")

    n <- rep(NA_real_, 2)
    on <- rep(NA_real_, 2)
    m <- rep(NA_real_, 2)
    om <- rep(NA_real_, 2)
    v <- rep(NA_real_, 2)
    ov <- rep(NA_real_, 2)

    for (i in 1:2) {
        m[i] <- sum(dataInput$getSampleSizes(stage = stage, subset = subset, group = i) *
            dataInput$getMeans(stage = stage, subset = subset, group = i), na.rm = TRUE) /
            sum(dataInput$getSampleSizes(stage = stage, subset = subset, group = i), na.rm = TRUE)

        #  calculate residual variance from full population (only if gMax = 2)
        if (length(subset) == 1 && subset == "S1" && varianceOption == "pooledFromFull") {
            if (dataInput$isStratified()) {
                v[i] <- sum((dataInput$getSampleSizes(stage = stage, subset = c("S1", "R"), group = i) - 1) *
                    dataInput$getStDev(stage = stage, subset = c("S1", "R"), group = i)^2, na.rm = TRUE) /
                    (sum(dataInput$getSampleSizes(stage = stage, subset = c("S1", "R"), group = i) - 1, na.rm = TRUE))
                n[i] <- sum(dataInput$getSampleSizes(stage = stage, subset = c("S1", "R"), group = i), na.rm = TRUE)
            } else {
                if (is.na(dataInput$getSampleSizes(stage = stage, subset = c("F"), group = i))) {
                    v[i] <- dataInput$getStDev(stage = stage, subset = c("S1"), group = i)^2
                    n[i] <- dataInput$getSampleSizes(stage = stage, subset = c("S1"), group = i)
                } else {
                    v[i] <- dataInput$getStDev(stage = stage, subset = c("F"), group = i)^2
                    n[i] <- dataInput$getSampleSizes(stage = stage, subset = c("F"), group = i)
                }
            }
        } else if (varianceOption == "pooledFromFull") {
            v[i] <- sum((dataInput$getSampleSizes(stage = stage, subset = subset, group = i) - 1) *
                dataInput$getStDev(stage = stage, subset = subset, group = i)^2 /
                sum(dataInput$getSampleSizes(stage = stage, subset = subset, group = i) - 1, na.rm = TRUE))
            n[i] <- sum(dataInput$getSampleSizes(stage = stage, subset = subset, group = i), na.rm = TRUE)
        } else {
            v[i] <- sum((dataInput$getSampleSizes(stage = stage, subset = subset, group = i) - 1) *
                dataInput$getStDev(stage = stage, subset = subset, group = i)^2 +
                dataInput$getSampleSizes(stage = stage, subset = subset, group = i) *
                    (dataInput$getMeans(stage = stage, subset = subset, group = i) - m[i])^2, na.rm = TRUE) /
                (sum(dataInput$getSampleSizes(stage = stage, subset = subset, group = i), na.rm = TRUE) - 1)
            n[i] <- sum(dataInput$getSampleSizes(stage = stage, subset = subset, group = i), na.rm = TRUE)
        }

        #   calculation for overall data
        on[i] <- sum(dataInput$getOverallSampleSizes(stage = stage, subset = subset, group = i), na.rm = TRUE)
        om[i] <- sum(dataInput$getOverallSampleSizes(stage = stage, subset = subset, group = i) *
            dataInput$getOverallMeans(stage = stage, subset = subset, group = i), na.rm = TRUE) / on[i]
        ov[i] <- sum((dataInput$getOverallSampleSizes(stage = stage, subset = subset, group = i) - 1) *
            dataInput$getOverallStDev(stage = stage, subset = subset, group = i)^2 +
            dataInput$getOverallSampleSizes(stage = stage, subset = subset, group = i) *
                (dataInput$getOverallMeans(stage = stage, subset = subset, group = i) - om[i])^2, na.rm = TRUE) /
            (sum(dataInput$getOverallSampleSizes(stage = stage, subset = subset, group = i), na.rm = TRUE) - 1)
    }

    df <- NA_real_
    if (stratifiedAnalysis) {
        weights <- dataInput$getSampleSizes(stage = stage, subset = subset, group = 1) *
            dataInput$getSampleSizes(stage = stage, subset = subset, group = 2) /
            (dataInput$getSampleSizes(stage = stage, subset = subset, group = 1) +
                dataInput$getSampleSizes(stage = stage, subset = subset, group = 2))

        if (varianceOption == "pooledFromFull") {
            pv <- ((n[1] - 1) * v[1] + (n[2] - 1) * v[2]) / (n[1] + n[2] - 2)
            testStatistics <- sum((dataInput$getMeans(stage = stage, subset = subset, group = 1) -
                dataInput$getMeans(stage = stage, subset = subset, group = 2) - thetaH0) * weights,
            na.rm = TRUE
            ) / sqrt(sum(pv * weights, na.rm = TRUE))
        } else if (varianceOption == "pooled") {
            pv <- ((dataInput$getSampleSizes(stage = stage, subset = subset, group = 1) - 1) *
                dataInput$getStDevs(stage = stage, subset = subset, group = 1)^2 +
                (dataInput$getSampleSizes(stage = stage, subset = subset, group = 2) - 1) *
                    dataInput$getStDevs(stage = stage, subset = subset, group = 2)^2) /
                (dataInput$getSampleSizes(stage = stage, subset = subset, group = 1) +
                    dataInput$getSampleSizes(stage = stage, subset = subset, group = 2) - 2)
            testStatistics <- sum((dataInput$getMeans(stage = stage, subset = subset, group = 1) -
                dataInput$getMeans(stage = stage, subset = subset, group = 2) - thetaH0) * weights,
            na.rm = TRUE
            ) / sqrt(sum(pv * weights, na.rm = TRUE))
        } else {
            pv <- dataInput$getStDevs(stage = stage, subset = subset, group = 1)^2 /
                dataInput$getSampleSizes(stage = stage, subset = subset, group = 1) +
                dataInput$getStDevs(stage = stage, subset = subset, group = 2)^2 /
                    dataInput$getSampleSizes(stage = stage, subset = subset, group = 2)
            testStatistics <- sum((dataInput$getMeans(stage = stage, subset = subset, group = 1) -
                dataInput$getMeans(stage = stage, subset = subset, group = 2) - thetaH0) * weights,
            na.rm = TRUE
            ) / sqrt(sum(pv * weights^2, na.rm = TRUE))
        }
        df <- sum(dataInput$getSampleSizes(stage = stage, subset = subset, group = 1), na.rm = TRUE) +
            sum(dataInput$getSampleSizes(stage = stage, subset = subset, group = 2), na.rm = TRUE) -
            length(dataInput$getSampleSizes(stage = stage, subset = subset, group = 1)) -
            length(dataInput$getSampleSizes(stage = stage, subset = subset, group = 2))
    }

    # non-stratified analysis
    else {
        if (varianceOption == "pooledFromFull") {
            pv <- ((n[1] - 1) * v[1] + (n[2] - 1) * v[2]) / (n[1] + n[2] - 2)
            testStatistics <- (m[1] - m[2] - thetaH0) / sqrt(pv *
                (1 / sum(dataInput$getSampleSizes(stage = stage, subset = subset, group = 1), na.rm = TRUE) +
                    1 / sum(dataInput$getSampleSizes(stage = stage, subset = subset, group = 2), na.rm = TRUE)))
            df <- n[1] + n[2] -
                length(dataInput$getSampleSizes(stage = stage, subset = subset, group = 1)) -
                length(dataInput$getSampleSizes(stage = stage, subset = subset, group = 2))
        } else if (varianceOption == "pooled") {
            pv <- ((n[1] - 1) * v[1] + (n[2] - 1) * v[2]) / (n[1] + n[2] - 2)
            testStatistics <- (m[1] - m[2] - thetaH0) / sqrt(pv * (1 / n[1] + 1 / n[2]))
            df <- n[1] + n[2] - 2
        } else {
            testStatistics <- (m[1] - m[2] - thetaH0) / sqrt(v[1] / n[1] + v[2] / n[2])
            u <- v[1] / n[1] / (v[1] / n[1] + v[2] / n[2])
            df <- 1 / (u^2 / (n[1] - 1) + (1 - u)^2 / (n[2] - 1))
        }
    }

    testStatistics[is.nan(testStatistics)] <- NA_real_
    if (any(is.nan(om))) {
        om <- rep(NA_real_, 2)
        ov <- rep(NA_real_, 2)
    }

    # consider the case n[1] = n[2] = 0
    df[!is.na(df) & df <= 0] <- NA_real_

    ov[!is.na(ov) & ov <= 0] <- NA_real_

    if ("R" %in% subset && is.na(dataInput$getSampleSizes(stage = stage, subset = "R", group = 1)) ||
            ("S1" %in% subset) && is.na(dataInput$getSampleSizes(stage = stage, subset = "S1", group = 1)) ||
            ("S2" %in% subset) && is.na(dataInput$getSampleSizes(stage = stage, subset = "S2", group = 1)) ||
            ("S3" %in% subset) && is.na(dataInput$getSampleSizes(stage = stage, subset = "S3", group = 1)) ||
            ("S4" %in% subset) && is.na(dataInput$getSampleSizes(stage = stage, subset = "S4", group = 1))
        ) {
        n <- rep(NA_real_, 2)
        m <- rep(NA_real_, 2)
        v <- rep(NA_real_, 2)
        on <- rep(NA_real_, 2)
        om <- rep(NA_real_, 2)
        ov <- rep(NA_real_, 2)
        df <- NA_real_
        testStatistics <- NA_real_
    }

    return(list(
        populationNs = n,
        populationMeans = m,
        overallMeans = om,
        overallStDevs = sqrt(((on[1] - 1) * ov[1] + (on[2] - 1) * ov[2]) / (on[1] + on[2] - 2)),
        overallSampleSizes1 = on[1],
        overallSampleSizes2 = on[2],
        df = df,
        testStatistics = testStatistics
    ))
}


.getStageResultsMeansEnrichment <- function(..., design, dataInput,
        thetaH0 = C_THETA_H0_MEANS_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        stratifiedAnalysis = C_STRATIFIED_ANALYSIS_DEFAULT,
        varianceOption = C_VARIANCE_OPTION_ENRICHMENT_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        calculateSingleStepAdjusted = FALSE,
        userFunctionCallEnabled = FALSE) {
    .assertIsTrialDesign(design)
    .assertIsDatasetMeans(dataInput)
    .assertIsValidThetaH0DataInput(thetaH0, dataInput)
    .assertIsValidDirectionUpper(directionUpper, design$sided)
    .assertIsSingleLogical(normalApproximation, "normalApproximation")
    .assertIsValidVarianceOptionEnrichment(design, varianceOption)
    .assertIsValidIntersectionTestEnrichment(design, intersectionTest)
    .warnInCaseOfUnknownArguments(
        functionName = ".getStageResultsMeansEnrichment",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), "stage"), ...
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
    if (varianceOption == "pooledFromFull") {
        if (gMax > 2) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "gMax (", gMax,
                ") > 2: varianceOption 'pooledFromFull' can only be used for one subset"
            )
        }
    }

    if (intersectionTest == "SpiessensDebois" && varianceOption != "pooledFromFull" && !normalApproximation) {
        stop("Spiessens & Depois t test can only be performed with pooled ",
            "residual (stratified) variance from full population,
			 select 'varianceOption' = \"pooledFromFull\"",
            call. = FALSE
        )
    }

    if (intersectionTest == "SpiessensDebois" && !stratifiedAnalysis && !normalApproximation) {
        stop("Spiessens & Depois t test can only be performed with pooled ",
            "residual (stratified) variance from full population,
			select 'stratifiedAnalysis' = TRUE",
            call. = FALSE
        )
    }


    if (dataInput$isStratified() && (gMax > 4)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "gMax (", gMax,
            ") > 4: Stratified analysis not implemented"
        )
    }

    stageResults <- StageResultsEnrichmentMeans(
        design = design,
        dataInput = dataInput,
        thetaH0 = thetaH0,
        direction = ifelse(directionUpper, C_DIRECTION_UPPER, C_DIRECTION_LOWER),
        normalApproximation = normalApproximation,
        directionUpper = directionUpper,
        stratifiedAnalysis = stratifiedAnalysis,
        varianceOption = varianceOption,
        stage = stage
    )

    .setValueAndParameterType(
        stageResults, "intersectionTest", intersectionTest,
        C_INTERSECTION_TEST_ENRICHMENT_DEFAULT
    )
    .setValueAndParameterType(
        stageResults, "stratifiedAnalysis", stratifiedAnalysis,
        C_STRATIFIED_ANALYSIS_DEFAULT
    )

    effectSizes <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    means1 <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    means2 <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    stDevs1 <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    stDevs2 <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallSampleSizes1 <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallSampleSizes2 <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallStDevs <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    testStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    overallTestStatistics <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    separatePValues <- matrix(NA_real_, nrow = gMax, ncol = kMax)
    dimnames(testStatistics) <- list(
        paste("population ", 1:gMax, sep = ""),
        paste("stage ", (1:kMax), sep = "")
    )
    dimnames(separatePValues) <- list(
        paste("population ", 1:gMax, sep = ""),
        paste("stage ", (1:kMax), sep = "")
    )

    subsets <- .createSubsetsByGMax(gMax = gMax, stratifiedInput = dataInput$isStratified(), subsetIdPrefix = "S")
    for (k in 1:stage) {
        for (population in 1:gMax) {
            subset <- subsets[[population]]
            results <- .calcMeansVariancesTestStatistics(dataInput, subset, k, thetaH0, stratifiedAnalysis, varianceOption)
            effectSizes[population, k] <- results$overallMeans[1] - results$overallMeans[2]
            testStatistics[population, k] <- results$testStatistics
            if (normalApproximation) {
                separatePValues[population, k] <- 1 - stats::pnorm(testStatistics[population, k])
            } else {
                separatePValues[population, k] <- 1 - stats::pt(testStatistics[population, k], results$df)
            }
            overallSampleSizes1[population, k] <- results$overallSampleSizes1
            overallSampleSizes2[population, k] <- results$overallSampleSizes2
            overallStDevs[population, k] <- results$overallStDevs
            if (!directionUpper) {
                separatePValues[population, k] <- 1 - separatePValues[population, k]
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
            for (population in 1:gMax) {
                if ((intersectionTest == "Bonferroni") || (intersectionTest == "Simes")) {
                    singleStepAdjustedPValues[population, k] <- min(1, separatePValues[population, k] * selected)
                } else if (intersectionTest == "Sidak") {
                    singleStepAdjustedPValues[population, k] <- 1 - (1 - separatePValues[population, k])^selected
                } else if (intersectionTest == "SpiessensDebois") {
                    if (!is.na(testStatistics[population, k])) {
                        df <- NA_real_
                        if (!normalApproximation) {
                            if (dataInput$isStratified()) {
                                df <- sum(dataInput$getSampleSizes(stage = k) - 1, na.rm = TRUE)
                            } else {
                                if (selected == 2) {
                                    df <- sum(dataInput$getSampleSizes(stage = k, subset = "F") - 2, na.rm = TRUE)
                                } else {
                                    df <- sum(dataInput$getSampleSizes(stage = k, subset = "S1") - 2, na.rm = TRUE)
                                }
                            }
                        }
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
                            type = ifelse(normalApproximation, "normal", "t"),
                            upper = ifelse(directionUpper, testStatistics[population, k],
                                -testStatistics[population, k]
                            ),
                            sigma = sigma, df = df
                        )
                    }
                }
                if (.isTrialDesignInverseNormal(design)) {
                    combInverseNormal[population, k] <- (weightsInverseNormal[1:k] %*%
                        .getOneMinusQNorm(singleStepAdjustedPValues[population, 1:k])) /
                        sqrt(sum(weightsInverseNormal[1:k]^2))
                } else if (.isTrialDesignFisher(design)) {
                    combFisher[population, k] <- prod(singleStepAdjustedPValues[
                        population,
                        1:k
                    ]^weightsFisher[1:k])
                }
            }
        }

        stageResults$overallTestStatistics <- overallTestStatistics
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
        stageResults$effectSizes <- effectSizes
        stageResults$overallStDevs <- overallStDevs
        stageResults$.overallSampleSizes1 <- overallSampleSizes1
        stageResults$.overallSampleSizes2 <- overallSampleSizes2
        stageResults$testStatistics <- testStatistics
        stageResults$separatePValues <- separatePValues
    }

    return(stageResults)
}


.getAnalysisResultsMeansEnrichment <- function(..., design, dataInput) {
    if (.isTrialDesignInverseNormal(design)) {
        return(.getAnalysisResultsMeansInverseNormalEnrichment(design = design, dataInput = dataInput, ...))
    }

    if (.isTrialDesignFisher(design)) {
        return(.getAnalysisResultsMeansFisherEnrichment(design = design, dataInput = dataInput, ...))
    }

    .stopWithWrongDesignMessage(design)
}

.getAnalysisResultsMeansInverseNormalEnrichment <- function(...,
        design, dataInput,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        stratifiedAnalysis = C_STRATIFIED_ANALYSIS_DEFAULT,
        varianceOption = C_VARIANCE_OPTION_ENRICHMENT_DEFAULT,
        thetaH0 = C_THETA_H0_MEANS_DEFAULT,
        thetaH1 = NA_real_, assumedStDevs = NA_real_,
        nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .assertIsTrialDesignInverseNormal(design)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsMeansInverseNormalEnrichment",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), "stage"), ...
    )

    results <- AnalysisResultsEnrichmentInverseNormal(design = design, dataInput = dataInput)

    results <- .getAnalysisResultsMeansEnrichmentAll(
        results = results, design = design, dataInput = dataInput,
        intersectionTest = intersectionTest, stage = stage,
        directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        stratifiedAnalysis = stratifiedAnalysis,
        varianceOption = varianceOption,
        thetaH0 = thetaH0, thetaH1 = thetaH1,
        assumedStDevs = assumedStDevs, nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        tolerance = tolerance
    )

    return(results)
}

.getAnalysisResultsMeansFisherEnrichment <- function(...,
        design, dataInput,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        stratifiedAnalysis = C_STRATIFIED_ANALYSIS_DEFAULT,
        varianceOption = C_VARIANCE_OPTION_ENRICHMENT_DEFAULT,
        thetaH0 = C_THETA_H0_MEANS_DEFAULT,
        thetaH1 = NA_real_, assumedStDevs = NA_real_,
        nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
    .assertIsTrialDesignFisher(design)
    .assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getAnalysisResultsMeansFisherEnrichment",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
            design,
            powerCalculationEnabled = TRUE
        ), "stage"), ...
    )

    results <- AnalysisResultsEnrichmentFisher(design = design, dataInput = dataInput)
    .setValueAndParameterType(results, "iterations", as.integer(iterations), C_ITERATIONS_DEFAULT)
    .setValueAndParameterType(results, "seed", seed, NA_real_)
    
    results <- .getAnalysisResultsMeansEnrichmentAll(
        results = results, design = design, dataInput = dataInput,
        intersectionTest = intersectionTest, stage = stage, directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        stratifiedAnalysis = stratifiedAnalysis,
        varianceOption = varianceOption,
        thetaH0 = thetaH0, thetaH1 = thetaH1,
        assumedStDevs = assumedStDevs, nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        tolerance = tolerance, iterations = iterations, seed = seed
    )

    return(results)
}


.getAnalysisResultsMeansEnrichmentAll <- function(...,
        results, design, dataInput, intersectionTest, stage,
        directionUpper, normalApproximation, stratifiedAnalysis,
        varianceOption, thetaH0, thetaH1, assumedStDevs,
        nPlanned, allocationRatioPlanned, tolerance, iterations, seed) {
    startTime <- Sys.time()

    stageResults <- .getStageResultsMeansEnrichment(
        design = design, dataInput = dataInput,
        intersectionTest = intersectionTest, stage = stage,
        thetaH0 = thetaH0, directionUpper = directionUpper,
        normalApproximation = normalApproximation,
        stratifiedAnalysis = stratifiedAnalysis,
        varianceOption = varianceOption,
        userFunctionCallEnabled = TRUE
    )
    .logProgress("Stage results calculated", startTime = startTime)

    normalApproximation <- stageResults$normalApproximation
    intersectionTest <- stageResults$intersectionTest

    results$.setStageResults(stageResults)
    thetaH1 <- .assertIsValidThetaH1ForEnrichment(thetaH1, stageResults, stage, results = results)
    assumedStDevs <- .assertIsValidAssumedStDevForMultiHypotheses(
        assumedStDevs, stageResults, stage,
        results = results
    )

    .setValueAndParameterType(
        results, "intersectionTest",
        intersectionTest, C_INTERSECTION_TEST_ENRICHMENT_DEFAULT
    )
    .setValueAndParameterType(results, "directionUpper", directionUpper, C_DIRECTION_UPPER_DEFAULT)
    .setValueAndParameterType(
        results, "normalApproximation",
        normalApproximation, C_NORMAL_APPROXIMATION_MEANS_DEFAULT
    )
    .setValueAndParameterType(results, "stratifiedAnalysis", stratifiedAnalysis, C_STRATIFIED_ANALYSIS_DEFAULT)
    .setValueAndParameterType(results, "varianceOption", varianceOption, C_VARIANCE_OPTION_ENRICHMENT_DEFAULT)
    .setValueAndParameterType(results, "thetaH0", thetaH0, C_THETA_H0_MEANS_DEFAULT)
    .setConditionalPowerArguments(results, dataInput, nPlanned, allocationRatioPlanned)
    .setNPlannedAndThetaH1AndAssumedStDevs(results, nPlanned, thetaH1, assumedStDevs)

    startTime <- Sys.time()

    results$.closedTestResults <- getClosedCombinationTestResults(stageResults = stageResults)

    .logProgress("Closed test calculated", startTime = startTime)

    if (design$kMax > 1) {

        # conditional power
        startTime <- Sys.time()
        if (.isTrialDesignFisher(design)) {
            results$.conditionalPowerResults <- .getConditionalPowerMeansEnrichment(
                stageResults = stageResults,
                stage = stage, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
                thetaH1 = thetaH1, assumedStDevs = assumedStDevs, iterations = iterations, seed = seed
            )
            .synchronizeIterationsAndSeed(results)
        } else {
            results$.conditionalPowerResults <- .getConditionalPowerMeansEnrichment(
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
    repeatedConfidenceIntervals <- .getRepeatedConfidenceIntervalsMeansEnrichment(
        design = design, dataInput = dataInput,
        intersectionTest = intersectionTest, stage = stage,
        normalApproximation = normalApproximation,
        stratifiedAnalysis = stratifiedAnalysis,
        varianceOption = varianceOption,
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

    if (stratifiedAnalysis && !dataInput$isStratified()) {
        message("Standard deviations from full (and sub-populations) need to be stratified estimates")
    }

    return(results)
}


.getRootThetaMeansEnrichment <- function(..., design, dataInput, population, stage,
        directionUpper, normalApproximation, stratifiedAnalysis, varianceOption, intersectionTest,
        thetaLow, thetaUp, firstParameterName, secondValue, tolerance) {
    result <- .getOneDimensionalRoot(
        function(theta) {
            stageResults <- .getStageResultsMeansEnrichment(
                design = design, dataInput = dataInput,
                stage = stage, thetaH0 = theta, directionUpper = directionUpper,
                intersectionTest = intersectionTest,
                normalApproximation = normalApproximation,
                stratifiedAnalysis = stratifiedAnalysis,
                varianceOption = varianceOption,
                calculateSingleStepAdjusted = TRUE
            )
            firstValue <- stageResults[[firstParameterName]][population, stage]
            if (.isTrialDesignGroupSequential(design)) {
                firstValue <- .getOneMinusQNorm(firstValue)
            }
            return(firstValue - secondValue)
        },
        lower = thetaLow, upper = thetaUp, tolerance = tolerance,
        callingFunctionInformation = ".getRootThetaMeansEnrichment"
    )
    return(result)
}

.getUpperLowerThetaMeansEnrichment <- function(..., design, dataInput, theta, population, stage,
        directionUpper, normalApproximation, stratifiedAnalysis, varianceOption, conditionFunction,
        intersectionTest, firstParameterName, secondValue) {
    stageResults <- .getStageResultsMeansEnrichment(
        design = design, dataInput = dataInput,
        stage = stage, thetaH0 = theta, directionUpper = directionUpper,
        intersectionTest = intersectionTest,
        normalApproximation = normalApproximation,
        stratifiedAnalysis = stratifiedAnalysis,
        varianceOption = varianceOption,
        calculateSingleStepAdjusted = TRUE
    )

    firstValue <- stageResults[[firstParameterName]][population, stage]
    maxSearchIterations <- 30
    while (conditionFunction(secondValue, firstValue)) {
        theta <- 2 * theta
        stageResults <- .getStageResultsMeansEnrichment(
            design = design, dataInput = dataInput,
            stage = stage, thetaH0 = theta, directionUpper = directionUpper,
            intersectionTest = intersectionTest, normalApproximation = normalApproximation,
            stratifiedAnalysis = stratifiedAnalysis,
            varianceOption = varianceOption,
            calculateSingleStepAdjusted = TRUE
        )

        firstValue <- stageResults[[firstParameterName]][population, stage]
        maxSearchIterations <- maxSearchIterations - 1
        if (maxSearchIterations < 0) {
            stop(
                C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                sprintf(
                    paste0(
                        "failed to find theta (k = %s, firstValue = %s, ",
                        "secondValue = %s, levels(firstValue) = %s, theta = %s)"
                    ),
                    stage, stageResults[[firstParameterName]][population, stage], secondValue,
                    firstValue, theta
                )
            )
        }
    }

    return(theta)
}

.getRepeatedConfidenceIntervalsMeansEnrichmentAll <- function(...,
        design, dataInput,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        stratifiedAnalysis = C_STRATIFIED_ANALYSIS_DEFAULT,
        varianceOption = C_VARIANCE_OPTION_ENRICHMENT_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
        firstParameterName) {
    .assertIsValidIntersectionTestEnrichment(design, intersectionTest)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    stageResults <- .getStageResultsMeansEnrichment(
        design = design, dataInput = dataInput,
        stage = stage, thetaH0 = 0, directionUpper = directionUpper,
        intersectionTest = intersectionTest,
        normalApproximation = normalApproximation,
        stratifiedAnalysis = stratifiedAnalysis,
        varianceOption = varianceOption, calculateSingleStepAdjusted = FALSE
    )

    gMax <- stageResults$getGMax()

    repeatedConfidenceIntervals <- array(NA_real_, dim = c(gMax, 2, design$kMax))

    # Repeated confidence intervals when using combination tests
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
        for (population in 1:gMax) {
            if (!is.na(stageResults$testStatistics[population, k]) && criticalValues[k] < C_QNORM_MAXIMUM) {

                # finding maximum upper and minimum lower bounds for RCIs
                thetaLow <- .getUpperLowerThetaMeansEnrichment(
                    design = design, dataInput = dataInput,
                    theta = -1, population = population, stage = k, directionUpper = TRUE,
                    normalApproximation = normalApproximation,
                    stratifiedAnalysis = stratifiedAnalysis,
                    varianceOption = varianceOption,
                    conditionFunction = conditionFunction,
                    intersectionTest = intersectionTest,
                    firstParameterName = firstParameterName,
                    secondValue = criticalValues[k]
                )

                thetaUp <- .getUpperLowerThetaMeansEnrichment(
                    design = design, dataInput = dataInput,
                    theta = 1, population = population, stage = k, directionUpper = FALSE,
                    normalApproximation = normalApproximation,
                    stratifiedAnalysis = stratifiedAnalysis, varianceOption = varianceOption,
                    conditionFunction = conditionFunction,
                    intersectionTest = intersectionTest, firstParameterName = firstParameterName,
                    secondValue = criticalValues[k]
                )

                # finding upper and lower RCI limits through root function
                repeatedConfidenceIntervals[population, 1, k] <- .getRootThetaMeansEnrichment(
                    design = design,
                    dataInput = dataInput, population = population, stage = k, directionUpper = TRUE,
                    normalApproximation = normalApproximation,
                    stratifiedAnalysis = stratifiedAnalysis,
                    varianceOption = varianceOption,
                    thetaLow = thetaLow, thetaUp = thetaUp,
                    intersectionTest = intersectionTest,
                    firstParameterName = firstParameterName,
                    secondValue = criticalValues[k], tolerance = tolerance
                )

                repeatedConfidenceIntervals[population, 2, k] <- .getRootThetaMeansEnrichment(
                    design = design,
                    dataInput = dataInput, population = population, stage = k, directionUpper = FALSE,
                    normalApproximation = normalApproximation,
                    stratifiedAnalysis = stratifiedAnalysis,
                    varianceOption = varianceOption,
                    thetaLow = thetaLow, thetaUp = thetaUp,
                    intersectionTest = intersectionTest,
                    firstParameterName = firstParameterName,
                    secondValue = criticalValues[k], tolerance = tolerance
                )

                # adjustment for binding futility bounds
				if (k > 1 && !is.na(bounds[k - 1]) && conditionFunction(bounds[k - 1], border) && design$bindingFutility) {
                    parameterName <- ifelse(.isTrialDesignFisher(design),
                        "singleStepAdjustedPValues", firstParameterName
                    )

                    #  Calculate new lower and upper bounds
                    if (directionUpper) {
                        thetaLow <- .getUpperLowerThetaMeansEnrichment(
                            design = design,
                            dataInput = dataInput,
                            theta = -1, population = population, stage = k - 1, directionUpper = TRUE,
                            normalApproximation = normalApproximation,
                            stratifiedAnalysis = stratifiedAnalysis,
                            varianceOption = varianceOption,
                            conditionFunction = conditionFunction,
                            intersectionTest = intersectionTest,
                            firstParameterName = parameterName,
                            secondValue = bounds[k - 1]
                        )
                    } else {
                        thetaUp <- .getUpperLowerThetaMeansEnrichment(
                            design = design,
                            dataInput = dataInput,
                            theta = 1, population = population, stage = k - 1, directionUpper = FALSE,
                            normalApproximation = normalApproximation,
                            stratifiedAnalysis = stratifiedAnalysis,
                            varianceOption = varianceOption,
                            conditionFunction = conditionFunction,
                            intersectionTest = intersectionTest,
                            firstParameterName = parameterName,
                            secondValue = bounds[k - 1]
                        )
                    }

                    futilityCorr[k] <- .getRootThetaMeansEnrichment(
                        design = design, dataInput = dataInput,
                        population = population, stage = k - 1, directionUpper = directionUpper,
                        normalApproximation = normalApproximation,
                        stratifiedAnalysis = stratifiedAnalysis,
                        varianceOption = varianceOption,
                        thetaLow = thetaLow, thetaUp = thetaUp,
                        intersectionTest = intersectionTest,
                        firstParameterName = parameterName,
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
.getRepeatedConfidenceIntervalsMeansEnrichmentInverseNormal <- function(...,
        design, dataInput,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        stratifiedAnalysis = C_STRATIFIED_ANALYSIS_DEFAULT,
        varianceOption = C_VARIANCE_OPTION_ENRICHMENT_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsMeansEnrichmentInverseNormal",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsMeansEnrichmentAll(
        design = design, dataInput = dataInput,
        normalApproximation = normalApproximation,
        stratifiedAnalysis = stratifiedAnalysis,
        varianceOption = varianceOption,
        directionUpper = directionUpper,
        intersectionTest = intersectionTest,
        tolerance = tolerance, firstParameterName = "combInverseNormal", ...
    ))
}

#
# RCIs based on Fisher's combination test
#
.getRepeatedConfidenceIntervalsMeansEnrichmentFisher <- function(...,
        design, dataInput,
        normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT,
        stratifiedAnalysis = C_STRATIFIED_ANALYSIS_DEFAULT,
        varianceOption = C_VARIANCE_OPTION_ENRICHMENT_DEFAULT,
        directionUpper = C_DIRECTION_UPPER_DEFAULT,
        intersectionTest = C_INTERSECTION_TEST_ENRICHMENT_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getRepeatedConfidenceIntervalsMeansEnrichmentFisher",
        ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = TRUE), "stage"), ...
    )

    return(.getRepeatedConfidenceIntervalsMeansEnrichmentAll(
        design = design, dataInput = dataInput,
        normalApproximation = normalApproximation,
        stratifiedAnalysis = stratifiedAnalysis,
        varianceOption = varianceOption,
        directionUpper = directionUpper,
        intersectionTest = intersectionTest,
        tolerance = tolerance,
        firstParameterName = "combFisher", ...
    ))
}

#
#  Calculation of lower and upper limits of repeated confidence intervals (RCIs) for Means
#
.getRepeatedConfidenceIntervalsMeansEnrichment <- function(..., design) {
    if (.isTrialDesignInverseNormal(design)) {
        return(.getRepeatedConfidenceIntervalsMeansEnrichmentInverseNormal(design = design, ...))
    }
    if (.isTrialDesignFisher(design)) {
        return(.getRepeatedConfidenceIntervalsMeansEnrichmentFisher(design = design, ...))
    }
    .stopWithWrongDesignMessage(design)
}

#
#  Calculation of conditional power for Means
#
.getConditionalPowerMeansEnrichment <- function(..., stageResults, stage = stageResults$stage,
        nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        thetaH1 = NA_real_, assumedStDevs = NA_real_,
        iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
    design <- stageResults$.design
    gMax <- stageResults$getGMax()
    kMax <- design$kMax

    stDevsH1 <- .getOptionalArgument("stDevsH1", ...)
    if (!is.null(stDevsH1) && !is.na(stDevsH1)) {
        if (!is.na(assumedStDevs)) {
            warning(sQuote("assumedStDevs"), " will be ignored because ", sQuote("stDevsH1"), " is defined", call. = FALSE)
        }
        assumedStDevs <- stDevsH1
    }

    results <- ConditionalPowerResultsEnrichmentMeans(
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
    thetaH1 <- .assertIsValidThetaH1ForEnrichment(thetaH1, stageResults, stage, results = results)
    if (length(thetaH1) != 1 && length(thetaH1) != gMax) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            sprintf(paste0(
                "length of 'thetaH1' (%s) ",
                "must be equal to 'gMax' (%s) or 1"
            ), .arrayToString(thetaH1), gMax)
        )
    }
    if (length(assumedStDevs) != 1 && length(assumedStDevs) != gMax) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            sprintf(paste0(
                "length of 'assumedStDevs' (%s) ",
                "must be equal to 'gMax' (%s) or 1"
            ), .arrayToString(assumedStDevs), gMax)
        )
    }

    if (length(assumedStDevs) == 1) {
        results$assumedStDevs <- rep(assumedStDevs, gMax)
        results$.setParameterType("assumedStDevs", C_PARAM_GENERATED)
    } else {
        if (any(is.na(assumedStDevs[!is.na(stageResults$testStatistics[, stage])]))) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "any of 'assumedStDevs' not correctly specified"
            )
        }
    }

    if (length(thetaH1) > 1) {
        if (any(is.na(thetaH1[!is.na(stageResults$testStatistics[, stage])]))) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "any of 'thetaH1' not correctly specified"
            )
        }
    }

    if (.isTrialDesignInverseNormal(design)) {
        return(.getConditionalPowerMeansEnrichmentInverseNormal(
            results = results, stageResults = stageResults, stage = stage,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            thetaH1 = thetaH1,
            assumedStDevs = assumedStDevs, ...
        ))
    } else if (.isTrialDesignFisher(design)) {
        return(.getConditionalPowerMeansEnrichmentFisher(
            results = results, stageResults = stageResults, stage = stage,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            thetaH1 = thetaH1,
            assumedStDevs = assumedStDevs,
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
.getConditionalPowerMeansEnrichmentInverseNormal <- function(..., results, stageResults, stage,
        allocationRatioPlanned, nPlanned, thetaH1, assumedStDevs) {
    design <- stageResults$.design
    .assertIsTrialDesignInverseNormal(design)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerMeansEnrichmentInverseNormal",
        ignore = c("stage", "design", "stDevsH1"), ...
    )

    kMax <- design$kMax
    gMax <- stageResults$getGMax()

    weights <- .getWeightsInverseNormal(design)
    informationRates <- design$informationRates
    nPlanned <- c(rep(NA_real_, stage), nPlanned)
    nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned

    .setValueAndParameterType(
        results, "allocationRatioPlanned",
        allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT
    )
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
    results$assumedStDevs <- assumedStDevs
    return(results)
}

#
# Calculation of conditional power based on Fisher's combination test
#
.getConditionalPowerMeansEnrichmentFisher <- function(..., results, stageResults, stage,
        allocationRatioPlanned, nPlanned, thetaH1, assumedStDevs,
        iterations, seed) {
    design <- stageResults$.design
    .assertIsTrialDesignFisher(design)
    .assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
    .warnInCaseOfUnknownArguments(
        functionName = ".getConditionalPowerMeansEnrichmentFisher",
        ignore = c("stage", "design", "stDevsH1"), ...
    )
    kMax <- design$kMax
    gMax <- stageResults$getGMax()
    criticalValues <- design$criticalValues
    weightsFisher <- .getWeightsFisher(design)

    results$conditionalPower <- matrix(NA_real_, nrow = gMax, ncol = kMax)

    results$iterations <- as.integer(iterations)
    results$.setParameterType("iterations", C_PARAM_USER_DEFINED)
    results$.setParameterType("seed", ifelse(is.na(seed), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
    results$seed <- .setSeed(seed)
    results$simulated <- FALSE
    results$.setParameterType("simulated", C_PARAM_DEFAULT_VALUE)

    .setValueAndParameterType(
        results, "allocationRatioPlanned",
        allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT
    )
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

    results$thetaH1 <- thetaH1
    results$assumedStDevs <- assumedStDevs
    return(results)
}

#
# Calculation of conditional power and likelihood values for plotting the graph
#
.getConditionalPowerLikelihoodMeansEnrichment <- function(..., stageResults, stage,
        nPlanned,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        thetaRange,
        assumedStDevs = NA_real_,
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

    populations <- numeric(gMax * length(thetaRange))
    effectValues <- numeric(gMax * length(thetaRange))
    condPowerValues <- numeric(gMax * length(thetaRange))
    likelihoodValues <- numeric(gMax * length(thetaRange))

    stdErr <- stageResults$overallStDevs[stage] *
        sqrt(1 / stageResults$.overallSampleSizes1[, stage] + 1 / stageResults$.overallSampleSizes2[, stage])

    results <- ConditionalPowerResultsEnrichmentMeans(
        .design = design,
        .stageResults = stageResults,
        assumedStDevs = assumedStDevs,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned
    )

    j <- 1
    for (i in seq(along = thetaRange)) {
        for (population in 1:gMax) {
            populations[j] <- population
            effectValues[j] <- thetaRange[i]

            if (.isTrialDesignInverseNormal(design)) {
                condPowerValues[j] <- .getConditionalPowerMeansEnrichmentInverseNormal(
                    results = results,
                    stageResults = stageResults, stage = stage, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned,
                    thetaH1 = thetaRange[i], assumedStDevs = assumedStDevs
                )$conditionalPower[population, kMax]
            } else if (.isTrialDesignFisher(design)) {
                condPowerValues[j] <- .getConditionalPowerMeansEnrichmentFisher(
                    results = results,
                    stageResults = stageResults, stage = stage, nPlanned = nPlanned,
                    allocationRatioPlanned = allocationRatioPlanned,
                    thetaH1 = thetaRange[i], assumedStDevs = assumedStDevs,
                    iterations = iterations, seed = seed
                )$conditionalPower[population, kMax]
            }

            likelihoodValues[j] <- stats::dnorm(
                thetaRange[i],
                stageResults$effectSizes[population, stage], stdErr[population]
            ) /
                stats::dnorm(0, 0, stdErr[population])
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
        populations = populations,
        xValues = effectValues,
        condPowerValues = condPowerValues,
        likelihoodValues = likelihoodValues,
        main = C_PLOT_MAIN_CONDITIONAL_POWER_WITH_LIKELIHOOD,
        xlab = "Effect size",
        ylab = C_PLOT_YLAB_CONDITIONAL_POWER_WITH_LIKELIHOOD,
        sub = subtitle
    ))
}
