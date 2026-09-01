## |
## |  *Analysis of endpoint-independent estimates*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |

#' @include f_logger.R
NULL

.getStageResultsGeneral <- function(
        ...,
        design,
        dataInput,
        thetaH0 = C_THETA_H0_GENERAL_DEFAULT,
        directionUpper = NA,
        stage = NA_integer_,
        userFunctionCallEnabled = FALSE) {
    .assertIsDatasetGeneral(dataInput)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design, stage = stage)
    directionUpper <- .assertIsValidDirectionUpper(directionUpper, design,
        objectType = "analysis", userFunctionCallEnabled = userFunctionCallEnabled,
        default = C_DIRECTION_UPPER_DEFAULT
    )
    thetaH0 <- .getDefaultThetaH0(dataInput, thetaH0)

    estimates <- dataInput$getEstimatesUpTo(stage)
    standardErrors <- dataInput$getStandardErrorsUpTo(stage)
    degreesOfFreedom <- dataInput$getDegreesOfFreedomUpTo(stage)

    testStatistics <- (estimates - thetaH0) / standardErrors
    pValues <- 1 - stats::pt(testStatistics, df = degreesOfFreedom)
    if (isFALSE(directionUpper)) {
        pValues <- 1 - pValues
    }

    informations <- 1 / standardErrors^2
    overallInformations <- cumsum(informations)
    overallEstimates <- cumsum(informations * estimates) / overallInformations
    overallStandardErrors <- 1 / sqrt(overallInformations)
    overallDegreesOfFreedom <- overallInformations^2 /
        cumsum(informations^2 / degreesOfFreedom)
    overallTestStatistics <- (overallEstimates - thetaH0) / overallStandardErrors
    overallPValues <- 1 - stats::pt(overallTestStatistics, df = overallDegreesOfFreedom)
    if (isFALSE(directionUpper)) {
        overallPValues <- 1 - overallPValues
    }

    weightsInverseNormal <- .getWeightsInverseNormal(design)
    weightsFisher <- .getWeightsFisher(design)
    combInverseNormal <- rep(NA_real_, stage)
    combFisher <- rep(NA_real_, stage)
    for (k in seq_len(stage)) {
        combInverseNormal[k] <-
            (weightsInverseNormal[seq_len(k)] %*% .getOneMinusQNorm(pValues[seq_len(k)])) /
            sqrt(sum(weightsInverseNormal[seq_len(k)]^2))
        combFisher[k] <- prod(pValues[seq_len(k)]^weightsFisher[seq_len(k)])
    }

    direction <- "undefined"
    if (design$sided == 1) {
        direction <- ifelse(!isFALSE(directionUpper), C_DIRECTION_UPPER, C_DIRECTION_LOWER)
    }

    return(StageResultsGeneral$new(
        design = design,
        dataInput = dataInput,
        stage = as.integer(stage),
        pValues = .fillWithNAs(pValues, design$kMax),
        testStatistics = .fillWithNAs(testStatistics, design$kMax),
        effectSizes = .fillWithNAs(overallEstimates, design$kMax),
        overallPValues = .fillWithNAs(overallPValues, design$kMax),
        overallTestStatistics = .fillWithNAs(overallTestStatistics, design$kMax),
        overallEstimates = .fillWithNAs(overallEstimates, design$kMax),
        overallStandardErrors = .fillWithNAs(overallStandardErrors, design$kMax),
        overallDegreesOfFreedom = .fillWithNAs(overallDegreesOfFreedom, design$kMax),
        combInverseNormal = .fillWithNAs(combInverseNormal, design$kMax),
        combFisher = .fillWithNAs(combFisher, design$kMax),
        weightsInverseNormal = weightsInverseNormal,
        weightsFisher = weightsFisher,
        thetaH0 = thetaH0,
        direction = direction
    ))
}

.getGeneralRepeatedConfidenceIntervalStatistic <- function(
        design,
        dataInput,
        stage,
        theta,
        directionUpper,
        parameterName = NULL) {
    stageResults <- .getStageResultsGeneral(
        design = design,
        dataInput = dataInput,
        stage = stage,
        thetaH0 = theta,
        directionUpper = directionUpper
    )
    if (is.null(parameterName)) {
        parameterName <- ifelse(.isTrialDesignGroupSequential(design),
            "overallPValues",
            ifelse(.isTrialDesignInverseNormalOrFixed(design), "combInverseNormal", "combFisher")
        )
    }
    value <- stageResults[[parameterName]][stage]
    if (.isTrialDesignGroupSequential(design) && identical(parameterName, "overallPValues")) {
        value <- .getOneMinusQNorm(value)
    }
    return(value)
}

.getGeneralRepeatedConfidenceIntervalRoot <- function(
        design,
        dataInput,
        stage,
        directionUpper,
        target,
        tolerance,
        parameterName = NULL) {
    objective <- function(theta) {
        .getGeneralRepeatedConfidenceIntervalStatistic(
            design = design,
            dataInput = dataInput,
            stage = stage,
            theta = theta,
            directionUpper = directionUpper,
            parameterName = parameterName
        ) - target
    }

    estimates <- dataInput$getEstimatesUpTo(stage)
    scale <- max(c(abs(estimates), dataInput$getStandardErrorsUpTo(stage), 1), na.rm = TRUE)
    lower <- min(estimates, na.rm = TRUE) - scale
    upper <- max(estimates, na.rm = TRUE) + scale
    iterations <- 0L
    while (objective(lower) * objective(upper) > 0 && iterations < 50L) {
        scale <- scale * 2
        lower <- min(estimates, na.rm = TRUE) - scale
        upper <- max(estimates, na.rm = TRUE) + scale
        iterations <- iterations + 1L
    }
    if (objective(lower) * objective(upper) > 0) {
        return(NA_real_)
    }

    return(.getOneDimensionalRoot(objective,
        lower = lower,
        upper = upper,
        tolerance = tolerance,
        callingFunctionInformation = paste0("General repeated confidence interval, stage ", stage)
    ))
}

.getRepeatedConfidenceIntervalsGeneral <- function(
        design,
        dataInput,
        ...,
        directionUpper = NA,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    criticalValues <- .getCriticalValues(design)
    criticalValues[is.infinite(criticalValues) & criticalValues > 0] <- C_QNORM_MAXIMUM
    criticalValues[is.infinite(criticalValues) & criticalValues < 0] <- C_QNORM_MINIMUM

    if (.isTrialDesignFisher(design)) {
        bounds <- design$alpha0Vec
        border <- C_ALPHA_0_VEC_DEFAULT
        conditionFunction <- .isFirstValueSmallerThanSecondValue
    } else {
        bounds <- .getFutilityBounds(design)
        border <- C_FUTILITY_BOUNDS_DEFAULT
        conditionFunction <- .isFirstValueGreaterThanSecondValue
    }

    result <- matrix(NA_real_, nrow = 2, ncol = design$kMax)
    futilityCorrection <- rep(NA_real_, design$kMax)
    for (k in seq_len(stage)) {
        if (criticalValues[k] >= C_QNORM_MAXIMUM) {
            next
        }
        result[1, k] <- .getGeneralRepeatedConfidenceIntervalRoot(
            design, dataInput, k, TRUE, criticalValues[k], tolerance
        )
        result[2, k] <- .getGeneralRepeatedConfidenceIntervalRoot(
            design, dataInput, k, FALSE, criticalValues[k], tolerance
        )
        if (!anyNA(result[, k])) {
            result[, k] <- sort(result[, k])
        }

        if (k > 1 && !is.na(bounds[k - 1]) &&
                conditionFunction(bounds[k - 1], border) && design$bindingFutility) {
            parameterName <- NULL
            if (.isTrialDesignFisher(design)) {
                parameterName <- "pValues"
            }
            correctionDirectionUpper <- ifelse(is.na(directionUpper), TRUE, directionUpper)
            futilityCorrection[k] <- .getGeneralRepeatedConfidenceIntervalRoot(
                design = design,
                dataInput = dataInput,
                stage = k - 1,
                directionUpper = correctionDirectionUpper,
                target = bounds[k - 1],
                tolerance = tolerance,
                parameterName = parameterName
            )
            if (isTRUE(correctionDirectionUpper)) {
                result[1, k] <- min(c(futilityCorrection[2:k], result[1, k]), na.rm = TRUE)
            } else {
                result[2, k] <- max(c(futilityCorrection[2:k], result[2, k]), na.rm = TRUE)
            }
        }
    }
    return(result)
}

.getAnalysisResultsGeneral <- function(
        ...,
        design,
        dataInput,
        directionUpper = NA,
        thetaH0 = C_THETA_H0_GENERAL_DEFAULT,
        nPlanned = NA_real_,
        allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .assertIsDatasetGeneral(dataInput)
    .assertIsValidTolerance(tolerance)
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)

    if (.isTrialDesignGroupSequential(design)) {
        results <- AnalysisResultsGroupSequential$new(design = design, dataInput = dataInput)
    } else if (.isTrialDesignInverseNormalOrFixed(design)) {
        results <- AnalysisResultsInverseNormal$new(design = design, dataInput = dataInput)
    } else if (.isTrialDesignFisher(design)) {
        results <- AnalysisResultsFisher$new(design = design, dataInput = dataInput)
    } else {
        .stopWithWrongDesignMessage(design, inclusiveConditionalDunnett = FALSE)
    }

    stageResults <- .getStageResultsGeneral(
        design = design,
        dataInput = dataInput,
        stage = stage,
        thetaH0 = thetaH0,
        directionUpper = directionUpper
    )
    results$.setStageResults(stageResults)
    .setValueAndParameterType(results, "thetaH0", thetaH0, C_THETA_H0_GENERAL_DEFAULT)
    .setValueAndParameterType(results, "directionUpper", directionUpper, C_DIRECTION_UPPER_DEFAULT)

    results$testActions <- getTestActions(stageResults = stageResults)
    results$.setParameterType("testActions", C_PARAM_GENERATED)

    repeatedConfidenceIntervals <- .getRepeatedConfidenceIntervalsGeneral(
        design = design,
        dataInput = dataInput,
        stage = stage,
        directionUpper = directionUpper,
        tolerance = tolerance
    )
    results$repeatedConfidenceIntervalLowerBounds <- repeatedConfidenceIntervals[1, ]
    results$repeatedConfidenceIntervalUpperBounds <- repeatedConfidenceIntervals[2, ]
    results$repeatedPValues <- getRepeatedPValues(stageResults = stageResults, tolerance = tolerance)
    for (parameterName in c(
            "repeatedConfidenceIntervalLowerBounds",
            "repeatedConfidenceIntervalUpperBounds",
            "repeatedPValues")) {
        results$.setParameterType(parameterName, C_PARAM_GENERATED)
    }

    if (design$kMax > 1) {
        finalPValue <- getFinalPValue(stageResults, showWarnings = FALSE)
        results$finalPValues <- .getVectorWithFinalValueAtFinalStage(
            kMax = design$kMax,
            finalValue = finalPValue$pFinal,
            finalStage = finalPValue$finalStage
        )
        results$finalStage <- finalPValue$finalStage
        results$.setParameterType("finalPValues", C_PARAM_GENERATED)
        results$.setParameterType("finalStage", C_PARAM_GENERATED)

        finalStage <- finalPValue$finalStage
        results$finalConfidenceIntervalLowerBounds <- .getVectorWithFinalValueAtFinalStage(
            kMax = design$kMax,
            finalValue = repeatedConfidenceIntervals[1, finalStage],
            finalStage = finalStage
        )
        results$finalConfidenceIntervalUpperBounds <- .getVectorWithFinalValueAtFinalStage(
            kMax = design$kMax,
            finalValue = repeatedConfidenceIntervals[2, finalStage],
            finalStage = finalStage
        )
        results$medianUnbiasedEstimates <- .getVectorWithFinalValueAtFinalStage(
            kMax = design$kMax,
            finalValue = mean(repeatedConfidenceIntervals[, finalStage]),
            finalStage = finalStage
        )
        for (parameterName in c(
                "finalConfidenceIntervalLowerBounds",
                "finalConfidenceIntervalUpperBounds",
                "medianUnbiasedEstimates")) {
            results$.setParameterType(parameterName, C_PARAM_GENERATED)
        }
    }

    for (parameterName in c(
            "normalApproximation", "equalVariances", "assumedStDev", "thetaH1",
            "nPlanned", "allocationRatioPlanned", "conditionalPower",
            "conditionalRejectionProbabilities")) {
        results$.setParameterType(parameterName, C_PARAM_NOT_APPLICABLE)
    }

    return(results)
}

.getFinalConfidenceIntervalGeneral <- function(
        ...,
        design,
        dataInput,
        directionUpper = NA,
        tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
    intervals <- .getRepeatedConfidenceIntervalsGeneral(
        design = design,
        dataInput = dataInput,
        stage = stage,
        directionUpper = directionUpper,
        tolerance = tolerance
    )
    return(list(
        finalStage = stage,
        medianUnbiased = mean(intervals[, stage]),
        finalConfidenceInterval = intervals[, stage]
    ))
}
