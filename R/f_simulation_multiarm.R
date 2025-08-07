## |
## |  *Simulation of multi-arm design with combination test and conditional error approach*
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
## |  File version: $Revision: 8781 $
## |  Last changed: $Date: 2025-08-07 13:34:35 +0200 (Do, 07 Aug 2025) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_utilities.R
NULL

.getIndicesOfClosedHypothesesSystemForSimulation <- function(gMax) {
    indices <- as.matrix(expand.grid(rep(list(1:0), gMax)))[1:(2^gMax - 1), ]
    if (gMax == 1) {
        indices <- as.matrix(indices)
    }

    return(indices)
}

.getIndicesOfClosedHypothesesSystemSorted <- function(gMax) {
    #   Not necessary for simulation and therefore not used
    indices <- as.data.frame(expand.grid(rep(list(1:0), gMax)))[1:(2^gMax - 1), ]
    if (gMax == 1) {
        return(as.matrix(indices))
    }

    y <- 10^(ncol(indices):1)
    indices$pos <- (as.matrix(indices) %*% y / 10)
    indices$sum <- as.numeric(rowSums(indices[, 1:gMax]))
    indices <- indices[order(indices$sum, indices$pos, decreasing = c(TRUE, TRUE)), ]
    indices <- indices[, 1:gMax]
    rownames(indices) <- as.character(1:nrow(indices))

    return(as.matrix(indices))
}

.selectTreatmentArms <- function(
    typeOfSelection,
    epsilonValue,
    rValue,
    threshold,
    selectArmsFunction,
    selectArmsFunctionArgs,
    survival = FALSE
) {
    effectVector <- selectArmsFunctionArgs$effectVector
    gMax <- length(effectVector)

    if (typeOfSelection != "userDefined") {
        if (typeOfSelection == "all") {
            selectedArms <- rep(TRUE, gMax)
        } else {
            selectedArms <- rep(FALSE, gMax)
            if (typeOfSelection == "best") {
                selectedArms[which.max(effectVector)] <- TRUE
            } else if (tolower(typeOfSelection) == "rbest") {
                selectedArms[order(effectVector, decreasing = TRUE)[1:rValue]] <- TRUE
                selectedArms[is.na(effectVector)] <- FALSE
            } else if (typeOfSelection == "epsilon") {
                selectedArms[max(effectVector, na.rm = TRUE) - effectVector <= epsilonValue] <- TRUE
                selectedArms[is.na(effectVector)] <- FALSE
            }
        }
        selectedArms[effectVector <= threshold] <- FALSE
    } else {
        functionArgumentNames <- .getFunctionArgumentNames(selectArmsFunction, ignoreThreeDots = TRUE)
        .assertIsValidFunction(
            fun = selectArmsFunction,
            funArgName = "selectArmsFunction",
            expectedArguments = names(selectArmsFunctionArgs),
            validateThreeDots = FALSE
        )
        selectedArms <- do.call(what = selectArmsFunction, args = selectArmsFunctionArgs[functionArgumentNames])

        msg <- paste0(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'selectArmsFunction' returned an illegal or undefined result (",
            .arrayToString(selectedArms),
            "); "
        )
        if (length(selectedArms) != gMax) {
            stop(msg, "the output must be a logical vector of length 'gMax' (", gMax, ")")
        }
        if (!is.logical(selectedArms)) {
            stop(msg, "the output must be a logical vector (is ", .getClassName(selectedArms), ")")
        }
    }
    if (!survival) {
        selectedArms <- c(selectedArms, TRUE)
    }
    return(selectedArms)
}

.performClosedCombinationTestForSimulationMultiArm <- function(
    ...,
    stageResults,
    design,
    indices,
    intersectionTest,
    successCriterion
) {
    if (.isTrialDesignGroupSequential(design) && (design$kMax > 1)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "Group sequential design cannot be used for designs with treatment arm selection"
        )
    }

    gMax <- nrow(stageResults$testStatistics)
    kMax <- design$kMax

    adjustedStageWisePValues <- matrix(NA_real_, nrow = 2^gMax - 1, ncol = kMax)
    overallAdjustedTestStatistics <- matrix(NA_real_, nrow = 2^gMax - 1, ncol = kMax)
    rejected <- matrix(FALSE, nrow = gMax, ncol = kMax)
    rejectedIntersections <- matrix(FALSE, nrow = nrow(indices), ncol = kMax)
    futility <- matrix(FALSE, nrow = gMax, ncol = kMax - 1)
    futilityIntersections <- matrix(FALSE, nrow = nrow(indices), ncol = kMax - 1)
    rejectedIntersectionsBefore <- matrix(FALSE, nrow = nrow(indices), ncol = 1)
    successStop <- rep(FALSE, kMax)
    futilityStop <- rep(FALSE, kMax - 1)

    if (.isTrialDesignFisher(design)) {
        weightsFisher <- .getWeightsFisher(design)
    } else {
        weightsInverseNormal <- .getWeightsInverseNormal(design)
    }

    if (gMax == 1) {
        intersectionTest <- "Bonferroni"
    }

    separatePValues <- stageResults$separatePValues
    if (intersectionTest == "Dunnett") {
        subjectsPerStage <- stageResults[[ifelse(
            !is.null(stageResults[["subjectsPerStage"]]),
            "subjectsPerStage",
            "cumulativeEventsPerStage"
        )]]
        testStatistics <- stageResults$testStatistics
    } else {
        subjectsPerStage <- NULL
        testStatistics <- NULL
    }

    for (k in 1:kMax) {
        if (intersectionTest == "Dunnett") {
            allocationRatiosPerStage <- rep(stageResults$allocationRatioPlanned[k], gMax)
            allocationRatiosPerStage[is.na(subjectsPerStage[1:gMax, k])] <- NA_real_
        }
        for (i in 1:(2^gMax - 1)) {
            if (!all(is.na(separatePValues[indices[i, ] == 1, k]))) {
                if (intersectionTest == "Dunnett") {
                    allocationRatiosSelected <- as.numeric(na.omit(allocationRatiosPerStage[indices[i, ] == 1]))
                    sigma <- sqrt(allocationRatiosSelected / (1 + allocationRatiosSelected)) %*%
                        sqrt(t(allocationRatiosSelected / (1 + allocationRatiosSelected)))
                    diag(sigma) <- 1

                    maxTestStatistic <- max(testStatistics[indices[i, ] == 1, k], na.rm = TRUE)

                    adjustedStageWisePValues[i, k] <- 1 -
                        .getMultivariateDistribution(
                            type = "normal",
                            upper = maxTestStatistic,
                            sigma = sigma,
                            df = NA_real_
                        )
                } else if (intersectionTest == "Bonferroni") {
                    #  Bonferroni adjusted p-values
                    adjustedStageWisePValues[i, k] <- min(c(
                        sum(indices[
                            i,
                            !is.na(separatePValues[, k])
                        ]) *
                            min(separatePValues[indices[i, ] == 1, k], na.rm = TRUE),
                        1
                    ))
                } else if (intersectionTest == "Simes") {
                    #  Simes adjusted p-values
                    adjustedStageWisePValues[i, k] <- min(
                        sum(indices[
                            i,
                            !is.na(separatePValues[, k])
                        ]) /
                            (1:sum(indices[i, !is.na(separatePValues[, k])])) *
                            sort(separatePValues[indices[i, ] == 1, k])
                    )
                } else if (intersectionTest == "Sidak") {
                    #  Sidak adjusted p-values
                    adjustedStageWisePValues[i, k] <- 1 -
                        (1 -
                            min(separatePValues[indices[i, ] == 1, k], na.rm = TRUE))^sum(indices[
                            i,
                            !is.na(separatePValues[, k])
                        ])
                } else if (intersectionTest == "Hierarchical") {
                    #  Hierarchically ordered hypotheses
                    separatePValues <- separatePValues
                    separatePValues[is.na(separatePValues[, 1:kMax])] <- 1
                    adjustedStageWisePValues[i, k] <- separatePValues[min(which(indices[i, ] == 1)), k]
                }

                if (.isTrialDesignFisher(design)) {
                    overallAdjustedTestStatistics[i, k] <-
                        prod(adjustedStageWisePValues[i, 1:k]^weightsFisher[1:k])
                } else {
                    overallAdjustedTestStatistics[i, k] <-
                        (weightsInverseNormal[1:k] %*% .getOneMinusQNorm(adjustedStageWisePValues[i, 1:k])) /
                        sqrt(sum(weightsInverseNormal[1:k]^2))
                }
            }

            criticalValues <- .getCriticalValues(design)
            if (.isTrialDesignFisher(design)) {
                rejectedIntersections[i, k] <- (overallAdjustedTestStatistics[i, k] <= criticalValues[k])
                if (k < kMax) {
                    futilityIntersections[i, k] <- (adjustedStageWisePValues[i, k] >= design$alpha0Vec[k])
                }
            } else if (.isTrialDesignInverseNormal(design)) {
                rejectedIntersections[i, k] <- (overallAdjustedTestStatistics[i, k] >= criticalValues[k])
                if (k < kMax) {
                    futilityIntersections[i, k] <- (overallAdjustedTestStatistics[i, k] <= design$futilityBounds[k])
                }
            }

            rejectedIntersections[is.na(rejectedIntersections[, k]), k] <- FALSE

            if ((k == kMax) && !rejectedIntersections[1, k]) {
                break
            }
        }

        rejectedIntersections[, k] <- rejectedIntersections[, k] | rejectedIntersectionsBefore
        rejectedIntersectionsBefore <- matrix(rejectedIntersections[, k], ncol = 1)

        for (j in 1:gMax) {
            rejected[j, k] <- all(rejectedIntersections[indices[, j] == 1, k], na.rm = TRUE)
            if (k < kMax) {
                futility[j, k] <- any(futilityIntersections[indices[, j] == 1, k], na.rm = TRUE)
            }
        }

        if (successCriterion == "all") {
            successStop[k] <- all(rejected[stageResults$selectedArms[1:gMax, k], k])
        } else {
            successStop[k] <- any(rejected[, k])
        }

        if (k < kMax) {
            futilityStop[k] <- all(futility[stageResults$selectedArms[1:gMax, k], k])
            if (all(!stageResults$selectedArms[1:gMax, k + 1], na.rm = TRUE)) {
                futilityStop[k] <- TRUE
            }
        }
    }

    return(list(
        separatePValues = separatePValues,
        adjustedStageWisePValues = adjustedStageWisePValues,
        overallAdjustedTestStatistics = overallAdjustedTestStatistics,
        rejected = rejected,
        rejectedIntersections = rejectedIntersections,
        selectedArms = stageResults$selectedArms,
        successStop = successStop,
        futilityStop = futilityStop
    ))
}

.getCriticalValuesDunnettForSimulation <- function(alpha, indices, allocationRatioPlanned) {
    if (allocationRatioPlanned[1] != allocationRatioPlanned[2]) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "The conditional Dunnett test assumes equal allocation ratios over the stages"
        )
    }

    gMax <- ncol(indices)
    frac <- rep(allocationRatioPlanned[1], gMax) / (1 + allocationRatioPlanned[1])
    criticalValuesDunnett <- rep(NA_real_, 2^gMax - 1)

    for (i in 1:(2^gMax - 1)) {
        zeta <- sqrt(frac[indices[i, ] == 1])
        sigma <- zeta %*% t(zeta)
        diag(sigma) <- 1
        criticalValuesDunnett[i] <- .getMultivariateDistribution(
            type = "quantile",
            upper = NA_real_,
            sigma = sigma,
            alpha = alpha
        )
    }
    return(criticalValuesDunnett)
}

.performClosedConditionalDunnettTestForSimulation <- function(
    stageResults,
    design,
    indices,
    criticalValuesDunnett,
    successCriterion
) {
    testStatistics <- stageResults$testStatistics
    separatePValues <- stageResults$separatePValues
    subjectsPerStage <- stageResults$subjectsPerStage
    overallTestStatistics <- stageResults$overallTestStatistics

    gMax <- nrow(testStatistics)
    informationAtInterim <- design$informationAtInterim
    secondStageConditioning <- design$secondStageConditioning
    kMax <- 2

    frac <- rep(stageResults$allocationRatioPlanned[1], gMax) / (1 + stageResults$allocationRatioPlanned[1])

    conditionalErrorRate <- matrix(NA_real_, nrow = 2^gMax - 1, ncol = 2)
    secondStagePValues <- matrix(NA_real_, nrow = 2^gMax - 1, ncol = 2)
    rejected <- matrix(FALSE, nrow = gMax, ncol = 2)
    rejectedIntersections <- matrix(FALSE, nrow = nrow(indices), ncol = kMax)
    futilityStop <- FALSE
    successStop <- rep(FALSE, kMax)

    signedTestStatistics <- testStatistics
    signedOverallTestStatistics <- overallTestStatistics
    signedOverallTestStatistics[, 2] <- sqrt(informationAtInterim) *
        testStatistics[, 1] +
        sqrt(1 - informationAtInterim) * testStatistics[, 2]

    if (all(!stageResults$selectedArms[1:gMax, 2], na.rm = TRUE)) {
        futilityStop <- TRUE
    }

    for (i in 1:(2^gMax - 1)) {
        integrand <- function(x) {
            innerProduct <- 1
            for (g in (1:gMax)) {
                if (indices[i, g] == 1) {
                    innerProduct <- innerProduct *
                        stats::pnorm(
                            ((criticalValuesDunnett[i] -
                                sqrt(informationAtInterim) * signedTestStatistics[g, 1] +
                                sqrt(1 - informationAtInterim) * sqrt(frac[g]) * x)) /
                                sqrt((1 - informationAtInterim) * (1 - frac[g]))
                        )
                }
            }
            return(innerProduct * dnorm(x))
        }
        conditionalErrorRate[i, 1] <- 1 - stats::integrate(integrand, lower = -Inf, upper = Inf)$value

        if (!all(is.na(separatePValues[indices[i, ] == 1, 2]))) {
            if (secondStageConditioning) {
                maxOverallTestStatistic <- max(
                    signedOverallTestStatistics[indices[i, ] == 1, 2],
                    na.rm = TRUE
                )
                integrand <- function(x) {
                    innerProduct <- 1
                    for (g in (1:gMax)) {
                        if ((indices[i, g] == 1) && !is.na(overallTestStatistics[g, 2])) {
                            innerProduct <- innerProduct *
                                stats::pnorm(
                                    ((maxOverallTestStatistic -
                                        sqrt(informationAtInterim) * signedTestStatistics[g, 1] +
                                        sqrt(1 - informationAtInterim) * sqrt(frac[g]) * x)) /
                                        sqrt((1 - informationAtInterim) * (1 - frac[g]))
                                )
                        }
                    }
                    return(innerProduct * dnorm(x))
                }
                secondStagePValues[i, 2] <- 1 - stats::integrate(integrand, lower = -Inf, upper = Inf)$value
            } else {
                maxTestStatistic <- max(signedTestStatistics[indices[i, ] == 1, 2], na.rm = TRUE)
                integrand <- function(x) {
                    innerProduct <- 1
                    for (g in (1:gMax)) {
                        if ((indices[i, g] == 1) && !is.na(separatePValues[g, 2])) {
                            innerProduct <- innerProduct *
                                stats::pnorm(((maxTestStatistic + sqrt(frac[g]) * x)) / sqrt(1 - frac[g]))
                        }
                    }
                    return(innerProduct * dnorm(x))
                }
                secondStagePValues[i, 2] <- 1 - stats::integrate(integrand, lower = -Inf, upper = Inf)$value
            }
        }

        rejectedIntersections[i, 2] <- (secondStagePValues[i, 2] <= conditionalErrorRate[i, 1])

        rejectedIntersections[is.na(rejectedIntersections[, 2]), 2] <- FALSE

        if (!rejectedIntersections[1, 2]) {
            break
        }
    }
    for (j in 1:gMax) {
        rejected[j, 2] <- all(rejectedIntersections[indices[, j] == 1, 2], na.rm = TRUE)
    }
    if (successCriterion == "all") {
        successStop[2] <- all(rejected[stageResults$selectedArms[1:gMax, 2], 2])
    } else {
        successStop[2] <- any(rejected[, 2])
    }

    return(list(
        separatePValues = separatePValues,
        conditionalErrorRate = conditionalErrorRate,
        secondStagePValues = secondStagePValues,
        rejected = rejected,
        rejectedIntersections = rejectedIntersections,
        selectedArms = stageResults$selectedArms,
        successStop = successStop,
        futilityStop = futilityStop
    ))
}

.createSimulationResultsMultiArmObject <- function(
    ...,
    design,
    activeArms,
    effectMatrix,
    typeOfShape,
    kappa = NA_real_, # survival only
    dropoutRate1 = NA_real_, # survival only
    dropoutRate2 = NA_real_, # survival only
    dropoutTime = NA_real_, # survival only
    eventTime = NA_real_, # survival only
    muMaxVector = NA_real_, # means only
    piMaxVector = NA_real_, # rates only
    piControl = NA_real_, # rates and survival only
    omegaMaxVector = NA_real_, # survival only
    gED50,
    slope,
    doseLevels,
    intersectionTest,
    stDev = NA_real_, # means only
    directionUpper = NA, # rates + survival only
    adaptations,
    typeOfSelection,
    effectMeasure,
    successCriterion,
    epsilonValue,
    rValue,
    threshold,
    plannedSubjects = NA_real_,
    accrualTime = NA_real_, # survival only
    accrualIntensity = NA_real_, # survival only
    maxNumberOfSubjects = NA_real_, # survival only
    plannedEvents = NA_real_, # survival only
    allocationRatioPlanned,
    minNumberOfSubjectsPerStage = NA_real_, # means + rates only
    maxNumberOfSubjectsPerStage = NA_real_, # means + rates only
    minNumberOfEventsPerStage = NA_real_, # survival only
    maxNumberOfEventsPerStage = NA_real_, # survival only
    conditionalPower,
    thetaH1 = NA_real_, # means + survival only
    stDevH1 = NA_real_, # means only
    piTreatmentsH1 = NA_real_, # rates only
    piControlH1 = NA_real_, # rates only
    maxNumberOfIterations,
    seed,
    calcSubjectsFunction = NULL, # means + rates only
    calcEventsFunction = NULL, # survival only
    selectArmsFunction,
    showStatistics,
    endpoint = c("means", "rates", "survival")
) {
    endpoint <- match.arg(endpoint)

    .assertIsSinglePositiveInteger(activeArms, "activeArms", naAllowed = TRUE, validateType = FALSE)

    if (endpoint == "means") {
        simulationResults <- SimulationResultsMultiArmMeans$new(design, showStatistics = showStatistics)
    } else if (endpoint == "rates") {
        simulationResults <- SimulationResultsMultiArmRates$new(design, showStatistics = showStatistics)
    } else if (endpoint == "survival") {
        simulationResults <- SimulationResultsMultiArmSurvival$new(design, showStatistics = showStatistics)
        .setValueAndParameterType(simulationResults, "maxNumberOfSubjects", maxNumberOfSubjects, NA_real_)
    }

    typeOfShape <- .assertIsValidTypeOfShape(typeOfShape)

    if (!is.null(effectMatrix)) {
        if (typeOfShape != "userDefined") {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "Change 'typeOfShape' (",
                typeOfShape,
                ") to 'userDefined'"
            )
        }
    }

    if (is.na(activeArms)) {
        if (!is.null(effectMatrix)) {
            effectMatrix <- .assertIsValidMatrix(
                effectMatrix,
                "effectMatrix",
                naAllowed = FALSE,
                returnSingleValueAsMatrix = TRUE
            )
            activeArms <- ncol(effectMatrix)
            simulationResults$activeArms <- activeArms
            simulationResults$.setParameterType("activeArms", C_PARAM_DERIVED)
        } else {
            activeArms <- 3L
            simulationResults$activeArms <- activeArms
            simulationResults$.setParameterType("activeArms", C_PARAM_DEFAULT_VALUE)
        }
    } else {
        if (!is.null(effectMatrix) && activeArms != ncol(effectMatrix)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "Number of columns of effect matrix (",
                ncol(effectMatrix),
                ") is not equal to specified 'activeArms' (",
                activeArms,
                ")"
            )
        }
        simulationResults$activeArms <- activeArms
        simulationResults$.setParameterType("activeArms", C_PARAM_USER_DEFINED)
    }

    if (activeArms > 8) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'activeArms' (", activeArms, ") max not exceed 8")
    }

    .assertIsSingleNumber(threshold, "threshold", naAllowed = FALSE)
    .assertIsSingleNumber(gED50, "gED50", naAllowed = TRUE)
    .assertIsInOpenInterval(gED50, "gED50", 
        lower = 0, upper = NULL, naAllowed = TRUE)

    .assertIsSingleNumber(slope, "slope", naAllowed = TRUE)
    .assertIsInOpenInterval(slope, "slope", 
        lower = 0, upper = NULL, naAllowed = TRUE)

    .assertIsSinglePositiveInteger(rValue, "rValue", naAllowed = TRUE, validateType = FALSE)

    .assertIsNumericVector(allocationRatioPlanned, "allocationRatioPlanned", naAllowed = TRUE)
    .assertIsInOpenInterval(
        allocationRatioPlanned,
        "allocationRatioPlanned",
        lower = 0,
        upper = C_ALLOCATION_RATIO_MAXIMUM,
        naAllowed = TRUE
    )

    .assertIsSingleNumber(conditionalPower, "conditionalPower", naAllowed = TRUE)
    .assertIsInOpenInterval(conditionalPower, "conditionalPower", 
        lower = 0, upper = 1, naAllowed = TRUE)

    .assertIsLogicalVector(adaptations, "adaptations", naAllowed = TRUE)

    if (endpoint %in% c("means", "rates")) {
        .assertIsNumericVector(minNumberOfSubjectsPerStage, "minNumberOfSubjectsPerStage", naAllowed = TRUE)
        .assertIsNumericVector(maxNumberOfSubjectsPerStage, "maxNumberOfSubjectsPerStage", naAllowed = TRUE)
    } else if (endpoint == "survival") {
        .assertIsNumericVector(minNumberOfEventsPerStage, "minNumberOfEventsPerStage", naAllowed = TRUE)
        .assertIsNumericVector(maxNumberOfEventsPerStage, "maxNumberOfEventsPerStage", naAllowed = TRUE)
    }

    .assertIsSinglePositiveInteger(maxNumberOfIterations, "maxNumberOfIterations", validateType = FALSE)
    .assertIsSingleLogical(showStatistics, "showStatistics", naAllowed = FALSE)
    .assertIsSingleNumber(seed, "seed", naAllowed = TRUE)

    if (endpoint %in% c("rates", "survival")) {
        .assertIsSingleLogical(directionUpper, "directionUpper")
    }

    if (endpoint %in% c("means", "survival")) {
        .assertIsSingleNumber(thetaH1, "thetaH1", naAllowed = TRUE) # means + survival only
    }

    if (endpoint == "means") {
        .assertIsValidStandardDeviation(stDev) # means only
        .assertIsSingleNumber(stDevH1, "stDevH1", naAllowed = TRUE)
        .assertIsInOpenInterval(stDevH1, "stDevH1", 
            lower = 0, upper = NULL, naAllowed = TRUE)
    }

    successCriterion <- .assertIsValidSuccessCriterion(successCriterion)
    effectMeasure <- .assertIsValidEffectMeasure(effectMeasure)

    gMax <- activeArms
    kMax <- design$kMax

    intersectionTest <- .getCorrectedIntersectionTestMultiArmIfNecessary(
        design,
        intersectionTest,
        userFunctionCallEnabled = TRUE
    )
    .assertIsValidIntersectionTestMultiArm(design, intersectionTest)

    typeOfSelection <- .assertIsValidTypeOfSelection(typeOfSelection, rValue, epsilonValue, activeArms)
    if (
        length(typeOfSelection) == 1 &&
            typeOfSelection == "userDefined" &&
            !is.null(threshold) &&
            length(threshold) == 1 &&
            threshold != -Inf
    ) {
        warning(
            "'threshold' (",
            threshold,
            ") will be ignored because 'typeOfSelection' = \"userDefined\"",
            call. = FALSE
        )
        threshold <- -Inf
    }

    if (length(typeOfSelection) == 1 && typeOfSelection != "userDefined" && !is.null(selectArmsFunction)) {
        warning("'selectArmsFunction' will be ignored because 'typeOfSelection' is not \"userDefined\"", call. = FALSE)
    } else if (!is.null(selectArmsFunction) && is.function(selectArmsFunction)) {
        simulationResults$selectArmsFunction <- selectArmsFunction
    }

    if (endpoint %in% c("rates", "survival")) {
        .setValueAndParameterType(simulationResults, "directionUpper", directionUpper, TRUE)
    }

    if (endpoint == "means") {
        effectMatrix <- .assertIsValidEffectMatrixMeans(
            simulationResults = simulationResults,
            typeOfShape = typeOfShape,
            effectMatrix = effectMatrix,
            muMaxVector = muMaxVector,
            gED50 = gED50,
            gMax = gMax,
            slope = slope,
            doseLevels = doseLevels
        )
        if (typeOfShape == "userDefined") {
            muMaxVector <- effectMatrix[, gMax]
        } else {
            .assertIsNumericVector(muMaxVector, "muMaxVector")
        }
        .setValueAndParameterType(
            simulationResults,
            "muMaxVector",
            muMaxVector,
            C_ALTERNATIVE_POWER_SIMULATION_DEFAULT
        )
        if (typeOfShape == "userDefined") {
            simulationResults$.setParameterType("muMaxVector", C_PARAM_DERIVED)
        }
    } else if (endpoint == "rates") {
        .assertIsSingleNumber(piTreatmentsH1, "piTreatmentsH1", naAllowed = TRUE)
        .assertIsInOpenInterval(piTreatmentsH1, "piTreatmentsH1", 
            lower = 0, upper = 1, naAllowed = TRUE)
        piTreatmentsH1 <- .ignoreParameterIfNotUsed(
            "piTreatmentsH1",
            piTreatmentsH1,
            kMax > 1,
            "design is fixed ('kMax' = 1)",
            "Assumed active rate(s)"
        )

        .setValueAndParameterType(simulationResults, "piTreatmentsH1", piTreatmentsH1, NA_real_)

        .assertIsSingleNumber(piControl, "piControl", naAllowed = FALSE) # , noDefaultAvailable = TRUE)
        .assertIsInOpenInterval(piControl, "piControl", 
            lower = 0, upper = 1, naAllowed = FALSE)
        .setValueAndParameterType(simulationResults, "piControl", piControl, 0.2)

        piControlH1 <- .ignoreParameterIfNotUsed(
            "piControlH1",
            piControlH1,
            kMax > 1,
            "design is fixed ('kMax' = 1)",
            "Assumed control rate"
        )

        .assertIsSingleNumber(piControlH1, "piControlH1", naAllowed = TRUE)
        .assertIsInOpenInterval(piControlH1, "piControlH1", 
            lower = 0, upper = 1, naAllowed = TRUE)
        .setValueAndParameterType(simulationResults, "piControlH1", piControlH1, NA_real_)

        effectMatrix <- .assertIsValidEffectMatrixRates(
            simulationResults = simulationResults,
            typeOfShape = typeOfShape,
            effectMatrix = effectMatrix,
            piMaxVector = piMaxVector,
            piControl = piControl,
            gED50 = gED50,
            gMax = gMax,
            slope = slope,
            doseLevels = doseLevels
        )

        if (typeOfShape == "userDefined") {
            piMaxVector <- effectMatrix[, gMax]
        }
        .setValueAndParameterType(simulationResults, "piMaxVector", piMaxVector, C_PI_1_DEFAULT)
        if (typeOfShape == "userDefined") {
            simulationResults$.setParameterType("piMaxVector", C_PARAM_DERIVED)
        }
    } else if (endpoint == "survival") {
        effectMatrix <- .assertIsValidEffectMatrixSurvival(
            simulationResults = simulationResults,
            typeOfShape = typeOfShape,
            effectMatrix = effectMatrix,
            omegaMaxVector = omegaMaxVector,
            gED50 = gED50,
            gMax = gMax,
            slope = slope,
            doseLevels = doseLevels
        )
        if (typeOfShape == "userDefined") {
            omegaMaxVector <- effectMatrix[, gMax]
        }
        .setValueAndParameterType(simulationResults, "omegaMaxVector", omegaMaxVector, C_RANGE_OF_HAZARD_RATIOS_DEFAULT)
        if (typeOfShape == "userDefined") {
            simulationResults$.setParameterType("omegaMaxVector", C_PARAM_DERIVED)
        }

        .assertIsSingleNumber(piControl, "piControl", naAllowed = TRUE) # , noDefaultAvailable = TRUE)
        .assertIsInOpenInterval(piControl, "piControl", 
            lower = 0, upper = 1, naAllowed = TRUE)
        .setValueAndParameterType(simulationResults, "piControl", piControl, 0.2)
        .setValueAndParameterType(simulationResults, "eventTime", eventTime, 12)

        if (!is.na(eventTime) && eventTime <= 0) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'eventTime' (", eventTime, ") must be > 0", call. = FALSE)
        }

        .setValueAndParameterType(simulationResults, "kappa", kappa, 1)

        .setValueAndParameterType(simulationResults, "accrualTime", accrualTime, 12)
        .setValueAndParameterType(simulationResults, "accrualIntensity", accrualIntensity, 0.1)

        .setValueAndParameterType(simulationResults, "dropoutRate1", dropoutRate1, 0)
        .setValueAndParameterType(simulationResults, "dropoutRate2", dropoutRate2, 0)
        .setValueAndParameterType(simulationResults, "dropoutTime", dropoutTime, 12)

        if (!is.na(dropoutTime) && dropoutTime <= 0) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dropoutTime' (", dropoutTime, ") must be > 0", call. = FALSE)
        }
        if (!is.na(dropoutRate1) && (dropoutRate1 < 0 || dropoutRate1 >= 1)) {
            stop(
                C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
                "'dropoutRate1' (",
                dropoutRate1,
                ") is out of bounds [0; 1)"
            )
        }
        if (!is.na(dropoutRate2) && (dropoutRate2 < 0 || dropoutRate2 >= 1)) {
            stop(
                C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
                "'dropoutRate2' (",
                dropoutRate2,
                ") is out of bounds [0; 1)"
            )
        }

        .assertIsIntegerVector(plannedEvents, "plannedEvents", validateType = FALSE)
        if (length(plannedEvents) != kMax) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'plannedEvents' (",
                .arrayToString(plannedEvents),
                ") must have length ",
                kMax
            )
        }
        .assertIsInClosedInterval(plannedEvents, "plannedEvents", lower = 1, upper = NULL)
        .assertValuesAreStrictlyIncreasing(plannedEvents, "plannedEvents")
        .setValueAndParameterType(simulationResults, "plannedEvents", plannedEvents, NA_real_)
    }

    .assertIsValidThreshold(threshold, gMax)

    if (endpoint %in% c("means", "rates")) {
        .assertIsValidPlannedSubjects(plannedSubjects, kMax)
    }

    if (endpoint %in% c("means", "survival")) {
        thetaH1 <- .ignoreParameterIfNotUsed(
            "thetaH1",
            thetaH1,
            kMax > 1,
            "design is fixed ('kMax' = 1)",
            "Assumed effect"
        )
    }

    if (endpoint == "means") {
        stDevH1 <- .ignoreParameterIfNotUsed(
            "stDevH1",
            stDevH1,
            kMax > 1,
            "design is fixed ('kMax' = 1)",
            "Assumed standard deviation"
        )
    }

    conditionalPower <- .ignoreParameterIfNotUsed(
        "conditionalPower",
        conditionalPower,
        kMax > 1,
        "design is fixed ('kMax' = 1)"
    )

    if (endpoint %in% c("means", "rates")) {
        # means + rates only

        minNumberOfSubjectsPerStage <- .ignoreParameterIfNotUsed(
            "minNumberOfSubjectsPerStage",
            minNumberOfSubjectsPerStage,
            kMax > 1,
            "design is fixed ('kMax' = 1)"
        )
        minNumberOfSubjectsPerStage <- .assertIsValidNumberOfSubjectsPerStage(
            minNumberOfSubjectsPerStage,
            "minNumberOfSubjectsPerStage",
            plannedSubjects,
            conditionalPower,
            calcSubjectsFunction,
            kMax,
            endpoint = endpoint
        )

        maxNumberOfSubjectsPerStage <- .ignoreParameterIfNotUsed(
            "maxNumberOfSubjectsPerStage",
            maxNumberOfSubjectsPerStage,
            kMax > 1,
            "design is fixed ('kMax' = 1)"
        )
        maxNumberOfSubjectsPerStage <- .assertIsValidNumberOfSubjectsPerStage(
            maxNumberOfSubjectsPerStage,
            "maxNumberOfSubjectsPerStage",
            plannedSubjects,
            conditionalPower,
            calcSubjectsFunction,
            kMax,
            endpoint = endpoint
        )

        if (kMax > 1) {
            if (
                !all(is.na(maxNumberOfSubjectsPerStage - minNumberOfSubjectsPerStage)) &&
                    any(maxNumberOfSubjectsPerStage - minNumberOfSubjectsPerStage < 0)
            ) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'maxNumberOfSubjectsPerStage' (",
                    .arrayToString(maxNumberOfSubjectsPerStage),
                    ") must be not smaller than minNumberOfSubjectsPerStage' (",
                    .arrayToString(minNumberOfSubjectsPerStage),
                    ")"
                )
            }
            .setValueAndParameterType(
                simulationResults,
                "minNumberOfSubjectsPerStage",
                minNumberOfSubjectsPerStage,
                NA_real_
            )
            .setValueAndParameterType(
                simulationResults,
                "maxNumberOfSubjectsPerStage",
                maxNumberOfSubjectsPerStage,
                NA_real_
            )
        }
    } else if (endpoint == "survival") {
        minNumberOfEventsPerStage <- .ignoreParameterIfNotUsed(
            "minNumberOfEventsPerStage",
            minNumberOfEventsPerStage,
            kMax > 1,
            "design is fixed ('kMax' = 1)"
        )
        minNumberOfEventsPerStage <- .assertIsValidNumberOfSubjectsPerStage(
            minNumberOfEventsPerStage,
            "minNumberOfEventsPerStage",
            plannedEvents,
            conditionalPower,
            calcEventsFunction,
            kMax,
            endpoint = endpoint
        )

        maxNumberOfEventsPerStage <- .ignoreParameterIfNotUsed(
            "maxNumberOfEventsPerStage",
            maxNumberOfEventsPerStage,
            kMax > 1,
            "design is fixed ('kMax' = 1)"
        )
        maxNumberOfEventsPerStage <- .assertIsValidNumberOfSubjectsPerStage(
            maxNumberOfEventsPerStage,
            "maxNumberOfEventsPerStage",
            plannedEvents,
            conditionalPower,
            calcEventsFunction,
            kMax,
            endpoint = endpoint
        )

        if (kMax > 1) {
            if (
                !all(is.na(maxNumberOfEventsPerStage - minNumberOfEventsPerStage)) &&
                    any(maxNumberOfEventsPerStage - minNumberOfEventsPerStage < 0)
            ) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'maxNumberOfEventsPerStage' (",
                    .arrayToString(maxNumberOfEventsPerStage),
                    ") must be not smaller than 'minNumberOfEventsPerStage' (",
                    .arrayToString(minNumberOfEventsPerStage),
                    ")"
                )
            }
            .setValueAndParameterType(
                simulationResults,
                "minNumberOfEventsPerStage",
                minNumberOfEventsPerStage,
                NA_real_
            )
            .setValueAndParameterType(
                simulationResults,
                "maxNumberOfEventsPerStage",
                maxNumberOfEventsPerStage,
                NA_real_
            )
        }
    }

    if (kMax == 1 && !is.na(conditionalPower)) {
        warning("'conditionalPower' will be ignored for fixed sample design", call. = FALSE)
    }
    if (endpoint %in% c("means", "rates") && kMax == 1 && !is.null(calcSubjectsFunction)) {
        warning("'calcSubjectsFunction' will be ignored for fixed sample design", call. = FALSE)
    }
    if (endpoint == "survival" && kMax == 1 && !is.null(calcEventsFunction)) {
        warning("'calcEventsFunction' will be ignored for fixed sample design", call. = FALSE)
    }

    if (endpoint %in% c("means", "rates") && is.na(conditionalPower) && is.null(calcSubjectsFunction)) {
        if (length(minNumberOfSubjectsPerStage) != 1 || !is.na(minNumberOfSubjectsPerStage)) {
            warning(
                "'minNumberOfSubjectsPerStage' (",
                .arrayToString(minNumberOfSubjectsPerStage),
                ") will be ignored because ",
                "neither 'conditionalPower' nor 'calcSubjectsFunction' is defined",
                call. = FALSE
            )
            simulationResults$minNumberOfSubjectsPerStage <- NA_real_
        }
        if (length(maxNumberOfSubjectsPerStage) != 1 || !is.na(maxNumberOfSubjectsPerStage)) {
            warning(
                "'maxNumberOfSubjectsPerStage' (",
                .arrayToString(maxNumberOfSubjectsPerStage),
                ") will be ignored because ",
                "neither 'conditionalPower' nor 'calcSubjectsFunction' is defined",
                call. = FALSE
            )
            simulationResults$maxNumberOfSubjectsPerStage <- NA_real_
        }
    }

    if (endpoint == "survival" && is.na(conditionalPower) && is.null(calcEventsFunction)) {
        if (length(minNumberOfEventsPerStage) != 1 || !is.na(minNumberOfEventsPerStage)) {
            warning(
                "'minNumberOfEventsPerStage' (",
                .arrayToString(minNumberOfEventsPerStage),
                ") ",
                "will be ignored because neither 'conditionalPower' nor 'calcEventsFunction' is defined",
                call. = FALSE
            )
            simulationResults$minNumberOfEventsPerStage <- NA_real_
        }
        if (length(maxNumberOfEventsPerStage) != 1 || !is.na(maxNumberOfEventsPerStage)) {
            warning(
                "'maxNumberOfEventsPerStage' (",
                .arrayToString(maxNumberOfEventsPerStage),
                ") ",
                "will be ignored because neither 'conditionalPower' nor 'calcEventsFunction' is defined",
                call. = FALSE
            )
            simulationResults$maxNumberOfEventsPerStage <- NA_real_
        }
    }

    if (endpoint %in% c("means", "rates")) {
        simulationResults$.setParameterType(
            "calcSubjectsFunction",
            ifelse(
                kMax == 1,
                C_PARAM_NOT_APPLICABLE,
                ifelse(!is.null(calcSubjectsFunction) && kMax > 1, C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE)
            )
        )
    } else if (endpoint == "survival") {
        simulationResults$.setParameterType(
            "calcEventsFunction",
            ifelse(
                kMax == 1,
                C_PARAM_NOT_APPLICABLE,
                ifelse(!is.null(calcEventsFunction) && kMax > 1, C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE)
            )
        )
    }

    if (endpoint == "means") {
        if (is.null(calcSubjectsFunction)) {
            calcSubjectsFunction <- .getSimulationMeansMultiArmStageSubjects
        } else {
            .assertIsValidFunction(
                fun = calcSubjectsFunction,
                funArgName = "calcSubjectsFunction",
                expectedFunction = .getSimulationMeansMultiArmStageSubjects
            )
        }
        simulationResults$calcSubjectsFunction <- calcSubjectsFunction
    } else if (endpoint == "rates") {
        if (is.null(calcSubjectsFunction)) {
            calcSubjectsFunction <- .getSimulationRatesMultiArmStageSubjects
        } else {
            .assertIsValidFunction(
                fun = calcSubjectsFunction,
                funArgName = "calcSubjectsFunction",
                expectedFunction = .getSimulationRatesMultiArmStageSubjects
            )
        }
        simulationResults$calcSubjectsFunction <- calcSubjectsFunction
    } else if (endpoint == "survival") {
        if (is.null(calcEventsFunction)) {
            calcEventsFunction <- .getSimulationSurvivalMultiArmStageEvents
        } else {
            .assertIsValidFunction(
                fun = calcEventsFunction,
                funArgName = "calcEventsFunction",
                expectedFunction = .getSimulationSurvivalMultiArmStageEvents
            )
        }
        simulationResults$calcEventsFunction <- calcEventsFunction
    }

    if (endpoint == "means") {
        .setValueAndParameterType(simulationResults, "stDev", stDev, C_STDEV_DEFAULT)
    }

    if (any(is.na(allocationRatioPlanned))) {
        allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
    }

    if (length(allocationRatioPlanned) == 1) {
        allocationRatioPlanned <- rep(allocationRatioPlanned, design$kMax)
    } else if (length(allocationRatioPlanned) != design$kMax) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'allocationRatioPlanned' (",
            .arrayToString(allocationRatioPlanned),
            ") ",
            "must have length 1 or ",
            design$kMax,
            " (kMax)"
        )
    }

    if (length(unique(allocationRatioPlanned)) == 1) {
        .setValueAndParameterType(
            simulationResults,
            "allocationRatioPlanned",
            allocationRatioPlanned[1],
            defaultValue = 1
        )
    } else {
        .setValueAndParameterType(
            simulationResults,
            "allocationRatioPlanned",
            allocationRatioPlanned,
            defaultValue = rep(1, design$kMax)
        )
    }

    if (endpoint %in% c("means", "rates")) {
        .setValueAndParameterType(simulationResults, "plannedSubjects", plannedSubjects, NA_real_)
        .setValueAndParameterType(
            simulationResults,
            "minNumberOfSubjectsPerStage",
            minNumberOfSubjectsPerStage,
            NA_real_,
            notApplicableIfNA = TRUE
        )
        .setValueAndParameterType(
            simulationResults,
            "maxNumberOfSubjectsPerStage",
            maxNumberOfSubjectsPerStage,
            NA_real_,
            notApplicableIfNA = TRUE
        )
    } else if (endpoint == "survival") {
        .setValueAndParameterType(simulationResults, "plannedEvents", plannedEvents, NA_real_)
        .setValueAndParameterType(
            simulationResults,
            "minNumberOfEventsPerStage",
            minNumberOfEventsPerStage,
            NA_real_,
            notApplicableIfNA = TRUE
        )
        .setValueAndParameterType(
            simulationResults,
            "maxNumberOfEventsPerStage",
            maxNumberOfEventsPerStage,
            NA_real_,
            notApplicableIfNA = TRUE
        )
    }
    .setValueAndParameterType(
        simulationResults,
        "conditionalPower",
        conditionalPower,
        NA_real_,
        notApplicableIfNA = TRUE
    )
    if (endpoint %in% c("means", "survival")) {
        .setValueAndParameterType(simulationResults, "thetaH1", thetaH1, NA_real_, notApplicableIfNA = TRUE)
    }
    if (endpoint == "means") {
        .setValueAndParameterType(simulationResults, "stDevH1", stDevH1, NA_real_, notApplicableIfNA = TRUE)
    }
    .setValueAndParameterType(
        simulationResults,
        "maxNumberOfIterations",
        as.integer(maxNumberOfIterations),
        C_MAX_SIMULATION_ITERATIONS_DEFAULT
    )
    simulationResults$.setParameterType("seed", ifelse(is.na(seed), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
    simulationResults$seed <- .setSeed(seed)

    if (is.null(adaptations) || all(is.na(adaptations))) {
        adaptations <- rep(TRUE, kMax - 1)
    }
    if (length(adaptations) != kMax - 1) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'adaptations' must have length ", (kMax - 1), " (kMax - 1)")
    }
    .setValueAndParameterType(simulationResults, "adaptations", adaptations, rep(TRUE, kMax - 1))

    if (typeOfShape == "sigmoidEmax") {
        .setValueAndParameterType(simulationResults, "gED50", gED50, NA_real_)
    }
    .setValueAndParameterType(simulationResults, "slope", slope, 1)
    if (typeOfSelection != "userDefined") {
        .setValueAndParameterType(simulationResults, "threshold", threshold, -Inf)
        .setValueAndParameterType(simulationResults, "epsilonValue", epsilonValue, NA_real_)
        .setValueAndParameterType(simulationResults, "rValue", rValue, NA_real_)
    }
    .setValueAndParameterType(
        simulationResults,
        "intersectionTest",
        intersectionTest,
        C_INTERSECTION_TEST_MULTIARMED_DEFAULT
    )
    .setValueAndParameterType(simulationResults, "typeOfSelection", typeOfSelection, C_TYPE_OF_SELECTION_DEFAULT)
    .setValueAndParameterType(simulationResults, "typeOfShape", typeOfShape, C_TYPE_OF_SHAPE_DEFAULT)
    .setValueAndParameterType(simulationResults, "successCriterion", successCriterion, C_SUCCESS_CRITERION_DEFAULT)
    .setValueAndParameterType(simulationResults, "effectMeasure", effectMeasure, C_EFFECT_MEASURE_DEFAULT)

    return(simulationResults)
}
