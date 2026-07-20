## |
## |  *Sample size and power rates*
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

#' @include f_core_utilities.R
NULL

.getSampleSizeFixedRates <- function(
        ...,
        alpha = 0.025,
        beta = 0.2,
        sided = 1,
        normalApproximation = TRUE,
        conservative = TRUE,
        riskRatio = FALSE,
        thetaH0 = 0,
        pi1 = seq(0.4, 0.6, 0.1),
        pi2 = 0.2,
        directionUpper = NA,
        groups = 2,
        allocationRatioPlanned = 1) {
    if (groups == 1) {
        nFixed <- rep(NA_real_, length(pi1))

        for (i in seq_len(length(pi1))) {
            effect <- pi1[i] - thetaH0
            if (normalApproximation) {
                nFixed[i] <- (.getOneMinusQNorm(alpha / sided) *
                    sqrt(thetaH0 * (1 - thetaH0)) +
                    .getOneMinusQNorm(beta) * sqrt(pi1[i] * (1 - pi1[i])))^2 /
                    effect^2
            } else {
                lowerTail <- isFALSE(directionUpper)
                if (is.na(directionUpper)) {
                    lowerTail <- effect < 0
                }
                iterations <- 1
                nup <- 2
                while (
                    (stats::pbinom(
                        stats::qbinom(alpha, nup, thetaH0, lower.tail = lowerTail) - as.integer(lowerTail),
                        nup,
                        pi1[i],
                        lower.tail = lowerTail
                    ) <
                        1 - beta) &&
                        (iterations <= 50)
                ) {
                    nup <- 2 * nup
                    iterations <- iterations + 1
                }
                if (iterations > 50) {
                    nFixed[i] <- Inf
                } else {
                    tryCatch(
                        if (conservative) {
                            nFixed[i] <- max(which(
                                stats::pbinom(
                                    stats::qbinom(alpha, (1:(2 * nup)), thetaH0, lower.tail = lowerTail) -
                                        as.integer(lowerTail),
                                    (1:(2 * nup)),
                                    pi1[i],
                                    lower.tail = lowerTail
                                ) <
                                    1 - beta
                            )) +
                                1
                        } else {
                            nFixed[i] <- min(which(
                                stats::pbinom(
                                    stats::qbinom(alpha, (1:nup), thetaH0, lower.tail = lowerTail) -
                                        as.integer(lowerTail),
                                    (1:nup),
                                    pi1[i],
                                    lower.tail = lowerTail
                                ) >=
                                    1 - beta
                            ))
                        },
                        error = function(e) {
                            nFixed[i] <<- NA_real_
                        },
                        warning = function(e) {
                            nFixed[i] <<- NA_real_
                        }
                    )
                }
            }
        }

        return(list(
            alpha = alpha,
            beta = beta,
            sided = sided,
            groups = groups,
            thetaH0 = thetaH0,
            pi1 = pi1,
            normalApproximation = normalApproximation,
            conservative = conservative,
            nFixed = nFixed
        ))
    }

    if (groups == 2) {
        n1Fixed <- rep(NA_real_, length(pi1))
        n2Fixed <- rep(NA_real_, length(pi1))
        nFixed <- rep(NA_real_, length(pi1))
        if (allocationRatioPlanned == 0) {
            allocationRatioPlannedVec <- rep(NA_real_, length(pi1))
        }

        for (i in seq_len(length(pi1))) {
            if (!riskRatio) {
                effect <- pi1[i] - pi2 - thetaH0

                # allocationRatioPlanned = 0 provides optimum sample size
                if (allocationRatioPlanned == 0) {
                    allocationRatioPlannedVec[i] <- stats::optimize(
                        function(x) {
                            fm <- .getFarringtonManningValues(
                                rate1 = pi1[i],
                                rate2 = pi2,
                                theta = thetaH0,
                                allocation = x,
                                method = "diff"
                            )
                            n1 <- (.getOneMinusQNorm(alpha / sided) *
                                sqrt(fm$ml1 * (1 - fm$ml1) + fm$ml2 * (1 - fm$ml2) * x) +
                                .getOneMinusQNorm(beta) * sqrt(pi1[i] * (1 - pi1[i]) + pi2 * (1 - pi2) * x))^2 /
                                effect^2
                            return((1 + x) / x * n1)
                        },
                        interval = c(0, 5),
                        tol = 0.0001
                    )$minimum
                    fm <- .getFarringtonManningValues(
                        rate1 = pi1[i],
                        rate2 = pi2,
                        theta = thetaH0,
                        allocation = allocationRatioPlannedVec[i],
                        method = "diff"
                    )
                    n1Fixed[i] <- (.getOneMinusQNorm(alpha / sided) *
                        sqrt(fm$ml1 * (1 - fm$ml1) + fm$ml2 * (1 - fm$ml2) * allocationRatioPlannedVec[i]) +
                        .getOneMinusQNorm(beta) *
                            sqrt(pi1[i] * (1 - pi1[i]) + pi2 * (1 - pi2) * allocationRatioPlannedVec[i]))^2 /
                        effect^2
                } else {
                    fm <- .getFarringtonManningValues(
                        rate1 = pi1[i],
                        rate2 = pi2,
                        theta = thetaH0,
                        allocation = allocationRatioPlanned,
                        method = "diff"
                    )
                    n1Fixed[i] <- (.getOneMinusQNorm(alpha / sided) *
                        sqrt(fm$ml1 * (1 - fm$ml1) + fm$ml2 * (1 - fm$ml2) * allocationRatioPlanned) +
                        .getOneMinusQNorm(beta) *
                            sqrt(pi1[i] * (1 - pi1[i]) + pi2 * (1 - pi2) * allocationRatioPlanned))^2 /
                        effect^2
                }
            } else {
                effect <- pi1[i] / pi2 - thetaH0

                if (allocationRatioPlanned == 0) {
                    # allocationRatioPlanned = 0 provides optimum sample size
                    allocationRatioPlannedVec[i] <- stats::optimize(
                        function(x) {
                            fm <- .getFarringtonManningValues(
                                rate1 = pi1[i],
                                rate2 = pi2,
                                theta = thetaH0,
                                allocation = x,
                                method = "ratio"
                            )
                            n1 <- (.getOneMinusQNorm(alpha / sided) *
                                sqrt(fm$ml1 * (1 - fm$ml1) + fm$ml2 * (1 - fm$ml2) * x * thetaH0^2) +
                                .getOneMinusQNorm(beta) *
                                    sqrt(
                                        pi1[i] *
                                            (1 - pi1[i]) +
                                            pi2 *
                                                (1 - pi2) *
                                                x *
                                                thetaH0^2
                                    ))^2 /
                                (effect * pi2)^2
                            return((1 + x) / x * n1)
                        },
                        interval = c(0, 5),
                        tol = 0.0001
                    )$minimum
                    fm <- .getFarringtonManningValues(
                        rate1 = pi1[i],
                        rate2 = pi2,
                        theta = thetaH0,
                        allocation = allocationRatioPlannedVec[i],
                        method = "ratio"
                    )
                    n1Fixed[i] <- (.getOneMinusQNorm(alpha / sided) *
                        sqrt(fm$ml1 * (1 - fm$ml1) + fm$ml2 * (1 - fm$ml2) * allocationRatioPlannedVec[i] * thetaH0^2) +
                        .getOneMinusQNorm(beta) *
                            sqrt(
                                pi1[i] * (1 - pi1[i]) + pi2 * (1 - pi2) * allocationRatioPlannedVec[i] * thetaH0^2
                            ))^2 /
                        (effect * pi2)^2
                } else {
                    fm <- .getFarringtonManningValues(
                        rate1 = pi1[i],
                        rate2 = pi2,
                        theta = thetaH0,
                        allocation = allocationRatioPlanned,
                        method = "ratio"
                    )
                    n1Fixed[i] <- (.getOneMinusQNorm(alpha / sided) *
                        sqrt(fm$ml1 * (1 - fm$ml1) + fm$ml2 * (1 - fm$ml2) * allocationRatioPlanned * thetaH0^2) +
                        .getOneMinusQNorm(beta) *
                            sqrt(pi1[i] * (1 - pi1[i]) + pi2 * (1 - pi2) * allocationRatioPlanned * thetaH0^2))^2 /
                        (effect * pi2)^2
                }
            }
        }
        if (allocationRatioPlanned == 0) {
            allocationRatioPlanned <- allocationRatioPlannedVec
        }

        n2Fixed <- n1Fixed / allocationRatioPlanned
        nFixed <- n1Fixed + n2Fixed

        return(list(
            alpha = alpha,
            beta = beta,
            sided = sided,
            groups = groups,
            allocationRatioPlanned = allocationRatioPlanned,
            thetaH0 = thetaH0,
            pi1 = pi1,
            pi2 = pi2,
            normalApproximation = normalApproximation,
            riskRatio = riskRatio,
            n1Fixed = n1Fixed,
            n2Fixed = n2Fixed,
            nFixed = nFixed
        ))
    }
}

.getSampleSizeSequentialRates <- function(fixedSampleSize, designCharacteristics) {
    kMax <- designCharacteristics$.design$kMax
    numberOfSubjects <- matrix(NA_real_, kMax, length(fixedSampleSize$pi1))
    numberOfSubjects1 <- matrix(NA_real_, kMax, length(fixedSampleSize$pi1))
    numberOfSubjects2 <- matrix(NA_real_, kMax, length(fixedSampleSize$pi1))
    maxNumberOfSubjects <- rep(NA_real_, length(fixedSampleSize$pi1))
    expectedNumberOfSubjectsH0 <- rep(NA_real_, length(fixedSampleSize$pi1))
    expectedNumberOfSubjectsH01 <- rep(NA_real_, length(fixedSampleSize$pi1))
    expectedNumberOfSubjectsH1 <- rep(NA_real_, length(fixedSampleSize$pi1))

    informationRates <- designCharacteristics$information / designCharacteristics$shift

    for (i in seq_len(length(fixedSampleSize$pi1))) {
        maxNumberOfSubjects[i] <- fixedSampleSize$nFixed[i] *
            designCharacteristics$inflationFactor

        numberOfSubjects[, i] <- maxNumberOfSubjects[i] *
            c(
                informationRates[1],
                (informationRates[2:kMax] - informationRates[1:(kMax - 1)])
            )

        expectedNumberOfSubjectsH0[i] <- designCharacteristics$averageSampleNumber0 *
            fixedSampleSize$nFixed[i]
        expectedNumberOfSubjectsH01[i] <- designCharacteristics$averageSampleNumber01 *
            fixedSampleSize$nFixed[i]
        expectedNumberOfSubjectsH1[i] <- designCharacteristics$averageSampleNumber1 *
            fixedSampleSize$nFixed[i]

        if (fixedSampleSize$groups == 2) {
            if (length(fixedSampleSize$allocationRatioPlanned) > 1) {
                allocationRatioPlanned <- fixedSampleSize$allocationRatioPlanned[i]
            } else {
                allocationRatioPlanned <- fixedSampleSize$allocationRatioPlanned
            }
            numberOfSubjects1[, i] <- numberOfSubjects[, i] * allocationRatioPlanned / (1 + allocationRatioPlanned)
            numberOfSubjects2[, i] <- numberOfSubjects[, i] / (1 + allocationRatioPlanned)
        }
    }

    if (fixedSampleSize$groups == 1) {
        return(list(
            alpha = fixedSampleSize$alpha,
            beta = fixedSampleSize$beta,
            sided = fixedSampleSize$sided,
            groups = fixedSampleSize$groups,
            thetaH0 = fixedSampleSize$thetaH0,
            pi1 = fixedSampleSize$pi1,
            normalApproximation = fixedSampleSize$normalApproximation,
            informationRates = matrix(informationRates, ncol = 1),
            maxNumberOfSubjects = maxNumberOfSubjects,
            numberOfSubjects = .getColumnCumSum(numberOfSubjects),
            expectedNumberOfSubjectsH0 = expectedNumberOfSubjectsH0,
            expectedNumberOfSubjectsH01 = expectedNumberOfSubjectsH01,
            expectedNumberOfSubjectsH1 = expectedNumberOfSubjectsH1,
            rejectPerStage = designCharacteristics$rejectionProbabilities,
            futilityPerStage = designCharacteristics$futilityProbabilities
        ))
    } else {
        return(list(
            alpha = fixedSampleSize$alpha,
            beta = fixedSampleSize$beta,
            sided = fixedSampleSize$sided,
            groups = fixedSampleSize$groups,
            allocationRatioPlanned = fixedSampleSize$allocationRatioPlanned,
            thetaH0 = fixedSampleSize$thetaH0,
            pi1 = fixedSampleSize$pi1,
            pi2 = fixedSampleSize$pi2,
            normalApproximation = fixedSampleSize$normalApproximation,
            riskRatio = fixedSampleSize$riskRatio,
            informationRates = matrix(informationRates, ncol = 1),
            maxNumberOfSubjects = maxNumberOfSubjects,
            numberOfSubjects = .getColumnCumSum(numberOfSubjects),
            numberOfSubjects1 = .getColumnCumSum(numberOfSubjects1),
            numberOfSubjects2 = .getColumnCumSum(numberOfSubjects2),
            expectedNumberOfSubjectsH0 = expectedNumberOfSubjectsH0,
            expectedNumberOfSubjectsH01 = expectedNumberOfSubjectsH01,
            expectedNumberOfSubjectsH1 = expectedNumberOfSubjectsH1,
            rejectPerStage = designCharacteristics$rejectionProbabilities,
            futilityPerStage = designCharacteristics$futilityProbabilities
        ))
    }
}

#
# note that 'maxNumberOfSubjects' is only applicable for 'objectType' = "power"
#
.createDesignPlanRates <- function(
        ...,
        objectType = c("sampleSize", "power"),
        design,
        normalApproximation = TRUE,
        conservative = TRUE,
        riskRatio = FALSE,
        thetaH0 = ifelse(riskRatio, 1, 0),
        pi1 = C_PI_1_SAMPLE_SIZE_DEFAULT,
        pi2 = C_PI_2_DEFAULT,
        directionUpper = NA,
        maxNumberOfSubjects = NA_real_,
        groups = 2,
        allocationRatioPlanned = NA_real_) {
    objectType <- match.arg(objectType)

    .assertIsTrialDesignInverseNormalOrGroupSequentialOrFixed(design)
    .assertIsValidAlphaAndBeta(design$alpha, design$beta)
    .assertIsValidSidedParameter(design$sided)
    .assertIsValidGroupsParameter(groups)
    .assertIsSingleLogical(conservative, "conservative", naAllowed = TRUE)
    if (is.na(conservative)) {
        conservative <- TRUE
    }
    .assertIsSingleLogical(normalApproximation, "normalApproximation")
    .assertIsSingleLogical(riskRatio, "riskRatio")
    directionUpper <- .assertIsValidDirectionUpper(
        directionUpper,
        design,
        objectType = objectType,
        userFunctionCallEnabled = TRUE,
        default = NA
    )

    if (groups == 1) {
        if (!anyNA(pi1) && any(pi1 == thetaH0) && (objectType == "sampleSize")) {
            stopIllegalArgument("any 'pi1' (", .arrayToString(pi1), ") must be != 'thetaH0' (", thetaH0, ")",
                functionName = ".createDesignPlanRates",
                parameter = "pi1", value = pi1,
                relatedParameter = "thetaH0",
                relatedValue = thetaH0
            )
        }

        if (anyNA(pi1) || any(pi1 <= 0) || any(pi1 >= 1)) {
            stopArgumentOutOfBounds("probability 'pi1' (", .arrayToString(pi1), ") is out of bounds (0; 1)",
                functionName = ".createDesignPlanRates",
                parameter = "pi1", value = pi1
            )
        }

        if (thetaH0 >= 1 || thetaH0 <= 0) {
            stopArgumentOutOfBounds("'thetaH0' (", thetaH0, ") is out of bounds (0; 1)",
                functionName = ".createDesignPlanRates",
                parameter = "thetaH0", value = thetaH0
            )
        }

        if (!normalApproximation && design$sided == 2 && (objectType == "sampleSize")) {
            stopIllegalArgument("exact sample size calculation not available for two-sided testing",
                functionName = ".createDesignPlanRates"
            )
        }

        if (normalApproximation && !conservative && (objectType == "sampleSize")) {
            stopIllegalArgument("'conservative' (", conservative, ") has no effect on sample size calculation",
                functionName = ".createDesignPlanRates",
                parameter = "conservative", value = conservative
            )
        }
    } else if (groups == 2) {
        if (
            !anyNA(c(pi1, pi2)) &&
                any(abs(pi1 - pi2 - thetaH0) < 1e-12) &&
                (objectType == "sampleSize") &&
                !riskRatio
            ) {
            stopIllegalArgument("any 'pi1 - pi2' (", .arrayToString(pi1 - pi2), ") ",
                "must be != 'thetaH0' (", thetaH0, ")",
                functionName = ".createDesignPlanRates",
                parameter = "pi1 - pi2", value = pi1 - pi2,
                relatedParameter = "thetaH0",
                relatedValue = thetaH0
            )
        }

        if (
            !anyNA(c(pi1, pi2)) &&
                any(abs(pi1 / pi2 - thetaH0) < 1e-12) &&
                (objectType == "sampleSize") &&
                riskRatio
            ) {
            stopIllegalArgument("any 'pi1 / pi2' (", .arrayToString(pi1 / pi2), ") ",
                "must be != 'thetaH0' (", thetaH0, ")",
                functionName = ".createDesignPlanRates",
                parameter = "pi1 / pi2", value = pi1 / pi2,
                relatedParameter = "thetaH0",
                relatedValue = thetaH0
            )
        }

        if (anyNA(pi1) || any(pi1 <= 0) || any(pi1 >= 1)) {
            stopArgumentOutOfBounds("probability 'pi1' (", .arrayToString(pi1), ") ",
                "is out of bounds (0; 1)",
                functionName = ".createDesignPlanRates",
                parameter = "pi1", value = pi1
            )
        }

        if (anyNA(pi2) || any(pi2 <= 0) || any(pi2 >= 1)) {
            stopArgumentOutOfBounds("probability 'pi2' (", .arrayToString(pi2), ") ",
                "is out of bounds (0; 1)",
                functionName = ".createDesignPlanRates",
                parameter = "pi2", value = pi2
            )
        }

        if (
            design$sided == 2 &&
                ((thetaH0 != 0 && !riskRatio) ||
                    (thetaH0 != 1 && riskRatio))
            ) {
            stopIllegalArgument("two-sided case ",
                "is implemented only for superiority testing",
                functionName = ".createDesignPlanRates"
            )
        }

        if (!normalApproximation) {
            stopIllegalArgument("only normal approximation case is implemented for two groups",
                functionName = ".createDesignPlanRates"
            )
        }

        if (!conservative) {
            stopIllegalArgument("'conservative' (", conservative, ") ",
                "has no effect on sample size calculation for two groups",
                functionName = ".createDesignPlanRates",
                parameter = "conservative", value = conservative
            )
        }

        if (is.na(allocationRatioPlanned)) {
            allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
        }

        if (allocationRatioPlanned < 0) {
            stopIllegalArgument("'allocationRatioPlanned' (", allocationRatioPlanned, ") must be >= 0",
                functionName = ".createDesignPlanRates",
                parameter = "allocationRatioPlanned", value = allocationRatioPlanned
            )
        }

        if (riskRatio && thetaH0 <= 0) {
            stopIllegalArgument("null hypothesis risk ratio is not allowed be negative ",
                "or zero, ", "i.e., 'thetaH0' must be > 0 if 'riskRatio' = TRUE",
                functionName = ".createDesignPlanRates",
                parameter = "thetaH0",
                relatedParameter = "riskRatio", value = thetaH0
            )
        }
    }

    designPlan <- TrialDesignPlanRates$new(design = design)
    designPlan$.setObjectType(objectType)

    designPlan$criticalValuesPValueScale <- matrix(design$stageLevels, ncol = 1)
    if (design$sided == 2) {
        designPlan$criticalValuesPValueScale <- designPlan$criticalValuesPValueScale * 2
    }
    designPlan$.setParameterType("criticalValuesPValueScale", C_PARAM_NOT_APPLICABLE)

    if (.hasApplicableFutilityBounds(design)) {
        designPlan$futilityBoundsPValueScale <-
            matrix(1 - stats::pnorm(.getFutilityBounds(design)), ncol = 1)
        designPlan$.setParameterType("futilityBoundsPValueScale", C_PARAM_GENERATED)
    }

    .setValueAndParameterType(designPlan, "directionUpper", directionUpper, C_DIRECTION_UPPER_DEFAULT)

    if (objectType == "power") {
        .assertIsValidMaxNumberOfSubjects(maxNumberOfSubjects)
        .setValueAndParameterType(designPlan, "maxNumberOfSubjects", maxNumberOfSubjects, NA_real_)

        designPlan$.setParameterType("effect", C_PARAM_DERIVED)
    }

    .setValueAndParameterType(designPlan, "normalApproximation", normalApproximation, TRUE)
    .setValueAndParameterType(designPlan, "conservative", conservative, TRUE)
    if (groups == 2) {
        designPlan$.setParameterType("conservative", C_PARAM_NOT_APPLICABLE)
    }
    .setValueAndParameterType(designPlan, "thetaH0", thetaH0, ifelse(riskRatio, 1, 0))
    .assertIsValidThetaH0(thetaH0, endpoint = "rates", groups = groups, ratioEnabled = riskRatio)
    if (objectType == "power") {
        .setValueAndParameterType(designPlan, "pi1", pi1, C_PI_1_DEFAULT)
    } else {
        .setValueAndParameterType(designPlan, "pi1", pi1, C_PI_1_SAMPLE_SIZE_DEFAULT)
    }
    .setValueAndParameterType(designPlan, "pi2", pi2, 0.2, notApplicableIfNA = TRUE)
    if (groups == 1) {
        if (designPlan$isUserDefinedParameter("pi2")) {
            warning(
                "'pi2' (",
                pi2,
                ") will be ignored ",
                "because it is not applicable for 'groups' = 1",
                call. = FALSE
            )
        }
        designPlan$.setParameterType("pi2", C_PARAM_NOT_APPLICABLE)

        if (isTRUE(riskRatio)) {
            warning(
                "'riskRatio' (",
                riskRatio,
                ") will be ignored ",
                "because it is not applicable for 'groups' = 1",
                call. = FALSE
            )
        }
        designPlan$.setParameterType("riskRatio", C_PARAM_NOT_APPLICABLE)

        if (length(allocationRatioPlanned) == 1 && !is.na(allocationRatioPlanned)) {
            warning(
                "'allocationRatioPlanned' (",
                allocationRatioPlanned,
                ") will be ignored because it is not applicable for 'groups' = 1",
                call. = FALSE
            )
        }
        designPlan$.setParameterType("allocationRatioPlanned", C_PARAM_NOT_APPLICABLE)
    } else {
        .setValueAndParameterType(designPlan, "riskRatio", riskRatio, FALSE)
        .setValueAndParameterType(
            designPlan,
            "allocationRatioPlanned",
            allocationRatioPlanned,
            C_ALLOCATION_RATIO_DEFAULT
        )
    }
    .setValueAndParameterType(designPlan, "groups", groups, 2)

    return(designPlan)
}

#' @title
#' Get Power Rates
#'
#' @description
#' Returns the power, stopping probabilities, and expected sample size for testing rates
#' in one or two samples at given maximum sample size.
#'
#' @inheritParams param_design_with_default
#' @inheritParams param_groups
#' @param riskRatio If \code{TRUE}, the power for one-sided
#'        testing of H0: \code{pi1 / pi2 = thetaH0} is calculated, default is \code{FALSE}.
#' @inheritParams param_thetaH0
#' @inheritParams param_pi1_rates
#' @inheritParams param_pi2_rates
#' @inheritParams param_directionUpper
#' @inheritParams param_maxNumberOfSubjects
#' @inheritParams param_allocationRatioPlanned
#' @inheritParams param_three_dots
#'
#' @details
#' At given design the function calculates the power, stopping probabilities, and expected sample size
#' for testing rates at given maximum sample size.
#' The sample sizes over the stages are calculated according to the specified information rate in the design.
#' In a two treatment groups design, additionally, an allocation ratio = \code{n1 / n2} can be specified
#' where \code{n1} and \code{n2} are the number of subjects in the two treatment groups.
#' If a null hypothesis value thetaH0 != 0 for testing the difference of two rates
#' or \code{thetaH0 != 1} for testing the risk ratio is specified, the
#' formulas according to Farrington & Manning (Statistics in Medicine, 1990) are used (only one-sided testing).
#' Critical bounds and stopping for futility bounds are provided at the effect scale
#' (rate, rate difference, or rate ratio, respectively).
#' For the two-sample case, the calculation here is performed at fixed pi2 as given as argument in the function.
#' Note that the power calculation for rates is always based on the normal approximation.
#'
#' @template return_object_trial_design_plan
#' @template how_to_get_help_for_generics
#'
#' @family power functions
#'
#' @template examples_get_power_rates
#'
#' @export
#'
getPowerRates <- function(
        design = NULL,
        ...,
        groups = 2L,
        riskRatio = FALSE,
        thetaH0 = ifelse(riskRatio, 1, 0),
        pi1 = seq(0.2, 0.5, 0.1),
        pi2 = 0.2,
        directionUpper = NA,
        maxNumberOfSubjects = NA_real_,
        allocationRatioPlanned = NA_real_) {
    if (is.null(design)) {
        design <- .getDefaultDesign(directionUpper = directionUpper, type = "power", ...)
        .warnInCaseOfUnknownArguments(
            functionName = "getPowerRates",
            ignore = .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = TRUE
            ),
            ...
        )
    } else {
        .warnInCaseOfUnknownArguments(functionName = "getPowerRates", ...)
        .assertIsTrialDesign(design)
        .warnInCaseOfTwoSidedPowerArgument(...)
        .warnInCaseOfTwoSidedPowerIsDisabled(design)
        design <- .resetPipeOperatorQueue(design)
    }

    designPlan <- .createDesignPlanRates(
        objectType = "power",
        design = design,
        riskRatio = riskRatio,
        thetaH0 = thetaH0,
        pi1 = pi1,
        pi2 = pi2,
        directionUpper = directionUpper,
        maxNumberOfSubjects = maxNumberOfSubjects,
        groups = groups,
        allocationRatioPlanned = allocationRatioPlanned,
        ...
    )

    if (!is.na(allocationRatioPlanned) && allocationRatioPlanned <= 0) {
        stopIllegalArgument("allocation ratio must be > 0",
            functionName = "getPowerRates"
        )
    }

    allocationRatioPlanned <- designPlan$allocationRatioPlanned

    theta <- rep(NA_real_, length(pi1))
    if (groups == 1) {
        designPlan$effect <- pi1 - thetaH0
        theta <- (pi1 - thetaH0) /
            sqrt(pi1 * (1 - pi1)) +
            sign(pi1 - thetaH0) *
                .getOneMinusQNorm(design$alpha / design$sided) *
                (1 - sqrt(thetaH0 * (1 - thetaH0) / (pi1 * (1 - pi1)))) /
                sqrt(maxNumberOfSubjects)
    } else {
        if (!riskRatio) {
            designPlan$effect <- pi1 - pi2 - thetaH0
            for (i in seq_len(length(pi1))) {
                fm <- .getFarringtonManningValues(
                    rate1 = pi1[i],
                    rate2 = pi2,
                    theta = thetaH0,
                    allocation = allocationRatioPlanned,
                    method = "diff"
                )
                theta[i] <- sqrt(allocationRatioPlanned) /
                    (1 + allocationRatioPlanned) *
                    (pi1[i] - pi2 - thetaH0) *
                    sqrt(1 + allocationRatioPlanned) /
                    sqrt(pi1[i] * (1 - pi1[i]) + allocationRatioPlanned * pi2 * (1 - pi2)) +
                    sign(pi1[i] - pi2 - thetaH0) *
                        .getOneMinusQNorm(design$alpha / design$sided) *
                        (1 -
                            sqrt(fm$ml1 * (1 - fm$ml1) + allocationRatioPlanned * fm$ml2 * (1 - fm$ml2)) /
                                sqrt(pi1[i] * (1 - pi1[i]) + allocationRatioPlanned * pi2 * (1 - pi2))) /
                        sqrt(maxNumberOfSubjects)
            }
        } else {
            designPlan$effect <- pi1 / pi2 - thetaH0
            for (i in seq_len(length(pi1))) {
                fm <- .getFarringtonManningValues(
                    rate1 = pi1[i],
                    rate2 = pi2,
                    theta = thetaH0,
                    allocation = allocationRatioPlanned,
                    method = "ratio"
                )
                theta[i] <- sqrt(allocationRatioPlanned) /
                    (1 + allocationRatioPlanned) *
                    (pi1[i] - thetaH0 * pi2) *
                    sqrt(1 + allocationRatioPlanned) /
                    sqrt(pi1[i] * (1 - pi1[i]) + allocationRatioPlanned * thetaH0^2 * pi2 * (1 - pi2)) +
                    sign(pi1[i] - thetaH0 * pi2) *
                        .getOneMinusQNorm(design$alpha / design$sided) *
                        (1 -
                            sqrt(fm$ml1 * (1 - fm$ml1) + allocationRatioPlanned * thetaH0^2 * fm$ml2 * (1 - fm$ml2)) /
                                sqrt(
                                    pi1[i] *
                                        (1 - pi1[i]) +
                                        allocationRatioPlanned *
                                            thetaH0^2 *
                                            pi2 *
                                            (1 - pi2)
                                )) /
                        sqrt(maxNumberOfSubjects)
            }
        }
    }

    theta <- .applyDirectionOfAlternative(theta, designPlan$directionUpper,
        type = "negateIfLower", phase = "planning"
    )

    powerAndAverageSampleNumber <- getPowerAndAverageSampleNumber(
        design,
        theta,
        maxNumberOfSubjects
    )

    designPlan$expectedNumberOfSubjects <- powerAndAverageSampleNumber$averageSampleNumber
    designPlan$overallReject <- powerAndAverageSampleNumber$overallReject
    designPlan$rejectPerStage <- powerAndAverageSampleNumber$rejectPerStage
    designPlan$futilityStop <- powerAndAverageSampleNumber$overallFutility
    designPlan$futilityPerStage <- powerAndAverageSampleNumber$futilityPerStage
    designPlan$earlyStop <- powerAndAverageSampleNumber$overallEarlyStop

    parameterNames <- c("overallReject")
    if (design$kMax > 1) {
        parameterNames <- c(
            parameterNames,
            "expectedNumberOfSubjects",
            "rejectPerStage",
            "futilityStop",
            "futilityPerStage",
            "earlyStop"
        )
    }
    for (parameterName in parameterNames) {
        designPlan$.setParameterType(parameterName, C_PARAM_GENERATED)
    }

    .addNumberOfSubjectsToPowerResult(designPlan)
    .addEffectScaleBoundaryDataToDesignPlan(designPlan)
    .hideFutilityStopsIfNotApplicable(designPlan)

    return(designPlan)
}

#' @title
#' Get Sample Size Rates
#'
#' @description
#' Returns the sample size for testing rates in one or two samples.
#'
#' @inheritParams param_design_with_default
#' @inheritParams param_groups
#' @param normalApproximation If \code{FALSE}, the sample size
#'        for the case of one treatment group is calculated exactly using the binomial distribution,
#'        default is \code{TRUE}.
#' @param conservative For the case of one treatment group and \code{normalApproximation = FALSE}, if
#'        \code{TRUE}, the sample size is calculated such that for larger sample size
#'        than the calculated, the power is larger than 1 - beta, for \code{conservative = FALSE}, the minimum
#'        sample size, for which power exceeds 1 - beta is calculated, default is \code{TRUE}.
#' @param riskRatio If \code{TRUE}, the sample size for one-sided
#'        testing of H0: \code{pi1 / pi2 = thetaH0} is calculated, default is \code{FALSE}.
#' @inheritParams param_thetaH0
#' @inheritParams param_pi1_rates
#' @inheritParams param_pi2_rates
#' @inheritParams param_directionUpper
#' @inheritParams param_allocationRatioPlanned_sampleSize
#' @inheritParams param_three_dots
#'
#' @details
#' At given design the function calculates the stage-wise and maximum sample size for testing rates.
#' In a two treatment groups design, additionally, an allocation ratio = \code{n1 / n2} can be specified
#' where \code{n1} and \code{n2} are the number of subjects in the two treatment groups.
#' If a null hypothesis value thetaH0 != 0 for testing the difference of two rates or
#' thetaH0 != 1 for testing the risk ratio is specified, the sample size
#' formula according to Farrington & Manning (Statistics in Medicine, 1990) is used.
#' Critical bounds and stopping for futility bounds are provided at the effect scale
#' (rate, rate difference, or rate ratio, respectively) for each sample size calculation separately.
#' For the two-sample case, the calculation here is performed at fixed pi2 as given as argument
#' in the function.
#'
#' @template return_object_trial_design_plan
#' @template how_to_get_help_for_generics
#'
#' @family sample size functions
#'
#' @template examples_get_sample_size_rates
#'
#' @export
#'
getSampleSizeRates <- function(
        design = NULL,
        ...,
        groups = 2L,
        normalApproximation = TRUE,
        conservative = TRUE,
        riskRatio = FALSE,
        thetaH0 = ifelse(riskRatio, 1, 0),
        pi1 = c(0.4, 0.5, 0.6),
        pi2 = 0.2,
        directionUpper = NA,
        allocationRatioPlanned = NA_real_) {
    if (is.null(design)) {
        design <- .getDefaultDesign(directionUpper = directionUpper, type = "sampleSize", ...)
        .warnInCaseOfUnknownArguments(
            functionName = "getSampleSizeRates",
            ignore = .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(
                design,
                powerCalculationEnabled = FALSE
            ),
            ...
        )
    } else {
        .assertIsTrialDesign(design)
        .warnInCaseOfUnknownArguments(functionName = "getSampleSizeRates", ...)
        .warnInCaseOfTwoSidedPowerArgument(...)
        design <- .resetPipeOperatorQueue(design)
    }

    designPlan <- .createDesignPlanRates(
        objectType = "sampleSize",
        design = design,
        normalApproximation = normalApproximation,
        conservative = conservative,
        riskRatio = riskRatio,
        thetaH0 = thetaH0,
        pi1 = pi1,
        pi2 = pi2,
        groups = groups,
        directionUpper = directionUpper,
        allocationRatioPlanned = allocationRatioPlanned,
        ...
    )

    return(.calculateSampleSizeMeansAndRates(designPlan))
}
