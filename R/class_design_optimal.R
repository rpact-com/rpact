## |
## |  *Optimal conditional error design class*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Original contribution: Morten Dreher
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text: https://www.r-project.org/Licenses/LGPL-3
## |

#'
#' Optimal Conditional Error Design
#'
#' @description R6 parameter set for an adaptive two-stage design based on an
#' optimal conditional error function.
#'
#' @details Create objects with [getDesignOptimalConditionalError()].
#' This class inherits from `ParameterSet`, not `TrialDesign`: its second-stage
#' information depends on the interim result. Functions accepting conventional
#' group sequential or combination-test designs cannot use this object.
#'
#' @include class_core_parameter_set.R
#'
#' @keywords internal
#'
#' @seealso [getDesignOptimalConditionalError()]
#'
TrialDesignOptimalConditionalError <- R6::R6Class(
    "TrialDesignOptimalConditionalError",
    inherit = ParameterSet,
    public = list(
        alpha = NULL,
        efficacyBounds = NULL,
        futilityBounds = NULL,
        efficacyBoundsScale = NULL,
        futilityBoundsScale = NULL,
        conditionalPower = NULL,
        conditionalPowerFunction = NULL,
        thetaH1 = NULL,
        minThetaH1 = NULL,
        maxThetaH1 = NULL,
        nonCentralityParameterH1 = NULL,
        minNonCentralityParameterH1 = NULL,
        maxNonCentralityParameterH1 = NULL,
        firstStageInformation = NULL,
        useInterimEstimate = NULL,
        likelihoodRatioDistribution = NULL,
        thetaLR = NULL,
        weightsLR = NULL,
        stDevLR = NULL,
        kappaLR = NULL,
        maxThetaLR = NULL,
        levelConstant = NULL,
        monotonisationConstants = NULL,
        minInformationPerStage = NULL,
        maxInformationPerStage = NULL,
        minConditionalError = NULL,
        maxConditionalError = NULL,
        minLevelConstant = NULL,
        maxLevelConstant = NULL,
        enforceMonotonicity = NULL,
        initialize = function(
                alpha = NA_real_,
                efficacyBounds = NA_real_,
                futilityBounds = NA_real_,
                conditionalPower = NA_real_,
                conditionalPowerFunction = NULL,
                thetaH1 = NA_real_,
                minThetaH1 = NA_real_,
                maxThetaH1 = NA_real_,
                firstStageInformation = NA_real_,
                useInterimEstimate = TRUE,
                likelihoodRatioDistribution = "",
                thetaLR = NA_real_,
                weightsLR = NA_real_,
                stDevLR = NA_real_,
                kappaLR = NA_real_,
                maxThetaLR = NA_real_,
                minInformationPerStage = 0,
                maxInformationPerStage = Inf,
                minConditionalError = 0,
                maxConditionalError = 1,
                minLevelConstant = 0,
                maxLevelConstant = 10,
                enforceMonotonicity = TRUE,
                efficacyBoundsScale = "pValue",
                futilityBoundsScale = "pValue",
                nonCentralityParameterH1 = NULL,
                minNonCentralityParameterH1 = NULL,
                maxNonCentralityParameterH1 = Inf,
                ...) {
            super$initialize()
            .assertIsSingleLogical(useInterimEstimate, "useInterimEstimate")
            .assertIsSingleLogical(enforceMonotonicity, "enforceMonotonicity")
            for (parameterName in c("conditionalPower", "thetaH1", "minThetaH1", "maxThetaH1")) {
                .assertIsSingleNumber(get(parameterName), parameterName, naAllowed = TRUE)
            }
            .warnInCaseOfUnknownArguments(
                functionName = "getDesignOptimalConditionalError",
                ...
            )
            for (parameterName in c("efficacyBoundsScale", "futilityBoundsScale")) {
                value <- get(parameterName)
                .assertIsSingleCharacter(value, parameterName)
                if (value != "pValue") {
                    stopIllegalArgument(
                        "'", parameterName, "' must be 'pValue' for optimal conditional error designs.",
                        parameter = parameterName, value = value, constraint = "must be pValue",
                        functionName = "getDesignOptimalConditionalError"
                    )
                }
                self[[parameterName]] <- value
            }
            # Range assertions for alpha, efficacyBounds, futilityBounds
            # General range assertions
            .assertIsSingleNumber(x = alpha, argumentName = "alpha")
            .assertIsSingleNumber(x = efficacyBounds, argumentName = "efficacyBounds")
            .assertIsSingleNumber(x = futilityBounds, argumentName = "futilityBounds")

            .assertIsInOpenInterval(x = alpha, xName = "alpha", lower = 0, upper = 1)
            .assertIsInClosedInterval(x = efficacyBounds, xName = "efficacyBounds", lower = 0, upper = 1)
            .assertIsInClosedInterval(x = futilityBounds, xName = "futilityBounds", lower = 0, upper = 1)

            # Context-related range assertions
            .assertIsInClosedInterval(x = efficacyBounds, xName = "efficacyBounds", lower = 0, upper = alpha)
            .assertIsInClosedInterval(x = futilityBounds, xName = "futilityBounds", lower = efficacyBounds, upper = 1)

            if (!is.na(conditionalPower)) {
                .assertIsInOpenInterval(conditionalPower, "conditionalPower", lower = 0, upper = 1)
                if (!is.null(conditionalPowerFunction) && !identical(conditionalPowerFunction, NA)) {
                    warnArgumentIgnored(
                        "Both 'conditionalPower' and 'conditionalPowerFunction' are provided. ",
                        "Using 'conditionalPower' and ignoring 'conditionalPowerFunction'.",
                        parameter = "conditionalPowerFunction",
                        relatedParameter = "conditionalPower",
                        relatedValue = conditionalPower,
                        reason = "When both are supplied, 'conditionalPower' takes precedence.",
                        userInstructions = paste0(
                            "Choose either conditionalPower or conditionalPowerFunction; remove ",
                            "conditionalPower if the function should determine conditional power."
                        )
                    )
                }
            } else {
                if (!is.function(conditionalPowerFunction)) {
                    stopMissingArgument(
                        "Specify 'conditionalPower' or a valid 'conditionalPowerFunction'.",
                        parameter = "conditionalPower",
                        value = conditionalPower,
                        constraint = "provide conditionalPower or a conditionalPowerFunction",
                        relatedParameter = "conditionalPowerFunction",
                        relatedValue = conditionalPowerFunction,
                        functionName = "getDesignOptimalConditionalError",
                        reason = paste0(
                            "The optimal conditional error design requires a conditional power target or a ",
                            "function defining that target."
                        ),
                        userInstructions = paste0(
                            "Specify conditionalPower or a valid conditionalPowerFunction consistent with the ",
                            "intended adaptation rule."
                        )
                    )
                }
                self$conditionalPowerFunction <- conditionalPowerFunction
                pValueGrid <- seq(efficacyBounds, futilityBounds, length.out = 50)
                conditionalPowerValues <- .getOptimalDesignConditionalPowerTarget(pValueGrid, self)
                if (any(diff(conditionalPowerValues) > 0)) {
                    warnInvalidInput("Conditional power function should not be increasing in the first-stage p-value.",
                        userInstructions = paste0(
                            "Supply a conditionalPowerFunction that does not increase with the first-stage ",
                            "p-value."
                        )
                    )
                }
            }

            .assertIsSingleNumber(x = firstStageInformation, argumentName = "firstStageInformation")
            .assertIsInOpenInterval(x = firstStageInformation, xName = "firstStageInformation", lower = 0, upper = Inf)
            .assertIsSingleLogical(x = useInterimEstimate, argumentName = "useInterimEstimate")

            # Set initial parameters
            self$alpha <- alpha
            self$efficacyBounds <- efficacyBounds
            self$futilityBounds <- futilityBounds
            self$conditionalPower <- conditionalPower
            self$firstStageInformation <- firstStageInformation
            self$likelihoodRatioDistribution <- likelihoodRatioDistribution
            self$useInterimEstimate <- useInterimEstimate

            .assertIsSingleNumber(x = minLevelConstant, argumentName = "minLevelConstant")
            .assertIsSingleNumber(x = maxLevelConstant, argumentName = "maxLevelConstant")

            if (!is.finite(minLevelConstant) || !is.finite(maxLevelConstant) ||
                    minLevelConstant >= maxLevelConstant) {
                stopConflictingArguments(
                    "minLevelConstant must be smaller than maxLevelConstant.",
                    parameter = "minLevelConstant",
                    value = minLevelConstant,
                    constraint = "finite bounds with minLevelConstant < maxLevelConstant",
                    relatedParameter = "maxLevelConstant",
                    relatedValue = maxLevelConstant,
                    functionName = "getDesignOptimalConditionalError"
                )
            }

            self$minLevelConstant <- minLevelConstant
            self$maxLevelConstant <- maxLevelConstant

            # Derive effect sizes for conditional power
            # When using an interim estimate, derive minimal or maximal effects
            if (useInterimEstimate) {
                # Neither lower limit provided -> error
                if (is.null(maxNonCentralityParameterH1)) maxNonCentralityParameterH1 <- Inf

                if (is.na(minThetaH1) && is.null(minNonCentralityParameterH1)) {
                    stopMissingArgument(
                        "Must provide a lower limit for the interim estimate by using minThetaH1.",
                        parameter = "minThetaH1",
                        value = minThetaH1,
                        constraint = "provide minThetaH1 or minNonCentralityParameterH1 when useInterimEstimate is TRUE",
                        relatedParameter = "minNonCentralityParameterH1",
                        relatedValue = minNonCentralityParameterH1,
                        functionName = "getDesignOptimalConditionalError"
                    )
                } else if (!is.na(minThetaH1)) {
                    .assertIsSingleNumber(x = minThetaH1, argumentName = "minThetaH1")
                    .assertIsInOpenInterval(x = minThetaH1, xName = "minThetaH1", lower = 0, upper = Inf)

                    .assertIsSingleNumber(x = maxThetaH1, argumentName = "maxThetaH1")
                    .assertIsInClosedInterval(x = maxThetaH1, xName = "maxThetaH1", lower = minThetaH1, upper = Inf)

                    self$minThetaH1 <- minThetaH1
                    self$maxThetaH1 <- maxThetaH1

                    if (!is.null(minNonCentralityParameterH1)) {
                        warnArgumentIgnored(
                            "Both 'minNonCentralityParameterH1' and 'minThetaH1' are provided. Using 'minThetaH1' and ignoring 'minNonCentralityParameterH1'.",
                            parameter = "minNonCentralityParameterH1",
                            value = minNonCentralityParameterH1,
                            relatedParameter = "minThetaH1",
                            relatedValue = minThetaH1,
                            userInstructions = paste0(
                                "Supply minThetaH1 or minNonCentralityParameterH1, not both; remove minThetaH1 if the noncentrality ",
                                "bound is intended."
                            )
                        )
                    }

                    self$minNonCentralityParameterH1 <- minThetaH1 * sqrt(firstStageInformation)
                    self$maxNonCentralityParameterH1 <- maxThetaH1 * sqrt(firstStageInformation)
                } else if (!is.null(minNonCentralityParameterH1)) {
                    .assertIsSingleNumber(x = minNonCentralityParameterH1, argumentName = "minNonCentralityParameterH1")
                    .assertIsInOpenInterval(x = minNonCentralityParameterH1, xName = "minNonCentralityParameterH1", lower = 0, upper = Inf)

                    .assertIsSingleNumber(x = maxNonCentralityParameterH1, argumentName = "maxNonCentralityParameterH1")
                    .assertIsInClosedInterval(x = maxNonCentralityParameterH1, xName = "maxNonCentralityParameterH1", lower = minNonCentralityParameterH1, upper = Inf)

                    self$minNonCentralityParameterH1 <- minNonCentralityParameterH1
                    self$maxNonCentralityParameterH1 <- ifelse(is.null(maxNonCentralityParameterH1), Inf, maxNonCentralityParameterH1)

                    self$minThetaH1 <- minNonCentralityParameterH1 / sqrt(firstStageInformation)
                    self$maxThetaH1 <- ifelse(maxNonCentralityParameterH1 == Inf, Inf, maxNonCentralityParameterH1 / sqrt(firstStageInformation))
                } else {
                    stopRuntimeIssue(
                        "Unexpected error occurred during determination of restrictions for interim estimate.",
                        parameter = c("minThetaH1", "maxThetaH1"),
                        value = list(minThetaH1 = minThetaH1, maxThetaH1 = maxThetaH1),
                        constraint = "interim estimate restrictions must be derivable",
                        relatedParameter = c("minNonCentralityParameterH1", "maxNonCentralityParameterH1"),
                        relatedValue = list(minNonCentralityParameterH1 = minNonCentralityParameterH1, maxNonCentralityParameterH1 = maxNonCentralityParameterH1),
                        functionName = "getDesignOptimalConditionalError"
                    )
                }
            } else {
                # When not using an interim estimate, derive fixed effects
                # If non-centrality parameter was not specified, calculate it from thetaH1


                if (!is.na(thetaH1)) {
                    .assertIsSingleNumber(x = thetaH1, argumentName = "thetaH1")
                    .assertIsInOpenInterval(x = thetaH1, xName = "thetaH1", lower = 0, upper = Inf)

                    self$thetaH1 <- thetaH1
                    if (!is.null(nonCentralityParameterH1)) {
                        warnArgumentIgnored(
                            "Both 'thetaH1' and 'nonCentralityParameterH1' are provided. Using 'thetaH1' and ignoring 'nonCentralityParameterH1'.",
                            parameter = "nonCentralityParameterH1",
                            value = nonCentralityParameterH1,
                            relatedParameter = "thetaH1",
                            relatedValue = thetaH1,
                            userInstructions = paste0(
                                "Supply thetaH1 or nonCentralityParameterH1, not both; remove thetaH1 if the noncentrality parameter ",
                                "is intended."
                            )
                        )
                    }
                    self$nonCentralityParameterH1 <- thetaH1 * sqrt(firstStageInformation)
                } else if (!is.null(nonCentralityParameterH1)) {
                    # If thetaH1 was not specified, calculate it from nonCentralityParameterH1
                    .assertIsSingleNumber(x = nonCentralityParameterH1, argumentName = "nonCentralityParameterH1")
                    .assertIsInOpenInterval(x = nonCentralityParameterH1, xName = "nonCentralityParameterH1", lower = 0, upper = Inf)

                    self$nonCentralityParameterH1 <- nonCentralityParameterH1
                    self$thetaH1 <- nonCentralityParameterH1 / sqrt(firstStageInformation)
                } else {
                    # Else, none of nonCentralityParameterH1 and thetaH1 were specified
                    stopMissingArgument(
                        "Must specify thetaH1 when using a fixed effect for conditional power.",
                        parameter = "thetaH1",
                        value = thetaH1,
                        constraint = "provide thetaH1 or nonCentralityParameterH1 when useInterimEstimate is FALSE",
                        relatedParameter = "nonCentralityParameterH1",
                        relatedValue = nonCentralityParameterH1,
                        functionName = "getDesignOptimalConditionalError",
                        reason = "A fixed effect for conditional power needs an explicit interim-effect assumption.",
                        userInstructions = paste0(
                            "Supply thetaH1 for the intended fixed effect, or choose the conditional power ",
                            "specification that matches the intended adaptation rule."
                        )
                    )
                }
            }

            # Range assertions for constraints
            # General range assertions
            .assertIsSingleNumber(x = minConditionalError, argumentName = "minConditionalError")
            .assertIsInClosedInterval(
                x = minConditionalError,
                xName = "minConditionalError",
                lower = 0,
                upper = 1
            )
            .assertIsSingleNumber(x = maxConditionalError, argumentName = "maxConditionalError")
            .assertIsInClosedInterval(
                x = maxConditionalError,
                xName = "maxConditionalError",
                lower = 0,
                upper = 1
            )

            .assertIsSingleNumber(x = minInformationPerStage, argumentName = "minInformationPerStage")
            .assertIsInClosedInterval(
                x = minInformationPerStage,
                xName = "minInformationPerStage",
                lower = 0,
                upper = Inf
            )
            .assertIsSingleNumber(x = maxInformationPerStage, argumentName = "maxInformationPerStage")
            .assertIsInClosedInterval(
                x = maxInformationPerStage,
                xName = "maxInformationPerStage",
                lower = 0,
                upper = Inf
            )

            if (maxInformationPerStage == 0) {
                stopArgumentOutOfRange(
                    "Maximum second-stage information must be larger than 0.",
                    parameter = "maxInformationPerStage",
                    value = maxInformationPerStage,
                    constraint = "must be greater than zero",
                    lowerBound = 0,
                    upperBound = Inf,
                    functionName = "getDesignOptimalConditionalError"
                )
            }

            # Context-related range assertions
            .assertIsInClosedInterval(
                x = minConditionalError,
                xName = "minConditionalError",
                lower = 0,
                upper = maxConditionalError
            )

            .assertIsInClosedInterval(
                x = minInformationPerStage,
                xName = "minInformationPerStage",
                lower = 0,
                upper = maxInformationPerStage
            )

            # Identify constraints for minimum conditional error / maximum second-stage information
            self$minConditionalError <- minConditionalError
            self$maxInformationPerStage <- maxInformationPerStage

            # Identify constraints for maximum conditional error / minimum second-stage information
            self$maxConditionalError <- maxConditionalError
            self$minInformationPerStage <- minInformationPerStage

            .assertIsSingleLogical(x = enforceMonotonicity, argumentName = "enforceMonotonicity")
            self$enforceMonotonicity <- enforceMonotonicity

            .assertIsSingleCharacter(x = likelihoodRatioDistribution, argumentName = "likelihoodRatioDistribution")

            for (parameterName in c("thetaLR", "stDevLR", "kappaLR", "maxThetaLR")) {
                value <- get(parameterName)
                if (is.numeric(value) && any(is.infinite(value))) {
                    stopIllegalArgument(
                        "'", parameterName, "' must contain finite values.",
                        parameter = parameterName,
                        value = value,
                        constraint = "must contain finite values",
                        functionName = "getDesignOptimalConditionalError"
                    )
                }
            }
            # Identify specific distribution parameters
            if (likelihoodRatioDistribution == "fixed") {
                if (any(is.na(thetaLR))) {
                    stopMissingArgument(
                        "Must provide thetaLR for fixed effect in likelihood ratio.",
                        parameter = "thetaLR",
                        value = thetaLR,
                        constraint = "required for the selected likelihood ratio distribution",
                        relatedParameter = "likelihoodRatioDistribution",
                        relatedValue = likelihoodRatioDistribution,
                        functionName = "getDesignOptimalConditionalError",
                        reason = paste0(
                            "The fixed likelihood-ratio specification requires its effect value or support ",
                            "points."
                        ),
                        userInstructions = paste0(
                            "Supply thetaLR for the intended fixed-effect specification; if another prior was ",
                            "intended, correct likelihoodRatioDistribution instead."
                        )
                    )
                } else {
                    .assertIsNumericVector(x = thetaLR, argumentName = "thetaLR")
                    self$thetaLR <- thetaLR
                    # If any of the weights are NA, use equal weights
                    if (any(is.na(weightsLR))) {
                        self$weightsLR <- rep(1 / length(thetaLR), length(thetaLR))
                        # For multiple effects, tell the user that equal weights are used.
                        if (length(thetaLR) > 1) {
                            message(
                                "At least one entry in weightsLR is NA. Using equal weights for effects in fixed likelihood ratio."
                            )
                        }
                    } else {
                        .assertIsNumericVector(x = weightsLR, argumentName = "weightsLR")
                        .assertIsInClosedInterval(x = weightsLR, xName = "weightsLR", lower = 0, upper = 1)
                        # Check if weightsLR and thetaLR are of equal length
                        if (length(weightsLR) != length(thetaLR)) {
                            stopArgumentLengthOutOfBounds(
                                "Must provide exactly one weight in weightsLR per entry of thetaLR.",
                                parameter = "weightsLR",
                                value = weightsLR,
                                constraint = "one weight per thetaLR entry",
                                relatedParameter = "thetaLR",
                                relatedValue = thetaLR,
                                expectedLength = length(thetaLR),
                                actualLength = length(weightsLR),
                                functionName = "getDesignOptimalConditionalError"
                            )
                        }
                        # Verify that weightsLR sums to 1
                        if (abs(sum(weightsLR) - 1) > sqrt(.Machine$double.eps)) {
                            stopIllegalArgument(
                                "Weights in weightsLR must sum to 1.",
                                parameter = "weightsLR",
                                value = weightsLR,
                                constraint = "weights must sum to one within sqrt(.Machine$double.eps)",
                                functionName = "getDesignOptimalConditionalError",
                                reason = paste0(
                                    "The fixed likelihood-ratio mixture needs valid probability weights for its ",
                                    "support points."
                                ),
                                userInstructions = paste0(
                                    "Specify one finite nonnegative weightsLR value per thetaLR value, ",
                                    "summing to 1; choose weights that represent the intended mixture."
                                )
                            )
                        }
                        self$weightsLR <- weightsLR / sum(weightsLR)
                    }
                }
            } else if (likelihoodRatioDistribution == "normal") {
                .assertIsSingleNumber(thetaLR, "thetaLR", naAllowed = TRUE)
                .assertIsSingleNumber(stDevLR, "stDevLR", naAllowed = TRUE)
                if (is.na(thetaLR) || is.na(stDevLR)) {
                    stopMissingArgument(
                        "Must provide thetaLR and stDevLR for normal prior in likelihood ratio.",
                        parameter = c("thetaLR", "stDevLR"),
                        value = list(thetaLR = thetaLR, stDevLR = stDevLR),
                        constraint = "required for the selected likelihood ratio distribution",
                        relatedParameter = "likelihoodRatioDistribution",
                        relatedValue = likelihoodRatioDistribution,
                        functionName = "getDesignOptimalConditionalError",
                        reason = "The normal likelihood-ratio prior requires both its location and scale.",
                        userInstructions = paste0(
                            "Specify thetaLR and stDevLR for the intended normal prior, or select the intended ",
                            "likelihoodRatioDistribution."
                        )
                    )
                } else {
                    .assertIsSingleNumber(x = thetaLR, argumentName = "thetaLR")
                    self$thetaLR <- thetaLR

                    .assertIsSingleNumber(x = stDevLR, argumentName = "stDevLR")
                    .assertIsInOpenInterval(x = stDevLR, xName = "stDevLR", lower = 0, upper = Inf)

                    self$stDevLR <- stDevLR
                }
            } else if (likelihoodRatioDistribution == "exp") {
                .assertIsSingleNumber(kappaLR, "kappaLR", naAllowed = TRUE)
                if (is.na(kappaLR)) {
                    stopMissingArgument(
                        "Must provide kappaLR for exponential prior in likelihood ratio.",
                        parameter = "kappaLR",
                        value = kappaLR,
                        constraint = "required for the selected likelihood ratio distribution",
                        relatedParameter = "likelihoodRatioDistribution",
                        relatedValue = likelihoodRatioDistribution,
                        functionName = "getDesignOptimalConditionalError",
                        reason = "The exponential likelihood-ratio prior requires its kappaLR parameter.",
                        userInstructions = paste0(
                            "Supply a valid kappaLR for the intended exponential prior, or select the intended ",
                            "likelihoodRatioDistribution."
                        )
                    )
                } else {
                    .assertIsSingleNumber(x = kappaLR, argumentName = "kappaLR")
                    .assertIsInOpenInterval(x = kappaLR, xName = "kappaLR", lower = 0, upper = Inf)
                    self$kappaLR <- kappaLR
                }
            } else if (likelihoodRatioDistribution == "unif") {
                .assertIsSingleNumber(maxThetaLR, "maxThetaLR", naAllowed = TRUE)
                if (is.na(maxThetaLR)) {
                    stopMissingArgument(
                        "Must provide maxThetaLR for uniform prior in likelihood ratio.",
                        parameter = "maxThetaLR",
                        value = maxThetaLR,
                        constraint = "required for the selected likelihood ratio distribution",
                        relatedParameter = "likelihoodRatioDistribution",
                        relatedValue = likelihoodRatioDistribution,
                        functionName = "getDesignOptimalConditionalError",
                        reason = "The uniform likelihood-ratio prior requires its upper effect bound.",
                        userInstructions = paste0(
                            "Supply maxThetaLR for the intended uniform prior, or select the intended ",
                            "likelihoodRatioDistribution."
                        )
                    )
                } else {
                    .assertIsSingleNumber(x = maxThetaLR, argumentName = "maxThetaLR")
                    .assertIsInOpenInterval(x = maxThetaLR, xName = "maxThetaLR", lower = 0, upper = Inf)
                    self$maxThetaLR <- maxThetaLR
                }
            } else if (likelihoodRatioDistribution == "maxlr") {} else {
                stopIllegalArgument(
                    "Distribution not matched. likelihoodRatioDistribution should be one of 'fixed', 'normal', 'exp', 'unif' or 'maxlr'.",
                    parameter = "likelihoodRatioDistribution",
                    value = likelihoodRatioDistribution,
                    constraint = "one of fixed, normal, exp, unif or maxlr",
                    functionName = "getDesignOptimalConditionalError"
                )
            }

            if (is.function(self$conditionalPowerFunction) && useInterimEstimate &&
                    (minInformationPerStage > 0 || maxInformationPerStage < Inf)) {
                warnNotValidated("Conditional power functions with interim estimates and information constraints may be non-monotone.",
                    userInstructions = paste0(
                        "Inspect monotonicity of the resulting conditional power function when combining ",
                        "interim estimates and information constraints."
                    )
                )
            }

            # Calculate monotonisation constants
            self$monotonisationConstants <- .getOptimalDesignMonotonisationConstants(
                fun = .getOptimalDesignQ,
                lower = efficacyBounds,
                upper = futilityBounds,
                argument = "pValue",
                design = self
            )

            # Calculate level constant
            self$levelConstant <- .getOptimalDesignLevelConstant(
                design = self
            )$root
            self$.initParameterTypes()
            for (parameterName in self$.getVisibleFieldNames()) {
                self$.setParameterType(parameterName, if (is.null(self[[parameterName]]) ||
                        (is.numeric(self[[parameterName]]) && all(is.na(self[[parameterName]])))) {
                    C_PARAM_NOT_APPLICABLE
                } else {
                    C_PARAM_USER_DEFINED
                })
            }
            for (parameterName in c("nonCentralityParameterH1", "minNonCentralityParameterH1", "maxNonCentralityParameterH1")) {
                if (!is.null(self[[parameterName]])) self$.setParameterType(parameterName, C_PARAM_DERIVED)
            }
            if (!useInterimEstimate && is.na(thetaH1)) {
                self$.setParameterType("nonCentralityParameterH1", C_PARAM_USER_DEFINED)
                self$.setParameterType("thetaH1", C_PARAM_DERIVED)
            } else if (useInterimEstimate && is.na(minThetaH1)) {
                self$.setParameterType("minNonCentralityParameterH1", C_PARAM_USER_DEFINED)
                self$.setParameterType("minThetaH1", C_PARAM_DERIVED)
                self$.setParameterType("maxNonCentralityParameterH1", if (missing(maxNonCentralityParameterH1)) {
                    C_PARAM_DEFAULT_VALUE
                } else {
                    C_PARAM_USER_DEFINED
                })
                self$.setParameterType("maxThetaH1", C_PARAM_DERIVED)
            }
            if (likelihoodRatioDistribution == "fixed" && any(is.na(weightsLR))) {
                self$.setParameterType("weightsLR", C_PARAM_DEFAULT_VALUE)
            }
            for (parameterName in c("levelConstant", "monotonisationConstants")) {
                self$.setParameterType(parameterName, C_PARAM_GENERATED)
            }
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
            invisible(self)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            self$.resetCat()
            if (showType == 2) {
                super$.show(showType = showType, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                .showOptimalDesign(self, consoleOutputEnabled = consoleOutputEnabled)
            }
        }
    )
)

#' Characteristics of an Optimal Conditional Error Design
#'
#' @description Operating characteristics returned by [getDesignCharacteristics()]
#'   for an optimal conditional error design. Inherits from `ParameterSet`.
#' @details `theta` contains the evaluated effects and `overallReject` the overall
#'   rejection probabilities. `rejectPerStage` has two rows (stages one and two),
#'   and `futilityPerStage` one row (interim futility). Columns correspond to `theta`.
#'   Final non-rejection is not classified as early futility.
#' @include class_core_parameter_set.R
#' @keywords internal
TrialDesignOptimalConditionalErrorCharacteristics <- R6::R6Class(
    "TrialDesignOptimalConditionalErrorCharacteristics",
    inherit = ParameterSet,
    public = list(
        .design = NULL,
        theta = NULL,
        overallReject = NULL,
        rejectPerStage = NULL,
        futilityPerStage = NULL,
        initialize = function(design, theta, overallReject, rejectPerStage, futilityPerStage) {
            super$initialize()
            self$.design <- design
            self$theta <- theta
            self$overallReject <- overallReject
            self$rejectPerStage <- rejectPerStage
            self$futilityPerStage <- futilityPerStage
            rownames(self$rejectPerStage) <- paste("stage =", 1:2)
            rownames(self$futilityPerStage) <- "stage = 1"
            self$.initParameterTypes()
            self$.setParameterType("theta", C_PARAM_USER_DEFINED)
            for (parameterName in c("overallReject", "rejectPerStage", "futilityPerStage")) {
                self$.setParameterType(parameterName, C_PARAM_GENERATED)
            }
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
            invisible(self)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            self$.resetCat()
            if (showType == 2) {
                super$.show(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                self$.showParametersOfOneGroup(self$.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getGeneratedParameters(), "Operating characteristics",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
            }
        }
    )
)
