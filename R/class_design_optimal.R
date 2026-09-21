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
#' @details Create objects with [getDesignOptimalConditionalErrorFunction()].
#' This class inherits from `ParameterSet`, not `TrialDesign`: its second-stage
#' information depends on the interim result. Functions accepting conventional
#' group sequential or combination-test designs cannot use this object.
#'
#' @include class_core_parameter_set.R
#'
#' @keywords internal
#'
#' @seealso [getDesignOptimalConditionalErrorFunction()]
#'
TrialDesignOptimalConditionalError <- R6::R6Class(
    "TrialDesignOptimalConditionalError",
    inherit = ParameterSet,
    public = list(
        alpha = NULL,
        alpha1 = NULL,
        alpha0 = NULL,
        conditionalPower = NULL,
        conditionalPowerFunction = NULL,
        delta1 = NULL,
        delta1Min = NULL,
        delta1Max = NULL,
        ncp1 = NULL,
        ncp1Min = NULL,
        ncp1Max = NULL,
        firstStageInformation = NULL,
        useInterimEstimate = NULL,
        likelihoodRatioDistribution = NULL,
        deltaLR = NULL,
        weightsDeltaLR = NULL,
        tauLR = NULL,
        kappaLR = NULL,
        deltaMaxLR = NULL,
        levelConstant = NULL,
        monotonisationConstants = NULL,
        minimumSecondStageInformation = NULL,
        maximumSecondStageInformation = NULL,
        minimumConditionalError = NULL,
        maximumConditionalError = NULL,
        levelConstantMinimum = NULL,
        levelConstantMaximum = NULL,
        enforceMonotonicity = NULL,
        initialize = function(
                alpha = NA_real_,
                alpha1 = NA_real_,
                alpha0 = NA_real_,
                conditionalPower = NA_real_,
                conditionalPowerFunction = NULL,
                delta1 = NA_real_,
                delta1Min = NA_real_,
                delta1Max = NA_real_,
                firstStageInformation = NA_real_,
                useInterimEstimate = TRUE,
                likelihoodRatioDistribution = "",
                deltaLR = NA_real_,
                weightsDeltaLR = NA_real_,
                tauLR = NA_real_,
                kappaLR = NA_real_,
                deltaMaxLR = NA_real_,
                minimumSecondStageInformation = 0,
                maximumSecondStageInformation = Inf,
                minimumConditionalError = 0,
                maximumConditionalError = 1,
                levelConstantMinimum = 0,
                levelConstantMaximum = 10,
                enforceMonotonicity = TRUE,
                ...) {
            super$initialize()
            .assertIsSingleLogical(useInterimEstimate, "useInterimEstimate")
            .assertIsSingleLogical(enforceMonotonicity, "enforceMonotonicity")
            for (parameterName in c("conditionalPower", "delta1", "delta1Min", "delta1Max")) {
                .assertIsSingleNumber(get(parameterName), parameterName, naAllowed = TRUE)
            }
            .warnInCaseOfUnknownArguments(
                functionName = "getDesignOptimalConditionalErrorFunction",
                ..., ignore = c("ncp1", "ncp1Min", "ncp1Max")
            )
            # Range assertions for alpha, alpha1, alpha0
            # General range assertions
            .assertIsSingleNumber(x = alpha, argumentName = "alpha")
            .assertIsSingleNumber(x = alpha1, argumentName = "alpha1")
            .assertIsSingleNumber(x = alpha0, argumentName = "alpha0")

            .assertIsInOpenInterval(x = alpha, xName = "alpha", lower = 0, upper = 1)
            .assertIsInClosedInterval(x = alpha1, xName = "alpha1", lower = 0, upper = 1)
            .assertIsInClosedInterval(x = alpha0, xName = "alpha0", lower = 0, upper = 1)

            # Context-related range assertions
            .assertIsInClosedInterval(x = alpha1, xName = "alpha1", lower = 0, upper = alpha)
            .assertIsInClosedInterval(x = alpha0, xName = "alpha0", lower = alpha1, upper = 1)

            if (!is.na(conditionalPower)) {
                .assertIsInOpenInterval(conditionalPower, "conditionalPower", lower = 0, upper = 1)
                if (!is.null(conditionalPowerFunction) && !identical(conditionalPowerFunction, NA)) {
                    warnArgumentIgnored("Both conditionalPower and conditionalPowerFunction are provided. Using conditionalPower.",
                        call. = FALSE,
                        parameter = "conditionalPowerFunction",
                        relatedParameter = "conditionalPower",
                        relatedValue = conditionalPower,
                        reason = "When both are supplied, conditionalPower takes precedence.",
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
                        functionName = "getDesignOptimalConditionalErrorFunction"
                    )
                }
                self$conditionalPowerFunction <- conditionalPowerFunction
                pValueGrid <- seq(alpha1, alpha0, length.out = 50)
                conditionalPowerValues <- .getOptimalConditionalPower(pValueGrid, self)
                if (any(diff(conditionalPowerValues) > 0)) {
                    warnInvalidInput("Conditional power function should not be increasing in the first-stage p-value.",
                        call. = FALSE,
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
            self$alpha1 <- alpha1
            self$alpha0 <- alpha0
            self$conditionalPower <- conditionalPower
            self$firstStageInformation <- firstStageInformation
            self$likelihoodRatioDistribution <- likelihoodRatioDistribution
            self$useInterimEstimate <- useInterimEstimate

            .assertIsSingleNumber(x = levelConstantMinimum, argumentName = "levelConstantMinimum")
            .assertIsSingleNumber(x = levelConstantMaximum, argumentName = "levelConstantMaximum")

            if (!is.finite(levelConstantMinimum) || !is.finite(levelConstantMaximum) ||
                    levelConstantMinimum >= levelConstantMaximum) {
                stopConflictingArguments(
                    "levelConstantMinimum must be smaller than levelConstantMaximum.",
                    parameter = "levelConstantMinimum",
                    value = levelConstantMinimum,
                    constraint = "finite bounds with levelConstantMinimum < levelConstantMaximum",
                    relatedParameter = "levelConstantMaximum",
                    relatedValue = levelConstantMaximum,
                    functionName = "getDesignOptimalConditionalErrorFunction"
                )
            }

            self$levelConstantMinimum <- levelConstantMinimum
            self$levelConstantMaximum <- levelConstantMaximum

            # Derive effect sizes for conditional power
            # When using an interim estimate, derive minimal or maximal effects
            if (useInterimEstimate) {
                # Neither lower limit provided -> error
                # Extract hidden arguments from ...
                ncp1Min <- list(...)$ncp1Min
                ncp1Max <- list(...)$ncp1Max
                if (is.null(ncp1Max)) ncp1Max <- Inf

                if (is.na(delta1Min) && is.null(ncp1Min)) {
                    stopMissingArgument(
                        "Must provide a lower limit for the interim estimate by using delta1Min.",
                        parameter = "delta1Min",
                        value = delta1Min,
                        constraint = "provide delta1Min or ncp1Min when useInterimEstimate is TRUE",
                        relatedParameter = "ncp1Min",
                        relatedValue = ncp1Min,
                        functionName = "getDesignOptimalConditionalErrorFunction"
                    )
                } else if (!is.na(delta1Min)) {
                    .assertIsSingleNumber(x = delta1Min, argumentName = "delta1Min")
                    .assertIsInOpenInterval(x = delta1Min, xName = "delta1Min", lower = 0, upper = Inf)

                    .assertIsSingleNumber(x = delta1Max, argumentName = "delta1Max")
                    .assertIsInClosedInterval(x = delta1Max, xName = "delta1Max", lower = delta1Min, upper = Inf)

                    self$delta1Min <- delta1Min
                    self$delta1Max <- delta1Max

                    if (!is.null(ncp1Min)) {
                        warnArgumentIgnored("Both ncp1Min and delta1Min are provided. Using delta1Min and ignoring ncp1Min.",
                            parameter = "ncp1Min",
                            value = ncp1Min,
                            relatedParameter = "delta1Min",
                            relatedValue = delta1Min,
                            userInstructions = paste0(
                                "Supply delta1Min or ncp1Min, not both; remove delta1Min if the noncentrality ",
                                "bound is intended."
                            )
                        )
                    }

                    self$ncp1Min <- delta1Min * sqrt(firstStageInformation)
                    self$ncp1Max <- delta1Max * sqrt(firstStageInformation)
                } else if (!is.null(ncp1Min)) {
                    .assertIsSingleNumber(x = ncp1Min, argumentName = "ncp1Min")
                    .assertIsInOpenInterval(x = ncp1Min, xName = "ncp1Min", lower = 0, upper = Inf)

                    .assertIsSingleNumber(x = ncp1Max, argumentName = "ncp1Max")
                    .assertIsInClosedInterval(x = ncp1Max, xName = "ncp1Max", lower = ncp1Min, upper = Inf)

                    self$ncp1Min <- ncp1Min
                    self$ncp1Max <- ifelse(is.null(ncp1Max), Inf, ncp1Max)

                    self$delta1Min <- ncp1Min / sqrt(firstStageInformation)
                    self$delta1Max <- ifelse(ncp1Max == Inf, Inf, ncp1Max / sqrt(firstStageInformation))
                } else {
                    stopRuntimeIssue(
                        "Unexpected error occurred during determination of restrictions for interim estimate.",
                        parameter = c("delta1Min", "delta1Max"),
                        value = list(delta1Min = delta1Min, delta1Max = delta1Max),
                        constraint = "interim estimate restrictions must be derivable",
                        relatedParameter = c("ncp1Min", "ncp1Max"),
                        relatedValue = list(ncp1Min = ncp1Min, ncp1Max = ncp1Max),
                        functionName = "getDesignOptimalConditionalErrorFunction"
                    )
                }
            } else {
                # When not using an interim estimate, derive fixed effects
                # If non-centrality parameter was not specified, calculate it from delta1

                # Extract hidden argument from ...
                ncp1 <- list(...)$ncp1

                if (!is.na(delta1)) {
                    .assertIsSingleNumber(x = delta1, argumentName = "delta1")
                    .assertIsInOpenInterval(x = delta1, xName = "delta1", lower = 0, upper = Inf)

                    self$delta1 <- delta1
                    if (!is.null(ncp1)) {
                        warnArgumentIgnored("Both delta1 and ncp1 are provided. Using delta1 and ignoring ncp1.",
                            parameter = "ncp1",
                            value = ncp1,
                            relatedParameter = "delta1",
                            relatedValue = delta1,
                            userInstructions = paste0(
                                "Supply delta1 or ncp1, not both; remove delta1 if the noncentrality parameter ",
                                "is intended."
                            )
                        )
                    }
                    self$ncp1 <- delta1 * sqrt(firstStageInformation)
                } else if (!is.null(ncp1)) {
                    # If delta1 was not specified, calculate it from ncp1
                    .assertIsSingleNumber(x = ncp1, argumentName = "ncp1")
                    .assertIsInOpenInterval(x = ncp1, xName = "ncp1", lower = 0, upper = Inf)

                    self$ncp1 <- ncp1
                    self$delta1 <- ncp1 / sqrt(firstStageInformation)
                } else {
                    # Else, none of ncp1 and delta1 were specified
                    stopMissingArgument(
                        "Must specify delta1 when using a fixed effect for conditional power.",
                        parameter = "delta1",
                        value = delta1,
                        constraint = "provide delta1 or ncp1 when useInterimEstimate is FALSE",
                        relatedParameter = "ncp1",
                        relatedValue = ncp1,
                        functionName = "getDesignOptimalConditionalErrorFunction"
                    )
                }
            }

            # Range assertions for constraints
            # General range assertions
            .assertIsSingleNumber(x = minimumConditionalError, argumentName = "minimumConditionalError")
            .assertIsInClosedInterval(
                x = minimumConditionalError,
                xName = "minimumConditionalError",
                lower = 0,
                upper = 1
            )
            .assertIsSingleNumber(x = maximumConditionalError, argumentName = "maximumConditionalError")
            .assertIsInClosedInterval(
                x = maximumConditionalError,
                xName = "maximumConditionalError",
                lower = 0,
                upper = 1
            )

            .assertIsSingleNumber(x = minimumSecondStageInformation, argumentName = "minimumSecondStageInformation")
            .assertIsInClosedInterval(
                x = minimumSecondStageInformation,
                xName = "minimumSecondStageInformation",
                lower = 0,
                upper = Inf
            )
            .assertIsSingleNumber(x = maximumSecondStageInformation, argumentName = "maximumSecondStageInformation")
            .assertIsInClosedInterval(
                x = maximumSecondStageInformation,
                xName = "maximumSecondStageInformation",
                lower = 0,
                upper = Inf
            )

            if (maximumSecondStageInformation == 0) {
                stopArgumentOutOfRange(
                    "Maximum second-stage information must be larger than 0.",
                    parameter = "maximumSecondStageInformation",
                    value = maximumSecondStageInformation,
                    constraint = "must be greater than zero",
                    lowerBound = 0,
                    upperBound = Inf,
                    functionName = "getDesignOptimalConditionalErrorFunction"
                )
            }

            # Context-related range assertions
            .assertIsInClosedInterval(
                x = minimumConditionalError,
                xName = "minimumConditionalError",
                lower = 0,
                upper = maximumConditionalError
            )

            .assertIsInClosedInterval(
                x = minimumSecondStageInformation,
                xName = "minimumSecondStageInformation",
                lower = 0,
                upper = maximumSecondStageInformation
            )

            # Identify constraints for minimum conditional error / maximum second-stage information
            self$minimumConditionalError <- minimumConditionalError
            self$maximumSecondStageInformation <- maximumSecondStageInformation

            # Identify constraints for maximum conditional error / minimum second-stage information
            self$maximumConditionalError <- maximumConditionalError
            self$minimumSecondStageInformation <- minimumSecondStageInformation

            .assertIsSingleLogical(x = enforceMonotonicity, argumentName = "enforceMonotonicity")
            self$enforceMonotonicity <- enforceMonotonicity

            .assertIsSingleCharacter(x = likelihoodRatioDistribution, argumentName = "likelihoodRatioDistribution")

            for (parameterName in c("deltaLR", "tauLR", "kappaLR", "deltaMaxLR")) {
                value <- get(parameterName)
                if (is.numeric(value) && any(is.infinite(value))) {
                    stopIllegalArgument(
                        "'", parameterName, "' must contain finite values.",
                        parameter = parameterName,
                        value = value,
                        constraint = "must contain finite values",
                        functionName = "getDesignOptimalConditionalErrorFunction"
                    )
                }
            }
            # Identify specific distribution parameters
            if (likelihoodRatioDistribution == "fixed") {
                if (any(is.na(deltaLR))) {
                    stopMissingArgument(
                        "Must provide deltaLR for fixed effect in likelihood ratio.",
                        parameter = "deltaLR",
                        value = deltaLR,
                        constraint = "required for the selected likelihood ratio distribution",
                        relatedParameter = "likelihoodRatioDistribution",
                        relatedValue = likelihoodRatioDistribution,
                        functionName = "getDesignOptimalConditionalErrorFunction"
                    )
                } else {
                    .assertIsNumericVector(x = deltaLR, argumentName = "deltaLR")
                    self$deltaLR <- deltaLR
                    # If any of the weights are NA, use equal weights
                    if (any(is.na(weightsDeltaLR))) {
                        self$weightsDeltaLR <- rep(1 / length(deltaLR), length(deltaLR))
                        # For multiple effects, tell the user that equal weights are used.
                        if (length(deltaLR) > 1) {
                            message(
                                "At least one entry in weightsDeltaLR is NA. Using equal weights for effects in fixed likelihood ratio."
                            )
                        }
                    } else {
                        .assertIsNumericVector(x = weightsDeltaLR, argumentName = "weightsDeltaLR")
                        .assertIsInClosedInterval(x = weightsDeltaLR, xName = "weightsDeltaLR", lower = 0, upper = 1)
                        # Check if weightsDeltaLR and deltaLR are of equal length
                        if (length(weightsDeltaLR) != length(deltaLR)) {
                            stopArgumentLengthOutOfBounds(
                                "Must provide exactly one weight in weightsDeltaLR per entry of deltaLR.",
                                parameter = "weightsDeltaLR",
                                value = weightsDeltaLR,
                                constraint = "one weight per deltaLR entry",
                                relatedParameter = "deltaLR",
                                relatedValue = deltaLR,
                                expectedLength = length(deltaLR),
                                actualLength = length(weightsDeltaLR),
                                functionName = "getDesignOptimalConditionalErrorFunction"
                            )
                        }
                        # Verify that weightsDeltaLR sums to 1
                        if (abs(sum(weightsDeltaLR) - 1) > sqrt(.Machine$double.eps)) {
                            stopIllegalArgument(
                                "Weights in weightsDeltaLR must sum to 1.",
                                parameter = "weightsDeltaLR",
                                value = weightsDeltaLR,
                                constraint = "weights must sum to one within sqrt(.Machine$double.eps)",
                                functionName = "getDesignOptimalConditionalErrorFunction"
                            )
                        }
                        self$weightsDeltaLR <- weightsDeltaLR / sum(weightsDeltaLR)
                    }
                }
            } else if (likelihoodRatioDistribution == "normal") {
                .assertIsSingleNumber(deltaLR, "deltaLR", naAllowed = TRUE)
                .assertIsSingleNumber(tauLR, "tauLR", naAllowed = TRUE)
                if (is.na(deltaLR) || is.na(tauLR)) {
                    stopMissingArgument(
                        "Must provide deltaLR and tauLR for normal prior in likelihood ratio.",
                        parameter = c("deltaLR", "tauLR"),
                        value = list(deltaLR = deltaLR, tauLR = tauLR),
                        constraint = "required for the selected likelihood ratio distribution",
                        relatedParameter = "likelihoodRatioDistribution",
                        relatedValue = likelihoodRatioDistribution,
                        functionName = "getDesignOptimalConditionalErrorFunction"
                    )
                } else {
                    .assertIsSingleNumber(x = deltaLR, argumentName = "deltaLR")
                    self$deltaLR <- deltaLR

                    .assertIsSingleNumber(x = tauLR, argumentName = "tauLR")
                    .assertIsInOpenInterval(x = tauLR, xName = "tauLR", lower = 0, upper = Inf)

                    self$tauLR <- tauLR
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
                        functionName = "getDesignOptimalConditionalErrorFunction"
                    )
                } else {
                    .assertIsSingleNumber(x = kappaLR, argumentName = "kappaLR")
                    .assertIsInOpenInterval(x = kappaLR, xName = "kappaLR", lower = 0, upper = Inf)
                    self$kappaLR <- kappaLR
                }
            } else if (likelihoodRatioDistribution == "unif") {
                .assertIsSingleNumber(deltaMaxLR, "deltaMaxLR", naAllowed = TRUE)
                if (is.na(deltaMaxLR)) {
                    stopMissingArgument(
                        "Must provide deltaMaxLR for uniform prior in likelihood ratio.",
                        parameter = "deltaMaxLR",
                        value = deltaMaxLR,
                        constraint = "required for the selected likelihood ratio distribution",
                        relatedParameter = "likelihoodRatioDistribution",
                        relatedValue = likelihoodRatioDistribution,
                        functionName = "getDesignOptimalConditionalErrorFunction"
                    )
                } else {
                    .assertIsSingleNumber(x = deltaMaxLR, argumentName = "deltaMaxLR")
                    .assertIsInOpenInterval(x = deltaMaxLR, xName = "deltaMaxLR", lower = 0, upper = Inf)
                    self$deltaMaxLR <- deltaMaxLR
                }
            } else if (likelihoodRatioDistribution == "maxlr") {} else {
                stopIllegalArgument(
                    "Distribution not matched. likelihoodRatioDistribution should be one of 'fixed', 'normal', 'exp', 'unif' or 'maxlr'.",
                    parameter = "likelihoodRatioDistribution",
                    value = likelihoodRatioDistribution,
                    constraint = "one of fixed, normal, exp, unif or maxlr",
                    functionName = "getDesignOptimalConditionalErrorFunction"
                )
            }

            if (is.function(self$conditionalPowerFunction) && useInterimEstimate &&
                    (minimumSecondStageInformation > 0 || maximumSecondStageInformation < Inf)) {
                warnNotValidated("Conditional power functions with interim estimates and information constraints may be non-monotone.",
                    call. = FALSE,
                    userInstructions = paste0(
                        "Inspect monotonicity of the resulting conditional power function when combining ",
                        "interim estimates and information constraints."
                    )
                )
            }

            # Calculate monotonisation constants
            self$monotonisationConstants <- .getMonotonisationConstants(
                fun = .getQ,
                lower = alpha1,
                upper = alpha0,
                argument = "firstStagePValue",
                design = self
            )

            # Calculate level constant
            self$levelConstant <- .getLevelConstant(
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
            for (parameterName in c("ncp1", "ncp1Min", "ncp1Max")) {
                if (!is.null(self[[parameterName]])) self$.setParameterType(parameterName, C_PARAM_DERIVED)
            }
            if (!useInterimEstimate && is.na(delta1)) {
                self$.setParameterType("ncp1", C_PARAM_USER_DEFINED)
                self$.setParameterType("delta1", C_PARAM_DERIVED)
            } else if (useInterimEstimate && is.na(delta1Min)) {
                self$.setParameterType("ncp1Min", C_PARAM_USER_DEFINED)
                self$.setParameterType("delta1Min", C_PARAM_DERIVED)
                self$.setParameterType("ncp1Max", if (is.null(list(...)$ncp1Max)) {
                    C_PARAM_DEFAULT_VALUE
                } else {
                    C_PARAM_USER_DEFINED
                })
                self$.setParameterType("delta1Max", C_PARAM_DERIVED)
            }
            if (likelihoodRatioDistribution == "fixed" && any(is.na(weightsDeltaLR))) {
                self$.setParameterType("weightsDeltaLR", C_PARAM_DEFAULT_VALUE)
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
                .showOptimalConditionalErrorDesign(self, consoleOutputEnabled = consoleOutputEnabled)
            }
        }
    )
)
