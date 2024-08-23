## |
## |  *Group sequential design*
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
## |  File version: $Revision: 8123 $
## |  Last changed: $Date: 2024-08-23 08:37:23 +0200 (Fr, 23 Aug 2024) $
## |  Last changed by: $Author: wassmer $
## |

#' @include f_core_constants.R
NULL

.getGroupSequentialProbabilities <- function(decisionMatrix, informationRates) {
    return(.getGroupSequentialProbabilitiesCpp(decisionMatrix, informationRates))
}

#' @title
#' Get Group Sequential Probabilities
#'
#' @description
#' Calculates probabilities in the group sequential setting.
#'
#' @param decisionMatrix A matrix with either 2 or 4 rows and kMax = length(informationRates) columns, see details.
#' @inheritParams param_informationRates
#'
#' @details
#' Given a sequence of information rates (fixing the correlation structure), and
#' decisionMatrix with either 2 or 4 rows and kMax = length(informationRates) columns,
#' this function calculates a probability matrix containing, for two rows, the probabilities:\cr
#' P(Z_1 < l_1), P(l_1 < Z_1 < u_1, Z_2 < l_2),..., P(l_kMax-1 < Z_kMax-1 < u_kMax-1, Z_kMax < l_l_kMax)\cr
#' P(Z_1 < u_1), P(l_1 < Z_1 < u_1, Z_2 < u_2),..., P(l_kMax-1 < Z_kMax-1 < u_kMax-1, Z_kMax < u_l_kMax)\cr
#' P(Z_1 < Inf), P(l_1 < Z_1 < u_1, Z_2 < Inf),..., P(l_kMax-1 < Z_kMax-1 < u_kMax-1, Z_kMax < Inf)\cr
#' with continuation matrix\cr
#' l_1,...,l_kMax\cr
#' u_1,...,u_kMax\cr
#' That is, the output matrix of the function provides per stage (column) the cumulative probabilities 
#' for values specified in decisionMatrix and Inf, and reaching the stage, i.e., the test 
#' statistics is in the continuation region for the preceding stages. 
#' For 4 rows, the continuation region contains of two regions and the probability matrix is
#' obtained analogously (cf., Wassmer and Brannath, 2016).
#'
#' @family design functions
#'
#' @template examples_get_group_sequential_probabilities
#'
#' @return Returns a numeric matrix containing the probabilities described in the details section.
#'
#' @export
#'
getGroupSequentialProbabilities <- function(decisionMatrix, informationRates) {
    .assertAreValidInformationRates(informationRates)
    .assertIsValidDecisionMatrix(decisionMatrix, length(informationRates))

    return(.getGroupSequentialProbabilities(decisionMatrix = decisionMatrix, informationRates = informationRates))
}

.validateGroupSequentialProbabilityResultsMulti <- function(...) {
    args <- list(...)
    for (variableName in names(args)) {
        if (!.validateGroupSequentialProbabilityResults(results = args[[variableName]], variableName)) {
            return(invisible())
        }
    }
}

.validateGroupSequentialProbabilityResults <- function(results, variableName) {
    numberOfNAs <- sum(is.na(results))
    if (numberOfNAs == 0) {
        return(TRUE)
    }

    warning(sprintf(
        paste0(
            C_EXCEPTION_TYPE_RUNTIME_ISSUE,
            "in .getGroupSequentialProbabilities(): ",
            "variable '%s' contains %s NA's (%.1f%%)"
        ),
        variableName, numberOfNAs, 100 * numberOfNAs / length(results)
    ), call. = FALSE)
    return(FALSE)
}

.validateTypeOfDesign <- function(design) {
    .assertDesignParameterExists(design, "typeOfDesign", C_DEFAULT_TYPE_OF_DESIGN)

    design$.setParameterType("userAlphaSpending", C_PARAM_NOT_APPLICABLE)
    design$.setParameterType("userBetaSpending", C_PARAM_NOT_APPLICABLE)
    design$.setParameterType("deltaWT", C_PARAM_NOT_APPLICABLE)
    design$.setParameterType("deltaPT1", C_PARAM_NOT_APPLICABLE)
    design$.setParameterType("deltaPT0", C_PARAM_NOT_APPLICABLE)
    design$.setParameterType("optimizationCriterion", C_PARAM_NOT_APPLICABLE)
    design$.setParameterType("gammaA", C_PARAM_NOT_APPLICABLE)
    design$.setParameterType("gammaB", C_PARAM_NOT_APPLICABLE)
    design$.setParameterType("typeBetaSpending", C_PARAM_NOT_APPLICABLE)
    design$.setParameterType("constantBoundsHP", C_PARAM_NOT_APPLICABLE)

    if (!(design$typeOfDesign %in% .getDesignTypes())) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "type of design (", design$typeOfDesign, ") must be one of the following: ", .printDesignTypes()
        )
    }

    if (design$typeOfDesign == C_TYPE_OF_DESIGN_WT) {
        .assertDesignParameterExists(design, "deltaWT", NA_real_)
        .assertIsSingleNumber(design$deltaWT, "deltaWT", naAllowed = FALSE)
        .showParameterOutOfValidatedBoundsMessage(design$deltaWT, "deltaWT", lowerBound = -0.5, upperBound = 1)
    } else if (design$typeOfDesign == C_TYPE_OF_DESIGN_PT) {
        .assertDesignParameterExists(design, "deltaPT1", NA_real_)
        .assertIsSingleNumber(design$deltaPT1, "deltaPT1", naAllowed = FALSE)
        .showParameterOutOfValidatedBoundsMessage(design$deltaPT1, "deltaPT1", lowerBound = -0.5, upperBound = 1)
        .assertDesignParameterExists(design, "deltaPT0", NA_real_)
        .assertIsSingleNumber(design$deltaPT0, "deltaPT0", naAllowed = FALSE)
        .showParameterOutOfValidatedBoundsMessage(design$deltaPT0, "deltaPT0", lowerBound = -0.5, upperBound = 1)
    } else if (design$typeOfDesign == C_TYPE_OF_DESIGN_WT_OPTIMUM) {
        .assertDesignParameterExists(design, "optimizationCriterion", C_OPTIMIZATION_CRITERION_DEFAULT)
        if (!.isOptimizationCriterion(design$optimizationCriterion)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "optimization criterion must be one of the following: ", .printOptimizationCriterion()
            )
        }
    } else if (design$typeOfDesign == C_TYPE_OF_DESIGN_HP) {
        .assertDesignParameterExists(design, "constantBoundsHP", C_CONST_BOUND_HP_DEFAULT)
        .assertIsSingleNumber(design$constantBoundsHP, "constantBoundsHP")
        .showParameterOutOfValidatedBoundsMessage(design$constantBoundsHP, "constantBoundsHP", lowerBound = 2)
    } else if (design$typeOfDesign == C_TYPE_OF_DESIGN_AS_KD) {
        .assertDesignParameterExists(design, "gammaA", NA_real_)
        .assertIsSingleNumber(design$gammaA, "gammaA", naAllowed = FALSE)
        .showParameterOutOfValidatedBoundsMessage(design$gammaA, "gammaA",
            lowerBound = 0.4, upperBound = 8,
            spendingFunctionName = "Kim & DeMets alpha spending"
        )
    } else if (design$typeOfDesign == C_TYPE_OF_DESIGN_AS_HSD) {
        .assertDesignParameterExists(design, "gammaA", NA_real_)
        .assertIsSingleNumber(design$gammaA, "gammaA", naAllowed = FALSE)
        .showParameterOutOfValidatedBoundsMessage(design$gammaA, "gammaA",
            lowerBound = -10, upperBound = 5,
            spendingFunctionName = "Hwang, Shih & DeCani alpha spending"
        )
    } else if (design$typeOfDesign == C_TYPE_OF_DESIGN_AS_USER) {
        .validateUserAlphaSpending(design)
        design$.setParameterType("userAlphaSpending", C_PARAM_USER_DEFINED)
    }

    if (.isUndefinedArgument(design$alpha)) {
        design$alpha <- C_ALPHA_DEFAULT
        design$.setParameterType("alpha", C_PARAM_DEFAULT_VALUE)
    }

    if (design$typeOfDesign == C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY) {
        .assertIsValidAlpha(design$alpha)
        design$.setParameterType("userAlphaSpending", C_PARAM_DEFAULT_VALUE)
    }

    if ((.isBetaSpendingDesignType(design$typeBetaSpending) || !.isAlphaSpendingDesignType(design$typeOfDesign)) &&
            (design$informationRates[length(design$informationRates)] != 1)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "For specified design, last information rate should be equal 1"
        )
    }

    if (.isAlphaSpendingDesignType(design$typeOfDesign)) {
        .assertDesignParameterExists(design, "typeBetaSpending", C_TYPE_OF_DESIGN_BS_NONE)

        if (!.isBetaSpendingDesignType(design$typeBetaSpending, noneIncluded = TRUE)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "type of beta spending must be one of the following: ", .printBetaSpendingDesignTypes()
            )
        }

        if (design$typeBetaSpending == C_TYPE_OF_DESIGN_BS_KD) {
            .assertDesignParameterExists(design, "gammaB", NA_real_)
            .assertIsSingleNumber(design$gammaB, "gammaB", naAllowed = FALSE)
            .showParameterOutOfValidatedBoundsMessage(design$gammaB, "gammaB",
                lowerBound = 0.4, upperBound = 8,
                spendingFunctionName = "Kim & DeMets beta spending", c(-0.4, 8)
            )
        }

        if (design$typeBetaSpending == C_TYPE_OF_DESIGN_BS_HSD) {
            .assertDesignParameterExists(design, "gammaB", NA_real_)
            .assertIsSingleNumber(design$gammaB, "gammaB", naAllowed = FALSE)
            .showParameterOutOfValidatedBoundsMessage(design$gammaB, "gammaB",
                lowerBound = -10, upperBound = 5,
                spendingFunctionName = "Hwang, Shih & DeCani beta spending"
            )
        }

        if (design$typeBetaSpending == C_TYPE_OF_DESIGN_BS_USER) {
            .validateUserBetaSpending(design)
            design$.setParameterType("userBetaSpending", C_PARAM_USER_DEFINED)
        }
    } else {
        if (.designParameterExists(design, "typeBetaSpending") && design$typeBetaSpending != C_TYPE_OF_DESIGN_BS_NONE) {
            warning("'typeBetaSpending' (", design$typeBetaSpending, ") will be ignored ",
                "because 'typeOfDesign' (", design$typeOfDesign, ") is not an alpha spending design",
                call. = FALSE
            )
            design$typeBetaSpending <- C_TYPE_OF_DESIGN_BS_NONE
            design$.setParameterType("typeBetaSpending", C_PARAM_DEFAULT_VALUE)
        }

        if (.designParameterExists(design, "userBetaSpending")) {
            userBetaSpending <- NA_real_
            warning("'userBetaSpending' (", .arrayToString(design$userBetaSpending), ") will be ignored ",
                "because 'typeOfDesign' (", design$typeOfDesign, ") is not an alpha spending design",
                call. = FALSE
            )
        }
    }

    if (.isUndefinedArgument(design$beta)) {
        design$beta <- C_BETA_DEFAULT
        design$.setParameterType("beta", C_PARAM_DEFAULT_VALUE)
    }

    return(invisible(design))
}

.validateBaseParameters <- function(design, twoSidedWarningForDefaultValues = TRUE) {
    if (.isDefinedArgument(design$kMax)) {
        .assertDesignParameterExists(design, "kMax", C_KMAX_DEFAULT)
        .assertIsValidKMax(design$kMax)

        if (.isDefinedArgument(design$informationRates)) {
            .assertAreValidInformationRates(design$informationRates, design$kMax)
        }

        if (.isDefinedArgument(design$futilityBounds)) {
            .assertAreValidFutilityBounds(design$futilityBounds, design$kMax)
        }
    }

    .assertDesignParameterExists(design, "sided", C_SIDED_DEFAULT)
    .assertIsValidSidedParameter(design$sided)

    .setKmaxBasedOnAlphaSpendingDefintion(design)

    design$informationRates <- .getValidatedInformationRates(design)
    design$futilityBounds <- .getValidatedFutilityBounds(design,
        twoSidedWarningForDefaultValues = twoSidedWarningForDefaultValues
    )

    .assertDesignParameterExists(design, "tolerance", C_DESIGN_TOLERANCE_DEFAULT)
    if (design$tolerance < 1e-10 || design$tolerance > 1e-03) {
        stop(
            C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
            "'tolerance' (", design$tolerance, ") out of bounds [1e-10; 1e-03]"
        )
    }

    return(invisible(design))
}

.createDesign <- function(...,
        designClass,
        kMax = NA_integer_,
        alpha = NA_real_,
        beta = NA_real_,
        sided = C_SIDED_DEFAULT,
        informationRates = NA_real_,
        futilityBounds = NA_real_,
        typeOfDesign = C_DEFAULT_TYPE_OF_DESIGN,
        deltaWT = NA_real_,
        deltaPT1 = NA_real_,
        deltaPT0 = NA_real_,
        optimizationCriterion = C_OPTIMIZATION_CRITERION_DEFAULT,
        gammaA = NA_real_,
        typeBetaSpending = C_TYPE_OF_DESIGN_BS_NONE,
        userAlphaSpending = NA_real_,
        userBetaSpending = NA_real_,
        gammaB = NA_real_,
        bindingFutility = C_BINDING_FUTILITY_DEFAULT,
        constantBoundsHP = C_CONST_BOUND_HP_DEFAULT,
        twoSidedPower = NA,
        betaAdjustment = NA,
        delayedInformation = NA_real_,
        tolerance = C_DESIGN_TOLERANCE_DEFAULT) {
    .assertIsSingleInteger(kMax, "kMax", naAllowed = TRUE, validateType = FALSE)
    .assertIsSingleCharacter(typeOfDesign, "typeOfDesign")

    if (typeOfDesign == C_TYPE_OF_DESIGN_AS_USER && !any(is.na(userAlphaSpending))) {
        if (!is.na(kMax) && kMax != length(userAlphaSpending)) {
            stop(sprintf(
                paste0(
                    C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                    "length of 'userAlphaSpending' (%s) must be equal to 'kMax' (%s)"
                ),
                length(userAlphaSpending), kMax
            ))
        }
        kMax <- length(userAlphaSpending)
        if (kMax > 1 && all(userAlphaSpending[1:(kMax - 1)] == 0)) {
            message("Changed type of design to ", sQuote(C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY))
            typeOfDesign <- C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY
        }
    }

    .assertIsSingleLogical(bindingFutility, "bindingFutility")
    .assertIsNumericVector(delayedInformation, "delayedInformation", naAllowed = TRUE)
    .assertIsInClosedInterval(delayedInformation, "delayedInformation", lower = 0, upper = NULL, naAllowed = TRUE)

    if (designClass == C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL) {
        design <- TrialDesignInverseNormal$new(
            kMax = kMax, bindingFutility = bindingFutility,
            delayedInformation = delayedInformation
        )
    } else if (designClass == C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL) {
        design <- TrialDesignGroupSequential$new(
            kMax = kMax, bindingFutility = bindingFutility,
            delayedInformation = delayedInformation
        )
    } else {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'designClass' ('", designClass, "') must be '", C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL, "' or ",
            "'", C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL, "'"
        )
    }

    .assertIsSingleInteger(sided, "sided", naAllowed = FALSE, validateType = FALSE)
    if (!is.integer(sided) && sided %in% c(1, 2)) {
        sided <- as.integer(sided)
    }

    .assertIsSingleCharacter(optimizationCriterion, "optimizationCriterion")
    .assertIsSingleCharacter(typeBetaSpending, "typeBetaSpending")
    .assertIsSingleLogical(twoSidedPower, "twoSidedPower", naAllowed = TRUE)
    .assertIsSingleLogical(betaAdjustment, "betaAdjustment", naAllowed = TRUE)
    .assertIsSingleNumber(alpha, "alpha", naAllowed = TRUE)
    .assertIsSingleNumber(beta, "beta", naAllowed = TRUE)
    .assertIsSingleNumber(deltaWT, "deltaWT", naAllowed = TRUE)
    .assertIsSingleNumber(deltaPT1, "deltaPT1", naAllowed = TRUE)
    .assertIsSingleNumber(deltaPT0, "deltaPT0", naAllowed = TRUE)
    .assertIsSingleNumber(gammaA, "gammaA", naAllowed = TRUE)
    .assertIsSingleNumber(gammaB, "gammaB", naAllowed = TRUE)
    .assertIsNumericVector(futilityBounds, "futilityBounds", naAllowed = TRUE)
    .assertIsNumericVector(informationRates, "informationRates", naAllowed = TRUE)
    .assertIsNumericVector(userAlphaSpending, "userAlphaSpending", naAllowed = TRUE)
    .assertIsNumericVector(userBetaSpending, "userBetaSpending", naAllowed = TRUE)

    design$alpha <- alpha
    design$beta <- beta
    design$sided <- sided
    design$typeOfDesign <- typeOfDesign
    design$deltaWT <- deltaWT
    design$deltaPT1 <- deltaPT1
    design$deltaPT0 <- deltaPT0
    design$gammaA <- gammaA
    design$gammaB <- gammaB
    design$optimizationCriterion <- optimizationCriterion
    design$typeBetaSpending <- typeBetaSpending
    design$futilityBounds <- futilityBounds
    design$informationRates <- informationRates
    design$userAlphaSpending <- userAlphaSpending
    design$userBetaSpending <- userBetaSpending
    design$bindingFutility <- bindingFutility
    design$delayedInformation <- delayedInformation

    if (!all(is.na(delayedInformation)) && any(delayedInformation > 0)) {
        design$.setParameterType("delayedInformation", C_PARAM_USER_DEFINED)
    }

    if (design$typeOfDesign != C_TYPE_OF_DESIGN_WT_OPTIMUM && optimizationCriterion != C_OPTIMIZATION_CRITERION_DEFAULT) {
        warning(
            "'optimizationCriterion' (", optimizationCriterion,
            ") will be ignored because it is only applicable for 'typeOfDesign' = \"", C_TYPE_OF_DESIGN_WT_OPTIMUM, "\"",
            call. = FALSE
        )
    }
    if (design$typeOfDesign == C_TYPE_OF_DESIGN_HP) {
        .assertIsSingleNumber(constantBoundsHP, "constantBoundsHP")
        .assertIsInClosedInterval(constantBoundsHP, "constantBoundsHP", lower = 2, upper = NULL)
        design$constantBoundsHP <- constantBoundsHP
    } else if (constantBoundsHP != C_CONST_BOUND_HP_DEFAULT) {
        warning(
            "'constantBoundsHP' (", constantBoundsHP,
            ") will be ignored because it is only applicable for 'typeOfDesign' = \"", C_TYPE_OF_DESIGN_HP, "\"",
            call. = FALSE
        )
    }
    if (is.na(twoSidedPower)) {
        design$twoSidedPower <- C_TWO_SIDED_POWER_DEFAULT
        design$.setParameterType("twoSidedPower", C_PARAM_DEFAULT_VALUE)
    } else {
        design$twoSidedPower <- twoSidedPower
        design$.setParameterType("twoSidedPower", ifelse(
            twoSidedPower == C_TWO_SIDED_POWER_DEFAULT, C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
        ))
    }
    if (design$sided == 2 && grepl("^bs", design$typeBetaSpending)) {
        if (is.na(betaAdjustment)) {
            design$betaAdjustment <- TRUE
            design$.setParameterType("betaAdjustment", C_PARAM_DEFAULT_VALUE)
        } else {
            design$betaAdjustment <- betaAdjustment
            design$.setParameterType("betaAdjustment", ifelse(betaAdjustment, C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
        }
    } else if (!is.na(betaAdjustment)) {
        warning(
            "'betaAdjustment' (", betaAdjustment,
            ") will be ignored because it is only applicable for two-sided beta-spending designs",
            call. = FALSE
        )
    }

    design$tolerance <- tolerance

    return(design)
}

.getDesignGroupSequentialKMax1 <- function(design) {
    design$criticalValues <- .getOneMinusQNorm(design$alpha / design$sided)
    design$alphaSpent[1] <- design$alpha
    design$.setParameterType("typeOfDesign", C_PARAM_NOT_APPLICABLE)
    if (!identical(design$typeOfDesign, C_DEFAULT_TYPE_OF_DESIGN)) {
        warning("'typeOfDesign' (", design$typeOfDesign, ") will be ignored because design is fixed", call. = FALSE)
    }
    return(invisible(design))
}

#
# Wang and Tsiatis design
#
.getDesignGroupSequentialWangAndTsiatis <- function(design) {
    if (design$typeOfDesign == C_TYPE_OF_DESIGN_P) {
        design$criticalValues <- .getDesignGroupSequentialPocockCpp(
            design$kMax,
            design$alpha,
            design$sided,
            design$informationRates,
            design$bindingFutility,
            design$futilityBounds,
            design$tolerance
        )
    } else if (design$typeOfDesign == C_TYPE_OF_DESIGN_OF) {
        design$criticalValues <- .getDesignGroupSequentialOBrienAndFlemingCpp(
            design$kMax,
            design$alpha,
            design$sided,
            design$informationRates,
            design$bindingFutility,
            design$futilityBounds,
            design$tolerance
        )
    } else {
        design$criticalValues <- .getDesignGroupSequentialDeltaWTCpp(
            design$kMax,
            design$alpha,
            design$sided,
            design$informationRates,
            design$bindingFutility,
            design$futilityBounds,
            design$tolerance,
            design$deltaWT
        )
    }

    .calculateAlphaSpent(design)
    return(invisible(design))
}

.getDesignGroupSequentialPampallonaTsiatis <- function(design) {
    cppResult <- .getDesignGroupSequentialPampallonaTsiatisCpp(
        design$tolerance, design$beta, design$alpha, design$kMax,
        design$deltaPT0, design$deltaPT1, design$informationRates,
        design$sided, design$bindingFutility
    )
    futilityBounds <- cppResult$futilityBounds
    criticalValues <- cppResult$criticalValues
    probs <- cppResult$probs

    if (design$sided == 1) {
        design$betaSpent <- cumsum(probs[1, ])
        design$power <- cumsum(probs[3, ] - probs[2, ])
    } else {
        design$betaSpent <- cumsum(probs[3, ] - probs[2, ])
        if (design$twoSidedPower) {
            design$power <- cumsum(probs[5, ] - probs[4, ] + probs[1, ])
        } else {
            design$power <- cumsum(probs[5, ] - probs[4, ])
        }
    }
    design$.setParameterType("betaSpent", C_PARAM_GENERATED)
    design$.setParameterType("power", C_PARAM_GENERATED)

    design$futilityBounds <- futilityBounds[1:(design$kMax - 1)]
    design$criticalValues <- criticalValues
    design$.setParameterType("futilityBounds", C_PARAM_GENERATED)
    design$.setParameterType("criticalValues", C_PARAM_GENERATED)

    .calculateAlphaSpent(design)

    design$futilityBounds[design$futilityBounds == 0] <- NA_real_

    .assertIsValidBetaSpent(design)

    return(invisible(design))
}

.calculateAlphaSpent <- function(design) {
    if (design$sided == 2) {
        if (design$bindingFutility) {
            futilityBounds <- design$futilityBounds
            futilityBounds[is.na(futilityBounds)] <- 0
            decisionMatrix <- matrix(c(
                -design$criticalValues, -futilityBounds, 0,
                futilityBounds, 0, design$criticalValues
            ), nrow = 4, byrow = TRUE)
        } else {
            decisionMatrix <- matrix(c(-design$criticalValues, design$criticalValues), nrow = 2, byrow = TRUE)
        }
    } else {
        if (design$bindingFutility) {
            decisionMatrix <- matrix(c(
                design$futilityBounds, C_FUTILITY_BOUNDS_DEFAULT,
                design$criticalValues
            ), nrow = 2, byrow = TRUE)
        } else {
            decisionMatrix <- matrix(c(
                rep(C_FUTILITY_BOUNDS_DEFAULT, design$kMax),
                design$criticalValues
            ), nrow = 2, byrow = TRUE)
        }
    }

    tryCatch(
        {
            probs <- .getGroupSequentialProbabilities(decisionMatrix, design$informationRates)
            if (design$sided == 1) {
                design$alphaSpent <- cumsum(probs[3, ] - probs[2, ])
            } else if (nrow(decisionMatrix) == 2) {
                design$alphaSpent <- cumsum(probs[3, ] - probs[2, ] + probs[1, ])
            } else {
                design$alphaSpent <- cumsum(probs[5, ] - probs[4, ] + probs[1, ])
            }
            if (!is.na(design$alphaSpent[design$kMax])) {
                design$alphaSpent[design$kMax] <- floor(design$alphaSpent[design$kMax] * 1e8) / 1e8
            }
            design$.setParameterType("alphaSpent", C_PARAM_GENERATED)
        },
        error = function(e) {
            warning("Failed to calculate 'alphaSpent': ", e, call. = FALSE)
        }
    )
}

#'
#' Haybittle & Peto design
#'
#' @noRd
#'
.getDesignGroupSequentialHaybittleAndPeto <- function(design) {
    scale <- .getOneDimensionalRoot(
        function(scale) {
            design$criticalValues <- c(rep(design$constantBoundsHP, design$kMax - 1), scale)
            if (design$sided == 2) {
                decisionMatrix <- matrix(c(-design$criticalValues, design$criticalValues), nrow = 2, byrow = TRUE)
                probs <- .getGroupSequentialProbabilities(decisionMatrix, design$informationRates)
                return(sum(probs[3, ] - probs[2, ] + probs[1, ]) - design$alpha)
            } else {
                if (design$bindingFutility) {
                    decisionMatrix <- matrix(c(
                        design$futilityBounds, C_FUTILITY_BOUNDS_DEFAULT,
                        design$criticalValues
                    ), nrow = 2, byrow = TRUE)
                } else {
                    decisionMatrix <- matrix(c(
                        rep(C_FUTILITY_BOUNDS_DEFAULT, design$kMax),
                        design$criticalValues
                    ), nrow = 2, byrow = TRUE)
                }
                probs <- .getGroupSequentialProbabilities(decisionMatrix, design$informationRates)
                return(sum(probs[3, ] - probs[2, ]) - design$alpha)
            }
        },
        lower = 0, upper = 8, tolerance = design$tolerance,
        callingFunctionInformation = ".getDesignGroupSequentialHaybittleAndPeto"
    )

    design$criticalValues <- c(rep(design$constantBoundsHP, design$kMax - 1), scale)

    .calculateAlphaSpent(design)

    return(invisible(design))
}

.getOptimumDesign <- function(deltaWT, design) {
    scale <- .getOneDimensionalRoot(
        function(scale) {
            criticalValues <- scale * design$informationRates^(deltaWT - 0.5)
            if (design$sided == 2) {
                decisionMatrix <- (matrix(c(-criticalValues, criticalValues), nrow = 2, byrow = TRUE))
                probs <- .getGroupSequentialProbabilities(decisionMatrix, design$informationRates)
                return(sum(probs[3, ] - probs[2, ] + probs[1, ]) - design$alpha)
            } else {
                if (design$bindingFutility) {
                    decisionMatrix <- matrix(c(
                        design$futilityBounds, C_FUTILITY_BOUNDS_DEFAULT,
                        criticalValues
                    ), nrow = 2, byrow = TRUE)
                } else {
                    decisionMatrix <- matrix(c(
                        rep(C_FUTILITY_BOUNDS_DEFAULT, design$kMax),
                        criticalValues
                    ), nrow = 2, byrow = TRUE)
                }
                probs <- .getGroupSequentialProbabilities(decisionMatrix, design$informationRates)
                return(sum(probs[3, ] - probs[2, ]) - design$alpha)
            }
        },
        lower = 0, upper = 5, tolerance = design$tolerance, callingFunctionInformation = ".getOptimumDesign"
    )

    design$criticalValues <- scale * design$informationRates^(deltaWT - 0.5)
    designCharacteristics <- .getDesignCharacteristics(design = design)

    y <- NA_integer_
    if (design$optimizationCriterion == C_OPTIMIZATION_CRITERION_ASNH1) {
        y <- designCharacteristics$averageSampleNumber1
    }
    if (design$optimizationCriterion == C_OPTIMIZATION_CRITERION_ASNIFH1) {
        y <- designCharacteristics$inflationFactor + designCharacteristics$averageSampleNumber1
    }
    if (design$optimizationCriterion == C_OPTIMIZATION_CRITERION_ASN_SUM) {
        y <- designCharacteristics$averageSampleNumber0 +
            designCharacteristics$averageSampleNumber01 + designCharacteristics$averageSampleNumber1
    }
    return(y)
}

#'
#' Optimum design within Wang and Tsiatis class
#'
#' @noRd
#'
.getDesignGroupSequentialWangAndTsiatisOptimum <- function(design) {
    .assertDesignParameterExists(design, "optimizationCriterion", C_OPTIMIZATION_CRITERION_DEFAULT)
    .assertIsOptimizationCriterion(design$optimizationCriterion)

    optimumDesign <- stats::optimize(.getOptimumDesign,
        design = design,
        interval = c(0, 1), tol = 0.0001
    )

    design$deltaWT <- round(optimumDesign$minimum, 3)
    design$.setParameterType("deltaWT", C_PARAM_GENERATED)

    # Recalculation of design characteristics with rounded design$deltaWT
    design$criticalValues <- .getDesignGroupSequentialDeltaWTCpp(
        design$kMax,
        design$alpha,
        design$sided,
        design$informationRates,
        design$bindingFutility,
        design$futilityBounds,
        design$tolerance,
        design$deltaWT
    )
    designCharacteristics <- .getDesignCharacteristics(design = design)
    design$power <- designCharacteristics$power
    design$.setParameterType("power", C_PARAM_GENERATED)

    .calculateAlphaSpent(design)

    return(invisible(design))
}

#'
#' Alpha spending approaches
#'
#' @noRd
#'
.getDesignGroupSequentialAlphaSpending <- function(design, userFunctionCallEnabled) {
    design$criticalValues <- .getDesignGroupSequentialAlphaSpendingCpp(
        design$kMax,
        design$alpha,
        design$gammaA,
        design$typeOfDesign,
        design$sided,
        design$informationRates,
        design$bindingFutility,
        design$futilityBounds,
        design$tolerance
    )
    .calculateAlphaSpent(design)
    return(.getDesignGroupSequentialBetaSpendingApproaches(design, userFunctionCallEnabled))
}

#'
#' User defined alpha spending approach
#'
#' @noRd
#'
.getDesignGroupSequentialUserDefinedAlphaSpending <- function(design, userFunctionCallEnabled) {
    design$criticalValues <- rep(NA_real_, design$kMax)
    if (design$typeOfDesign == C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY) {
        design$userAlphaSpending <- rep(0, design$kMax)
        design$userAlphaSpending[design$kMax] <- design$alpha
    }
    if (design$typeOfDesign == C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY && !design$bindingFutility) {
        design$criticalValues[1:(design$kMax - 1)] <- C_QNORM_THRESHOLD
        design$criticalValues[design$kMax] <- .getOneMinusQNorm(design$alpha / design$sided)
    } else {
        design$criticalValues <- .getDesignGroupSequentialUserDefinedAlphaSpendingCpp(
            design$kMax, design$userAlphaSpending, design$sided,
            design$informationRates, design$bindingFutility, design$futilityBounds, design$tolerance
        )
        if (design$typeOfDesign == C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY) {
            design$criticalValues[1:(design$kMax - 1)] <- C_QNORM_THRESHOLD
        }
    }

    .calculateAlphaSpent(design)

    return(invisible(.getDesignGroupSequentialBetaSpendingApproaches(design, userFunctionCallEnabled)))
}

#'
#' Only for alpha spending approaches
#'
#' @noRd
#'
.getDesignGroupSequentialBetaSpendingApproaches <- function(design, userFunctionCallEnabled) {
    # beta spending approaches (additional to alpha spending)!
    if (.isBetaSpendingDesignType(design$typeBetaSpending,
            userDefinedBetaSpendingIncluded = FALSE, noneIncluded = FALSE
        )) {
        .getDesignGroupSequentialBetaSpending(design, userFunctionCallEnabled)
    }

    # User defined beta spending
    if (design$typeBetaSpending == C_TYPE_OF_DESIGN_BS_USER) {
        .getDesignGroupSequentialUserDefinedBetaSpending(design)
    }

    return(invisible(design))
}

.fillVec <- function(vec, n) {
    return(c(vec, rep(NA_real_, n - length(vec))))
}

#'
#' Beta spending approaches (additional to alpha spending)
#' Find shift with beta spending such that last critical values coincide
#'
#' @noRd
#'
.getDesignGroupSequentialBetaSpending <- function(design, userFunctionCallEnabled) {
    cppResult <- .getDesignGroupSequentialBetaSpendingCpp(
        design$criticalValues,
        design$kMax,
        design$userAlphaSpending,
        design$userBetaSpending,
        design$informationRates,
        design$bindingFutility,
        design$tolerance,
        design$typeOfDesign,
        design$typeBetaSpending,
        design$gammaA,
        design$gammaB,
        design$alpha,
        design$beta,
        design$sided,
        design$betaAdjustment,
        design$twoSidedPower
    )

    design$futilityBounds <- cppResult$futilityBounds
    design$criticalValues <- cppResult$criticalValues
    design$betaSpent <- cppResult$betaSpent
    design$power <- cppResult$power

    if (design$sided == 2) {
        .calculateAlphaSpent(design)
    }

    design$.setParameterType("betaSpent", C_PARAM_GENERATED)
    design$.setParameterType("power", C_PARAM_GENERATED)
    design$.setParameterType("futilityBounds", C_PARAM_GENERATED)

    return(invisible(design))
}

#'
#' User defined beta spending
#'
#' Find shift with beta spending such that last critical values coincide
#'
#' @noRd
#'
.getDesignGroupSequentialUserDefinedBetaSpending <- function(design) {
    if (design$typeBetaSpending != C_TYPE_OF_DESIGN_BS_USER) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'typeBetaSpending' ('", design$typeBetaSpending, "') must be '", C_TYPE_OF_DESIGN_BS_USER, "'"
        )
    }

    cppResult <- .getDesignGroupSequentialUserDefinedBetaSpendingCpp(
        design$criticalValues,
        design$kMax,
        design$userAlphaSpending,
        design$userBetaSpending,
        design$sided,
        design$informationRates,
        design$bindingFutility,
        design$tolerance,
        design$typeOfDesign,
        design$gammaA,
        design$alpha,
        design$betaAdjustment,
        design$twoSidedPower
    )

    design$futilityBounds <- cppResult$futilityBounds
    design$criticalValues <- cppResult$criticalValues
    design$betaSpent <- cppResult$betaSpent
    design$power <- cppResult$power

    if (design$sided == 2) {
        .calculateAlphaSpent(design)
    }

    design$.setParameterType("betaSpent", C_PARAM_GENERATED)
    design$.setParameterType("power", C_PARAM_GENERATED)
    design$.setParameterType("futilityBounds", C_PARAM_GENERATED)
    return(invisible(design))
}

#'
#' Calculate stopping, rejection and futility probabilities for delayed response design
#'
#' @noRd
#'
.calculateDecisionProbabilities <- function(sqrtShift, informationRates, delayedInformation,
        contRegionUpper, contRegionLower, decisionCriticalValues) {
    kMax <- length(informationRates)
    power <- numeric(kMax)
    futilityProbabilities <- numeric(kMax - 1)
    stoppingProbabilities <- numeric(kMax - 1)
    rejectionProbabilities <- numeric(kMax)
    contRegionLower <- c(contRegionLower, decisionCriticalValues[kMax])

    for (stage in 1:(kMax)) {
        if (!is.na(delayedInformation[stage]) && delayedInformation[stage] > 0) {
            # information rate vector in case of recruitment stop at 'stage'
            informationRatesUponDelay <- c(
                informationRates[1:stage],
                informationRates[stage] + delayedInformation[stage]
            )
            if (stage == 1) {
                probs1 <- .getGroupSequentialProbabilities(
                    matrix(
                        c(
                            contRegionUpper[stage] - sqrtShift * sqrt(informationRatesUponDelay[1]),
                            decisionCriticalValues[stage] - sqrtShift * sqrt(informationRatesUponDelay[2]),
                            C_UPPER_BOUNDS_DEFAULT, C_UPPER_BOUNDS_DEFAULT
                        ),
                        nrow = 2, byrow = TRUE
                    ), informationRatesUponDelay
                )
                probs2 <- .getGroupSequentialProbabilities(
                    matrix(
                        c(
                            -C_UPPER_BOUNDS_DEFAULT, decisionCriticalValues[stage] - sqrtShift * sqrt(informationRatesUponDelay[2]),
                            contRegionLower[stage] - sqrtShift * sqrt(informationRatesUponDelay[1]), C_UPPER_BOUNDS_DEFAULT
                        ),
                        nrow = 2, byrow = TRUE
                    ), informationRatesUponDelay
                )
                rejectionProbabilities[stage] <- probs1[2, stage + 1] - probs1[1, stage + 1] +
                    probs2[2, stage + 1] - probs2[1, stage + 1]
                power[stage] <- rejectionProbabilities[stage]
                futilityProbabilities[stage] <- probs2[2, stage]
                stoppingProbabilities[stage] <- probs2[2, stage] + 1 - probs1[1, stage]
            } else if (stage < kMax) {
                probs1 <- .getGroupSequentialProbabilities(
                    matrix(
                        c(
                            contRegionLower[1:(stage - 1)] - sqrtShift * sqrt(informationRatesUponDelay[1:(stage - 1)]),
                            contRegionUpper[stage] - sqrtShift * sqrt(informationRatesUponDelay[stage]),
                            decisionCriticalValues[stage] - sqrtShift * sqrt(informationRatesUponDelay[stage + 1]),
                            contRegionUpper[1:(stage - 1)] - sqrtShift * sqrt(informationRatesUponDelay[1:(stage - 1)]),
                            C_UPPER_BOUNDS_DEFAULT, C_UPPER_BOUNDS_DEFAULT
                        ),
                        nrow = 2, byrow = TRUE
                    ), informationRatesUponDelay
                )
                probs2 <- .getGroupSequentialProbabilities(
                    matrix(
                        c(
                            contRegionLower[1:(stage - 1)] - sqrtShift *
                                sqrt(informationRatesUponDelay[1:(stage - 1)]), -C_UPPER_BOUNDS_DEFAULT,
                            decisionCriticalValues[stage] - sqrtShift * sqrt(informationRatesUponDelay[stage + 1]),
                            contRegionUpper[1:(stage - 1)] - sqrtShift * sqrt(informationRatesUponDelay[1:(stage - 1)]),
                            contRegionLower[stage] - sqrtShift * sqrt(informationRatesUponDelay[stage]), C_UPPER_BOUNDS_DEFAULT
                        ),
                        nrow = 2, byrow = TRUE
                    ), informationRatesUponDelay
                )
                rejectionProbabilities[stage] <- probs1[2, stage + 1] - probs1[1, stage + 1] +
                    probs2[2, stage + 1] - probs2[1, stage + 1]
                power[stage] <- sum(rejectionProbabilities[1:stage])
                futilityProbabilities[stage] <- probs2[2, stage]
                stoppingProbabilities[stage] <- probs2[2, stage] + probs1[2, stage] - probs1[1, stage]
            } else {
                probs <- .getGroupSequentialProbabilities(
                    matrix(
                        c(
                            contRegionLower[1:(stage - 1)] - sqrtShift * sqrt(informationRates[1:(stage - 1)]),
                            decisionCriticalValues[stage] - sqrtShift * sqrt(informationRates[stage]),
                            contRegionUpper[1:(stage - 1)] - sqrtShift * sqrt(informationRates[1:(stage - 1)]),
                            C_UPPER_BOUNDS_DEFAULT
                        ),
                        nrow = 2, byrow = TRUE
                    ), informationRates
                )
                rejectionProbabilities[stage] <- probs[2, stage] - probs[1, stage]
                power[stage] <- sum(rejectionProbabilities[1:stage])
            }
        } else {
            if (stage == 1) {
                probs <- .getGroupSequentialProbabilities(
                    matrix(
                        c(
                            contRegionLower[stage] - sqrtShift * sqrt(informationRates[stage]),
                            contRegionUpper[stage] - sqrtShift * sqrt(informationRates[stage])
                        ),
                        nrow = 2, byrow = TRUE
                    ), informationRates[1]
                )
            } else {
                probs <- .getGroupSequentialProbabilities(
                    matrix(
                        c(
                            contRegionLower[1:(stage - 1)] - sqrtShift * sqrt(informationRates[1:(stage - 1)]),
                            contRegionLower[stage] - sqrtShift * sqrt(informationRates[stage]),
                            contRegionUpper[1:(stage - 1)] - sqrtShift * sqrt(informationRates[1:(stage - 1)]),
                            contRegionUpper[stage] - sqrtShift * sqrt(informationRates[stage])
                        ),
                        nrow = 2, byrow = TRUE
                    ), informationRates[1:stage]
                )
            }
            rejectionProbabilities[stage] <- probs[3, stage] - probs[2, stage]
            if (stage < kMax) {
                futilityProbabilities[stage] <- probs[1, stage]
                stoppingProbabilities[stage] <- futilityProbabilities[stage] + rejectionProbabilities[stage]
            }
            power[stage] <- sum(rejectionProbabilities[1:stage])
        }
    }

    return(list(
        probs = probs,
        power = power,
        rejectionProbabilities = rejectionProbabilities,
        futilityProbabilities = futilityProbabilities,
        stoppingProbabilities = stoppingProbabilities
    ))
}

#'
#' @title
#' Get Design Inverse Normal
#'
#' @description
#' Provides adjusted boundaries and defines a group sequential design for its use in
#' the inverse normal combination test.
#'
#' @inheritParams getDesignGroupSequential
#'
#' @template details_group_sequential_design
#'
#' @template return_object_trial_design
#' @template how_to_get_help_for_generics
#'
#' @family design functions
#'
#' @template examples_get_design_inverse_normal
#'
#' @export
#'
getDesignInverseNormal <- function(...,
        kMax = NA_integer_,
        alpha = NA_real_,
        beta = NA_real_,
        sided = 1L, # C_SIDED_DEFAULT
        informationRates = NA_real_,
        futilityBounds = NA_real_,
        typeOfDesign = c("OF", "P", "WT", "PT", "HP", "WToptimum", "asP", "asOF", "asKD", "asHSD", "asUser", "noEarlyEfficacy"), # C_DEFAULT_TYPE_OF_DESIGN,
        deltaWT = NA_real_,
        deltaPT1 = NA_real_,
        deltaPT0 = NA_real_,
        optimizationCriterion = c("ASNH1", "ASNIFH1", "ASNsum"), # C_OPTIMIZATION_CRITERION_DEFAULT
        gammaA = NA_real_,
        typeBetaSpending = c("none", "bsP", "bsOF", "bsKD", "bsHSD", "bsUser"), # C_TYPE_OF_DESIGN_BS_NONE
        userAlphaSpending = NA_real_,
        userBetaSpending = NA_real_,
        gammaB = NA_real_,
        bindingFutility = NA,
        betaAdjustment = NA,
        constantBoundsHP = 3, # C_CONST_BOUND_HP_DEFAULT,
        twoSidedPower = NA,
        tolerance = 1e-08 # C_DESIGN_TOLERANCE_DEFAULT
        ) {
    .warnInCaseOfUnknownArguments(functionName = "getDesignInverseNormal", ...)
    return(.getDesignGroupSequential(
        designClass = C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL,
        kMax = kMax,
        alpha = alpha,
        beta = beta,
        sided = sided,
        informationRates = informationRates,
        futilityBounds = futilityBounds,
        typeOfDesign = typeOfDesign,
        deltaWT = deltaWT,
        deltaPT1 = deltaPT1,
        deltaPT0 = deltaPT0,
        optimizationCriterion = optimizationCriterion,
        gammaA = gammaA,
        typeBetaSpending = typeBetaSpending,
        userAlphaSpending = userAlphaSpending,
        userBetaSpending = userBetaSpending,
        gammaB = gammaB,
        bindingFutility = bindingFutility,
        betaAdjustment = betaAdjustment,
        constantBoundsHP = constantBoundsHP,
        twoSidedPower = twoSidedPower,
        tolerance = tolerance,
        userFunctionCallEnabled = TRUE
    ))
}

.getDesignInverseNormal <- function(...,
        kMax = NA_integer_,
        alpha = NA_real_,
        beta = NA_real_,
        sided = C_SIDED_DEFAULT,
        informationRates = NA_real_,
        futilityBounds = NA_real_,
        typeOfDesign = C_DEFAULT_TYPE_OF_DESIGN,
        deltaWT = NA_real_,
        deltaPT1 = NA_real_,
        deltaPT0 = NA_real_,
        optimizationCriterion = C_OPTIMIZATION_CRITERION_DEFAULT,
        gammaA = NA_real_,
        typeBetaSpending = C_TYPE_OF_DESIGN_BS_NONE,
        userAlphaSpending = NA_real_,
        userBetaSpending = NA_real_,
        gammaB = NA_real_,
        bindingFutility = NA,
        betaAdjustment = NA,
        constantBoundsHP = C_CONST_BOUND_HP_DEFAULT,
        twoSidedPower = NA,
        tolerance = C_DESIGN_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(functionName = "getDesignInverseNormal", ...)

    return(.getDesignGroupSequential(
        designClass = C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL,
        kMax = kMax,
        alpha = alpha,
        beta = beta,
        sided = sided,
        informationRates = informationRates,
        futilityBounds = futilityBounds,
        typeOfDesign = typeOfDesign,
        deltaWT = deltaWT,
        deltaPT1 = deltaPT1,
        deltaPT0 = deltaPT0,
        optimizationCriterion = optimizationCriterion,
        gammaA = gammaA,
        typeBetaSpending = typeBetaSpending,
        userAlphaSpending = userAlphaSpending,
        userBetaSpending = userBetaSpending,
        gammaB = gammaB,
        bindingFutility = bindingFutility,
        betaAdjustment = betaAdjustment,
        constantBoundsHP = constantBoundsHP,
        twoSidedPower = twoSidedPower,
        tolerance = tolerance,
        userFunctionCallEnabled = FALSE
    ))
}

.getDesignGroupSequentialDefaultValues <- function() {
    return(list(
        kMax = NA_integer_,
        alpha = NA_real_,
        beta = NA_real_,
        sided = C_SIDED_DEFAULT,
        informationRates = NA_real_,
        futilityBounds = NA_real_,
        typeOfDesign = C_DEFAULT_TYPE_OF_DESIGN,
        deltaWT = NA_real_,
        deltaPT1 = NA_real_,
        deltaPT0 = NA_real_,
        optimizationCriterion = C_OPTIMIZATION_CRITERION_DEFAULT,
        gammaA = NA_real_,
        typeBetaSpending = C_TYPE_OF_DESIGN_BS_NONE,
        userAlphaSpending = NA_real_,
        userBetaSpending = NA_real_,
        gammaB = NA_real_,
        twoSidedPower = C_TWO_SIDED_POWER_DEFAULT,
        tolerance = C_DESIGN_TOLERANCE_DEFAULT
    ))
}

.getDesignInverseNormalDefaultValues <- function() {
    return(.getDesignGroupSequentialDefaultValues())
}

#
# Param: userFunctionCallEnabled if \code{TRUE}, additional parameter validation methods will be called.
#
.getDesignGroupSequential <- function(...,
        designClass = C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL,
        kMax = NA_integer_,
        alpha = NA_real_,
        beta = NA_real_,
        sided = C_SIDED_DEFAULT,
        informationRates = NA_real_,
        futilityBounds = NA_real_,
        typeOfDesign = C_DEFAULT_TYPE_OF_DESIGN,
        deltaWT = NA_real_,
        deltaPT1 = NA_real_,
        deltaPT0 = NA_real_,
        optimizationCriterion = C_OPTIMIZATION_CRITERION_DEFAULT,
        gammaA = NA_real_,
        typeBetaSpending = C_TYPE_OF_DESIGN_BS_NONE,
        userAlphaSpending = NA_real_,
        userBetaSpending = NA_real_,
        gammaB = NA_real_,
        bindingFutility = C_BINDING_FUTILITY_DEFAULT,
        constantBoundsHP = C_CONST_BOUND_HP_DEFAULT,
        twoSidedPower = NA,
        betaAdjustment = NA,
        delayedInformation = NA_real_,
        tolerance = C_DESIGN_TOLERANCE_DEFAULT,
        userFunctionCallEnabled = FALSE) {
    typeOfDesign <- .matchArgument(typeOfDesign, C_DEFAULT_TYPE_OF_DESIGN)
    optimizationCriterion <- .matchArgument(optimizationCriterion, C_OPTIMIZATION_CRITERION_DEFAULT)
    typeBetaSpending <- .matchArgument(typeBetaSpending, C_TYPE_OF_DESIGN_BS_NONE)

    if (.isDefinedArgument(kMax, argumentExistsValidationEnabled = userFunctionCallEnabled)) {
        .assertIsValidKMax(kMax, showWarnings = TRUE)
        if (!is.integer(kMax)) {
            kMax <- as.integer(kMax)
        }
    }

    if (is.na(bindingFutility)) {
        bindingFutility <- C_BINDING_FUTILITY_DEFAULT
    } else if (userFunctionCallEnabled && typeOfDesign != C_TYPE_OF_DESIGN_PT &&
            !(typeBetaSpending == "bsP" || typeBetaSpending == "bsOF" || typeBetaSpending == "bsKD" ||
                typeBetaSpending == "bsHSD" || typeBetaSpending == "bsUser") &&
            ((!is.na(kMax) && kMax == 1) || any(is.na(futilityBounds)) ||
                (!any(is.na(futilityBounds)) && all(futilityBounds == C_FUTILITY_BOUNDS_DEFAULT)))) {
        warning("'bindingFutility' (", bindingFutility, ") will be ignored", call. = FALSE)
    }

    design <- .createDesign(
        designClass = designClass,
        kMax = kMax,
        alpha = alpha,
        beta = beta,
        sided = sided,
        informationRates = informationRates,
        futilityBounds = futilityBounds,
        typeOfDesign = typeOfDesign,
        deltaWT = deltaWT,
        deltaPT1 = deltaPT1,
        deltaPT0 = deltaPT0,
        optimizationCriterion = optimizationCriterion,
        gammaA = gammaA,
        typeBetaSpending = typeBetaSpending,
        userAlphaSpending = userAlphaSpending,
        userBetaSpending = userBetaSpending,
        gammaB = gammaB,
        bindingFutility = bindingFutility,
        constantBoundsHP = constantBoundsHP,
        twoSidedPower = twoSidedPower,
        betaAdjustment = betaAdjustment,
        delayedInformation = delayedInformation,
        tolerance = tolerance
    )

    if (userFunctionCallEnabled) {
        .validateBaseParameters(design, twoSidedWarningForDefaultValues = FALSE)
        .validateTypeOfDesign(design)

        .assertIsValidTolerance(tolerance)
        .assertDesignParameterExists(design, "alpha", C_ALPHA_DEFAULT)
        .assertDesignParameterExists(design, "beta", C_BETA_DEFAULT)
        .assertDesignParameterExists(design, "sided", C_SIDED_DEFAULT)
        .assertDesignParameterExists(design, "typeOfDesign", C_DEFAULT_TYPE_OF_DESIGN)
        .assertDesignParameterExists(design, "bindingFutility", C_BINDING_FUTILITY_DEFAULT)
        .assertDesignParameterExists(design, "tolerance", C_DESIGN_TOLERANCE_DEFAULT)

        if (typeOfDesign != C_TYPE_OF_DESIGN_PT) {
            if (!is.na(deltaPT1)) {
                warning("'deltaPT1' (", deltaPT1, ") will be ignored", call. = FALSE)
            }
            if (!is.na(deltaPT0)) {
                warning("'deltaPT0' (", deltaPT0, ") will be ignored", call. = FALSE)
            }
        }
        if (typeOfDesign != C_TYPE_OF_DESIGN_WT && !is.na(deltaWT)) {
            warning("'deltaWT' (", deltaWT, ") will be ignored", call. = FALSE)
        }
        if (typeOfDesign != C_TYPE_OF_DESIGN_AS_KD &&
                typeOfDesign != C_TYPE_OF_DESIGN_AS_HSD && !is.na(gammaA)) {
            warning("'gammaA' (", gammaA, ") will be ignored", call. = FALSE)
        }
        if (typeBetaSpending != C_TYPE_OF_DESIGN_BS_KD &&
                typeBetaSpending != C_TYPE_OF_DESIGN_BS_HSD && !is.na(gammaB)) {
            warning("'gammaB' (", gammaB, ") will be ignored", call. = FALSE)
        }
        if (typeBetaSpending != C_TYPE_OF_DESIGN_BS_USER && !all(is.na(userBetaSpending))) {
            warning("'userBetaSpending' (", .arrayToString(userBetaSpending), ") will be ignored", call. = FALSE)
        }
        if (!(typeOfDesign %in% c(C_TYPE_OF_DESIGN_AS_USER)) &&
                !all(is.na(userAlphaSpending))) {
            warning("'userAlphaSpending' (", .arrayToString(userAlphaSpending), ") will be ignored", call. = FALSE)
        }
    }

    if (design$sided == 2 && design$bindingFutility && design$typeOfDesign != C_TYPE_OF_DESIGN_PT &&
            !.isBetaSpendingDesignType(design$typeBetaSpending)) {
        warning("'bindingFutility' will be ignored because the test is defined as two-sided", call. = FALSE)
        design$bindingFutility <- FALSE
    }

    if (design$sided == 1 && design$twoSidedPower) {
        warning("'twoSidedPower' will be ignored because the test is defined as one-sided", call. = FALSE)
        design$twoSidedPower <- FALSE
    }

    if (userFunctionCallEnabled) {
        .validateAlphaAndBeta(design)
    }

    design$alphaSpent <- rep(NA_real_, design$kMax)
    design$betaSpent <- rep(NA_real_, design$kMax)
    design$power <- rep(NA_real_, design$kMax)

    if (userFunctionCallEnabled) {
        design$.setParameterType("betaSpent", C_PARAM_NOT_APPLICABLE)
        design$.setParameterType("power", C_PARAM_NOT_APPLICABLE)
        design$.setParameterType("alphaSpent", C_PARAM_NOT_APPLICABLE)
        design$.setParameterType("criticalValues", C_PARAM_GENERATED)
    }

    if (design$kMax == 1) {
        .getDesignGroupSequentialKMax1(design)
    } else {
        # Wang and Tsiatis design
        if (design$typeOfDesign == C_TYPE_OF_DESIGN_WT ||
                design$typeOfDesign == C_TYPE_OF_DESIGN_P ||
                design$typeOfDesign == C_TYPE_OF_DESIGN_OF) {
            .getDesignGroupSequentialWangAndTsiatis(design)
        }

        # Pampallona & Tsiatis design
        else if (design$typeOfDesign == C_TYPE_OF_DESIGN_PT) {
            .getDesignGroupSequentialPampallonaTsiatis(design)
        }

        # Haybittle & Peto design
        else if (design$typeOfDesign == C_TYPE_OF_DESIGN_HP) {
            .getDesignGroupSequentialHaybittleAndPeto(design)
        }

        # Optimum design within Wang and Tsiatis class
        else if (design$typeOfDesign == C_TYPE_OF_DESIGN_WT_OPTIMUM) {
            .getDesignGroupSequentialWangAndTsiatisOptimum(design)
        }

        # alpha spending approaches
        else if (.isAlphaSpendingDesignType(design$typeOfDesign, userDefinedAlphaSpendingIncluded = FALSE)) {
            .getDesignGroupSequentialAlphaSpending(design, userFunctionCallEnabled)
        }

        # user defined alpha spending approach
        else if (design$typeOfDesign %in% c(C_TYPE_OF_DESIGN_AS_USER, C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY)) {
            .getDesignGroupSequentialUserDefinedAlphaSpending(design, userFunctionCallEnabled)
        } else {
            stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "no calculation routine defined for ", design$typeOfDesign)
        }
    }

    design$stageLevels <- 1 - stats::pnorm(design$criticalValues)
    design$.setParameterType("stageLevels", C_PARAM_GENERATED)

    if (design$kMax == 1) {
        design$.setParameterType("futilityBounds", C_PARAM_NOT_APPLICABLE)
    }

    if (!all(is.na(design$futilityBounds))) {
        if (length(design$futilityBounds) == 0 || all(design$futilityBounds == C_FUTILITY_BOUNDS_DEFAULT)) {
            design$.setParameterType("bindingFutility", C_PARAM_NOT_APPLICABLE)
            design$.setParameterType("futilityBounds", C_PARAM_NOT_APPLICABLE)
        } else if (userFunctionCallEnabled &&
                any(design$futilityBounds > design$criticalValues[1:(design$kMax - 1)] - 0.01, na.rm = TRUE)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'futilityBounds' (", .arrayToString(design$futilityBounds), ") too extreme for this situation"
            )
        }
    }

    .assertIsValidAlphaSpent(design, userFunctionCallEnabled)

    design$.initStages()

    # we use 7.5 instead of C_QNORM_THRESHOLD as threshold
    design$criticalValues[!is.na(design$criticalValues) & design$criticalValues <= -7.5] <- -Inf
    design$criticalValues[!is.na(design$criticalValues) & design$criticalValues >= 7.5] <- Inf

    design$futilityBounds[!is.na(design$futilityBounds) & design$futilityBounds <=
        C_FUTILITY_BOUNDS_DEFAULT] <- C_FUTILITY_BOUNDS_DEFAULT

    if (design$kMax == 1) {
        if (!identical(design$informationRates, 1)) {
            if (!is.na(design$informationRates)) {
                warning("Information rate", ifelse(length(design$informationRates) != 1, "s", ""), " ",
                    .arrayToString(design$informationRates, vectorLookAndFeelEnabled = TRUE),
                    " will be ignored",
                    call. = FALSE
                )
            }
            design$informationRates <- 1
        }
        design$.setParameterType("informationRates", C_PARAM_NOT_APPLICABLE)
        design$.setParameterType("stages", C_PARAM_NOT_APPLICABLE)
    }

    .assertIsNumericVector(delayedInformation, "delayedInformation", naAllowed = TRUE)
    if (all(is.na(delayedInformation))) {
        # delayed response design is disabled
        return(design)
    }

    if (all(!is.na(delayedInformation)) && all(delayedInformation < 1e-03)) {
        warning("At least one delayed information value must be >= 1e-03 to enable delayed response.",
            " 'delayedInformation' (", .arrayToString(delayedInformation), ") will be ignored",
            call. = FALSE
        )
        return(design)
    }

    # proceed with delayed response design
    .assertIsInClosedInterval(delayedInformation, "delayedInformation", lower = 0, upper = NULL)
    kMax <- design$kMax
    contRegionUpper <- design$criticalValues
    contRegionLower <- design$futilityBounds
    informationRates <- design$informationRates

    decisionCriticalValues <- numeric(kMax)
    reversalProbabilities <- numeric(kMax - 1)

    if (!all(is.na(delayedInformation)) &&
            (design$sided != 1 || all(design$futilityBounds <= C_FUTILITY_BOUNDS_DEFAULT + 1e-06))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "decision critical values for delayed response design are only available for ",
            "one-sided designs with valid futility bounds"
        )
    }
    if (length(delayedInformation) == 1) {
        delayedInformation <- rep(delayedInformation, kMax - 1)
    }
    if (length(delayedInformation) != kMax - 1) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'delayedInformation' (", .arrayToString(delayedInformation), ") must have length ",
            (kMax - 1), " (kMax - 1)"
        )
    }

    indices <- which(delayedInformation > 0 & delayedInformation < 1e-03)
    n <- length(indices)
    if (n > 0) {
        warning("The", ifelse(n == 1, "", paste0(" ", n)), " delayed information value", ifelse(n == 1, "", "s"), " ",
            .arrayToString(delayedInformation[indices], mode = "and"),
            " will be replaced by 1e-03 to achieve reasonable results",
            call. = FALSE
        )
        delayedInformation[indices] <- 1e-03
    }

    # sensible interim choices are restricted by amount of delayed information
    eps <- design$informationRates[1:(design$kMax - 1)] + delayedInformation
    if (!any(is.na(eps)) && any(eps >= 1)) {
        stages <- which(eps >= 1)
        stagesInfo <- ifelse(length(stages) == 1, paste0(" ", stages), paste0("s ", .arrayToString(stages, mode = "and")))
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'delayedInformation[stage] + informationRates[stage]' for ",
            "stage", stagesInfo, " too large (>= 1). Recruitment stop analysis information + pipeline data ",
            "information cannot exceed overall trial information. Instead, the ",
            "recruitment stop analysis would be skipped, directly proceeding to the ",
            "final analysis"
        )
    }

    # loop iterating through the stages calculation the decision critical values
    for (stage in 1:(kMax - 1)) {
        if (!is.na(delayedInformation[stage]) && delayedInformation[stage] >= 1e-03 - 1e-06) {
            # information rate vector in case of recruitment stop at 'stage'
            informationRatesUponDelay <- c(
                informationRates[1:stage],
                informationRates[stage] + delayedInformation[stage]
            )
            if (stage == 1) {
                decisionCriticalValues[stage] <- .getOneDimensionalRoot(function(secondCriticalValue) {
                    probs1 <- .getGroupSequentialProbabilities(
                        matrix(c(contRegionUpper[stage], secondCriticalValue, C_UPPER_BOUNDS_DEFAULT, C_UPPER_BOUNDS_DEFAULT),
                            nrow = 2, byrow = TRUE
                        ), informationRatesUponDelay
                    )
                    probs2 <- .getGroupSequentialProbabilities(
                        matrix(
                            c(
                                -C_UPPER_BOUNDS_DEFAULT, secondCriticalValue,
                                contRegionLower[stage], C_UPPER_BOUNDS_DEFAULT
                            ),
                            nrow = 2, byrow = TRUE
                        ), informationRatesUponDelay
                    )
                    return(probs1[1, stage + 1] - probs2[2, stage + 1] + probs2[1, stage + 1])
                }, lower = -C_UPPER_BOUNDS_DEFAULT, upper = C_UPPER_BOUNDS_DEFAULT, tolerance = design$tolerance)

                probs <- .getGroupSequentialProbabilities(
                    matrix(
                        c(
                            contRegionUpper[stage], decisionCriticalValues[stage],
                            C_UPPER_BOUNDS_DEFAULT, C_UPPER_BOUNDS_DEFAULT
                        ),
                        nrow = 2, byrow = TRUE
                    ), informationRatesUponDelay
                )
            } else {
                decisionCriticalValues[stage] <- .getOneDimensionalRoot(function(secondCriticalValue) {
                    probs1 <- .getGroupSequentialProbabilities(
                        matrix(
                            c(
                                contRegionLower[1:(stage - 1)], contRegionUpper[stage], secondCriticalValue,
                                contRegionUpper[1:(stage - 1)], C_UPPER_BOUNDS_DEFAULT, C_UPPER_BOUNDS_DEFAULT
                            ),
                            nrow = 2, byrow = TRUE
                        ), informationRatesUponDelay
                    )
                    probs2 <- .getGroupSequentialProbabilities(
                        matrix(
                            c(
                                contRegionLower[1:(stage - 1)], -C_UPPER_BOUNDS_DEFAULT, secondCriticalValue,
                                contRegionUpper[1:(stage - 1)], contRegionLower[stage], C_UPPER_BOUNDS_DEFAULT
                            ),
                            nrow = 2, byrow = TRUE
                        ), informationRatesUponDelay
                    )
                    return(probs1[1, stage + 1] - probs2[2, stage + 1] + probs2[1, stage + 1])
                }, lower = -C_UPPER_BOUNDS_DEFAULT, upper = C_UPPER_BOUNDS_DEFAULT, tolerance = design$tolerance)

                probs <- .getGroupSequentialProbabilities(
                    matrix(
                        c(
                            contRegionLower[1:(stage - 1)], contRegionUpper[stage], decisionCriticalValues[stage],
                            contRegionUpper[1:(stage - 1)], C_UPPER_BOUNDS_DEFAULT, C_UPPER_BOUNDS_DEFAULT
                        ),
                        nrow = 2, byrow = TRUE
                    ), informationRatesUponDelay
                )
            }
            if (stage < kMax) {
                reversalProbabilities[stage] <- probs[1, stage + 1]
            }
        } else {
            decisionCriticalValues[stage] <- NA_real_
            reversalProbabilities[stage] <- NA_real_
        }
        decisionCriticalValues[kMax] <- contRegionUpper[kMax]

        alphaSpent <- .calculateDecisionProbabilities(
            sqrtShift = 0, informationRates,
            delayedInformation, contRegionUpper, contRegionLower, decisionCriticalValues
        )$power
    }

    decisionCriticalValues[decisionCriticalValues <= -C_UPPER_BOUNDS_DEFAULT + 1e-06] <- NA_real_
    decisionCriticalValues[decisionCriticalValues >= C_UPPER_BOUNDS_DEFAULT - 1e-06] <- NA_real_

    design$decisionCriticalValues <- decisionCriticalValues
    design$reversalProbabilities <- reversalProbabilities
    design$delayedInformation <- delayedInformation
    design$delayedInformation[design$delayedInformation < 1e-03] <- 0
    design$.setParameterType("decisionCriticalValues", C_PARAM_GENERATED)
    design$.setParameterType("reversalProbabilities", C_PARAM_GENERATED)

    warning("The delayed information design feature is experimental and ",
        "hence not fully validated (see www.rpact.com/experimental)",
        call. = FALSE
    )

    return(design)
}

# to avoid error messages in case of repeated p-values computation
.assertIsValidAlphaSpent <- function(design, userFunctionCallEnabled = TRUE) {
    if (!userFunctionCallEnabled) {
        return(invisible())
    }

    if (design$informationRates[design$kMax] != 1) {
        return(invisible())
    }

    if (is.na(design$alphaSpent[design$kMax]) || abs(design$alphaSpent[design$kMax] - design$alpha) > 1e-05) {
        stop(
            C_EXCEPTION_TYPE_RUNTIME_ISSUE, "critical values cannot be calculated ",
            "(alpha: ", design$alpha, "; alpha spent at maximum stage: ", design$alphaSpent[design$kMax], ")"
        )
    }
}

.assertIsValidBetaSpent <- function(design, ..., userFunctionCallEnabled = TRUE, iteration = 1) {
    if (!userFunctionCallEnabled) {
        return(invisible())
    }

    if (design$informationRates[design$kMax] != 1) {
        return(invisible())
    }

    if (iteration < 0) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "critical values cannot be calculated")
    }

    if (is.na(design$betaSpent[design$kMax]) || abs(design$betaSpent[design$kMax] - design$beta) > 1e-05) {
        stop(
            C_EXCEPTION_TYPE_RUNTIME_ISSUE, "critical values cannot be calculated ",
            "(beta spent at maximum stage: ", design$betaSpent[design$kMax], ")"
        )
    }
}

#'
#' @title
#' Get Design Group Sequential
#'
#' @description
#' Provides adjusted boundaries and defines a group sequential design.
#'
#' @inheritParams param_kMax
#' @inheritParams param_alpha
#' @inheritParams param_beta
#' @inheritParams param_sided
#' @inheritParams param_typeOfDesign
#' @inheritParams param_informationRates
#' @param futilityBounds The futility bounds, defined on the test statistic z scale
#'        (numeric vector of length \code{kMax - 1}).
#' @inheritParams param_bindingFutility
#' @param deltaWT Delta for Wang & Tsiatis Delta class.
#' @param deltaPT1 Delta1 for Pampallona & Tsiatis class rejecting H0 boundaries.
#' @param deltaPT0 Delta0 for Pampallona & Tsiatis class rejecting H1 boundaries.
#' @param constantBoundsHP The constant bounds up to stage \code{kMax - 1} for the
#'        Haybittle & Peto design (default is \code{3}).
#' @param optimizationCriterion Optimization criterion for optimum design within
#'        Wang & Tsiatis class (\code{"ASNH1"}, \code{"ASNIFH1"},
#'        \code{"ASNsum"}), default is \code{"ASNH1"}, see details.
#' @param typeBetaSpending Type of beta spending. Type of of beta spending is one of the following:
#'        O'Brien & Fleming type beta spending, Pocock type beta spending,
#'        Kim & DeMets beta spending, Hwang, Shi & DeCani beta spending, user defined
#'        beta spending (\code{"bsOF"}, \code{"bsP"}, \code{"bsKD"},
#'        \code{"bsHSD"}, \code{"bsUser"}, default is \code{"none"}).
#' @param gammaA Parameter for alpha spending function.
#' @param gammaB Parameter for beta spending function.
#' @inheritParams param_userAlphaSpending
#' @param delayedInformation Delay of information for delayed response designs. Can be a numeric value or a
#' 		  numeric vector of length \code{kMax - 1}
#' @param userBetaSpending The user defined beta spending. Vector of length \code{kMax} containing the cumulative
#'        beta-spending up to each interim stage.
#' @param twoSidedPower For two-sided testing, if \code{twoSidedPower = TRUE} is specified
#'        the sample size calculation is performed by considering both tails of the distribution.
#'        Default is \code{FALSE}, i.e., it is assumed that one tail probability is equal to 0 or the power
#'        should be directed to one part.
#' @param betaAdjustment For two-sided beta spending designs, if \code{betaAdjustement = TRUE} a linear
#' 		  adjustment of the beta spending values is performed if an overlapping of decision regions for futility
#' 		  stopping at earlier stages occurs, otherwise no adjustment is performed (default is \code{TRUE}).
#' @param tolerance The numerical tolerance, default is \code{1e-08}.
#' @inheritParams param_three_dots
#'
#' @template details_group_sequential_design
#'
#' @template return_object_trial_design
#' @template how_to_get_help_for_generics
#'
#' @family design functions
#'
#' @template examples_get_design_group_sequential
#'
#' @export
#'
getDesignGroupSequential <- function(...,
        kMax = NA_integer_,
        alpha = NA_real_,
        beta = NA_real_,
        sided = 1L, # C_SIDED_DEFAULT
        informationRates = NA_real_,
        futilityBounds = NA_real_,
        typeOfDesign = c("OF", "P", "WT", "PT", "HP", "WToptimum", "asP", "asOF", "asKD", "asHSD", "asUser", "noEarlyEfficacy"), # C_DEFAULT_TYPE_OF_DESIGN,
        deltaWT = NA_real_,
        deltaPT1 = NA_real_,
        deltaPT0 = NA_real_,
        optimizationCriterion = c("ASNH1", "ASNIFH1", "ASNsum"), # C_OPTIMIZATION_CRITERION_DEFAULT
        gammaA = NA_real_,
        typeBetaSpending = c("none", "bsP", "bsOF", "bsKD", "bsHSD", "bsUser"), # C_TYPE_OF_DESIGN_BS_NONE
        userAlphaSpending = NA_real_,
        userBetaSpending = NA_real_,
        gammaB = NA_real_,
        bindingFutility = NA,
        betaAdjustment = NA,
        constantBoundsHP = 3, # C_CONST_BOUND_HP_DEFAULT,
        twoSidedPower = NA,
        delayedInformation = NA_real_,
        tolerance = 1e-08 # C_DESIGN_TOLERANCE_DEFAULT
        ) {
    .warnInCaseOfUnknownArguments(functionName = "getDesignGroupSequential", ...)
    return(.getDesignGroupSequential(
        designClass = C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL,
        kMax = kMax,
        alpha = alpha,
        beta = beta,
        sided = sided,
        informationRates = informationRates,
        futilityBounds = futilityBounds,
        typeOfDesign = typeOfDesign,
        deltaWT = deltaWT,
        deltaPT1 = deltaPT1,
        deltaPT0 = deltaPT0,
        optimizationCriterion = optimizationCriterion,
        gammaA = gammaA,
        typeBetaSpending = typeBetaSpending,
        userAlphaSpending = userAlphaSpending,
        userBetaSpending = userBetaSpending,
        gammaB = gammaB,
        bindingFutility = bindingFutility,
        constantBoundsHP = constantBoundsHP,
        twoSidedPower = twoSidedPower,
        betaAdjustment = betaAdjustment,
        delayedInformation = delayedInformation,
        tolerance = tolerance,
        userFunctionCallEnabled = TRUE
    ))
}

.getFixedSampleSize <- function(alpha, beta, sided, twoSidedPower = C_TWO_SIDED_POWER_DEFAULT) {
    .assertIsValidAlphaAndBeta(alpha = alpha, beta = beta)
    .assertIsValidSidedParameter(sided)

    if (sided == 1) {
        return((.getOneMinusQNorm(alpha) + .getOneMinusQNorm(beta))^2)
    }
    if (twoSidedPower) {
        n <- .getOneDimensionalRoot(
            function(n) {
                stats::pnorm(-.getOneMinusQNorm(alpha / 2) - sqrt(n)) -
                    stats::pnorm(.getOneMinusQNorm(alpha / 2) - sqrt(n)) + beta
            },
            lower = 0,
            upper = 2 * (.getOneMinusQNorm(alpha / 2) + .getOneMinusQNorm(beta))^2,
            tolerance = 1e-08, callingFunctionInformation = ".getFixedSampleSize"
        )
    } else {
        n <- (.getOneMinusQNorm(alpha / 2) + .getOneMinusQNorm(beta))^2
    }
    return(n)
}

#' @title
#' Get Design Characteristics
#'
#' @description
#' Calculates the characteristics of a design and returns it.
#'
#' @inheritParams param_design
#' @inheritParams param_three_dots
#'
#' @details
#' Calculates the inflation factor (IF),
#' the expected reduction in sample size under H1, under H0, and under a value in between H0 and H1.
#' Furthermore, absolute information values are calculated
#' under the prototype case testing H0: mu = 0 against H1: mu = 1.
#'
#' @return Returns a \code{\link{TrialDesignCharacteristics}} object.
#' The following generics (R generic functions) are available for this result object:
#' \itemize{
#'   \item \code{\link[=names.FieldSet]{names()}} to obtain the field names,
#'   \item \code{\link[=print.FieldSet]{print()}} to print the object,
#'   \item \code{\link[=summary.ParameterSet]{summary()}} to display a summary of the object,
#'   \item \code{\link[=plot.ParameterSet]{plot()}} to plot the object,
#'   \item \code{\link[=as.data.frame.TrialDesignCharacteristics]{as.data.frame()}} to coerce the object to a \code{\link[base]{data.frame}},
#'   \item \code{\link[=as.matrix.FieldSet]{as.matrix()}} to coerce the object to a \code{\link[base]{matrix}}.
#' }
#' @template how_to_get_help_for_generics
#'
#' @family design functions
#'
#' @template examples_get_design_characteristics
#'
#' @export
#'
getDesignCharacteristics <- function(design = NULL, ...) {
    if (is.null(design)) {
        design <- .getDefaultDesign(..., type = "characteristics")
        .warnInCaseOfUnknownArguments(
            functionName = "getDesignCharacteristics",
            ignore = .getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, powerCalculationEnabled = FALSE), ...
        )
    } else {
        .assertIsTrialDesign(design)
        .warnInCaseOfUnknownArguments(functionName = "getDesignCharacteristics", ...)
        .warnInCaseOfTwoSidedPowerArgument(...)
    }

    return(.getDesignCharacteristics(design = design, userFunctionCallEnabled = TRUE))
}

.getDesignCharacteristics <- function(..., design, userFunctionCallEnabled = FALSE) {
    .assertIsTrialDesignInverseNormalOrGroupSequential(design)
    .assertDesignParameterExists(design, "sided", C_SIDED_DEFAULT)
    .assertIsValidSidedParameter(design$sided)

    if (userFunctionCallEnabled) {
        .validateAlphaAndBeta(design = design)
    }

    design$informationRates <- .getValidatedInformationRates(design, writeToDesign = FALSE)

    if ((design$typeOfDesign == C_TYPE_OF_DESIGN_PT ||
            .isBetaSpendingDesignType(design$typeBetaSpending)) && design$sided == 2 && design$kMax == 2) {
        design$futilityBounds[is.na(design$futilityBounds)] <- 0
    }

    design$futilityBounds <- .getValidatedFutilityBounds(design,
        writeToDesign = FALSE, twoSidedWarningForDefaultValues = FALSE
    )

    designCharacteristics <- TrialDesignCharacteristics$new(design = design)

    designCharacteristics$rejectionProbabilities <- rep(NA_real_, design$kMax)
    designCharacteristics$.setParameterType("rejectionProbabilities", C_PARAM_NOT_APPLICABLE)

    designCharacteristics$futilityProbabilities <- rep(NA_real_, design$kMax - 1)
    designCharacteristics$.setParameterType("futilityProbabilities", C_PARAM_NOT_APPLICABLE)

    nFixed <- .getFixedSampleSize(
        alpha = design$alpha, beta = design$beta,
        sided = design$sided, twoSidedPower = design$twoSidedPower
    )
    designCharacteristics$nFixed <- nFixed
    designCharacteristics$.setParameterType("nFixed", C_PARAM_GENERATED)

    design$criticalValues[design$criticalValues > 7.5] <- 7.5
    if (length(design$decisionCriticalValues) > 0) {
        design$decisionCriticalValues[!is.na(design$decisionCriticalValues) & design$decisionCriticalValues > 7.5] <- 7.5
    }
    informationRates <- design$informationRates

    if (design$kMax == 1) {
        designCharacteristics$shift <- nFixed
        designCharacteristics$.setParameterType("shift", C_PARAM_GENERATED)

        designCharacteristics$inflationFactor <- designCharacteristics$shift / nFixed
        designCharacteristics$.setParameterType("inflationFactor", C_PARAM_GENERATED)

        designCharacteristics$power <- 1 - design$beta
        designCharacteristics$.setParameterType("power", design$.getParameterType("power"))

        designCharacteristics$.setParameterType("information", C_PARAM_NOT_APPLICABLE)

        designCharacteristics$.setParameterType("averageSampleNumber1", C_PARAM_NOT_APPLICABLE)
        designCharacteristics$.setParameterType("averageSampleNumber01", C_PARAM_NOT_APPLICABLE)
        designCharacteristics$.setParameterType("averageSampleNumber0", C_PARAM_NOT_APPLICABLE)
        designCharacteristics$.setParameterType(".probs", C_PARAM_NOT_APPLICABLE)

        return(designCharacteristics)
    }

    if (!any(is.na(design$delayedInformation)) && length(design$decisionCriticalValues) > 0) {
        kMax <- design$kMax
        contRegionUpper <- design$criticalValues
        contRegionLower <- design$futilityBounds
        informationRates <- design$informationRates
        decisionCriticalValues <- design$decisionCriticalValues
        informationRates <- design$informationRates
        delayedInformation <- design$delayedInformation
        kMax <- length(informationRates)

        shift <- .getOneDimensionalRoot(
            function(shift) {
                resultsH1 <- .calculateDecisionProbabilities(sqrt(shift), informationRates, delayedInformation, contRegionUpper, contRegionLower, decisionCriticalValues)
                return(resultsH1$power[kMax] - 1 + design$beta)
            },
            lower = 0, upper = 4 * nFixed,
            tolerance = design$tolerance, callingFunctionInformation = ".getDesignCharacteristics"
        )

        stopping <- numeric(kMax)
        futility <- numeric(kMax)
        rejectionProbabilities <- numeric(kMax)

        resultsH1 <- .calculateDecisionProbabilities(
            sqrtShift = sqrt(shift), informationRates, delayedInformation,
            contRegionUpper, contRegionLower, decisionCriticalValues
        )
        resultsH01 <- .calculateDecisionProbabilities(
            sqrtShift = sqrt(shift) / 2, informationRates, delayedInformation,
            contRegionUpper, contRegionLower, decisionCriticalValues
        )
        resultsH0 <- .calculateDecisionProbabilities(
            sqrtShift = 0, informationRates, delayedInformation,
            contRegionUpper, contRegionLower, decisionCriticalValues
        )

        designCharacteristics$shift <- shift
        designCharacteristics$.probs <- resultsH1$probs
        designCharacteristics$power <- resultsH1$power

        designCharacteristics$information <- informationRates * shift

        designCharacteristics$averageSampleNumber1 <-
            (shift - sum(resultsH1$stoppingProbabilities * (informationRates[kMax] -
                delayedInformation - informationRates[1:(kMax - 1)]) * shift)) / nFixed
        designCharacteristics$averageSampleNumber01 <-
            (shift - sum(resultsH01$stoppingProbabilities * (informationRates[kMax] -
                delayedInformation - informationRates[1:(kMax - 1)]) * shift)) / nFixed
        designCharacteristics$averageSampleNumber0 <-
            (shift - sum(resultsH0$stoppingProbabilities * (informationRates[kMax] -
                delayedInformation - informationRates[1:(kMax - 1)]) * shift)) / nFixed
        futilityProbabilities <- resultsH1$futilityProbabilities
        rejectionProbabilities <- resultsH1$power
        stoppingProbabilities <- resultsH1$stoppingProbabilities
        rejectionProbabilities[2:kMax] <- resultsH1$power[2:kMax] - rejectionProbabilities[1:(kMax - 1)]

        designCharacteristics$rejectionProbabilities <- rejectionProbabilities
        designCharacteristics$futilityProbabilities <- futilityProbabilities
    } else if ((design$typeOfDesign == C_TYPE_OF_DESIGN_PT ||
            .isBetaSpendingDesignType(design$typeBetaSpending)) && design$sided == 2) {
        design$futilityBounds[is.na(design$futilityBounds)] <- 0

        shift <- .getOneDimensionalRoot(
            function(shift) {
                decisionMatrix <- matrix(c(
                    -design$criticalValues - sqrt(shift * informationRates),
                    c(-design$futilityBounds - sqrt(shift * informationRates[1:(design$kMax - 1)]), 0),
                    c(design$futilityBounds - sqrt(shift * informationRates[1:(design$kMax - 1)]), 0),
                    design$criticalValues - sqrt(shift * informationRates)
                ), nrow = 4, byrow = TRUE)
                probs <- .getGroupSequentialProbabilities(decisionMatrix, informationRates)
                if (design$twoSidedPower) {
                    return(sum(probs[5, ] - probs[4, ] + probs[1, ]) - 1 + design$beta)
                } else {
                    return(sum(probs[5, ] - probs[4, ]) - 1 + design$beta)
                }
            },
            lower = 0, upper = 4 * (.getOneMinusQNorm(design$alpha / design$sided) + .getOneMinusQNorm(design$beta))^2,
            tolerance = design$tolerance, callingFunctionInformation = ".getDesignCharacteristics"
        )

        decisionMatrix <- matrix(c(
            -design$criticalValues - sqrt(shift * informationRates),
            c(-design$futilityBounds - sqrt(shift * informationRates[1:(design$kMax - 1)]), 0),
            c(design$futilityBounds - sqrt(shift * informationRates[1:(design$kMax - 1)]), 0),
            design$criticalValues - sqrt(shift * informationRates)
        ), nrow = 4, byrow = TRUE)
        probs <- .getGroupSequentialProbabilities(decisionMatrix, informationRates)
        designCharacteristics$shift <- shift
        designCharacteristics$.probs <- probs
        if (design$twoSidedPower) {
            designCharacteristics$power <- cumsum(probs[5, ] - probs[4, ] + probs[1, ])
            designCharacteristics$rejectionProbabilities <- probs[5, ] - probs[4, ] + probs[1, ]
        } else {
            designCharacteristics$power <- cumsum(probs[5, ] - probs[4, ])
            designCharacteristics$rejectionProbabilities <- probs[5, ] - probs[4, ]
        }
        if (design$kMax > 1) {
            designCharacteristics$futilityProbabilities <- probs[3, 1:(design$kMax - 1)] - probs[2, 1:(design$kMax - 1)]
        }

        designCharacteristics$information <- informationRates * shift
        designCharacteristics$averageSampleNumber1 <- .getAverageSampleNumber(
            design$kMax, design$informationRates, probs, shift, nFixed
        )

        decisionMatrix <- matrix(c(
            -design$criticalValues,
            c(-design$futilityBounds, 0),
            c(design$futilityBounds, 0),
            design$criticalValues
        ), nrow = 4, byrow = TRUE)
        probs0 <- .getGroupSequentialProbabilities(decisionMatrix, informationRates)
        designCharacteristics$averageSampleNumber0 <- .getAverageSampleNumber(
            design$kMax, design$informationRates, probs0, shift, nFixed
        )

        decisionMatrix <- matrix(c(
            -design$criticalValues - sqrt(shift * informationRates) / 2,
            c(-design$futilityBounds - sqrt(shift * informationRates[1:(design$kMax - 1)]) / 2, 0),
            c(design$futilityBounds - sqrt(shift * informationRates[1:(design$kMax - 1)]) / 2, 0),
            design$criticalValues - sqrt(shift * informationRates) / 2
        ), nrow = 4, byrow = TRUE)
        probs01 <- .getGroupSequentialProbabilities(decisionMatrix, informationRates)
        designCharacteristics$averageSampleNumber01 <- .getAverageSampleNumber(
            design$kMax, design$informationRates, probs01, shift, nFixed
        )

        design$futilityBounds[design$futilityBounds == 0] <- NA_real_
    } else {
        shift <- .getOneDimensionalRoot(
            function(shift) {
                if (design$sided == 2) {
                    decisionMatrix <- matrix(c(
                        -design$criticalValues - sqrt(shift * informationRates),
                        design$criticalValues - sqrt(shift * informationRates)
                    ), nrow = 2, byrow = TRUE)
                    probs <- .getGroupSequentialProbabilities(decisionMatrix, informationRates)
                    if (design$twoSidedPower) {
                        return(sum(probs[3, ] - probs[2, ] + probs[1, ]) - 1 + design$beta)
                    } else {
                        return(sum(probs[3, ] - probs[2, ]) - 1 + design$beta)
                    }
                } else {
                    shiftedFutilityBounds <- design$futilityBounds - sqrt(shift * informationRates[1:(design$kMax - 1)])
                    shiftedFutilityBounds[design$futilityBounds <= C_FUTILITY_BOUNDS_DEFAULT] <-
                        C_FUTILITY_BOUNDS_DEFAULT
                    decisionMatrix <- matrix(c(
                        shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT,
                        design$criticalValues - sqrt(shift * informationRates)
                    ), nrow = 2, byrow = TRUE)
                    probs <- .getGroupSequentialProbabilities(decisionMatrix, informationRates)
                    return(sum(probs[3, ] - probs[2, ]) - 1 + design$beta)
                }
            },
            lower = 0, upper = 4 * (.getOneMinusQNorm(design$alpha / design$sided) + .getOneMinusQNorm(design$beta))^2,
            tolerance = design$tolerance, callingFunctionInformation = ".getDesignCharacteristics"
        )

        if (design$sided == 2) {
            decisionMatrix <- matrix(c(
                -design$criticalValues - sqrt(shift * informationRates),
                design$criticalValues - sqrt(shift * informationRates)
            ), nrow = 2, byrow = TRUE)
        } else {
            shiftedFutilityBounds <- design$futilityBounds - sqrt(shift * informationRates[1:(design$kMax - 1)])
            shiftedFutilityBounds[design$futilityBounds <= C_FUTILITY_BOUNDS_DEFAULT] <-
                C_FUTILITY_BOUNDS_DEFAULT
            decisionMatrix <- matrix(c(
                shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT,
                design$criticalValues - sqrt(shift * informationRates)
            ), nrow = 2, byrow = TRUE)
        }
        probs <- .getGroupSequentialProbabilities(decisionMatrix, informationRates)
        designCharacteristics$shift <- shift
        designCharacteristics$.probs <- probs
        if (design$twoSidedPower) {
            designCharacteristics$power <- cumsum(probs[3, ] - probs[2, ] + probs[1, ])
            designCharacteristics$rejectionProbabilities <- probs[3, ] - probs[2, ] + probs[1, ]
        } else {
            designCharacteristics$power <- cumsum(probs[3, ] - probs[2, ])
            designCharacteristics$rejectionProbabilities <- probs[3, ] - probs[2, ]
        }

        if (design$kMax > 1) {
            if (design$sided == 2) {
                designCharacteristics$futilityProbabilities <- rep(0, design$kMax - 1)
            } else {
                designCharacteristics$futilityProbabilities <- probs[1, 1:(design$kMax - 1)]
                designCharacteristics$futilityProbabilities[design$futilityBounds == C_FUTILITY_BOUNDS_DEFAULT] <- 0
            }
            designCharacteristics$power <- .getNoEarlyEfficacyZeroCorrectedValues(
                design, designCharacteristics$power
            )
            designCharacteristics$rejectionProbabilities <- .getNoEarlyEfficacyZeroCorrectedValues(
                design, designCharacteristics$rejectionProbabilities
            )
        }
        designCharacteristics$information <- informationRates * shift
        designCharacteristics$averageSampleNumber1 <- .getAverageSampleNumber(
            design$kMax,
            design$informationRates, probs, shift, nFixed
        )
        if (design$sided == 2) {
            decisionMatrix <- matrix(c(-design$criticalValues, design$criticalValues), nrow = 2, byrow = TRUE)
        } else {
            decisionMatrix <- matrix(c(
                design$futilityBounds, C_FUTILITY_BOUNDS_DEFAULT,
                design$criticalValues
            ), nrow = 2, byrow = TRUE)
        }
        probs0 <- .getGroupSequentialProbabilities(decisionMatrix, informationRates)
        designCharacteristics$averageSampleNumber0 <- .getAverageSampleNumber(
            design$kMax,
            design$informationRates, probs0, shift, nFixed
        )
        if (design$sided == 2) {
            decisionMatrix <- matrix(c(
                -design$criticalValues - sqrt(shift * informationRates) / 2,
                design$criticalValues - sqrt(shift * informationRates) / 2
            ), nrow = 2, byrow = TRUE)
        } else {
            shiftedFutilityBounds <- design$futilityBounds - sqrt(shift * informationRates[1:(design$kMax - 1)]) / 2
            shiftedFutilityBounds[design$futilityBounds <= C_FUTILITY_BOUNDS_DEFAULT] <- C_FUTILITY_BOUNDS_DEFAULT
            decisionMatrix <- matrix(c(shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT, design$criticalValues -
                sqrt(shift * informationRates) / 2), nrow = 2, byrow = TRUE)
        }
        probs01 <- .getGroupSequentialProbabilities(decisionMatrix, informationRates)
        designCharacteristics$averageSampleNumber01 <- .getAverageSampleNumber(
            design$kMax, design$informationRates, probs01, shift, nFixed
        )
    }
    design$criticalValues[design$criticalValues >= 7.5 - 1e-8] <- Inf
    designCharacteristics$.setParameterType("shift", C_PARAM_GENERATED)
    designCharacteristics$.setParameterType("power", C_PARAM_GENERATED)
    designCharacteristics$.setParameterType(".probs", C_PARAM_GENERATED)
    designCharacteristics$.setParameterType("rejectionProbabilities", C_PARAM_GENERATED)
    designCharacteristics$.setParameterType("information", C_PARAM_GENERATED)
    designCharacteristics$.setParameterType("futilityProbabilities", C_PARAM_GENERATED)
    designCharacteristics$.setParameterType("averageSampleNumber0", C_PARAM_GENERATED)
    designCharacteristics$.setParameterType("averageSampleNumber01", C_PARAM_GENERATED)
    designCharacteristics$.setParameterType("averageSampleNumber1", C_PARAM_GENERATED)

    designCharacteristics$inflationFactor <- shift / nFixed
    designCharacteristics$.setParameterType("inflationFactor", C_PARAM_GENERATED)

    if (is.na(designCharacteristics$inflationFactor) ||
            designCharacteristics$inflationFactor > 4 || designCharacteristics$inflationFactor < 1 - 1e-05) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "inflation factor cannot be calculated")
    }

    return(designCharacteristics)
}

.getAverageSampleNumber <- function(kMax, informationRates, probs, shift, nFixed) {
    if (nrow(probs) == 3) {
        return((shift - sum((probs[3, 1:(kMax - 1)] - probs[2, 1:(kMax - 1)] + probs[1, 1:(kMax - 1)]) *
            (informationRates[kMax] - informationRates[1:(kMax - 1)]) * shift)) / nFixed)
    } else {
        return((shift - sum((probs[5, 1:(kMax - 1)] -
            probs[4, 1:(kMax - 1)] + probs[3, 1:(kMax - 1)] - probs[2, 1:(kMax - 1)] + probs[1, 1:(kMax - 1)]) *
            (informationRates[kMax] - informationRates[1:(kMax - 1)]) * shift)) / nFixed)
    }
}

#'
#' @title
#' Get Power And Average Sample Number
#'
#' @description
#' Returns the power and average sample number of the specified design.
#'
#' @inheritParams param_design
#' @inheritParams param_theta
#' @inheritParams param_nMax
#'
#' @details
#' This function returns the power and average sample number (ASN) of the specified
#' design for the prototype case which is testing H0: mu = mu0 in a one-sample design.
#' \code{theta} represents the standardized effect \code{(mu - mu0) / sigma} and power and ASN
#' is calculated for maximum sample size \code{nMax}.
#' For other designs than the one-sample test of a mean the standardized effect needs to be adjusted accordingly.
#'
#' @return Returns a \code{\link{PowerAndAverageSampleNumberResult}} object.
#' The following generics (R generic functions) are available for this result object:
#' \itemize{
#'   \item \code{\link[=names.FieldSet]{names()}} to obtain the field names,
#'   \item \code{\link[=print.FieldSet]{print()}} to print the object,
#'   \item \code{\link[=summary.ParameterSet]{summary()}} to display a summary of the object,
#'   \item \code{\link[=plot.ParameterSet]{plot()}} to plot the object,
#'   \item \code{\link[=as.data.frame.PowerAndAverageSampleNumberResult]{as.data.frame()}}
#'         to coerce the object to a \code{\link[base]{data.frame}},
#'   \item \code{\link[=as.matrix.FieldSet]{as.matrix()}} to coerce the object to a \code{\link[base]{matrix}}.
#' }
#' @template how_to_get_help_for_generics
#'
#' @family design functions
#'
#' @template examples_get_power_and_average_sample_number
#'
#' @export
#'
getPowerAndAverageSampleNumber <- function(design, theta = seq(-1, 1, 0.02), nMax = 100) {
    .assertIsTrialDesign(design)
    .assertIsSingleNumber(nMax, "nMax")
    .assertIsInClosedInterval(nMax, "nMax", lower = 1, upper = NULL)
    return(PowerAndAverageSampleNumberResult$new(design = design, theta = theta, nMax = nMax))
}

.getSimulatedRejectionsDelayedResponse <- function(delta, informationRates, delayedInformation,
        contRegionUpper, contRegionLower, decisionCriticalValues, iterations, seed = NA_real_) {
    seed <- .setSeed(seed)
    kMax <- length(informationRates)
    zVector <- numeric(kMax)
    reject <- 0L
    for (i in 1:iterations) {
        for (stage in 1:kMax) {
            if (stage == 1) {
                zVector[stage] <- stats::rnorm(1, delta * sqrt(informationRates[1]), 1)
            } else {
                zVector[stage] <- (sqrt(informationRates[stage - 1]) * zVector[stage - 1] +
                    sqrt(informationRates[stage] - informationRates[stage - 1]) *
                        stats::rnorm(1, delta * sqrt(informationRates[stage] - informationRates[stage - 1]), 1)) /
                    sqrt(informationRates[stage])
            }
            if (!is.na(decisionCriticalValues[stage]) && stage < kMax &&
                    (zVector[stage] > contRegionUpper[stage] || zVector[stage] < contRegionLower[stage])) {
                if ((sqrt(informationRates[stage]) * zVector[stage] + sqrt(delayedInformation[stage]) *
                        stats::rnorm(1, delta * sqrt(delayedInformation[stage]), 1)) /
                        sqrt(informationRates[stage] + delayedInformation[stage]) > decisionCriticalValues[stage]) {
                    reject <- reject + 1L
                }
                break
            }
            if (stage == kMax && zVector[stage] > decisionCriticalValues[stage]) {
                reject <- reject + 1L
            }
        }
    }
    simulatedAlpha <- reject / iterations
    return(list(
        simulatedAlpha = simulatedAlpha,
        delta = delta,
        iterations = iterations,
        seed = seed
    ))
}


#'
#' @title
#' Simulated Rejections Delayed Response
#'
#' @description
#' Simulates the rejection probability of a delayed response group sequential design with specified parameters.
#'
#' @inheritParams param_design
#' @param delta The delay value.
#' @param iterations The number of simulation iterations.
#' @inheritParams param_seed
#'
#' @details
#' By default, delta = 0, i.e., the Type error rate is simulated.
#'
#' @return Returns a list summarizing the rejection probabilities.
#'
#' @noRd
#'
getSimulatedRejectionsDelayedResponse <- function(design, ..., delta = 0, iterations = 10000, seed = NA_real_) {
    .assertIsTrialDesignInverseNormalOrGroupSequential(design)
    .assertIsSingleNumber(delta, "delta")
    .assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
    if (!design$.isDelayedResponseDesign()) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'design' must be a delayed response design with specified 'delayedInformation'")
    }
    startTime <- Sys.time()
    result <- .getSimulatedRejectionsDelayedResponse(
        delta = delta,
        informationRates = design$informationRates,
        delayedInformation = design$delayedInformation,
        contRegionUpper = design$criticalValues,
        contRegionLower = design$futilityBounds,
        decisionCriticalValues = design$decisionCriticalValues,
        iterations = iterations,
        seed = seed
    )

    simulatedAlpha <- result$simulatedAlpha
    stdError <- sqrt(simulatedAlpha * (1 - simulatedAlpha) / iterations)
    ciLower <- simulatedAlpha - 1.96 * stdError
    ciUpper <- simulatedAlpha + 1.96 * stdError
    result$confidenceIntervall <- c(ciLower, ciUpper)
    # simulated Type I error rate is within the 95% error bounds
    result$alphaWithin95ConfidenceIntervall <- design$alpha > ciLower && design$alpha < ciUpper
    result$time <- Sys.time() - startTime
    return(result)
}
