## |
## |  *Fisher combination test*
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
## |  File version: $Revision: 7126 $
## |  Last changed: $Date: 2023-06-23 14:26:39 +0200 (Fr, 23 Jun 2023) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_constants.R
#' @include f_core_utilities.R
#' @include f_logger.R
NULL

.getFisherCombinationSize <- function(kMax, alpha0Vec, criticalValues, tVec,
        cases = .getFisherCombinationCases(kMax = kMax, tVec = tVec)) {
    return(getFisherCombinationSizeCpp(kMax, alpha0Vec, criticalValues, tVec, cases))
}

#' @title
#' Get Design Fisher
#'
#' @description
#' Performs Fisher's combination test and returns critical values for this design.
#'
#' @inheritParams param_kMax
#' @inheritParams param_alpha
#' @param method \code{"equalAlpha"}, \code{"fullAlpha"}, \code{"noInteraction"}, or \code{"userDefinedAlpha"},
#' default is \code{"equalAlpha"} (for details, see Wassmer, 1999).
#' @inheritParams param_userAlphaSpending
#' @param alpha0Vec Stopping for futility bounds for stage-wise p-values.
#' @inheritParams param_informationRates
#' @inheritParams param_sided
#' @param bindingFutility If \code{bindingFutility = TRUE} is specified the calculation of
#'        the critical values is affected by the futility bounds (default is \code{TRUE}).
#' @param tolerance The numerical tolerance, default is \code{1e-14}.
#' @param iterations The number of simulation iterations, e.g.,
#'        \code{getDesignFisher(iterations = 100000)} checks the validity of the critical values for the design.
#'        The default value of \code{iterations} is 0, i.e., no simulation will be executed.
#' @param seed Seed for simulating the power for Fisher's combination test. See above, default is a random seed.
#' @inheritParams param_three_dots
#'
#' @details
#' \code{getDesignFisher()} calculates the critical values and stage levels for
#' Fisher's combination test as described in Bauer (1989), Bauer and Koehne (1994),
#' Bauer and Roehmel (1995), and Wassmer (1999) for equally and unequally sized stages.
#'
#' @seealso \code{\link[=getDesignSet]{getDesignSet()}} for creating a set of designs to compare.
#'
#' @template return_object_trial_design
#' @template how_to_get_help_for_generics
#'
#' @family design functions
#'
#' @template examples_get_design_Fisher
#'
#' @export
#'
getDesignFisher <- function(...,
        kMax = NA_integer_,
        alpha = NA_real_,
        method = c("equalAlpha", "fullAlpha", "noInteraction", "userDefinedAlpha"), # C_FISHER_METHOD_DEFAULT
        userAlphaSpending = NA_real_,
        alpha0Vec = NA_real_,
        informationRates = NA_real_,
        sided = 1, # C_SIDED_DEFAULT
        bindingFutility = NA,
        tolerance = 1e-14, # C_ANALYSIS_TOLERANCE_FISHER_DEFAULT
        iterations = 0,
        seed = NA_real_) {
    .assertIsValidTolerance(tolerance)
    .assertIsValidIterationsAndSeed(iterations, seed)
    .warnInCaseOfUnknownArguments(functionName = "getDesignFisher", ...)

    return(.getDesignFisher(
        kMax = kMax, alpha = alpha, method = method,
        userAlphaSpending = userAlphaSpending, alpha0Vec = alpha0Vec, informationRates = informationRates,
        sided = sided, bindingFutility = bindingFutility,
        tolerance = tolerance, iterations = iterations, seed = seed, userFunctionCallEnabled = TRUE
    ))
}

.getDesignFisherDefaultValues <- function() {
    return(list(
        kMax = NA_integer_,
        alpha = NA_real_,
        method = C_FISHER_METHOD_DEFAULT,
        userAlphaSpending = NA_real_,
        alpha0Vec = NA_real_,
        informationRates = NA_real_,
        sided = 1,
        bindingFutility = C_BINDING_FUTILITY_FISHER_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_FISHER_DEFAULT,
        iterations = 0,
        seed = NA_real_
    ))
}

.getFisherCombinationCases <- function(kMax, tVec) {
    return(getFisherCombinationCasesCpp(kMax, tVec))
}

#'
#' @param userFunctionCallEnabled if \code{TRUE}, additional parameter validation methods will be called.
#'
#' @noRd
#'
.getDesignFisher <- function(kMax = NA_integer_, alpha = NA_real_, method = C_FISHER_METHOD_DEFAULT,
        userAlphaSpending = NA_real_, alpha0Vec = NA_real_, informationRates = NA_real_,
        sided = 1, bindingFutility = C_BINDING_FUTILITY_FISHER_DEFAULT,
        tolerance = C_ANALYSIS_TOLERANCE_FISHER_DEFAULT, iterations = 0, seed = NA_real_,
        userFunctionCallEnabled = FALSE) {
    method <- .matchArgument(method, C_FISHER_METHOD_DEFAULT)

    .assertIsNumericVector(alpha0Vec, "alpha0Vec", naAllowed = TRUE)

    if (.isDefinedArgument(kMax, argumentExistsValidationEnabled = userFunctionCallEnabled)) {
        .assertIsValidKMax(kMax, kMaxUpperBound = C_KMAX_UPPER_BOUND_FISHER)

        if (!is.integer(kMax)) {
            kMax <- as.integer(kMax)
        }
    }

    if (!is.integer(sided) && sided %in% c(1, 2)) {
        sided <- as.integer(sided)
    }

    if (sided != 1) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "Fisher's combination test only available for one-sided testing")
    }

    if (is.na(bindingFutility)) {
        bindingFutility <- C_BINDING_FUTILITY_FISHER_DEFAULT
    } else if (userFunctionCallEnabled &&
            ((!is.na(kMax) && kMax == 1) ||
                (!any(is.na(alpha0Vec)) && all(alpha0Vec == C_ALPHA_0_VEC_DEFAULT)))) {
        warning("'bindingFutility' (", bindingFutility, ") will be ignored", call. = FALSE)
    }

    design <- TrialDesignFisher(
        kMax = kMax,
        alpha = alpha,
        method = method,
        sided = sided,
        userAlphaSpending = userAlphaSpending,
        alpha0Vec = alpha0Vec,
        informationRates = informationRates,
        bindingFutility = bindingFutility,
        tolerance = tolerance,
        iterations = as.integer(iterations),
        seed = seed
    )

    .assertDesignParameterExists(design, "sided", C_SIDED_DEFAULT)
    .assertIsValidSidedParameter(design$sided)

    .assertDesignParameterExists(design, "method", C_FISHER_METHOD_DEFAULT)
    .assertIsSingleCharacter(design$method, "method")
    if (!.isFisherMethod(design$method)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'method' must be one of the following: ", .printFisherMethods()
        )
    }

    .assertDesignParameterExists(design, "bindingFutility", C_BINDING_FUTILITY_FISHER_DEFAULT)

    .assertDesignParameterExists(design, "tolerance", C_ANALYSIS_TOLERANCE_FISHER_DEFAULT)

    .setKmaxBasedOnAlphaSpendingDefintion(design)

    design$informationRates <- .getValidatedInformationRates(design)
    design$alpha0Vec <- .getValidatedAlpha0Vec(design)

    if (design$sided == 2 && design$bindingFutility && any(design$alpha0Vec < 1)) {
        warning("Binding futility will be ignored because the test is defined as two-sided", call. = FALSE)
    }

    if (design$method == C_FISHER_METHOD_USER_DEFINED_ALPHA) {
        .validateUserAlphaSpending(design)
    } else {
        design$.setParameterType("userAlphaSpending", C_PARAM_NOT_APPLICABLE)
        if (.isDefinedArgument(design$userAlphaSpending)) {
            warning("'userAlphaSpending' will be ignored because 'method' is not '",
                C_FISHER_METHOD_USER_DEFINED_ALPHA, "'",
                call. = FALSE
            )
        }
    }

    if (.isUndefinedArgument(design$alpha)) {
        design$alpha <- C_ALPHA_DEFAULT
    }

    .assertDesignParameterExists(design, "alpha", C_ALPHA_DEFAULT)
    .assertIsSingleNumber(design$alpha, "alpha")
    .assertIsValidSidedParameter(sided)
    if (sided != 1) {
        design$alpha <- design$alpha / sided
    }
    if (userFunctionCallEnabled) {
        .assertIsValidAlpha(design$alpha)
    }

    .assertDesignParameterExists(design, "kMax", 3)
    .assertIsSingleInteger(design$kMax, "kMax")
    .assertIsValidKMax(design$kMax, kMaxUpperBound = C_KMAX_UPPER_BOUND_FISHER)
    if (design$method == C_FISHER_METHOD_NO_INTERACTION && design$kMax < 3) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "method '", C_FISHER_METHOD_NO_INTERACTION,
            "' is only allowed for kMax > 2 (kMax is ", design$kMax, ")"
        )
    }

    if (design$kMax > 1) {
        design$scale <- round(sqrt((design$informationRates[2:design$kMax] -
            design$informationRates[1:(design$kMax - 1)]) / design$informationRates[1]), 10)
    }
    design$criticalValues <- rep(NA_real_, design$kMax)

    design$.setParameterType("scale", C_PARAM_NOT_APPLICABLE)
    design$.setParameterType("criticalValues", C_PARAM_GENERATED)

    if (design$bindingFutility) {
        alpha0Vec <- design$alpha0Vec
    } else {
        alpha0Vec <- rep(1, design$kMax - 1)
    }

    if (design$method == C_FISHER_METHOD_NO_INTERACTION && !any(is.na(alpha0Vec)) &&
            all(alpha0Vec == C_ALPHA_0_VEC_DEFAULT)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "for specified 'method' (\"", C_FISHER_METHOD_NO_INTERACTION,
            "\") the 'alpha0Vec' must be unequal to ", .arrayToString(alpha0Vec, vectorLookAndFeelEnabled = TRUE),
            " and 'bindingFutility' must be TRUE"
        )
    }

    design$.setParameterType("stageLevels", C_PARAM_GENERATED)
    design$.setParameterType("alphaSpent", C_PARAM_GENERATED)
    design$.setParameterType("nonStochasticCurtailment", C_PARAM_GENERATED)

    tryCatch(
        {
            cases <- .getFisherCombinationCases(kMax = design$kMax, tVec = design$scale)
            result <- getDesignFisherTryCpp(
                design$kMax, design$alpha, design$tolerance,
                design$criticalValues, design$scale, alpha0Vec, design$userAlphaSpending, design$method
            )
            design$criticalValues <- result$criticalValues
            design$alphaSpent <- result$alphaSpent
            design$stageLevels <- result$stageLevels
            design$nonStochasticCurtailment <- result$nonStochasticCurtailment
            size <- result$size

            design$stageLevels <- sapply(1:design$kMax, function(k) {
                .getFisherCombinationSize(k, rep(1, k - 1),
                    rep(design$criticalValues[k], k), design$scale,
                    cases = cases
                )
            })

            design$alphaSpent <- sapply(1:design$kMax, function(k) {
                .getFisherCombinationSize(k, alpha0Vec[1:(k - 1)],
                    design$criticalValues[1:k], design$scale,
                    cases = cases
                )
            })

            design$nonStochasticCurtailment <- FALSE
            if (design$stageLevels[1] < 1e-10) {
                design$criticalValues[1:(design$kMax - 1)] <- design$criticalValues[design$kMax]
                design$stageLevels <- sapply(
                    1:design$kMax,
                    function(k) {
                        .getFisherCombinationSize(k, rep(1, k - 1),
                            rep(design$criticalValues[k], k), design$scale,
                            cases = cases
                        )
                    }
                )
                design$alphaSpent <- sapply(
                    1:design$kMax,
                    function(k) {
                        .getFisherCombinationSize(k, alpha0Vec[1:(k - 1)],
                            design$criticalValues[1:k], design$scale,
                            cases = cases
                        )
                    }
                )
                design$nonStochasticCurtailment <- TRUE
            }
        },
        error = function(e) {
            warning("Output may be wrong because an error occured: ", e$message, call. = FALSE)
        }
    )

    if (userFunctionCallEnabled) {
        if (design$method == C_FISHER_METHOD_NO_INTERACTION && abs(size - design$alpha) > 1e-03) {
            stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "numerical overflow in computation routine")
        }

        if (design$method == C_FISHER_METHOD_EQUAL_ALPHA && !all(is.na(design$stageLevels)) &&
                abs(mean(na.omit(design$stageLevels)) - design$stageLevels[1]) > 1e-03) {
            stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "numerical overflow in computation routine")
        }

        if (design$kMax > 1) {
            diff <- na.omit(design$criticalValues[2:design$kMax] - design$criticalValues[1:(design$kMax - 1)])
            if (length(diff) > 0 && any(diff > 1e-12)) {
                .logDebug(
                    "Stop creation of Fisher design because critical values are ",
                    .arrayToString(criticalValues, vectorLookAndFeelEnabled = TRUE), ", ",
                    "i.e., differences are ", .arrayToString(diff, vectorLookAndFeelEnabled = TRUE)
                )
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "no calculation possible")
            }

            if (!all(is.na(design$stageLevels)) && any(na.omit(design$stageLevels[1:(design$kMax - 1)]) > design$alpha)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'alpha' (", design$alpha, ") not correctly specified"
                )
            }
        }

        if (design$method == C_FISHER_METHOD_USER_DEFINED_ALPHA) {
            if (any(abs(design$alphaSpent - design$userAlphaSpending) > 1e-05)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'alpha' (", design$alpha, ") or 'userAlphaSpending' (",
                    .arrayToString(design$userAlphaSpending), ") not correctly specified"
                )
            }
        }
    }

    design$.setParameterType("simAlpha", C_PARAM_NOT_APPLICABLE)
    design$simAlpha <- NA_real_
    if (!is.null(design$iterations) && !is.na(design$iterations) && design$iterations > 0) {
        design$.setParameterType("seed", ifelse(!is.null(design$seed) && !is.na(design$seed),
            C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE
        ))
        design$seed <- .setSeed(design$seed)
        design$simAlpha <- getSimulatedAlphaCpp(
            kMax = design$kMax,
            alpha0 = design$alpha0Vec,
            criticalValues = design$criticalValues,
            tVec = design$scale,
            iterations = iterations
        )
        design$.setParameterType("simAlpha", C_PARAM_GENERATED)
        design$.setParameterType("iterations", C_PARAM_USER_DEFINED)
    }

    if (design$kMax == 1) {
        design$.setParameterType("alpha0Vec", C_PARAM_NOT_APPLICABLE)
    }

    if (length(design$alpha0Vec) == 0 ||
            all(design$alpha0Vec == C_ALPHA_0_VEC_DEFAULT)) {
        design$.setParameterType("bindingFutility", C_PARAM_NOT_APPLICABLE)
    }

    design$.initStages()

    return(design)
}
