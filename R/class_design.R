## |
## |  *Trial design classes*
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
## |  File version: $Revision: 6810 $
## |  Last changed: $Date: 2023-02-13 12:58:47 +0100 (Mo, 13 Feb 2023) $
## |  Last changed by: $Author: pahlke $
## |


#' @include f_core_constants.R
#' @include f_core_plot.R
#' @include f_core_utilities.R
NULL

#'
#' @name TrialDesign
#'
#' @title
#' Basic Trial Design
#'
#' @description
#' Basic class for trial designs.
#' 
#' @template field_kMax
#' @template field_alpha
#' @template field_stages
#' @template field_informationRates
#' @template field_userAlphaSpending
#' @template field_criticalValues
#' @template field_stageLevels
#' @template field_alphaSpent
#' @template field_bindingFutility
#' @template field_tolerance
#'
#' @details
#' \code{TrialDesign} is the basic class for
#' \itemize{
#'   \item \code{\link{TrialDesignFisher}},
#'   \item \code{\link{TrialDesignGroupSequential}}, and
#'   \item \code{\link{TrialDesignInverseNormal}}.
#' }
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include f_core_constants.R
#' @include f_core_plot.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
TrialDesign <- setRefClass("TrialDesign",
    contains = "ParameterSet",
    fields = list(
        .plotSettings = "PlotSettings",
        kMax = "integer",
        alpha = "numeric",
        stages = "integer",
        informationRates = "numeric",
        userAlphaSpending = "numeric",
        criticalValues = "numeric",
        stageLevels = "numeric",
        alphaSpent = "numeric",
        bindingFutility = "logical",
        tolerance = "numeric"
    ),
    methods = list(
        initialize = function(...,
                alpha = NA_real_,
                informationRates = NA_real_,
                userAlphaSpending = NA_real_,
                criticalValues = NA_real_,
                stageLevels = NA_real_,
                alphaSpent = NA_real_,
                bindingFutility = NA,
                tolerance = 1e-06 # C_ANALYSIS_TOLERANCE_DEFAULT
                ) {
            callSuper(...,
                alpha = alpha,
                informationRates = informationRates,
                userAlphaSpending = userAlphaSpending,
                criticalValues = criticalValues,
                stageLevels = stageLevels,
                alphaSpent = alphaSpent,
                bindingFutility = bindingFutility,
                tolerance = tolerance
            )

            .plotSettings <<- PlotSettings()

            if (inherits(.self, "TrialDesignConditionalDunnett")) {
                .parameterNames <<- C_PARAMETER_NAMES
            } else {
                .parameterNames <<- .getSubListByNames(.getParameterNames(design = .self), c(
                    "stages",
                    "kMax",
                    "alpha",
                    "informationRates",
                    "userAlphaSpending",
                    "criticalValues",
                    "stageLevels",
                    "alphaSpent",
                    "bindingFutility",
                    "tolerance"
                ))
            }

            .parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS

            .initStages()
        },
        show = function(showType = 1, digits = NA_integer_) {
            .show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing trial design objects"
            .resetCat()
            if (showType == 3) {
                .createSummary(.self, digits = digits)$.show(
                    showType = 1,
                    digits = digits, consoleOutputEnabled = consoleOutputEnabled
                )
            } else if (showType == 2) {
                callSuper(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                .cat("Design parameters and output of ", .toString(), ":\n\n",
                    heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                .showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showParametersOfOneGroup(.getDerivedParameters(), "Derived from user defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showParametersOfOneGroup(.getGeneratedParameters(), "Output",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            s <- "unknown trial design"
            if (.isTrialDesignGroupSequential(.self)) {
                s <- "group sequential design"
            } else if (.isTrialDesignInverseNormal(.self)) {
                s <- "inverse normal combination test design"
            } else if (.isTrialDesignFisher(.self)) {
                s <- "Fisher's combination test design"
            } else if (.isTrialDesignConditionalDunnett(.self)) {
                s <- "conditional Dunnett test design"
            }
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
        },
        .initStages = function() {
            if (length(kMax) == 1 && !is.na(kMax) && kMax > 0) {
                stages <<- c(1L:kMax)
                if (kMax == C_KMAX_DEFAULT) {
                    .setParameterType("stages", C_PARAM_DEFAULT_VALUE)
                } else {
                    type <- .getParameterType("kMax")
                    .setParameterType("stages", ifelse(type != C_PARAM_TYPE_UNKNOWN, type, C_PARAM_USER_DEFINED))
                }
            } else {
                .setParameterType("stages", C_PARAM_NOT_APPLICABLE)
            }
        },
        .isDelayedResponseDesign = function() {
            return((inherits(.self, "TrialDesignGroupSequential") || inherits(.self, "TrialDesignInverseNormal")) &&
                .self$kMax > 1 &&
                !is.null(.self[["delayedInformation"]]) &&
                !any(is.na(.self$delayedInformation)) && any(.self$delayedInformation > 0))
        }
    )
)

#'
#' @name TrialDesignCharacteristics
#'
#' @title
#' Trial Design Characteristics
#'
#' @description
#' Class for trial design characteristics.
#'
#' @details
#' \code{TrialDesignCharacteristics} contains all fields required to collect the characteristics of a design.
#' This object should not be created directly; use \code{\link[=getDesignCharacteristics]{getDesignCharacteristics()}}
#' with suitable arguments to create it.
#'
#' @seealso \code{\link[=getDesignCharacteristics]{getDesignCharacteristics()}} for getting the design characteristics.
#'
#' @include class_core_parameter_set.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
TrialDesignCharacteristics <- setRefClass("TrialDesignCharacteristics",
    contains = "ParameterSet",
    fields = list(
        .design = "TrialDesign",
        .probs = "matrix",
        nFixed = "numeric",
        shift = "numeric",
        inflationFactor = "numeric",
        stages = "integer",
        information = "numeric",
        power = "numeric",
        rejectionProbabilities = "numeric", # efficacy probabilities
        futilityProbabilities = "numeric",
        averageSampleNumber1 = "numeric",
        averageSampleNumber01 = "numeric",
        averageSampleNumber0 = "numeric"
    ),
    methods = list(
        initialize = function(design, ...) {
            callSuper(.design = design, ...)
            .parameterNames <<- .getParameterNames(design = design)
            .parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS
            .parameterFormatFunctions[["nFixed"]] <<- ".formatProbabilities"
            .initStages()
        },
        show = function(showType = 1, digits = NA_integer_) {
            .show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing trial design characteristics objects"
            .resetCat()
            if (showType == 2) {
                callSuper(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                .showParametersOfOneGroup(.getGeneratedParameters(),
                    title = .toString(startWithUpperCase = TRUE),
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        .initStages = function() {
            if (!is.na(.design$kMax) && .design$kMax > 0) {
                stages <<- c(1L:.design$kMax)
                if (.design$kMax == C_KMAX_DEFAULT) {
                    .setParameterType("stages", C_PARAM_DEFAULT_VALUE)
                } else {
                    .setParameterType("stages", C_PARAM_USER_DEFINED)
                }
            } else {
                .setParameterType("stages", C_PARAM_NOT_APPLICABLE)
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            if (.design$.isDelayedResponseDesign()) {
                prefix <- "delayed response"
                if (startWithUpperCase) {
                    prefix <- .firstCharacterToUpperCase(prefix)
                }
                return(paste(prefix, .design$.toString(startWithUpperCase = FALSE), "characteristics"))
            }

            return(paste(.design$.toString(startWithUpperCase = startWithUpperCase), "characteristics"))
        }
    )
)

#'
#' @title
#' Coerce TrialDesignCharacteristics to a Data Frame
#'
#' @description
#' Returns the \code{TrialDesignCharacteristics} as data frame.
#'
#' @param x A \code{\link{TrialDesignCharacteristics}} object.
#' @inheritParams param_niceColumnNamesEnabled
#' @inheritParams param_includeAllParameters
#' @inheritParams param_three_dots
#'
#' @details
#' Each element of the \code{\link{TrialDesignCharacteristics}} is converted to a column in the data frame.
#'
#' @template return_dataframe
#'
#' @examples
#' as.data.frame(getDesignCharacteristics(getDesignGroupSequential()))
#'
#' @export
#'
#' @keywords internal
#'
as.data.frame.TrialDesignCharacteristics <- function(x, row.names = NULL,
        optional = FALSE, niceColumnNamesEnabled = FALSE, includeAllParameters = FALSE, ...) {
    if (x$.design$kMax > 1) {
        parameterNamesToBeExcluded <- c("nFixed", "shift")
    } else {
        parameterNamesToBeExcluded <- c("inflationFactor")
    }
    return(.getAsDataFrame(parameterSet = x, 
        parameterNames = parameterNamesToBeExcluded,
        niceColumnNamesEnabled = niceColumnNamesEnabled, 
        includeAllParameters = includeAllParameters,
        handleParameterNamesAsToBeExcluded = TRUE,
        tableColumnNames = .getTableColumnNames(design = x$.design)
    ))
}

#'
#' @name TrialDesignFisher
#'
#' @title
#' Fisher Design
#'
#' @description
#' Trial design for Fisher's combination test.
#' 
#' @template field_kMax
#' @template field_alpha
#' @template field_stages
#' @template field_informationRates
#' @template field_userAlphaSpending
#' @template field_criticalValues
#' @template field_stageLevels
#' @template field_alphaSpent
#' @template field_bindingFutility
#' @template field_tolerance
#' @field method "equalAlpha", "fullAlpha", "noInteraction", or "userDefinedAlpha", default is "equalAlpha" (for details, see Wassmer, 1999)
#' @field alpha0Vec Stopping for futility bounds for stage-wise p-values.
#' @field scale Is a numeric vector of length \code{kMax}-1 that applies to Fisher's design with unequally spaced information rates.
#' @field nonStochasticCurtailment Logical. If \code{TRUE}, the stopping rule is based on the phenomenon of non-stochastic curtailment rather than stochastic reasoning.
#' @template field_sided
#' @field simAlpha The observed alpha error, if simulations have been performed. Is a numeric vector of length 1.
#' @template field_iterations
#' @template field_seed
#'
#' @details
#' This object should not be created directly; use \code{\link[=getDesignFisher]{getDesignFisher()}}
#' with suitable arguments to create a Fisher combination test design.
#'
#' @seealso \code{\link[=getDesignFisher]{getDesignFisher()}} for creating a Fisher combination test design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
TrialDesignFisher <- setRefClass(C_CLASS_NAME_TRIAL_DESIGN_FISHER,
    contains = "TrialDesign",
    fields = list(
        method = "character",
        alpha0Vec = "numeric",
        scale = "numeric",
        nonStochasticCurtailment = "logical",
        sided = "integer",
        simAlpha = "numeric",
        iterations = "integer",
        seed = "numeric"
    ),
    methods = list(
        initialize = function(...,
                method = NA_character_,
                alpha0Vec = NA_real_,
                scale = NA_real_,
                nonStochasticCurtailment = FALSE,
                sided = as.integer(C_SIDED_DEFAULT),
                simAlpha = NA_real_,
                iterations = 0L,
                seed = NA_real_,
                tolerance = C_ANALYSIS_TOLERANCE_FISHER_DEFAULT) {
            callSuper(...,
                method = method,
                alpha0Vec = alpha0Vec,
                scale = scale,
                nonStochasticCurtailment = nonStochasticCurtailment,
                sided = sided,
                simAlpha = simAlpha,
                iterations = iterations,
                seed = seed,
                tolerance = tolerance
            )

            .parameterNames <<- c(.parameterNames, .getSubListByNames(
                .getParameterNames(design = .self), c(
                    "method",
                    "alpha0Vec",
                    "scale",
                    "nonStochasticCurtailment",
                    "sided",
                    "simAlpha",
                    "iterations",
                    "seed"
                )
            ))

            .parameterFormatFunctions$criticalValues <<- ".formatCriticalValuesFisher"

            .initParameterTypes()
            .setParameterType("iterations", C_PARAM_NOT_APPLICABLE)
            .setParameterType("seed", C_PARAM_NOT_APPLICABLE)
            .initStages()
        },
        hasChanged = function(kMax, alpha, sided, method, informationRates, alpha0Vec, userAlphaSpending, bindingFutility) {
            informationRatesTemp <- informationRates
            if (any(is.na(informationRatesTemp))) {
                informationRatesTemp <- .getInformationRatesDefault(kMax)
            }
            alpha0VecTemp <- alpha0Vec[1:(kMax - 1)]
            if (any(is.na(alpha0VecTemp))) {
                alpha0VecTemp <- rep(C_FUTILITY_BOUNDS_DEFAULT, kMax - 1)
            }

            if (!identical(kMax, .self$kMax)) {
                return(TRUE)
            }
            if (!identical(alpha, .self$alpha)) {
                return(TRUE)
            }
            if (!identical(sided, .self$sided)) {
                return(TRUE)
            }
            if (!identical(method, .self$method)) {
                return(TRUE)
            }
            if (!identical(informationRatesTemp, .self$informationRates)) {
                return(TRUE)
            }
            if (!identical(alpha0VecTemp, .self$alpha0Vec)) {
                return(TRUE)
            }
            if (!identical(userAlphaSpending, .self$userAlphaSpending)) {
                return(TRUE)
            }
            if (!identical(bindingFutility, .self$bindingFutility)) {
                return(TRUE)
            }
            return(FALSE)
        },

        # Defines the order of the parameter output
        .getParametersToShow = function() {
            return(c(
                "method",
                "kMax",
                "stages",
                "informationRates",
                "alpha",
                "alpha0Vec",
                "bindingFutility",
                "sided",
                "tolerance",
                "iterations",
                "seed",
                "alphaSpent",
                "userAlphaSpending",
                "criticalValues",
                "stageLevels",
                "scale",
                "simAlpha",
                "nonStochasticCurtailment"
            ))
        }
    )
)

#'
#' @name TrialDesignInverseNormal
#'
#' @title
#' Inverse Normal Design
#'
#' @description
#' Trial design for inverse normal method.
#' 
#' @template field_kMax
#' @template field_alpha
#' @template field_stages
#' @template field_informationRates
#' @template field_userAlphaSpending
#' @template field_criticalValues
#' @template field_stageLevels
#' @template field_alphaSpent
#' @template field_bindingFutility
#' @template field_tolerance
#' @template field_typeOfDesign
#' @template field_beta
#' @template field_deltaWT
#' @template field_deltaPT1
#' @template field_deltaPT0
#' @template field_futilityBounds
#' @template field_gammaA
#' @template field_gammaB
#' @template field_optimizationCriterion
#' @template field_sided
#' @template field_betaSpent
#' @template field_typeBetaSpending
#' @template field_userBetaSpending
#' @template field_power
#' @template field_twoSidedPower
#' @template field_constantBoundsHP
#' @template field_betaAdjustment
#' @template field_delayedInformation
#' @template field_decisionCriticalValues
#' @template field_reversalProbabilities
#'
#' @details
#' This object should not be created directly; use \code{\link[=getDesignInverseNormal]{getDesignInverseNormal()}}
#' with suitable arguments to create a inverse normal design.
#'
#' @seealso \code{\link[=getDesignInverseNormal]{getDesignInverseNormal()}} for creating a inverse normal design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
TrialDesignInverseNormal <- setRefClass(C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL,
    contains = "TrialDesign",
    fields = list(
        typeOfDesign = "character",
        beta = "numeric",
        deltaWT = "numeric",
        deltaPT1 = "numeric",
        deltaPT0 = "numeric",
        futilityBounds = "numeric",
        gammaA = "numeric",
        gammaB = "numeric",
        optimizationCriterion = "character",
        sided = "integer",
        betaSpent = "numeric",
        typeBetaSpending = "character",
        userBetaSpending = "numeric",
        power = "numeric",
        twoSidedPower = "logical",
        constantBoundsHP = "numeric",
        betaAdjustment = "logical",
        delayedInformation = "numeric",
        decisionCriticalValues = "numeric",
        reversalProbabilities = "numeric"
    ),
    methods = list(
        initialize = function(...,
                beta = C_BETA_DEFAULT,
                betaSpent = NA_real_,
                sided = C_SIDED_DEFAULT,
                futilityBounds = NA_real_,
                typeOfDesign = C_DEFAULT_TYPE_OF_DESIGN,
                deltaWT = NA_real_,
                deltaPT1 = NA_real_,
                deltaPT0 = NA_real_,
                optimizationCriterion = C_OPTIMIZATION_CRITERION_DEFAULT,
                gammaA = NA_real_,
                gammaB = NA_real_,
                typeBetaSpending = C_TYPE_OF_DESIGN_BS_NONE,
                userBetaSpending = NA_real_,
                power = NA_real_,
                twoSidedPower = C_TWO_SIDED_POWER_DEFAULT,
                constantBoundsHP = NA_real_,
                betaAdjustment = TRUE, # impl as constant
                delayedInformation = NA_real_) {
            callSuper(...,
                beta = beta,
                betaSpent = betaSpent,
                sided = sided,
                futilityBounds = futilityBounds,
                typeOfDesign = typeOfDesign,
                deltaWT = deltaWT,
                deltaPT1 = deltaPT1,
                deltaPT0 = deltaPT0,
                optimizationCriterion = optimizationCriterion,
                gammaA = gammaA,
                gammaB = gammaB,
                typeBetaSpending = typeBetaSpending,
                userBetaSpending = userBetaSpending,
                power = power,
                twoSidedPower = twoSidedPower,
                constantBoundsHP = constantBoundsHP,
                betaAdjustment = betaAdjustment,
                delayedInformation = delayedInformation
            )

            .initParameterNames()
            .parameterFormatFunctions$criticalValues <<- ".formatCriticalValues"
            .initParameterTypes()
            .initStages()

            .setParameterType("betaAdjustment", C_PARAM_NOT_APPLICABLE)
            .setParameterType("delayedInformation", C_PARAM_NOT_APPLICABLE)
            .setParameterType("decisionCriticalValues", C_PARAM_NOT_APPLICABLE)
            .setParameterType("reversalProbabilities", C_PARAM_NOT_APPLICABLE)
        },
        .initParameterNames = function() {
            .parameterNames <<- c(.parameterNames, .getSubListByNames(
                .getParameterNames(design = .self), c(
                    "beta",
                    "betaSpent",
                    "sided",
                    "futilityBounds",
                    "typeOfDesign",
                    "deltaWT",
                    "deltaPT1",
                    "deltaPT0",
                    "optimizationCriterion",
                    "gammaA",
                    "gammaB",
                    "typeBetaSpending",
                    "userBetaSpending",
                    "power",
                    "twoSidedPower",
                    "constantBoundsHP",
                    "betaAdjustment",
                    "delayedInformation",
                    "decisionCriticalValues",
                    "reversalProbabilities"
                )
            ))
        },
        .formatComparisonResult = function(x) {
            if (is.null(x) || length(x) == 0 || !is.numeric(x)) {
                return(x)
            }

            s <- sprintf("%.9f", x)
            s <- sub("\\.0+", "", s)
            return(s)
        },
        .pasteComparisonResult = function(name, newValue, oldValue) {
            return(paste0(
                name, "_new = ", .arrayToString(.formatComparisonResult(newValue)), " (", .getClassName(newValue), "), ",
                name, "_old = ", .arrayToString(.formatComparisonResult(oldValue)), " (", .getClassName(oldValue), ")"
            ))
        },
        hasChanged = function(...,
                kMax,
                alpha,
                beta,
                sided,
                typeOfDesign,
                deltaWT,
                deltaPT1,
                deltaPT0,
                informationRates,
                futilityBounds,
                optimizationCriterion,
                typeBetaSpending,
                gammaA,
                gammaB,
                bindingFutility,
                userAlphaSpending,
                userBetaSpending,
                twoSidedPower,
                constantBoundsHP,
                betaAdjustment = TRUE,
                delayedInformation = NA_real_) {
            informationRatesTemp <- informationRates
            if (any(is.na(informationRatesTemp))) {
                informationRatesTemp <- .getInformationRatesDefault(kMax)
            }
            futilityBoundsTemp <- futilityBounds[1:(kMax - 1)]
            if (any(is.na(futilityBoundsTemp))) {
                futilityBoundsTemp <- rep(C_FUTILITY_BOUNDS_DEFAULT, kMax - 1)
            }

            if (!identical(kMax, .self$kMax)) {
                return(.pasteComparisonResult("kMax", kMax, .self$kMax))
            }
            if (!identical(alpha, .self$alpha)) {
                return(.pasteComparisonResult("alpha", alpha, .self$alpha))
            }
            if (!identical(beta, .self$beta)) {
                return(.pasteComparisonResult("beta", beta, .self$beta))
            }
            if (!identical(sided, .self$sided)) {
                return(.pasteComparisonResult("sided", sided, .self$sided))
            }
            if (!identical(twoSidedPower, .self$twoSidedPower)) {
                return(.pasteComparisonResult("twoSidedPower", twoSidedPower, .self$twoSidedPower))
            }
            if (kMax == 1) {
                return(FALSE)
            }

            if (!identical(betaAdjustment, .self$betaAdjustment)) {
                return(.pasteComparisonResult("betaAdjustment", betaAdjustment, .self$betaAdjustment))
            }
            if (!identical(delayedInformation, .self$delayedInformation)) {
                return(.pasteComparisonResult("delayedInformation", delayedInformation, .self$delayedInformation))
            }
            if (!identical(typeOfDesign, .self$typeOfDesign)) {
                return(.pasteComparisonResult("typeOfDesign", typeOfDesign, .self$typeOfDesign))
            }
            if (typeOfDesign == C_TYPE_OF_DESIGN_WT) {
                if (!identical(deltaWT, .self$deltaWT)) {
                    return(.pasteComparisonResult("deltaWT", deltaWT, .self$deltaWT))
                }
            }
            if (typeOfDesign == C_TYPE_OF_DESIGN_PT) {
                if (!identical(deltaPT1, .self$deltaPT1)) {
                    return(.pasteComparisonResult("deltaPT1", deltaPT1, .self$deltaPT1))
                }
                if (!identical(deltaPT0, .self$deltaPT0)) {
                    return(.pasteComparisonResult("deltaPT0", deltaPT0, .self$deltaPT0))
                }
            }
            if (!identical(informationRatesTemp, .self$informationRates)) {
                return(.pasteComparisonResult("informationRates", informationRatesTemp, .self$informationRates))
            }
            if (.getParameterType("futilityBounds") != C_PARAM_GENERATED &&
                    (!grepl("^as.*", typeOfDesign) || typeBetaSpending == C_TYPE_OF_DESIGN_BS_NONE) &&
                    !identical(futilityBoundsTemp, .self$futilityBounds)) {
                return(.pasteComparisonResult("futilityBounds", futilityBoundsTemp, .self$futilityBounds))
            }
            if (!identical(optimizationCriterion, .self$optimizationCriterion)) {
                return(.pasteComparisonResult("optimizationCriterion", optimizationCriterion, .self$optimizationCriterion))
            }
            if (!identical(typeBetaSpending, .self$typeBetaSpending)) {
                return(.pasteComparisonResult("typeBetaSpending", typeBetaSpending, .self$typeBetaSpending))
            }
            if (!identical(gammaA, .self$gammaA)) {
                return(.pasteComparisonResult("gammaA", gammaA, .self$gammaA))
            }
            if (!identical(gammaB, .self$gammaB)) {
                return(.pasteComparisonResult("gammaB", gammaB, .self$gammaB))
            }
            if ((typeOfDesign == C_TYPE_OF_DESIGN_PT && !identical(bindingFutility, .self$bindingFutility)) ||
                    (!identical(bindingFutility, .self$bindingFutility) &&
                        .getParameterType("futilityBounds") != C_PARAM_NOT_APPLICABLE &&
                        (sided == 1 || !grepl("^as.*", typeOfDesign) || typeBetaSpending == C_TYPE_OF_DESIGN_BS_NONE) &&
                        (any(na.omit(futilityBounds) > -6) || any(na.omit(.self$futilityBounds) > -6))
                    )) {
                return(.pasteComparisonResult("bindingFutility", bindingFutility, .self$bindingFutility))
            }
            if (!identical(userAlphaSpending, .self$userAlphaSpending)) {
                return(.pasteComparisonResult("userAlphaSpending", userAlphaSpending, .self$userAlphaSpending))
            }
            if (!identical(userBetaSpending, .self$userBetaSpending)) {
                return(.pasteComparisonResult("userBetaSpending", userBetaSpending, .self$userBetaSpending))
            }
            if (!identical(twoSidedPower, .self$twoSidedPower)) {
                return(.pasteComparisonResult("twoSidedPower", twoSidedPower, .self$twoSidedPower))
            }
            if (typeOfDesign == C_TYPE_OF_DESIGN_HP) {
                if (!identical(constantBoundsHP, .self$constantBoundsHP)) {
                    return(.pasteComparisonResult("constantBoundsHP", constantBoundsHP, .self$constantBoundsHP))
                }
            }
            return(FALSE)
        },

        # Defines the order of the parameter output
        .getParametersToShow = function() {
            return(c(
                "typeOfDesign",
                "kMax",
                "stages",
                "informationRates",
                "alpha",
                "beta",
                "power",
                "twoSidedPower",
                "deltaWT",
                "deltaPT1",
                "deltaPT0",
                "futilityBounds",
                "bindingFutility",
                "constantBoundsHP",
                "gammaA",
                "gammaB",
                "optimizationCriterion",
                "sided",
                "betaAdjustment",
                "delayedInformation",
                "tolerance",
                "alphaSpent",
                "userAlphaSpending",
                "betaSpent",
                "typeBetaSpending",
                "userBetaSpending",
                "criticalValues",
                "stageLevels",
                "decisionCriticalValues",
                "reversalProbabilities"
            ))
        }
    )
)

#'
#' @name TrialDesignGroupSequential
#'
#' @title
#' Group Sequential Design
#'
#' @description
#' Trial design for group sequential design.
#' 
#' @template field_kMax
#' @template field_alpha
#' @template field_stages
#' @template field_informationRates
#' @template field_userAlphaSpending
#' @template field_criticalValues
#' @template field_stageLevels
#' @template field_alphaSpent
#' @template field_bindingFutility
#' @template field_tolerance
#' @template field_typeOfDesign
#' @template field_beta
#' @template field_deltaWT
#' @template field_deltaPT1
#' @template field_deltaPT0
#' @template field_futilityBounds
#' @template field_gammaA
#' @template field_gammaB
#' @template field_optimizationCriterion
#' @template field_sided
#' @template field_betaSpent
#' @template field_typeBetaSpending
#' @template field_userBetaSpending
#' @template field_power
#' @template field_twoSidedPower
#' @template field_constantBoundsHP
#' @template field_betaAdjustment
#' @template field_delayedInformation
#' @template field_decisionCriticalValues
#' @template field_reversalProbabilities
#'
#' @details
#' This object should not be created directly; use \code{\link[=getDesignGroupSequential]{getDesignGroupSequential()}}
#' with suitable arguments to create a group sequential design.
#'
#' @seealso \code{\link[=getDesignGroupSequential]{getDesignGroupSequential()}} for creating a group sequential design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
TrialDesignGroupSequential <- setRefClass(
    C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL,
    contains = C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL,
    methods = list(
        initialize = function(...) {
            callSuper(...)
            .parameterFormatFunctions$criticalValues <<- ".formatCriticalValues"
            .initStages()
        },
        show = function(showType = 1, digits = NA_integer_) {
            "Method for automatically printing trial design objects"
            callSuper(showType = showType, digits = digits)
        }
    )
)

#'
#' @name TrialDesignConditionalDunnett
#'
#' @title
#' Conditional Dunnett Design
#'
#' @description
#' Trial design for conditional Dunnett tests.
#' 
#' @template field_kMax
#' @template field_alpha
#' @template field_stages
#' @template field_informationRates
#' @template field_userAlphaSpending
#' @template field_criticalValues
#' @template field_stageLevels
#' @template field_alphaSpent
#' @template field_bindingFutility
#' @template field_tolerance
#' @template field_informationAtInterim 
#' @field secondStageConditioning Logical. The way the second stage p-values are calculated within 
#' the closed system of hypotheses. If secondStageConditioning = FALSE is specified, the unconditional adjusted p-values are used, 
#' otherwise conditional adjusted p-values are calculated, default is secondStageConditioning = TRUE.
#' @template field_sided
#'
#' @details
#' This object should not be created directly; use \code{\link[=getDesignConditionalDunnett]{getDesignConditionalDunnett()}}
#' with suitable arguments to create a conditional Dunnett test design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
#' @seealso \code{\link[=getDesignConditionalDunnett]{getDesignConditionalDunnett()}} for creating a conditional Dunnett test design.

TrialDesignConditionalDunnett <- setRefClass(
    C_CLASS_NAME_TRIAL_DESIGN_CONDITIONAL_DUNNETT,
    contains = "TrialDesign",
    fields = list(
        informationAtInterim = "numeric",
        secondStageConditioning = "logical",
        sided = "integer"
    ),
    methods = list(
        initialize = function(...) {
            callSuper(...)

            notApplicableParameters <- c(
                "kMax",
                "stages",
                "informationRates",
                "userAlphaSpending",
                "criticalValues",
                "stageLevels",
                "alphaSpent",
                "bindingFutility",
                "tolerance"
            )
            for (notApplicableParameter in notApplicableParameters) {
                .setParameterType(notApplicableParameter, C_PARAM_NOT_APPLICABLE)
            }
            .setParameterType("alpha", ifelse(
                identical(alpha, C_ALPHA_DEFAULT), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
            ))
            .setParameterType("informationAtInterim", ifelse(
                identical(informationAtInterim, 0.5), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
            ))
            .setParameterType("secondStageConditioning", ifelse(
                identical(secondStageConditioning, TRUE), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
            ))

            kMax <<- 2L
            sided <<- 1L

            .initStages()
        },
        show = function(showType = 1, digits = NA_integer_) {
            "Method for automatically printing trial design objects"
            callSuper(showType = showType, digits = digits)
        }
    )
)

#'
#' @title
#' Get Design Conditional Dunnett Test
#'
#' @description
#' Defines the design to perform an analysis with the conditional Dunnett test.
#'
#' @inheritParams param_alpha
#' @param informationAtInterim The information to be expected at interim, default is \code{informationAtInterim = 0.5}.
#' @param secondStageConditioning The way the second stage p-values are calculated within the closed system of hypotheses.
#'        If \code{secondStageConditioning = FALSE} is specified, the unconditional adjusted p-values are used, otherwise
#'  	  conditional adjusted p-values are calculated, default is \code{secondStageConditioning = TRUE}
#'        (for details, see Koenig et al., 2008).
#'
#' @details
#' For performing the conditional Dunnett test the design must be defined through this function.
#' You can define the information fraction and the way of how to compute the second stage
#' p-values only in the design definition, and not in the analysis call.\cr
#' See \code{\link[=getClosedConditionalDunnettTestResults]{getClosedConditionalDunnettTestResults()}} 
#' for an example and Koenig et al. (2008) and
#' Wassmer & Brannath (2016), chapter 11 for details of the test procedure.
#'
#' @template return_object_trial_design
#' @template how_to_get_help_for_generics
#'
#' @family design functions
#'
#' @export
#'
getDesignConditionalDunnett <- function(alpha = 0.025, # C_ALPHA_DEFAULT
        informationAtInterim = 0.5, secondStageConditioning = TRUE) {
    .assertIsValidAlpha(alpha)
    .assertIsSingleNumber(informationAtInterim, "informationAtInterim")
    .assertIsInOpenInterval(informationAtInterim, "informationAtInterim", 0, 1)
    return(TrialDesignConditionalDunnett(
        alpha = alpha,
        informationAtInterim = informationAtInterim,
        secondStageConditioning = secondStageConditioning
    ))
}

#'
#' @title
#' Trial Design Plotting
#'
#' @description
#' Plots a trial design.
#'
#' @details
#' Generic function to plot a trial design.
#'
#' @param x The trial design, obtained from \cr
#'        \code{\link[=getDesignGroupSequential]{getDesignGroupSequential()}}, \cr
#'        \code{\link[=getDesignInverseNormal]{getDesignInverseNormal()}} or \cr
#'        \code{\link[=getDesignFisher]{getDesignFisher()}}.
#' @param y Not available for this kind of plot (is only defined to be compatible to the generic plot function).
#' @param main The main title.
#' @param xlab The x-axis label.
#' @param ylab The y-axis label.
#' @inheritParams param_palette
#' @inheritParams param_theta
#' @inheritParams param_nMax
#' @inheritParams param_plotPointsEnabled
#' @inheritParams param_showSource
#' @inheritParams param_plotSettings
#' @inheritParams param_legendPosition
#' @inheritParams param_grid
#' @param type The plot type (default = \code{1}). The following plot types are available:
#' \itemize{
#'   \item \code{1}: creates a 'Boundaries' plot
#'   \item \code{3}: creates a 'Stage Levels' plot
#'   \item \code{4}: creates a 'Error Spending' plot
#'   \item \code{5}: creates a 'Power and Early Stopping' plot
#'   \item \code{6}: creates an 'Average Sample Size and Power / Early Stop' plot
#'   \item \code{7}: creates an 'Power' plot
#'   \item \code{8}: creates an 'Early Stopping' plot
#'   \item \code{9}: creates an 'Average Sample Size' plot
#'   \item \code{"all"}: creates all available plots and returns it as a grid plot or list
#' }
#' @inheritParams param_three_dots_plot
#'
#' @details
#' Generic function to plot a trial design.
#'
#' Note that \code{\link[=param_nMax]{nMax}} is not an argument that it passed to \code{ggplot2}.
#' Rather, the underlying calculations (e.g. power for different theta's or average sample size) are based
#' on calls to function \code{\link[=getPowerAndAverageSampleNumber]{getPowerAndAverageSampleNumber()}} 
#' which has argument \code{\link[=param_nMax]{nMax}}.
#' I.e., \code{\link[=param_nMax]{nMax}} is not an argument to ggplot2 but to 
#' \code{\link[=getPowerAndAverageSampleNumber]{getPowerAndAverageSampleNumber()}}
#' which is called prior to plotting.
#'
#' @seealso \code{\link[=plot.TrialDesignSet]{plot()}} to compare different designs or design parameters visual.
#'
#' @template return_object_ggplot
#'
#' @examples
#' \dontrun{
#' design <- getDesignInverseNormal(
#'     kMax = 3, alpha = 0.025,
#'     typeOfDesign = "asKD", gammaA = 2,
#'     informationRates = c(0.2, 0.7, 1),
#'     typeBetaSpending = "bsOF"
#' )
#' if (require(ggplot2)) {
#'     plot(design) # default: type = 1
#' }
#' }
#'
#' @export
#'
plot.TrialDesign <- function(x, y, ..., main = NA_character_,
        xlab = NA_character_, ylab = NA_character_, type = 1L, palette = "Set1",
        theta = seq(-1, 1, 0.01), nMax = NA_integer_, plotPointsEnabled = NA,
        legendPosition = NA_integer_, showSource = FALSE,
        grid = 1, plotSettings = NULL) {
    fCall <- match.call(expand.dots = FALSE)
    designName <- deparse(fCall$x)
    .assertGgplotIsInstalled()
    .assertIsSingleInteger(grid, "grid", validateType = FALSE)
    typeNumbers <- .getPlotTypeNumber(type, x)
    if (is.null(plotSettings)) {
        plotSettings <- .getGridPlotSettings(x, typeNumbers, grid)
    }
    p <- NULL
    plotList <- list()
    for (typeNumber in typeNumbers) {
        p <- .plotTrialDesign(
            x = x, y = y, main = main,
            xlab = xlab, ylab = ylab, type = typeNumber, palette = palette,
            theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
            legendPosition = .getGridLegendPosition(legendPosition, typeNumbers, grid),
            showSource = showSource, designName = designName,
            plotSettings = plotSettings, ...
        )
        .printPlotShowSourceSeparator(showSource, typeNumber, typeNumbers)
        if (length(typeNumbers) > 1) {
            caption <- .getPlotCaption(x, typeNumber, stopIfNotFound = TRUE)
            plotList[[caption]] <- p
        }
    }
    if (length(typeNumbers) == 1) {
        if (.isSpecialPlotShowSourceArgument(showSource)) {
            return(invisible(p))
        }

        return(p)
    }

    if (.isSpecialPlotShowSourceArgument(showSource)) {
        return(invisible(plotList))
    }

    return(.createPlotResultObject(plotList, grid))
}

.plotTrialDesign <- function(..., x, y, main,
        xlab, ylab, type, palette,
        theta, nMax, plotPointsEnabled,
        legendPosition, showSource, designName, plotSettings = NULL) {
    .assertGgplotIsInstalled()

    .assertIsSingleInteger(type, "type", naAllowed = FALSE, validateType = FALSE)
    if (any(.isTrialDesignFisher(x)) && !(type %in% c(1, 3, 4))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'type' (", type, ") is not allowed for Fisher designs; must be 1, 3 or 4"
        )
    }

    .warnInCaseOfUnknownArguments(
        functionName = "plot",
        ignore = c("xlim", "ylim", "companyAnnotationEnabled", "variedParameters"), ...
    )

    if ((type < 5 || type > 9) && !identical(theta, seq(-1, 1, 0.01))) {
        warning("'theta' (", .reconstructSequenceCommand(theta), ") will be ignored for plot type ", type, call. = FALSE)
    }

    if (!missing(y) && !is.null(y) && length(y) == 1 && inherits(y, "TrialDesign")) {
        args <- list(...)
        variedParameters <- args[["variedParameters"]]
        if (is.null(variedParameters)) {
            if (.isTrialDesignInverseNormalOrGroupSequential(x) &&
                    .isTrialDesignInverseNormalOrGroupSequential(y) &&
                    x$typeOfDesign != y$typeOfDesign) {
                variedParameters <- "typeOfDesign"
            } else {
                stop(
                    C_EXCEPTION_TYPE_MISSING_ARGUMENT,
                    "'variedParameters' needs to be specified, e.g., variedParameters = \"typeOfDesign\""
                )
            }
        }
        designSet <- getDesignSet(designs = c(x, y), variedParameters = variedParameters)
    } else {
        designSet <- TrialDesignSet(design = x, singleDesign = TRUE)
        if (!is.null(plotSettings)) {
            designSet$.plotSettings <- plotSettings
        }
    }

    .plotTrialDesignSet(
        x = designSet, y = y, main = main, xlab = xlab, ylab = ylab, type = type,
        palette = palette, theta = theta, nMax = nMax,
        plotPointsEnabled = plotPointsEnabled, legendPosition = legendPosition,
        showSource = showSource, designSetName = designName, ...
    )
}

#'
#' @title
#' Coerce TrialDesign to a Data Frame
#'
#' @description
#' Returns the \code{TrialDesign} as data frame.
#'
#' @param x A \code{\link{TrialDesign}} object.
#' @inheritParams param_niceColumnNamesEnabled
#' @inheritParams param_includeAllParameters
#' @inheritParams param_three_dots
#'
#' @details
#' Each element of the \code{\link{TrialDesign}} is converted to a column in the data frame.
#'
#' @template return_dataframe
#'
#' @examples
#' as.data.frame(getDesignGroupSequential())
#'
#' @export
#'
#' @keywords internal
#'
as.data.frame.TrialDesign <- function(x, row.names = NULL,
        optional = FALSE, niceColumnNamesEnabled = FALSE, includeAllParameters = FALSE, ...) {
    .assertIsTrialDesign(x)

    if (includeAllParameters) {
        parameterNames <- NULL
    } else {
        parameterNames <- x$.getParametersToShow()
    }
    return(.getAsDataFrame(parameterSet = x, 
        parameterNames = parameterNames,
        niceColumnNamesEnabled = niceColumnNamesEnabled,
        includeAllParameters = includeAllParameters,
        tableColumnNames = .getTableColumnNames(design = x)
    ))
}
