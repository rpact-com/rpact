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
## |  File version: $Revision: 8578 $
## |  Last changed: $Date: 2025-03-04 08:17:05 +0100 (Di, 04 Mrz 2025) $
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
#'   \item \code{\link{TrialDesignGroupSequential}},
#'   \item \code{\link{TrialDesignInverseNormal}}, and
#'   \item \code{\link{TrialDesignConditionalDunnett}}.
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
TrialDesign <- R6::R6Class("TrialDesign",
    inherit = ParameterSet,
    public = list(
        .plotSettings = NULL,
        kMax = NULL,
        alpha = NULL,
        stages = NULL,
        informationRates = NULL,
        userAlphaSpending = NULL,
        criticalValues = NULL,
        stageLevels = NULL,
        alphaSpent = NULL,
        bindingFutility = NULL,
        directionUpper = NULL,
        tolerance = NULL,
        initialize = function(...,
                kMax = NA_integer_,
                alpha = NA_real_,
                informationRates = NA_real_,
                userAlphaSpending = NA_real_,
                criticalValues = NA_real_,
                stageLevels = NA_real_,
                alphaSpent = NA_real_,
                bindingFutility = NA,
                directionUpper = NA,
                tolerance = 1e-06 # C_ANALYSIS_TOLERANCE_DEFAULT
                ) {
            self$kMax <- kMax
            self$alpha <- alpha
            self$informationRates <- informationRates
            self$userAlphaSpending <- userAlphaSpending
            self$criticalValues <- criticalValues
            self$stageLevels <- stageLevels
            self$alphaSpent <- alphaSpent
            self$bindingFutility <- bindingFutility
            self$directionUpper <- directionUpper
            self$tolerance <- tolerance
            super$initialize(...)

            self$.plotSettings <- PlotSettings$new()

            self$.initStages()
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing trial design objects"
            self$.resetCat()
            if (showType == 3) {
                .createSummary(self, digits = digits)$.show(
                    showType = 1,
                    digits = digits, consoleOutputEnabled = consoleOutputEnabled
                )
            } else if (showType == 2) {
                super$.show(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                self$.cat("Design parameters and output of ", self$.toString(), ":\n\n",
                    heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getDerivedParameters(), "Derived from user defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getDefaultParameters(), "Default parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getGeneratedParameters(), "Output",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            s <- "unknown trial design"
            if (.isTrialDesignGroupSequential(self)) {
                s <- "group sequential design"
            } else if (.isTrialDesignInverseNormal(self)) {
                s <- "inverse normal combination test design"
            } else if (.isTrialDesignFisher(self)) {
                s <- "Fisher's combination test design"
            } else if (.isTrialDesignConditionalDunnett(self)) {
                s <- "conditional Dunnett test design"
            }
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
        },
        .initStages = function() {
            if (length(self$kMax) == 1 && !is.na(self$kMax) && self$kMax > 0) {
                self$stages <- c(1L:self$kMax)
                if (self$kMax == C_KMAX_DEFAULT) {
                    self$.setParameterType("stages", C_PARAM_DEFAULT_VALUE)
                } else {
                    type <- self$.getParameterType("kMax")
                    self$.setParameterType("stages", ifelse(type != C_PARAM_TYPE_UNKNOWN, type, C_PARAM_USER_DEFINED))
                }
            } else {
                self$.setParameterType("stages", C_PARAM_NOT_APPLICABLE)
            }
        },
        .isDelayedResponseDesign = function() {
            return((inherits(self, "TrialDesignGroupSequential") || inherits(self, "TrialDesignInverseNormal")) &&
                self$kMax > 1 &&
                !is.null(self[["delayedInformation"]]) &&
                !any(is.na(self$delayedInformation)) && any(self$delayedInformation > 0))
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
#' @template field_nFixed
#' @template field_shift
#' @template field_inflationFactor
#' @template field_stages
#' @template field_information
#' @template field_power
#' @template field_rejectionProbabilities
#' @template field_futilityProbabilities
#' @template field_averageSampleNumber1
#' @template field_averageSampleNumber01
#' @template field_averageSampleNumber0
#'
#' @details
#' \code{TrialDesignCharacteristics} contains all fields required
#' to collect the characteristics of a design.
#' This object should not be created directly; use \code{getDesignCharacteristics}
#' with suitable arguments to create it.
#'
#' @seealso \code{\link{getDesignCharacteristics}} for getting the design characteristics.
#'
#' @include class_core_parameter_set.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
TrialDesignCharacteristics <- R6::R6Class("TrialDesignCharacteristics",
    inherit = ParameterSet,
    public = list(
        .design = NULL,
        .probs = NULL,
        nFixed = NULL,
        shift = NULL,
        inflationFactor = NULL,
        stages = NULL,
        information = NULL,
        power = NULL,
        rejectionProbabilities = NULL, # efficacy probabilities
        futilityProbabilities = NULL,
        averageSampleNumber1 = NULL,
        averageSampleNumber01 = NULL,
        averageSampleNumber0 = NULL,
        initialize = function(design, ...) {
            self$.design <- design
            super$initialize(...)
            self$.initStages()
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing trial design characteristics objects"
            self$.resetCat()
            if (showType == 2) {
                super$.show(
                    showType = showType, digits = digits,
                    consoleOutputEnabled = consoleOutputEnabled
                )
            } else {
                self$.showParametersOfOneGroup(self$.getGeneratedParameters(),
                    title = self$.toString(startWithUpperCase = TRUE),
                    orderByParameterName = FALSE,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showUnknownParameters(
                    consoleOutputEnabled = consoleOutputEnabled
                )
            }
        },
        .initStages = function() {
            if (!is.na(self$.design$kMax) && self$.design$kMax > 0) {
                self$stages <- c(1L:self$.design$kMax)
                if (self$.design$kMax == C_KMAX_DEFAULT) {
                    self$.setParameterType("stages", C_PARAM_DEFAULT_VALUE)
                } else {
                    self$.setParameterType("stages", C_PARAM_USER_DEFINED)
                }
            } else {
                self$.setParameterType("stages", C_PARAM_NOT_APPLICABLE)
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            if (self$.design$.isDelayedResponseDesign()) {
                prefix <- "delayed response"
                if (startWithUpperCase) {
                    prefix <- .firstCharacterToUpperCase(prefix)
                }
                return(paste(prefix, self$.design$.toString(startWithUpperCase = FALSE), "characteristics"))
            }

            return(paste(self$.design$.toString(startWithUpperCase = startWithUpperCase), "characteristics"))
        }
    )
)

#'
#' @title
#' Trial Design Characteristics Printing
#'
#' @param x The trial design characteristics object.
#' @param markdown If \code{TRUE}, the object \code{x} will be printed using markdown syntax;
#'        normal representation will be used otherwise (default is \code{FALSE})
#' @param showDesign Show the design print output above the design characteristics, default is \code{TRUE}.
#' @inheritParams param_three_dots_plot
#'
#' @description
#' Prints the design characteristics object.
#'
#' @details
#' Generic function to print all kinds of design characteristics.
#'
#' @export
#'
print.TrialDesignCharacteristics <- function(x, ..., markdown = NA, showDesign = TRUE) {
    sysCalls <- sys.calls()

    if (is.na(markdown)) {
        markdown <- .isMarkdownEnabled("print")
    }

    if (isTRUE(markdown)) {
        if (.isPrintCall(sysCalls)) {
            result <- paste0(utils::capture.output(x$.catMarkdownText()), collapse = "\n")
            return(knitr::asis_output(result))
        }

        if (showDesign) {
            .addObjectToPipeOperatorQueue(x$.design)
        }
        .addObjectToPipeOperatorQueue(x)
        return(invisible(x))
    }

    if (showDesign) {
        print.ParameterSet(x$.design, ..., markdown = markdown)
    }
    print.ParameterSet(x, ..., markdown = markdown)
}

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
#' \dontrun{
#' as.data.frame(getDesignCharacteristics(getDesignGroupSequential()))
#' }
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
    return(.getAsDataFrame(
        parameterSet = x,
        parameterNames = parameterNamesToBeExcluded,
        niceColumnNamesEnabled = niceColumnNamesEnabled,
        includeAllParameters = includeAllParameters,
        handleParameterNamesAsToBeExcluded = TRUE
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
#' @template field_method
#' @template field_alpha0Vec
#' @template field_scale
#' @template field_nonStochasticCurtailment
#' @template field_sided
#' @template field_simAlpha
#' @template field_iterations
#' @template field_seed
#'
#' @details
#' This object should not be created directly; use \code{\link{getDesignFisher}}
#' with suitable arguments to create a Fisher combination test design.
#'
#' @seealso \code{\link{getDesignFisher}} for creating a Fisher combination test design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
TrialDesignFisher <- R6::R6Class("TrialDesignFisher",
    inherit = TrialDesign,
    public = list(
        method = NULL,
        alpha0Vec = NULL,
        scale = NULL,
        nonStochasticCurtailment = NULL,
        sided = NULL,
        simAlpha = NULL,
        iterations = NULL,
        seed = NULL,
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
            self$method <- method
            self$alpha0Vec <- alpha0Vec
            self$scale <- scale
            self$nonStochasticCurtailment <- nonStochasticCurtailment
            self$sided <- sided
            self$simAlpha <- simAlpha
            super$initialize(...) # important: don't move to first line of constructor
            self$iterations <- iterations
            self$seed <- seed
            self$tolerance <- tolerance

            self$.initParameterTypes()
            self$.setParameterType("iterations", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("seed", C_PARAM_NOT_APPLICABLE)
            self$.initStages()
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

            if (!isTRUE(all.equal(kMax, self$kMax))) {
                return(TRUE)
            }
            if (!identical(alpha, self$alpha)) {
                return(TRUE)
            }
            if (!isTRUE(all.equal(sided, self$sided))) {
                return(TRUE)
            }
            if (!identical(method, self$method)) {
                return(TRUE)
            }
            if (!identical(informationRatesTemp, self$informationRates)) {
                return(TRUE)
            }
            if (!identical(alpha0VecTemp, self$alpha0Vec)) {
                return(TRUE)
            }
            if (!identical(userAlphaSpending, self$userAlphaSpending)) {
                return(TRUE)
            }
            if (!identical(bindingFutility, self$bindingFutility)) {
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
                "directionUpper",
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
TrialDesignInverseNormal <- R6::R6Class("TrialDesignInverseNormal",
    inherit = TrialDesign,
    public = list(
        typeOfDesign = NULL,
        beta = NULL,
        deltaWT = NULL,
        deltaPT1 = NULL,
        deltaPT0 = NULL,
        futilityBounds = NULL,
        gammaA = NULL,
        gammaB = NULL,
        optimizationCriterion = NULL,
        sided = NULL,
        betaSpent = NULL,
        typeBetaSpending = NULL,
        userBetaSpending = NULL,
        power = NULL,
        twoSidedPower = NULL,
        constantBoundsHP = NULL,
        betaAdjustment = NULL,
        delayedInformation = NULL,
        decisionCriticalValues = NULL,
        reversalProbabilities = NULL,
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
                betaAdjustment = TRUE,
                delayedInformation = NA_real_) {
            self$beta <- beta
            self$betaSpent <- betaSpent
            self$sided <- sided
            self$futilityBounds <- futilityBounds
            self$typeOfDesign <- typeOfDesign
            self$deltaWT <- deltaWT
            self$deltaPT1 <- deltaPT1
            self$deltaPT0 <- deltaPT0
            self$optimizationCriterion <- optimizationCriterion
            self$gammaA <- gammaA
            self$gammaB <- gammaB
            self$typeBetaSpending <- typeBetaSpending
            self$userBetaSpending <- userBetaSpending
            self$power <- power
            self$twoSidedPower <- twoSidedPower
            self$constantBoundsHP <- constantBoundsHP
            self$betaAdjustment <- betaAdjustment
            self$delayedInformation <- delayedInformation
            super$initialize(...)

            self$.initParameterTypes()
            self$.initStages()

            self$.setParameterType("betaAdjustment", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("delayedInformation", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("decisionCriticalValues", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("reversalProbabilities", C_PARAM_NOT_APPLICABLE)
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
                name, "_new = ", .arrayToString(self$.formatComparisonResult(newValue)), " (", .getClassName(newValue), "), ",
                name, "_old = ", .arrayToString(self$.formatComparisonResult(oldValue)), " (", .getClassName(oldValue), ")"
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

            if (!isTRUE(all.equal(kMax, self$kMax))) {
                return(self$.pasteComparisonResult("kMax", kMax, self$kMax))
            }
            if (!identical(alpha, self$alpha)) {
                return(self$.pasteComparisonResult("alpha", alpha, self$alpha))
            }
            if (!identical(beta, self$beta)) {
                return(self$.pasteComparisonResult("beta", beta, self$beta))
            }
            if (!isTRUE(all.equal(sided, self$sided))) {
                return(self$.pasteComparisonResult("sided", sided, self$sided))
            }
            if (!identical(twoSidedPower, self$twoSidedPower)) {
                return(self$.pasteComparisonResult("twoSidedPower", twoSidedPower, self$twoSidedPower))
            }
            if (kMax == 1) {
                return(FALSE)
            }

            if (!identical(betaAdjustment, self$betaAdjustment)) {
                return(self$.pasteComparisonResult(
                    "betaAdjustment",
                    betaAdjustment, self$betaAdjustment
                ))
            }
            if (!identical(delayedInformation, self$delayedInformation)) {
                return(self$.pasteComparisonResult(
                    "delayedInformation",
                    delayedInformation, self$delayedInformation
                ))
            }
            if (!identical(typeOfDesign, self$typeOfDesign)) {
                return(self$.pasteComparisonResult("typeOfDesign", typeOfDesign, self$typeOfDesign))
            }
            if (typeOfDesign == C_TYPE_OF_DESIGN_WT) {
                if (!identical(deltaWT, self$deltaWT)) {
                    return(self$.pasteComparisonResult("deltaWT", deltaWT, self$deltaWT))
                }
            }
            if (typeOfDesign == C_TYPE_OF_DESIGN_PT) {
                if (!identical(deltaPT1, self$deltaPT1)) {
                    return(self$.pasteComparisonResult("deltaPT1", deltaPT1, self$deltaPT1))
                }
                if (!identical(deltaPT0, self$deltaPT0)) {
                    return(self$.pasteComparisonResult("deltaPT0", deltaPT0, self$deltaPT0))
                }
            }
            if (!identical(informationRatesTemp, self$informationRates)) {
                return(self$.pasteComparisonResult(
                    "informationRates",
                    informationRatesTemp, self$informationRates
                ))
            }
            if (!self$isGeneratedParameter("futilityBounds") &&
                    (!grepl("^as.*", typeOfDesign) || typeBetaSpending == C_TYPE_OF_DESIGN_BS_NONE) &&
                    !identical(futilityBoundsTemp, self$futilityBounds)) {
                return(self$.pasteComparisonResult(
                    "futilityBounds",
                    futilityBoundsTemp, self$futilityBounds
                ))
            }
            if (!identical(optimizationCriterion, self$optimizationCriterion)) {
                return(self$.pasteComparisonResult(
                    "optimizationCriterion",
                    optimizationCriterion, self$optimizationCriterion
                ))
            }
            if (!identical(typeBetaSpending, self$typeBetaSpending)) {
                return(self$.pasteComparisonResult(
                    "typeBetaSpending",
                    typeBetaSpending, self$typeBetaSpending
                ))
            }
            if (!identical(gammaA, self$gammaA)) {
                return(self$.pasteComparisonResult("gammaA", gammaA, self$gammaA))
            }
            if (!identical(gammaB, self$gammaB)) {
                return(self$.pasteComparisonResult("gammaB", gammaB, self$gammaB))
            }
            if ((typeOfDesign == C_TYPE_OF_DESIGN_PT && !identical(bindingFutility, self$bindingFutility)) ||
                    (!identical(bindingFutility, self$bindingFutility) &&
                        self$.getParameterType("futilityBounds") != C_PARAM_NOT_APPLICABLE &&
                        (sided == 1 || !grepl("^as.*", typeOfDesign) || typeBetaSpending == C_TYPE_OF_DESIGN_BS_NONE) &&
                        (any(na.omit(futilityBounds) > -6) || any(na.omit(self$futilityBounds) > -6))
                    )) {
                return(self$.pasteComparisonResult(
                    "bindingFutility",
                    bindingFutility, self$bindingFutility
                ))
            }
            if (!identical(userAlphaSpending, self$userAlphaSpending)) {
                return(self$.pasteComparisonResult(
                    "userAlphaSpending",
                    userAlphaSpending, self$userAlphaSpending
                ))
            }
            if (!identical(userBetaSpending, self$userBetaSpending)) {
                return(self$.pasteComparisonResult(
                    "userBetaSpending",
                    userBetaSpending, self$userBetaSpending
                ))
            }
            if (!identical(twoSidedPower, self$twoSidedPower)) {
                return(self$.pasteComparisonResult(
                    "twoSidedPower",
                    twoSidedPower, self$twoSidedPower
                ))
            }
            if (typeOfDesign == C_TYPE_OF_DESIGN_HP) {
                if (!identical(constantBoundsHP, self$constantBoundsHP)) {
                    return(self$.pasteComparisonResult(
                        "constantBoundsHP",
                        constantBoundsHP, self$constantBoundsHP
                    ))
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
                "directionUpper",
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
#' This object should not be created directly;
#' use \code{\link[=getDesignGroupSequential]{getDesignGroupSequential()}}
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
TrialDesignGroupSequential <- R6::R6Class("TrialDesignGroupSequential",
    inherit = TrialDesignInverseNormal,
    public = list(
        initialize = function(...) {
            super$initialize(...)
            self$.initStages()
        },
        show = function(showType = 1, digits = NA_integer_) {
            "Method for automatically printing trial design objects"
            super$show(showType = showType, digits = digits)
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
#' @template field_secondStageConditioning
#' @template field_sided
#'
#' @details
#' This object should not be created directly; use \code{\link{getDesignConditionalDunnett}}
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
#' @seealso \code{\link{getDesignConditionalDunnett}} for creating a conditional Dunnett test design.
#'
TrialDesignConditionalDunnett <- R6::R6Class("TrialDesignConditionalDunnett",
    inherit = TrialDesign,
    public = list(
        informationAtInterim = NULL,
        secondStageConditioning = NULL,
        sided = NULL,
        initialize = function(...,
                informationAtInterim = NULL,
                secondStageConditioning = NULL,
                directionUpper = NA) {
            super$initialize(...)
            self$informationAtInterim <- informationAtInterim
            self$secondStageConditioning <- secondStageConditioning
            self$directionUpper <- directionUpper
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
                self$.setParameterType(notApplicableParameter, C_PARAM_NOT_APPLICABLE)
            }
            self$.setParameterType("alpha", ifelse(
                identical(self$alpha, C_ALPHA_DEFAULT),
                C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
            ))
            self$.setParameterType("informationAtInterim", ifelse(
                identical(self$informationAtInterim, 0.5),
                C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
            ))
            self$.setParameterType("secondStageConditioning", ifelse(
                identical(self$secondStageConditioning, TRUE),
                C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
            ))

            self$kMax <- 2L
            self$sided <- 1L

            self$.initStages()
        },
        show = function(showType = 1, digits = NA_integer_) {
            "Method for automatically printing trial design objects"
            super$show(showType = showType, digits = digits)
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
#' @inheritParams param_directionUpper
#' @inheritParams param_three_dots
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
        informationAtInterim = 0.5,
        ...,
        secondStageConditioning = TRUE,
        directionUpper = NA) {
    .assertIsValidAlpha(alpha)
    .assertIsSingleNumber(informationAtInterim, "informationAtInterim")
    .assertIsInOpenInterval(informationAtInterim, "informationAtInterim", 0, 1)
    .assertIsSingleLogical(directionUpper, "directionUpper", naAllowed = TRUE)
    design <- TrialDesignConditionalDunnett$new(
        alpha = alpha,
        informationAtInterim = informationAtInterim,
        secondStageConditioning = secondStageConditioning,
        directionUpper = directionUpper
    )
    design$.setParameterType("directionUpper", ifelse(!is.na(directionUpper),
        C_PARAM_USER_DEFINED, C_PARAM_NOT_APPLICABLE
    ))
    return(design)
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
plot.TrialDesign <- function(x,
        y,
        ...,
        main = NA_character_,
        xlab = NA_character_,
        ylab = NA_character_,
        type = 1L,
        palette = "Set1",
        theta = seq(-1, 1, 0.01),
        nMax = NA_integer_,
        plotPointsEnabled = NA,
        legendPosition = NA_integer_,
        showSource = FALSE,
        grid = 1,
        plotSettings = NULL) {
    .assertIsValidPlotType(type, naAllowed = FALSE)
    .assertIsSingleInteger(grid, "grid", naAllowed = FALSE, validateType = FALSE)
    markdown <- .getOptionalArgument("markdown", ..., optionalArgumentDefaultValue = NA)
    if (is.na(markdown)) {
        markdown <- .isMarkdownEnabled("plot")
    }

    args <- list(
        x = x,
        y = NULL,
        main = main,
        xlab = xlab,
        ylab = ylab,
        type = type,
        palette = palette,
        theta = theta,
        nMax = nMax,
        plotPointsEnabled = plotPointsEnabled,
        legendPosition = legendPosition,
        showSource = showSource,
        grid = grid,
        plotSettings = plotSettings,
        ...
    )

    if (markdown) {
        sep <- .getMarkdownPlotPrintSeparator()
        if (length(type) > 1 && grid == 1) {
            grid <- 0
            args$grid <- 0
        }
        if (grid > 0) {
            print(do.call(.plot.TrialDesign, args))
        } else {
            do.call(.plot.TrialDesign, args)
        }
        return(.knitPrintQueue(x, sep = sep, prefix = sep))
    }

    return(do.call(.plot.TrialDesign, args))
}

.plot.TrialDesign <- function(x,
        y,
        ...,
        main = NA_character_,
        xlab = NA_character_,
        ylab = NA_character_,
        type = 1L,
        palette = "Set1",
        theta = seq(-1, 1, 0.01),
        nMax = NA_integer_,
        plotPointsEnabled = NA,
        legendPosition = NA_integer_,
        showSource = FALSE,
        grid = 1,
        plotSettings = NULL) {
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
            x = x,
            y = y,
            main = main,
            xlab = xlab,
            ylab = ylab,
            type = typeNumber,
            palette = palette,
            theta = theta,
            nMax = nMax,
            plotPointsEnabled = plotPointsEnabled,
            legendPosition = .getGridLegendPosition(legendPosition, typeNumbers, grid),
            showSource = showSource,
            designName = designName,
            plotSettings = plotSettings,
            ...
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

#' @rdname plot.TrialDesign
#' @export
plot.TrialDesignCharacteristics <- function(x, y, ..., type = 1L, grid = 1) {
    .assertIsValidPlotType(type, naAllowed = FALSE)
    .assertIsSingleInteger(grid, "grid", naAllowed = FALSE, validateType = FALSE)
    markdown <- .getOptionalArgument("markdown", ..., optionalArgumentDefaultValue = NA)
    if (is.na(markdown)) {
        markdown <- .isMarkdownEnabled("plot")
    }

    if (markdown) {
        sep <- .getMarkdownPlotPrintSeparator()
        if (length(type) > 1 && grid == 1) {
            grid <- 0
            args$grid <- 0
        }
        if (grid > 0) {
            print(.plot.TrialDesign(x = x$.design, y = y, type = type, grid = grid, ...))
        } else {
            .plot.TrialDesign(x = x$.design, y = y, type = type, grid = grid, ...)
        }
        return(.knitPrintQueue(x, sep = sep, prefix = sep))
    }

    plot(x = x$.design, y = y, ...)
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
            "'type' (", type, ") is not allowed for Fisher designs; must be 1, 3 or 4",
            call. = FALSE
        )
    }

    .warnInCaseOfUnknownArguments(
        functionName = "plot",
        ignore = c("xlim", "ylim", "companyAnnotationEnabled", "variedParameters"), ...
    )

    if ((type < 5 || type > 9) && !identical(theta, seq(-1, 1, 0.01))) {
        warning("'theta' (", .reconstructSequenceCommand(theta), ") ",
            "will be ignored for plot type ", type,
            call. = FALSE
        )
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
                    "'variedParameters' needs to be specified, ",
                    "e.g., variedParameters = \"typeOfDesign\"",
                    call. = FALSE
                )
            }
        }
        designSet <- getDesignSet(designs = c(x, y), variedParameters = variedParameters)
    } else {
        designSet <- TrialDesignSet$new(design = x, singleDesign = TRUE)
        if (!is.null(plotSettings)) {
            designSet$.plotSettings <- plotSettings
        }
    }

    .plotTrialDesignSet(
        x = designSet,
        y = y,
        main = main,
        xlab = xlab,
        ylab = ylab,
        type = type,
        palette = palette,
        theta = theta,
        nMax = nMax,
        plotPointsEnabled = plotPointsEnabled,
        legendPosition = legendPosition,
        showSource = showSource,
        designSetName = designName,
        ...
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
#' Each element of the \code{\link{TrialDesign}} is
#' converted to a column in the data frame.
#'
#' @template return_dataframe
#'
#' @examples
#' \dontrun{
#' as.data.frame(getDesignGroupSequential())
#' }
#'
#' @export
#'
#' @keywords internal
#'
as.data.frame.TrialDesign <- function(x, row.names = NULL,
        optional = FALSE, niceColumnNamesEnabled = FALSE,
        includeAllParameters = FALSE, ...) {
    .assertIsTrialDesign(x)

    if (includeAllParameters) {
        parameterNames <- NULL
    } else {
        parameterNames <- x$.getParametersToShow()
    }
    return(.getAsDataFrame(
        parameterSet = x,
        parameterNames = parameterNames,
        niceColumnNamesEnabled = niceColumnNamesEnabled,
        includeAllParameters = includeAllParameters
    ))
}
