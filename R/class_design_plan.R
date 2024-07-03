## |
## |  *Trial design plan classes*
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
## |  File version: $Revision: 8023 $
## |  Last changed: $Date: 2024-07-01 08:50:30 +0200 (Mo, 01 Jul 2024) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_constants.R
#' @include f_design_general_utilities.R
NULL

C_VARIABLE_DESIGN_PLAN_PARAMETERS <- c(
    "lambda1", "pi1", "median1",
    "alternative", "hazardRatio", "theta"
)

C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS <- list(
    normalApproximation = FALSE,
    meanRatio = FALSE,
    thetaH0 = 0,
    alternative = seq(0.2, 1, 0.2),
    stDev = 1,
    groups = 2L,
    allocationRatioPlanned = 1
)

C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES <- list(
    normalApproximation = TRUE,
    riskRatio = FALSE,
    thetaH0 = 0,
    pi1 = C_PI_1_SAMPLE_SIZE_DEFAULT,
    pi2 = C_PI_2_DEFAULT,
    groups = 2L,
    allocationRatioPlanned = 1
)

C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL <- list(
    typeOfComputation = "Schoenfeld",
    thetaH0 = 1,
    pi2 = C_PI_2_DEFAULT,
    pi1 = C_PI_1_SAMPLE_SIZE_DEFAULT,
    allocationRatioPlanned = 1,
    accountForObservationTimes = NA,
    eventTime = 12,
    accrualTime = C_ACCRUAL_TIME_DEFAULT,
    accrualIntensity = C_ACCRUAL_INTENSITY_DEFAULT,
    kappa = 1,
    piecewiseSurvivalTime = NA_real_,
    lambda2 = NA_real_,
    lambda1 = NA_real_,
    followUpTime = C_FOLLOW_UP_TIME_DEFAULT,
    maxNumberOfSubjects = 0,
    dropoutRate1 = 0,
    dropoutRate2 = 0,
    dropoutTime = 12
)

C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_COUNT_DATA <- list(
    thetaH0 = 1,
    overdispersion = 0,
    allocationRatioPlanned = 1
)

#'
#' @name TrialDesignPlan
#'
#' @title
#' Basic Trial Design Plan
#'
#' @description
#' Basic class for trial design plans.
#'
#' @details
#' \code{TrialDesignPlan} is the basic class for
#' \itemize{
#'   \item \code{\link{TrialDesignPlanMeans}},
#'   \item \code{\link{TrialDesignPlanRates}}, and
#'   \item \code{\link{TrialDesignPlanSurvival}}.
#' }
#'
#' @include f_core_constants.R
#' @include f_core_utilities.R
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_design.R
#' @include class_design_set.R
#' @include f_core_plot.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
TrialDesignPlan <- R6::R6Class("TrialDesignPlan",
    inherit = ParameterSet,
    public = list(
        .plotSettings = NULL,
        .design = NULL,
        .objectType = NULL, # "sampleSize" or "power"
        initialize = function(design, ...) {
            self$.design <- design

            super$initialize(...)

            self$.plotSettings <- PlotSettings$new()

            if (.isTrialDesignPlanMeans(self)) {
                defaultValueList <- C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS
            } else if (.isTrialDesignPlanRates(self)) {
                defaultValueList <- C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES
            } else if (.isTrialDesignPlanSurvival(self)) {
                defaultValueList <- C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL
            } else if (.isTrialDesignPlanCountData(self)) {
                defaultValueList <- C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_COUNT_DATA
            }
            for (parameterName in self$.getVisibleFieldNames()) {
                defaultValue <- defaultValueList[[parameterName]]
                existingValue <- self[[parameterName]]
                if (all(is.na(existingValue))) {
                    self$.setParameterType(parameterName, C_PARAM_DEFAULT_VALUE)
                } else if (!is.null(defaultValue) && length(defaultValue) == length(existingValue) &&
                        !any(is.na(defaultValue)) && !any(is.na(existingValue)) &&
                        sum(defaultValue == existingValue) == length(defaultValue)) {
                    self$.setParameterType(parameterName, C_PARAM_DEFAULT_VALUE)
                } else {
                    self$.setParameterType(parameterName, C_PARAM_USER_DEFINED)
                }
            }
            self$.setParameterType("optimumAllocationRatio", C_PARAM_NOT_APPLICABLE)
        },
        .setObjectType = function(objectType) {
            if (length(objectType) == 0 || !(objectType %in% c("sampleSize", "power"))) {
                stop(
                    C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'.objectType' (", objectType,
                    ") must be specified as 'sampleSize' or 'power'"
                )
            }
            self$.objectType <- objectType
        },
        .isSampleSizeObject = function() {
            if (length(self$.objectType) == 0 || !(self$.objectType %in% c("sampleSize", "power"))) {
                stop(
                    C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                    "'.objectType' must be specified as 'sampleSize' or 'power'"
                )
            }
            return(self$.objectType == "sampleSize")
        },
        .isPowerObject = function() {
            if (length(self$.objectType) == 0 || !(self$.objectType %in% c("sampleSize", "power"))) {
                stop(
                    C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                    "'.objectType' must be specified as 'sampleSize' or 'power'"
                )
            }
            return(self$.objectType == "power")
        },
        getPlotSettings = function() {
            return(self$.plotSettings)
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing trial plan objects"
            self$.resetCat()
            if (showType == 3) {
                .createSummary(self, digits = digits)$.show(
                    showType = 1,
                    digits = digits, consoleOutputEnabled = consoleOutputEnabled
                )
            } else if (showType == 2) {
                super$.show(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                self$.cat("Design plan parameters and output for ", self$.toString(), ":\n\n",
                    heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )

                self$.showParametersOfOneGroup(.getDesignParametersToShow(self),
                    "Design parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )

                self$.showParametersOfOneGroup(self$.getUserDefinedParameters(),
                    "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getDefaultParameters(),
                    "Default parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getGeneratedParameters(),
                    ifelse(identical(self$.objectType, "sampleSize"),
                        "Sample size and output", "Power and output"
                    ),
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )

                self$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)

                if (inherits(self, "TrialDesignPlanSurvival") ||
                        (!is.null(self$groups) && self$groups == 2) ||
                        self$.design$kMax > 1) {
                    self$.cat("Legend:\n",
                        heading = 2,
                        consoleOutputEnabled = consoleOutputEnabled
                    )
                    if (inherits(self, "TrialDesignPlanSurvival") ||
                            (!is.null(self$groups) && self$groups == 2)) {
                        self$.cat("  (i): values of treatment arm i\n",
                            consoleOutputEnabled = consoleOutputEnabled
                        )
                    }
                    if (self$.design$kMax > 1) {
                        self$.cat("  [k]: values at stage k\n",
                            consoleOutputEnabled = consoleOutputEnabled
                        )
                    }
                } else {
                    self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
                }
            }
        },
        getAlpha = function() {
            return(self$.design$alpha)
        },
        getBeta = function() {
            if (.isTrialDesignInverseNormalOrGroupSequential(self$.design)) {
                return(self$.design$beta)
            }
            return(NA_real_)
        },
        getSided = function() {
            return(self$.design$sided)
        },
        getTwoSidedPower = function() {
            if (.isTrialDesignInverseNormalOrGroupSequential(self$.design)) {
                return(self$.design$twoSidedPower)
            }
            return(NA)
        },
        .toString = function(startWithUpperCase = FALSE) {
            if (.isTrialDesignPlanMeans(self)) {
                result <- "means"
            } else if (.isTrialDesignPlanRates(self)) {
                result <- "rates"
            } else if (.isTrialDesignPlanSurvival(self)) {
                result <- "survival data"
            } else {
                result <- paste0("unknown data class '", .getClassName(self), "'")
            }
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(result), result))
        }
    )
)

#'
#' @title
#' Coerce Trial Design Plan to a Data Frame
#'
#' @description
#' Returns the \code{\link{TrialDesignPlan}} as data frame.
#'
#' @param x A \code{\link{TrialDesignPlan}} object.
#' @inheritParams param_niceColumnNamesEnabled
#' @inheritParams param_includeAllParameters
#' @inheritParams param_three_dots
#'
#' @details
#' Coerces the design plan to a data frame.
#'
#' @template return_dataframe
#'
#' @examples
#' as.data.frame(getSampleSizeMeans())
#'
#' @export
#'
#' @keywords internal
#'
as.data.frame.TrialDesignPlan <- function(x, row.names = NULL,
        optional = FALSE, niceColumnNamesEnabled = FALSE, includeAllParameters = FALSE, ...) {
    return(.getAsDataFrame(
        parameterSet = x,
        parameterNames = NULL,
        niceColumnNamesEnabled = niceColumnNamesEnabled,
        includeAllParameters = includeAllParameters
    ))
}

#'
#' @name TrialDesignPlanMeans
#'
#' @title
#' Trial Design Plan Means
#'
#' @description
#' Trial design plan for means.
#'
#' @template field_meanRatio
#' @template field_thetaH0
#' @template field_normalApproximation
#' @template field_alternative
#' @template field_stDev
#' @template field_groups
#' @template field_allocationRatioPlanned
#' @template field_optimumAllocationRatio
#' @template field_directionUpper
#' @template field_effect
#' @template field_overallReject
#' @template field_rejectPerStage
#' @template field_futilityStop
#' @template field_futilityPerStage
#' @template field_earlyStop
#' @template field_expectedNumberOfSubjects
#' @template field_nFixed
#' @template field_nFixed1
#' @template field_nFixed2
#' @template field_informationRates
#' @template field_maxNumberOfSubjects
#' @template field_maxNumberOfSubjects1
#' @template field_maxNumberOfSubjects2
#' @template field_numberOfSubjects
#' @template field_numberOfSubjects1
#' @template field_numberOfSubjects2
#' @template field_expectedNumberOfSubjectsH0
#' @template field_expectedNumberOfSubjectsH01
#' @template field_expectedNumberOfSubjectsH1
#' @template field_criticalValuesEffectScale
#' @template field_criticalValuesEffectScaleLower
#' @template field_criticalValuesEffectScaleUpper
#' @template field_criticalValuesPValueScale
#' @template field_futilityBoundsEffectScale
#' @template field_futilityBoundsEffectScaleLower
#' @template field_futilityBoundsEffectScaleUpper
#' @template field_futilityBoundsPValueScale
#'
#' @details
#' This object cannot be created directly; use \code{\link[=getSampleSizeMeans]{getSampleSizeMeans()}}
#' with suitable arguments to create a design plan for a dataset of means.
#'
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_design_set.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
TrialDesignPlanMeans <- R6::R6Class("TrialDesignPlanMeans",
    inherit = TrialDesignPlan,
    public = list(
        meanRatio = NULL,
        thetaH0 = NULL,
        normalApproximation = NULL,
        alternative = NULL,
        stDev = NULL,
        groups = NULL,
        allocationRatioPlanned = NULL,
        optimumAllocationRatio = NULL,
        directionUpper = NULL,
        effect = NULL,
        maxNumberOfSubjects = NULL,
        maxNumberOfSubjects1 = NULL,
        maxNumberOfSubjects2 = NULL,
        numberOfSubjects = NULL,
        numberOfSubjects1 = NULL,
        numberOfSubjects2 = NULL,
        overallReject = NULL,
        rejectPerStage = NULL,
        futilityStop = NULL,
        futilityPerStage = NULL,
        earlyStop = NULL,
        expectedNumberOfSubjects = NULL,
        nFixed = NULL,
        nFixed1 = NULL,
        nFixed2 = NULL,
        informationRates = NULL,
        expectedNumberOfSubjectsH0 = NULL,
        expectedNumberOfSubjectsH01 = NULL,
        expectedNumberOfSubjectsH1 = NULL,
        criticalValuesEffectScale = matrix(),
        criticalValuesEffectScaleLower = NULL,
        criticalValuesEffectScaleUpper = NULL,
        criticalValuesPValueScale = NULL,
        futilityBoundsEffectScale = NULL,
        futilityBoundsEffectScaleLower = NULL,
        futilityBoundsEffectScaleUpper = NULL,
        futilityBoundsPValueScale = NULL,
        initialize = function(...,
                normalApproximation = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["normalApproximation"]],
                meanRatio = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["meanRatio"]],
                thetaH0 = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["thetaH0"]],
                alternative = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["alternative"]],
                stDev = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["stDev"]],
                groups = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["groups"]],
                allocationRatioPlanned = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["allocationRatioPlanned"]]) {
            super$initialize(...)

            self$normalApproximation <- normalApproximation
            self$meanRatio <- meanRatio
            self$thetaH0 <- thetaH0
            self$alternative <- alternative
            self$stDev <- stDev
            self$groups <- groups
            self$allocationRatioPlanned <- allocationRatioPlanned

            self$optimumAllocationRatio <- FALSE
            visibleFieldNames <- self$.getVisibleFieldNames()
            startIndex <- which(visibleFieldNames == "directionUpper")
            for (i in startIndex:length(visibleFieldNames)) {
                self$.setParameterType(visibleFieldNames[i], C_PARAM_NOT_APPLICABLE)
            }

            if (self$groups == 1) {
                self$.setParameterType("meanRatio", C_PARAM_NOT_APPLICABLE)
                self$.setParameterType("allocationRatioPlanned", C_PARAM_NOT_APPLICABLE)
            }

            self$.setParameterType("maxNumberOfSubjects1", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("maxNumberOfSubjects2", C_PARAM_NOT_APPLICABLE)

            self$.setParameterType("criticalValuesEffectScale", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("criticalValuesEffectScaleLower", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("criticalValuesEffectScaleUpper", C_PARAM_NOT_APPLICABLE)

            self$.setParameterType("futilityBoundsEffectScale", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("futilityBoundsEffectScaleLower", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("futilityBoundsEffectScaleUpper", C_PARAM_NOT_APPLICABLE)
        },
        show = function(showType = 1, digits = NA_integer_) {
            "Method for automatically printing trial plan objects"
            super$show(showType = showType, digits = digits)
        },
        recreate = function(alternative = NA_real_) {
            alternativeTemp <- alternative
            if (any(is.na(alternative))) {
                alternativeTemp <- self$alternative
            }
            if (self$.objectType == "sampleSize") {
                result <- getSampleSizeMeans(
                    design = self$.design,
                    normalApproximation = self$.getParameterValueIfUserDefinedOrDefault("normalApproximation"),
                    meanRatio = self$meanRatio,
                    thetaH0 = self$.getParameterValueIfUserDefinedOrDefault("thetaH0"),
                    alternative = alternativeTemp,
                    stDev = self$.getParameterValueIfUserDefinedOrDefault("stDev"),
                    groups = self$.getParameterValueIfUserDefinedOrDefault("groups"),
                    allocationRatioPlanned = self$.getParameterValueIfUserDefinedOrDefault("allocationRatioPlanned")
                )
            } else {
                result <- getPowerMeans(
                    design = self$.design,
                    normalApproximation = self$.getParameterValueIfUserDefinedOrDefault("normalApproximation"),
                    meanRatio = self$meanRatio,
                    thetaH0 = self$.getParameterValueIfUserDefinedOrDefault("thetaH0"),
                    alternative = alternativeTemp,
                    stDev = self$.getParameterValueIfUserDefinedOrDefault("stDev"),
                    directionUpper = self$.getParameterValueIfUserDefinedOrDefault("directionUpper"),
                    maxNumberOfSubjects = self$.getParameterValueIfUserDefinedOrDefault("maxNumberOfSubjects"),
                    groups = self$.getParameterValueIfUserDefinedOrDefault("groups"),
                    allocationRatioPlanned = self$.getParameterValueIfUserDefinedOrDefault("allocationRatioPlanned")
                )
            }
            result$.plotSettings <- self$.plotSettings
            return(result)
        }
    )
)

#'
#' @name TrialDesignPlanRates
#'
#' @title
#' Trial Design Plan Rates
#'
#' @description
#' Trial design plan for rates.
#'
#' @template field_riskRatio
#' @template field_thetaH0
#' @template field_normalApproximation
#' @template field_pi1
#' @template field_pi2
#' @template field_groups
#' @template field_allocationRatioPlanned
#' @template field_optimumAllocationRatio
#' @template field_directionUpper
#' @template field_effect
#' @template field_overallReject
#' @template field_rejectPerStage
#' @template field_futilityStop
#' @template field_futilityPerStage
#' @template field_earlyStop
#' @template field_expectedNumberOfSubjects
#' @template field_nFixed
#' @template field_nFixed1
#' @template field_nFixed2
#' @template field_informationRates
#' @template field_maxNumberOfSubjects
#' @template field_maxNumberOfSubjects1
#' @template field_maxNumberOfSubjects2
#' @template field_numberOfSubjects
#' @template field_numberOfSubjects1
#' @template field_numberOfSubjects2
#' @template field_expectedNumberOfSubjectsH0
#' @template field_expectedNumberOfSubjectsH01
#' @template field_expectedNumberOfSubjectsH1
#' @template field_criticalValuesEffectScale
#' @template field_criticalValuesEffectScaleLower
#' @template field_criticalValuesEffectScaleUpper
#' @template field_criticalValuesPValueScale
#' @template field_futilityBoundsEffectScale
#' @template field_futilityBoundsEffectScaleLower
#' @template field_futilityBoundsEffectScaleUpper
#' @template field_futilityBoundsPValueScale
#'
#' @details
#' This object cannot be created directly; use \code{\link[=getSampleSizeRates]{getSampleSizeRates()}}
#' with suitable arguments to create a design plan for a dataset of rates.
#'
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_design_set.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
TrialDesignPlanRates <- R6::R6Class("TrialDesignPlanRates",
    inherit = TrialDesignPlan,
    public = list(
        riskRatio = NULL,
        thetaH0 = NULL,
        normalApproximation = NULL,
        pi1 = NULL,
        pi2 = NULL,
        groups = NULL,
        allocationRatioPlanned = NULL,
        optimumAllocationRatio = NULL,
        directionUpper = NULL,
        effect = NULL,
        maxNumberOfSubjects = NULL,
        maxNumberOfSubjects1 = NULL,
        maxNumberOfSubjects2 = NULL,
        numberOfSubjects = NULL,
        numberOfSubjects1 = NULL,
        numberOfSubjects2 = NULL,
        overallReject = NULL,
        rejectPerStage = NULL,
        futilityStop = NULL,
        futilityPerStage = NULL,
        earlyStop = NULL,
        expectedNumberOfSubjects = NULL,
        nFixed = NULL,
        nFixed1 = NULL,
        nFixed2 = NULL,
        informationRates = NULL,
        expectedNumberOfSubjectsH0 = NULL,
        expectedNumberOfSubjectsH01 = NULL,
        expectedNumberOfSubjectsH1 = NULL,
        criticalValuesEffectScale = matrix(),
        criticalValuesEffectScaleLower = NULL,
        criticalValuesEffectScaleUpper = NULL,
        criticalValuesPValueScale = NULL,
        futilityBoundsEffectScale = NULL,
        futilityBoundsEffectScaleLower = NULL,
        futilityBoundsEffectScaleUpper = NULL,
        futilityBoundsPValueScale = NULL,
        initialize = function(...,
                normalApproximation = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["normalApproximation"]],
                riskRatio = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["riskRatio"]],
                thetaH0 = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["thetaH0"]],
                pi1 = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["pi1"]],
                pi2 = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["pi2"]],
                groups = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["groups"]],
                allocationRatioPlanned = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["allocationRatioPlanned"]]) {
            super$initialize(...)
            self$normalApproximation <- normalApproximation
            self$riskRatio <- riskRatio
            self$thetaH0 <- thetaH0
            self$pi1 <- pi1
            self$pi2 <- pi2
            self$groups <- groups
            self$allocationRatioPlanned <- allocationRatioPlanned

            self$optimumAllocationRatio <- FALSE
            visibleFieldNames <- self$.getVisibleFieldNames()
            startIndex <- which(visibleFieldNames == "directionUpper")
            for (i in startIndex:length(visibleFieldNames)) {
                self$.setParameterType(visibleFieldNames[i], C_PARAM_NOT_APPLICABLE)
            }

            if (self$groups == 1) {
                self$.setParameterType("meanRatio", C_PARAM_NOT_APPLICABLE)
                self$.setParameterType("allocationRatioPlanned", C_PARAM_NOT_APPLICABLE)
            }

            self$.setParameterType("maxNumberOfSubjects1", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("maxNumberOfSubjects2", C_PARAM_NOT_APPLICABLE)

            self$.setParameterType("criticalValuesEffectScale", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("criticalValuesEffectScaleLower", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("criticalValuesEffectScaleUpper", C_PARAM_NOT_APPLICABLE)

            self$.setParameterType("futilityBoundsEffectScale", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("futilityBoundsEffectScaleLower", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("futilityBoundsEffectScaleUpper", C_PARAM_NOT_APPLICABLE)
        },
        show = function(showType = 1, digits = NA_integer_) {
            "Method for automatically printing trial plan objects"
            super$show(showType = showType, digits = digits)
        },
        recreate = function(pi1 = NA_real_) {
            pi1Temp <- pi1
            if (any(is.na(pi1))) {
                pi1Temp <- self$pi1
            }
            if (self$.objectType == "sampleSize") {
                return(getSampleSizeRates(
                    design = self$.design,
                    normalApproximation = self$.getParameterValueIfUserDefinedOrDefault("normalApproximation"),
                    riskRatio = self$riskRatio,
                    thetaH0 = self$.getParameterValueIfUserDefinedOrDefault("thetaH0"),
                    pi1 = pi1Temp,
                    pi2 = self$.getParameterValueIfUserDefinedOrDefault("pi2"),
                    groups = self$.getParameterValueIfUserDefinedOrDefault("groups"),
                    allocationRatioPlanned = self$.getParameterValueIfUserDefinedOrDefault("allocationRatioPlanned")
                ))
            } else {
                return(getPowerRates(
                    design = self$.design,
                    riskRatio = self$riskRatio,
                    thetaH0 = self$.getParameterValueIfUserDefinedOrDefault("thetaH0"),
                    pi1 = pi1Temp,
                    pi2 = self$.getParameterValueIfUserDefinedOrDefault("pi2"),
                    directionUpper = self$.getParameterValueIfUserDefinedOrDefault("directionUpper"),
                    maxNumberOfSubjects = self$.getParameterValueIfUserDefinedOrDefault("maxNumberOfSubjects"),
                    groups = self$.getParameterValueIfUserDefinedOrDefault("groups"),
                    allocationRatioPlanned = self$.getParameterValueIfUserDefinedOrDefault("allocationRatioPlanned")
                ))
            }
        }
    )
)

#'
#' @name TrialDesignPlanSurvival
#'
#' @title
#' Trial Design Plan Survival
#'
#' @description
#' Trial design plan for survival data.
#'
#' @template field_thetaH0
#' @template field_typeOfComputation
#' @template field_directionUpper
#' @template field_pi1_survival
#' @template field_pi2_survival
#' @template field_median1
#' @template field_median2
#' @template field_lambda1
#' @template field_lambda2
#' @template field_hazardRatio
#' @template field_maxNumberOfSubjects
#' @template field_maxNumberOfSubjects1
#' @template field_maxNumberOfSubjects2
#' @template field_maxNumberOfEvents
#' @template field_allocationRatioPlanned
#' @template field_optimumAllocationRatio
#' @template field_accountForObservationTimes
#' @template field_eventTime
#' @template field_accrualTime
#' @template field_totalAccrualTime
#' @template field_accrualIntensity
#' @template field_accrualIntensityRelative
#' @template field_kappa
#' @template field_piecewiseSurvivalTime
#' @template field_followUpTime
#' @template field_dropoutRate1
#' @template field_dropoutRate2
#' @template field_dropoutTime
#' @template field_chi
#' @template field_expectedNumberOfEvents
#' @template field_eventsFixed
#' @template field_nFixed
#' @template field_nFixed1
#' @template field_nFixed2
#' @template field_overallReject
#' @template field_rejectPerStage
#' @template field_futilityStop
#' @template field_futilityPerStage
#' @template field_earlyStop
#' @template field_informationRates
#' @template field_analysisTime
#' @template field_studyDurationH1
#' @template field_studyDuration
#' @template field_maxStudyDuration
#' @template field_eventsPerStage
#' @template field_singleEventsPerStage
#' @template field_cumulativeEventsPerStage
#' @template field_expectedEventsH0
#' @template field_expectedEventsH01
#' @template field_expectedEventsH1
#' @template field_numberOfSubjects
#' @template field_numberOfSubjects1
#' @template field_numberOfSubjects2
#' @template field_expectedNumberOfSubjectsH1
#' @template field_expectedNumberOfSubjects
#' @template field_criticalValuesEffectScale
#' @template field_criticalValuesEffectScaleLower
#' @template field_criticalValuesEffectScaleUpper
#' @template field_criticalValuesPValueScale
#' @template field_futilityBoundsEffectScale
#' @template field_futilityBoundsEffectScaleLower
#' @template field_futilityBoundsEffectScaleUpper
#' @template field_futilityBoundsPValueScale
#'
#' @details
#' This object cannot be created directly; use \code{\link[=getSampleSizeSurvival]{getSampleSizeSurvival()}}
#' with suitable arguments to create a design plan for a dataset of survival data.
#'
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_design_set.R
#' @include class_time.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
TrialDesignPlanSurvival <- R6::R6Class("TrialDesignPlanSurvival",
    inherit = TrialDesignPlan,
    public = list(
        .piecewiseSurvivalTime = NULL,
        .accrualTime = NULL,
        .calculateFollowUpTime = NULL,
        thetaH0 = NULL,
        typeOfComputation = NULL,
        directionUpper = NULL,
        pi1 = NULL,
        pi2 = NULL,
        median1 = NULL,
        median2 = NULL,
        lambda1 = NULL,
        lambda2 = NULL,
        hazardRatio = NULL,
        maxNumberOfSubjects = NULL,
        maxNumberOfSubjects1 = NULL,
        maxNumberOfSubjects2 = NULL,
        numberOfSubjects = NULL,
        numberOfSubjects1 = NULL,
        numberOfSubjects2 = NULL,
        maxNumberOfEvents = NULL,
        allocationRatioPlanned = NULL,
        optimumAllocationRatio = NULL,
        accountForObservationTimes = NULL,
        eventTime = NULL,
        accrualTime = NULL,
        totalAccrualTime = NULL,
        accrualIntensity = NULL,
        accrualIntensityRelative = NULL,
        kappa = NULL,
        piecewiseSurvivalTime = NULL,
        followUpTime = NULL,
        dropoutRate1 = NULL,
        dropoutRate2 = NULL,
        dropoutTime = NULL,
        chi = NULL,
        expectedNumberOfEvents = NULL,
        eventsFixed = NULL,
        nFixed = NULL,
        nFixed1 = NULL,
        nFixed2 = NULL,
        overallReject = NULL,
        rejectPerStage = NULL,
        futilityStop = NULL,
        futilityPerStage = NULL,
        earlyStop = NULL,
        informationRates = NULL,
        analysisTime = NULL,
        studyDurationH1 = NULL,
        studyDuration = NULL,
        maxStudyDuration = NULL,
        eventsPerStage = NULL, # deprecated
        singleEventsPerStage = NULL,
        cumulativeEventsPerStage = NULL,
        expectedEventsH0 = NULL,
        expectedEventsH01 = NULL,
        expectedEventsH1 = NULL,
        expectedNumberOfSubjectsH1 = NULL,
        expectedNumberOfSubjects = NULL,
        criticalValuesEffectScale = matrix(),
        criticalValuesEffectScaleLower = NULL,
        criticalValuesEffectScaleUpper = NULL,
        criticalValuesPValueScale = NULL,
        futilityBoundsEffectScale = NULL,
        futilityBoundsEffectScaleLower = NULL,
        futilityBoundsEffectScaleUpper = NULL,
        futilityBoundsPValueScale = NULL,
        initialize = function(..., typeOfComputation = NULL,
                thetaH0 = NULL,
                allocationRatioPlanned = NULL,
                accountForObservationTimes = NULL,
                eventTime = NULL,
                accrualTime = NULL,
                accrualIntensity = NULL,
                kappa = NULL,
                followUpTime = NULL,
                maxNumberOfSubjects = NULL,
                dropoutRate1 = NULL,
                dropoutRate2 = NULL,
                dropoutTime = NULL,
                hazardRatio = NULL) {
            self$typeOfComputation <- typeOfComputation
            self$thetaH0 <- thetaH0
            self$allocationRatioPlanned <- allocationRatioPlanned
            self$accountForObservationTimes <- accountForObservationTimes
            self$eventTime <- eventTime
            self$accrualTime <- accrualTime
            self$accrualIntensity <- accrualIntensity
            self$kappa <- kappa
            self$followUpTime <- followUpTime
            self$maxNumberOfSubjects <- maxNumberOfSubjects
            self$dropoutRate1 <- dropoutRate1
            self$dropoutRate2 <- dropoutRate2
            self$dropoutTime <- dropoutTime
            self$hazardRatio <- hazardRatio

            super$initialize(...)

            self$optimumAllocationRatio <- FALSE
            visibleFieldNames <- self$.getVisibleFieldNames()
            startIndex <- which(visibleFieldNames == "hazardRatio")
            for (i in startIndex:length(visibleFieldNames)) {
                self$.setParameterType(visibleFieldNames[i], C_PARAM_NOT_APPLICABLE)
            }

            self$.setParameterType("maxNumberOfSubjects", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("maxNumberOfSubjects1", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("maxNumberOfSubjects2", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("median1", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("median2", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("accountForObservationTimes", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("chi", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("maxStudyDuration", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("accrualIntensityRelative", C_PARAM_NOT_APPLICABLE)

            self$.setParameterType("criticalValuesEffectScale", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("criticalValuesEffectScaleLower", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("criticalValuesEffectScaleUpper", C_PARAM_NOT_APPLICABLE)

            self$.setParameterType("futilityBoundsEffectScale", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("futilityBoundsEffectScaleLower", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("futilityBoundsEffectScaleUpper", C_PARAM_NOT_APPLICABLE)

            self$.setParameterType("singleEventsPerStage", C_PARAM_NOT_APPLICABLE)

            # set default values
            for (parameterName in c(
                "eventTime", "accrualTime", "accrualIntensity",
                "kappa", "piecewiseSurvivalTime", "lambda1", "lambda2",
                "followUpTime", "dropoutTime"
            )) {
                self$.setDefaultValue(parameterName)
            }
        },
        .setDefaultValue = function(argumentName) {
            if (is.null(self[[argumentName]]) || all(is.na(self[[argumentName]]))) {
                self[[argumentName]] <- C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL[[argumentName]]
                self$.setParameterType(argumentName, C_PARAM_DEFAULT_VALUE)
            }
        },
        show = function(showType = 1, digits = NA_integer_) {
            "Method for automatically printing trial plan objects"
            super$show(showType = showType, digits = digits)
        },
        .warnInCaseArgumentExists = function(argument, argumentName) {
            if (!all(is.na(argument)) && any(argument > 0)) {
                warning(sprintf(
                    "Specified '%s' (%s) not taken into account",
                    argumentName, .arrayToString(argument)
                ), call. = FALSE)
            }
        },
        recreate = function(..., hazardRatio = NA_real_, pi1 = NA_real_, maxNumberOfSubjects = NA_integer_) {
            hr <- NA_real_
            if (self$.getParameterType("hazardRatio") == C_PARAM_USER_DEFINED) {
                hr <- hazardRatio
                if (any(is.na(hazardRatio))) {
                    hr <- self$hazardRatio
                }
            }

            lambda2 <- self$.getParameterValueIfUserDefinedOrDefault("lambda2")

            pi1Temp <- NA_real_
            if (self$.getParameterType("pi1") == C_PARAM_USER_DEFINED) {
                pi1Temp <- pi1
                if (any(is.na(pi1))) {
                    pi1Temp <- self$pi1
                }
            } else if (all(is.na(lambda2))) {
                if (self$.objectType == "sampleSize") {
                    pi1Temp <- C_PI_1_SAMPLE_SIZE_DEFAULT
                } else {
                    pi1Temp <- C_PI_1_DEFAULT
                }
            }
            accrualTimeTemp <- self$.getParameterValueIfUserDefinedOrDefault("accrualTime")
            if (!is.null(accrualTimeTemp) && length(accrualTimeTemp) > 0 &&
                    !all(is.na(accrualTimeTemp)) && accrualTimeTemp[1] != 0L) {
                accrualTimeTemp <- c(0L, as.integer(accrualTimeTemp))
            }
            accrualIntensityTemp <- self$.getParameterValueIfUserDefinedOrDefault("accrualIntensity")
            if (all(is.na(accrualIntensityTemp))) {
                accrualIntensityTemp <- C_ACCRUAL_INTENSITY_DEFAULT
            }
            
            if (is.null(maxNumberOfSubjects) || length(maxNumberOfSubjects) != 1 || is.na(maxNumberOfSubjects)) {
                maxNumberOfSubjects <- self$.getParameterValueIfUserDefinedOrDefault("maxNumberOfSubjects")
            }
            
            args <- list(
                design = self$.design,
                typeOfComputation = self$.getParameterValueIfUserDefinedOrDefault("typeOfComputation"),
                thetaH0 = self$.getParameterValueIfUserDefinedOrDefault("thetaH0"),
                pi1 = pi1Temp,
                pi2 = self$.getParameterValueIfUserDefinedOrDefault("pi2"),
                allocationRatioPlanned = self$allocationRatioPlanned,
                eventTime = ifelse(all(is.na(self$eventTime)), C_EVENT_TIME_DEFAULT, self$eventTime),
                accrualTime = accrualTimeTemp,
                accrualIntensity = accrualIntensityTemp,
                kappa = self$kappa,
                piecewiseSurvivalTime = self$.getParameterValueIfUserDefinedOrDefault("piecewiseSurvivalTime"),
                lambda2 = lambda2,
                lambda1 = self$.getParameterValueIfUserDefinedOrDefault("lambda1"),
                hazardRatio = hr,
                maxNumberOfSubjects = maxNumberOfSubjects,
                dropoutRate1 = self$dropoutRate1,
                dropoutRate2 = self$dropoutRate2,
                dropoutTime = self$dropoutTime
            )
            
            if (self$.objectType == "sampleSize") {
                args$ accountForObservationTimes = self$.getParameterValueIfUserDefinedOrDefault("accountForObservationTimes")
                args$followUpTime = self$.getParameterValueIfUserDefinedOrDefault("followUpTime")
                return(do.call(getSampleSizeSurvival, args))
            } else {
                directionUpperTemp <- self$directionUpper
                if (length(directionUpperTemp) > 1) {
                    directionUpperTemp <- directionUpperTemp[1]
                }
                args$directionUpper = directionUpperTemp
                args$maxNumberOfEvents = self$.getParameterValueIfUserDefinedOrDefault("maxNumberOfEvents")
                return(do.call(getPowerSurvival, args))
            }
        }
    )
)

#'
#' @name TrialDesignPlanCountData
#'
#' @title
#' Trial Design Plan Count Data
#'
#' @description
#' Trial design plan for count data.
#'
#' @template field_thetaH0
#' @template field_groups
#' @template field_allocationRatioPlanned
#' @template field_optimumAllocationRatio
#' @template field_directionUpper
#' @template field_lambda1
#' @template field_lambda2
#' @template field_lambda
#' @template field_theta
#' @template field_nFixed
#' @template field_nFixed1
#' @template field_nFixed2
#' @template field_maxNumberOfSubjects
#' @template field_maxNumberOfSubjects1
#' @template field_maxNumberOfSubjects2
#' @template field_overallReject
#' @template field_rejectPerStage
#' @template field_futilityStop
#' @template field_futilityPerStage
#' @template field_earlyStop
#' @template field_overdispersion
#' @template field_fixedExposureTime
#' @template field_accrualTime
#' @template field_accrualIntensity
#' @template field_followUpTime
#' @template field_calendarTime
#' @template field_expectedStudyDurationH1
#' @template field_studyTime
#' @template field_numberOfSubjects
#' @template field_expectedNumberOfSubjectsH1
#' @template field_informationOverStages
#' @template field_expectedInformationH0
#' @template field_expectedInformationH01
#' @template field_expectedInformationH1
#' @template field_maxInformation
#' @template field_futilityBoundsPValueScale
#'
#' @details
#' This object cannot be created directly; use \code{\link[=getSampleSizeCounts]{getSampleSizeCounts()}}
#' with suitable arguments to create a design plan for a dataset of rates.
#'
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_design_set.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
TrialDesignPlanCountData <- R6::R6Class("TrialDesignPlanCountData",
    inherit = TrialDesignPlan,
    public = list(
        .designCharacteristics = NULL,
        thetaH0 = NULL,
        groups = NULL,
        allocationRatioPlanned = NULL,
        optimumAllocationRatio = NULL,
        directionUpper = NULL,
        lambda1 = NULL,
        lambda2 = NULL,
        lambda = NULL,
        theta = NULL,
        nFixed = NULL,
        nFixed1 = NULL,
        nFixed2 = NULL,
        maxNumberOfSubjects = NULL,
        maxNumberOfSubjects1 = NULL,
        maxNumberOfSubjects2 = NULL,
        overallReject = NULL,
        rejectPerStage = NULL,
        futilityStop = NULL,
        futilityPerStage = NULL,
        earlyStop = NULL,
        overdispersion = NULL,
        fixedExposureTime = NULL,
        accrualTime = NULL,
        accrualIntensity = NULL,
        followUpTime = NULL,
        calendarTime = NULL,
        expectedStudyDurationH1 = NULL,
        studyTime = NULL,
        numberOfSubjects = NULL,
        expectedNumberOfSubjectsH1 = NULL,
        informationOverStages = NULL,
        expectedInformationH0 = NULL,
        expectedInformationH01 = NULL,
        expectedInformationH1 = NULL,
        maxInformation = NULL,
        futilityBoundsPValueScale = NULL,
        initialize = function(...,
                designCharacteristics,
                lambda1 = NA_real_,
                lambda2 = NA_real_,
                lambda = NA_real_,
                theta = NA_real_,
                thetaH0 = 1,
                overdispersion = 0,
                fixedExposureTime = NA_real_,
                accrualTime = NA_real_,
                accrualIntensity = NA_real_,
                followUpTime = NA_real_,
                allocationRatioPlanned = NA_real_) {
            super$initialize(...)
            self$.designCharacteristics <- designCharacteristics
            self$lambda1 <- lambda1
            self$lambda2 <- lambda2
            self$lambda <- lambda
            self$theta <- theta
            self$thetaH0 <- thetaH0
            self$overdispersion <- overdispersion
            self$fixedExposureTime <- fixedExposureTime
            self$accrualTime <- accrualTime
            self$accrualIntensity <- accrualIntensity
            self$followUpTime <- followUpTime
            self$allocationRatioPlanned <- allocationRatioPlanned

            self$groups <- 2L
            self$optimumAllocationRatio <- FALSE
            self$.setParameterType("groups", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("directionUpper", C_PARAM_NOT_APPLICABLE)
            self$.setParameterType("optimumAllocationRatio", C_PARAM_NOT_APPLICABLE)
        },
        .toString = function(startWithUpperCase = FALSE) {
            s <- "count data"
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
        },
        recreate = function(..., lambda1 = NA_real_, theta = NA_real_) {
            if (all(is.na(lambda1))) {
                lambda1Temp <- self$.getParameterValueIfUserDefinedOrDefault("lambda1")
            } else {
                lambda1Temp <- lambda1
                if (any(is.na(lambda1))) {
                    lambda1Temp <- self$lambda1
                }
            }
            if (all(is.na(theta))) {
                thetaTemp <- self$.getParameterValueIfUserDefinedOrDefault("theta")
            } else {
                thetaTemp <- theta
                if (any(is.na(theta))) {
                    thetaTemp <- self$theta
                }
            }
            if (self$.objectType == "sampleSize") {
                result <- getSampleSizeCounts(
                    design = self$.design,
                    lambda1 = lambda1Temp,
                    lambda2 = self$.getParameterValueIfUserDefinedOrDefault("lambda2"),
                    lambda = self$.getParameterValueIfUserDefinedOrDefault("lambda"),
                    theta = thetaTemp,
                    thetaH0 = self$.getParameterValueIfUserDefinedOrDefault("thetaH0"),
                    overdispersion = self$.getParameterValueIfUserDefinedOrDefault("overdispersion"),
                    fixedExposureTime = self$.getParameterValueIfUserDefinedOrDefault("fixedExposureTime"),
                    accrualTime = self$.getParameterValueIfUserDefinedOrDefault("accrualTime"),
                    accrualIntensity = self$.getParameterValueIfUserDefinedOrDefault("accrualIntensity"),
                    followUpTime = self$.getParameterValueIfUserDefinedOrDefault("followUpTime"),
                    maxNumberOfSubjects = self$.getParameterValueIfUserDefinedOrDefault("maxNumberOfSubjects"),
                    allocationRatioPlanned = self$.getParameterValueIfUserDefinedOrDefault("allocationRatioPlanned")
                )
            } else {
                result <- getPowerCounts(
                    design = self$.design,
                    directionUpper = self$.getParameterValueIfUserDefinedOrDefault("directionUpper"),
                    maxNumberOfSubjects = self$.getParameterValueIfUserDefinedOrDefault("maxNumberOfSubjects"),
                    lambda1 = lambda1Temp,
                    lambda2 = self$.getParameterValueIfUserDefinedOrDefault("lambda2"),
                    lambda = self$.getParameterValueIfUserDefinedOrDefault("lambda"),
                    theta = thetaTemp,
                    thetaH0 = self$.getParameterValueIfUserDefinedOrDefault("thetaH0"),
                    overdispersion = self$.getParameterValueIfUserDefinedOrDefault("overdispersion"),
                    fixedExposureTime = self$.getParameterValueIfUserDefinedOrDefault("fixedExposureTime"),
                    accrualTime = self$.getParameterValueIfUserDefinedOrDefault("accrualTime"),
                    accrualIntensity = self$.getParameterValueIfUserDefinedOrDefault("accrualIntensity"),
                    followUpTime = self$.getParameterValueIfUserDefinedOrDefault("followUpTime"),
                    allocationRatioPlanned = self$.getParameterValueIfUserDefinedOrDefault("allocationRatioPlanned")
                )
            }
            result$.plotSettings <- self$.plotSettings
            return(result)
        }
    )
)
