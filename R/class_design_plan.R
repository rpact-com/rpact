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
## |  File version: $Revision: 7620 $
## |  Last changed: $Date: 2024-02-09 12:57:37 +0100 (Fr, 09 Feb 2024) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_constants.R
#' @include f_design_general_utilities.R
NULL

C_VARIABLE_DESIGN_PLAN_PARAMETERS <- c("lambda1", "pi1", "median1", "alternative", "hazardRatio", "theta")

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
TrialDesignPlan <- setRefClass("TrialDesignPlan",
    contains = "ParameterSet",
    fields = list(
        .plotSettings = "PlotSettings",
        .design = "TrialDesign",
        .objectType = "character" # "sampleSize" or "power"
    ),
    methods = list(
        initialize = function(design, ...) {
            callSuper(.design = design, ...)

            .plotSettings <<- PlotSettings()

            if (.isTrialDesignPlanMeans(.self)) {
                defaultValueList <- C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS
            } else if (.isTrialDesignPlanRates(.self)) {
                defaultValueList <- C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES
            } else if (.isTrialDesignPlanSurvival(.self)) {
                defaultValueList <- C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL
            } else if (.isTrialDesignPlanCountData(.self)) {
                defaultValueList <- C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_COUNT_DATA
            }
            for (parameterName in .getVisibleFieldNames()) {
                defaultValue <- defaultValueList[[parameterName]]
                existingValue <- .self[[parameterName]]
                if (all(is.na(existingValue))) {
                    .setParameterType(parameterName, C_PARAM_DEFAULT_VALUE)
                } else if (!is.null(defaultValue) && length(defaultValue) == length(existingValue) &&
                        !any(is.na(defaultValue)) && !any(is.na(existingValue)) &&
                        sum(defaultValue == existingValue) == length(defaultValue)) {
                    .setParameterType(parameterName, C_PARAM_DEFAULT_VALUE)
                } else {
                    .setParameterType(parameterName, C_PARAM_USER_DEFINED)
                }
            }
            .setParameterType("optimumAllocationRatio", C_PARAM_NOT_APPLICABLE)
        },
        .setObjectType = function(objectType) {
            if (length(objectType) == 0 || !(objectType %in% c("sampleSize", "power"))) {
                stop(
                    C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'.objectType' (", objectType,
                    ") must be specified as 'sampleSize' or 'power'"
                )
            }
            .objectType <<- objectType
        },
        .isSampleSizeObject = function() {
            if (length(.objectType) == 0 || !(.objectType %in% c("sampleSize", "power"))) {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'.objectType' must be specified as 'sampleSize' or 'power'")
            }
            return(.objectType == "sampleSize")
        },
        .isPowerObject = function() {
            if (length(.objectType) == 0 || !(.objectType %in% c("sampleSize", "power"))) {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'.objectType' must be specified as 'sampleSize' or 'power'")
            }
            return(.objectType == "power")
        },
        getPlotSettings = function() {
            return(.plotSettings)
        },
        show = function(showType = 1, digits = NA_integer_) {
            .show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing trial plan objects"
            .resetCat()
            if (showType == 3) {
                .createSummary(.self, digits = digits)$.show(
                    showType = 1,
                    digits = digits, consoleOutputEnabled = consoleOutputEnabled
                )
            } else if (showType == 2) {
                callSuper(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                .cat("Design plan parameters and output for ", .toString(), ":\n\n",
                    heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )

                .showParametersOfOneGroup(.getDesignParametersToShow(.self), "Design parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )

                .showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showParametersOfOneGroup(.getGeneratedParameters(),
                    ifelse(identical(.objectType, "sampleSize"), "Sample size and output", "Power and output"),
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )

                .showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)

                if (inherits(.self, "TrialDesignPlanSurvival") || groups == 2 || .design$kMax > 1) {
                    .cat("Legend:\n",
                        heading = 2,
                        consoleOutputEnabled = consoleOutputEnabled
                    )
                    if (inherits(.self, "TrialDesignPlanSurvival") || groups == 2) {
                        .cat("  (i): values of treatment arm i\n",
                            consoleOutputEnabled = consoleOutputEnabled
                        )
                    }
                    if (.design$kMax > 1) {
                        .cat("  [k]: values at stage k\n", consoleOutputEnabled = consoleOutputEnabled)
                    }
                }

                .cat("\n", consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        getAlpha = function() {
            return(.design$alpha)
        },
        getBeta = function() {
            if (.isTrialDesignInverseNormalOrGroupSequential(.design)) {
                return(.design$beta)
            }
            return(NA_real_)
        },
        getSided = function() {
            return(.design$sided)
        },
        getTwoSidedPower = function() {
            if (.isTrialDesignInverseNormalOrGroupSequential(.design)) {
                return(.design$twoSidedPower)
            }
            return(NA)
        },
        .toString = function(startWithUpperCase = FALSE) {
            if (.isTrialDesignPlanMeans(.self)) {
                s <- "means"
            } else if (.isTrialDesignPlanRates(.self)) {
                s <- "rates"
            } else if (.isTrialDesignPlanSurvival(.self)) {
                s <- "survival data"
            } else {
                s <- paste0("unknown data class '", .getClassName(.self), "'")
            }
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
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
TrialDesignPlanMeans <- setRefClass("TrialDesignPlanMeans",
    contains = "TrialDesignPlan",
    fields = list(
        meanRatio = "logical",
        thetaH0 = "numeric",
        normalApproximation = "logical",
        alternative = "numeric",
        stDev = "numeric",
        groups = "numeric",
        allocationRatioPlanned = "numeric",
        optimumAllocationRatio = "logical",
        directionUpper = "logical",
        effect = "numeric",
        maxNumberOfSubjects = "numeric",
        maxNumberOfSubjects1 = "numeric",
        maxNumberOfSubjects2 = "numeric",
        numberOfSubjects = "matrix",
        numberOfSubjects1 = "matrix",
        numberOfSubjects2 = "matrix",
        overallReject = "numeric",
        rejectPerStage = "matrix",
        futilityStop = "numeric",
        futilityPerStage = "matrix",
        earlyStop = "numeric",
        expectedNumberOfSubjects = "numeric",
        nFixed = "numeric",
        nFixed1 = "numeric",
        nFixed2 = "numeric",
        informationRates = "matrix",
        expectedNumberOfSubjectsH0 = "numeric",
        expectedNumberOfSubjectsH01 = "numeric",
        expectedNumberOfSubjectsH1 = "numeric",
        criticalValuesEffectScale = "matrix",
        criticalValuesEffectScaleLower = "matrix",
        criticalValuesEffectScaleUpper = "matrix",
        criticalValuesPValueScale = "matrix",
        futilityBoundsEffectScale = "matrix",
        futilityBoundsEffectScaleLower = "matrix",
        futilityBoundsEffectScaleUpper = "matrix",
        futilityBoundsPValueScale = "matrix"
    ),
    methods = list(
        initialize = function(...,
                normalApproximation = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["normalApproximation"]],
                meanRatio = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["meanRatio"]],
                thetaH0 = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["thetaH0"]],
                alternative = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["alternative"]],
                stDev = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["stDev"]],
                groups = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["groups"]],
                allocationRatioPlanned = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["allocationRatioPlanned"]]) {
            callSuper(...,
                normalApproximation = normalApproximation,
                meanRatio = meanRatio,
                thetaH0 = thetaH0,
                alternative = alternative,
                stDev = stDev,
                groups = groups,
                allocationRatioPlanned = allocationRatioPlanned
            )

            optimumAllocationRatio <<- FALSE
            visibleFieldNames <- .getVisibleFieldNames()
            startIndex <- which(visibleFieldNames == "directionUpper")
            for (i in startIndex:length(visibleFieldNames)) {
                .setParameterType(visibleFieldNames[i], C_PARAM_NOT_APPLICABLE)
            }

            if (groups == 1) {
                .setParameterType("meanRatio", C_PARAM_NOT_APPLICABLE)
                .setParameterType("allocationRatioPlanned", C_PARAM_NOT_APPLICABLE)
            }

            .setParameterType("maxNumberOfSubjects1", C_PARAM_NOT_APPLICABLE)
            .setParameterType("maxNumberOfSubjects2", C_PARAM_NOT_APPLICABLE)

            .setParameterType("criticalValuesEffectScale", C_PARAM_NOT_APPLICABLE)
            .setParameterType("criticalValuesEffectScaleLower", C_PARAM_NOT_APPLICABLE)
            .setParameterType("criticalValuesEffectScaleUpper", C_PARAM_NOT_APPLICABLE)

            .setParameterType("futilityBoundsEffectScale", C_PARAM_NOT_APPLICABLE)
            .setParameterType("futilityBoundsEffectScaleLower", C_PARAM_NOT_APPLICABLE)
            .setParameterType("futilityBoundsEffectScaleUpper", C_PARAM_NOT_APPLICABLE)
        },
        clone = function(alternative = NA_real_) {
            alternativeTemp <- alternative
            if (any(is.na(alternative))) {
                alternativeTemp <- .self$alternative
            }
            if (.objectType == "sampleSize") {
                result <- getSampleSizeMeans(
                    design = .self$.design,
                    normalApproximation = .self$.getParameterValueIfUserDefinedOrDefault("normalApproximation"),
                    meanRatio = .self$meanRatio, # .getParameterValueIfUserDefinedOrDefault("meanRatio"),
                    thetaH0 = .self$.getParameterValueIfUserDefinedOrDefault("thetaH0"),
                    alternative = alternativeTemp,
                    stDev = .self$.getParameterValueIfUserDefinedOrDefault("stDev"),
                    groups = .self$.getParameterValueIfUserDefinedOrDefault("groups"),
                    allocationRatioPlanned = .self$.getParameterValueIfUserDefinedOrDefault("allocationRatioPlanned")
                )
            } else {
                result <- getPowerMeans(
                    design = .self$.design,
                    normalApproximation = .self$.getParameterValueIfUserDefinedOrDefault("normalApproximation"),
                    meanRatio = .self$meanRatio, # .getParameterValueIfUserDefinedOrDefault("meanRatio"),
                    thetaH0 = .self$.getParameterValueIfUserDefinedOrDefault("thetaH0"),
                    alternative = alternativeTemp,
                    stDev = .self$.getParameterValueIfUserDefinedOrDefault("stDev"),
                    directionUpper = .self$.getParameterValueIfUserDefinedOrDefault("directionUpper"),
                    maxNumberOfSubjects = .self$.getParameterValueIfUserDefinedOrDefault("maxNumberOfSubjects"),
                    groups = .self$.getParameterValueIfUserDefinedOrDefault("groups"),
                    allocationRatioPlanned = .self$.getParameterValueIfUserDefinedOrDefault("allocationRatioPlanned")
                )
            }
            result$.plotSettings <- .self$.plotSettings
            return(result)
        },
        show = function(showType = 1, digits = NA_integer_) {
            "Method for automatically printing trial plan objects"
            callSuper(showType = showType, digits = digits)
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
TrialDesignPlanRates <- setRefClass("TrialDesignPlanRates",
    contains = "TrialDesignPlan",
    fields = list(
        riskRatio = "logical",
        thetaH0 = "numeric",
        normalApproximation = "logical",
        pi1 = "numeric",
        pi2 = "numeric",
        groups = "numeric",
        allocationRatioPlanned = "numeric",
        optimumAllocationRatio = "logical",
        directionUpper = "logical",
        effect = "numeric",
        maxNumberOfSubjects = "numeric",
        maxNumberOfSubjects1 = "numeric",
        maxNumberOfSubjects2 = "numeric",
        numberOfSubjects = "matrix",
        numberOfSubjects1 = "matrix",
        numberOfSubjects2 = "matrix",
        overallReject = "numeric",
        rejectPerStage = "matrix",
        futilityStop = "numeric",
        futilityPerStage = "matrix",
        earlyStop = "numeric",
        expectedNumberOfSubjects = "numeric",
        nFixed = "numeric",
        nFixed1 = "numeric",
        nFixed2 = "numeric",
        informationRates = "matrix",
        expectedNumberOfSubjectsH0 = "numeric",
        expectedNumberOfSubjectsH01 = "numeric",
        expectedNumberOfSubjectsH1 = "numeric",
        criticalValuesEffectScale = "matrix",
        criticalValuesEffectScaleLower = "matrix",
        criticalValuesEffectScaleUpper = "matrix",
        criticalValuesPValueScale = "matrix",
        futilityBoundsEffectScale = "matrix",
        futilityBoundsEffectScaleLower = "matrix",
        futilityBoundsEffectScaleUpper = "matrix",
        futilityBoundsPValueScale = "matrix"
    ),
    methods = list(
        initialize = function(...,
                normalApproximation = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["normalApproximation"]],
                riskRatio = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["riskRatio"]],
                thetaH0 = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["thetaH0"]],
                pi1 = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["pi1"]],
                pi2 = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["pi2"]],
                groups = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["groups"]],
                allocationRatioPlanned = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["allocationRatioPlanned"]]) {
            callSuper(...,
                normalApproximation = normalApproximation,
                riskRatio = riskRatio,
                thetaH0 = thetaH0,
                pi1 = pi1,
                pi2 = pi2,
                groups = groups,
                allocationRatioPlanned = allocationRatioPlanned
            )

            optimumAllocationRatio <<- FALSE
            visibleFieldNames <- .getVisibleFieldNames()
            startIndex <- which(visibleFieldNames == "directionUpper")
            for (i in startIndex:length(visibleFieldNames)) {
                .setParameterType(visibleFieldNames[i], C_PARAM_NOT_APPLICABLE)
            }

            if (groups == 1) {
                .setParameterType("meanRatio", C_PARAM_NOT_APPLICABLE)
                .setParameterType("allocationRatioPlanned", C_PARAM_NOT_APPLICABLE)
            }

            .setParameterType("maxNumberOfSubjects1", C_PARAM_NOT_APPLICABLE)
            .setParameterType("maxNumberOfSubjects2", C_PARAM_NOT_APPLICABLE)

            .setParameterType("criticalValuesEffectScale", C_PARAM_NOT_APPLICABLE)
            .setParameterType("criticalValuesEffectScaleLower", C_PARAM_NOT_APPLICABLE)
            .setParameterType("criticalValuesEffectScaleUpper", C_PARAM_NOT_APPLICABLE)

            .setParameterType("futilityBoundsEffectScale", C_PARAM_NOT_APPLICABLE)
            .setParameterType("futilityBoundsEffectScaleLower", C_PARAM_NOT_APPLICABLE)
            .setParameterType("futilityBoundsEffectScaleUpper", C_PARAM_NOT_APPLICABLE)
        },
        clone = function(pi1 = NA_real_) {
            pi1Temp <- pi1
            if (any(is.na(pi1))) {
                pi1Temp <- .self$pi1
            }
            if (.objectType == "sampleSize") {
                return(getSampleSizeRates(
                    design = .self$.design,
                    normalApproximation = .self$.getParameterValueIfUserDefinedOrDefault("normalApproximation"),
                    riskRatio = .self$riskRatio, # .getParameterValueIfUserDefinedOrDefault("riskRatio"),
                    thetaH0 = .self$.getParameterValueIfUserDefinedOrDefault("thetaH0"),
                    pi1 = pi1Temp,
                    pi2 = .self$.getParameterValueIfUserDefinedOrDefault("pi2"),
                    groups = .self$.getParameterValueIfUserDefinedOrDefault("groups"),
                    allocationRatioPlanned = .self$.getParameterValueIfUserDefinedOrDefault("allocationRatioPlanned")
                ))
            } else {
                return(getPowerRates(
                    design = .self$.design,
                    riskRatio = .self$riskRatio, # .getParameterValueIfUserDefinedOrDefault("riskRatio"),
                    thetaH0 = .self$.getParameterValueIfUserDefinedOrDefault("thetaH0"),
                    pi1 = pi1Temp,
                    pi2 = .self$.getParameterValueIfUserDefinedOrDefault("pi2"),
                    directionUpper = .self$.getParameterValueIfUserDefinedOrDefault("directionUpper"),
                    maxNumberOfSubjects = .self$.getParameterValueIfUserDefinedOrDefault("maxNumberOfSubjects"),
                    groups = .self$.getParameterValueIfUserDefinedOrDefault("groups"),
                    allocationRatioPlanned = .self$.getParameterValueIfUserDefinedOrDefault("allocationRatioPlanned")
                ))
            }
        },
        show = function(showType = 1, digits = NA_integer_) {
            "Method for automatically printing trial plan objects"
            callSuper(showType = showType, digits = digits)
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
TrialDesignPlanSurvival <- setRefClass("TrialDesignPlanSurvival",
    contains = "TrialDesignPlan",
    fields = list(
        .piecewiseSurvivalTime = "PiecewiseSurvivalTime",
        .accrualTime = "AccrualTime",
        .calculateFollowUpTime = "logical",
        thetaH0 = "numeric",
        typeOfComputation = "character",
        directionUpper = "logical",
        pi1 = "numeric",
        pi2 = "numeric",
        median1 = "numeric",
        median2 = "numeric",
        lambda1 = "numeric",
        lambda2 = "numeric",
        hazardRatio = "numeric",
        maxNumberOfSubjects = "numeric",
        maxNumberOfSubjects1 = "numeric",
        maxNumberOfSubjects2 = "numeric",
        numberOfSubjects = "matrix",
        numberOfSubjects1 = "matrix",
        numberOfSubjects2 = "matrix",
        maxNumberOfEvents = "numeric",
        allocationRatioPlanned = "numeric",
        optimumAllocationRatio = "logical",
        accountForObservationTimes = "logical",
        eventTime = "numeric",
        accrualTime = "numeric",
        totalAccrualTime = "numeric",
        accrualIntensity = "numeric",
        accrualIntensityRelative = "numeric",
        kappa = "numeric",
        piecewiseSurvivalTime = "numeric",
        followUpTime = "numeric",
        dropoutRate1 = "numeric",
        dropoutRate2 = "numeric",
        dropoutTime = "numeric",
        chi = "numeric",
        expectedNumberOfEvents = "numeric",
        eventsFixed = "numeric",
        nFixed = "numeric",
        nFixed1 = "numeric",
        nFixed2 = "numeric",
        overallReject = "numeric",
        rejectPerStage = "matrix",
        futilityStop = "numeric",
        futilityPerStage = "matrix",
        earlyStop = "numeric",
        informationRates = "matrix",
        analysisTime = "matrix",
        studyDurationH1 = "numeric",
        studyDuration = "numeric",
        maxStudyDuration = "numeric",
        eventsPerStage = "matrix",
        expectedEventsH0 = "numeric",
        expectedEventsH01 = "numeric",
        expectedEventsH1 = "numeric",
        expectedNumberOfSubjectsH1 = "numeric",
        expectedNumberOfSubjects = "numeric",
        criticalValuesEffectScale = "matrix",
        criticalValuesEffectScaleLower = "matrix",
        criticalValuesEffectScaleUpper = "matrix",
        criticalValuesPValueScale = "matrix",
        futilityBoundsEffectScale = "matrix",
        futilityBoundsEffectScaleLower = "matrix",
        futilityBoundsEffectScaleUpper = "matrix",
        futilityBoundsPValueScale = "matrix"
    ),
    methods = list(
        initialize = function(...) {
            callSuper(...)

            optimumAllocationRatio <<- FALSE
            visibleFieldNames <- .getVisibleFieldNames()
            startIndex <- which(visibleFieldNames == "hazardRatio")
            for (i in startIndex:length(visibleFieldNames)) {
                .setParameterType(visibleFieldNames[i], C_PARAM_NOT_APPLICABLE)
            }

            .setParameterType("maxNumberOfSubjects", C_PARAM_NOT_APPLICABLE)
            .setParameterType("maxNumberOfSubjects1", C_PARAM_NOT_APPLICABLE)
            .setParameterType("maxNumberOfSubjects2", C_PARAM_NOT_APPLICABLE)
            .setParameterType("median1", C_PARAM_NOT_APPLICABLE)
            .setParameterType("median2", C_PARAM_NOT_APPLICABLE)
            .setParameterType("accountForObservationTimes", C_PARAM_NOT_APPLICABLE)
            .setParameterType("chi", C_PARAM_NOT_APPLICABLE)
            .setParameterType("maxStudyDuration", C_PARAM_NOT_APPLICABLE)
            .setParameterType("accrualIntensityRelative", C_PARAM_NOT_APPLICABLE)

            .setParameterType("criticalValuesEffectScale", C_PARAM_NOT_APPLICABLE)
            .setParameterType("criticalValuesEffectScaleLower", C_PARAM_NOT_APPLICABLE)
            .setParameterType("criticalValuesEffectScaleUpper", C_PARAM_NOT_APPLICABLE)

            .setParameterType("futilityBoundsEffectScale", C_PARAM_NOT_APPLICABLE)
            .setParameterType("futilityBoundsEffectScaleLower", C_PARAM_NOT_APPLICABLE)
            .setParameterType("futilityBoundsEffectScaleUpper", C_PARAM_NOT_APPLICABLE)

            # set default values
            for (parameterName in c(
                "eventTime", "accrualTime", "accrualIntensity",
                "kappa", "piecewiseSurvivalTime", "lambda1", "lambda2",
                "followUpTime", "dropoutTime"
            )) {
                .setDefaultValue(parameterName)
            }
        },
        clone = function(hazardRatio = NA_real_, pi1 = NA_real_) {
            hr <- NA_real_
            if (.getParameterType("hazardRatio") == C_PARAM_USER_DEFINED) {
                hr <- hazardRatio
                if (any(is.na(hazardRatio))) {
                    hr <- .self$hazardRatio
                }
            }
            pi1Temp <- NA_real_
            if (.getParameterType("pi1") == C_PARAM_USER_DEFINED) {
                pi1Temp <- pi1
                if (any(is.na(pi1))) {
                    pi1Temp <- .self$pi1
                }
            }
            accrualTimeTemp <- .self$.getParameterValueIfUserDefinedOrDefault("accrualTime")
            if (!is.null(accrualTimeTemp) && length(accrualTimeTemp) > 0 &&
                    !all(is.na(accrualTimeTemp)) && accrualTimeTemp[1] != 0) {
                accrualTimeTemp <- c(0, accrualTimeTemp)
            }
            accrualIntensityTemp <- .self$.getParameterValueIfUserDefinedOrDefault("accrualIntensity")
            if (all(is.na(accrualIntensityTemp))) {
                accrualIntensityTemp <- C_ACCRUAL_INTENSITY_DEFAULT
            }
            if (.objectType == "sampleSize") {
                return(getSampleSizeSurvival(
                    design = .self$.design,
                    typeOfComputation = .self$.getParameterValueIfUserDefinedOrDefault("typeOfComputation"),
                    thetaH0 = .self$.getParameterValueIfUserDefinedOrDefault("thetaH0"),
                    pi1 = pi1Temp,
                    pi2 = .self$.getParameterValueIfUserDefinedOrDefault("pi2"),
                    allocationRatioPlanned = .self$allocationRatioPlanned,
                    accountForObservationTimes = .self$.getParameterValueIfUserDefinedOrDefault("accountForObservationTimes"),
                    eventTime = .self$eventTime,
                    accrualTime = accrualTimeTemp,
                    accrualIntensity = accrualIntensityTemp,
                    kappa = .self$kappa,
                    piecewiseSurvivalTime = .self$.getParameterValueIfUserDefinedOrDefault("piecewiseSurvivalTime"),
                    lambda2 = .self$.getParameterValueIfUserDefinedOrDefault("lambda2"),
                    lambda1 = .self$.getParameterValueIfUserDefinedOrDefault("lambda1"),
                    followUpTime = .self$.getParameterValueIfUserDefinedOrDefault("followUpTime"),
                    maxNumberOfSubjects = .self$.getParameterValueIfUserDefinedOrDefault("maxNumberOfSubjects"),
                    dropoutRate1 = .self$dropoutRate1,
                    dropoutRate2 = .self$dropoutRate2,
                    dropoutTime = .self$dropoutTime,
                    hazardRatio = hr
                ))
            } else {
                directionUpperTemp <- directionUpper
                if (length(directionUpperTemp) > 1) {
                    directionUpperTemp <- directionUpperTemp[1]
                }
                return(getPowerSurvival(
                    design = .self$.design,
                    typeOfComputation = .self$.getParameterValueIfUserDefinedOrDefault("typeOfComputation"),
                    thetaH0 = .self$.getParameterValueIfUserDefinedOrDefault("thetaH0"),
                    pi1 = pi1Temp,
                    pi2 = .self$.getParameterValueIfUserDefinedOrDefault("pi2"),
                    directionUpper = directionUpperTemp,
                    allocationRatioPlanned = .self$allocationRatioPlanned,
                    eventTime = .self$eventTime,
                    accrualTime = accrualTimeTemp,
                    accrualIntensity = accrualIntensityTemp,
                    kappa = .self$kappa,
                    piecewiseSurvivalTime = .self$.getParameterValueIfUserDefinedOrDefault("piecewiseSurvivalTime"),
                    lambda2 = .self$.getParameterValueIfUserDefinedOrDefault("lambda2"),
                    lambda1 = .self$.getParameterValueIfUserDefinedOrDefault("lambda1"),
                    hazardRatio = hr,
                    maxNumberOfSubjects = .self$.getParameterValueIfUserDefinedOrDefault("maxNumberOfSubjects"),
                    maxNumberOfEvents = .self$.getParameterValueIfUserDefinedOrDefault("maxNumberOfEvents"),
                    dropoutRate1 = .self$dropoutRate1,
                    dropoutRate2 = .self$dropoutRate2,
                    dropoutTime = .self$dropoutTime
                ))
            }
        },
        .setDefaultValue = function(argumentName) {
            if (is.null(.self[[argumentName]]) || all(is.na(.self[[argumentName]]))) {
                .self[[argumentName]] <<- C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL[[argumentName]]
                .setParameterType(argumentName, C_PARAM_DEFAULT_VALUE)
            }
        },
        show = function(showType = 1, digits = NA_integer_) {
            "Method for automatically printing trial plan objects"
            callSuper(showType = showType, digits = digits)
        },
        .warnInCaseArgumentExists = function(argument, argumentName) {
            if (!all(is.na(argument)) && any(argument > 0)) {
                warning(sprintf(
                    "Specified '%s' (%s) not taken into account",
                    argumentName, .arrayToString(argument)
                ), call. = FALSE)
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
TrialDesignPlanCountData <- setRefClass("TrialDesignPlanCountData",
    contains = "TrialDesignPlan",
    fields = list(
        .designCharacteristics = "ANY",
        thetaH0 = "numeric",
        groups = "integer",
        allocationRatioPlanned = "numeric",
        optimumAllocationRatio = "logical",
        directionUpper = "logical",
        lambda1 = "numeric",
        lambda2 = "numeric",
        lambda = "numeric",
        theta = "numeric",
        nFixed = "numeric",
        nFixed1 = "numeric",
        nFixed2 = "numeric",
        maxNumberOfSubjects = "numeric",
        maxNumberOfSubjects1 = "numeric",
        maxNumberOfSubjects2 = "numeric",
        overallReject = "numeric",
        rejectPerStage = "matrix",
        futilityStop = "numeric",
        futilityPerStage = "matrix",
        earlyStop = "numeric",
        overdispersion = "numeric",
        fixedExposureTime = "numeric",
        accrualTime = "numeric",
        accrualIntensity = "numeric",
        followUpTime = "numeric",
        calendarTime = "matrix",
        expectedStudyDurationH1 = "numeric",
        studyTime = "numeric",
        numberOfSubjects = "matrix",
        expectedNumberOfSubjectsH1 = "numeric",
        informationOverStages = "matrix",
        expectedInformationH0 = "numeric",
        expectedInformationH01 = "numeric",
        expectedInformationH1 = "numeric",
        maxInformation = "numeric",
        futilityBoundsPValueScale = "matrix"
    ),
    methods = list(
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
            callSuper(...,
                .designCharacteristics = designCharacteristics,
                lambda1 = lambda1,
                lambda2 = lambda2,
                lambda = lambda,
                theta = theta,
                thetaH0 = thetaH0,
                overdispersion = overdispersion,
                fixedExposureTime = fixedExposureTime,
                accrualTime = accrualTime,
                accrualIntensity = accrualIntensity,
                followUpTime = followUpTime,
                allocationRatioPlanned = allocationRatioPlanned
            )

            groups <<- 2L
            optimumAllocationRatio <<- FALSE
            .self$.setParameterType("groups", C_PARAM_NOT_APPLICABLE)
            .self$.setParameterType("directionUpper", C_PARAM_NOT_APPLICABLE)
            .self$.setParameterType("optimumAllocationRatio", C_PARAM_NOT_APPLICABLE)
        },
        .toString = function(startWithUpperCase = FALSE) {
            s <- "count data"
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
        },
        clone = function(..., lambda1 = NA_real_, theta = NA_real_) {
            if (all(is.na(lambda1))) {
                lambda1Temp <- .self$.getParameterValueIfUserDefinedOrDefault("lambda1")
            } else {
                lambda1Temp <- lambda1
                if (any(is.na(lambda1))) {
                    lambda1Temp <- .self$lambda1
                }
            }
            if (all(is.na(theta))) {
                thetaTemp <- .self$.getParameterValueIfUserDefinedOrDefault("theta")
            } else {
                thetaTemp <- theta
                if (any(is.na(theta))) {
                    thetaTemp <- .self$theta
                }
            }
            if (.objectType == "sampleSize") {
                result <- getSampleSizeCounts(
                    design = .self$.design,
                    lambda1 = lambda1Temp,
                    lambda2 = .self$.getParameterValueIfUserDefinedOrDefault("lambda2"),
                    lambda = .self$.getParameterValueIfUserDefinedOrDefault("lambda"),
                    theta = thetaTemp,
                    thetaH0 = .self$.getParameterValueIfUserDefinedOrDefault("thetaH0"),
                    overdispersion = .self$.getParameterValueIfUserDefinedOrDefault("overdispersion"),
                    fixedExposureTime = .self$.getParameterValueIfUserDefinedOrDefault("fixedExposureTime"),
                    accrualTime = .self$.getParameterValueIfUserDefinedOrDefault("accrualTime"),
                    accrualIntensity = .self$.getParameterValueIfUserDefinedOrDefault("accrualIntensity"),
                    followUpTime = .self$.getParameterValueIfUserDefinedOrDefault("followUpTime"),
                    maxNumberOfSubjects = .self$.getParameterValueIfUserDefinedOrDefault("maxNumberOfSubjects"),
                    allocationRatioPlanned = .self$.getParameterValueIfUserDefinedOrDefault("allocationRatioPlanned")
                )
            } else {
                result <- getPowerCounts(
                    design = .self$.design,
                    directionUpper = .self$.getParameterValueIfUserDefinedOrDefault("directionUpper"),
                    maxNumberOfSubjects = .self$.getParameterValueIfUserDefinedOrDefault("maxNumberOfSubjects"),
                    lambda1 = lambda1Temp,
                    lambda2 = .self$.getParameterValueIfUserDefinedOrDefault("lambda2"),
                    lambda = .self$.getParameterValueIfUserDefinedOrDefault("lambda"),
                    theta = thetaTemp,
                    thetaH0 = .self$.getParameterValueIfUserDefinedOrDefault("thetaH0"),
                    overdispersion = .self$.getParameterValueIfUserDefinedOrDefault("overdispersion"),
                    fixedExposureTime = .self$.getParameterValueIfUserDefinedOrDefault("fixedExposureTime"),
                    accrualTime = .self$.getParameterValueIfUserDefinedOrDefault("accrualTime"),
                    accrualIntensity = .self$.getParameterValueIfUserDefinedOrDefault("accrualIntensity"),
                    followUpTime = .self$.getParameterValueIfUserDefinedOrDefault("followUpTime"),
                    allocationRatioPlanned = .self$.getParameterValueIfUserDefinedOrDefault("allocationRatioPlanned")
                )
            }
            result$.plotSettings <- .self$.plotSettings
            return(result)
        }
    )
)
