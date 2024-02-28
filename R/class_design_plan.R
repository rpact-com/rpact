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
## |  File version: $Revision: 7352 $
## |  Last changed: $Date: 2023-10-12 07:56:59 +0200 (Do, 12 Okt 2023) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_constants.R
#' @include f_design_utilities.R
NULL

C_VARIABLE_DESIGN_PLAN_PARAMETERS <- c("lambda1", "pi1", "median1", "alternative", "hazardRatio")

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
TrialDesignPlan <- R6Class("TrialDesignPlan",
    inherit = ParameterSet,
    public = list(
        .plotSettings = NULL,
        .design = NULL,
        .objectType = NULL, # "sampleSize" or "power"
        initialize = function(design, ...) {
            self$.design <- design
            
            super$initialize(...)#TODO

            self$.plotSettings <- PlotSettings$new()
            self$.parameterNames <- .getParameterNames(design = design, designPlan = self)
            self$.parameterFormatFunctions <- C_PARAMETER_FORMAT_FUNCTIONS

            if (.isTrialDesignPlanMeans(self)) {
                defaultValueList <- C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS
            } else if (.isTrialDesignPlanRates(self)) {
                defaultValueList <- C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES
            } else if (.isTrialDesignPlanSurvival(self)) {
                defaultValueList <- C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL
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
        .setSampleSizeObject = function(objectType) {
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
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'.objectType' must be specified as 'sampleSize' or 'power'")
            }
            return(self$.objectType == "sampleSize")
        },
        .isPowerObject = function() {
            if (length(self$.objectType) == 0 || !(self$.objectType %in% c("sampleSize", "power"))) {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'.objectType' must be specified as 'sampleSize' or 'power'")
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

                self$.showParametersOfOneGroup(.getDesignParametersToShow(self), "Design parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )

                self$.showParametersOfOneGroup(self$.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getDefaultParameters(), "Default parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getGeneratedParameters(), "Sample size and output",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )

                self$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)

                if (inherits(self, "TrialDesignPlanSurvival") || inherits(self, "TrialDesignPlanSurvival") || self$groups == 2 || self$.design$kMax > 1) {#TODO Groups????
                    self$.cat("Legend:\n",
                        heading = 2,
                        consoleOutputEnabled = consoleOutputEnabled
                    )
                    if (inherits(self, "TrialDesignPlanSurvival") || inherits(self, "TrialDesignPlanSurvival") || self$groups == 2) {
                        self$.cat("  (i): values of treatment arm i\n",
                            consoleOutputEnabled = consoleOutputEnabled
                        )
                    }
                    if (self$.design$kMax > 1) {
                        self$.cat("  [k]: values at stage k\n", consoleOutputEnabled = consoleOutputEnabled)
                    }
                }

                self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
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
                s <- "means"
            } else if (.isTrialDesignPlanRates(self)) {
                s <- "rates"
            } else if (.isTrialDesignPlanSurvival(self)) {
                s <- "survival data"
            } else {
                s <- paste0("unknown data class '", .getClassName(self), "'")
            }
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s)) #TODO correct closure of s?
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
TrialDesignPlanMeans <- R6Class("TrialDesignPlanMeans",
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
        maxNumberOfSubjects = NULL,
        maxNumberOfSubjects1 = NULL,
        maxNumberOfSubjects2 = NULL,
        numberOfSubjects = NULL,
        numberOfSubjects1 = NULL,
        numberOfSubjects2 = NULL,
        expectedNumberOfSubjectsH0 = NULL,
        expectedNumberOfSubjectsH01 = NULL,
        expectedNumberOfSubjectsH1 = NULL,
        criticalValuesEffectScale = NULL,
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
            
            super$initialize(...)#TODO

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
TrialDesignPlanRates <- R6Class("TrialDesignPlanRates",
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
        expectedNumberOfSubjects = NULL,
        nFixed = NULL,
        nFixed1 = NULL,
        nFixed2 = NULL,
        overallReject = NULL,
        rejectPerStage = NULL,
        futilityStop = NULL,
        futilityPerStage = NULL,
        earlyStop = NULL,
        informationRates = NULL,
        maxNumberOfSubjects = NULL,
        maxNumberOfSubjects1 = NULL,
        maxNumberOfSubjects2 = NULL,
        numberOfSubjects = NULL,
        numberOfSubjects1 = NULL,
        numberOfSubjects2 = NULL,
        expectedNumberOfSubjectsH0 = NULL,
        expectedNumberOfSubjectsH01 = NULL,
        expectedNumberOfSubjectsH1 = NULL,
        criticalValuesEffectScale = NULL,
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
            super$initialize(...) #TODO
          
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
TrialDesignPlanSurvival <- R6Class("TrialDesignPlanSurvival",
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
        eventsPerStage = NULL,
        expectedEventsH0 = NULL,
        expectedEventsH01 = NULL,
        expectedEventsH1 = NULL,
        numberOfSubjects = NULL,
        numberOfSubjects1 = NULL,
        numberOfSubjects2 = NULL,
        expectedNumberOfSubjectsH1 = NULL,
        expectedNumberOfSubjects = NULL,
        criticalValuesEffectScale = NULL,
        criticalValuesEffectScaleLower = NULL,
        criticalValuesEffectScaleUpper = NULL,
        criticalValuesPValueScale = NULL,
        futilityBoundsEffectScale = NULL,
        futilityBoundsEffectScaleLower = NULL,
        futilityBoundsEffectScaleUpper = NULL,
        futilityBoundsPValueScale = NULL,
        initialize = function(...,typeOfComputation = NULL,
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
        }
    )
)

.addPlotSubTitleItems <- function(designPlan, designMaster, items, type) {
    if (type %in% c(1, 3, 4)) {
        return(invisible())
    }

    if (.isTrialDesignPlanMeans(designPlan)) {
        nMax <- designPlan$maxNumberOfSubjects[1] # use first value for plotting

        if (!(type %in% c(5))) {
            items$add("N", round(nMax, 1), "max")
        }

        if ((type %in% c(5)) && !(items$title == "Sample Size")) {
            items$add("N", round(nMax, 1), "max")
        }

        if (designPlan$meanRatio) {
            items$add("coefficient of variation", designPlan$stDev)
        } else {
            items$add("standard deviation", designPlan$stDev)
        }

        if (designPlan$groups == 1) {
            if (type %in% c(2, (5:9))) {
                items$add("H0: mu", designPlan$thetaH0)
                items$add("allocation ratio", round(designPlan$allocationRatioPlanned, 2))
            }
        } else {
            if (type %in% c(2, (5:9))) {
                if (designPlan$meanRatio) {
                    items$add("H0: mean ratio", designPlan$thetaH0)
                } else {
                    items$add("H0: mean difference", designPlan$thetaH0)
                }
                items$add("allocation ratio", round(designPlan$allocationRatioPlanned, 2))
            }
        }
    } else if (.isTrialDesignPlanRates(designPlan)) {
        nMax <- designPlan$maxNumberOfSubjects[1] # use first value for plotting

        if (!(type %in% c(5))) {
            items$add("N", round(nMax, 1), "max")
        }

        if ((type %in% c(5)) && !(items$title == "Sample Size")) {
            items$add("N", round(nMax, 1), "max")
        }

        if (designPlan$groups == 2 && !(type %in% c(3, 4)) &&
                length(designPlan$pi2) == 1 && !is.na(designPlan$pi2)) {
            items$add("pi", designPlan$pi2, 2)
        }

        if (designPlan$groups == 1) {
            if (type %in% c(2, (5:9))) {
                items$add("H0: pi", designPlan$thetaH0)
            }
        } else {
            if (type %in% c(2, (5:9))) {
                if (designPlan$riskRatio) {
                    items$add("H0: risk ratio", designPlan$thetaH0)
                } else {
                    items$add("H0: risk difference", designPlan$thetaH0)
                }
                items$add("allocation ratio", round(designPlan$allocationRatioPlanned, 2))
            }
        }
    } else if (.isTrialDesignPlanSurvival(designPlan)) {
        if (designPlan$.isPowerObject() && !(type %in% (13:14))) {
            items$add("maximum number of events", designPlan$maxNumberOfEvents[1])
        }
        if (type %in% (10:12)) {
            items$add("maximum number of subjects", designPlan$maxNumberOfSubjects[1])
        }
        if (type %in% c(2, (5:12))) {
            items$add("H0: hazard ratio", designPlan$thetaH0)
            items$add("allocation ratio", round(designPlan$allocationRatioPlanned, 2))
        }
    }
}

.assertIsValidVariedParameterVectorForPlotting <- function(designPlan, plotType) {
    if (.isTrialDesignPlanMeans(designPlan)) {
        if (is.null(designPlan$alternative) || any(is.na(designPlan$alternative)) ||
                length(designPlan$alternative) <= 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType,
                " is only available if 'alternative' with length > 1 is defined"
            )
        }
    } else if (.isTrialDesignPlanRates(designPlan)) {
        if (is.null(designPlan$pi1) || any(is.na(designPlan$pi1)) ||
                length(designPlan$pi1) <= 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType,
                " is only available if 'pi1' with length > 1 is defined"
            )
        }
    } else if (.isTrialDesignPlanSurvival(designPlan)) {
        if (is.null(designPlan$hazardRatio) || any(is.na(designPlan$hazardRatio)) ||
                length(designPlan$hazardRatio) <= 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType,
                " is only available if 'hazardRatio' with length > 1 is defined"
            )
        }
    }
}

.plotTrialDesignPlan <- function(designPlan, type = 1L, main = NA_character_,
        xlab = NA_character_, ylab = NA_character_, palette = "Set1",
        theta = seq(-1, 1, 0.02), plotPointsEnabled = NA,
        legendPosition = NA_integer_, showSource = FALSE,
        designPlanName = NA_character_, plotSettings = NULL, ...) {
    .assertGgplotIsInstalled()
    .assertIsTrialDesignPlan(designPlan)
    .assertIsValidLegendPosition(legendPosition)
    .assertIsSingleInteger(type, "type", naAllowed = FALSE, validateType = FALSE)
    theta <- .assertIsValidThetaRange(thetaRange = theta)

    survivalDesignPlanEnabled <- .isTrialDesignPlanSurvival(designPlan)

    nMax <- ifelse(survivalDesignPlanEnabled, designPlan$maxNumberOfEvents[1],
        designPlan$maxNumberOfSubjects[1]
    ) # use first value for plotting

    if (is.null(plotSettings)) {
        plotSettings <- designPlan$.plotSettings
    }

    designMaster <- designPlan$.design

    if (designMaster$kMax == 1 && (type %in% c(1:4))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type,
            ") is not available for 'kMax' = 1"
        )
    }

    if (designPlan$.isSampleSizeObject()) {
        if (survivalDesignPlanEnabled) {
            if (!(type %in% c(1:5, 13, 14))) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type,
                    ") is not allowed; must be 1, 2, 3, 4, 5, 13 or 14"
                )
            }
        } else {
            if (!(type %in% c(1:5))) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type,
                    ") is not allowed; must be 1, 2, 3, 4, 5"
                )
            }
        }
    }

    if (is.na(plotPointsEnabled)) {
        plotPointsEnabled <- type < 4
    }

    ratioEnabled <- (survivalDesignPlanEnabled ||
        (.isTrialDesignPlanMeans(designPlan) && designPlan$meanRatio) ||
        (.isTrialDesignPlanRates(designPlan) && designPlan$riskRatio))

    variedParameters <- logical(0)

    showSourceHint <- ""
    if (type %in% c(5:12)) {
        if (.isTrialDesignPlanMeans(designPlan) && length(designPlan$alternative) == 2 &&
                designPlan$.getParameterType("alternative") == C_PARAM_USER_DEFINED) {
            if (!is.logical(showSource) || isTRUE(showSource)) {
                showSourceHint <- .getVariedParameterHint(designPlan$alternative, "alternative")
            }
            designPlan <- designPlan$clone(
                alternative =
                    .getVariedParameterVector(designPlan$alternative, "alternative")
            )
        } else if ((.isTrialDesignPlanRates(designPlan) || survivalDesignPlanEnabled) &&
                length(designPlan$pi1) == 2 &&
                designPlan$.getParameterType("pi1") == C_PARAM_USER_DEFINED) {
            if (!is.logical(showSource) || isTRUE(showSource)) {
                showSourceHint <- .getVariedParameterHint(designPlan$pi1, "pi1")
            }
            designPlan <- designPlan$clone(
                pi1 =
                    .getVariedParameterVector(designPlan$pi1, "pi1")
            )
        } else if (survivalDesignPlanEnabled && length(designPlan$hazardRatio) == 2 &&
                designPlan$.getParameterType("hazardRatio") == C_PARAM_USER_DEFINED) {
            if (!is.logical(showSource) || isTRUE(showSource)) {
                showSourceHint <- .getVariedParameterHint(designPlan$hazardRatio, "hazardRatio")
            }
            designPlan <- designPlan$clone(
                hazardRatio =
                    .getVariedParameterVector(designPlan$hazardRatio, "hazardRatio")
            )
        }
    }

    srcCmd <- NULL

    reducedParam <- NULL
    if (type %in% c(1:4)) {
        reducedParam <- .warnInCaseOfUnusedValuesForPlotting(designPlan)
    }

    if (type == 1) { # Boundary plot
        if (survivalDesignPlanEnabled) {
            if (is.na(main)) {
                main <- PlotSubTitleItems(title = "Boundaries Z Scale")
                .addPlotSubTitleItems(designPlan, designMaster, main, type)
                if (!is.null(reducedParam)) {
                    main$add(reducedParam$title, reducedParam$value, reducedParam$subscript)
                }
            }

            if (designMaster$sided == 1) {
                designPlan <- data.frame(
                    eventsPerStage = designPlan$eventsPerStage[, 1],
                    criticalValues = designMaster$criticalValues,
                    futilityBounds = c(designMaster$futilityBounds, designMaster$criticalValues[designMaster$kMax])
                )
            } else {
                designPlan <- data.frame(
                    eventsPerStage = designPlan$eventsPerStage[, 1],
                    criticalValues = designMaster$criticalValues,
                    criticalValuesMirrored = -designMaster$criticalValues
                )
            }

            xParameterName <- "eventsPerStage"
            if (designMaster$sided == 1) {
                if (any(designMaster$futilityBounds > C_FUTILITY_BOUNDS_DEFAULT)) {
                    yParameterNames <- c("futilityBounds", "criticalValues")
                } else {
                    yParameterNames <- "criticalValues"
                }
                yParameterNamesSrc <- yParameterNames
            } else {
                yParameterNames <- c("criticalValues", "criticalValuesMirrored")
                yParameterNamesSrc <- c("criticalValues", paste0("-", designPlanName, "$.design$criticalValues"))
            }

            if (is.na(legendPosition)) {
                legendPosition <- C_POSITION_RIGHT_TOP
            }

            srcCmd <- .showPlotSourceInformation(
                objectName = paste0(designPlanName, "$.design"),
                xParameterName = paste0(designPlanName, "$", xParameterName, "[, 1]"),
                yParameterNames = yParameterNamesSrc,
                hint = showSourceHint, nMax = nMax,
                type = type, showSource = showSource
            )
        } else {
            if (is.na(main)) {
                main <- PlotSubTitleItems(title = "Boundaries")
                .addPlotSubTitleItems(designPlan, designMaster, main, type)
                if (!is.null(reducedParam)) {
                    main$add(reducedParam$title, reducedParam$value, reducedParam$subscript)
                }
            }

            designSet <- TrialDesignSet$new(design = designMaster, singleDesign = TRUE)
            designSet$.plotSettings <- designPlan$.plotSettings
            designPlanName <- paste0(designPlanName, "$.design")
            return(.plotTrialDesignSet(
                x = designSet, y = NULL, main = main,
                xlab = xlab, ylab = ylab, type = type,
                palette = palette, theta = theta, nMax = nMax,
                plotPointsEnabled = plotPointsEnabled, legendPosition = legendPosition,
                designSetName = designPlanName, showSource = showSource,
                plotSettings = plotSettings # , ...
            ))
        }
    } else if (type == 2) { # Effect Scale Boundary plot
        if (is.na(main)) {
            main <- PlotSubTitleItems(title = "Boundaries Effect Scale")
            .addPlotSubTitleItems(designPlan, designMaster, main, type)
            if (!is.null(reducedParam)) {
                main$add(reducedParam$title, reducedParam$value, reducedParam$subscript)
            }
        }

        if (is.na(ylab)) {
            if (.isTrialDesignPlanMeans(designPlan)) {
                if (designPlan$groups == 1) {
                    ylab <- "Mean"
                } else if (!designPlan$meanRatio) {
                    ylab <- "Mean Difference"
                } else {
                    ylab <- "Mean Ratio"
                }
            } else if (.isTrialDesignPlanRates(designPlan)) {
                if (designPlan$groups == 1) {
                    ylab <- "Rate"
                } else if (!designPlan$riskRatio) {
                    ylab <- "Rate Difference"
                } else {
                    ylab <- "Risk Ratio"
                }
            } else if (survivalDesignPlanEnabled) {
                ylab <- "Hazard Ratio"
            }
        }

        groupedPlotEnabled <- FALSE
        yParameterNamesSrc <- c()
        if (designMaster$sided == 1) {
            if (any(designMaster$futilityBounds > C_FUTILITY_BOUNDS_DEFAULT)) {
                data <- data.frame(
                    criticalValuesEffectScale = designPlan$criticalValuesEffectScale[, 1],
                    futilityBoundsEffectScale = c(
                        designPlan$futilityBoundsEffectScale[, 1],
                        designPlan$criticalValuesEffectScale[designMaster$kMax, 1]
                    )
                )
                yParameterNamesSrc <- c(yParameterNamesSrc, "criticalValuesEffectScale[, 1]")
                yParameterNamesSrc <- c(yParameterNamesSrc, paste0(
                    "c(", designPlanName, "$futilityBoundsEffectScale[, 1], ",
                    designPlanName, "$criticalValuesEffectScale[nrow(", designPlanName, "$criticalValuesEffectScale), 1])"
                ))
            } else {
                data <- data.frame(
                    criticalValuesEffectScale = designPlan$criticalValuesEffectScale[, 1]
                )
                yParameterNamesSrc <- c(yParameterNamesSrc, "criticalValuesEffectScale[, 1]")
            }
        } else if (designMaster$typeOfDesign == C_TYPE_OF_DESIGN_PT) {
            data <- data.frame(
                criticalValues = designPlan$criticalValuesEffectScaleUpper[, 1],
                criticalValuesMirrored = designPlan$criticalValuesEffectScaleLower[, 1],
                futilityBounds = c(
                    designPlan$futilityBoundsEffectScaleUpper[, 1],
                    designPlan$criticalValuesEffectScaleUpper[designMaster$kMax, 1]
                ),
                futilityBoundsMirrored = c(
                    designPlan$futilityBoundsEffectScaleLower[, 1],
                    designPlan$criticalValuesEffectScaleLower[designMaster$kMax, 1]
                )
            )
            yParameterNamesSrc <- c(yParameterNamesSrc, "criticalValuesEffectScaleUpper[, 1]")
            yParameterNamesSrc <- c(yParameterNamesSrc, "criticalValuesEffectScaleLower[, 1]")
            yParameterNamesSrc <- c(yParameterNamesSrc, paste0(
                "c(", designPlanName, "$futilityBoundsEffectScaleUpper[, 1], ",
                designPlanName, "$criticalValuesEffectScaleUpper[nrow(", designPlanName, "$criticalValuesEffectScaleUpper), 1])"
            ))
            yParameterNamesSrc <- c(yParameterNamesSrc, paste0(
                "c(", designPlanName, "$futilityBoundsEffectScaleLower[, 1], ",
                designPlanName, "$criticalValuesEffectScaleLower[nrow(", designPlanName, "$criticalValuesEffectScaleLower), 1])"
            ))
            groupedPlotEnabled <- TRUE
        } else {
            data <- data.frame(
                criticalValuesEffectScale = designPlan$criticalValuesEffectScaleUpper[, 1],
                criticalValuesEffectScaleMirrored = designPlan$criticalValuesEffectScaleLower[, 1]
            )
            yParameterNamesSrc <- c(yParameterNamesSrc, "criticalValuesEffectScaleUpper[, 1]")
            yParameterNamesSrc <- c(yParameterNamesSrc, "criticalValuesEffectScaleLower[, 1]")
        }

        if (survivalDesignPlanEnabled) {
            xParameterName <- "eventsPerStage"
            xParameterNameSrc <- paste0(designPlanName, "$", xParameterName, "[, 1]")
            data <- cbind(data.frame(eventsPerStage = designPlan$eventsPerStage[, 1]), data)
        } else {
            xParameterName <- "informationRates"
            xParameterNameSrc <- paste0(designPlanName, "$.design$", xParameterName)
            data <- cbind(data.frame(informationRates = designMaster$informationRates), data)
        }
        if (designMaster$sided == 1 || designMaster$typeOfDesign == C_TYPE_OF_DESIGN_PT) {
            if (any(designMaster$futilityBounds > C_FUTILITY_BOUNDS_DEFAULT)) {
                yParameterNames <- c("futilityBoundsEffectScale", "criticalValuesEffectScale")
            } else {
                yParameterNames <- "criticalValuesEffectScale"
            }
        } else {
            yParameterNames <- c("criticalValuesEffectScale", "criticalValuesEffectScaleMirrored")
        }

        if (is.na(legendPosition)) {
            legendPosition <- C_POSITION_RIGHT_TOP
        }

        if (is.na(legendPosition)) {
            legendPosition <- C_POSITION_RIGHT_TOP
        }

        srcCmd <- .showPlotSourceInformation(
            objectName = designPlanName,
            xParameterName = xParameterNameSrc,
            yParameterNames = yParameterNamesSrc,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )

        if (groupedPlotEnabled) {
            tableColumnNames <- C_TABLE_COLUMN_NAMES
            criticalValuesName <- designPlan$.getDataFrameColumnCaption("criticalValuesEffectScale", tableColumnNames, TRUE)
            futilityBoundsName <- designPlan$.getDataFrameColumnCaption("futilityBoundsEffectScale", tableColumnNames, TRUE)

            designPlan <- data.frame(
                xValues = rep(data[[xParameterName]], 4),
                yValues = c(
                    data$criticalValues, data$criticalValuesMirrored,
                    data$futilityBounds, data$futilityBoundsMirrored
                ),
                categories = c(
                    rep(criticalValuesName, nrow(data)), rep("criticalValuesMirrored", nrow(data)),
                    rep(futilityBoundsName, nrow(data)), rep("futilityBoundsMirrored", nrow(data))
                ),
                groups = c(rep(criticalValuesName, 2 * nrow(data)), rep(futilityBoundsName, 2 * nrow(data)))
            )
        } else {
            designPlan <- data
        }
    } else if (type == 3) { # Stage Levels
        if (is.na(main)) {
            main <- PlotSubTitleItems(title = "Boundaries p Values Scale")
            .addPlotSubTitleItems(designPlan, designMaster, main, type)
            if (!is.null(reducedParam)) {
                main$add(reducedParam$title, reducedParam$value, reducedParam$subscript)
            }
        }

        if (survivalDesignPlanEnabled) {
            xParameterName <- "eventsPerStage"
            yParameterNames <- "stageLevels"
            designPlan <- data.frame(
                eventsPerStage = designPlan$eventsPerStage[, 1],
                stageLevels = designMaster$stageLevels
            )
            xParameterNameSrc <- "eventsPerStage[, 1]"
            yParameterNamesSrc <- ".design$stageLevels"
        } else {
            xParameterName <- "informationRates"
            yParameterNames <- "stageLevels"
            designPlan <- TrialDesignSet$new(design = designMaster, singleDesign = TRUE)
            xParameterNameSrc <- ".design$informationRates"
            yParameterNamesSrc <- ".design$stageLevels"
        }

        srcCmd <- .showPlotSourceInformation(
            objectName = designPlanName,
            xParameterName = xParameterNameSrc,
            yParameterNames = yParameterNamesSrc,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
    } else if (type == 4) { # Alpha Spending
        if (is.na(main)) {
            main <- PlotSubTitleItems(title = "Error Spending")
            .addPlotSubTitleItems(designPlan, designMaster, main, type)
            if (!is.null(reducedParam)) {
                main$add(reducedParam$title, reducedParam$value, reducedParam$subscript)
            }
        }
        if (survivalDesignPlanEnabled) {
            xParameterName <- "eventsPerStage"
            yParameterNames <- "alphaSpent"
            designPlan <- data.frame(
                eventsPerStage = designPlan$eventsPerStage[, 1],
                alphaSpent = designMaster$alphaSpent
            )
            xParameterNameSrc <- "eventsPerStage[, 1]"
            yParameterNamesSrc <- ".design$alphaSpent"
        } else {
            xParameterName <- "informationRates"
            yParameterNames <- "alphaSpent"
            designPlan <- TrialDesignSet$new(design = designMaster, singleDesign = TRUE)
            xParameterNameSrc <- ".design$informationRates"
            yParameterNamesSrc <- ".design$alphaSpent"
        }
        plotPointsEnabled <- ifelse(is.na(plotPointsEnabled), FALSE, plotPointsEnabled)

        srcCmd <- .showPlotSourceInformation(
            objectName = designPlanName,
            xParameterName = xParameterNameSrc,
            yParameterNames = yParameterNamesSrc,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
    } else if (type == 5) { # Power and Stopping Probabilities

        .assertIsValidVariedParameterVectorForPlotting(designPlan, type)

        if (designPlan$.isSampleSizeObject()) {
            if (is.na(main)) {
                main <- PlotSubTitleItems(title = "Sample Size")
                .addPlotSubTitleItems(designPlan, designMaster, main, type)
            }

            yAxisScalingEnabled <- TRUE

            if (.isTrialDesignPlanMeans(designPlan)) {
                xParameterName <- "alternative"
                yParameterNames <- c("nFixed")
                if (designMaster$kMax > 1) {
                    yParameterNames <- c(yParameterNames, "maxNumberOfSubjects", "expectedNumberOfSubjectsH1")
                }
                if (is.na(ylab)) {
                    ylab <- "Sample Size"
                }
                yAxisScalingEnabled <- FALSE
                if (is.na(legendPosition)) {
                    legendPosition <- C_POSITION_RIGHT_TOP
                }
                yParameterNamesSrc <- yParameterNames
            } else if (.isTrialDesignPlanRates(designPlan)) {
                xParameterName <- "pi1"
                yParameterNames <- c("nFixed")
                if (designMaster$kMax > 1) {
                    yParameterNames <- c(yParameterNames, "maxNumberOfSubjects", "expectedNumberOfSubjectsH1")
                }
                if (is.na(ylab)) {
                    ylab <- "Sample Size"
                }
                yAxisScalingEnabled <- FALSE
                if (is.na(legendPosition)) {
                    legendPosition <- C_POSITION_RIGHT_TOP
                }
                yParameterNamesSrc <- yParameterNames
            } else if (survivalDesignPlanEnabled) {
                designPlan <- data.frame(
                    hazardRatio = designPlan$hazardRatio,
                    eventsFixed = designPlan$eventsFixed,
                    maxNumberOfEvents = designPlan$eventsPerStage[designMaster$kMax, ],
                    expectedEventsH1 = designPlan$expectedEventsH1
                )
                xParameterName <- "hazardRatio"
                yParameterNames <- c("eventsFixed")
                if (designMaster$kMax > 1) {
                    yParameterNames <- c(yParameterNames, "maxNumberOfEvents", "expectedEventsH1")
                }
                if (is.na(ylab)) {
                    ylab <- "# Events"
                }

                if (is.na(legendPosition)) {
                    legendPosition <- C_POSITION_RIGHT_TOP
                }
                yParameterNamesSrc <- c(
                    "eventsFixed",
                    paste0("eventsPerStage[", designMaster$kMax, ", ]"), "expectedEventsH1"
                )
            }

            srcCmd <- .showPlotSourceInformation(
                objectName = designPlanName,
                xParameterName = xParameterName,
                yParameterNames = yParameterNamesSrc,
                hint = showSourceHint, nMax = nMax,
                type = type, showSource = showSource
            )
            if (!is.null(srcCmd)) {
                if (.isSpecialPlotShowSourceArgument(showSource)) {
                    return(invisible(srcCmd))
                }
                return(srcCmd)
            }

            return(.plotParameterSet(
                parameterSet = designPlan, designMaster = designMaster,
                xParameterName = xParameterName,
                yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
                palette = palette, theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
                legendPosition = legendPosition, variedParameters = variedParameters,
                qnormAlphaLineEnabled = FALSE, yAxisScalingEnabled = FALSE,
                plotSettings = plotSettings # , ...
            ))
        } else {
            if (is.na(main)) {
                main <- PlotSubTitleItems(title = "Overall Power and Early Stopping")
                .addPlotSubTitleItems(designPlan, designMaster, main, type)
            }
            if (survivalDesignPlanEnabled) {
                xParameterName <- "hazardRatio"
            } else {
                xParameterName <- "effect"
            }
            yParameterNames <- c("overallReject", "futilityStop", "earlyStop")

            if (is.na(ylab)) {
                ylab <- ""
            }
            if (is.na(legendPosition)) {
                legendPosition <- C_POSITION_LEFT_TOP
            }

            srcCmd <- .showPlotSourceInformation(
                objectName = designPlanName,
                xParameterName = xParameterName,
                yParameterNames = yParameterNames,
                hint = showSourceHint, nMax = nMax,
                type = type, showSource = showSource
            )
            if (!is.null(srcCmd)) {
                if (.isSpecialPlotShowSourceArgument(showSource)) {
                    return(invisible(srcCmd))
                }
                return(srcCmd)
            }

            if (is.null(list(...)[["ylim"]])) {
                ylim <- c(0, 1)
                return(.plotParameterSet(
                    parameterSet = designPlan, designMaster = designMaster,
                    xParameterName = xParameterName,
                    yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
                    palette = palette, theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
                    legendPosition = legendPosition, variedParameters = variedParameters,
                    qnormAlphaLineEnabled = FALSE, yAxisScalingEnabled = FALSE,
                    plotSettings = plotSettings, ylim = ylim # , ...
                ))
            } else {
                return(.plotParameterSet(
                    parameterSet = designPlan, designMaster = designMaster,
                    xParameterName = xParameterName,
                    yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
                    palette = palette, theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
                    legendPosition = legendPosition, variedParameters = variedParameters,
                    qnormAlphaLineEnabled = FALSE, yAxisScalingEnabled = FALSE,
                    plotSettings = plotSettings # , ...
                ))
            }
        }
    } else if (type == 6) { # Average Sample Size / Average Event Number
        .assertIsValidVariedParameterVectorForPlotting(designPlan, type)

        if (is.na(main)) {
            titlePart <- ifelse(survivalDesignPlanEnabled, "Number of Events", "Sample Size")
            main <- PlotSubTitleItems(title = paste0("Expected ", titlePart, " and Power / Early Stop"))
            .addPlotSubTitleItems(designPlan, designMaster, main, type)
        }

        if (survivalDesignPlanEnabled) {
            xParameterName <- "hazardRatio"
            yParameterNames <- "expectedNumberOfEvents"
            expectedNumberOfEvents <- designPlan[["expectedNumberOfEvents"]]
            if (is.null(expectedNumberOfEvents) || length(expectedNumberOfEvents) == 0) {
                yParameterNames <- "expectedEventsH1"
            }
            yParameterNames <- c(yParameterNames, "overallReject", "earlyStop") # overallReject = power
            if (is.na(legendPosition)) {
                legendPosition <- C_POSITION_RIGHT_CENTER
            }
        } else {
            xParameterName <- "effect"
            yParameterNames <- c("expectedNumberOfSubjects", "overallReject", "earlyStop") # overallReject = power
        }
        srcCmd <- .showPlotSourceInformation(
            objectName = designPlanName,
            xParameterName = xParameterName,
            yParameterNames = yParameterNames,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
    } else if (type == 7) {
        .assertIsValidVariedParameterVectorForPlotting(designPlan, type)

        if (is.na(main)) {
            main <- PlotSubTitleItems(title = "Overall Power")
            .addPlotSubTitleItems(designPlan, designMaster, main, type)
        }

        if (survivalDesignPlanEnabled) {
            xParameterName <- "hazardRatio"
        } else {
            xParameterName <- "effect"
        }
        yParameterNames <- "overallReject"
        if (is.na(legendPosition)) {
            legendPosition <- C_POSITION_RIGHT_CENTER
        }
        srcCmd <- .showPlotSourceInformation(
            objectName = designPlanName,
            xParameterName = xParameterName,
            yParameterNames = yParameterNames,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
    } else if (type == 8) {
        .assertIsValidVariedParameterVectorForPlotting(designPlan, type)

        if (is.na(main)) {
            main <- PlotSubTitleItems(title = "Overall Early Stopping")
            .addPlotSubTitleItems(designPlan, designMaster, main, type)
        }

        if (survivalDesignPlanEnabled) {
            xParameterName <- "hazardRatio"
        } else {
            xParameterName <- "effect"
        }
        yParameterNames <- c("earlyStop", "futilityStop")
        if (is.na(legendPosition)) {
            legendPosition <- C_POSITION_RIGHT_CENTER
        }
        srcCmd <- .showPlotSourceInformation(
            objectName = designPlanName,
            xParameterName = xParameterName,
            yParameterNames = yParameterNames,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
    } else if (type == 9) {
        .assertIsValidVariedParameterVectorForPlotting(designPlan, type)

        if (is.na(main)) {
            if (survivalDesignPlanEnabled) {
                main <- PlotSubTitleItems(title = "Expected Number of Events")
            } else {
                main <- PlotSubTitleItems(title = "Expected Sample Size")
            }
            .addPlotSubTitleItems(designPlan, designMaster, main, type)
        }

        if (survivalDesignPlanEnabled) {
            xParameterName <- "hazardRatio"
            yParameterNames <- "expectedNumberOfEvents"
            expectedNumberOfEvents <- designPlan[["expectedNumberOfEvents"]]
            if (is.null(expectedNumberOfEvents) || length(expectedNumberOfEvents) == 0) {
                yParameterNames <- c("expectedEventsH0", "expectedEventsH1")
                if (is.na(legendPosition)) {
                    legendPosition <- C_POSITION_RIGHT_CENTER
                }
            }
        } else {
            xParameterName <- "effect"
            yParameterNames <- "expectedNumberOfSubjects"
        }
        srcCmd <- .showPlotSourceInformation(
            objectName = designPlanName,
            xParameterName = xParameterName,
            yParameterNames = yParameterNames,
            hint = showSourceHint, nMax = nMax,
            type = type, showSource = showSource
        )
    } else if (survivalDesignPlanEnabled) {
        if (type == 10) { # Study Duration
            .assertIsValidVariedParameterVectorForPlotting(designPlan, type)
            if (is.na(main)) {
                main <- PlotSubTitleItems(title = "Study Duration")
                .addPlotSubTitleItems(designPlan, designMaster, main, type)
            }
            xParameterName <- "hazardRatio"
            yParameterNames <- "studyDuration"
            srcCmd <- .showPlotSourceInformation(
                objectName = designPlanName,
                xParameterName = xParameterName,
                yParameterNames = yParameterNames,
                hint = showSourceHint, nMax = nMax,
                type = type, showSource = showSource
            )
        } else if (type == 11) {
            .assertIsValidVariedParameterVectorForPlotting(designPlan, type)
            if (is.na(main)) {
                main <- PlotSubTitleItems(title = "Expected Number of Subjects")
                .addPlotSubTitleItems(designPlan, designMaster, main, type)
            }
            xParameterName <- "hazardRatio"
            yParameterNames <- "expectedNumberOfSubjects"
            srcCmd <- .showPlotSourceInformation(
                objectName = designPlanName,
                xParameterName = xParameterName,
                yParameterNames = yParameterNames,
                hint = showSourceHint, nMax = nMax,
                type = type, showSource = showSource
            )
        } else if (type == 12) { # Analysis Time
            .assertIsValidVariedParameterVectorForPlotting(designPlan, type)
            if (is.na(main)) {
                main <- PlotSubTitleItems(title = "Analysis Time")
                .addPlotSubTitleItems(designPlan, designMaster, main, type)
            }

            xParameterName <- "hazardRatio"
            yParameterNames <- "analysisTime"
            yParameterNamesSrc <- c()
            for (i in 1:nrow(designPlan[["analysisTime"]])) {
                yParameterNamesSrc <- c(yParameterNamesSrc, paste0("analysisTime[", i, ", ]"))
            }

            data <- NULL
            for (k in 1:designMaster$kMax) {
                part <- data.frame(
                    categories = rep(k, length(designPlan$hazardRatio)),
                    xValues = designPlan$hazardRatio,
                    yValues = designPlan$analysisTime[k, ]
                )
                if (is.null(data)) {
                    data <- part
                } else {
                    data <- rbind(data, part)
                }
            }

            srcCmd <- .showPlotSourceInformation(
                objectName = designPlanName,
                xParameterName = xParameterName,
                yParameterNames = yParameterNamesSrc,
                hint = showSourceHint,
                type = type, showSource = showSource
            )
            if (!is.null(srcCmd)) {
                if (.isSpecialPlotShowSourceArgument(showSource)) {
                    return(invisible(srcCmd))
                }
                return(srcCmd)
            }

            return(.plotDataFrame(data,
                mainTitle = main,
                xlab = NA_character_, ylab = NA_character_, xAxisLabel = "Hazard Ratio",
                yAxisLabel1 = "Analysis Time", yAxisLabel2 = NA_character_,
                plotPointsEnabled = TRUE, legendTitle = "Stage",
                legendPosition = legendPosition, sided = designMaster$sided,
                plotSettings = plotSettings, ...
            ))
        } else if (type == 13 || type == 14) { # Cumulative Distribution Function / Survival function
            return(.plotSurvivalFunction(designPlan,
                designMaster = designMaster, type = type, main = main,
                xlab = xlab, ylab = ylab, palette = palette,
                legendPosition = legendPosition, showSource = showSource,
                designPlanName = designPlanName,
                plotSettings = plotSettings, ...
            ))
        } else {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, ") is not allowed; must be 1, 2, ..., 14")
        }
    } else {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, ") is not allowed; must be 1, 2, ..., 9")
    }

    if (!is.null(srcCmd)) {
        if (.isSpecialPlotShowSourceArgument(showSource)) {
            return(invisible(srcCmd))
        }
        return(srcCmd)
    }

    p <- .plotParameterSet(
        parameterSet = designPlan, designMaster = designMaster,
        xParameterName = xParameterName,
        yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
        palette = palette, theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
        legendPosition = legendPosition, variedParameters = variedParameters,
        qnormAlphaLineEnabled = (type != 2), ratioEnabled = ratioEnabled,
        plotSettings = plotSettings # , ...
    )

    if (type == 1 && survivalDesignPlanEnabled) {
        p <- .addDecistionCriticalValuesToPlot(p = p, designMaster = designMaster, type = type, nMax = nMax)
    }
    return(p)
}

.getSurvivalFunctionPlotCommand <- function(functionType = c("pwExpDist", "lambdaStep"), timeValues, lambda,
        designPlan, type, piecewiseSurvivalEnabled, multiplyByHazardRatio = FALSE) {
    functionType <- match.arg(functionType)
    signPrefix <- ifelse(type == 13, "", "-")
    if (functionType == "pwExpDist") {
        functionName <- "getPiecewiseExponentialDistribution"
    } else {
        functionName <- "getLambdaStepFunction"
    }
    cmd <- paste0(
        signPrefix, functionName,
        "(", .reconstructSequenceCommand(timeValues),
        ", piecewiseLambda = ", .arrayToString(lambda, vectorLookAndFeelEnabled = TRUE)
    )
    if (piecewiseSurvivalEnabled) {
        cmd <- paste0(
            cmd, ", piecewiseSurvivalTime = ",
            .arrayToString(designPlan$piecewiseSurvivalTime, vectorLookAndFeelEnabled = TRUE)
        )
    }
    if (functionType == "pwExpDist") {
        cmd <- paste0(cmd, ", kappa = ", designPlan$kappa)
    }
    cmd <- paste0(cmd, ")")
    if (multiplyByHazardRatio) {
        cmd <- paste0(cmd, " * ", designPlan$hazardRatio[1])
    }
    return(cmd)
}

# Cumulative Distribution Function / Survival function
.plotSurvivalFunction <- function(designPlan, ..., designMaster, type = c(13, 14), main = NA_character_,
        xlab = NA_character_, ylab = NA_character_, palette = "Set1",
        legendPosition = NA_integer_, showSource = FALSE,
        designPlanName = NA_character_, plotSettings = NULL) {
    startTime <- Sys.time()
    if (is.null(designPlan$piecewiseSurvivalTime) ||
            length(designPlan$piecewiseSurvivalTime) == 0) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'piecewiseSurvivalTime' must be specified")
    }

    type <- type[1]
    if (!(type %in% c(13, 14))) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' must be 13 or 14")
    }

    lambda1 <- designPlan[["lambda1"]]
    lambda2 <- designPlan[["lambda2"]]
    if (is.null(lambda2) || length(lambda2) == 0) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'lambda2' must be specified")
    }

    if (is.null(designPlan$kappa) || length(designPlan$kappa) == 0) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'kappa' must be specified")
    }

    if (is.null(designPlan$hazardRatio) || length(designPlan$hazardRatio) == 0) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'hazardRatio' must be specified")
    }

    piecewiseSurvivalEnabled <- designPlan$.piecewiseSurvivalTime$piecewiseSurvivalEnabled

    if (is.na(main)) {
        if (type == 13) {
            main <- PlotSubTitleItems(title = "Cumulative Distribution Function")
        } else {
            main <- PlotSubTitleItems(title = "Survival Function")
        }
        .addPlotSubTitleItems(designPlan, designMaster, main, type)
        if (!piecewiseSurvivalEnabled) {
            if (designPlan$.piecewiseSurvivalTime$.isLambdaBased(minNumberOfLambdas = 1)) {
                main$add("lambda", round(designPlan$lambda1[1], 4), 1)
                main$add("lambda", round(designPlan$lambda2, 4), 2)
            } else {
                main$add("pi", round(designPlan$pi1[1], 3), 1)
                main$add("pi", round(designPlan$pi2, 3), 2)
            }
        } else if (length(designPlan$hazardRatio) == 1) {
            main$add("Hazard Ratio", round(designPlan$hazardRatio[1], 3))
        }
    }

    if (!piecewiseSurvivalEnabled || (length(designPlan$piecewiseSurvivalTime) == 1 &&
            designPlan$piecewiseSurvivalTime[1] == 0)) {
        timeTo <- max(designPlan$analysisTime[designMaster$kMax, ])
    } else {
        timeTo <- max(designPlan$piecewiseSurvivalTime)
    }
    if (is.na(timeTo) || !is.numeric(timeTo) || is.infinite(timeTo)) {
        # warning("Unable to determine upper bound of time values", call. = FALSE)
        timeTo <- 0
    }

    timeTo <- timeTo + 10
    by <- timeTo / 1000
    timeValues <- seq(from = 0, to = timeTo, by = by)

    data <- data.frame(
        time = timeValues,
        lambdaGroup1 = rep(-1, length(timeValues)),
        lambdaGroup2 = rep(-1, length(timeValues)),
        survival1 = rep(-1, length(timeValues)),
        survival2 = rep(-1, length(timeValues)),
        survivalGroup1 = rep(-1, length(timeValues)),
        survivalGroup2 = rep(-1, length(timeValues))
    )

    signPrefix <- ifelse(type == 13, "", "-")
    if (piecewiseSurvivalEnabled) {
        data$survival2 <- .getPiecewiseExponentialDistribution(
            timeValues,
            lambda2, designPlan$piecewiseSurvivalTime, designPlan$kappa
        )

        yParameterNames <- .getSurvivalFunctionPlotCommand(
            "pwExpDist",
            timeValues, lambda2, designPlan, type, piecewiseSurvivalEnabled
        )

        if (!is.null(lambda1) && !all(is.na(lambda1)) &&
                length(lambda1) == length(lambda2)) {
            data$survival1 <- .getPiecewiseExponentialDistribution(
                timeValues,
                lambda1, designPlan$piecewiseSurvivalTime, designPlan$kappa
            )
            yParameterNames <- c(
                yParameterNames,
                .getSurvivalFunctionPlotCommand(
                    "pwExpDist",
                    timeValues, lambda1, designPlan, type, piecewiseSurvivalEnabled
                )
            )
        } else {
            .warnInCaseOfUnusedValuesForPlottingSurvival(designPlan$hazardRatio)
            data$survival1 <- data$survival2 * designPlan$hazardRatio[1]
            yParameterNames <- c(
                yParameterNames,
                .getSurvivalFunctionPlotCommand("pwExpDist", timeValues, lambda2,
                    designPlan, type, piecewiseSurvivalEnabled,
                    multiplyByHazardRatio = TRUE
                )
            )
        }

        yParameterNames <- c(
            yParameterNames,
            .getSurvivalFunctionPlotCommand(
                "lambdaStep",
                timeValues, lambda2, designPlan, type, piecewiseSurvivalEnabled
            )
        )
        if (!is.null(lambda1) && !all(is.na(lambda1)) &&
                length(lambda1) == length(lambda2)) {
            yParameterNames <- c(
                yParameterNames,
                .getSurvivalFunctionPlotCommand(
                    "lambdaStep",
                    timeValues, lambda1, designPlan, type, piecewiseSurvivalEnabled
                )
            )
        } else {
            yParameterNames <- c(
                yParameterNames,
                .getSurvivalFunctionPlotCommand("lambdaStep", timeValues, lambda2,
                    designPlan, type, piecewiseSurvivalEnabled,
                    multiplyByHazardRatio = TRUE
                )
            )
        }
    } else {
        if (designPlan$.piecewiseSurvivalTime$.isLambdaBased(minNumberOfLambdas = 1)) {
            if (length(designPlan$lambda1) > 1) {
                lambda1 <- designPlan$lambda1[1]
                warning("Only the first 'lambda1' (", round(lambda1, 4),
                    ") was used for plotting",
                    call. = FALSE
                )
            }
        } else {
            .warnInCaseOfUnusedValuesForPlottingRates(designPlan$pi1)
        }

        if (!is.na(designPlan$pi1[1]) && !is.na(designPlan$pi2) && !is.na(designPlan$eventTime)) {
            lambda2 <- (-log(1 - designPlan$pi2))^(1 / designPlan$kappa) / designPlan$eventTime
            lambda1 <- (-log(1 - designPlan$pi1[1]))^(1 / designPlan$kappa) / designPlan$eventTime
        }

        data$survival2 <- .getPiecewiseExponentialDistribution(
            timeValues,
            lambda2, 0, designPlan$kappa
        )
        data$survival1 <- .getPiecewiseExponentialDistribution(
            timeValues,
            lambda1, 0, designPlan$kappa
        )

        yParameterNames <- .getSurvivalFunctionPlotCommand(
            "pwExpDist",
            timeValues, lambda2, designPlan, type, piecewiseSurvivalEnabled
        )
        yParameterNames <- c(
            yParameterNames,
            .getSurvivalFunctionPlotCommand(
                "pwExpDist",
                timeValues, lambda1, designPlan, type, piecewiseSurvivalEnabled
            )
        )
        yParameterNames <- c(
            yParameterNames,
            .getSurvivalFunctionPlotCommand(
                "lambdaStep",
                timeValues, lambda2, designPlan, type, piecewiseSurvivalEnabled
            )
        )
        yParameterNames <- c(
            yParameterNames,
            .getSurvivalFunctionPlotCommand(
                "lambdaStep", timeValues, lambda1,
                designPlan, type, piecewiseSurvivalEnabled
            )
        )
    }

    # two groups: 1 = treatment, 2 = control
    if (type == 14) {
        data$survival1 <- 1 - data$survival1
        data$survival2 <- 1 - data$survival2
    }

    if (piecewiseSurvivalEnabled) {
        data$lambdaGroup2 <- .getLambdaStepFunction(
            timeValues,
            designPlan$piecewiseSurvivalTime, lambda2
        )
        if (length(lambda1) == 1) {
            if (!is.na(lambda1)) {
                data$lambdaGroup1 <- rep(lambda1, length(data$lambdaGroup2))
            } else {
                data$lambdaGroup1 <- data$lambdaGroup2 * designPlan$hazardRatio[1]
            }
        } else {
            data$lambdaGroup1 <- .getLambdaStepFunction(
                timeValues,
                designPlan$piecewiseSurvivalTime, lambda1
            )
        }
    } else {
        data$lambdaGroup2 <- .getLambdaStepFunction(timeValues, 0, lambda2)
        data$lambdaGroup1 <- .getLambdaStepFunction(timeValues, 0, lambda1)
    }

    scalingBaseValues1 <- na.omit(c(data$survival1, data$survival2))
    scalingBaseValues2 <- na.omit(c(data$lambdaGroup1, data$lambdaGroup2))
    scalingFactor <- 1
    if (length(scalingBaseValues1) > 0 && length(scalingBaseValues2) > 0) {
        scalingFactor <- max(scalingBaseValues1) / max(.getNextHigherValue(scalingBaseValues2))
    }
    data2 <- data.frame(
        categories = c(
            rep("Treatm. piecew. exp.", nrow(data)),
            rep("Control piecew. exp.", nrow(data)),
            rep("Treatm. piecew. lambda", nrow(data)),
            rep("Control piecew. lambda", nrow(data))
        ),
        xValues = rep(data$time, 4),
        yValues = c(
            data$survival1,
            data$survival2,
            data$lambdaGroup1 * scalingFactor,
            data$lambdaGroup2 * scalingFactor
        )
    )

    if (is.na(legendPosition)) {
        if (type == 13) {
            legendPosition <- C_POSITION_LEFT_TOP
        } else {
            legendPosition <- C_POSITION_RIGHT_TOP
        }
    }

    if (is.na(palette) || palette == "Set1") {
        palette <- "Paired"
    }

    if (type == 13) {
        yAxisLabel1 <- "Cumulative Distribution Function"
    } else {
        yAxisLabel1 <- "Survival Function"
    }

    srcCmd <- .showPlotSourceInformation(
        objectName = designPlanName,
        xParameterName = "time",
        yParameterNames = yParameterNames,
        showSource = showSource,
        xValues = timeValues
    )
    if (!is.null(srcCmd)) {
        if (.isSpecialPlotShowSourceArgument(showSource)) {
            return(invisible(srcCmd))
        }
        return(srcCmd)
    }

    if (is.null(plotSettings)) {
        plotSettings <- designPlan$.plotSettings
    }

    return(.plotDataFrame(data2,
        mainTitle = main,
        xlab = xlab, ylab = ylab, xAxisLabel = "Time",
        yAxisLabel1 = yAxisLabel1, yAxisLabel2 = "Lambda",
        plotPointsEnabled = FALSE, legendTitle = NA_character_,
        legendPosition = legendPosition, scalingFactor1 = 1,
        scalingFactor2 = scalingFactor, palette = palette, sided = designMaster$sided,
        plotSettings = plotSettings
    ))
}

.warnInCaseOfUnusedValuesForPlottingMeans <- function(alternative) {
    if (length(alternative) > 1) {
        warning("Only the first 'alternative' (", round(alternative[1], 3),
            ") was used for plotting",
            call. = FALSE
        )
        return(list(title = "alternative", value = alternative[1], subscript = NA_character_))
    }
    return(NULL)
}

.warnInCaseOfUnusedValuesForPlottingRates <- function(pi1) {
    if (length(pi1) > 1) {
        warning("Only the first 'pi1' (", round(pi1[1], 3),
            ") was used for plotting",
            call. = FALSE
        )
        return(list(title = "pi", value = pi1[1], subscript = "1"))
    }
    return(NULL)
}

.warnInCaseOfUnusedValuesForPlottingSurvival <- function(hazardRatio) {
    if (length(hazardRatio) > 1) {
        warning("Only the first 'hazardRatio' (", round(hazardRatio[1], 3),
            ") was used for plotting",
            call. = FALSE
        )
        return(list(title = "hazardRatio", value = hazardRatio[1], subscript = NA_character_))
    }
    return(NULL)
}

.warnInCaseOfUnusedValuesForPlotting <- function(designPlan) {
    if (.isTrialDesignPlanMeans(designPlan) && designPlan$.isSampleSizeObject()) {
        return(.warnInCaseOfUnusedValuesForPlottingMeans(designPlan$alternative))
    }
    if (.isTrialDesignPlanRates(designPlan) && designPlan$.isSampleSizeObject()) {
        return(.warnInCaseOfUnusedValuesForPlottingRates(designPlan$pi1))
    }
    if (.isTrialDesignPlanSurvival(designPlan) && designPlan$.isSampleSizeObject()) {
        return(.warnInCaseOfUnusedValuesForPlottingSurvival(designPlan$hazardRatio))
    }
    return(NULL)
}

#'
#' @title
#' Trial Design Plan Plotting
#'
#' @param x The trial design plan, obtained from \cr
#'        \code{\link[=getSampleSizeMeans]{getSampleSizeMeans()}}, \cr
#'        \code{\link[=getSampleSizeRates]{getSampleSizeRates()}}, \cr
#'        \code{\link[=getSampleSizeSurvival]{getSampleSizeSurvival()}}, \cr
#'        \code{\link[=getPowerMeans]{getPowerMeans()}}, \cr
#'        \code{\link[=getPowerRates]{getPowerRates()}} or \cr
#'        \code{\link[=getPowerSurvival]{getPowerSurvival()}}.
#' @param y Not available for this kind of plot (is only defined to be compatible to the generic plot function).
#' @param main The main title.
#' @param xlab The x-axis label.
#' @param ylab The y-axis label.
#' @inheritParams param_palette
#' @inheritParams param_theta
#' @inheritParams param_plotPointsEnabled
#' @inheritParams param_showSource
#' @inheritParams param_plotSettings
#' @inheritParams param_legendPosition
#' @inheritParams param_grid
#' @param type The plot type (default = \code{1}). The following plot types are available:
#' \itemize{
#'   \item \code{1}: creates a 'Boundaries' plot
#'   \item \code{2}: creates a 'Boundaries Effect Scale' plot
#'   \item \code{3}: creates a 'Boundaries p Values Scale' plot
#'   \item \code{4}: creates a 'Error Spending' plot
#'   \item \code{5}: creates a 'Sample Size' or 'Overall Power and Early Stopping' plot
#'   \item \code{6}: creates a 'Number of Events' or 'Sample Size' plot
#'   \item \code{7}: creates an 'Overall Power' plot
#'   \item \code{8}: creates an 'Overall Early Stopping' plot
#'   \item \code{9}: creates an 'Expected Number of Events' or 'Expected Sample Size' plot
#'   \item \code{10}: creates a 'Study Duration' plot
#'   \item \code{11}: creates an 'Expected Number of Subjects' plot
#'   \item \code{12}: creates an 'Analysis Times' plot
#'   \item \code{13}: creates a 'Cumulative Distribution Function' plot
#'   \item \code{14}: creates a 'Survival Function' plot
#'   \item \code{"all"}: creates all available plots and returns it as a grid plot or list
#' }
#' @inheritParams param_three_dots_plot
#'
#' @description
#' Plots a trial design plan.
#'
#' @details
#' Generic function to plot all kinds of trial design plans.
#'
#' @examples
#' \dontrun{
#' if (require(ggplot2)) plot(getSampleSizeMeans())
#' }
#'
#' @template return_object_ggplot
#'
#' @export
#'
plot.TrialDesignPlan <- function(x, y, ..., main = NA_character_,
        xlab = NA_character_, ylab = NA_character_,
        type = ifelse(x$.design$kMax == 1, 5L, 1L), palette = "Set1",
        theta = seq(-1, 1, 0.01), plotPointsEnabled = NA,
        legendPosition = NA_integer_, showSource = FALSE,
        grid = 1, plotSettings = NULL) {
    fCall <- match.call(expand.dots = FALSE)
    designPlanName <- deparse(fCall$x)
    .assertGgplotIsInstalled()
    .assertIsSingleInteger(grid, "grid", validateType = FALSE)

    nMax <- list(...)[["nMax"]]
    if (!is.null(nMax)) {
        warning(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'nMax' (", nMax,
            ") will be ignored because it will be taken from design plan"
        )
    }

    typeNumbers <- .getPlotTypeNumber(type, x)
    if (is.null(plotSettings)) {
        plotSettings <- .getGridPlotSettings(x, typeNumbers, grid)
    }
    p <- NULL
    plotList <- list()
    for (typeNumber in typeNumbers) {
        p <- .plotTrialDesignPlan(
            designPlan = x,
            main = main, xlab = xlab, ylab = ylab, type = typeNumber,
            palette = palette, theta = theta, plotPointsEnabled = plotPointsEnabled,
            legendPosition = .getGridLegendPosition(legendPosition, typeNumbers, grid),
            showSource = showSource, designPlanName = designPlanName,
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

    if (length(plotList) == 0) {
        message("No plots available for the specified design plan for ", x$.toString())
    }

    if (.isSpecialPlotShowSourceArgument(showSource)) {
        return(invisible(plotList))
    }

    return(.createPlotResultObject(plotList, grid))
}
