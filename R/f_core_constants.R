## |
## |  *Constants*
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

#' @include class_dictionary.R
#' @include f_core_utilities.R
NULL

C_LOG_LEVEL_TRACE <- "TRACE"
C_LOG_LEVEL_DEBUG <- "DEBUG"
C_LOG_LEVEL_INFO <- "INFO"
C_LOG_LEVEL_WARN <- "WARN"
C_LOG_LEVEL_ERROR <- "ERROR"
C_LOG_LEVEL_PROGRESS <- "PROGRESS"
C_LOG_LEVEL_DISABLED <- "DISABLED"

C_SUMMARY_OUTPUT_SIZE_DEFAULT <- "large"
C_SUMMARY_LIST_ITEM_PREFIX_DEFAULT <- "  "

# used in 'class_core_plot_settings.R'
C_POSITION_OUTSIDE_PLOT <- 0
C_POSITION_LEFT_TOP <- 1
C_POSITION_LEFT_CENTER <- 2
C_POSITION_LEFT_BOTTOM <- 3
C_POSITION_RIGHT_TOP <- 4
C_POSITION_RIGHT_CENTER <- 5
C_POSITION_RIGHT_BOTTOM <- 6
C_POSITION_OUTSIDE_BOTTOM <- "bottom"

C_DESIGN_TOLERANCE_DEFAULT <- 1e-08
C_CONST_NEWTON_COTES <- 15
C_TWO_SIDED_POWER_DEFAULT <- FALSE
C_BINDING_FUTILITY_DEFAULT <- FALSE
C_BINDING_FUTILITY_FISHER_DEFAULT <- TRUE
C_CONST_BOUND_HP_DEFAULT <- 3
C_ALPHA_DEFAULT <- 0.025
C_BETA_DEFAULT <- 0.2
C_SIDED_DEFAULT <- 1L
C_KMAX_DEFAULT <- 3L
C_KMAX_UPPER_BOUND <- 50L
C_KMAX_UPPER_BOUND_FISHER <- 6L

C_MAX_NUMBER_OF_EFFECTS <- 50L

C_NA_MAX_DEFAULT <- 100L
C_POWER_ASN_THETA_DEFAULT <- seq(-1, 1, 0.02)

C_ANALYSIS_TOLERANCE_DEFAULT <- 1e-06
C_ANALYSIS_TOLERANCE_FISHER_DEFAULT <- 1e-14

C_UPPER_BOUNDS_DEFAULT <- 8
C_FUTILITY_BOUNDS_DEFAULT <- -6
C_FUTILITY_BOUNDS_MIN_VALUE <- C_FUTILITY_BOUNDS_DEFAULT
C_FUTILITY_BOUNDS_MAX_VALUE <- abs(C_FUTILITY_BOUNDS_DEFAULT)
C_ALPHA_0_VEC_DEFAULT <- 1
C_THETA_H0_MEANS_DEFAULT <- 0
C_THETA_H0_RATES_DEFAULT <- 0
C_THETA_H0_SURVIVAL_DEFAULT <- 1
C_THETA_H0_COUNTS_DEFAULT <- 1
C_ALLOCATION_RATIO_DEFAULT <- 1
C_ALLOCATION_RATIO_MAXIMUM <- 100
C_DIRECTION_UPPER_DEFAULT <- TRUE
C_NORMAL_APPROXIMATION_MEANS_DEFAULT <- FALSE
C_NORMAL_APPROXIMATION_RATES_DEFAULT <- TRUE
C_EQUAL_VARIANCES_DEFAULT <- TRUE
C_ITERATIONS_DEFAULT <- 1000L
C_ACCEPT_DEVIATION_INFORMATIONRATES <- 0.05

C_THETA_RANGE_SEQUENCE_LENGTH_DEFAULT <- 50
C_VARIED_PARAMETER_SEQUENCE_LENGTH_DEFAULT <- 30

C_HEADING_BASE_NUMBER_DEFAULT <- -2L

C_CLASS_NAME_TRIAL_DESIGN_FIXED <- "TrialDesignFixed"
C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL <- "TrialDesignGroupSequential"
C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL <- "TrialDesignInverseNormal"
C_CLASS_NAME_TRIAL_DESIGN_FISHER <- "TrialDesignFisher"
C_CLASS_NAME_TRIAL_DESIGN_CONDITIONAL_DUNNETT <- "TrialDesignConditionalDunnett"

.getTrialDesignClassNames <- function(inclusiveConditionalDunnett = TRUE) {
    trialDesignClassNames <- c(
        C_CLASS_NAME_TRIAL_DESIGN_FIXED,
        C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL,
        C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL,
        C_CLASS_NAME_TRIAL_DESIGN_FISHER
    )
    if (inclusiveConditionalDunnett) {
        trialDesignClassNames <- c(trialDesignClassNames, C_CLASS_NAME_TRIAL_DESIGN_CONDITIONAL_DUNNETT)
    }
    return(trialDesignClassNames)
}

C_MARKDOWN_PLOT_PRINT_SEPARATOR <- "\n\n-----\n\n"

C_EXCEPTION_TYPE_RUNTIME_ISSUE <- "Runtime exception: "
C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT <- "Illegal argument: "
C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT <- "Illegal data input: "
C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS <- "Conflicting arguments: "
C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS <- "Argument out of bounds: "
C_EXCEPTION_TYPE_ARGUMENT_LENGTH_OUT_OF_BOUNDS <- "Argument length out of bounds: "
C_EXCEPTION_TYPE_INDEX_OUT_OF_BOUNDS <- "Index out of bounds: "
C_EXCEPTION_TYPE_MISSING_ARGUMENT <- "Missing argument: "
C_EXCEPTION_TYPE_INCOMPLETE_ARGUMENTS <- "Incomplete associated arguments: "

C_DIRECTION_LOWER <- "lower"
C_DIRECTION_UPPER <- "upper"

C_QNORM_EPSILON <- 1e-100 # a value between 1e-323 and 1e-16
C_QNORM_MAXIMUM <- -stats::qnorm(C_QNORM_EPSILON)
C_QNORM_MINIMUM <- -C_QNORM_MAXIMUM
C_QNORM_THRESHOLD <- floor(C_QNORM_MAXIMUM)

#
# Constants used in 'f_analysis_multiarm' and 'f_analysis_enrichment'
#
C_INTERSECTION_TEST_MULTIARMED_DEFAULT <- "Dunnett"
C_INTERSECTION_TEST_ENRICHMENT_DEFAULT <- "Simes"
C_INTERSECTION_TESTS_MULTIARMED <- c(
    "Bonferroni",
    "Simes",
    "Sidak",
    "Dunnett",
    "Hierarchical"
)
C_INTERSECTION_TESTS_ENRICHMENT <- c(
    "Bonferroni",
    "Simes",
    "Sidak",
    "SpiessensDebois"
)
C_RATES_STD_ERROR_ESTIMATE <- c("pooled", "unpooled")
C_RATES_STD_ERROR_ESTIMATE_DEFAULT <- "pooled"
C_VARIANCE_OPTION_DUNNETT <- "overallPooled"
C_VARIANCE_OPTIONS_MULTIARMED <- c("overallPooled", "pairwisePooled", "notPooled")
C_VARIANCE_OPTION_MULTIARMED_DEFAULT <- "overallPooled"
C_VARIANCE_OPTIONS_ENRICHMENT <- c("pooled", "notPooled", "pooledFromFull")
C_VARIANCE_OPTION_ENRICHMENT_DEFAULT <- "pooled"
C_STRATIFIED_ANALYSIS_DEFAULT <- TRUE

#
# Constants used in 'parameters.R'
#
C_PARAM_USER_DEFINED <- "u"
C_PARAM_DEFAULT_VALUE <- "d"
C_PARAM_GENERATED <- "g"
C_PARAM_DERIVED <- ">"
C_PARAM_NOT_APPLICABLE <- "."
C_PARAM_TYPE_UNKNOWN <- "?"

#
# Constants used in 'f_simulation_survival.R'
#
C_PI_2_DEFAULT <- 0.2
C_PI_1_DEFAULT <- seq(0.2, 0.5, 0.1)
C_PI_1_SAMPLE_SIZE_DEFAULT <- c(0.4, 0.5, 0.6)
C_DROP_OUT_RATE_1_DEFAULT <- 0
C_DROP_OUT_RATE_2_DEFAULT <- 0
C_DROP_OUT_TIME_DEFAULT <- 12
C_EVENT_TIME_DEFAULT <- 12
C_ALLOCATION_1_DEFAULT <- 1
C_ALLOCATION_2_DEFAULT <- 1
C_MAX_ITERATIONS_DEFAULT <- 10L
C_MAX_SIMULATION_ITERATIONS_DEFAULT <- 1000L
C_ACCRUAL_TIME_DEFAULT <- c(0L, 12L)
C_ACCRUAL_INTENSITY_DEFAULT <- 0.1
C_FOLLOW_UP_TIME_DEFAULT <- 6

#
# Constants used in 'f_simulation_multiarm[...].R'
#

C_ACTIVE_ARMS_DEFAULT <- 3L
C_POPULATIONS_DEFAULT <- 3L
C_TYPES_OF_SELECTION <- c("best", "rBest", "epsilon", "all", "userDefined")
C_TYPE_OF_SELECTION_DEFAULT <- C_TYPES_OF_SELECTION[1]
C_TYPES_OF_SHAPE <- c("linear", "sigmoidEmax", "userDefined")
C_TYPE_OF_SHAPE_DEFAULT <- C_TYPES_OF_SHAPE[1]

C_SUCCESS_CRITERIONS <- c("all", "atLeastOne")
C_SUCCESS_CRITERION_DEFAULT <- C_SUCCESS_CRITERIONS[1]
C_EFFECT_MEASURES <- c("effectEstimate", "testStatistic")
C_EFFECT_MEASURE_DEFAULT <- C_EFFECT_MEASURES[1]

#
# Additional constants used in 'f_design_sample_size_calculator.R'
#

C_ALTERNATIVE_DEFAULT <- seq(0.2, 1, 0.2)
C_ALTERNATIVE_POWER_SIMULATION_DEFAULT <- seq(0, 1, 0.2)
C_ALTERNATIVE_POWER_SIMULATION_MEAN_RATIO_DEFAULT <- seq(1, 2, 0.2)
C_RANGE_OF_HAZARD_RATIOS_DEFAULT <- seq(1, 2.6, 0.4)
C_STDEV_DEFAULT <- 1

#
# Constants used in 'core_group_sequential_design.R'
#
# Type of design is one of the following:
# O'Brien & Fleming,
# Pocock,
# Wang & Tsiatis Delta class,
# Haybittle & Peto,
# Optimum design within Wang & Tsiatis class,
# Pocock type alpha spending,
# O'Brien & Fleming type alpha spending,
# Kim & DeMets alpha spending,
# Hwang, Shi & DeCani alpha spending,
# user defined alpha spending
#
C_TYPE_OF_DESIGN_OF <- "OF" # O'Brien & Fleming
C_TYPE_OF_DESIGN_P <- "P" # Pocock,
C_TYPE_OF_DESIGN_WT <- "WT" # Wang & Tsiatis Delta class
C_TYPE_OF_DESIGN_PT <- "PT" # Pampallona & Tsiatis class
C_TYPE_OF_DESIGN_HP <- "HP" # Haybittle & Peto
C_TYPE_OF_DESIGN_WT_OPTIMUM <- "WToptimum" # Optimum design within Wang & Tsiatis class
C_TYPE_OF_DESIGN_AS_P <- "asP" # Pocock type alpha spending
C_TYPE_OF_DESIGN_AS_OF <- "asOF" # O'Brien & Fleming type alpha spending
C_TYPE_OF_DESIGN_AS_KD <- "asKD" # Kim & DeMets alpha spending
C_TYPE_OF_DESIGN_AS_HSD <- "asHSD" # Hwang, Shi & DeCani alpha spending
C_TYPE_OF_DESIGN_AS_USER <- "asUser" # user defined alpha spending
C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY <- "noEarlyEfficacy" # no early efficacy stop
C_DEFAULT_TYPE_OF_DESIGN <- C_TYPE_OF_DESIGN_OF # the default type of design

C_TYPE_OF_DESIGN_LIST <- createDictionary("C_TYPE_OF_DESIGN_LIST", list(
    "OF" = "O'Brien & Fleming",
    "P" = "Pocock",
    "WT" = "Wang & Tsiatis Delta class",
    "PT" = "Pampallona & Tsiatis class",
    "HP" = "Haybittle & Peto",
    "WToptimum" = "Optimum design within Wang & Tsiatis class",
    "asP" = "Pocock type alpha spending",
    "asOF" = "O'Brien & Fleming type alpha spending",
    "asKD" = "Kim & DeMets alpha spending",
    "asHSD" = "Hwang, Shi & DeCani alpha spending",
    "asUser" = "User defined alpha spending",
    "noEarlyEfficacy" = "No early efficacy stop"
))

C_TYPE_OF_FISHER_LIST <- createDictionary("C_TYPE_OF_FISHER_LIST", list(
    "equalAlpha" = "Constant levels",
    "fullAlpha" = "Full last stage level",
    "noInteraction" = "Levels with no Interaction",
    "userDefinedAlpha" = "User defined levels"
))

C_PLOT_SHOW_SOURCE_ARGUMENTS <- c("commands", "axes", "test", "validate")

C_PLOT_MAIN_CONDITIONAL_POWER_WITH_LIKELIHOOD <- "Conditional Power with Likelihood"
C_PLOT_YLAB_CONDITIONAL_POWER_WITH_LIKELIHOOD <- "Conditional power / Likelihood"

.getDesignTypes <- function() {
    return(c(
        C_TYPE_OF_DESIGN_OF,
        C_TYPE_OF_DESIGN_P,
        C_TYPE_OF_DESIGN_WT,
        C_TYPE_OF_DESIGN_PT,
        C_TYPE_OF_DESIGN_HP,
        C_TYPE_OF_DESIGN_WT_OPTIMUM,
        C_TYPE_OF_DESIGN_AS_P,
        C_TYPE_OF_DESIGN_AS_OF,
        C_TYPE_OF_DESIGN_AS_KD,
        C_TYPE_OF_DESIGN_AS_HSD,
        C_TYPE_OF_DESIGN_AS_USER,
        C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY
    ))
}

.printDesignTypes <- function() {
    .arrayToString(.getDesignTypes(), encapsulate = TRUE)
}

.isAlphaSpendingDesignType <- function(typeOfDesign, userDefinedAlphaSpendingIncluded = TRUE) {
    if (userDefinedAlphaSpendingIncluded &&
            ((typeOfDesign == C_TYPE_OF_DESIGN_AS_USER) || (typeOfDesign == C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY))) {
        return(TRUE)
    }

    return(typeOfDesign %in% c(
        C_TYPE_OF_DESIGN_AS_P, C_TYPE_OF_DESIGN_AS_OF,
        C_TYPE_OF_DESIGN_AS_KD, C_TYPE_OF_DESIGN_AS_HSD
    ))
}

# Type of beta spending design is one of the following:
# Pocock type beta spending,
# O'Brien & Fleming type beta spending,
# Kim & DeMets beta spending,
# Hwang, Shi & DeCani beta spending,
# user defined beta spending
# "none", "bsP", "bsOF", "bsKD", "bsHSD", "bsUser"
C_TYPE_OF_DESIGN_BS_NONE <- "none"
C_TYPE_OF_DESIGN_BS_P <- "bsP" # Pocock type beta spending
C_TYPE_OF_DESIGN_BS_OF <- "bsOF" # O'Brien & Fleming type beta spending
C_TYPE_OF_DESIGN_BS_KD <- "bsKD" # Kim & DeMets beta spending
C_TYPE_OF_DESIGN_BS_HSD <- "bsHSD" # Hwang, Shi & DeCani beta spending
C_TYPE_OF_DESIGN_BS_USER <- "bsUser" # user defined beta spending

C_TYPE_OF_DESIGN_BS_LIST <- createDictionary("C_TYPE_OF_DESIGN_BS_LIST", list(
    "none" = "none",
    "bsP" = "Pocock type beta spending",
    "bsOF" = "O'Brien & Fleming type beta spending",
    "bsKD" = "Kim & DeMets beta spending",
    "bsHSD" = "Hwang, Shi & DeCani beta spending",
    "bsUser" = "user defined beta spending"
))

C_CIPHERS <- list(token = "310818669631424001", secret = "9318655074497250732")

.getBetaSpendingDesignTypes <- function() {
    return(c(
        C_TYPE_OF_DESIGN_BS_NONE,
        C_TYPE_OF_DESIGN_BS_P,
        C_TYPE_OF_DESIGN_BS_OF,
        C_TYPE_OF_DESIGN_BS_KD,
        C_TYPE_OF_DESIGN_BS_HSD,
        C_TYPE_OF_DESIGN_BS_USER
    ))
}

.printBetaSpendingDesignTypes <- function() {
    .arrayToString(.getBetaSpendingDesignTypes(), encapsulate = TRUE)
}

.isBetaSpendingDesignType <- function(
        typeOfDesign,
        userDefinedBetaSpendingIncluded = TRUE,
        noneIncluded = FALSE) {
    if (userDefinedBetaSpendingIncluded && typeOfDesign == C_TYPE_OF_DESIGN_BS_USER) {
        return(TRUE)
    }

    if (noneIncluded && typeOfDesign == C_TYPE_OF_DESIGN_BS_NONE) {
        return(TRUE)
    }

    return(typeOfDesign %in% c(
        C_TYPE_OF_DESIGN_BS_P,
        C_TYPE_OF_DESIGN_BS_OF,
        C_TYPE_OF_DESIGN_BS_KD,
        C_TYPE_OF_DESIGN_BS_HSD
    ))
}

C_OPTIMIZATION_CRITERION_ASNH1 <- "ASNH1"
C_OPTIMIZATION_CRITERION_ASNIFH1 <- "ASNIFH1"
C_OPTIMIZATION_CRITERION_ASN_SUM <- "ASNsum"
C_OPTIMIZATION_CRITERION_DEFAULT <- C_OPTIMIZATION_CRITERION_ASNH1

.getOptimizationCriterions <- function() {
    return(c(
        C_OPTIMIZATION_CRITERION_ASNH1,
        C_OPTIMIZATION_CRITERION_ASNIFH1,
        C_OPTIMIZATION_CRITERION_ASN_SUM
    ))
}

.printOptimizationCriterion <- function() {
    .arrayToString(.getOptimizationCriterions(), encapsulate = TRUE)
}

.isOptimizationCriterion <- function(x) {
    return(x %in% .getOptimizationCriterions())
}

C_FISHER_METHOD_FULL_ALPHA <- "fullAlpha"
C_FISHER_METHOD_EQUAL_ALPHA <- "equalAlpha"
C_FISHER_METHOD_NO_INTERACTION <- "noInteraction"
C_FISHER_METHOD_USER_DEFINED_ALPHA <- "userDefinedAlpha"
C_FISHER_METHOD_DEFAULT <- C_FISHER_METHOD_EQUAL_ALPHA

