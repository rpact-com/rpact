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
## |  File version: $Revision: 7126 $
## |  Last changed: $Date: 2023-06-23 14:26:39 +0200 (Fr, 23 Jun 2023) $
## |  Last changed by: $Author: pahlke $
## |

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
C_KMAX_UPPER_BOUND <- 20L
C_KMAX_UPPER_BOUND_FISHER <- 6L

C_NA_MAX_DEFAULT <- 100L
C_POWER_ASN_THETA_DEFAULT <- seq(-1, 1, 0.02)

C_ANALYSIS_TOLERANCE_DEFAULT <- 1e-06
C_ANALYSIS_TOLERANCE_FISHER_DEFAULT <- 1e-14

C_UPPER_BOUNDS_DEFAULT <- 8
C_FUTILITY_BOUNDS_DEFAULT <- -6
C_ALPHA_0_VEC_DEFAULT <- 1
C_THETA_H0_MEANS_DEFAULT <- 0
C_THETA_H0_RATES_DEFAULT <- 0
C_THETA_H0_SURVIVAL_DEFAULT <- 1
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

C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL <- "TrialDesignGroupSequential"
C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL <- "TrialDesignInverseNormal"
C_CLASS_NAME_TRIAL_DESIGN_FISHER <- "TrialDesignFisher"
C_CLASS_NAME_TRIAL_DESIGN_CONDITIONAL_DUNNETT <- "TrialDesignConditionalDunnett"

.getTrialDesignClassNames <- function(inclusiveConditionalDunnett = TRUE) {
    trialDesignClassNames <- c(
        C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL,
        C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL,
        C_CLASS_NAME_TRIAL_DESIGN_FISHER
    )
    if (inclusiveConditionalDunnett) {
        trialDesignClassNames <- c(trialDesignClassNames, C_CLASS_NAME_TRIAL_DESIGN_CONDITIONAL_DUNNETT)
    }
    return(trialDesignClassNames)
}

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
C_VARIANCE_OPTION_DUNNETT <- "overallPooled"
C_VARIANCE_OPTION_MULTIARMED_DEFAULT <- "overallPooled"
C_VARIANCE_OPTIONS_MULTIARMED <- c("overallPooled", "pairwisePooled", "notPooled")
C_VARIANCE_OPTION_ENRICHMENT_DEFAULT <- "pooled"
C_VARIANCE_OPTIONS_ENRICHMENT <- c("pooled", "notPooled", "pooledFromFull")
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
C_ACCRUAL_TIME_DEFAULT <- c(0, 12)
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

C_TYPE_OF_DESIGN_LIST <- list(
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
)

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

#
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

C_TYPE_OF_DESIGN_BS_LIST <- list(
    "none" = "none",
    "bsP" = "Pocock type beta spending",
    "bsOF" = "O'Brien & Fleming type beta spending",
    "bsKD" = "Kim & DeMets beta spending",
    "bsHSD" = "Hwang, Shi & DeCani beta spending",
    "bsUser" = "user defined beta spending"
)

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

.isBetaSpendingDesignType <- function(typeOfDesign,
        userDefinedBetaSpendingIncluded = TRUE, noneIncluded = FALSE) {
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

##
## -------------------------------------------
##

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

##
## -------------------------------------------
##

C_FISHER_METHOD_FULL_ALPHA <- "fullAlpha"
C_FISHER_METHOD_EQUAL_ALPHA <- "equalAlpha"
C_FISHER_METHOD_NO_INTERACTION <- "noInteraction"
C_FISHER_METHOD_USER_DEFINED_ALPHA <- "userDefinedAlpha"
C_FISHER_METHOD_DEFAULT <- C_FISHER_METHOD_EQUAL_ALPHA

.getFisherMethods <- function() {
    return(c(
        C_FISHER_METHOD_FULL_ALPHA,
        C_FISHER_METHOD_EQUAL_ALPHA,
        C_FISHER_METHOD_NO_INTERACTION,
        C_FISHER_METHOD_USER_DEFINED_ALPHA
    ))
}

.printFisherMethods <- function() {
    .arrayToString(.getFisherMethods(), encapsulate = TRUE)
}

.isFisherMethod <- function(method) {
    return(method %in% .getFisherMethods())
}

##
## -------------------------------------------
##

C_PARAMETER_NAMES <- list(
    iterations = "Iterations",
    seed = "Seed",
    groups = "Treatment groups",
    stages = "Stages",
    sampleSizes = "Sample sizes",
    means = "Means",
    stDevs = "Standard deviations",
    overallEvents = "Cumulative events",
    overallAllocationRatios = "Cumulative allocation ratios",
    expectedEvents = "Expected events",
    varianceEvents = "Variance of events",
    overallExpectedEvents = "Cumulative expected events",
    overallVarianceEvents = "Cumulative variance of events",
    bindingFutility = "Binding futility",
    constantBoundsHP = "Haybittle Peto constants",
    betaAdjustment = "Beta adjustment",
    kMax = "Maximum number of stages",
    alpha = "Significance level",
    finalStage = "Final stage",
    informationRates = "Information rates",
    criticalValues = "Critical values",
    criticalValuesDelayedInformation = "Upper bounds of continuation",
    stageLevels = "Stage levels (one-sided)",
    alphaSpent = "Cumulative alpha spending",
    tolerance = "Tolerance",
    method = "Method",
    alpha0Vec = "Alpha_0",
    scale = "Scale",
    nonStochasticCurtailment = "Non stochastic curtailment",
    simAlpha = "Simulated alpha",
    beta = "Type II error rate",
    betaSpent = "Cumulative beta spending",
    sided = "Test",
    futilityBounds = "Futility bounds (binding)",
    futilityBoundsNonBinding = "Futility bounds (non-binding)",
    futilityBoundsDelayedInformation = "Lower bounds of continuation (binding)",
    futilityBoundsDelayedInformationNonBinding = "Lower bounds of continuation (non-binding)",
    typeOfDesign = "Type of design",
    deltaWT = "Delta for Wang & Tsiatis Delta class",
    deltaPT0 = "Delta0 for Pampallona & Tsiatis class",
    deltaPT1 = "Delta1 for Pampallona & Tsiatis class",
    optimizationCriterion = "Optimization criterion for optimum design within Wang & Tsiatis class",
    gammaA = "Parameter for alpha spending function",
    gammaB = "Parameter for beta spending function",
    typeBetaSpending = "Type of beta spending",
    userAlphaSpending = "User defined alpha spending",
    userBetaSpending = "User defined beta spending",
    probs = "Exit probabilities",
    power = "Power",
    theta = "Effect",
    direction = "Direction",
    normalApproximation = "Normal approximation",
    equalVariances = "Equal variances",
    shift = "Shift",
    inflationFactor = "Inflation factor",
    information = "Informations",
    rejectionProbabilities = "Rejection probabilities under H1",
    futilityProbabilities = "Futility probabilities under H1",
    averageSampleNumber1 = "Ratio expected vs fixed sample size under H1",
    averageSampleNumber01 = "Ratio expected vs fixed sample size under a value between H0 and H1",
    averageSampleNumber0 = "Ratio expected vs fixed sample size under H0",
    allocationRatioPlanned = "Planned allocation ratio",
    thetaH0 = "Theta H0",
    thetaH1 = "Assumed effect under alternative",
    stDevH1 = "Assumed standard deviation under alternative",
    assumedStDev = "Assumed standard deviation",
    assumedStDevs = "Assumed standard deviations",
    pi1 = "Assumed treatment rate",
    pi2 = "Assumed control rate",
    overallPi1 = "Cumulative treatment rate",
    overallPi2 = "Cumulative control rate",
    pi1H1 = "pi(1) under H1",
    pi2H1 = "pi(2) under H1",
    nPlanned = "Planned sample size",
    piControl = "Assumed control rate",
    piControls = "Assumed control rates",
    piTreatment = "Assumed treatment rate",
    piTreatments = "Assumed treatment rates",
    piTreatmentH1 = "pi(treatment) under H1",
    piTreatmentsH1 = "pi(treatment) under H1",
    overallPiControl = "Cumulative control rate",
    overallPiTreatments = "Cumulative treatment rate",
    overallPisControl = "Cumulative control rate",
    overallPisTreatment = "Cumulative treatment rate",
    effectSizes = "Cumulative effect sizes",
    testStatistics = "Stage-wise test statistics",
    pValues = "Stage-wise p-values",
    testActions = "Actions",
    conditionalPower = "Conditional power",
    conditionalPowerAchieved = "Conditional power (achieved)",
    conditionalPowerSimulated = "Conditional power (simulated)",
    conditionalRejectionProbabilities = "Conditional rejection probability",
    repeatedConfidenceIntervalLowerBounds = "Repeated confidence intervals (lower)",
    repeatedConfidenceIntervalUpperBounds = "Repeated confidence intervals (upper)",
    repeatedPValues = "Repeated p-values",
    finalPValues = "Final p-value",
    finalConfidenceIntervalLowerBounds = "Final CIs (lower)",
    finalConfidenceIntervalUpperBounds = "Final CIs (upper)",
    medianUnbiasedEstimates = "Median unbiased estimate",
    overallSampleSizes = "Cumulative sample sizes",
    overallSampleSizes1 = "Cumulative sample sizes (1)",
    overallSampleSizes2 = "Cumulative sample sizes (2)",
    overallTestStatistics = "Overall test statistics",
    overallPValues = "Overall p-values",
    overallMeans = "Cumulative means",
    overallMeans1 = "Cumulative means (1)",
    overallMeans2 = "Cumulative means (2)",
    overallStDevs1 = "Cumulative standard deviations (1)",
    overallStDevs2 = "Cumulative standard deviations (2)",
    overallStDevs = "Cumulative (pooled) standard deviations",
    testStatistics = "Stage-wise test statistics",
    combInverseNormal = "Combination test statistics", # Inverse normal combination
    combFisher = "Combination test statistics", # Fisher combination
    weightsFisher = "Fixed weights",
    weightsInverseNormal = "Fixed weights",
    overallLogRanks = "Cumulative log-ranks",
    overallEvents = "Cumulative number of events",
    overallEvents1 = "Cumulative number of events (1)",
    overallEvents2 = "Cumulative number of events (2)",
    overallAllocationRatios = "Cumulative allocation ratios",
    events = "Number of events",
    allocationRatios = "Allocation ratios",
    logRanks = "Log-ranks",
    nMax = "N_max",
    averageSampleNumber = "Average sample sizes (ASN)",
    calculatedPower = "Power",
    earlyStop = "Early stop",
    rejectPerStage = "Reject per stage",
    futilityPerStage = "Futility stop per stage",
    overallEarlyStop = "Early stop",
    overallReject = "Overall reject",
    overallFutility = "Overall futility",
    riskRatio = "Risk ratio",
    meanRatio = "Mean ratio",
    alternative = "Alternatives",
    stDev = "Standard deviation",
    nFixed = "Number of subjects fixed",
    nFixed1 = "Number of subjects fixed (1)",
    nFixed2 = "Number of subjects fixed (2)",
    maxNumberOfSubjects = "Maximum number of subjects",
    maxNumberOfSubjects1 = "Maximum number of subjects (1)",
    maxNumberOfSubjects2 = "Maximum number of subjects (2)",
    numberOfSubjects = "Number of subjects",
    numberOfSubjects1 = "Number of subjects (1)",
    numberOfSubjects2 = "Number of subjects (2)",
    expectedNumberOfSubjectsH0 = "Expected number of subjects under H0",
    expectedNumberOfSubjectsH01 = "Expected number of subjects under H0/H1",
    expectedNumberOfSubjectsH1 = "Expected number of subjects under H1",
    expectedNumberOfSubjects = "Expected number of subjects",
    chi = "Probability of an event",
    hazardRatio = "Hazard ratio",
    hazardRatios = "Hazard ratios",
    typeOfComputation = "Type of computation",
    accountForObservationTimes = "Account for observation times",
    eventTime = "Event time",
    accrualTime = "Accrual time",
    totalAccrualTime = "Total accrual time",
    remainingTime = "Remaining time",
    followUpTime = "Follow up time",
    dropoutRate1 = "Drop-out rate (1)",
    dropoutRate2 = "Drop-out rate (2)",
    dropoutTime = "Drop-out time",
    eventsFixed = "Number of events fixed",
    expectedEventsH0 = "Expected number of events under H0",
    expectedEventsH01 = "Expected number of events under H0/H1",
    expectedEventsH1 = "Expected number of events under H1",
    analysisTime = "Analysis times",
    studyDurationH1 = "Expected study duration under H1",
    expectedNumberOfSubjectsH1 = "Expected number of subjects under H1",
    twoSidedPower = "Two-sided power",
    plannedEvents = "Planned cumulative events",
    plannedSubjects = "Planned cumulative subjects", # per arm (multi-arm); overall (base)
    minNumberOfEventsPerStage = "Minimum number of events per stage",
    maxNumberOfEventsPerStage = "Maximum number of events per stage",
    minNumberOfSubjectsPerStage = "Minimum number of subjects per stage",
    maxNumberOfSubjectsPerStage = "Maximum number of subjects per stage",
    accrualIntensity = "Accrual intensity",
    accrualIntensityRelative = "Accrual intensity (relative)",
    maxNumberOfIterations = "Maximum number of iterations",
    allocation1 = "Allocation 1",
    allocation2 = "Allocation 2",
    expectedNumberOfEvents = "Expected number of events",
    expectedNumberOfEventsPerStage = "Expected number of events by stage",
    eventsNotAchieved = "Events not achieved",
    subjects = "Subjects",
    overallReject = "Overall reject",
    futilityStop = "Overall futility stop",
    studyDuration = "Expected study duration",
    maxStudyDuration = "Maximal study duration",
    directionUpper = "Direction upper",
    piecewiseSurvivalTime = "Piecewise survival times",
    lambda1 = "lambda(1)",
    lambda2 = "lambda(2)",
    kappa = "kappa",
    earlyStopPerStage = "Early stop per stage",
    effect = "Effect",
    maxNumberOfEvents = "Maximum number of events",
    criticalValuesEffectScale = "Critical values (treatment effect scale)",
    criticalValuesEffectScaleDelayedInformation = "Upper bounds of continuation (treatment effect scale)",
    criticalValuesEffectScaleLower = "Lower critical values (treatment effect scale)",
    criticalValuesEffectScaleUpper = "Upper critical values (treatment effect scale)",
    criticalValuesPValueScale = "Local one-sided significance levels",
    ".design$stageLevels" = "Local one-sided significance levels",
    futilityBoundsEffectScale = "Futility bounds (treatment effect scale)",
    futilityBoundsEffectScaleDelayedInformation = "Lower bounds of continuation (treatment effect scale)",
    futilityBoundsEffectScaleLower = "Lower futility bounds (treatment effect scale)",
    futilityBoundsEffectScaleUpper = "Upper futility bounds (treatment effect scale)",
    futilityBoundsPValueScale = "Futility bounds (one-sided p-value scale)",
    futilityBoundsPValueScaleDelayedInformation = "Lower bounds of continuation (one-sided p-value scale)",
    analysisTime = "Analysis time",
    eventsPerStage1 = "Observed # events by stage (1)",
    eventsPerStage2 = "Observed # events by stage (2)",
    testStatistic = "Test statistic",
    logRankStatistic = "Log-rank statistic",
    hazardRatioEstimateLR = "Hazard ratio estimate LR",
    delayedResponseAllowed = "Delayed response allowed",
    delayedResponseEnabled = "Delayed response enabled",
    piecewiseSurvivalEnabled = "Piecewise exponential survival enabled",
    median1 = "median(1)",
    median2 = "median(2)",
    eventsPerStage = "Number of events per stage",
    overallEventsPerStage = "Cumulative number of events",
    expectedNumberOfEvents = "Observed number of events",
    expectedNumberOfSubjects = "Observed number of subjects",
    singleNumberOfEventsPerStage = "Single number of events",
    endOfAccrualIsUserDefined = "End of accrual is user defined",
    followUpTimeMustBeUserDefined = "Follow-up time must be user defined",
    maxNumberOfSubjectsIsUserDefined = "Max number of subjects is user defined",
    maxNumberOfSubjectsCanBeCalculatedDirectly = "Max number of subjects can be calculated directly",
    absoluteAccrualIntensityEnabled = "Absolute accrual intensity is enabled",
    time = "Time",
    cumulativeEventProbabilities = "Cumulative event probabilities",
    eventProbabilities1 = "Event probabilities (1)",
    eventProbabilities2 = "Event probabilities (2)",
    informationAtInterim = "Information at interim",
    secondStageConditioning = "Conditional second stage p-values",
    separatePValues = "Separate p-values",
    singleStepAdjustedPValues = "Single step adjusted p-values",
    intersectionTest = "Intersection test",
    varianceOption = "Variance option",
    overallPooledStDevs = "Cumulative (pooled) standard deviations",
    optimumAllocationRatio = "Optimum allocation ratio",
    rejected = "Rejected",
    indices = "Indices of hypothesis",
    adjustedStageWisePValues = "Adjusted stage-wise p-values",
    overallAdjustedTestStatistics = "Overall adjusted test statistics",
    rejectedIntersections = "Rejected intersections",
    conditionalErrorRate = "Conditional error rate",
    secondStagePValues = "Second stage p-values",
    effectMatrix = "Effect matrix",
    typeOfShape = "Type of shape",
    gED50 = "ED50",
    slope = "Slope",
    adaptations = "Adaptations",
    typeOfSelection = "Type of selection",
    effectMeasure = "Effect measure",
    successCriterion = "Success criterion",
    epsilonValue = "Epsilon value",
    rValue = "r value",
    threshold = "Threshold",
    rejectAtLeastOne = "Reject at least one",
    selectedArms = "Selected arms",
    rejectedArmsPerStage = "Rejected arms per stage",
    selectedPopulations = "Selected populations",
    rejectedPopulationsPerStage = "Rejected populations per stage",
    successPerStage = "Success per stage",
    effectEstimate = "Effect estimate",
    subjectsControlArm = "Subjects (control arm)",
    subjectsActiveArm = "Subjects (active arm)",
    pValue = "p-value",
    conditionalCriticalValue = "Conditional critical value",
    piControlH1 = "pi(control) under H1",
    piMaxVector = "pi_max",
    omegaMaxVector = "omega_max",
    muMaxVector = "mu_max",
    activeArms = "Active arms",
    populations = "Populations",
    numberOfEvents = "Number of events",
    calcSubjectsFunction = "Calculate subjects function",
    calcEventsFunction = "Calculate events function",
    selectArmsFunction = "Select arms function",
    numberOfActiveArms = "Number of active arms",
    selectPopulationsFunction = "Select populations function",
    numberOfPopulations = "Number of populations",
    correlationComputation = "Correlation computation method",
    subsets = "Subsets",
    subset = "Subset",
    stratifiedAnalysis = "Stratified analysis",
    maxInformation = "Maximum information",
    informationEpsilon = "Information epsilon",
    effectList = "Effect list",
    subGroups = "Sub-groups",
    prevalences = "Prevalences",
    effects = "Effects",
    situation = "Situation",
    delayedInformation = "Delayed information",
    decisionCriticalValues = "Decision critical values",
    reversalProbabilities = "Reversal probabilities",
    locationSampleSize = "Location sample sizes",
    variationSampleSize = "Variation sample sizes",
    subscoreSampleSize = "Sub-score sample sizes",
    locationConditionalPower = "Location conditional power",
    variationConditionalPower = "Variation conditional power",
    subscoreConditionalPower = "Sub-score conditional power",
    performanceScore = "Performance scores"
)

C_TABLE_COLUMN_NAMES <- list(
    iterations = "Iterations",
    seed = "Seed",
    groups = "Treatment group",
    stages = "Stage",
    sampleSizes = "Sample size",
    means = "Mean",
    stDevs = "Standard deviation",
    overallEvents = "Cumulative event",
    overallAllocationRatios = "Cumulative allocation ratio",
    overallMeans = "Cumulative mean",
    expectedEvents = "Expected event",
    varianceEvents = "Variance of event",
    overallExpectedEvents = "Cumulative expected event",
    overallVarianceEvents = "Cumulative variance of event",
    bindingFutility = "Binding futility",
    constantBoundsHP = "Haybittle Peto constant",
    betaAdjustment = "Beta adjustment",
    kMax = "Maximum # stages",
    alpha = "Significance level",
    finalStage = "Final stage",
    informationRates = "Information rate",
    criticalValues = "Critical value",
    criticalValuesDelayedInformation = "Upper bounds of continuation",
    stageLevels = "Stage level",
    alphaSpent = "Cumulative alpha spending",
    tolerance = "Tolerance",
    method = "Method",
    alpha0Vec = "Alpha_0",
    scale = "Scale",
    nonStochasticCurtailment = "Non stochastic curtailment",
    simAlpha = "Simulated alpha",
    beta = "Type II error rate",
    betaSpent = "Cumulative beta spending",
    sided = "Test",
    futilityBounds = "Futility bound (binding)",
    futilityBoundsNonBinding = "Futility bound (non-binding)",
    futilityBoundsDelayedInformation = "Lower bounds of continuation (binding)",
    futilityBoundsDelayedInformationNonBinding = "Lower bounds of continuation (non-binding)",
    typeOfDesign = "Type of design",
    deltaWT = "Delta (Wang & Tsiatis)",
    deltaPT0 = "Delta0 (Pampallona & Tsiatis)",
    deltaPT1 = "Delta1 (Pampallona & Tsiatis)",
    optimizationCriterion = "Optimization criterion (Wang & Tsiatis)",
    gammaA = "Parameter for alpha spending function",
    gammaB = "Parameter for beta spending function",
    typeBetaSpending = "Type of beta spending",
    userAlphaSpending = "User defined alpha spending",
    userBetaSpending = "User defined beta spending",
    probs = "Internal calculation probabilities",
    power = "Power",
    theta = "Effect",
    direction = "Direction",
    normalApproximation = "Normal approximation",
    equalVariances = "Equal variance",
    assumedStDev = "Assumed standard deviation",
    assumedStDevs = "Assumed standard deviation",
    stDevH1 = "Assumed standard deviation under H1",
    shift = "Shift",
    inflationFactor = "Inflation factor",
    information = "Information",
    rejectionProbabilities = "Rejection probability under H1",
    futilityProbabilities = "Futility probability under H1",
    averageSampleNumber1 = "Ratio expected vs fixed sample size under H1",
    averageSampleNumber01 = "Ratio expected vs fixed sample size under a value between H0 and H1",
    averageSampleNumber0 = "Ratio expected vs fixed sample size under H0",
    allocationRatioPlanned = "Planned allocation ratio",
    thetaH0 = "Theta H0", # Effect
    thetaH1 = "Assumed effect",
    pi1 = "pi(1)",
    pi2 = "pi(2)",
    pi1H1 = "pi(1) under H1",
    pi2H1 = "pi(2) under H1",
    nPlanned = "Planned sample size",
    piControl = "Assumed control rate",
    piControls = "Assumed control rates",
    piTreatment = "Assumed treatment rate",
    piTreatments = "Assumed treatment rates",
    piTreatmentH1 = "pi(treatment) under H1",
    piTreatmentsH1 = "pi(treatment) under H1",
    overallPiControl = "Cumulative control rate",
    overallPiTreatments = "Cumulative treatment rate",
    overallPisControl = "Cumulative control rate",
    overallPisTreatment = "Cumulative treatment rate",
    stages = "Stage",
    effectSizes = "Overall effect size",
    testStatistics = "Stage-wise test statistic",
    pValues = "p-value",
    testActions = "Action",
    conditionalPower = "Conditional power",
    conditionalPowerAchieved = "Conditional power (achieved)",
    conditionalPowerSimulated = "Conditional power (simulated)",
    conditionalRejectionProbabilities = "Conditional rejection probabilities",
    repeatedConfidenceIntervalLowerBounds = "Repeated confidence interval (lower)",
    repeatedConfidenceIntervalUpperBounds = "Repeated confidence interval (upper)",
    repeatedPValues = "Repeated p-value",
    finalPValues = "Final p-value",
    finalConfidenceIntervalLowerBounds = "Final CI (lower)",
    finalConfidenceIntervalUpperBounds = "Final CI (upper)",
    medianUnbiasedEstimates = "Median unbiased estimate",
    overallSampleSizes = "Cumulative sample size",
    overallSampleSizes1 = "Cumulative sample size (1)",
    overallSampleSizes2 = "Cumulative sample size (2)",
    overallTestStatistics = "Overall test statistic",
    overallPValues = "Overall p-value",
    overallMeans1 = "Cumulative mean (1)",
    overallMeans2 = "Cumulative mean (2)",
    overallStDevs1 = "Cumulative standard deviation (1)",
    overallStDevs2 = "Cumulative standard deviation (2)",
    overallStDevs = "Cumulative (pooled) standard deviation",
    testStatistics = "Test statistic",
    combInverseNormal = "Inverse Normal Combination",
    combFisher = "Fisher Combination",
    weightsFisher = "Fixed weight",
    weightsInverseNormal = "Fixed weight",
    overallLogRanks = "Cumulative log-rank",
    overallEvents = "Cumulative # events",
    overallEvents1 = "Cumulative # events (1)",
    overallEvents2 = "Cumulative # events (2)",
    overallAllocationRatios = "Cumulative allocation ratio",
    events = "# events",
    allocationRatios = "Allocation ratio",
    logRanks = "Log-rank",
    nMax = "N_max",
    averageSampleNumber = "Average sample size (ASN)",
    calculatedPower = "Power",
    earlyStop = "Early stop",
    rejectPerStage = "Reject per stage",
    futilityPerStage = "Futility stop per stage",
    overallEarlyStop = "Early stop",
    overallReject = "Overall reject",
    overallFutility = "Overall futility",
    riskRatio = "Risk ratio",
    meanRatio = "Mean ratio",
    alternative = "Alternative",
    stDev = "Standard deviation",
    nFixed = "# subjects fixed",
    nFixed1 = "# subjects fixed (1)",
    nFixed2 = "# subjects fixed (2)",
    maxNumberOfSubjects = "Max # subjects",
    maxNumberOfSubjects1 = "Max # subjects (1)",
    maxNumberOfSubjects2 = "Max # subjects (2)",
    numberOfSubjects = "# subjects",
    numberOfSubjects1 = "# subjects (1)",
    numberOfSubjects2 = "# subjects (2)",
    expectedNumberOfSubjectsH0 = "Expected # subjects under H0",
    expectedNumberOfSubjectsH01 = "Expected # subjects under H0/H1",
    expectedNumberOfSubjectsH1 = "Expected # subjects under H1",
    expectedNumberOfSubjects = "Expected # subjects",
    chi = "Probability of an event",
    hazardRatio = "Hazard ratio",
    hazardRatios = "Hazard ratios",
    typeOfComputation = "Type of computation",
    accountForObservationTimes = "Account for observation times",
    eventTime = "Event time",
    accrualTime = "Accrual time",
    totalAccrualTime = "Total accrual time",
    remainingTime = "Remaining time",
    followUpTime = "Follow up time",
    dropoutRate1 = "Drop-out rate (1)",
    dropoutRate2 = "Drop-out rate (2)",
    dropoutTime = "Drop-out time",
    eventsFixed = "# events fixed",
    expectedEventsH0 = "Expected # events under H0",
    expectedEventsH01 = "Expected # events under H0/H1",
    expectedEventsH1 = "Expected # events under H1",
    analysisTime = "Analysis time",
    eventsPerStage1 = "Observed # events by stage (1)",
    eventsPerStage2 = "Observed # events by stage (2)",
    studyDurationH1 = "Expected study duration H1",
    expectedNumberOfSubjectsH1 = "Expected # subjects H1",
    twoSidedPower = "Two-sided power",
    plannedEvents = "Planned cumulative events",
    plannedSubjects = "Planned cumulative subjects",
    minNumberOfEventsPerStage = "Minimum # events per stage",
    maxNumberOfEventsPerStage = "Maximum # events per stage",
    minNumberOfSubjectsPerStage = "Minimum # of subjects per stage",
    maxNumberOfSubjectsPerStage = "Maximum # of subjects per stage",
    accrualIntensity = "Accrual intensity",
    accrualIntensityRelative = "Accrual intensity (relative)",
    maxNumberOfIterations = "Maximum # iterations",
    allocation1 = "Allocation 1",
    allocation2 = "Allocation 2",
    expectedNumberOfEvents = "Expected # events",
    expectedNumberOfEventsPerStage = "Expected # events by stage",
    eventsNotAchieved = "Events not achieved",
    subjects = "Subjects",
    futilityStop = "Overall futility stop",
    studyDuration = "Expected study duration",
    maxStudyDuration = "Maximal study duration",
    directionUpper = "Direction upper",
    piecewiseSurvivalTime = "Piecewise survival times",
    lambda1 = "lambda(1)",
    lambda2 = "lambda(2)",
    kappa = "kappa",
    earlyStopPerStage = "Early stop per stage",
    effect = "Effect",
    maxNumberOfEvents = "Maximum # events",
    criticalValuesEffectScale = "Critical value (treatment effect scale)",
    criticalValuesEffectScaleDelayedInformation = "Upper bound of continuation (treatment effect scale)",
    criticalValuesEffectScaleLower = "Lower critical value (treatment effect scale)",
    criticalValuesEffectScaleUpper = "Upper critical value (treatment effect scale)",
    criticalValuesPValueScale = "Local one-sided significance level",
    ".design$stageLevels" = "Local one-sided significance level",
    futilityBoundsEffectScale = "Futility bound (treatment effect scale)",
    futilityBoundsEffectScaleDelayedInformation = "Lower bounds of continuation (treatment effect scale)",
    futilityBoundsEffectScaleLower = "Lower futility bound (treatment effect scale)",
    futilityBoundsEffectScaleUpper = "Upper futility bound (treatment effect scale)",
    futilityBoundsPValueScale = "Futility bound (one-sided p-value scale)",
    futilityBoundsPValueScaleDelayedInformation = "Lower bound of continuation (one-sided p-value scale)",
    delayedResponseAllowed = "Delayed response allowed",
    delayedResponseEnabled = "Delayed response enabled",
    piecewiseSurvivalEnabled = "Piecewise exponential survival enabled",
    median1 = "median(1)",
    median2 = "median(2)",
    eventsPerStage = "Cumulative # events",
    eventsPerStage = "# events per stage",
    overallEventsPerStage = "Cumulative # events",
    expectedNumberOfEvents = "Observed # events",
    expectedNumberOfSubjects = "Observed # subjects",
    singleNumberOfEventsPerStage = "Single # events",
    endOfAccrualIsUserDefined = "End of accrual is user defined",
    followUpTimeMustBeUserDefined = "Follow-up time must be user defined",
    maxNumberOfSubjectsIsUserDefined = "Max number of subjects is user defined",
    maxNumberOfSubjectsCanBeCalculatedDirectly = "Max number of subjects can be calculated directly",
    absoluteAccrualIntensityEnabled = "Absolute accrual intensity is enabled",
    time = "Time",
    cumulativeEventProbabilities = "Cumulative event probability",
    eventProbabilities1 = "Event probability (1)",
    eventProbabilities2 = "Event probability (2)",
    informationAtInterim = "Information at interim",
    secondStageConditioning = "Conditional second stage p-value",
    separatePValues = "Separate p-value",
    singleStepAdjustedPValues = "Single step adjusted p-value",
    intersectionTest = "Intersection test",
    varianceOption = "Variance option",
    overallPooledStDevs = "Cumulative (pooled) standard deviation",
    optimumAllocationRatio = "Optimum allocation ratio",
    rejected = "Rejected",
    indices = "Indices of hypothesis",
    adjustedStageWisePValues = "Adjusted stage-wise p-value",
    overallAdjustedTestStatistics = "Overall adjusted test statistics",
    rejectedIntersections = "Rejected intersection",
    conditionalErrorRate = "Conditional error rate",
    secondStagePValues = "Second stage p-value",
    effectMatrix = "Effect matrix",
    typeOfShape = "Type of shape",
    gED50 = "ED50",
    slope = "Slope",
    adaptations = "Adaptations",
    typeOfSelection = "Type of selection",
    effectMeasure = "Effect measure",
    successCriterion = "Success criterion",
    epsilonValue = "Epsilon value",
    rValue = "r value",
    threshold = "Threshold",
    rejectAtLeastOne = "Reject at least one",
    selectedArms = "Selected arm",
    rejectedArmsPerStage = "Rejected arm per stage",
    successPerStage = "Success per stage",
    effectEstimate = "Effect estimate",
    subjectsControlArm = "Subjects (control arm)",
    subjectsActiveArm = "Subjects (active arm)",
    pValue = "p-value",
    conditionalCriticalValue = "Conditional critical value",
    piControlH1 = "pi(control) under H1",
    piMaxVector = "pi_max",
    omegaMaxVector = "omega_max",
    muMaxVector = "mu_max",
    activeArms = "Active arm",
    populations = "Population",
    numberOfEvents = "Number of events",
    calcSubjectsFunction = "Calc subjects fun",
    calcEventsFunction = "Calc events fun",
    selectArmsFunction = "Select arms fun",
    numberOfActiveArms = "Number of active arms",
    correlationComputation = "Correlation computation",
    subsets = "Subset",
    subset = "Subset",
    stratifiedAnalysis = "Stratified analysis",
    maxInformation = "Maximum information",
    informationEpsilon = "Information epsilon",
    effectList = "Effect list",
    subGroups = "Sub-group",
    prevalences = "Prevalence",
    effects = "Effect",
    situation = "Situation",
    delayedInformation = "Delayed information",
    decisionCriticalValues = "Decision critical value",
    reversalProbabilities = "Reversal probability",
    locationSampleSize = "Location sample size",
    variationSampleSize = "Variation sample size",
    subscoreSampleSize = "Sub-score sample size",
    locationConditionalPower = "Location conditional power",
    variationConditionalPower = "Variation conditional power",
    subscoreConditionalPower = "Sub-score conditional power",
    performanceScore = "Performance score"
)

.getParameterCaptions <- function(captionList, ...,
        design = NULL,
        designPlan = NULL,
        stageResults = NULL,
        analysisResults = NULL,
        dataset = NULL,
        designCharacteristics = NULL,
        tableColumns = FALSE) {
    parameterNames <- captionList

    if (!is.null(design)) {
        parameterNameFutilityBounds <- "futilityBounds"
        if (.isDelayedInformationEnabled(design = design)) {
            if (!is.na(design$bindingFutility) && !design$bindingFutility) {
                parameterNameFutilityBounds <- "futilityBoundsDelayedInformationNonBinding"
            } else {
                parameterNameFutilityBounds <- "futilityBoundsDelayedInformation"
            }
            parameterNames$criticalValues <- captionList[["criticalValuesDelayedInformation"]]

            parameterNames$criticalValuesEffectScale <- captionList[["criticalValuesEffectScaleDelayedInformation"]]
            parameterNames$futilityBoundsEffectScale <- captionList[["futilityBoundsEffectScaleDelayedInformation"]]
            parameterNames$futilityBoundsPValueScale <- captionList[["futilityBoundsPValueScaleDelayedInformation"]]
        } else if (!is.na(design$bindingFutility) && !design$bindingFutility) {
            parameterNameFutilityBounds <- "futilityBoundsNonBinding"
        }
        parameterNames$futilityBounds <- captionList[[parameterNameFutilityBounds]]
    }

    if (!is.null(designPlan) && (inherits(designPlan, "TrialDesignPlanSurvival") || inherits(designPlan, "TrialDesignPlanSurvivalR6")) &&
            !is.null(designPlan$.piecewiseSurvivalTime) &&
            designPlan$.piecewiseSurvivalTime$piecewiseSurvivalEnabled) {
        parameterNames$lambda2 <- "Piecewise survival lambda (2)"
        parameterNames$lambda1 <- "Piecewise survival lambda (1)"
    }

    if (!is.null(designPlan) &&
            (inherits(designPlan, "TrialDesignPlanSurvival") || inherits(designPlan, "TrialDesignPlanSurvivalR6")) &&
            identical(designPlan$.design$kMax, 1L)) {
        parameterNames$maxNumberOfEvents <- "Number of events"
    }

    if (!is.null(designPlan) && (inherits(designPlan, "TrialDesignPlan") || inherits(designPlan, "TrialDesignPlanR6")) &&
            identical(designPlan$.design$kMax, 1L)) {
        parameterNames$studyDuration <- "Study duration"
    }

    if (!is.null(analysisResults)) {
        pluralExt <- ifelse(tableColumns, "", "s")
        if (.isTrialDesignConditionalDunnett(analysisResults$.design)) {
            parameterNames$repeatedConfidenceIntervalLowerBounds <-
                paste0("Overall confidence interval", pluralExt, " (lower)")
            parameterNames$repeatedConfidenceIntervalUpperBounds <-
                paste0("Overall confidence interval", pluralExt, " (upper)")
            parameterNames$repeatedPValues <- paste0("Overall p-value", pluralExt)
        } else if (identical(analysisResults$.design$kMax, 1L)) {
            parameterNames$repeatedConfidenceIntervalLowerBounds <- paste0("Confidence interval", pluralExt, " (lower)")
            parameterNames$repeatedConfidenceIntervalUpperBounds <- paste0("Confidence interval", pluralExt, " (upper)")
            parameterNames$repeatedPValues <- paste0("Overall p-value", pluralExt)
        }
    }

    if (!is.null(designPlan) &&
            ((inherits(designPlan, "TrialDesignPlanMeans") || inherits(designPlan, "TrialDesignPlanMeansR6")) ||
                (inherits(designPlan, "SimulationResultsMeans") || inherits(designPlan, "SimulationResultsMeansR6"))) &&
            isTRUE(designPlan$meanRatio)) {
        parameterNames$stDev <- "Coefficient of variation"
    }

    if (!is.null(design) && .getClassName(design) != "TrialDesign" && design$sided == 2) {
        parameterNames$criticalValuesPValueScale <- paste0("Local two-sided significance level", ifelse(tableColumns, "", "s"))
    }

    if ((!is.null(stageResults) && stageResults$isOneSampleDataset()) ||
            (!is.null(dataset) && inherits(dataset, "DatasetMeans"))) {
        parameterNames$overallStDevs <- paste0("Cumulative standard deviation", ifelse(tableColumns, "", "s"))
    }

    return(parameterNames)
}

.getParameterNames <- function(...,
        design = NULL,
        designPlan = NULL,
        stageResults = NULL,
        analysisResults = NULL,
        dataset = NULL,
        designCharacteristics = NULL) {
    .getParameterCaptions(
        captionList = C_PARAMETER_NAMES,
        design = design,
        designPlan = designPlan,
        stageResults = stageResults,
        analysisResults = analysisResults,
        dataset = dataset,
        designCharacteristics = designCharacteristics
    )
}

.getTableColumnNames <- function(...,
        design = NULL,
        designPlan = NULL,
        stageResults = NULL,
        analysisResults = NULL,
        dataset = NULL,
        designCharacteristics = NULL) {
    .getParameterCaptions(
        captionList = C_TABLE_COLUMN_NAMES,
        design = design,
        designPlan = designPlan,
        stageResults = stageResults,
        analysisResults = analysisResults,
        dataset = dataset,
        designCharacteristics = designCharacteristics,
        tableColumns = TRUE
    )
}

C_PARAMETER_FORMAT_FUNCTIONS <- list(
    means = ".formatMeans",
    stDevs = ".formatStDevs",
    stDev = ".formatStDevs",
    thetaH0 = ".formatStDevs",
    alternative = ".formatStDevs",
    assumedStDev = ".formatStDevs",
    assumedStDevs = ".formatStDevs",
    overallAllocationRatios = ".formatRatios",
    allocationRatioPlanned = ".formatRatios",
    alpha = ".formatProbabilities",
    beta = ".formatProbabilities",
    informationRates = ".formatRates",
    stageLevels = ".formatProbabilities",
    alphaSpent = ".formatProbabilities",
    alpha0Vec = ".formatProbabilities",
    simAlpha = ".formatProbabilities",
    criticalValues = ".formatCriticalValuesFisher", # will be set in class TrialDesignFisher
    criticalValues = ".formatCriticalValues", # will be set in class TrialDesignGroupSequential
    betaSpent = ".formatProbabilities",
    futilityBounds = ".formatCriticalValues",
    alpha0Vec = ".formatProbabilities",
    constantBoundsHP = ".formatCriticalValues",
    nMax = ".formatProbabilities",
    nFixed = ".formatSampleSizes",
    nFixed1 = ".formatSampleSizes",
    nFixed2 = ".formatSampleSizes",
    shift = ".formatProbabilities",
    inflationFactor = ".formatProbabilities",
    information = ".formatRates",
    power = ".formatProbabilities",
    rejectionProbabilities = ".formatProbabilities",
    futilityProbabilities = ".formatFutilityProbabilities",
    probs = ".formatProbabilities",
    averageSampleNumber1 = ".formatProbabilities",
    averageSampleNumber01 = ".formatProbabilities",
    averageSampleNumber0 = ".formatProbabilities",
    effectSizes = ".formatMeans",
    thetaH1 = ".formatMeans",
    stDevH1 = ".formatStDevs",
    testStatistics = ".formatTestStatistics",
    pValues = ".formatPValues",
    conditionalPower = ".formatConditionalPower",
    conditionalPowerAchieved = ".formatConditionalPower",
    conditionalPowerSimulated = ".formatConditionalPower",
    conditionalRejectionProbabilities = ".formatProbabilities",
    repeatedConfidenceIntervalLowerBounds = ".formatMeans",
    repeatedConfidenceIntervalUpperBounds = ".formatMeans",
    repeatedPValues = ".formatRepeatedPValues",
    finalPValues = ".formatPValues",
    finalConfidenceIntervalLowerBounds = ".formatMeans",
    finalConfidenceIntervalUpperBounds = ".formatMeans",
    medianUnbiasedEstimates = ".formatMeans",
    overallTestStatistics = ".formatTestStatistics",
    overallPValues = ".formatPValues",
    overallMeans = ".formatMeans",
    overallMeans1 = ".formatMeans",
    overallMeans2 = ".formatMeans",
    overallStDevs1 = ".formatStDevs",
    overallStDevs2 = ".formatStDevs",
    overallStDevs = ".formatStDevs",
    overallPooledStDevs = ".formatStDevs",
    testStatistics = ".formatTestStatistics",
    combInverseNormal = ".formatTestStatistics",
    combFisher = ".formatTestStatisticsFisher",
    weightsFisher = ".formatRates",
    weightsInverseNormal = ".formatRates",
    overallLogRanks = ".formatTestStatistics",
    logRanks = ".formatTestStatistics",
    theta = ".formatMeans",
    averageSampleNumber = ".formatCriticalValues", # ".formatSampleSizes",
    calculatedPower = ".formatProbabilities",
    earlyStop = ".formatProbabilities",
    rejectPerStage = ".formatProbabilities",
    futilityPerStage = ".formatProbabilities",
    overallEarlyStop = ".formatProbabilities",
    overallReject = ".formatProbabilities",
    overallFutility = ".formatProbabilities",
    earlyStopPerStage = ".formatProbabilities",
    effect = ".formatMeans",
    maxNumberOfSubjects = ".formatSampleSizes",
    maxNumberOfSubjects1 = ".formatSampleSizes",
    maxNumberOfSubjects2 = ".formatSampleSizes",
    maxNumberOfEvents = ".formatEvents",
    numberOfSubjects = ".formatSampleSizes",
    numberOfSubjects1 = ".formatSampleSizes",
    numberOfSubjects2 = ".formatSampleSizes",
    expectedNumberOfSubjectsH0 = ".formatSampleSizes",
    expectedNumberOfSubjectsH01 = ".formatSampleSizes",
    expectedNumberOfSubjectsH1 = ".formatSampleSizes",
    expectedNumberOfSubjects = ".formatSampleSizes",
    chi = ".formatRates",
    hazardRatio = ".formatRates",
    hazardRatios = ".formatRates",
    pi1 = ".formatRates",
    pi2 = ".formatRates",
    pi1H1 = ".formatRates",
    pi2H1 = ".formatRates",
    piecewiseSurvivalTime = ".formatTime",
    lambda2 = ".formatRates",
    lambda1 = ".formatRates",
    eventTime = ".formatEventTime",
    accrualTime = ".formatTime",
    totalAccrualTime = ".formatTime",
    remainingTime = ".formatTime",
    followUpTime = ".formatTime",
    dropoutRate1 = ".formatRates",
    dropoutRate2 = ".formatRates",
    dropoutTime = ".formatTime",
    eventsFixed = ".formatEvents",
    expectedEventsH0 = ".formatEvents",
    expectedEventsH01 = ".formatEvents",
    expectedEventsH1 = ".formatEvents",
    analysisTime = ".formatTime",
    studyDurationH1 = ".formatDurations",
    expectedNumberOfSubjectsH1 = ".formatSampleSizes",
    expectedEvents = ".formatEvents",
    varianceEvents = ".formatEvents",
    overallExpectedEvents = ".formatEvents",
    overallVarianceEvents = ".formatEvents",
    events = ".formatEvents",
    overallEvents = ".formatEvents",
    expectedNumberOfEvents = ".formatEvents",
    expectedNumberOfEventsPerStage = ".formatEvents",
    eventsNotAchieved = ".formatRates",
    subjects = ".formatSampleSizes",
    futilityStop = ".formatProbabilities",
    studyDuration = ".formatDurations",
    maxStudyDuration = ".formatDurations",
    criticalValuesEffectScale = ".formatCriticalValues",
    criticalValuesEffectScaleLower = ".formatCriticalValues",
    criticalValuesEffectScaleUpper = ".formatCriticalValues",
    criticalValuesPValueScale = ".formatProbabilities",
    futilityBoundsEffectScale = ".formatCriticalValues",
    futilityBoundsPValueScale = ".formatProbabilities",
    median1 = ".formatRatesDynamic",
    median2 = ".formatRatesDynamic",
    accrualIntensity = ".formatAccrualIntensities",
    accrualIntensityRelative = ".formatAccrualIntensities",
    eventsPerStage = ".formatEvents",
    expectedNumberOfEvents = ".formatEvents",
    expectedNumberOfSubjects = ".formatEvents",
    singleNumberOfEventsPerStage = ".formatEvents",
    time = ".formatTime",
    cumulativeEventProbabilities = ".formatProbabilities",
    eventProbabilities1 = ".formatProbabilities",
    eventProbabilities2 = ".formatProbabilities",
    informationAtInterim = ".formatRates",
    separatePValues = ".formatPValues",
    singleStepAdjustedPValues = ".formatPValues",
    userAlphaSpending = ".formatHowItIs",
    userBetaSpending = ".formatHowItIs",
    piControl = ".formatRates",
    piControls = ".formatRates",
    piTreatment = ".formatRates",
    piTreatments = ".formatRates",
    piTreatmentH1 = ".formatRates",
    piTreatmentsH1 = ".formatRates",
    overallPiControl = ".formatRates",
    overallPiTreatments = ".formatRates",
    overallPisControl = ".formatRates",
    overallPisTreatment = ".formatRates",
    adjustedStageWisePValues = ".formatPValues",
    overallAdjustedTestStatistics = ".formatTestStatisticsFisher", # will be set in class ClosedCombinationTestResults
    overallAdjustedTestStatistics = ".formatTestStatistics",
    conditionalErrorRate = ".formatProbabilities",
    secondStagePValues = ".formatPValues",
    sampleSizes = ".formatSampleSizes",
    overallSampleSizes = ".formatSampleSizes",
    effectMatrix = ".formatMeans",
    gED50 = ".formatHowItIs",
    slope = ".formatHowItIs",
    epsilonValue = ".formatHowItIs",
    threshold = ".formatHowItIs",
    rejectAtLeastOne = ".formatProbabilities",
    selectedArms = ".formatProbabilities",
    rejectedArmsPerStage = ".formatProbabilities",
    successPerStage = ".formatProbabilities",
    effectEstimate = ".formatMeans",
    subjectsControlArm = ".formatSampleSizes",
    subjectsActiveArm = ".formatSampleSizes",
    pValue = ".formatPValues",
    conditionalCriticalValue = ".formatCriticalValues",
    piControlH1 = ".formatRates",
    piMaxVector = ".formatRates",
    omegaMaxVector = ".formatRates",
    muMaxVector = ".formatMeans",
    numberOfEvents = ".formatEvents",
    numberOfActiveArms = ".formatRates",
    maxInformation = ".formatHowItIs",
    informationEpsilon = ".formatProbabilities",
    delayedInformation = ".formatRates",
    decisionCriticalValues = ".formatCriticalValues",
    reversalProbabilities = ".formatProbabilities",
    locationSampleSize = ".formatProbabilities",
    variationSampleSize = ".formatProbabilities",
    subscoreSampleSize = ".formatProbabilities",
    locationConditionalPower = ".formatProbabilities",
    variationConditionalPower = ".formatProbabilities",
    subscoreConditionalPower = ".formatProbabilities",
    performanceScore = ".formatProbabilities"
)
