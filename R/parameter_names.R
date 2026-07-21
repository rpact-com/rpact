## |
## |  *Parameter Names*
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

C_PARAMETER_NAMES <- createDictionary("C_PARAMETER_NAMES", list(
    iterations = "Iterations",
    seed = "Seed",
    groups = "Treatment groups",
    stages = "Stages",
    sampleSizes = "Sample sizes",
    means = "Means",
    stDevs = "Standard deviations",
    overallEvents = "Cumulative events",
    overallEvents1 = "Cumulative events (1)",
    overallEvents2 = "Cumulative events (2)",
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
    conservative = "Conservative",
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
    combInverseNormal = "Combination test statistics", # Inverse normal combination
    combFisher = "Combination test statistics", # Fisher combination
    weightsFisher = "Fixed weights",
    weightsInverseNormal = "Fixed weights",
    overallLogRanks = "Cumulative log-ranks",
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
    accrualTimeOriginal = "Accrual time",
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
    studyDurationH1 = "Expected study duration under H1",
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
    eventsPerStage1 = "Observed events by stage (1)",
    eventsPerStage2 = "Observed events by stage (2)",
    testStatistic = "Test statistic",
    logRankStatistic = "Log-rank statistic",
    hazardRatioEstimateLR = "Hazard ratio estimate LR",
    delayedResponseAllowed = "Delayed response allowed",
    delayedResponseEnabled = "Delayed response enabled",
    piecewiseSurvivalEnabled = "Piecewise exponential survival enabled",
    median1 = "median(1)",
    median2 = "median(2)",
    eventsPerStage = "Number of events per stage",
    expectedNumberOfEventsPerStage = "Expected number of events by stage",
    eventsNotAchieved = "Events not achieved",
    cumulativeEventsPerStage = "Cumulative number of events",
    expectedNumberOfEvents = "Expected number of events",
    singleEventsPerStage = "Single number of events",
    singleEventsPerArmAndStage = "Single number of events",
    singleEventsPerSubsetAndStage = "Single number of events",
    populationEventsPerStage = "Number of events per population",
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
    doseLevels = "Dose levels",
    adaptations = "Adaptations",
    typeOfSelection = "Type of selection",
    effectMeasure = "Effect measure",
    successCriterion = "Success criterion",
    epsilonValue = "Epsilon value",
    rValue = "r value",
    threshold = "Threshold",
    rejectAtLeastOne = "Overall reject at least one",
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
    omegaMaxVector = "Effect size in highest dose arm",
    muMaxVector = "mu_max",
    activeArms = "Active arms",
    populations = "Populations",
    numberOfEvents = "Number of events",
    calcSubjectsFunction = "Calculate subjects function",
    calcEventsFunction = "Calculate events function",
    selectArmsFunction = "Select arms function",
    numberOfSelectedArms = "Number of selected active arms",
    numberOfActiveArms = "Number of active arms", # deprecated
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
    performanceScore = "Performance scores",
    overdispersion = "Overdispersion",
    lambda = "Lambda",
    fixedExposureTime = "Fixed exposure time",
    calendarTime = "Calendar time",
    studyTime = "Study time",
    studySubjects = "Study subjects",
    expectedStudyDurationH1 = "Expected study duration under H1",
    informationOverStages = "Information over stages",
    expectedInformationH0 = "Expected information under H0",
    expectedInformationH01 = "Expected information under H0/H1",
    expectedInformationH1 = "Expected information under H1",
    plannedCalendarTime = "Planned calendar time",
    efficacyStops = "Efficacy stops",
    futilityStops = "Futility stops",
    stdErrorEstimate = "Standard error estimate"
))

C_PARAMETER_NAMES_PLOT_SETTINGS <- createDictionary("C_PARAMETER_NAMES_PLOT_SETTINGS", list(
    "lineSize" = "Line size",
    "pointSize" = "Point size",
    "pointColor" = "Point color",
    "mainTitleFontSize" = "Main title font size",
    "axesTextFontSize" = "Axes text font size",
    "legendFontSize" = "Legend font size",
    "scalingFactor" = "Scaling factor"
))

.getTrialDesign <- function(obj) {
    if (inherits(obj, "TrialDesignSet") && length(obj$designs) > 0) {
        obj <- obj$designs[[1]]
    }

    if (!inherits(obj, "TrialDesign")) {
        obj <- obj[[".design"]]
    }

    if (is.null(obj) || !inherits(obj, "TrialDesign")) {
        return(NULL)
    }

    return(obj)
}

.getParameterNameTrialDesign <- function(parameterName, obj) {
    design <- .getTrialDesign(obj)
    if (is.null(design)) {
        return(parameterName)
    }

    if (identical(parameterName, "futilityBounds")) {
        if (.isDelayedInformationEnabled(design = design)) {
            if (!is.na(design$bindingFutility) && !design$bindingFutility) {
                return("futilityBoundsDelayedInformationNonBinding")
            }
            return("futilityBoundsDelayedInformation")
        } else if (!is.na(design$bindingFutility) && !design$bindingFutility) {
            return("futilityBoundsNonBinding")
        }
    }
    if (identical(parameterName, "criticalValues") && .isDelayedInformationEnabled(design = design)) {
        return("criticalValuesDelayedInformation")
    }
    if (identical(parameterName, "criticalValuesEffectScale") && .isDelayedInformationEnabled(design = design)) {
        return("criticalValuesEffectScaleDelayedInformation")
    }
    if (identical(parameterName, "futilityBoundsEffectScale") && .isDelayedInformationEnabled(design = design)) {
        return("futilityBoundsEffectScaleDelayedInformation")
    }
    if (identical(parameterName, "futilityBoundsPValueScale") && .isDelayedInformationEnabled(design = design)) {
        return("futilityBoundsPValueScaleDelayedInformation")
    }
    return(parameterName)
}

.getParameterCaption <- function(parameterName, obj = NULL, ..., tableOutputEnabled = FALSE) {
    .assertIsSingleCharacter(parameterName, "parameterName")

    if (grepl("\\$", parameterName)) {
        parts <- strsplit(parameterName, "\\$", fixed = TRUE)[[1]]
        if (length(parts) == 2) {
            if (!is.null(obj[[parts[1]]])) {
                obj <- obj[[parts[1]]]
            }
            parameterName <- parts[2]
        }
    }

    if (is.null(obj)) {
        if (tableOutputEnabled) {
            return(C_TABLE_COLUMN_NAMES[[parameterName]])
        }

        return(C_PARAMETER_NAMES[[parameterName]])
    }

    parameterName <- .getParameterNameTrialDesign(parameterName, obj)

    if (inherits(obj, "PlotSettings")) {
        return(C_PARAMETER_NAMES_PLOT_SETTINGS[[parameterName]])
    }

    pluralExt <- ifelse(tableOutputEnabled, "", "s")

    if (identical(parameterName, "futilityBounds") &&
            inherits(obj, "TrialDesignSet") && length(obj$designs) > 1) {
        bindingFutilityValues <- logical(0)
        for (design in obj$designs) {
            bindingFutilityValues <- unique(c(bindingFutilityValues, design$bindingFutility))
        }
        if (length(bindingFutilityValues) > 1) {
            return(paste0("Futility bound", pluralExt))
        }
    }

    if (identical(parameterName, "eventsPerStage") &&
            (inherits(obj, "TrialDesignPlanSurvival") ||
                inherits(obj, "SimulationResultsMultiArmSurvival"))) {
        return(ifelse(tableOutputEnabled, "Cumulative events", "Cumulative events per stage"))
    }

    if (identical(parameterName, "lambda1") &&
            inherits(obj, "TrialDesignPlanSurvival") &&
            !is.null(obj$.piecewiseSurvivalTime) &&
            obj$.piecewiseSurvivalTime$piecewiseSurvivalEnabled) {
        return("Piecewise survival lambda (1)")
    }

    if (identical(parameterName, "lambda2") &&
            inherits(obj, "TrialDesignPlanSurvival") &&
            !is.null(obj$.piecewiseSurvivalTime) &&
            obj$.piecewiseSurvivalTime$piecewiseSurvivalEnabled) {
        return("Piecewise survival lambda (2)")
    }

    if (identical(parameterName, "maxNumberOfEvents") &&
            inherits(obj, "TrialDesignPlanSurvival") &&
            identical(obj$.design$kMax, 1L)) {
        return("Number of events")
    }

    if (identical(parameterName, "studyDuration") &&
            inherits(obj, "TrialDesignPlan") &&
            identical(obj$.design$kMax, 1L)) {
        return("Study duration")
    }

    if (inherits(obj, "AnalysisResults")) {
        if (identical(parameterName, "repeatedConfidenceIntervalLowerBounds") &&
                .isTrialDesignConditionalDunnett(obj$.design)) {
            return(paste0("Overall confidence interval", pluralExt, " (lower)"))
        }
        if (identical(parameterName, "repeatedConfidenceIntervalUpperBounds") &&
                .isTrialDesignConditionalDunnett(obj$.design)) {
            return(paste0("Overall confidence interval", pluralExt, " (upper)"))
        }
    }

    if (identical(parameterName, "stDev") &&
            (inherits(obj, "TrialDesignPlanMeans") || inherits(obj, "SimulationResultsMeans")) &&
            isTRUE(obj$meanRatio)) {
        return("Coefficient of variation")
    }

    if (identical(parameterName, "criticalValuesPValueScale") && inherits(obj, "TrialDesign") &&
            .getClassName(obj) != "TrialDesign" && obj$sided == 2) {
        return(paste0("Local two-sided significance level", ifelse(tableOutputEnabled, "", "s")))
    }

    if (identical(parameterName, "overallStDevs") &&
            ((inherits(obj, "StageResults") && obj$isOneSampleDataset()) ||
                inherits(obj, "DatasetMeans"))) {
        return(paste0("Cumulative standard deviation", ifelse(tableOutputEnabled, "", "s")))
    }

    if (tableOutputEnabled) {
        paramCaption <- C_TABLE_COLUMN_NAMES[[parameterName]]
    } else {
        paramCaption <- C_PARAMETER_NAMES[[parameterName]]
    }

    if (grepl("DelayedInformation", parameterName)) {
        design <- .getTrialDesign(obj)
        if (!is.null(design) && isFALSE(design$directionUpper)) {
            if (grepl("Lower bounds of continuation", paramCaption)) {
                paramCaption <- sub("Lower bounds of continuation", "Upper bounds of continuation", paramCaption)
            } else {
                paramCaption <- sub("Upper bounds of continuation", "Lower bounds of continuation", paramCaption)
            }
        }
    }

    if (parameterName %in% c("rejectAtLeastOne", "rejectedArmsPerStage") &&
            inherits(obj, "SimulationResultsMultiArmSurvival") &&
            !is.null(obj$activeArms) && obj$activeArms == 1) {
        if (identical(parameterName, "rejectAtLeastOne")) {
            paramCaption <- "Overall reject"
        } else if (identical(parameterName, "rejectedArmsPerStage")) {
            paramCaption <- "Reject per stage"
        }
    }

    return(paramCaption)
}
