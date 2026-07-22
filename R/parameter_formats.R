## |
## |  *Parameter Formats*
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

C_PARAMETER_FORMAT_FUNCTIONS <- createDictionary("C_PARAMETER_FORMAT_FUNCTIONS", list(
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
    criticalValues = ".formatCriticalValues",
    betaSpent = ".formatProbabilities",
    futilityBounds = ".formatCriticalValues",
    constantBoundsHP = ".formatCriticalValues",
    nMax = ".formatProbabilities",
    nFixed = ".formatSampleSizes",
    nFixed1 = ".formatSampleSizes",
    nFixed2 = ".formatSampleSizes",
    shift = ".formatProbabilities",
    inflationFactor = ".formatProbabilities",
    information = ".formatRates",
    power = ".formatProbabilities",
    sided = ".formatSided",
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
    median = ".formatRates",
    lambdaControl = ".formatRates",
    medianControl = ".formatRates",
    pi1H1 = ".formatRates",
    pi2H1 = ".formatRates",
    piecewiseSurvivalTime = ".formatTime",
    lambda2 = ".formatRates",
    lambda1 = ".formatRates",
    eventTime = ".formatEventTime",
    accrualTime = ".formatTime",
    accrualTimeOriginal = ".formatTime",
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
    expectedEvents = ".formatEvents",
    varianceEvents = ".formatEvents",
    overallExpectedEvents = ".formatEvents",
    overallVarianceEvents = ".formatEvents",
    events = ".formatEvents",
    overallEvents = ".formatEvents",
    expectedNumberOfEvents = ".formatEvents",
    expectedNumberOfEventsPerStage = ".formatEvents",
    cumulativeEventsPerStage = ".formatEvents",
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
    singleEventsPerStage = ".formatEvents",
    singleEventsPerArmAndStage = ".formatEvents",
    singleEventsPerSubsetAndStage = ".formatEvents",
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
    overallPi1 = ".formatRates",
    overallPi2 = ".formatRates",
    adjustedStageWisePValues = ".formatPValues",
    overallAdjustedTestStatistics = ".formatTestStatistics",
    conditionalErrorRate = ".formatProbabilities",
    secondStagePValues = ".formatPValues",
    sampleSizes = ".formatSampleSizes",
    overallSampleSizes = ".formatSampleSizes",
    effectMatrix = ".formatMeans",
    gED50 = ".formatHowItIs",
    slope = ".formatHowItIs",
    doseLevels = ".formatHowItIs",
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
    numberOfSelectedArms = ".formatRates",
    numberOfActiveArms = ".formatRates", # deprecated
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
    performanceScore = ".formatProbabilities",
    overdispersion = ".formatStDevs",
    lambda = ".formatRates",
    fixedExposureTime = ".formatTime",
    calendarTime = ".formatTime",
    studyTime = ".formatTime",
    studySubjects = ".formatSampleSizes",
    expectedStudyDurationH1 = ".formatTime",
    informationOverStages = ".formatRatesDynamic",
    expectedInformationH0 = ".formatRatesDynamic",
    expectedInformationH01 = ".formatRatesDynamic",
    expectedInformationH1 = ".formatRatesDynamic"
))

.getParameterFormatFunction <- function(parameterName, obj = NULL) {
    if (is.null(obj) || !inherits(obj, "FieldSet")) {
        return(C_PARAMETER_FORMAT_FUNCTIONS[[parameterName]])
    }

    if (parameterName == "overallAdjustedTestStatistics" &&
            inherits(obj, "ClosedCombinationTestResults") &&
            !is.null(obj[[".design"]]) &&
            inherits(obj$.design, C_CLASS_NAME_TRIAL_DESIGN_FISHER)) {
        return(".formatTestStatisticsFisher")
    }

    if (parameterName == "maxInformation" && inherits(obj, "TrialDesignPlanCountData")) {
        return(".formatRatesDynamic")
    }

    if (parameterName == "nFixed" && inherits(obj, "TrialDesignCharacteristics")) {
        return(".formatProbabilities")
    }

    if (parameterName == "criticalValues" && inherits(obj, "TrialDesignFisher")) {
        return(".formatCriticalValuesFisher")
    }

    if (parameterName == "accrualTime" && inherits(obj, "TrialDesignPlanCountData") &&
            obj$isUserDefinedParameter("accrualTime")) {
        return(".formatHowItIs")
    }

    if (parameterName == "accrualTimeOriginal" && inherits(obj, "TrialDesignPlanCountData") &&
            obj$isUserDefinedParameter("accrualTimeOriginal")) {
        return(".formatHowItIs")
    }

    if (parameterName == "userAlphaSpending" &&
            obj$isGeneratedParameter("userAlphaSpending")) {
        return(".formatProbabilities")
    }

    if (parameterName == "userBetaSpending" &&
            obj$isGeneratedParameter("userBetaSpending")) {
        return(".formatProbabilities")
    }

    return(C_PARAMETER_FORMAT_FUNCTIONS[[parameterName]])
}
