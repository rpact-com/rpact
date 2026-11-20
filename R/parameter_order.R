## |
## |  rpact: Confirmatory Adaptive Clinical Trial Design and Analysis
## |  This file is part of the rpact R package.
## |
## |  RPACT — Statistical Tools for Drug Development
## |
## |  Authors: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  SPDX-FileCopyrightText: RPACT GmbH, Germany <info@rpact.com>
## |  SPDX-License-Identifier: LGPL-3.0-only
## |
## |  Licensed under the GNU Lesser General Public License, version 3.
## |  License text: https://www.gnu.org/licenses/lgpl-3.0.html
## |
## |  Package documentation: https://www.rpact.org
## |  Resources and customer portal: https://connect.rpact.app
## |  Consulting and services: https://www.rpact.com
## |  Contact: info@rpact.com
## |

# Defines the order of the parameter output
C_PARAMETER_ORDER_DESIGN_FIXED <- c(
    "stages",
    "alpha",
    "beta",
    "twoSidedPower",
    "directionUpper",
    "sided",
    "criticalValues",
    "stageLevels"
)

.assertIsUniqueVector(C_PARAMETER_ORDER_DESIGN_FIXED, "C_PARAMETER_ORDER_DESIGN_FIXED")

C_PARAMETER_ORDER_DESIGN_FISHER <- c(
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
)

.assertIsUniqueVector(C_PARAMETER_ORDER_DESIGN_FISHER, "C_PARAMETER_ORDER_DESIGN_FISHER")

C_PARAMETER_ORDER_DESIGN_GS_AND_IN <- c(
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
    "efficacyStops",
    "futilityStops",
    "criticalValues",
    "stageLevels",
    "decisionCriticalValues",
    "reversalProbabilities"
)

.assertIsUniqueVector(C_PARAMETER_ORDER_DESIGN_GS_AND_IN, "C_PARAMETER_ORDER_DESIGN_GS_AND_IN")

C_PARAMETER_ORDER_DESIGN_PLAN <- c(
    "typeOfComputation",
    "alternative",
    "stDev",
    "normalApproximation",
    "conservative",
    "meanRatio",
    "groups",
    "activeArms",
    "populations",
    "thetaH0",
    "pi1",
    "pi2",
    "piTreatment",
    "piTreatments",
    "piControl",
    "piControls",
    "lambda1",
    "lambda2",
    "median1",
    "median2",
    "lambdaTreatment",
    "lambdaControl",
    "medianTreatment",
    "medianControl",
    "hazardRatio",
    "effect",
    "effectMatrix",
    "typeOfShape",
    "gED50",
    "slope",
    "simulationType",
    "directionUpper",
    "allocationRatioPlanned",
    "allocation1",
    "allocation2",
    "optimumAllocationRatio",
    "accountForObservationTimes",
    "eventTime",
    "accrualTime",
    "totalAccrualTime",
    "accrualIntensity",
    "accrualIntensityRelative",
    "kappa",
    "piecewiseSurvivalTime",
    "followUpTime",
    "dropoutRate1",
    "dropoutRate2",
    "dropoutTime",
    "seed",
    "singleEventsPerStage",
    "singleEventsPerArmAndStage",
    "singleEventsPerSubsetAndStage",
    "eventsPerStage",
    "populationEventsPerStage",
    "cumulativeEventsPerStage",
    "overallEventsPerStage",
    "expectedNumberOfEventsPerStage",
    "expectedNumberOfEvents",
    "expectedEventsH0",
    "expectedEventsH01",
    "expectedEventsH1",
    "eventsNotAchieved",
    "maxNumberOfEvents",
    "maxNumberOfSubjects",
    "maxNumberOfSubjects1",
    "maxNumberOfSubjects2",
    "numberOfSubjects",
    "numberOfSubjects1",
    "numberOfSubjects2",
    "expectedNumberOfSubjects",
    "expectedNumberOfSubjectsH0",
    "expectedNumberOfSubjectsH01",
    "expectedNumberOfSubjectsH1",
    "sampleSizes",
    "rejectPerStage",
    "rejectedArmsPerStage",
    "rejectedPopulationsPerStage",
    "rejectAtLeastOne",
    "overallReject",
    "futilityPerStage",    
    "futilityStop",
    "earlyStop",
    "analysisTime",
    "studyDuration",
    "maxStudyDuration",
    "successPerStage",
    "selectedArms",
    "selectedPopulations",
    "numberOfSelectedArms",
    "numberOfPopulations",
    "criticalValuesEffectScale",
    "criticalValuesEffectScaleDelayedInformation",
    "criticalValuesEffectScaleLower",
    "criticalValuesEffectScaleUpper",
    "criticalValuesPValueScale",
    "futilityBoundsEffectScale",
    "futilityBoundsEffectScaleDelayedInformation",
    "futilityBoundsEffectScaleLower",
    "futilityBoundsEffectScaleUpper",
    "futilityBoundsPValueScale",
    "futilityBoundsPValueScaleDelayedInformation",
    "conditionalPowerAchieved",
    "observedDropoutProportion1",
    "observedDropoutProportion2",
    "dropoutObservationTime1",
    "dropoutObservationTime2",
    "maxNumberOfIterations",
    "iterations",
    "plannedEvents",
    "plannedSubjects"
)

.assertIsUniqueVector(C_PARAMETER_ORDER_DESIGN_PLAN, "C_PARAMETER_ORDER_DESIGN_PLAN")
