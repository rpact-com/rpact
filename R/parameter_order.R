## |
## |  *Parameter Order*
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

C_PARAMETER_ORDER_DESIGN_PLAN <- c(
    "typeOfComputation", 
    "maxNumberOfSubjects", 
    "maxNumberOfSubjects1", 
    "maxNumberOfSubjects2", 
    "maxNumberOfEvents", 
    "alternative", 
    "stDev", 
    "normalApproximation", 
    "conservative", 
    "meanRatio", 
    "groups", 
    "thetaH0", 
    "lambda1", 
    "lambda2", 
    "pi1", 
    "pi2", 
    "median1", 
    "median2", 
    "hazardRatio", 
    "effect", 
    "directionUpper", 
    "allocationRatioPlanned", 
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
    "dropoutTime"
)
