TrialDesignOptimalConditionalError <- setRefClass(
  Class = "TrialDesignOptimalConditionalError",
  fields = list(
    alpha = "numeric",
    alpha1 = "numeric",
    alpha0 = "numeric",
    conditionalPower = "numeric",
    conditionalPowerFunction = "function",
    delta1 = "numeric",
    delta1Min = "numeric",
    delta1Max = "numeric",
    firstStageInformation = "numeric",
    useInterimEstimate = "logical",
    likelihoodRatioDistribution = "character",
    deltaLR = "numeric",
    weightsDeltaLR = "numeric",
    tauLR = "numeric",
    kappaLR = "numeric",
    deltaMaxLR = "numeric",
    levelConstant = "numeric",
    monotonisationConstants = "list",
    minimumSecondStageInformation = "numeric",
    maximumSecondStageInformation = "numeric",
    minimumConditionalError = "numeric",
    maximumConditionalError = "numeric",
    levelConstantMinimum = "numeric",
    levelConstantMaximum = "numeric",
    ncp1 = "numeric",
    ncp1Min = "numeric",
    ncp1Max = "numeric",
    enforceMonotonicity = "logical"
  ),
  methods = list(
    initialize = function(
    alpha = NA_real_,
    alpha1 = NA_real_,
    alpha0 = NA_real_,
    conditionalPower = NA_real_,
    conditionalPowerFunction = NA,
    delta1 = NA_real_,
    delta1Min = NA_real_,
    delta1Max = NA_real_,
    firstStageInformation = NA_real_,
    useInterimEstimate = TRUE,
    likelihoodRatioDistribution = "",
    deltaLR = NA_real_,
    weightsDeltaLR = NA_real_,
    tauLR = NA_real_,
    kappaLR = NA_real_,
    deltaMaxLR = NA_real_,
    levelConstant = NA_real_,
    monotonisationConstants,
    minimumSecondStageInformation = 0,
    maximumSecondStageInformation = Inf,
    minimumConditionalError = 0,
    maximumConditionalError = 1,
    levelConstantMinimum = 0,
    levelConstantMaximum = 10,
    ncp1 = NA_real_,
    ncp1Min = NA_real_,
    ncp1Max = NA_real_,
    enforceMonotonicity = TRUE
    ) {
      
      # Range assertions for alpha, alpha1, alpha0
      # General range assertions
      .assertIsSingleNumber(x = alpha, xName = "alpha")
      .assertIsSingleNumber(x = alpha1, xName = "alpha1")
      .assertIsSingleNumber(x = alpha0, xName = "alpha0")
      
      .assertIsInOpenInterval(x = alpha, xName = "alpha", lower = 0, upper = 1)
      .assertIsInClosedInterval(x = alpha1, xName = "alpha1", lower = 0, upper = 1)
      .assertIsInClosedInterval(x = alpha0, xName = "alpha0", lower = 0, upper = 1)
      
      # Context-related range assertions
      .assertIsInClosedInterval(x = alpha1, xName = "alpha1", lower = 0, upper = alpha)
      .assertIsInClosedInterval(x = alpha0, xName = "alpha0", lower = alpha1, upper = 1)
      
      if(is.na(conditionalPower) && is.null(suppressWarnings(body(conditionalPowerFunction)))) {
        stop("Must specify either conditionalPower or a valid conditionalPowerFunction.")
      }
      else {
        if(!is.na(conditionalPower)) {
          
          .assertIsSingleNumber(x = conditionalPower, xName = "conditionalPower")
          .assertIsInOpenInterval(x = conditionalPower, xName = "conditionalPower", lower = 0, upper = 1)
          
          if(!is.null(suppressWarnings(body(conditionalPowerFunction)))) {
            warning("Both conditionalPower and conditionalPowerFunction are provided. Using conditionalPower and ignoring conditionalPowerFunction.")
          }
        }
        else if(!is.null(suppressWarnings(body(conditionalPowerFunction)))) {
          
          # Check if function is increasing
          # Grid of values
          pValueGrid <- seq(from = alpha1, to = alpha0, length.out = 50)
          conditionalPowerValues <- conditionalPowerFunction(pValueGrid)
          
          # Any function value larger than previous?
          if(any(conditionalPowerValues[2:(length(conditionalPowerValues))] > conditionalPowerValues[1:(length(conditionalPowerValues)-1)])) {
            warning("Conditional power function should not be increasing in the first-stage p-value.")
          }
          
          if(useInterimEstimate && (minimumSecondStageInformation > 0 || maximumSecondStageInformation < Inf)) {
            warning("Use of conditional power function, interim estimate and information constraints may lead to non-monotone conditional error function.")
          }
          
          .self$conditionalPowerFunction <- conditionalPowerFunction
        }
      }
      
      .assertIsInOpenInterval(x = firstStageInformation, xName = "firstStageInformation", lower = 0, upper = Inf)
      .assertIsSingleLogical(x = useInterimEstimate, xName = "useInterimEstimate")
      
      # Set initial parameters
      .self$alpha <- alpha
      .self$alpha1 <- alpha1
      .self$alpha0 <- alpha0
      .self$conditionalPower <- conditionalPower
      .self$firstStageInformation <- firstStageInformation
      .self$likelihoodRatioDistribution <- likelihoodRatioDistribution
      .self$useInterimEstimate <- useInterimEstimate
      
      .assertIsSingleNumber(x = levelConstantMinimum, xName = "levelConstantMinimum")
      .assertIsSingleNumber(x = levelConstantMaximum, xName = "levelConstantMaximum")
      
      if(levelConstantMinimum >= levelConstantMaximum) {
        stop("levelConstantMinimum must be smaller than levelConstantMaximum.")
      }
      
      .self$levelConstantMinimum <- levelConstantMinimum
      .self$levelConstantMaximum <- levelConstantMaximum
      
      # Derive effect sizes for conditional power
      # When using an interim estimate, derive minimal or maximal effects
      if(useInterimEstimate) {
        # Neither lower limit provided -> error
        if((is.na(ncp1Min) && (is.na(delta1Min)))) {
          stop("Must provide a lower limit for the interim estimate by using ncp1Min or delta1Min.")
        }
        else if(!is.na(delta1Min)) {
          
          .assertIsSingleNumber(x = delta1Min, xName = "delta1Min")
          .assertIsInOpenInterval(x = delta1Min, xName = "delta1Min", lower = 0, upper = Inf)
          .assertIsSingleNumber(x = delta1Max, xName = "delta1Max")
      
          .assertIsInClosedInterval(x = delta1Max, xName = "delta1Max", lower = delta1Min, upper = Inf)
          .self$delta1Min <- delta1Min
          .self$delta1Max <- delta1Max
          
          if(!is.na(ncp1Min)) {
            warning("Both ncp1Min and delta1Min are provided. Using delta1Min and ignoring ncp1Min.")
          }
          
          .self$ncp1Min <- delta1Min * sqrt(firstStageInformation)
          .self$ncp1Max <- delta1Max * sqrt(firstStageInformation)
        }
        else if(!is.na(ncp1Min)) {
          
          .assertIsSingleNumber(x = ncp1Min, xName = "ncp1Min")
          .assertIsInOpenInterval(x = ncp1Min, xName = "ncp1Min", lower = 0, upper = Inf)
          
          .assertIsSingleNumber(x = ncp1Max, xName = "ncp1Max")
          .assertIsInClosedInterval(x = ncp1Max, xName = "ncp1Max", lower = ncp1Min, upper = Inf)
          
          .self$ncp1Min <- ncp1Min
          .self$ncp1Max <- ncp1Max
          
          .self$delta1Min <- ncp1Min / sqrt(firstStageInformation)
          .self$delta1Max <- ifelse(ncp1Max == Inf, Inf, ncp1Max/sqrt(firstStageInformation))
        }
        else {
          stop("Unexpected error occured during determination of restrictions for interim estimate.")
        }
        
      }
      # When not using an interim estimate, derive fixed effects
      else {
        # If non-centrality parameter was not specified, calculate it from delta1
        if(!is.na(delta1)) {
          .assertIsInOpenInterval(x = delta1, xName = "delta1", lower = 0, upper = Inf)
          
          .self$delta1 <- delta1
          if(!is.na(ncp1)) {
            warning("Both delta1 and ncp1 are provided. Using delta1 and ignoring ncp1.")
          }
          .self$ncp1 <- delta1*sqrt(firstStageInformation)
        }
        # If delta1 was not specified, calculate it from ncp1
        else if(!is.na(ncp1)) {
          .assertIsInOpenInterval(x = ncp1, xName = "ncp1", lower = 0, upper = Inf)
          
          .self$ncp1 <- ncp1
          .self$delta1 <- ncp1/sqrt(firstStageInformation)
        }
        # Else, none of ncp1 and delta1 were specified
        else {
          stop("Must specify delta1 or ncp1 when using a fixed effect for conditional power.")
        }
      }
      
      # Range assertions for constraints
      # General range assertions
      .assertIsInClosedInterval(x = minimumConditionalError, xName = "minimumConditionalError", lower = 0, upper = 1)
      .assertIsInClosedInterval(x = maximumConditionalError, xName = "maximumConditionalError", lower = 0, upper = 1)
      
      .assertIsInClosedInterval(x = minimumSecondStageInformation, xName = "minimumSecondStageInformation", lower = 0, upper = Inf)
      .assertIsInClosedInterval(x = maximumSecondStageInformation, xName = "maximumSecondStageInformation", lower = 0, upper = Inf)
      
      if(maximumSecondStageInformation == 0) {
        stop("Maximum second-stage information must be larger than 0.")
      }
      
      # Context-related range assertions
      .assertIsInClosedInterval(x = minimumConditionalError, xName = "minimumConditionalError", lower = 0, upper = maximumConditionalError)
      
      .assertIsInClosedInterval(x = minimumSecondStageInformation, xName = "minimumSecondStageInformation", lower = 0, upper = maximumSecondStageInformation)
      
      # Identify constraints for minimum conditional error / maximum second-stage information
      .self$minimumConditionalError <- minimumConditionalError
      .self$maximumSecondStageInformation <- maximumSecondStageInformation
      
      # Identify constraints for maximum conditional error / minimum second-stage information
      .self$maximumConditionalError <- maximumConditionalError
      .self$minimumSecondStageInformation <- minimumSecondStageInformation
      
      
      .assertIsSingleLogical(x = enforceMonotonicity, xName = "enforceMonotonicity")
      .self$enforceMonotonicity <- enforceMonotonicity
      
      # Identify specific distribution parameters
      if(likelihoodRatioDistribution == "fixed") {
        if(any(is.na(deltaLR))) {
          stop("Must provide deltaLR for fixed effect in likelihood ratio.")
        }
        else {
          .self$deltaLR <- deltaLR
          # If any of the weights are NA, use equal weights
          if(any(is.na(weightsDeltaLR))) {
            .self$weightsDeltaLR <- rep(1/length(deltaLR), length(deltaLR))
            # For multiple effects, tell the user that equal weights are used.
            if(length(deltaLR) > 1) {
              message("At least one entry in weightsDeltaLR is NA. Using equal weights for effects in fixed likelihood ratio.")
            }
          }
          else {
            .assertIsInClosedInterval(x = weightsDeltaLR, xName = "weightsDeltaLR", lower = 0, upper = 1)
            # Check if weightsDeltaLR and deltaLR are of equal length
            if(length(weightsDeltaLR) != length(deltaLR)) {
              stop("Must provide exactly one weight in weightsDeltaLR per entry of deltaLR.")
            }
            # Verify that weightsDeltaLR sums to 1
            if(sum(weightsDeltaLR) != 1) {
              stop("Weights in weightsDeltaLR must sum to 1.")
            }
            .self$weightsDeltaLR <- weightsDeltaLR
          }
        }
      }
      else if(likelihoodRatioDistribution == "normal") {
        if(is.na(deltaLR) || is.na(tauLR)) {
          stop("Must provide deltaLR and tauLR for normal prior in likelihood ratio.")
        }
        else {
          .assertIsInOpenInterval(x = tauLR, xName = "tauLR", lower = 0, upper = Inf)
          
          .self$deltaLR <- deltaLR
          .self$tauLR <- tauLR
        }
      }
      else if(likelihoodRatioDistribution == "exp") {
        if(is.na(kappaLR)) {
          stop("Must provide kappaLR for exponential prior in likelihood ratio.")
        }
        else {
          .assertIsInOpenInterval(x = kappaLR, xName = "kappaLR", lower = 0, upper = Inf)
          .self$kappaLR <- kappaLR
        }
      }
      else if(likelihoodRatioDistribution == "unif") {
        if(is.na(deltaMaxLR)) {
          stop("Must provide deltaMaxLR for uniform prior in likelihood ratio.")
        }
        else {
          .assertIsInOpenInterval(x = deltaMaxLR, xName = "deltaMaxLR", lower = 0, upper = Inf)
          .self$deltaMaxLR <- deltaMaxLR
        }
      }
      else if(likelihoodRatioDistribution == "maxlr") {
        
      }
      else {
        stop("Distribution not matched. likelihoodRatioDistribution should be one of 'fixed', 'normal', 'exp', 'unif' or 'maxlr'.")
      }
      
      # Calculate monotonisation constants
      .self$monotonisationConstants <- getMonotonisationConstants(
        fun = "getQ", lower = alpha1, upper = alpha0, argument = "firstStagePValue",
        design = .self
      )
      
      # Calculate level constant
      .self$levelConstant <- getLevelConstant(
        design = .self
      )$root
    },
    show = function() {
      print.TrialDesignOptimalConditionalError(.self)
    }
  )
)