library("R6")
## |
## |  *Analysis result classes*
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

#'
#' @name ConditionalPowerResults
#'
#' @title
#' Conditional Power Results
#'
#' @description
#' Class for conditional power calculations
#' 
#' @template field_nPlanned 
#' @template field_allocationRatioPlanned
#' @template field_iterations
#' @template field_seed
#' @template field_simulated
#' @template field_conditionalPower
#' @template field_thetaH1
#' @template field_assumedStDev
#'
#' @details
#' This object cannot be created directly; use \code{\link[=getConditionalPower]{getConditionalPower()}}
#' with suitable arguments to create the results of a group sequential or a combination test design.
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
ConditionalPowerResults <- R6Class("ConditionalPowerResults",
                                       inherit = ParameterSet,
                                       public = list(
                                         .plotSettings = NULL,
                                         .design = NULL,
                                         .stageResults = NULL,
                                         .plotData = NULL,
                                         nPlanned = NULL,
                                         allocationRatioPlanned = NULL,
                                         iterations = NULL,
                                         seed = NULL,
                                         simulated = NULL,
                                         initialize = function(..., .design = NULL, .stageResults = NULL, .plotData = NULL, nPlanned = NULL, allocationRatioPlanned = NULL, iterations = NULL, seed = NULL, simulated = NULL) {
                                           self$.design <- .design 
                                           self$.stageResults <- .stageResults 
                                           self$.plotData <- .plotData 
                                           self$nPlanned <- nPlanned 
                                           self$allocationRatioPlanned <- allocationRatioPlanned 
                                           self$iterations <- iterations 
                                           self$seed <- seed 
                                           self$simulated <- simulated 
                                           
                                           super$initialize(...)
                                           
                                           self$.plotSettings <- PlotSettings$new()
                                           
                                           if (!is.null(self$.stageResults) && is.null(self$.design)) {
                                             self$.design <- self$.stageResults$.design
                                           }
                                           
                                           if (is.null(self$simulated) || length(self$simulated) == 0 || is.na(self$simulated)) {
                                             self$simulated <- FALSE
                                           }
                                           
                                           if (!is.null(self$.design) && length(self$.design$kMax) == 1 && self$.design$kMax == 1L) {
                                             self$.setParameterType("nPlanned", C_PARAM_NOT_APPLICABLE)
                                             self$.setParameterType("allocationRatioPlanned", C_PARAM_NOT_APPLICABLE)
                                             self$.setParameterType("conditionalPower", C_PARAM_NOT_APPLICABLE)
                                           } else {
                                             self$.setParameterType("nPlanned", C_PARAM_GENERATED)
                                             self$.setParameterType("allocationRatioPlanned", C_PARAM_USER_DEFINED)
                                             self$.setParameterType("conditionalPower", C_PARAM_GENERATED)
                                           }
                                           self$.setParameterType("simulated", C_PARAM_NOT_APPLICABLE)
                                         },
                                         show = function(showType = 1, digits = NA_integer_) {
                                           self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
                                         },
                                         .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
                                           "Method for automatically printing conditional power result objects"
                                           self$.resetCat()
                                           if (showType == 2) {
                                             super$.show(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
                                           } else {
                                             if (!is.null(self$.design) && length(self$.design$kMax) == 1 && self$.design$kMax == 1) {
                                               self$.cat(self$.toString(), ": not applicable for fixed design (kMax = 1)\n",
                                                    heading = 1,
                                                    consoleOutputEnabled = consoleOutputEnabled
                                               )
                                             } else {
                                               self$.cat(self$.toString(), ":\n\n",
                                                    heading = 1,
                                                    consoleOutputEnabled = consoleOutputEnabled
                                               )
                                             }
                                             self$.showParametersOfOneGroup(self$.getUserDefinedParameters(), "User defined parameters",
                                                                       orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                                             )
                                             self$.showParametersOfOneGroup(self$.getDefaultParameters(), "Default parameters",
                                                                       orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                                             )
                                             self$.showParametersOfOneGroup(self$.getGeneratedParameters(), "Output",
                                                                       orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                                             )
                                             self$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
                                           }
                                         },
                                         .toString = function(startWithUpperCase = FALSE) {
                                           return("Conditional power results")
                                         }
                                       )
)

#'
#' @name ConditionalPowerResultsMeans
#'
#' @title
#' Conditional Power Results Means
#' 
#' @description
#' Class for conditional power calculations of means data
#' 
#' @template field_nPlanned 
#' @template field_allocationRatioPlanned
#' @template field_iterations
#' @template field_seed
#' @template field_simulated
#' @template field_conditionalPower
#' @template field_thetaH1
#' @template field_assumedStDev
#'
#' @details
#' This object cannot be created directly; use \code{\link{getConditionalPower}}
#' with suitable arguments to create the results of a group sequential or a combination test design.
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
ConditionalPowerResultsMeans <- R6Class("ConditionalPowerResultsMeans",
                                            inherit = ConditionalPowerResults,
                                            public = list(
                                              conditionalPower = NULL,
                                              thetaH1 = NULL,
                                              assumedStDev = NULL,
                                              initialize = function(..., conditionalPower = NULL, thetaH1 = NULL, assumedStDev = NULL) {
                                                self$conditionalPower<- conditionalPower
                                                self$thetaH1<- thetaH1
                                                self$assumedStDev<- assumedStDev
                                                
                                                super$initialize(...)
                                                
                                                if ((is.null(self$conditionalPower) || length(self$conditionalPower) == 0) &&
                                                    !is.null(self$.design) && !is.null(self$.design$kMax) && length(self$.design$kMax) > 0) {
                                                  self$conditionalPower <- rep(NA_real_, self$.design$kMax)
                                                }
                                                
                                                if (is.null(self$thetaH1) || length(self$thetaH1) == 0 || all(is.na(self$thetaH1))) {
                                                  self$thetaH1 <- NA_real_
                                                }
                                                if (is.null(self$assumedStDev) || length(self$assumedStDev) == 0 || all(is.na(self$assumedStDev))) {
                                                  self$assumedStDev <- NA_real_
                                                }
                                              },
                                              .toString = function(startWithUpperCase = FALSE) {
                                                return("Conditional power results means")
                                              }
                                            )
)

ConditionalPowerResultsMultiHypotheses <- R6Class("ConditionalPowerResultsMultiHypotheses",
                                                      inherit = ConditionalPowerResults,
                                                      public = list(
                                                        conditionalPower = NULL,
                                                        initialize = function(..., conditionalPower = NULL) {
                                                          self$conditionalPower <- conditionalPower
                                                          super$initialize(...)
                                                          
                                                          if (self$.readyForInitialization()) {
                                                            gMax <- self$getGMax()
                                                            kMax <- self$.design$kMax
                                                            if (is.null(self$conditionalPower) || (nrow(self$conditionalPower) == 0 && ncol(self$conditionalPower) == 0)) {
                                                              self$conditionalPower <- matrix(rep(NA_real_, gMax * kMax), nrow = gMax, ncol = kMax)
                                                            }
                                                          }
                                                        },
                                                        .toString = function(startWithUpperCase = FALSE) {
                                                          s <- "Conditional power results"
                                                          s <- paste0(s, " ", ifelse(grepl("Enrichment", .getClassName(self$.stageResults)), "enrichment", "multi-arm"))
                                                          if (grepl("Means", .getClassName(self))) {
                                                            s <- paste0(s, " means")
                                                          } else if (grepl("Rates", .getClassName(self))) {
                                                            s <- paste0(s, " rates")
                                                          } else if (grepl("Survival", .getClassName(self))) {
                                                            s <- paste0(s, " survival")
                                                          }
                                                          return(s)
                                                        },
                                                        getGMax = function() {
                                                          return(self$.stageResults$getGMax())
                                                        },
                                                        .readyForInitialization = function() {
                                                          if (is.null(self$.design)) {
                                                            return(FALSE)
                                                          }
                                                          
                                                          if (length(self$.design$kMax) != 1) {
                                                            return(FALSE)
                                                          }
                                                          
                                                          if (is.null(self$.stageResults)) {
                                                            return(FALSE)
                                                          }
                                                          
                                                          if (is.null(self$.stageResults$testStatistics)) {
                                                            return(FALSE)
                                                          }
                                                          
                                                          return(TRUE)
                                                        }
                                                      )
)

ConditionalPowerResultsMultiArmMeans <- R6Class("ConditionalPowerResultsMultiArmMeans",
                                                    inherit = ConditionalPowerResultsMultiHypotheses,
                                                    public = list(
                                                      thetaH1 = NULL,
                                                      assumedStDevs = NULL,
                                                      initialize = function(..., thetaH1 = NULL, assumedStDevs = NULL) {
                                                        self$thetaH1 <- thetaH1
                                                        self$assumedStDevs <- assumedStDevs
                                                        super$initialize(...)
                                                        
                                                        if (self$.readyForInitialization()) {
                                                          gMax <- self$getGMax()
                                                          if (is.null(self$thetaH1) || length(self$thetaH1) == 0 || all(is.na(self$thetaH1))) {
                                                            self$thetaH1 <- rep(NA_real_, gMax)
                                                          }
                                                          if (is.null(self$assumedStDevs) || length(self$assumedStDevs) == 0 || all(is.na(self$assumedStDevs))) {
                                                            self$assumedStDevs <- rep(NA_real_, gMax)
                                                          }
                                                        }
                                                      }
                                                    )
)

#'
#' @name ConditionalPowerResultsRates
#'
#' @title
#' Conditional Power Results Rates
#' 
#' @description
#' Class for conditional power calculations of rates data
#' 
#' @template field_nPlanned 
#' @template field_allocationRatioPlanned
#' @template field_iterations
#' @template field_seed
#' @template field_simulated
#' @template field_conditionalPower
#' @template field_pi1
#' @template field_pi2
#'
#' @details
#' This object cannot be created directly; use \code{\link{getConditionalPower}}
#' with suitable arguments to create the results of a group sequential or a combination test design.
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
ConditionalPowerResultsRates <- R6Class("ConditionalPowerResultsRates",
                                            inherit = ConditionalPowerResults,
                                            public = list(
                                              conditionalPower = NULL,
                                              pi1 = NULL,
                                              pi2 = NULL,
                                              initialize = function(..., conditionalPower = NULL,
                                                                    pi1 = NULL,
                                                                    pi2 = NULL) {
                                                self$conditionalPower <- conditionalPower
                                                self$pi1 <- pi1
                                                self$pi2 <- pi2
                                                
                                                super$initialize(...)
                                                
                                                if ((is.null(self$conditionalPower) || length(self$conditionalPower) == 0) &&
                                                    !is.null(self$.design) && !is.null(self$.design$kMax) && length(self$.design$kMax) > 0) {
                                                  self$conditionalPower <- rep(NA_real_, self$.design$kMax)
                                                }
                                                
                                                if (is.null(self$pi1) || length(self$pi1) == 0 || all(is.na(self$pi1))) {
                                                  self$pi1 <- NA_real_
                                                }
                                                if (is.null(self$pi2) || length(self$pi2) == 0 || all(is.na(self$pi2))) {
                                                  self$pi2 <- NA_real_
                                                }
                                              },
                                              .toString = function(startWithUpperCase = FALSE) {
                                                return("Conditional power results rates")
                                              }
                                            )
)

ConditionalPowerResultsMultiArmRates <- R6Class("ConditionalPowerResultsMultiArmRates",
                                                    inherit = ConditionalPowerResultsMultiHypotheses,
                                                    public = list(
                                                      piTreatments = NULL,
                                                      piControl = NULL,
                                                      initialize = function(..., piTreatments = NULL, piControl = NULL) {
                                                        self$piTreatments <- piTreatments
                                                        self$piControl <- piControl
                                                        super$initialize(...)
                                                        
                                                        if (self$.readyForInitialization()) {
                                                          gMax <- self$getGMax()
                                                          if (is.null(self$piControl) || length(self$piControl) == 0 || all(is.na(self$piControl))) {
                                                            self$piControl <- NA_real_
                                                          }
                                                          if (is.null(self$piTreatments) || length(self$piTreatments) == 0 || all(is.na(self$piTreatments))) {
                                                            self$piTreatments <- rep(NA_real_, gMax)
                                                          }
                                                        }
                                                      }
                                                    )
)

#'
#' @name ConditionalPowerResultsSurvival
#'
#' @title
#' Conditional Power Results Survival
#' 
#' @description
#' Class for conditional power calculations of survival data
#' 
#' @template field_nPlanned 
#' @template field_allocationRatioPlanned
#' @template field_iterations
#' @template field_seed
#' @template field_simulated
#' @template field_conditionalPower
#' @template field_thetaH1_survival
#'
#' @details
#' This object cannot be created directly; use \code{\link{getConditionalPower}}
#' with suitable arguments to create the results of a group sequential or a combination test design.
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
ConditionalPowerResultsSurvival <- R6Class("ConditionalPowerResultsSurvival",
                                               inherit = ConditionalPowerResults,
                                               public = list(
                                                 conditionalPower = NULL,
                                                 thetaH1 = NULL,
                                                 initialize = function(..., conditionalPower = NULL, thetaH1 = NULL) {
                                                   self$conditionalPower <- conditionalPower
                                                   self$thetaH1 <- thetaH1
                                                   super$initialize(...)
                                                   
                                                   if ((is.null(self$conditionalPower) || length(self$conditionalPower) == 0) &&
                                                       !is.null(self$.design) && !is.null(self$.design$kMax) && length(self$.design$kMax) > 0) {
                                                     self$conditionalPower <- rep(NA_real_, self$.design$kMax)
                                                   }
                                                   
                                                   if (is.null(self$thetaH1) || length(self$thetaH1) == 0 || all(is.na(self$thetaH1))) {
                                                     self$thetaH1 <- NA_real_
                                                   }
                                                 },
                                                 .toString = function(startWithUpperCase = FALSE) {
                                                   return("Conditional power results survival")
                                                 }
                                               )
)

ConditionalPowerResultsMultiArmSurvival <- R6Class("ConditionalPowerResultsMultiArmSurvival",
                                                       inherit = ConditionalPowerResultsMultiHypotheses,
                                                       public = list(
                                                         thetaH1 = NULL,
                                                         initialize = function(..., thetaH1 = NULL) {
                                                           self$thetaH1 <- thetaH1
                                                           super$initialize(...)
                                                           
                                                           if (self$.readyForInitialization()) {
                                                             gMax <- self$getGMax()
                                                             if (is.null(self$thetaH1) || length(self$thetaH1) == 0 || all(is.na(self$thetaH1))) {
                                                               self$thetaH1 <- rep(NA_real_, gMax)
                                                             }
                                                           }
                                                         }
                                                       )
)

#'
#' @name ConditionalPowerResultsEnrichmentMeans
#'
#' @title
#' Conditional Power Results Enrichment Means
#' 
#' @description
#' Class for conditional power calculations of enrichment means data
#' 
#' @template field_nPlanned 
#' @template field_allocationRatioPlanned
#' @template field_iterations
#' @template field_seed
#' @template field_simulated
#' @template field_conditionalPower
#' @template field_thetaH1
#' @template field_assumedStDevs
#'
#' @details
#' This object cannot be created directly; use \code{\link{getConditionalPower}}
#' with suitable arguments to create the results of a group sequential or a combination test design.
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
ConditionalPowerResultsEnrichmentMeans <- R6Class("ConditionalPowerResultsEnrichmentMeans",
                                                      inherit = ConditionalPowerResultsMultiArmMeans
)

#'
#' @name ConditionalPowerResultsEnrichmentRates
#'
#' @title
#' Conditional Power Results Enrichment Rates
#' 
#' @description
#' Class for conditional power calculations of enrichment rates data
#' 
#' @template field_nPlanned 
#' @template field_allocationRatioPlanned
#' @template field_iterations
#' @template field_seed
#' @template field_simulated
#' @template field_conditionalPower
#' @template field_piTreatments
#' @template field_piControls
#'
#' @details
#' This object cannot be created directly; use \code{\link{getConditionalPower}}
#' with suitable arguments to create the results of a group sequential or a combination test design.
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
ConditionalPowerResultsEnrichmentRates <- R6Class("ConditionalPowerResultsEnrichmentRates",
                                                      inherit = ConditionalPowerResultsMultiHypotheses,
                                                      public = list(
                                                        piTreatments = NULL,
                                                        piControls = NULL,
                                                        initialize = function(..., piTreatments = NULL, piControls = NULL) {
                                                          self$piTreatments <- piTreatments
                                                          self$piControls <- piControls
                                                          super$initialize(...)
                                                          
                                                          if (self$.readyForInitialization()) {
                                                            gMax <- self$getGMax()
                                                            if (is.null(self$piControls) || length(self$piControls) == 0 || all(is.na(self$piControls))) {
                                                              self$piControls <- rep(NA_real_, gMax)
                                                            }
                                                            if (is.null(self$piTreatments) || length(self$piTreatments) == 0 || all(is.na(self$piTreatments))) {
                                                              self$piTreatments <- rep(NA_real_, gMax)
                                                            }
                                                          }
                                                        }
                                                      )
)


ConditionalPowerResultsEnrichmentSurvival <- R6Class("ConditionalPowerResultsEnrichmentSurvival",
                                                         inherit = ConditionalPowerResultsMultiArmSurvival
)

#'
#' @name ClosedCombinationTestResults
#'
#' @title
#' Analysis Results Closed Combination Test
#'
#' @description
#' Class for multi-arm analysis results based on a closed combination test.
#'
#' @template field_intersectionTest
#' @template field_indices
#' @template field_adjustedStageWisePValues
#' @template field_overallAdjustedTestStatistics
#' @template field_separatePValues
#' @template field_conditionalErrorRate
#' @template field_secondStagePValues
#' @template field_rejected
#' @template field_rejectedIntersections
#' 
#' @details
#' This object cannot be created directly; use \code{\link{getAnalysisResults}}
#' with suitable arguments to create the multi-arm analysis results of a closed combination test design.
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
ClosedCombinationTestResults <- R6Class("ClosedCombinationTestResults",
                                            inherit = ParameterSet,
                                            public = list(
                                              .plotSettings = NULL,
                                              .design = NULL,
                                              .enrichment = NULL,
                                              intersectionTest = NULL,
                                              indices = NULL,
                                              adjustedStageWisePValues = NULL,
                                              overallAdjustedTestStatistics = NULL,
                                              separatePValues = NULL,
                                              conditionalErrorRate = NULL,
                                              secondStagePValues = NULL,
                                              rejected = NULL,
                                              rejectedIntersections = NULL,
                                              initialize = function(..., .design = NULL,
                                                                    .enrichment = NULL,
                                                                    intersectionTest = NULL,
                                                                    indices = NULL,
                                                                    adjustedStageWisePValues = NULL,
                                                                    overallAdjustedTestStatistics = NULL,
                                                                    separatePValues = NULL,
                                                                    conditionalErrorRate = NULL,
                                                                    secondStagePValues = NULL,
                                                                    rejected = NULL,
                                                                    rejectedIntersections = NULL) {
                                                self$.design <- .design
                                                self$.enrichment <- .enrichment
                                                self$intersectionTest <- intersectionTest
                                                self$indices <- indices
                                                self$adjustedStageWisePValues <- adjustedStageWisePValues
                                                self$overallAdjustedTestStatistics <- overallAdjustedTestStatistics
                                                self$separatePValues <- separatePValues
                                                self$conditionalErrorRate <- conditionalErrorRate
                                                self$secondStagePValues <- secondStagePValues
                                                self$rejected <- rejected
                                                self$rejectedIntersections <- rejectedIntersections
                                                
                                                super$initialize(...)
                                                
                                                self$.plotSettings <- PlotSettings$new()
                                                
                                                self$.setParameterType("intersectionTest", C_PARAM_USER_DEFINED)
                                                
                                                parametersGenerated <- c(
                                                  "indices",
                                                  "separatePValues",
                                                  "rejected",
                                                  "rejectedIntersections"
                                                )
                                                if (inherits(self$.design, "TrialDesignConditionalDunnett")) {
                                                  parametersGenerated <- c(
                                                    parametersGenerated,
                                                    "conditionalErrorRate",
                                                    "secondStagePValues"
                                                  )
                                                } else {
                                                  parametersGenerated <- c(
                                                    parametersGenerated,
                                                    "adjustedStageWisePValues",
                                                    "overallAdjustedTestStatistics"
                                                  )
                                                }
                                                for (param in parametersGenerated) {
                                                  self$.setParameterType(param, C_PARAM_GENERATED)
                                                }
                                              },
                                              show = function(showType = 1, digits = NA_integer_) {
                                                self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
                                              },
                                              .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
                                                "Method for automatically printing closed combination test result objects"
                                                self$.resetCat()
                                                if (showType == 2) {
                                                  super$.show(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
                                                } else {
                                                  self$.cat(self$.toString(), ":\n\n",
                                                       heading = 1,
                                                       consoleOutputEnabled = consoleOutputEnabled
                                                  )
                                                  
                                                  self$.showParametersOfOneGroup(self$.getUserDefinedParameters(), "User defined parameters",
                                                                            orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                                                  )
                                                  
                                                  designParametersToShow <- c(
                                                    ".design$stages",
                                                    ".design$alpha"
                                                  )
                                                  if (inherits(self$.design, "TrialDesignConditionalDunnett")) {
                                                    designParametersToShow <- c(
                                                      designParametersToShow,
                                                      ".design$informationAtInterim",
                                                      ".design$secondStageConditioning"
                                                    )
                                                  }
                                                  self$.showParametersOfOneGroup(designParametersToShow, "Design parameters",
                                                                            orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                                                  )
                                                  
                                                  self$.showParametersOfOneGroup(self$.getGeneratedParameters(), "Output",
                                                                            orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                                                  )
                                                  
                                                  self$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
                                                  
                                                  self$.cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                                                  if (isTRUE(self$.enrichment)) {
                                                    self$.cat(paste0("  S[i]: population i\n"), consoleOutputEnabled = consoleOutputEnabled)
                                                    self$.cat(paste0("  F: full population\n"), consoleOutputEnabled = consoleOutputEnabled)
                                                  } else {
                                                    self$.cat(paste0(
                                                      "  (i): results of treatment arm i vs. control group ",
                                                      (nrow(self$separatePValues) + 1), "\n"
                                                    ), consoleOutputEnabled = consoleOutputEnabled)
                                                    self$.cat("  [i]: hypothesis number\n",
                                                         consoleOutputEnabled = consoleOutputEnabled
                                                    )
                                                  }
                                                }
                                              },
                                              .toString = function(startWithUpperCase = FALSE) {
                                                s <- "Closed combination test results"
                                                if (inherits(self$.design, "TrialDesignConditionalDunnett")) {
                                                  s <- paste0(s, " (Conditional Dunnett)")
                                                }
                                                return(s)
                                              },
                                              .getHypothesisTreatmentArms = function(number) {
                                                result <- c()
                                                for (i in 1:ncol(self$indices)) {
                                                  if (self$indices[number, i] == 1) {
                                                    result <- c(result, i)
                                                  }
                                                }
                                                return(result)
                                              },
                                              .getHypothesisTreatmentArmVariants = function() {
                                                result <- c()
                                                for (number in 1:nrow(self$indices)) {
                                                  arms <- self$.getHypothesisTreatmentArms(number)
                                                  result <- c(result, paste0(arms, collapse = ", "))
                                                }
                                                return(result)
                                              },
                                              .getHypothesisPopulationVariants = function() {
                                                result <- c()
                                                gMax <- 1
                                                for (number in 1:nrow(self$indices)) {
                                                  arms <- self$.getHypothesisTreatmentArms(number)
                                                  if (number == 1) {
                                                    gMax <- length(arms)
                                                  }
                                                  arms <- paste0("S", arms)
                                                  arms[arms == paste0("S", gMax)] <- "F"
                                                  result <- c(result, paste0(arms, collapse = ", "))
                                                }
                                                return(result)
                                              }
                                            )
)

#'
#' @name AnalysisResults
#'
#' @title
#' Basic Class for Analysis Results
#'
#' @description
#' A basic class for analysis results.
#'
#' @details
#' \code{AnalysisResults} is the basic class for
#' \itemize{
#'   \item \code{\link{AnalysisResultsFisher}},
#'   \item \code{\link{AnalysisResultsGroupSequential}},
#'   \item \code{\link{AnalysisResultsInverseNormal}},
#'   \item \code{\link{AnalysisResultsMultiArmFisher}},
#'   \item \code{\link{AnalysisResultsMultiArmInverseNormal}},
#'   \item \code{\link{AnalysisResultsConditionalDunnett}},
#'   \item \code{\link{AnalysisResultsEnrichmentFisher}},
#'   \item \code{\link{AnalysisResultsEnrichmentInverseNormal}}.
#' }
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_analysis_stage_results.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResults <- R6Class("AnalysisResults",
                               inherit = ParameterSet,
                               public = list(
                                 .plotSettings = NULL,
                                 .design = NULL,
                                 .dataInput = NULL,
                                 .stageResults = NULL,
                                 .conditionalPowerResults = NULL,
                                 normalApproximation = NULL,
                                 directionUpper = NULL,
                                 thetaH0 = NULL,
                                 pi1 = NULL,
                                 pi2 = NULL,
                                 nPlanned = NULL,
                                 allocationRatioPlanned = NULL,
                                 initialize = function(design, dataInput, ..., .stageResults = NULL, .conditionalPowerResults = NULL, directionUpper = NULL, thetaH0 = NULL) {
                                   self$.design <- design
                                   self$.dataInput <- dataInput
                                   self$.stageResults <- .stageResults
                                   self$.conditionalPowerResults <- .conditionalPowerResults
                                   self$directionUpper <- directionUpper
                                   self$thetaH0 <- thetaH0
                                   
                                   super$initialize(...)
                                   
                                   self$.plotSettings <- PlotSettings$new()
                                 },
                                 .setStageResults = function(stageResults) {
                                   self$.stageResults <- stageResults
                                 },
                                 getPlotSettings = function() {
                                   return(self$.plotSettings)
                                 },
                                 show = function(showType = 1, digits = NA_integer_) {
                                   self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
                                 },
                                 .getStageResultParametersToShow = function() {
                                   stageResultParametersToShow <- c()
                                   if (self$.design$kMax > 1) {
                                     if (!grepl("Rates", .getClassName(self$.dataInput)) || self$.dataInput$getNumberOfGroups() > 1) {
                                       stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$effectSizes")
                                     }
                                     
                                     if (grepl("Means", .getClassName(self$.dataInput))) {
                                       stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$overallStDevs")
                                     }
                                     if (grepl("Rates", .getClassName(self$.dataInput))) {
                                       if (.isMultiArmAnalysisResults(self)) {
                                         stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$overallPiTreatments")
                                         stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$overallPiControl")
                                       } else if (.isEnrichmentAnalysisResults(self)) {
                                         stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$overallPisTreatment")
                                         stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$overallPisControl")
                                       } else {
                                         stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$overallPi1")
                                         if (self$.dataInput$getNumberOfGroups() > 1) {
                                           stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$overallPi2")
                                         }
                                       }
                                     }
                                   }
                                   stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$testStatistics")
                                   if (grepl("(MultiArm|Dunnett|Enrichment)", .getClassName(self))) {
                                     stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$separatePValues")
                                   } else {
                                     stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$pValues")
                                   }
                                   
                                   if (self$.design$kMax == 1) {
                                     # return(stageResultParametersToShow)
                                   }
                                   
                                   # show combination test statistics
                                   if (.isTrialDesignInverseNormal(self$.design)) {
                                     stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$combInverseNormal")
                                   } else if (.isTrialDesignGroupSequential(self$.design)) {
                                     stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$overallTestStatistics")
                                     stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$overallPValues")
                                   } else if (.isTrialDesignFisher(self$.design)) {
                                     stageResultParametersToShow <- c(stageResultParametersToShow, ".stageResults$combFisher")
                                   }
                                   return(stageResultParametersToShow)
                                 },
                                 .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
                                   "Method for automatically printing analysis result objects"
                                   self$.resetCat()
                                   if (showType == 2) {
                                     super$.show(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
                                   } else {
                                     self$.cat(self$.toString(startWithUpperCase = TRUE), ":\n\n",
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
                                     
                                     self$.showParametersOfOneGroup(self$.getStageResultParametersToShow(), "Stage results",
                                                               orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                                     )
                                     
                                     # show multi-arm parameters
                                     if (grepl("(MultiArm|Dunnett|Enrichment)", .getClassName(self))) {
                                       if (.isTrialDesignConditionalDunnett(self$.design)) {
                                         self$.showParametersOfOneGroup(".closedTestResults$conditionalErrorRate",
                                                                   "Conditional error rate",
                                                                   orderByParameterName = FALSE,
                                                                   consoleOutputEnabled = consoleOutputEnabled
                                         )
                                         self$.showParametersOfOneGroup(".closedTestResults$secondStagePValues",
                                                                   "Second stage p-values",
                                                                   orderByParameterName = FALSE,
                                                                   consoleOutputEnabled = consoleOutputEnabled
                                         )
                                       } else {
                                         self$.showParametersOfOneGroup(".closedTestResults$adjustedStageWisePValues",
                                                                   "Adjusted stage-wise p-values",
                                                                   orderByParameterName = FALSE,
                                                                   consoleOutputEnabled = consoleOutputEnabled
                                         )
                                         self$.showParametersOfOneGroup(".closedTestResults$overallAdjustedTestStatistics",
                                                                   "Overall adjusted test statistics",
                                                                   orderByParameterName = FALSE,
                                                                   consoleOutputEnabled = consoleOutputEnabled
                                         )
                                       }
                                       
                                       self$.showParametersOfOneGroup(".closedTestResults$rejected", "Test actions",
                                                                 orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                                       )
                                     }
                                     
                                     generatedParams <- self$.getGeneratedParameters()
                                     generatedParams <- generatedParams[!(generatedParams %in%
                                                                            c("assumedStDevs", "thetaH1", "pi1", "pi2", "piTreatments", "piTreatments", "piControl", "piControls"))]
                                     
                                     if (grepl("(MultiArm|Dunnett|Enrichment)", .getClassName(self))) {
                                       if (all(c("conditionalPowerSimulated", "conditionalRejectionProbabilities") %in% generatedParams)) {
                                         generatedParams <- .moveValue(
                                             generatedParams,
                                             "conditionalPowerSimulated", "conditionalRejectionProbabilities"
                                         )
                                       }
                                       
                                       self$.showParametersOfOneGroup(generatedParams, "Further analysis results",
                                                                 orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                                       )
                                     } else {
                                       self$.showParametersOfOneGroup(generatedParams, "Analysis results",
                                                                 orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                                       )
                                     }
                                     
                                     self$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
                                     
                                     if (grepl("(MultiArm|Dunnett)", .getClassName(self))) {
                                       self$.cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                                       self$.cat(
                                         paste0(
                                         "  (i): results of treatment arm i vs. control group ",
                                         self$.dataInput$getNumberOfGroups(), "\n"
                                       ),
                                       consoleOutputEnabled = consoleOutputEnabled
                                       )
                                     } else if (.isEnrichmentAnalysisResults(self)) {
                                       self$.cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                                       self$.cat(paste0("  S[i]: population i\n"), consoleOutputEnabled = consoleOutputEnabled)
                                       self$.cat(paste0("  F: full population\n"), consoleOutputEnabled = consoleOutputEnabled)
                                     } else if (grepl("Rates", .getClassName(self$.dataInput)) && self$.dataInput$getNumberOfGroups() == 2) {
                                       self$.cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                                       self$.cat("  (i): values of treatment arm i\n", consoleOutputEnabled = consoleOutputEnabled)
                                     }
                                   }
                                 },
                                 .toString = function(startWithUpperCase = FALSE) {
                                   str <- "analysis results"
                                   if (inherits(self, "AnalysisResultsMultiArm")) {
                                     str <- paste0("multi-arm ", str)
                                   } else if (inherits(self, "AnalysisResultsEnrichment")) {
                                     str <- paste0("enrichment ", str)
                                   }
                                   if (startWithUpperCase) {
                                     str <- .firstCharacterToUpperCase(str)
                                   }
                                   
                                   numberOfGroups <- self$.dataInput$getNumberOfGroups()
                                   str <- paste0(str, " (")
                                   
                                   str <- paste0(str, tolower(sub("Dataset(Enrichment)?", "", .getClassName(self$.dataInput))))
                                   if (grepl("Survival", .getClassName(.getClassName))) {
                                     str <- paste0(str, " data")
                                   }
                                   
                                   if (numberOfGroups == 1) {
                                     str <- paste0(str, " of one group")
                                   } else {
                                     str <- paste0(str, " of ", numberOfGroups, " groups")
                                   }
                                   
                                   if (self$.design$kMax > 1) {
                                     if (grepl("GroupSequential", .getClassName(self))) {
                                       str <- paste0(str, ", group sequential design")
                                     } else if (grepl("InverseNormal", .getClassName(self))) {
                                       str <- paste0(str, ", inverse normal combination test design")
                                     } else if (grepl("Fisher", .getClassName(self))) {
                                       str <- paste0(str, ", Fisher's combination test design")
                                     } else if (grepl("Dunnett", .getClassName(self))) {
                                       str <- paste0(str, ", conditional Dunnett design")
                                     }
                                   } else {
                                     str <- paste0(str, ", fixed sample size design")
                                   }
                                   
                                   str <- paste0(str, ")")
                                   return(str)
                                 },
                                 getNumberOfStages = function() {
                                   return(self$.stageResults$getNumberOfStages())
                                 },
                                 getDataInput = function() {
                                   return(self$.dataInput)
                                 }
                               )
)

AnalysisResultsBase <- R6Class("AnalysisResultsBase",
                                   inherit = AnalysisResults,
                                   public = list(
                                     thetaH1 = NULL,
                                     assumedStDev = NULL,
                                     equalVariances = NULL,
                                     testActions = NULL,
                                     conditionalRejectionProbabilities = NULL,
                                     conditionalPower = NULL,
                                     repeatedConfidenceIntervalLowerBounds = NULL,
                                     repeatedConfidenceIntervalUpperBounds = NULL,
                                     repeatedPValues = NULL,
                                     finalStage = NULL,
                                     finalPValues = NULL,
                                     finalConfidenceIntervalLowerBounds = NULL,
                                     finalConfidenceIntervalUpperBounds = NULL,
                                     medianUnbiasedEstimates = NULL,
                                     initialize = function(design, dataInput, ..., thetaH1 = NULL,
                                                           assumedStDev = NULL,
                                                           equalVariances = NULL,
                                                           testActions = NULL,
                                                           conditionalRejectionProbabilities = NULL,
                                                           conditionalPower = NULL,
                                                           repeatedConfidenceIntervalLowerBounds = NULL,
                                                           repeatedConfidenceIntervalUpperBounds = NULL,
                                                           repeatedPValues = NULL,
                                                           finalStage = NULL,
                                                           finalPValues = NULL,
                                                           finalConfidenceIntervalLowerBounds = NULL,
                                                           finalConfidenceIntervalUpperBounds = NULL,
                                                           medianUnbiasedEstimates = NULL) {
                                       self$thetaH1 <- thetaH1
                                       self$assumedStDev <- assumedStDev
                                       self$equalVariances <- equalVariances
                                       self$testActions <- testActions
                                       self$conditionalRejectionProbabilities <- conditionalRejectionProbabilities
                                       self$conditionalPower <- conditionalPower
                                       self$repeatedConfidenceIntervalLowerBounds <- repeatedConfidenceIntervalLowerBounds
                                       self$repeatedConfidenceIntervalUpperBounds <- repeatedConfidenceIntervalUpperBounds
                                       self$repeatedPValues <- repeatedPValues
                                       self$finalStage <- finalStage
                                       self$finalPValues <- finalPValues
                                       self$finalConfidenceIntervalLowerBounds <- finalConfidenceIntervalLowerBounds
                                       self$finalConfidenceIntervalUpperBounds <- finalConfidenceIntervalUpperBounds
                                       self$medianUnbiasedEstimates <- medianUnbiasedEstimates
                                       
                                       super$initialize(design = design, dataInput = dataInput, ...)
                                       self$finalStage <- NA_integer_
                                     }
                                   )
)

#'
#' @name AnalysisResultsMultiHypotheses
#'
#' @title
#' Basic Class for Analysis Results Multi-Hypotheses
#'
#' @description
#' A basic class for multi-hypotheses analysis results.
#'
#' @details
#' \code{AnalysisResultsMultiHypotheses} is the basic class for
#' \itemize{
#'   \item \code{\link{AnalysisResultsMultiArm}} and
#'   \item \code{\link{AnalysisResultsEnrichment}}.
#' }
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_analysis_stage_results.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsMultiHypotheses <- R6Class("AnalysisResultsMultiHypotheses",
                                              inherit = AnalysisResults,
                                              public = list(
                                                .closedTestResults = NULL,
                                                thetaH1 = NULL, # means only
                                                assumedStDevs = NULL, # means only
                                                piTreatments = NULL, # rates only
                                                intersectionTest = NULL,
                                                varianceOption = NULL,
                                                conditionalRejectionProbabilities = NULL,
                                                conditionalPower = NULL,
                                                repeatedConfidenceIntervalLowerBounds = NULL,
                                                repeatedConfidenceIntervalUpperBounds = NULL,
                                                repeatedPValues = NULL,
                                                initialize = function(design, dataInput, ..., .closedTestResults = NULL,
                                                                      thetaH1 = NULL,
                                                                      assumedStDevs = NULL,
                                                                      piTreatments = NULL,
                                                                      intersectionTest = NULL,
                                                                      varianceOption = NULL,
                                                                      conditionalRejectionProbabilities = NULL,
                                                                      conditionalPower = NULL,
                                                                      repeatedConfidenceIntervalLowerBounds = NULL,
                                                                      repeatedConfidenceIntervalUpperBounds = NULL,
                                                                      repeatedPValues = NULL) {
                                                  self$.closedTestResults <- .closedTestResults
                                                  self$thetaH1 <- thetaH1
                                                  self$assumedStDevs <- assumedStDevs
                                                  self$piTreatments <- piTreatments
                                                  self$intersectionTest <- intersectionTest
                                                  self$varianceOption <- varianceOption
                                                  self$conditionalRejectionProbabilities <- conditionalRejectionProbabilities
                                                  self$conditionalPower <- conditionalPower
                                                  self$repeatedConfidenceIntervalLowerBounds <- repeatedConfidenceIntervalLowerBounds
                                                  self$repeatedConfidenceIntervalUpperBounds <- repeatedConfidenceIntervalUpperBounds
                                                  self$repeatedPValues <- repeatedPValues
                                                  super$initialize(design = design, dataInput = dataInput, ...)
                                            
                                                  for (param in c("thetaH1", "assumedStDevs", "piTreatments")) {
                                                    self$.setParameterType(param, C_PARAM_NOT_APPLICABLE)
                                                  }
                                                }
                                              )
)

#'
#' @name AnalysisResultsMultiArm
#'
#' @title
#' Basic Class for Analysis Results Multi-Arm
#'
#' @description
#' A basic class for multi-arm analysis results.
#'
#' @details
#' \code{AnalysisResultsMultiArm} is the basic class for
#' \itemize{
#'   \item \code{\link{AnalysisResultsMultiArmFisher}},
#'   \item \code{\link{AnalysisResultsMultiArmInverseNormal}}, and
#'   \item \code{\link{AnalysisResultsConditionalDunnett}}.
#' }
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_analysis_stage_results.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsMultiArm <- R6Class("AnalysisResultsMultiArm",
                                       inherit = AnalysisResultsMultiHypotheses,
                                       public = list(
                                         piControl = NULL, # rates only
                                         initialize = function(design, dataInput, ..., piControl = NULL) {
                                           self$piControl <- piControl
                                           super$initialize(design = design, dataInput = dataInput, ...)
                                           self$.setParameterType("piControl", C_PARAM_NOT_APPLICABLE)
                                         },
                                         .getParametersToShow = function() {
                                           parametersToShow <- self$.getVisibleFieldNames()
                                           
                                           if ("piTreatments" %in% parametersToShow && "piControl" %in% parametersToShow) {
                                             index <- which(parametersToShow == "piTreatments")
                                             parametersToShow <- parametersToShow[parametersToShow != "piControl"]
                                             parametersToShow <- c(
                                               parametersToShow[1:index],
                                               "piControl", parametersToShow[(index + 1):length(parametersToShow)]
                                             )
                                           }
                                           
                                           return(parametersToShow)
                                         }
                                       )
)

#'
#' @name AnalysisResultsEnrichment
#'
#' @title
#' Basic Class for Analysis Results Enrichment
#'
#' @description
#' A basic class for enrichment analysis results.
#'
#' @details
#' \code{AnalysisResultsEnrichment} is the basic class for
#' \itemize{
#'   \item \code{\link{AnalysisResultsEnrichmentFisher}} and
#'   \item \code{\link{AnalysisResultsEnrichmentInverseNormal}}.
#' }
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_analysis_stage_results.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsEnrichment <- R6Class("AnalysisResultsEnrichment",
                                         inherit = AnalysisResultsMultiHypotheses,
                                         public = list(
                                           piControls = NULL, # rates only
                                           initialize = function(design, dataInput, ..., piControls = NULL) {
                                             self$piControls <- piControls
                                             super$initialize(design = design, dataInput = dataInput, ...)
                                             self$.setParameterType("piControls", C_PARAM_NOT_APPLICABLE)
                                           }
                                         )
)

#'
#' @title
#' Analysis Results Summary
#'
#' @description
#' Displays a summary of \code{\link{AnalysisResults}} object.
#'
#' @param object An \code{\link{AnalysisResults}} object.
#' @inheritParams param_digits
#' @inheritParams param_three_dots
#'
#' @details
#' Summarizes the parameters and results of an analysis results object.
#'
#' @template details_summary
#'
#' @template return_object_summary
#' @template how_to_get_help_for_generics
#'
#' @export
#'
#' @keywords internal
#'
summary.AnalysisResults <- function(object, ..., type = 1, digits = NA_integer_) {
  return(summary.ParameterSet(object = object, ..., type = type, digits = digits))
}

#'
#' @title
#' Coerce AnalysisResults to a Data Frame
#'
#' @description
#' Returns the \code{\link{AnalysisResults}} object as data frame.
#'
#' @param x An \code{\link{AnalysisResults}} object created by \code{\link[=getAnalysisResults]{getAnalysisResults()}}.
#' @inheritParams param_niceColumnNamesEnabled
#' @inheritParams param_three_dots
#'
#' @details
#' Coerces the analysis results to a data frame.
#'
#' @template return_dataframe
#'
#' @export
#'
#' @keywords internal
#'
as.data.frame.AnalysisResults <- function(x, row.names = NULL, optional = FALSE, ...,
        niceColumnNamesEnabled = FALSE) {
    parametersToShow <- .getDesignParametersToShow(x)
    if (inherits(x, "AnalysisResultsMultiArm")) {
        parametersToShow <- c(parametersToShow, ".closedTestResults$rejected")
    }
    parametersToShow <- c(parametersToShow, x$.getUserDefinedParameters())
    parametersToShow <- c(parametersToShow, x$.getDefaultParameters())
    parametersToShow <- c(parametersToShow, x$.getStageResultParametersToShow())
    parametersToShow <- c(parametersToShow, x$.getGeneratedParameters())

    parametersToShow <- parametersToShow[!(parametersToShow %in% c(
        "finalStage", "allocationRatioPlanned", "thetaH0", "thetaH1", "pi1", "pi2"
    ))]
    return(.getAsDataFrame(
        parameterSet = x,
        parameterNames = parametersToShow,
        niceColumnNamesEnabled = niceColumnNamesEnabled
    ))
}

#'
#' @title
#' Names of a Analysis Results Object
#'
#' @description
#' Function to get the names of an \code{\link{AnalysisResults}} object.
#'
#' @param x An \code{\link{AnalysisResults}} object created by \code{\link[=getAnalysisResults]{getAnalysisResults()}}.
#'
#' @details
#' Returns the names of an analysis results that can be accessed by the user.
#'
#' @template return_names
#'
#' @export
#'
#' @keywords internal
#'
names.AnalysisResults <- function(x) {
  namesToShow <- c(".design", ".dataInput", ".stageResults", ".conditionalPowerResults")
  if (.isMultiArmAnalysisResults(x)) {
    namesToShow <- c(namesToShow, ".closedTestResults")
  }
  namesToShow <- c(namesToShow, x$.getVisibleFieldNames())
  return(namesToShow)
}

#'
#' @name AnalysisResultsGroupSequential
#'
#' @title
#' Analysis Results Group Sequential
#'
#' @description
#' Class for analysis results results based on a group sequential design.
#'
#' @template field_normalApproximation
#' @template field_directionUpper
#' @template field_thetaH0
#' @template field_pi1
#' @template field_pi2
#' @template field_nPlanned
#' @template field_allocationRatioPlanned
#' @template field_thetaH1
#' @template field_assumedStDev
#' @template field_equalVariances
#' @template field_testActions
#' @template field_conditionalRejectionProbabilities
#' @template field_conditionalPower
#' @template field_repeatedConfidenceIntervalLowerBounds
#' @template field_repeatedConfidenceIntervalUpperBounds
#' @template field_repeatedPValues
#' @template field_finalStage
#' @template field_finalPValues
#' @template field_finalConfidenceIntervalLowerBounds
#' @template field_finalConfidenceIntervalUpperBounds
#' @template field_medianUnbiasedEstimates
#' @template field_maxInformation
#' @template field_informationEpsilon
#' 
#' @details
#' This object cannot be created directly; use \code{\link{getAnalysisResults}}
#' with suitable arguments to create the analysis results of a group sequential design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsGroupSequential <- R6Class("AnalysisResultsGroupSequential",
                                              inherit = AnalysisResultsBase,
                                              public = list(
                                                maxInformation = NULL,
                                                informationEpsilon = NULL,
                                                initialize = function(design, dataInput, ..., maxInformation = NULL, informationEpsilon = NULL) {
                                                  self$maxInformation <- maxInformation
                                                  self$informationEpsilon <- informationEpsilon
                                                  
                                                  super$initialize(design = design, dataInput = dataInput, ...)
                                                  
                                                  self$.setParameterType("maxInformation", C_PARAM_NOT_APPLICABLE)
                                                  self$.setParameterType("informationEpsilon", C_PARAM_NOT_APPLICABLE)
                                                }
                                              )
)

#'
#' @name AnalysisResultsInverseNormal
#'
#' @title
#' Analysis Results Inverse Normal
#'
#' @description
#' Class for analysis results results based on an inverse normal design.
#'
#' @template field_normalApproximation
#' @template field_directionUpper
#' @template field_thetaH0
#' @template field_pi1
#' @template field_pi2
#' @template field_nPlanned
#' @template field_allocationRatioPlanned
#' @template field_thetaH1
#' @template field_assumedStDev
#' @template field_equalVariances
#' @template field_testActions
#' @template field_conditionalRejectionProbabilities
#' @template field_conditionalPower
#' @template field_repeatedConfidenceIntervalLowerBounds
#' @template field_repeatedConfidenceIntervalUpperBounds
#' @template field_repeatedPValues
#' @template field_finalStage
#' @template field_finalPValues
#' @template field_finalConfidenceIntervalLowerBounds
#' @template field_finalConfidenceIntervalUpperBounds
#' @template field_medianUnbiasedEstimates
#' 
#' @details
#' This object cannot be created directly; use \code{\link{getAnalysisResults}}
#' with suitable arguments to create the analysis results of a inverse normal design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsInverseNormal <- R6Class("AnalysisResultsInverseNormal",
                                            inherit = AnalysisResultsBase
)

#'
#' @name AnalysisResultsMultiArmInverseNormal
#'
#' @title
#' Analysis Results Multi-Arm Inverse Normal
#'
#' @description
#' Class for multi-arm analysis results based on a inverse normal design.
#'
#' @template field_normalApproximation
#' @template field_directionUpper
#' @template field_thetaH0
#' @template field_pi1
#' @template field_pi2
#' @template field_nPlanned
#' @template field_allocationRatioPlanned
#' @template field_thetaH1
#' @template field_assumedStDevs
#' @template field_piTreatments
#' @template field_intersectionTest
#' @template field_varianceOption
#' @template field_conditionalRejectionProbabilities
#' @template field_conditionalPower
#' @template field_repeatedConfidenceIntervalLowerBounds
#' @template field_repeatedConfidenceIntervalUpperBounds
#' @template field_repeatedPValues
#' @template field_piControl
#' 
#' @details
#' This object cannot be created directly; use \code{\link{getAnalysisResults}}
#' with suitable arguments to create the multi-arm analysis results of an inverse normal design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsMultiArmInverseNormal <- R6Class("AnalysisResultsMultiArmInverseNormal",
                                                    inherit = AnalysisResultsMultiArm
)

#'
#' @name AnalysisResultsEnrichmentInverseNormal
#'
#' @title
#' Analysis Results Enrichment Inverse Normal
#'
#' @description
#' Class for enrichment analysis results based on a inverse normal design.
#'
#' @template field_normalApproximation
#' @template field_directionUpper
#' @template field_thetaH0
#' @template field_pi1
#' @template field_pi2
#' @template field_nPlanned
#' @template field_allocationRatioPlanned
#' @template field_thetaH1
#' @template field_assumedStDevs
#' @template field_piTreatments
#' @template field_intersectionTest
#' @template field_varianceOption
#' @template field_conditionalRejectionProbabilities
#' @template field_conditionalPower
#' @template field_repeatedConfidenceIntervalLowerBounds
#' @template field_repeatedConfidenceIntervalUpperBounds
#' @template field_repeatedPValues
#' @template field_piControls
#' @template field_stratifiedAnalysis
#' 
#' @details
#' This object cannot be created directly; use \code{\link{getAnalysisResults}}
#' with suitable arguments to create the enrichment analysis results of an inverse normal design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsEnrichmentInverseNormal <- R6Class("AnalysisResultsEnrichmentInverseNormal",
                                                      inherit = AnalysisResultsEnrichment,
                                                      public = list(
                                                        stratifiedAnalysis = NULL
                                                      )
)

#'
#' @name AnalysisResultsFisher
#'
#' @title
#' Analysis Results Fisher
#'
#' @description
#' Class for analysis results based on a Fisher combination test design.
#'
#' @template field_normalApproximation
#' @template field_directionUpper
#' @template field_thetaH0
#' @template field_pi1
#' @template field_pi2
#' @template field_nPlanned
#' @template field_allocationRatioPlanned
#' @template field_thetaH1
#' @template field_assumedStDev
#' @template field_equalVariances
#' @template field_testActions
#' @template field_conditionalRejectionProbabilities
#' @template field_conditionalPower
#' @template field_repeatedConfidenceIntervalLowerBounds
#' @template field_repeatedConfidenceIntervalUpperBounds
#' @template field_repeatedPValues
#' @template field_finalStage
#' @template field_finalPValues
#' @template field_finalConfidenceIntervalLowerBounds
#' @template field_finalConfidenceIntervalUpperBounds
#' @template field_medianUnbiasedEstimates
#' @template field_conditionalPowerSimulated
#' @template field_iterations
#' @template field_seed
#' 
#' @details
#' This object cannot be created directly; use \code{\link{getAnalysisResults}}
#' with suitable arguments to create the analysis results of a Fisher combination test design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsFisher <- R6Class("AnalysisResultsFisher",
                                     inherit = AnalysisResultsBase,
                                     public = list(
                                       conditionalPowerSimulated = NULL,
                                       iterations = NULL,
                                       seed = NULL,
                                       initialize = function(design, dataInput, ..., iterations = NULL, seed = NULL) {
                                         self$iterations <- iterations 
                                         self$seed <- seed 
                                         super$initialize(design = design, dataInput = dataInput, ...)
                                         self$conditionalPowerSimulated <- -1
                                       }
                                     )
)

#'
#' @title
#' Analysis Results Multi-Arm Fisher
#'
#' @description
#' Class for multi-arm analysis results based on a Fisher combination test design.
#'
#' @template field_normalApproximation
#' @template field_directionUpper
#' @template field_thetaH0
#' @template field_pi1
#' @template field_pi2
#' @template field_nPlanned
#' @template field_allocationRatioPlanned
#' @template field_thetaH1
#' @template field_assumedStDevs
#' @template field_piTreatments
#' @template field_intersectionTest
#' @template field_varianceOption
#' @template field_conditionalRejectionProbabilities
#' @template field_conditionalPower
#' @template field_repeatedConfidenceIntervalLowerBounds
#' @template field_repeatedConfidenceIntervalUpperBounds
#' @template field_repeatedPValues
#' @template field_piControl
#' @template field_conditionalPowerSimulated
#' @template field_iterations
#' @template field_seed
#' 
#' @details
#' This object cannot be created directly; use \code{\link{getAnalysisResults}}
#' with suitable arguments to create the multi-arm analysis results of a Fisher combination test design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsMultiArmFisher <- R6Class("AnalysisResultsMultiArmFisher",
                                             inherit = AnalysisResultsMultiArm,
                                             public = list(
                                               conditionalPowerSimulated = NULL,
                                               iterations = NULL,
                                               seed = NULL
                                             )
)

#'
#' @name AnalysisResultsEnrichmentFisher
#'
#' @title
#' Analysis Results Enrichment Fisher
#'
#' @description
#' Class for enrichment analysis results based on a Fisher combination test design.
#'
#' @template field_normalApproximation
#' @template field_directionUpper
#' @template field_thetaH0
#' @template field_pi1
#' @template field_pi2
#' @template field_nPlanned
#' @template field_thetaH1
#' @template field_assumedStDevs
#' @template field_piTreatments
#' @template field_intersectionTest
#' @template field_varianceOption
#' @template field_conditionalRejectionProbabilities
#' @template field_repeatedConfidenceIntervalLowerBounds
#' @template field_repeatedConfidenceIntervalUpperBounds
#' @template field_repeatedPValues
#' @template field_piControls
#' @template field_conditionalPowerSimulated
#' @template field_iterations
#' @template field_seed
#' @template field_stratifiedAnalysis
#' 
#' 
#' @details
#' This object cannot be created directly; use \code{\link{getAnalysisResults}}
#' with suitable arguments to create the multi-arm analysis results of a Fisher combination test design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsEnrichmentFisher <- R6Class("AnalysisResultsEnrichmentFisher",
                                               inherit = AnalysisResultsEnrichment,
                                               public = list(
                                                 conditionalPowerSimulated = NULL,
                                                 iterations = NULL,
                                                 seed = NULL,
                                                 stratifiedAnalysis = NULL
                                               )
)

#'
#' @name AnalysisResultsConditionalDunnett
#'
#' @title
#' Analysis Results Multi-Arm Conditional Dunnett
#'
#' @description
#' Class for multi-arm analysis results based on a conditional Dunnett test design.
#'
#' @template field_normalApproximation
#' @template field_directionUpper
#' @template field_thetaH0
#' @template field_pi1
#' @template field_pi2
#' @template field_nPlanned
#' @template field_allocationRatioPlanned
#' @template field_thetaH1
#' @template field_assumedStDevs
#' @template field_piTreatments
#' @template field_intersectionTest
#' @template field_varianceOption
#' @template field_conditionalRejectionProbabilities
#' @template field_conditionalPower
#' @template field_repeatedConfidenceIntervalLowerBounds
#' @template field_repeatedConfidenceIntervalUpperBounds
#' @template field_repeatedPValues
#' @template field_piControl
#' 
#' @details
#' This object cannot be created directly; use \code{\link{getAnalysisResults}}
#' with suitable arguments to create the multi-arm analysis results of a conditional Dunnett test design.
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
AnalysisResultsConditionalDunnett <- R6Class("AnalysisResultsConditionalDunnett",
                                                 inherit = AnalysisResultsMultiArm,
                                                 public = list()
)

.getAnalysisResultsPlotArguments <- function(x,
                                             nPlanned = NA_real_, allocationRatioPlanned = NA_real_) {
  if (all(is.na(nPlanned))) {
    nPlanned <- stats::na.omit(x$nPlanned)
  }
  
  if (is.na(allocationRatioPlanned) && length(x$allocationRatioPlanned) == 1) {
    allocationRatioPlanned <- x$allocationRatioPlanned
  }
  
  if (length(allocationRatioPlanned) != 1) {
    allocationRatioPlanned <- NA_real_
  }
  
  if ((.isConditionalPowerEnabled(x$nPlanned) || .isConditionalPowerEnabled(nPlanned)) && is.na(allocationRatioPlanned)) {
    allocationRatioPlanned <- 1
  }
  
  return(list(
    stageResults = x$.stageResults,
    nPlanned = nPlanned,
    allocationRatioPlanned = allocationRatioPlanned
  ))
}

.getConfidenceIntervalPlotLegendLabels <- function(x, treatmentArmsToShow) {
  if (.isEnrichmentAnalysisResults(x)) {
    gMax <- x$.stageResults$getGMax()
    labels <- paste0("S", treatmentArmsToShow)
    labels[treatmentArmsToShow == gMax] <- "F"
    labels <- factor(labels, levels = unique(labels))
    return(labels)
  }
  
  return(paste0(treatmentArmsToShow, " vs control"))
}

.getConfidenceIntervalData <- function(x, treatmentArmsToShow = NULL) {
  data <- .getConfidenceIntervalDataPerBound(x, "lower", treatmentArmsToShow)
  data$upper <- .getConfidenceIntervalDataPerBound(x, "upper", treatmentArmsToShow)$upper
  data$yValues <- (data$upper + data$lower) / 2
  data <- na.omit(data)
  return(data)
}

.getConfidenceIntervalDataPerBound <- function(x, ciName = c("lower", "upper"), treatmentArmsToShow = NULL) {
  ciName <- match.arg(ciName)
  paramName <- ifelse(ciName == "lower", "repeatedConfidenceIntervalLowerBounds", "repeatedConfidenceIntervalUpperBounds")
  data <- x[[paramName]]
  
  if (is.matrix(data) && !is.null(treatmentArmsToShow) &&
      length(treatmentArmsToShow) > 0 && !any(is.na(treatmentArmsToShow))) {
    data <- data[treatmentArmsToShow, ]
  }
  
  if (is.matrix(data) && nrow(data) == 1) {
    data <- as.numeric(data)
  }
  
  if (is.matrix(data)) {
    kMax <- ncol(data)
    if (is.null(treatmentArmsToShow) || length(treatmentArmsToShow) == 0 || all(is.na(treatmentArmsToShow))) {
      treatmentArmsToShow <- 1:nrow(data)
    }
    groups <- length(treatmentArmsToShow)
    result <- data.frame(ci = data[, 1])
    colnames(result) <- ciName
    result$xValues <- rep(1, groups)
    result$categories <- .getConfidenceIntervalPlotLegendLabels(x, treatmentArmsToShow)
    if (kMax == 1) {
      return(result)
    }
    
    for (stage in 2:kMax) {
      resultPart <- data.frame(ci = data[, stage])
      colnames(resultPart) <- ciName
      resultPart$xValues <- rep(stage, groups)
      resultPart$categories <- .getConfidenceIntervalPlotLegendLabels(x, treatmentArmsToShow)
      result <- rbind(result, resultPart)
    }
    return(result)
  }
  
  if (is.null(treatmentArmsToShow) || length(treatmentArmsToShow) == 0 || all(is.na(treatmentArmsToShow))) {
    treatmentArmsToShow <- 1
  }
  
  kMax <- length(data)
  result <- data.frame(ci = data)
  colnames(result) <- ciName
  result$xValues <- 1:kMax
  result$categories <- rep(.getConfidenceIntervalPlotLegendLabels(x, treatmentArmsToShow), kMax)
  return(result)
}

#'
#' @title
#' Analysis Results Plotting
#'
#' @description
#' Plots the conditional power together with the likelihood function.
#'
#' @param x The analysis results at given stage, obtained from \code{\link[=getAnalysisResults]{getAnalysisResults()}}.
#' @param y Not available for this kind of plot (is only defined to be compatible to the generic plot function).
#' @inheritParams param_nPlanned
#' @inheritParams param_stage
#' @inheritParams param_allocationRatioPlanned
#' @param main The main title, default is \code{"Dataset"}.
#' @param xlab The x-axis label, default is \code{"Stage"}.
#' @param ylab The y-axis label.
#' @param legendTitle The legend title, default is \code{""}.
#' @inheritParams param_palette
#' @inheritParams param_showSource
#' @inheritParams param_plotSettings
#' @inheritParams param_legendPosition
#' @inheritParams param_grid
#' @param type The plot type (default = 1). Note that at the moment only one type (the conditional power plot) is available.
#' @param ... Optional \link[=param_three_dots_plot]{plot arguments}. Furthermore the following arguments can be defined:
#' \itemize{
#' \item \code{thetaRange}: A range of assumed effect sizes if testing means or a survival design was specified.
#'       Additionally, if testing means was selected, \code{assumedStDev} (assumed standard deviation)
#'       can be specified (default is \code{1}).
#' \item \code{piTreatmentRange}: A range of assumed rates pi1 to calculate the conditional power.
#'       Additionally, if a two-sample comparison was selected, \code{pi2} can be specified (default is the value from
#'       \code{\link[=getAnalysisResults]{getAnalysisResults()}}).
#' \item \code{directionUpper}: Specifies the direction of the alternative,
#'       only applicable for one-sided testing; default is \code{TRUE}
#'       which means that larger values of the test statistics yield smaller p-values.
#' \item \code{\link[=param_thetaH0]{thetaH0}}: The null hypothesis value, default is \code{0} for
#'       the normal and the binary case, it is \code{1} for the survival case.
#'       For testing a rate in one sample, a value thetaH0 in (0, 1) has to be specified for
#'       defining the null hypothesis H0: \code{pi = thetaH0}.
#' }
#'
#' @details
#' The conditional power is calculated only if effect size and sample size is specified.
#'
#' @template return_object_ggplot
#'
#' @template examples_plot_analysis_results
#'
#' @export
#'
plot.AnalysisResults <- function(x, y, ..., type = 1L,
                                 nPlanned = NA_real_,
                                 allocationRatioPlanned = NA_real_,
                                 main = NA_character_, xlab = NA_character_, ylab = NA_character_,
                                 legendTitle = NA_character_, palette = "Set1", legendPosition = NA_integer_,
                                 showSource = FALSE, grid = 1, plotSettings = NULL) {
  .assertGgplotIsInstalled()
  functionCall <- match.call(expand.dots = TRUE)
  analysisResultsName <- as.character(functionCall$x)[1]
  .assertIsSingleInteger(grid, "grid", validateType = FALSE)
  typeNumbers <- .getPlotTypeNumber(type, x)
  p <- NULL
  plotList <- list()
  for (typeNumber in typeNumbers) {
    p <- .plotAnalysisResults(
      x = x, y = y, type = typeNumber,
      nPlanned = nPlanned,
      allocationRatioPlanned = allocationRatioPlanned,
      main = main, xlab = xlab, ylab = ylab,
      legendTitle = legendTitle, palette = palette, legendPosition = legendPosition,
      showSource = showSource, functionCall = functionCall,
      analysisResultsName = analysisResultsName, plotSettings = plotSettings, ...
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
  
  if (.isSpecialPlotShowSourceArgument(showSource)) {
    return(invisible(plotList))
  }
  
  return(.createPlotResultObject(plotList, grid))
}

.plotAnalysisResultsRCI <- function(...,
                                    x, y, nPlanned, allocationRatioPlanned, main, xlab, ylab,
                                    legendTitle, palette, legendPosition, showSource, analysisResultsName, plotSettings = NULL) {
  .assertIsAnalysisResults(x)
  .warnInCaseOfUnknownArguments(functionName = "plot", ignore = c("treatmentArms", "populations"), ...)
  
  if (.isEnrichmentAnalysisResults(x)) {
    gMax <- x$.stageResults$getGMax()
    treatmentArmsToShow <- .getPopulationsToShow(x, gMax = gMax, ...)
  } else {
    treatmentArmsToShow <- .getTreatmentArmsToShow(x, ...)
  }
  
  data <- .getConfidenceIntervalData(x, treatmentArmsToShow)
  if (nrow(data) == 0) {
    stop(
      C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
      "unable to create plot because no RCIs are available in the specified analysis result"
    )
  }
  
  .warnInCaseOfUnusedArgument(nPlanned, "nPlanned", NA_real_, "plot")
  .warnInCaseOfUnusedArgument(allocationRatioPlanned, "allocationRatioPlanned", NA_real_, "plot")
  
  plotData <- list(
    main = "Repeated Confidence Intervals",
    xlab = "Stage",
    ylab = "RCI",
    sub = NA_character_ # subtitle
  )
  
  if (is.na(legendPosition)) {
    if (!.isMultiHypothesesAnalysisResults(x)) {
      legendPosition <- ifelse(length(treatmentArmsToShow) == 1 && treatmentArmsToShow == 1,
                               -1, C_POSITION_RIGHT_CENTER
      )
    } else {
      legendPosition <- C_POSITION_RIGHT_TOP
    }
  }
  
  treatmentArmsToShowCmd <- ""
  if (!is.null(treatmentArmsToShow) && !identical(sort(unique(treatmentArmsToShow)), 1:nrow(data))) {
    treatmentArmsToShowCmd <- paste0(", ", .arrayToString(treatmentArmsToShow, mode = "vector"))
  }
  dataCmd <- paste0("rpact:::.getConfidenceIntervalData(", analysisResultsName, treatmentArmsToShowCmd, ")")
  srcCmd <- .showPlotSourceInformation(
    objectName = analysisResultsName,
    xParameterName = paste0(dataCmd, "$xValues"),
    yParameterNames = c(
      paste0(dataCmd, "$lower"),
      paste0(dataCmd, "$yValues"),
      paste0(dataCmd, "$upper")
    ),
    type = 2L, showSource = showSource, lineType = FALSE
  )
  
  p <- .createAnalysisResultsPlotObject(x,
                                        data = data, plotData = plotData, main = main, xlab = xlab, ylab = ylab,
                                        legendTitle = legendTitle, palette = palette, legendPosition = legendPosition,
                                        kMax = x$.design$kMax, plotSettings = plotSettings
  )
  p <- p + ggplot2::expand_limits(x = c(1, x$.design$kMax))
  return(p)
}

.plotAnalysisResults <- function(...,
                                 x, y, type, nPlanned, allocationRatioPlanned, main, xlab, ylab,
                                 legendTitle, palette, legendPosition, showSource, functionCall,
                                 analysisResultsName, plotSettings = NULL) {
  .assertIsSingleInteger(type, "type", naAllowed = FALSE, validateType = FALSE)
  if (!(type %in% c(1, 2))) {
    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, ") is not allowed; must be 1 or 2")
  }
  
  .assertIsAnalysisResults(x)
  .assertIsValidLegendPosition(legendPosition = legendPosition)
  
  if (type == 2) {
    return(.plotAnalysisResultsRCI(
      x = x, y = y, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned,
      main = main, xlab = xlab, ylab = ylab,
      legendTitle = legendTitle, palette = palette,
      legendPosition = legendPosition, showSource = showSource,
      analysisResultsName = analysisResultsName,
      plotSettings = plotSettings, ...
    ))
  }
  
  if (!.isConditionalPowerEnabled(x$nPlanned) && !.isConditionalPowerEnabled(nPlanned)) {
    stop("'nPlanned' must be defined to create conditional power plot")
  }
  
  .warnInCaseOfUnknownArguments(
    functionName = "plot",
    ignore = c("thetaRange", "assumedStDev", "assumedStDevs", "treatmentArms", "populations", "pi2", "piTreatmentRange"),
    ...
  )
  
  if (is.na(legendPosition)) {
    legendPosition <- C_POSITION_RIGHT_CENTER
  }
  
  plotArgs <- .getAnalysisResultsPlotArguments(
    x = x, nPlanned = nPlanned,
    allocationRatioPlanned = allocationRatioPlanned
  )
  
  functionCall$x <- x$.stageResults
  functionCall$y <- NULL
  functionCall$stageResultsName <- paste0(analysisResultsName, "$.stageResults")
  functionCall$nPlanned <- plotArgs$nPlanned
  functionCall$main <- main
  functionCall$xlab <- xlab
  functionCall$ylab <- ylab
  functionCall$legendTitle <- legendTitle
  functionCall$palette <- palette
  functionCall$legendPosition <- legendPosition
  functionCall$type <- type
  functionCall$plotSettings <- plotSettings
  functionCall$allocationRatioPlanned <- plotArgs$allocationRatioPlanned
  if (.isTrialDesignFisher(x$.design)) {
    functionCall$iterations <- x$iterations
    functionCall$seed <- x$seed
  }
  
  if (x$getDataInput()$isDatasetMeans()) {
    if (.isMultiHypothesesAnalysisResults(x)) {
      assumedStDevs <- eval.parent(functionCall$assumedStDevs)
      if (is.null(assumedStDevs)) {
        assumedStDevs <- as.numeric(x$assumedStDevs)
      }
      
      gMax <- x$.stageResults$getGMax()
      .assertIsValidAssumedStDevs(assumedStDevs, gMax)
      
      functionCall$assumedStDevs <- assumedStDevs
    } else {
      assumedStDev <- eval.parent(functionCall$assumedStDev)
      if (is.null(assumedStDev)) {
        assumedStDev <- x$assumedStDev
      }
      functionCall$assumedStDev <- assumedStDev
    }
  }
  
  if (x$getDataInput()$isDatasetMeans() || x$getDataInput()$isDatasetSurvival()) {
    thetaRange <- eval.parent(functionCall$thetaRange)
    if (is.null(thetaRange)) {
      thetaRangeMin <- min(x$thetaH0, min(na.omit(as.numeric(x$thetaH1))))
      thetaRangeMax <- 2 * max(x$thetaH0, max(na.omit(as.numeric(x$thetaH1))))
      thetaRange <- seq(
        thetaRangeMin, thetaRangeMax,
        (thetaRangeMax - thetaRangeMin) / C_THETA_RANGE_SEQUENCE_LENGTH_DEFAULT
      )
    } else {
      thetaRange <- .assertIsValidThetaRange(
        thetaRange = thetaRange,
        survivalDataEnabled = x$getDataInput()$isDatasetSurvival()
      )
    }
    functionCall$thetaRange <- thetaRange
  } else if (x$getDataInput()$isDatasetRates()) {
    if (.isMultiArmAnalysisResults(x)) {
      piControl <- eval.parent(functionCall$piControl)
      if (is.null(piControl)) {
        piControl <- as.numeric(x$piControl)
      }
      functionCall$piControl <- piControl
    } else if (.isEnrichmentAnalysisResults(x)) {
      piControl <- eval.parent(functionCall$piControl)
      if (is.null(piControl)) {
        piControls <- as.numeric(x$piControls)
      }
      functionCall$piControls <- piControls
    } else {
      pi2 <- eval.parent(functionCall$pi2)
      if (is.null(pi2)) {
        pi2 <- x$pi2
      }
      functionCall$pi2 <- pi2
    }
    
    piTreatmentRange <- eval.parent(functionCall$piTreatmentRange)
    if (is.null(piTreatmentRange)) {
      piTreatmentRange <- seq(0, 1, 1 / C_THETA_RANGE_SEQUENCE_LENGTH_DEFAULT) # default
    } else {
      piTreatmentRange <- .assertIsValidPiTreatmentRange(piTreatmentRange = piTreatmentRange)
    }
    functionCall$piTreatmentRange <- piTreatmentRange
  }
  
  functionCall[[1L]] <- as.name("plot")
  return(eval.parent(functionCall))
}
