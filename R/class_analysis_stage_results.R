library("R6")
## |
## |  *Stage results classes*
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
## |  File version: $Revision: 7645 $
## |  Last changed: $Date: 2024-02-16 16:12:34 +0100 (Fr, 16 Feb 2024) $
## |  Last changed by: $Author: pahlke $
## |

#'
#' @name StageResults
#'
#' @title
#' Basic Stage Results
#'
#' @description
#' Basic class for stage results.
#'
#' @template field_stages
#' @template field_testStatistics
#' @template field_pValues 
#' @template field_combInverseNormal
#' @template field_combFisher
#' @template field_effectSizes
#' @template field_testActions
#' @template field_weightsFisher
#' @template field_weightsInverseNormal
#'
#' @details
#' \code{StageResults} is the basic class for
#' \itemize{
#'   \item \code{\link{StageResultsMeans}},
#'   \item \code{\link{StageResultsRates}},
#'   \item \code{\link{StageResultsSurvival}},
#'   \item \code{\link{StageResultsMultiArmMeans}},
#'   \item \code{\link{StageResultsMultiArmRates}},
#'   \item \code{\link{StageResultsMultiArmSurvival}},
#'   \item \code{\link{StageResultsEnrichmentMeans}},
#'   \item \code{\link{StageResultsEnrichmentRates}}, and
#'   \item \code{\link{StageResultsEnrichmentSurvival}}.
#' }
#'
#' @include f_core_utilities.R
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_analysis_dataset.R
#' @include f_core_constants.R
#' @include class_core_plot_settings.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
StageResults <- R6Class("StageResults",
                            inherit = ParameterSet,
                            public = list(
                              .plotSettings = NULL,
                              .design = NULL,
                              .dataInput = NULL,
                              stage = NULL,
                              stages = NULL,
                              pValues = NULL,
                              weightsFisher = NULL,
                              weightsInverseNormal = NULL,
                              thetaH0 = NULL,
                              direction = NULL,
                              initialize = function(..., stage = NULL, stages = NULL, pValues = NULL, weightsFisher = NULL, weightsInverseNormal = NULL, thetaH0 = NULL, direction = NULL) {
                                self$stage <- stage 
                                self$stages <- stages 
                                self$pValues <- pValues 
                                self$weightsFisher <- weightsFisher 
                                self$weightsInverseNormal <- weightsInverseNormal 
                                self$thetaH0 <- thetaH0 
                                self$direction <- direction 
                                super$initialize(...)
                              },
                              init = function(design, dataInput) {
                                self$.design <- design
                                self$.dataInput <- dataInput
                                
                                self$.plotSettings <- PlotSettings$new()
                                if (!missing(design)) {
                                  self$stages <- c(1:design$kMax)
                                  if (design$kMax == C_KMAX_DEFAULT) {
                                    self$.setParameterType("stages", C_PARAM_DEFAULT_VALUE)
                                  } else {
                                    self$.setParameterType("stages", C_PARAM_USER_DEFINED)
                                  }
                                  self$.parameterNames <- .getParameterNames(design = design)
                                }
                                self$.parameterFormatFunctions <- C_PARAMETER_FORMAT_FUNCTIONS
                                
                                self$.setParameterType("stage", C_PARAM_NOT_APPLICABLE)
                                
                                self$.setParameterType("pValues", ifelse(
                                  self$.isMultiArm(), C_PARAM_NOT_APPLICABLE, C_PARAM_GENERATED
                                ))
                                self$.setParameterType("thetaH0", ifelse(
                                  identical(self$thetaH0, C_THETA_H0_MEANS_DEFAULT), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                                ))
                                self$.setParameterType("direction", ifelse(
                                  identical(self$direction, C_DIRECTION_UPPER), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                                ))
                              },
                              getPlotSettings = function() {
                                return(self$.plotSettings)
                              },
                              show = function(showType = 1, digits = NA_integer_) {
                                self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
                              },
                              .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
                                "Method for automatically printing stage results"
                                self$.resetCat()
                                if (showType == 2) {
                                  super$.show(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
                                } else {
                                  self$.cat(self$.toString(startWithUpperCase = TRUE), ":\n\n",
                                       heading = 1,
                                       consoleOutputEnabled = consoleOutputEnabled
                                  )
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
                                  
                                  if (grepl("Enrichment", .getClassName(self))) {
                                    self$.cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                                    self$.cat(paste0("  S[i]: population i\n"), consoleOutputEnabled = consoleOutputEnabled)
                                    self$.cat(paste0("  F: full population\n"), consoleOutputEnabled = consoleOutputEnabled)
                                  } else if (grepl("MultiArm", .getClassName(self))) {
                                    self$.cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                                    self$.cat(paste0(
                                      "  (i): results of treatment arm i vs. control group ",
                                      self$.dataInput$getNumberOfGroups(), "\n"
                                    ),
                                    consoleOutputEnabled = consoleOutputEnabled
                                    )
                                  } else if (self$.dataInput$getNumberOfGroups(survivalCorrectionEnabled = FALSE) >= 2) {
                                    self$.cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                                    self$.cat("  (i): values of treatment arm i\n", consoleOutputEnabled = consoleOutputEnabled)
                                  }
                                }
                              },
                              isDirectionUpper = function() {
                                return(self$direction == C_DIRECTION_UPPER)
                              },
                              .isMultiArm = function() {
                                return(grepl("multi", tolower(.getClassName(self))))
                              },
                              .isEnrichment = function() {
                                return(grepl("enrichment", tolower(.getClassName(self))))
                              },
                              getGMax = function() {
                                if (!is.matrix(self$testStatistics)) {
                                  return(1L)
                                }
                                
                                gMax <- nrow(self$testStatistics)
                                if (is.null(gMax) || gMax == 0) {
                                  gMax <- 1L
                                }
                                return(gMax)
                              },
                              .getParametersToShow = function() {
                                return(c("stages"))
                              },
                              .toString = function(startWithUpperCase = FALSE) {
                                s <- "stage results of"
                                
                                if (grepl("MultiArm", .getClassName(self))) {
                                  s <- paste(s, "multi-arm")
                                } else if (grepl("Enrichment", .getClassName(self))) {
                                  s <- paste(s, "enrichment")
                                }
                                
                                if (grepl("Means", .getClassName(self))) {
                                  s <- paste(s, "means")
                                }
                                
                                if (grepl("Rates", .getClassName(self))) {
                                  s <- paste(s, "rates")
                                }
                                
                                if (grepl("Survival", .getClassName(self))) {
                                  s <- paste(s, "survival data")
                                }
                                
                                if (startWithUpperCase) {
                                  s <- .firstCharacterToUpperCase(s)
                                }
                                
                                return(s)
                              },
                              getDataInput = function() {
                                return(self$.dataInput)
                              },
                              getNumberOfGroups = function() {
                                return(self$.dataInput$getNumberOfGroups())
                              },
                              isOneSampleDataset = function() {
                                return(self$getNumberOfGroups() == 1)
                              },
                              isTwoSampleDataset = function() {
                                return(self$getNumberOfGroups() == 2)
                              },
                              isDatasetMeans = function() {
                                return(self$.dataInput$isDatasetMeans())
                              },
                              isDatasetRates = function() {
                                return(self$.dataInput$isDatasetRates())
                              },
                              isDatasetSurvival = function() {
                                return(self$.dataInput$isDatasetSurvival())
                              },
                              getNumberOfStages = function() {
                                if (self$.isMultiArm()) {
                                  if (inherits(self, "StageResultsMultiArmRates")) {
                                    return(max(
                                      ncol(stats::na.omit(self$testStatistics)),
                                      ncol(stats::na.omit(self$separatePValues))
                                    ))
                                  }
                                  return(max(
                                    ncol(stats::na.omit(self$effectSizes)),
                                    ncol(stats::na.omit(self$separatePValues))
                                  ))
                                }
                                return(max(
                                  length(stats::na.omit(self$effectSizes)),
                                  length(stats::na.omit(self$pValues))
                                ))
                              }
                            )
)

#'
#' @name StageResultsMeans
#'
#' @title
#' Stage Results of Means
#'
#' @description
#' Class for stage results of means.
#'
#' @template field_stages
#' @template field_testStatistics
#' @template field_overallTestStatistics
#' @template field_pValues
#' @template field_overallPValues
#' @template field_effectSizes
#' @template field_testActions
#' @template field_direction
#' @template field_normalApproximation
#' @template field_equalVariances
#' @template field_combFisher
#' @template field_weightsFisher
#' @template field_combInverseNormal
#' @template field_weightsInverseNormal
#' @field ... Names of \code{dataInput}.
#' 
#' @details
#' This object cannot be created directly; use \code{getStageResults}
#' with suitable arguments to create the stage results of a dataset of means.
#'
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_analysis_dataset.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
StageResultsMeans <- R6Class("StageResultsMeans",
                                 inherit = StageResults,
                                 public = list(
                                   combInverseNormal = NULL,
                                   combFisher = NULL,
                                   overallTestStatistics = NULL,
                                   overallPValues = NULL,
                                   effectSizes = NULL,
                                   testStatistics = NULL,
                                   overallMeans = NULL,
                                   overallMeans1 = NULL,
                                   overallMeans2 = NULL,
                                   overallStDevs = NULL,
                                   overallStDevs1 = NULL,
                                   overallStDevs2 = NULL,
                                   overallSampleSizes = NULL,
                                   overallSampleSizes1 = NULL,
                                   overallSampleSizes2 = NULL,
                                   equalVariances = NULL,
                                   normalApproximation = NULL,
                                   initialize = function(design, dataInput, ...,
                                                         combInverseNormal = NULL,
                                                         combFisher = NULL,
                                                         overallTestStatistics = NULL,
                                                         overallPValues = NULL,
                                                         effectSizes = NULL,
                                                         testStatistics = NULL,
                                                         overallMeans = NULL,
                                                         overallMeans1 = NULL,
                                                         overallMeans2 = NULL,
                                                         overallStDevs = NULL,
                                                         overallStDevs1 = NULL,
                                                         overallStDevs2 = NULL,
                                                         overallSampleSizes = NULL,
                                                         overallSampleSizes1 = NULL,
                                                         overallSampleSizes2 = NULL,
                                                         equalVariances = TRUE, normalApproximation = FALSE) {
                                     super$initialize(.design = design, .dataInput = dataInput, ...)#TODO
                                     
                                     self$combInverseNormal <- combInverseNormal
                                     self$combFisher <- combFisher
                                     self$overallTestStatistics <- overallTestStatistics
                                     self$overallPValues <- overallPValues
                                     self$effectSizes <- effectSizes
                                     self$testStatistics <- testStatistics
                                     self$overallMeans <- overallMeans
                                     self$overallMeans1 <- overallMeans1
                                     self$overallMeans2 <- overallMeans2
                                     self$overallStDevs <- overallStDevs
                                     self$overallStDevs1 <- overallStDevs1
                                     self$overallStDevs2 <- overallStDevs2
                                     self$overallSampleSizes <- overallSampleSizes
                                     self$overallSampleSizes1 <- overallSampleSizes1
                                     self$overallSampleSizes2 <- overallSampleSizes2
                                     
                                     self$equalVariances <- equalVariances
                                     self$normalApproximation <- normalApproximation
                                     
                                     self$init(design = design, dataInput = dataInput)
                                     
                                     for (param in c(
                                       "weightsFisher",
                                       "weightsInverseNormal",
                                       "combFisher",
                                       "combInverseNormal"
                                     )) {
                                       self$.setParameterType(param, C_PARAM_NOT_APPLICABLE)
                                     }
                                     
                                     for (param in self$.getParametersToShow()) {
                                       if (self$.getParameterType(param) == C_PARAM_TYPE_UNKNOWN) {
                                         self$.setParameterType(param, C_PARAM_GENERATED)
                                       }
                                     }
                                     
                                     self$.setParameterType("equalVariances", ifelse(
                                       identical(self$equalVariances, TRUE), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                                     ))
                                     self$.setParameterType("normalApproximation", ifelse(
                                       identical(self$normalApproximation, FALSE), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                                     ))
                                   },
                                   .getParametersToShow = function() {
                                     parametersToShow <- c(
                                       "stages",
                                       "overallTestStatistics",
                                       "overallPValues"
                                     )
                                     if (self$.dataInput$getNumberOfGroups() == 1) {
                                       parametersToShow <- c(
                                         parametersToShow,
                                         "overallMeans",
                                         "overallStDevs",
                                         "overallSampleSizes"
                                       )
                                     } else if (self$.dataInput$getNumberOfGroups() == 2) {
                                       parametersToShow <- c(
                                         parametersToShow,
                                         "overallMeans1",
                                         "overallMeans2",
                                         "overallStDevs1",
                                         "overallStDevs2",
                                         "overallSampleSizes1",
                                         "overallSampleSizes2"
                                       )
                                     }
                                     parametersToShow <- c(
                                       parametersToShow,
                                       "testStatistics",
                                       "pValues",
                                       "effectSizes"
                                     )
                                     if (.isTrialDesignInverseNormal(self$.design)) {
                                       parametersToShow <- c(
                                         parametersToShow,
                                         "combInverseNormal",
                                         "weightsInverseNormal"
                                       )
                                     } else if (.isTrialDesignFisher(self$.design)) {
                                       parametersToShow <- c(
                                         parametersToShow,
                                         "combFisher",
                                         "weightsFisher"
                                       )
                                     }
                                     parametersToShow <- c(
                                       parametersToShow,
                                       "thetaH0",
                                       "direction",
                                       "normalApproximation"
                                     )
                                     if (self$.dataInput$getNumberOfGroups() == 2) {
                                       parametersToShow <- c(
                                         parametersToShow,
                                         "equalVariances"
                                       )
                                     }
                                     return(parametersToShow)
                                   }
                                 )
)

#' @name StageResultsMultiArmMeans
#'
#' @title
#' Stage Results Multi Arm Means
#'
#' @description
#' Class for stage results of multi arm means data
#'
#' @template field_stages
#' @template field_testStatistics
#' @template field_pValues 
#' @template field_combInverseNormal
#' @template field_combFisher
#' @template field_effectSizes
#' @template field_testActions
#' @template field_weightsFisher
#' @template field_weightsInverseNormal
#' @template field_combInverseNormal
#' @template field_combFisher
#' @template field_overallTestStatistics
#' @template field_overallStDevs
#' @template field_overallPooledStDevs
#' @template field_overallPValues
#' @template field_testStatistics
#' @template field_separatePValues
#' @template field_effectSizes
#' @template field_singleStepAdjustedPValues
#' @template field_intersectionTest
#' @template field_varianceOption
#' @template field_normalApproximation
#' @template field_directionUpper
#'
#' @details
#' This object cannot be created directly; use \code{getStageResults}
#' with suitable arguments to create the stage results of a dataset of multi arm means.
#' 
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_analysis_dataset.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
StageResultsMultiArmMeans <- R6Class("StageResultsMultiArmMeans",
                                         inherit = StageResults,
                                         public = list(
                                           combInverseNormal = NULL,
                                           combFisher = NULL,
                                           overallTestStatistics = NULL,
                                           overallStDevs = NULL,
                                           overallPooledStDevs = NULL,
                                           overallPValues = NULL,
                                           testStatistics = NULL,
                                           separatePValues = NULL,
                                           effectSizes = NULL,
                                           singleStepAdjustedPValues = NULL,
                                           intersectionTest = NULL,
                                           varianceOption = NULL,
                                           normalApproximation = NULL,
                                           directionUpper = NULL,
                                           initialize = function(design, dataInput, ...,
                                                                 combInverseNormal = NULL,
                                                                 combFisher = NULL,
                                                                 overallTestStatistics = NULL,
                                                                 overallStDevs = NULL,
                                                                 overallPooledStDevs = NULL,
                                                                 overallPValues = NULL,
                                                                 testStatistics = NULL,
                                                                 separatePValues = NULL,
                                                                 effectSizes = NULL,
                                                                 singleStepAdjustedPValues = NULL,
                                                                 intersectionTest = NULL,varianceOption = C_VARIANCE_OPTION_MULTIARMED_DEFAULT,
                                                                 normalApproximation = FALSE, directionUpper = NULL) {
                                             super$initialize(...)
                                             self$combInverseNormal <- combInverseNormal
                                             self$combFisher <- combFisher
                                             self$overallTestStatistics <- overallTestStatistics
                                             self$overallStDevs <- overallStDevs
                                             self$overallPooledStDevs <- overallPooledStDevs
                                             self$overallPValues <- overallPValues
                                             self$testStatistics <- testStatistics
                                             self$separatePValues <- separatePValues
                                             self$effectSizes <- effectSizes
                                             self$singleStepAdjustedPValues <- singleStepAdjustedPValues
                                             self$varianceOption <- varianceOption 
                                             self$normalApproximation <- normalApproximation
                                             self$directionUpper <- directionUpper
                                             
                                             
                                             self$init(design = design, dataInput = dataInput)
                                             
                                             for (param in c(
                                               "singleStepAdjustedPValues",
                                               "weightsFisher",
                                               "weightsInverseNormal",
                                               "combFisher",
                                               "combInverseNormal"
                                             )) {
                                               self$.setParameterType(param, C_PARAM_NOT_APPLICABLE)
                                             }
                                             
                                             for (param in self$.getParametersToShow()) {
                                               if (self$.getParameterType(param) == C_PARAM_TYPE_UNKNOWN) {
                                                 self$.setParameterType(param, C_PARAM_GENERATED)
                                               }
                                             }
                                             
                                             self$.setParameterType("varianceOption", ifelse(
                                               identical(self$varianceOption, C_VARIANCE_OPTION_MULTIARMED_DEFAULT), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                                             ))
                                             self$.setParameterType("normalApproximation", ifelse(
                                               identical(self$normalApproximation, FALSE), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                                             ))
                                             self$.setParameterType("directionUpper", ifelse(
                                               identical(self$directionUpper, C_DIRECTION_UPPER_DEFAULT), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                                             ))
                                           },
                                           .getParametersToShow = function() {
                                             parametersToShow <- c(
                                               "stages",
                                               "thetaH0",
                                               "direction",
                                               "normalApproximation",
                                               "directionUpper",
                                               "varianceOption",
                                               "intersectionTest",
                                               "overallTestStatistics",
                                               "overallPValues",
                                               "overallStDevs",
                                               "overallPooledStDevs",
                                               "testStatistics",
                                               "separatePValues",
                                               "effectSizes",
                                               "singleStepAdjustedPValues"
                                             )
                                             if (.isTrialDesignInverseNormal(self$.design)) {
                                               parametersToShow <- c(
                                                 parametersToShow,
                                                 "combInverseNormal",
                                                 "weightsInverseNormal"
                                               )
                                             } else if (.isTrialDesignFisher(self$.design)) {
                                               parametersToShow <- c(
                                                 parametersToShow,
                                                 "combFisher",
                                                 "weightsFisher"
                                               )
                                             }
                                             return(parametersToShow)
                                           }
                                         )
)

#'
#' @name StageResultsRates
#'
#' @title
#' Stage Results of Rates
#'
#' @description
#' Class for stage results of rates.
#' 
#' @template field_stages
#' @template field_testStatistics
#' @template field_overallTestStatistics
#' @template field_pValues
#' @template field_overallPValues
#' @template field_effectSizes
#' @template field_direction
#' @template field_testActions
#' @template field_thetaH0
#' @template field_normalApproximation
#' @template field_weightsFisher
#' @template field_weightsInverseNormal
#' @template field_combInverseNormal
#' @template field_combFisher
#' @field ... Names of \code{dataInput}.
#'
#' @details
#' This object cannot be created directly; use \code{getStageResults}
#' with suitable arguments to create the stage results of a dataset of rates.
#'
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_analysis_dataset.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
StageResultsRates <- R6Class("StageResultsRates",
                                 inherit = StageResults,
                                 public = list(
                                   combInverseNormal = NULL,
                                   combFisher = NULL,
                                   overallTestStatistics = NULL,
                                   overallPValues = NULL,
                                   effectSizes = NULL,
                                   testStatistics = NULL,
                                   overallPi1 = NULL,
                                   overallPi2 = NULL,
                                   overallEvents = NULL,
                                   overallEvents1 = NULL,
                                   overallEvents2 = NULL,
                                   overallSampleSizes = NULL,
                                   overallSampleSizes1 = NULL,
                                   overallSampleSizes2 = NULL,
                                   normalApproximation = NULL,
                                   initialize = function(design, dataInput, ...,
                                                         combInverseNormal = NULL,
                                                         combFisher = NULL,
                                                         overallTestStatistics = NULL,
                                                         overallPValues = NULL,
                                                         effectSizes = NULL,
                                                         testStatistics = NULL,
                                                         overallPi1 = NULL,
                                                         overallPi2 = NULL,
                                                         overallEvents = NULL,
                                                         overallEvents1 = NULL,
                                                         overallEvents2 = NULL,
                                                         overallSampleSizes = NULL,
                                                         overallSampleSizes1 = NULL,
                                                         overallSampleSizes2 = NULL,
                                                         normalApproximation = TRUE) {
                                     super$initialize(.design = design, .dataInput = dataInput, ...)#TODO
                                     
                                     self$combInverseNormal <- combInverseNormal
                                     self$combFisher <- combFisher
                                     self$overallTestStatistics <- overallTestStatistics
                                     self$overallPValues <- overallPValues
                                     self$effectSizes <- effectSizes
                                     self$testStatistics <- testStatistics
                                     self$overallPi1 <- overallPi1
                                     self$overallPi2 <- overallPi2
                                     self$overallEvents <- overallEvents
                                     self$overallEvents1 <- overallEvents1
                                     self$overallEvents2 <- overallEvents2
                                     self$overallSampleSizes <- overallSampleSizes
                                     self$overallSampleSizes1 <- overallSampleSizes1
                                     self$overallSampleSizes2 <- overallSampleSizes2
                                     
                                     self$normalApproximation <- normalApproximation 
                                     
                                     self$init(design = design, dataInput = dataInput)
                                     
                                     for (param in c(
                                       "weightsFisher",
                                       "weightsInverseNormal",
                                       "combFisher",
                                       "combInverseNormal"
                                     )) {
                                       self$.setParameterType(param, C_PARAM_NOT_APPLICABLE)
                                     }
                                     
                                     for (param in self$.getParametersToShow()) {
                                       if (self$.getParameterType(param) == C_PARAM_TYPE_UNKNOWN) {
                                         self$.setParameterType(param, C_PARAM_GENERATED)
                                       }
                                     }
                                     
                                     self$.setParameterType("normalApproximation", ifelse(
                                       identical(self$normalApproximation, TRUE), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                                     ))
                                   },
                                   .getParametersToShow = function() {
                                     parametersToShow <- c(
                                       "stages",
                                       "overallTestStatistics",
                                       "overallPValues"
                                     )
                                     if (self$.dataInput$getNumberOfGroups() == 1) {
                                       parametersToShow <- c(
                                         parametersToShow,
                                         "overallEvents",
                                         "overallSampleSizes",
                                         "overallPi1"
                                       )
                                     } else if (self$.dataInput$getNumberOfGroups() == 2) {
                                       parametersToShow <- c(
                                         parametersToShow,
                                         "overallEvents1",
                                         "overallEvents2",
                                         "overallSampleSizes1",
                                         "overallSampleSizes2",
                                         "overallPi1",
                                         "overallPi2"
                                       )
                                     }
                                     parametersToShow <- c(
                                       parametersToShow,
                                       "testStatistics",
                                       "pValues"
                                     )
                                     if (self$.dataInput$getNumberOfGroups() > 1) {
                                       parametersToShow <- c(parametersToShow, "effectSizes")
                                     }
                                     
                                     if (.isTrialDesignInverseNormal(self$.design)) {
                                       parametersToShow <- c(
                                         parametersToShow,
                                         "combInverseNormal",
                                         "weightsInverseNormal"
                                       )
                                     } else if (.isTrialDesignFisher(self$.design)) {
                                       parametersToShow <- c(
                                         parametersToShow,
                                         "combFisher",
                                         "weightsFisher"
                                       )
                                     }
                                     parametersToShow <- c(
                                       parametersToShow,
                                       "thetaH0",
                                       "direction",
                                       "normalApproximation"
                                     )
                                     return(parametersToShow)
                                   }
                                 )
)

#' @name StageResultsMultiArmRates
#'
#' @title
#' Stage Results Multi Arm Rates
#'
#' @description
#' Class for stage results of multi arm rates data
#'
#' @template field_stages
#' @template field_testStatistics
#' @template field_pValues 
#' @template field_combInverseNormal
#' @template field_combFisher
#' @template field_effectSizes
#' @template field_testActions
#' @template field_weightsFisher
#' @template field_weightsInverseNormal
#' @template field_combInverseNormal
#' @template field_combFisher
#' @template field_overallTestStatistics
#' @template field_overallPValues
#' @template field_testStatistics
#' @template field_separatePValues
#' @template field_effectSizes
#' @template field_singleStepAdjustedPValues
#' @template field_intersectionTest
#' @template field_normalApproximation
#' @template field_directionUpper
#'
#' @details
#' This object cannot be created directly; use \code{getStageResults}
#' with suitable arguments to create the stage results of a dataset of multi arm rates.
#' 
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_analysis_dataset.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
StageResultsMultiArmRates <- R6Class("StageResultsMultiArmRates",
                                         inherit = StageResults,
                                         public = list(
                                           stage = NULL,
                                           overallPiTreatments = NULL,
                                           overallPiControl = NULL,
                                           combInverseNormal = NULL,
                                           combFisher = NULL,
                                           overallTestStatistics = NULL,
                                           overallPValues = NULL,
                                           testStatistics = NULL,
                                           separatePValues = NULL,
                                           effectSizes = NULL,
                                           singleStepAdjustedPValues = NULL,
                                           intersectionTest = NULL,
                                           normalApproximation = NULL,
                                           directionUpper = NULL,
                                           initialize = function(design, dataInput, ...,
                                                                 overallPiTreatments = NULL,
                                                                 overallPiControl = NULL,
                                                                 combInverseNormal = NULL,
                                                                 combFisher = NULL,
                                                                 overallTestStatistics = NULL,
                                                                 overallPValues = NULL,
                                                                 testStatistics = NULL,
                                                                 separatePValues = NULL,
                                                                 effectSizes = NULL,
                                                                 singleStepAdjustedPValues = NULL,
                                                                 intersectionTest = NULL,
                                                                 normalApproximation = FALSE,
                                                                 directionUpper = NULL) {
                                             super$initialize(.design = design, .dataInput = dataInput, ...)#TODO
                                             
                                             self$overallPiTreatments <- overallPiTreatments 
                                             self$overallPiControl <- overallPiControl 
                                             self$combInverseNormal <- combInverseNormal 
                                             self$combFisher <- combFisher 
                                             self$overallTestStatistics <- overallTestStatistics 
                                             self$overallPValues <- overallPValues 
                                             self$testStatistics <- testStatistics 
                                             self$separatePValues <- separatePValues 
                                             self$effectSizes <- effectSizes 
                                             self$singleStepAdjustedPValues <- singleStepAdjustedPValues 
                                             self$intersectionTest <- intersectionTest
                                             self$normalApproximation <- normalApproximation
                                             self$directionUpper <- directionUpper 
                                             
                                             self$init(design = design, dataInput = dataInput)
                                             
                                             for (param in c(
                                               "singleStepAdjustedPValues",
                                               "weightsFisher",
                                               "weightsInverseNormal",
                                               "combFisher",
                                               "combInverseNormal"
                                             )) {
                                               self$.setParameterType(param, C_PARAM_NOT_APPLICABLE)
                                             }
                                             
                                             for (param in self$.getParametersToShow()) {
                                               if (self$.getParameterType(param) == C_PARAM_TYPE_UNKNOWN) {
                                                 self$.setParameterType(param, C_PARAM_GENERATED)
                                               }
                                             }
                                             
                                             self$.setParameterType("normalApproximation", ifelse(
                                               identical(self$normalApproximation, FALSE), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                                             ))
                                             self$.setParameterType("directionUpper", ifelse(
                                               identical(self$directionUpper, C_DIRECTION_UPPER_DEFAULT), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                                             ))
                                           },
                                           .getParametersToShow = function() {
                                             parametersToShow <- c(
                                               "stages",
                                               "thetaH0",
                                               "direction",
                                               "normalApproximation",
                                               "directionUpper",
                                               "overallPiControl",
                                               "overallPiTreatments",
                                               "intersectionTest",
                                               "overallTestStatistics",
                                               "overallPValues",
                                               "testStatistics",
                                               "separatePValues",
                                               "singleStepAdjustedPValues"
                                             )
                                             if (.isTrialDesignInverseNormal(self$.design)) {
                                               parametersToShow <- c(
                                                 parametersToShow,
                                                 "combInverseNormal",
                                                 "weightsInverseNormal"
                                               )
                                             } else if (.isTrialDesignFisher(self$.design)) {
                                               parametersToShow <- c(
                                                 parametersToShow,
                                                 "combFisher",
                                                 "weightsFisher"
                                               )
                                             }
                                             return(parametersToShow)
                                           }
                                         )
)

#'
#' @name StageResultsSurvival
#'
#' @title
#' Stage Results of Survival Data
#'
#' @description
#' Class for stage results survival data.
#' 
#' @template field_stages
#' @template field_testStatistics
#' @template field_overallTestStatistics
#' @template field_separatePValues
#' @template field_singleStepAdjustedPValues
#' @template field_overallPValues
#' @template field_direction
#' @template field_directionUpper
#' @template field_intersectionTest
#' @template field_combInverseNormal
#' @template field_combFisher
#' @template field_effectSizes
#' @template field_testActions
#' @template field_thetaH0
#' @template field_weightsFisher
#' @template field_weightsInverseNormal
#' @template field_normalApproximation
#' @field ... Names of \code{dataInput}.
#'
#' @details
#' This object cannot be created directly; use \code{getStageResults}
#' with suitable arguments to create the stage results of a dataset of survival data.
#'
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_analysis_dataset.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
StageResultsSurvival <- R6Class("StageResultsSurvival",
                                    inherit = StageResults,
                                    public = list(
                                      combInverseNormal = NULL,
                                      combFisher = NULL,
                                      overallPValues = NULL,
                                      effectSizes = NULL,
                                      overallTestStatistics = NULL,
                                      overallEvents = NULL,
                                      overallAllocationRatios = NULL,
                                      events = NULL,
                                      allocationRatios = NULL,
                                      testStatistics = NULL,
                                      initialize = function(design, dataInput, ...,
                                                            combInverseNormal = NULL,
                                                            combFisher = NULL,
                                                            overallPValues = NULL,
                                                            effectSizes = NULL,
                                                            overallTestStatistics = NULL,
                                                            overallEvents = NULL,
                                                            overallAllocationRatios = NULL,
                                                            events = NULL,
                                                            allocationRatios = NULL,
                                                            testStatistics = NULL) {
                                        super$initialize(.design = design, .dataInput = dataInput, ...)#TODO
                                        
                                        self$combInverseNormal <- combInverseNormal
                                        self$combFisher <- combFisher
                                        self$overallPValues <- overallPValues
                                        self$effectSizes <- effectSizes
                                        self$overallTestStatistics <- overallTestStatistics
                                        self$overallEvents <- overallEvents
                                        self$overallAllocationRatios <- overallAllocationRatios
                                        self$events <- events
                                        self$allocationRatios <- allocationRatios
                                        self$testStatistics <- testStatistics
                                        
                                        self$init(design = design, dataInput = dataInput)
                                        
                                        for (param in c(
                                          "weightsFisher",
                                          "weightsInverseNormal",
                                          "combFisher",
                                          "combInverseNormal"
                                        )) {
                                          self$.setParameterType(param, C_PARAM_NOT_APPLICABLE)
                                        }
                                        
                                        for (param in self$.getParametersToShow()) {
                                          if (self$.getParameterType(param) == C_PARAM_TYPE_UNKNOWN) {
                                            self$.setParameterType(param, C_PARAM_GENERATED)
                                          }
                                        }
                                      },
                                      .getParametersToShow = function() {
                                        parametersToShow <- c(
                                          "stages",
                                          "overallTestStatistics",
                                          "overallPValues",
                                          "overallEvents",
                                          "overallAllocationRatios",
                                          "events",
                                          "allocationRatios",
                                          "testStatistics",
                                          "pValues",
                                          "overallPValues",
                                          "effectSizes"
                                        )
                                        if (.isTrialDesignInverseNormal(self$.design)) {
                                          parametersToShow <- c(
                                            parametersToShow,
                                            "combInverseNormal",
                                            "weightsInverseNormal"
                                          )
                                        } else if (.isTrialDesignFisher(self$.design)) {
                                          parametersToShow <- c(
                                            parametersToShow,
                                            "combFisher",
                                            "weightsFisher"
                                          )
                                        }
                                        parametersToShow <- c(
                                          parametersToShow,
                                          "thetaH0",
                                          "direction"
                                        )
                                        return(parametersToShow)
                                      }
                                    )
)

#' @name StageResultsMultiArmSurvival
#'
#' @title
#' Stage Results Multi Arm Survival
#'
#' @description
#' Class for stage results of multi arm survival data
#'
#' @template field_stages
#' @template field_testStatistics
#' @template field_pValues 
#' @template field_combInverseNormal
#' @template field_combFisher
#' @template field_effectSizes
#' @template field_testActions
#' @template field_weightsFisher
#' @template field_weightsInverseNormal
#' @template field_combInverseNormal
#' @template field_combFisher
#' @template field_overallTestStatistics
#' @template field_overallPValues
#' @template field_testStatistics
#' @template field_separatePValues
#' @template field_effectSizes
#' @template field_singleStepAdjustedPValues
#' @template field_intersectionTest
#' @template field_directionUpper
#'
#' @details
#' This object cannot be created directly; use \code{getStageResults}
#' with suitable arguments to create the stage results of a dataset of multi arm survival.
#' 
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_analysis_dataset.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
StageResultsMultiArmSurvival <- R6Class("StageResultsMultiArmSurvival",
                                            inherit = StageResults,
                                            public = list(
                                              stage = NULL,
                                              combInverseNormal = NULL,
                                              combFisher = NULL,
                                              overallTestStatistics = NULL,
                                              overallPValues = NULL,
                                              testStatistics = NULL,
                                              separatePValues = NULL,
                                              effectSizes = NULL,
                                              singleStepAdjustedPValues = NULL,
                                              intersectionTest = NULL,
                                              directionUpper = NULL,
                                              initialize = function(design, dataInput, ...,
                                                                    combInverseNormal = NULL,
                                                                    combFisher = NULL,
                                                                    overallTestStatistics = NULL,
                                                                    overallPValues = NULL,
                                                                    testStatistics = NULL,
                                                                    separatePValues = NULL,
                                                                    effectSizes = NULL,
                                                                    singleStepAdjustedPValues = NULL,
                                                                    intersectionTest = NULL,
                                                                    directionUpper = NULL) {
                                                super$initialize(.design = design, .dataInput = dataInput, ...) #TODO
                                                
                                                self$combInverseNormal <- combInverseNormal
                                                self$combFisher <- combFisher
                                                self$overallTestStatistics <- overallTestStatistics
                                                self$overallPValues <- overallPValues
                                                self$testStatistics <- testStatistics
                                                self$separatePValues <- separatePValues
                                                self$effectSizes <- effectSizes
                                                self$singleStepAdjustedPValues <- singleStepAdjustedPValues
                                                self$intersectionTest <- intersectionTest
                                                self$directionUpper <- directionUpper
                                                
                                                self$init(design = design, dataInput = dataInput)
                                                
                                                for (param in c(
                                                  "singleStepAdjustedPValues",
                                                  "weightsFisher",
                                                  "weightsInverseNormal",
                                                  "combFisher",
                                                  "combInverseNormal"
                                                )) {
                                                  self$.setParameterType(param, C_PARAM_NOT_APPLICABLE)
                                                }
                                                
                                                for (param in self$.getParametersToShow()) {
                                                  if (self$.getParameterType(param) == C_PARAM_TYPE_UNKNOWN) {
                                                    self$.setParameterType(param, C_PARAM_GENERATED)
                                                  }
                                                }
                                                
                                                self$.setParameterType("directionUpper", ifelse(
                                                  identical(self$directionUpper, C_DIRECTION_UPPER_DEFAULT), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
                                                ))
                                              },
                                              .getParametersToShow = function() {
                                                parametersToShow <- c(
                                                  "stages",
                                                  "thetaH0",
                                                  "direction",
                                                  "directionUpper",
                                                  "intersectionTest",
                                                  "overallTestStatistics",
                                                  "overallPValues",
                                                  "testStatistics",
                                                  "separatePValues",
                                                  "effectSizes",
                                                  "singleStepAdjustedPValues"
                                                )
                                                if (.isTrialDesignInverseNormal(self$.design)) {
                                                  parametersToShow <- c(
                                                    parametersToShow,
                                                    "combInverseNormal",
                                                    "weightsInverseNormal"
                                                  )
                                                } else if (.isTrialDesignFisher(self$.design)) {
                                                  parametersToShow <- c(
                                                    parametersToShow,
                                                    "combFisher",
                                                    "weightsFisher"
                                                  )
                                                }
                                                return(parametersToShow)
                                              }
                                            )
)

#'
#' @name StageResultsEnrichmentMeans
#'
#' @title
#' Stage Results Enrichment Means
#'
#' @description
#' Class for stage results of enrichment means data
#'
#' @template field_stages
#' @template field_thetaH0
#' @template field_direction
#' @template field_normalApproximation
#' @template field_directionUpper
#' @template field_varianceOption
#' @template field_intersectionTest
#' @template field_testStatistics
#' @template field_overallTestStatistics
#' @template field_pValues 
#' @template field_overallPValues
#' @template field_overallStDevs
#' @template field_overallPooledStDevs
#' @template field_separatePValues
#' @template field_effectSizes
#' @template field_singleStepAdjustedPValues
#' @template field_stratifiedAnalysis
#' @template field_combInverseNormal
#' @template field_combFisher
#' @template field_weightsFisher
#' @template field_weightsInverseNormal
#'
#' @details
#' This object cannot be created directly; use \code{getStageResults}
#' with suitable arguments to create the stage results of a dataset of enrichment means.
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
StageResultsEnrichmentMeans <- R6Class("StageResultsEnrichmentMeans",
                                           inherit = StageResultsMultiArmMeans,
                                           public = list(
                                             .overallSampleSizes1 = NULL,
                                             .overallSampleSizes2 = NULL,
                                             stratifiedAnalysis = NULL,
                                             .getParametersToShow = function() {#TODO init
                                               return(c(super$.getParametersToShow(), "stratifiedAnalysis"))
                                             }
                                           )
)

#'
#' @name StageResultsEnrichmentRates
#'
#' @title
#' Stage Results Enrichment Rates
#'
#' @description
#' Class for stage results of enrichment rates data.
#'
#' @template field_stages
#' @template field_testStatistics
#' @template field_pValues 
#' @template field_combInverseNormal
#' @template field_combFisher
#' @template field_effectSizes
#' @template field_testActions
#' @template field_weightsFisher
#' @template field_weightsInverseNormal
#'
#' @details
#' This object cannot be created directly; use \code{getStageResults}
#' with suitable arguments to create the stage results of a dataset of enrichment rates.
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
StageResultsEnrichmentRates <- R6Class("StageResultsEnrichmentRates",
                                           inherit = StageResultsMultiArmRates,
                                           public = list(
                                             .overallSampleSizes1 = NULL,
                                             .overallSampleSizes2 = NULL,
                                             overallPisTreatment = NULL,
                                             overallPisControl = NULL,
                                             stratifiedAnalysis = NULL,
                                             .getParametersToShow = function() {
                                               parametersToShow <- super$.getParametersToShow()
                                               parametersToShow <- parametersToShow[!(parametersToShow %in% c("overallPiTreatments", "overallPiControl"))]
                                               return(c(parametersToShow, "stratifiedAnalysis", "overallPisTreatment", "overallPisControl"))
                                             }
                                           )
)

#'
#' @name StageResultsEnrichmentSurvival
#'
#' @title
#' Stage Results Enrichment Survival
#'
#' @description
#' Class for stage results of enrichment survival data.
#'
#' @template field_stages
#' @template field_testStatistics
#' @template field_pValues 
#' @template field_combInverseNormal
#' @template field_combFisher
#' @template field_effectSizes
#' @template field_testActions
#' @template field_weightsFisher
#' @template field_weightsInverseNormal
#'
#' @details
#' This object cannot be created directly; use \code{getStageResults}
#' with suitable arguments to create the stage results of a dataset of enrichment survival.
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
StageResultsEnrichmentSurvival <- R6Class("StageResultsEnrichmentSurvival",
                                              inherit = StageResultsMultiArmSurvival,
                                              public = list(
                                                stratifiedAnalysis = NULL,
                                                .overallEvents = NULL,
                                                .getParametersToShow = function() {
                                                  return(c(super$.getParametersToShow(), "stratifiedAnalysis"))
                                                }
                                              )
)

#'
#' @title
#' Names of a Stage Results Object
#'
#' @description
#' Function to get the names of a \code{\link{StageResults}} object.
#'
#' @param x A \code{\link{StageResults}} object.
#'
#' @details
#' Returns the names of stage results that can be accessed by the user.
#'
#' @template return_names
#'
#' @export
#'
#' @keywords internal
#'
names.StageResults <- function(x) {
  return(x$.getParametersToShow())
}

#'
#' @title
#' Coerce Stage Results to a Data Frame
#'
#' @description
#' Returns the \code{StageResults} as data frame.
#'
#' @param x A \code{\link{StageResults}} object.
#' @inheritParams param_niceColumnNamesEnabled
#' @inheritParams param_includeAllParameters
#' @inheritParams param_three_dots
#'
#' @details
#' Coerces the stage results to a data frame.
#'
#' @template return_dataframe
#'
#' @export
#'
#' @keywords internal
#'
as.data.frame.StageResults <- function(x, row.names = NULL,
        optional = FALSE, niceColumnNamesEnabled = FALSE,
        includeAllParameters = FALSE, type = 1, ...) {
    if (type == 1) {
        parametersToShow <- x$.getParametersToShow()

        return(.getAsDataFrame(
            parameterSet = x,
            parameterNames = parametersToShow,
            niceColumnNamesEnabled = niceColumnNamesEnabled,
            includeAllParameters = includeAllParameters
        ))
    }

    kMax <- length(x$stages)
    group1 <- rep(1, kMax)
    group2 <- rep(2, kMax)
    empty <- rep(NA_real_, kMax)
    stageResults <- data.frame(
        Stage = c(x$stages, x$stages),
        Group = c(group1, group2),
        "Cumulative Mean" = c(x$overallMeans1, x$overallMeans2),
        "Cumulative stDev" = c(x$overallStDevs1, x$overallStDevs2),
        "Cumulative test statistics" = c(x$overallTestStatistics, empty),
        "Overall p-value" = c(x$overallPValues, empty),
        "Cumulative stDev" = c(x$overallStDevs, empty),
        "Stage-wise test statistic" = c(x$testStatistics, empty),
        "Stage-wise p-value" = c(x$pValues, empty),
        "Comb Inverse Normal" = c(x$combInverseNormal, empty),
        "Comb Fisher" = c(x$combFisher, empty),
        "Weights Fisher" = c(x$weightsFisher, empty),
        "Weights Inverse Normal" = c(x$weightsInverseNormal, empty),
        row.names = row.names,
        ...
    )
    stageResults <- stageResults[with(stageResults, order(Stage, Group)), ]
    return(stageResults)
}

.getTreatmentArmsToShow <- function(x, ...) {
  dataInput <- x
  if (!inherits(dataInput, "Dataset")) {
    dataInput <- x[[".dataInput"]]
  }
  if (is.null(dataInput) || !inherits(dataInput, "Dataset")) {
    stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "failed to get 'dataInput' from ", .getClassName(x))
  }
  
  numberOfTreatments <- dataInput$getNumberOfGroups()
  if (numberOfTreatments > 1) {
    validComparisons <- 1L:as.integer(numberOfTreatments - 1)
  } else {
    validComparisons <- 1L
  }
  
  treatmentArmsToShow <- .getOptionalArgument("treatmentArms", ...)
  if (!is.null(treatmentArmsToShow)) {
    treatmentArmsToShow <- as.integer(na.omit(treatmentArmsToShow))
  }
  if (is.null(treatmentArmsToShow) || length(treatmentArmsToShow) == 0 ||
      all(is.na(treatmentArmsToShow)) || !is.numeric(treatmentArmsToShow)) {
    treatmentArmsToShow <- validComparisons
  } else if (!all(treatmentArmsToShow %in% validComparisons)) {
    stop(
      C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'treatmentArms' (",
      .arrayToString(treatmentArmsToShow), ") must be a vector ",
      "containing one or more values of ", .arrayToString(validComparisons)
    )
  }
  treatmentArmsToShow <- sort(unique(treatmentArmsToShow))
  return(treatmentArmsToShow)
}

.getPopulationsToShow <- function(x, ..., gMax) {
  dataInput <- x
  if (!inherits(dataInput, "Dataset")) {
    dataInput <- x[[".dataInput"]]
  }
  if (is.null(dataInput) || !inherits(dataInput, "Dataset")) {
    stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "failed to get 'dataInput' from ", .getClassName(x))
  }
  
  numberOfPopulations <- gMax
  if (numberOfPopulations > 1) {
    validComparisons <- 1L:as.integer(numberOfPopulations)
  } else {
    validComparisons <- 1L
  }
  
  populationsToShow <- .getOptionalArgument("populations", ...)
  
  if (!is.null(populationsToShow)) {
    populationsToShow <- as.integer(na.omit(populationsToShow))
  }
  if (is.null(populationsToShow) || length(populationsToShow) == 0 ||
      all(is.na(populationsToShow)) || !is.numeric(populationsToShow)) {
    populationsToShow <- validComparisons
  } else if (!all(populationsToShow %in% validComparisons)) {
    stop(
      C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'populations' (",
      .arrayToString(populationsToShow), ") must be a vector ",
      "containing one or more values of ", .arrayToString(validComparisons)
    )
  }
  populationsToShow <- sort(unique(populationsToShow))
  return(populationsToShow)
}

#'
#' @title
#' Stage Results Plotting
#'
#' @description
#' Plots the conditional power together with the likelihood function.
#'
#' @param x The stage results at given stage, obtained from \code{\link[=getStageResults]{getStageResults()}} or 
#'        \code{\link[=getAnalysisResults]{getAnalysisResults()}}.
#' @param y Not available for this kind of plot (is only defined to be compatible to the generic plot function).
#' @inheritParams param_stage
#' @inheritParams param_nPlanned
#' @inheritParams param_allocationRatioPlanned
#' @param main The main title.
#' @param xlab The x-axis label.
#' @param ylab The y-axis label.
#' @param legendTitle The legend title.
#' @inheritParams param_palette
#' @inheritParams param_showSource
#' @inheritParams param_plotSettings
#' @inheritParams param_legendPosition
#' @param type The plot type (default = 1). Note that at the moment only one type
#'        (the conditional power plot) is available.
#' @param ... Optional \link[=param_three_dots_plot]{plot arguments}. Furthermore the following arguments can be defined:
#' \itemize{
#' \item \code{thetaRange}: A range of assumed effect sizes if testing means or a survival design was specified.
#'       Additionally, if testing means was selected, an assumed standard deviation can be specified (default is 1).
#' \item \code{piTreatmentRange}: A range of assumed rates pi1 to calculate the conditional power.
#'       Additionally, if a two-sample comparison was selected, pi2 can be specified (default is the value from
#'       \code{\link[=getAnalysisResults]{getAnalysisResults()}}).
#' \item \code{directionUpper}: Specifies the direction of the alternative,
#'       only applicable for one-sided testing; default is \code{TRUE}
#'       which means that larger values of the test statistics yield smaller p-values.
#' \item \code{\link[=param_thetaH0]{thetaH0}}: The null hypothesis value, default is 0 for the normal and the binary case,
#'       it is 1 for the survival case.
#'       For testing a rate in one sample, a value thetaH0 in (0,1) has to be specified for
#'       defining the null hypothesis H0: pi = thetaH0.
#' }
#'
#' @details
#' Generic function to plot all kinds of stage results.
#' The conditional power is calculated only if effect size and sample size is specified.
#'
#' @template return_object_ggplot
#'
#' @examples
#' design <- getDesignGroupSequential(
#'     kMax = 4, alpha = 0.025,
#'     informationRates = c(0.2, 0.5, 0.8, 1),
#'     typeOfDesign = "WT", deltaWT = 0.25
#' )
#'
#' dataExample <- getDataset(
#'     n = c(20, 30, 30),
#'     means = c(50, 51, 55),
#'     stDevs = c(130, 140, 120)
#' )
#'
#' stageResults <- getStageResults(design, dataExample, thetaH0 = 20)
#' 
#' \dontrun{
#' if (require(ggplot2)) plot(stageResults, nPlanned = c(30), thetaRange = c(0, 100))
#' }
#' 
#' @export
#'
plot.StageResults <- function(x, y, ..., type = 1L,
                              nPlanned, allocationRatioPlanned = 1, # C_ALLOCATION_RATIO_DEFAULT
                              main = NA_character_, xlab = NA_character_, ylab = NA_character_,
                              legendTitle = NA_character_, palette = "Set1", legendPosition = NA_integer_,
                              showSource = FALSE, plotSettings = NULL) {
  fCall <- match.call(expand.dots = FALSE)
  
  .assertGgplotIsInstalled()
  .assertIsStageResults(x)
  .assertIsValidLegendPosition(legendPosition)
  if (.isConditionalPowerEnabled(nPlanned)) {
    .assertIsValidAllocationRatioPlanned(allocationRatioPlanned, x$.dataInput$getNumberOfGroups())
  }
  .stopInCaseOfIllegalStageDefinition2(...)
  
  if (x$.design$kMax == 1) {
    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "cannot plot stage results of a fixed design")
  }
  
  if (!is.logical(showSource) || isTRUE(showSource)) {
    stageResultsName <- .getOptionalArgument("stageResultsName", ...)
    if (is.null(stageResultsName)) {
      stageResultsName <- deparse(fCall$x)
    }
    cat("Source data of the plot:\n")
    cat("  Use getConditionalPower(..., addPlotData = TRUE) to create the data.\n", sep = "")
    cat("Simple plot command example:\n", sep = "")
    
    cmd <- paste0(
      "condPow <- getConditionalPower(", stageResultsName,
      ", nPlanned = ", .arrayToString(nPlanned, vectorLookAndFeelEnabled = TRUE)
    )

    yParameterName1 <- "Conditional power"
    yParameterName2 <- "Likelihood"

    if (.isMultiArmStageResults(x)) {
        treatmentArmsToShow <- .getTreatmentArmsToShow(x, ...)
        data <- data.frame(
            xValues = numeric(0),
            yValues = numeric(0),
            categories = character(),
            treatmentArms = numeric(0)
        )
        for (treatmentArm in treatmentArmsToShow) {
            legend1 <- ifelse(length(treatmentArmsToShow) == 1, yParameterName1,
                paste0(yParameterName1, " (", treatmentArm, " vs control)")
            )
            legend2 <- ifelse(length(treatmentArmsToShow) == 1, yParameterName2,
                paste0(yParameterName2, " (", treatmentArm, " vs control)")
            )

            treatmentArmIndices <- which(plotData$treatmentArms == treatmentArm)

            if (all(is.na(plotData$condPowerValues[treatmentArmIndices]))) {
                if (!all(is.na(plotData$likelihoodValues[treatmentArmIndices]))) {
                    data <- rbind(data, data.frame(
                        xValues = plotData$xValues[treatmentArmIndices],
                        yValues = plotData$likelihoodValues[treatmentArmIndices],
                        categories = rep(legend2, length(plotData$xValues[treatmentArmIndices])),
                        treatmentArms = rep(treatmentArm, length(plotData$xValues[treatmentArmIndices]))
                    ))
                }
            } else {
                data <- rbind(data, data.frame(
                    xValues = c(
                        plotData$xValues[treatmentArmIndices],
                        plotData$xValues[treatmentArmIndices]
                    ),
                    yValues = c(
                        plotData$condPowerValues[treatmentArmIndices],
                        plotData$likelihoodValues[treatmentArmIndices]
                    ),
                    categories = c(
                        rep(legend1, length(plotData$xValues[treatmentArmIndices])),
                        rep(legend2, length(plotData$xValues[treatmentArmIndices]))
                    ),
                    treatmentArms = c(
                        rep(treatmentArm, length(plotData$xValues[treatmentArmIndices])),
                        rep(treatmentArm, length(plotData$xValues[treatmentArmIndices]))
                    )
                ))
            }
        }
    } else if (.isEnrichmentStageResults(x)) {
        gMax <- max(na.omit(plotData$populations))
        populationsToShow <- .getPopulationsToShow(x, ..., gMax = gMax)
        data <- data.frame(
            xValues = numeric(0),
            yValues = numeric(0),
            categories = character(),
            populations = numeric(0)
        )
        for (population in populationsToShow) {
            populationName <- ifelse(population == gMax, "F", paste0("S", population))
            legend1 <- ifelse(length(populationsToShow) == 1, yParameterName1,
                paste0(yParameterName1, " (", populationName, ")")
            )
            legend2 <- ifelse(length(populationsToShow) == 1, yParameterName2,
                paste0(yParameterName2, " (", populationName, ")")
            )

            populationIndices <- which(plotData$populations == population)

            if (all(is.na(plotData$condPowerValues[populationIndices]))) {
                if (!all(is.na(plotData$likelihoodValues[populationIndices]))) {
                    data <- rbind(data, data.frame(
                        xValues = plotData$xValues[populationIndices],
                        yValues = plotData$likelihoodValues[populationIndices],
                        categories = rep(legend2, length(plotData$xValues[populationIndices])),
                        populations = rep(population, length(plotData$xValues[populationIndices]))
                    ))
                }
            } else {
                data <- rbind(data, data.frame(
                    xValues = c(
                        plotData$xValues[populationIndices],
                        plotData$xValues[populationIndices]
                    ),
                    yValues = c(
                        plotData$condPowerValues[populationIndices],
                        plotData$likelihoodValues[populationIndices]
                    ),
                    categories = c(
                        rep(legend1, length(plotData$xValues[populationIndices])),
                        rep(legend2, length(plotData$xValues[populationIndices]))
                    ),
                    populations = c(
                        rep(population, length(plotData$xValues[populationIndices])),
                        rep(population, length(plotData$xValues[populationIndices]))
                    )
                ))
            }
        }
      } else {
        data <- rbind(data, data.frame(
          xValues = c(
            plotData$xValues[populationIndices],
            plotData$xValues[populationIndices]
          ),
          yValues = c(
            plotData$condPowerValues[populationIndices],
            plotData$likelihoodValues[populationIndices]
          ),
          categories = c(
            rep(legend1, length(plotData$xValues[populationIndices])),
            rep(legend2, length(plotData$xValues[populationIndices]))
          ),
          populations = c(
            rep(population, length(plotData$xValues[populationIndices])),
            rep(population, length(plotData$xValues[populationIndices]))
          )
        ))
      }
    }
  } else {
    if (all(is.na(plotData$condPowerValues))) {
      legendPosition <- -1
      data <- data.frame(
        xValues = plotData$xValues,
        yValues = plotData$likelihoodValues,
        categories = rep(yParameterName2, length(plotData$xValues))
      )
    } else {
      data <- data.frame(
        xValues = c(plotData$xValues, plotData$xValues),
        yValues = c(plotData$condPowerValues, plotData$likelihoodValues),
        categories = c(
          rep(yParameterName1, length(plotData$xValues)),
          rep(yParameterName2, length(plotData$xValues))
        )
      )
    }
  }
  
  data$categories <- factor(data$categories, levels = unique(data$categories))
  
  main <- ifelse(is.na(main), C_PLOT_MAIN_CONDITIONAL_POWER_WITH_LIKELIHOOD, main)
  ylab <- ifelse(is.na(ylab), C_PLOT_YLAB_CONDITIONAL_POWER_WITH_LIKELIHOOD, ylab)
  
  if (is.na(legendTitle)) {
    legendTitle <- "Parameter"
  }
  
  return(.createAnalysisResultsPlotObject(x,
                                          data = data, plotData = plotData, main = main, xlab = xlab, ylab = ylab,
                                          legendTitle = legendTitle, palette = palette, legendPosition = legendPosition, numberOfPairedLines = 2L,
                                          plotSettings = plotSettings
  ))
}

.createAnalysisResultsPlotObject <- function(x, ..., data, plotData,
                                             main = NA_character_, xlab = NA_character_, ylab = NA_character_,
                                             legendTitle = NA_character_, palette = "Set1", legendPosition = NA_integer_,
                                             numberOfPairedLines = NA_integer_, plotSettings = NULL) {
  ciModeEnabled <- !is.null(data[["lower"]]) && !is.null(data[["upper"]])
  
  if (!ciModeEnabled) {
    p <- ggplot2::ggplot(data, ggplot2::aes(
      x = .data[["xValues"]], y = .data[["yValues"]],
      colour = factor(.data[["categories"]]),
      linetype = factor(.data[["categories"]])
    ))
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(
      x = .data[["xValues"]], y = .data[["yValues"]],
      colour = factor(.data[["categories"]])
    ))
  }
  
  if (is.null(plotSettings)) {
    plotSettings <- x$getPlotSettings()
  }
  
  p <- plotSettings$setTheme(p)
  p <- plotSettings$hideGridLines(p)
  
  # set main title
  mainTitle <- ifelse(!is.call(main) && !isS4(main) && is.na(main), plotData$main, main)
  p <- plotSettings$setMainTitle(p, mainTitle, subtitle = plotData$sub)
  
  # set legend
  if (is.na(legendPosition)) {
    legendPosition <- C_POSITION_LEFT_TOP
  }
  p <- plotSettings$setLegendPosition(p, legendPosition = legendPosition)
  p <- plotSettings$setLegendBorder(p)
  p <- plotSettings$setLegendTitle(p, legendTitle)
  p <- plotSettings$setLegendLabelSize(p)
  
  # set axes labels
  p <- plotSettings$setAxesLabels(p,
                                  xAxisLabel = plotData$xlab, yAxisLabel1 = plotData$ylab,
                                  xlab = xlab, ylab = ylab
  )
  
  # plot lines and points
  if (!ciModeEnabled) {
    if (is.na(numberOfPairedLines)) {
      numberOfPairedLines <- 2
      if (x$.isMultiArm()) {
        numberOfPairedLines <- length(unique(data$treatmentArms)) - 1
      } else if (x$.isEnrichment()) {
        numberOfPairedLines <- length(unique(data$populations)) - 1
      }
    }
    
    p <- plotSettings$plotValues(p, plotPointsEnabled = FALSE, pointBorder = 1)
    n <- length(unique(data$categories)) / numberOfPairedLines
    if (n > 1) {
      lineTypeValues <- rep(1:numberOfPairedLines, n)
      colorTypes <- sort(rep(1:n, numberOfPairedLines))
      for (i in c(1, 3)) {
        colorTypes[colorTypes >= i] <- colorTypes[colorTypes >= i] + 1
      }
      p <- p + ggplot2::scale_color_manual(name = legendTitle, values = colorTypes)
      p <- p + ggplot2::scale_linetype_manual(name = legendTitle, values = lineTypeValues)
    } else {
      colorValues <- c(2, 4)
      if (!x$.isMultiArm()) {
        colorValues <- c(2, 2) # use only one color
      }
      p <- p + ggplot2::scale_color_manual(name = legendTitle, values = colorValues)
      p <- p + ggplot2::scale_linetype_manual(name = legendTitle, values = c(1, 2))
    }
  }
  
  # plot confidence intervall
  else {
    pd <- ggplot2::position_dodge(0.15)
    
    p <- p + ggplot2::geom_errorbar(
      data = data,
      ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]),
      width = 0.15, position = pd, size = 0.8
    )
    p <- p + ggplot2::geom_line(position = pd, linetype = "longdash")
    p <- p + ggplot2::geom_point(position = pd, size = 2.0)
    
    
    stage <- unique(data$xValues)
    kMax <- list(...)[["kMax"]]
    if (length(stage) == 1 && !is.null(kMax)) {
      stages <- 1:kMax
      p <- p + ggplot2::scale_x_continuous(breaks = stages)
    } else if (length(stage) > 1 && all(stage %in% 1:10)) {
      p <- p + ggplot2::scale_x_continuous(breaks = stage)
    }

    # plot confidence intervall
    else {
        pd <- ggplot2::position_dodge(0.15)

        p <- p + ggplot2::geom_errorbar(
            data = data,
            ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]),
            width = 0.15, position = pd, linewidth = 0.8
        )
        p <- p + ggplot2::geom_line(position = pd, linetype = "longdash")
        p <- p + ggplot2::geom_point(position = pd, size = 2.0)


        stage <- unique(data$xValues)
        kMax <- list(...)[["kMax"]]
        if (length(stage) == 1 && !is.null(kMax)) {
            stages <- 1:kMax
            p <- p + ggplot2::scale_x_continuous(breaks = stages)
        } else if (length(stage) > 1 && all(stage %in% 1:10)) {
            p <- p + ggplot2::scale_x_continuous(breaks = stage)
        }
    }

    p <- plotSettings$setAxesAppearance(p)
    p <- plotSettings$enlargeAxisTicks(p)

    companyAnnotationEnabled <- .getOptionalArgument("companyAnnotationEnabled", ...)
    if (is.null(companyAnnotationEnabled) || !is.logical(companyAnnotationEnabled)) {
        companyAnnotationEnabled <- FALSE
    }

    p <- plotSettings$addCompanyAnnotation(p, enabled = companyAnnotationEnabled)

    # start plot generation
    return(p)
}
