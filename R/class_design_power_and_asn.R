## |
## |  *Power and average sample number result classes*
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
## |  File version: $Revision: 8474 $
## |  Last changed: $Date: 2025-01-14 14:32:53 +0100 (Di, 14 Jan 2025) $
## |  Last changed by: $Author: pahlke $
## |


#'
#' @name PowerAndAverageSampleNumberResult
#'
#' @title
#' Power and Average Sample Number Result
#'
#' @description
#' Class for power and average sample number (ASN) results.
#'
#' @template field_nMax
#' @template field_theta
#' @template field_averageSampleNumber
#' @template field_calculatedPower
#' @template field_overallEarlyStop
#' @template field_earlyStop
#' @template field_overallReject
#' @template field_rejectPerStage
#' @template field_overallFutility
#' @template field_futilityPerStage
#'
#' @details
#' This object cannot be created directly;
#' use \code{\link[=getPowerAndAverageSampleNumber]{getPowerAndAverageSampleNumber()}}
#' with suitable arguments to create it.
#'
#' @include class_core_parameter_set.R
#' @include class_design.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
PowerAndAverageSampleNumberResult <- R6::R6Class("PowerAndAverageSampleNumberResult",
    inherit = ParameterSet,
    public = list(
        .design = NULL,
        nMax = NULL,
        theta = NULL,
        averageSampleNumber = NULL,
        calculatedPower = NULL,
        overallEarlyStop = NULL,
        earlyStop = NULL,
        overallReject = NULL,
        rejectPerStage = NULL,
        overallFutility = NULL,
        futilityPerStage = NULL,
        initialize = function(design, theta = seq(-1, 1, 0.05), nMax = 100L, ...) {
            super$initialize(...)

            self$.design <- design
            self$theta <- theta
            self$nMax <- nMax

            self$theta <- .assertIsValidThetaRange(thetaRange = theta, thetaAutoSeqEnabled = FALSE)
            self$.initPowerAndAverageSampleNumber()
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing a power and average sample size (ASN) result"
            self$.resetCat()
            if (showType == 2) {
                super$.show(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                self$.cat("Power and average sample size (ASN):\n\n",
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
                if (self$.design$kMax > 1) {
                    self$.cat("Legend:\n",
                        heading = 2,
                        consoleOutputEnabled = consoleOutputEnabled
                    )
                    if (self$.design$kMax > 1) {
                        self$.cat("  [k]: values at stage k\n", consoleOutputEnabled = consoleOutputEnabled)
                    }
                    self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
                }
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            s <- "power and average sample size (ASN)"
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
        },
        .initPowerAndAverageSampleNumber = function() {
            .assertIsTrialDesignInverseNormalOrGroupSequential(self$.design)
            .assertIsValidSidedParameter(self$.design$sided)

            if (self$nMax <= 0) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
                    "'nMax' must be an integer > 0", 
                    call. = FALSE)
            }

            self$.setParameterType("nMax", ifelse(self$nMax == C_NA_MAX_DEFAULT,
                C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
            ))

            thetaIsDefault <- length(self$theta) == length(C_POWER_ASN_THETA_DEFAULT) &&
                sum(self$theta == C_POWER_ASN_THETA_DEFAULT) == length(self$theta)
            self$.setParameterType("theta", ifelse(thetaIsDefault,
                C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
            ))

            kMax <- self$.design$kMax

            # initialization
            numberOfThetas <- length(self$theta)

            self$averageSampleNumber <- rep(NA_real_, numberOfThetas)
            self$.setParameterType("averageSampleNumber", C_PARAM_GENERATED)

            self$calculatedPower <- rep(NA_real_, numberOfThetas)
            self$.setParameterType("calculatedPower", C_PARAM_GENERATED)

            self$earlyStop <- matrix(NA_real_, kMax, numberOfThetas)
            self$.setParameterType("earlyStop", C_PARAM_GENERATED)

            self$rejectPerStage <- matrix(NA_real_, kMax, numberOfThetas)
            self$.setParameterType("rejectPerStage", C_PARAM_GENERATED)

            self$futilityPerStage <- matrix(NA_real_, kMax - 1, numberOfThetas)
            self$.setParameterType("futilityPerStage", C_PARAM_GENERATED)

            rowNames <- paste("stage =", c(1:kMax))
            rownames(self$earlyStop) <- rowNames
            rownames(self$rejectPerStage) <- rowNames
            if (kMax > 1) {
                rownames(self$futilityPerStage) <- rowNames[1:(kMax - 1)]
            }

            for (i in 1:numberOfThetas) {
                result <- self$.getPowerAndAverageSampleNumber(theta = self$theta[i])

                self$averageSampleNumber[i] <- result$averageSampleNumber
                self$calculatedPower[i] <- result$calculatedPower
                self$earlyStop[1:(kMax - 1), i] <- result$earlyStop[1:(kMax - 1)]
                self$rejectPerStage[, i] <- result$rejectPerStage[1:kMax]
                self$futilityPerStage[, i] <- result$futilityPerStage[1:(kMax - 1)]
            }

            self$overallEarlyStop <- self$.getOverallParameter(self$earlyStop)
            self$.setParameterType("overallEarlyStop", C_PARAM_GENERATED)

            self$overallReject <- self$.getOverallParameter(self$rejectPerStage)
            self$.setParameterType("overallReject", C_PARAM_GENERATED)

            self$overallFutility <- self$.getOverallParameter(self$futilityPerStage)
            self$.setParameterType("overallFutility", C_PARAM_GENERATED)
        },
        .getPowerAndAverageSampleNumber = function(theta) {
            kMax <- self$.design$kMax
            futilityBounds <- self$.design$futilityBounds
            informationRates <- self$.design$informationRates
            criticalValues <- .getCriticalValues(self$.design)
            sided <- self$.design$sided
            delayedInformation <- self$.design$delayedInformation

            .earlyStop <- rep(NA_real_, kMax)
            .futilityPerStage <- rep(NA_real_, kMax)

            if (!any(is.na(delayedInformation))) {
                contRegionLower <- futilityBounds
                contRegionUpper <- criticalValues
                decisionCriticalValues <- self$.design$decisionCriticalValues
                probs <- .calculateDecisionProbabilities(
                    sqrtShift = sqrt(self$nMax) * theta,
                    informationRates, 
                    delayedInformation, 
                    contRegionUpper, 
                    contRegionLower, 
                    decisionCriticalValues
                )

                .averageSampleNumber <- self$nMax - sum(probs$stoppingProbabilities *
                    (informationRates[kMax] - delayedInformation - informationRates[1:(kMax - 1)]) * self$nMax)
                .calculatedPower <- probs$power[kMax]
                .rejectPerStage <- probs$rejectionProbabilities
                .earlyStop <- probs$stoppingProbabilities
                .futilityPerStage <- probs$futilityProbabilities
            } else {
                if (sided == 2) {
                    if (self$.design$typeOfDesign == C_TYPE_OF_DESIGN_PT ||
                            !is.null(self$.design$typeBetaSpending) && self$.design$typeBetaSpending != "none") {
                        futilityBounds[is.na(futilityBounds)] <- 0
                        decisionMatrix <- matrix(c(
                            -criticalValues - theta * sqrt(self$nMax * informationRates),
                            c(-futilityBounds - theta * sqrt(self$nMax * informationRates[1:(kMax - 1)]), 0),
                            c(futilityBounds - theta * sqrt(self$nMax * informationRates[1:(kMax - 1)]), 0),
                            criticalValues - theta * sqrt(self$nMax * informationRates)
                        ), nrow = 4, byrow = TRUE)
                    } else {
                        decisionMatrix <- matrix(c(
                            -criticalValues - theta * sqrt(self$nMax * informationRates),
                            criticalValues - theta * sqrt(self$nMax * informationRates)
                        ), nrow = 2, byrow = TRUE)
                    }
                } else {
                    shiftedFutilityBounds <- futilityBounds - theta * sqrt(self$nMax * informationRates[1:(kMax - 1)])
                    shiftedFutilityBounds[futilityBounds <= C_FUTILITY_BOUNDS_DEFAULT] <- C_FUTILITY_BOUNDS_DEFAULT
                    decisionMatrix <- matrix(c(
                        shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT,
                        criticalValues - theta * sqrt(self$nMax * informationRates)
                    ), nrow = 2, byrow = TRUE)
                }

                probs <- .getGroupSequentialProbabilities(decisionMatrix, informationRates)

                if (nrow(probs) == 3) {
                    .averageSampleNumber <- self$nMax - sum((probs[3, 1:(kMax - 1)] - probs[2, 1:(kMax - 1)] + probs[1, 1:(kMax - 1)]) *
                        (informationRates[kMax] - informationRates[1:(kMax - 1)]) * self$nMax)
                } else {
                    .averageSampleNumber <- self$nMax - sum((probs[5, 1:(kMax - 1)] -
                        probs[4, 1:(kMax - 1)] + probs[3, 1:(kMax - 1)] - probs[2, 1:(kMax - 1)] + probs[1, 1:(kMax - 1)]) *
                        (informationRates[kMax] - informationRates[1:(kMax - 1)]) * self$nMax)
                }

                if (sided == 2) {
                    if (nrow(probs) == 3) {
                        .calculatedPower <- sum(probs[3, 1:kMax] - probs[2, 1:kMax] + probs[1, 1:kMax])
                        .rejectPerStage <- probs[3, 1:kMax] - probs[2, 1:kMax] + probs[1, 1:kMax]
                    } else {
                        .calculatedPower <- sum(probs[5, 1:kMax] - probs[4, 1:kMax] + probs[1, 1:kMax])
                        .rejectPerStage <- probs[5, 1:kMax] - probs[4, 1:kMax] + probs[1, 1:kMax]
                        if (kMax > 1) {
                            .futilityPerStage <- probs[3, 1:kMax] - probs[2, 1:kMax]
                        }
                    }
                } else {
                    .calculatedPower <- sum(probs[3, 1:kMax] - probs[2, 1:kMax])
                    .rejectPerStage <- probs[3, 1:kMax] - probs[2, 1:kMax]
                    if (kMax > 1) {
                        .futilityPerStage <- probs[1, 1:(kMax - 1)]
                        .rejectPerStage <- .getNoEarlyEfficacyZeroCorrectedValues(self$.design, .rejectPerStage)
                    }
                }

                if (kMax > 1) {
                    if (nrow(probs) == 3) {
                        .earlyStop <- probs[3, 1:(kMax - 1)] - probs[2, 1:(kMax - 1)] + probs[1, 1:(kMax - 1)]
                    } else {
                        .earlyStop <- probs[5, 1:(kMax - 1)] - probs[4, 1:(kMax - 1)] + probs[3, 1:(kMax - 1)] -
                            probs[2, 1:(kMax - 1)] + probs[1, 1:(kMax - 1)]
                    }
                }
            }

            return(list(
                averageSampleNumber = .averageSampleNumber,
                calculatedPower = .calculatedPower,
                earlyStop = .earlyStop,
                rejectPerStage = .rejectPerStage,
                futilityPerStage = .futilityPerStage
            ))
        },
        .getOverallParameter = function(parameter) {
            if (is.null(parameter) || length(parameter) == 0) {
                return(rep(NA_real_, length(self$theta)))
            }

            overallParameter <- parameter
            overallParameter[is.na(overallParameter)] <- 0
            overallParameter <- colSums(overallParameter)
            return(overallParameter)
        }
    )
)

#'
#' @title
#' Coerce Power And Average Sample Number Result to a Data Frame
#'
#' @description
#' Returns the \code{\link{PowerAndAverageSampleNumberResult}} as data frame.
#'
#' @param x A \code{\link{PowerAndAverageSampleNumberResult}} object.
#' @inheritParams param_niceColumnNamesEnabled
#' @inheritParams param_includeAllParameters
#' @inheritParams param_three_dots
#'
#' @details
#' Coerces the \code{\link{PowerAndAverageSampleNumberResult}} object to a data frame.
#'
#' @template return_dataframe
#'
#' @examples
#' \dontrun{
#' data <- as.data.frame(getPowerAndAverageSampleNumber(getDesignGroupSequential()))
#' head(data)
#' dim(data)
#' }
#' 
#' @export
#'
#' @keywords internal
#'
as.data.frame.PowerAndAverageSampleNumberResult <- function(x, row.names = NULL,
        optional = FALSE, niceColumnNamesEnabled = FALSE, includeAllParameters = FALSE, ...) {
    parameterNames <- x$.getVisibleFieldNames()
    parameterNames <- parameterNames[parameterNames != "nMax"]
    dataFrame <- .getAsDataFrame(
        parameterSet = x,
        parameterNames = parameterNames,
        niceColumnNamesEnabled = niceColumnNamesEnabled,
        includeAllParameters = includeAllParameters
    )
    return(dataFrame)
}
