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
## |  File version: $Revision: 7620 $
## |  Last changed: $Date: 2024-02-09 12:57:37 +0100 (Fr, 09 Feb 2024) $
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
#' This object cannot be created directly; use \code{\link[=getPowerAndAverageSampleNumber]{getPowerAndAverageSampleNumber()}}
#' with suitable arguments to create it.
#'
#' @include class_core_parameter_set.R
#' @include class_design.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
PowerAndAverageSampleNumberResult <- setRefClass("PowerAndAverageSampleNumberResult",
    contains = "ParameterSet",
    fields = list(
        .design = "TrialDesign",
        nMax = "numeric",
        theta = "numeric",
        averageSampleNumber = "numeric",
        calculatedPower = "numeric",
        overallEarlyStop = "numeric",
        earlyStop = "matrix",
        overallReject = "numeric",
        rejectPerStage = "matrix",
        overallFutility = "numeric",
        futilityPerStage = "matrix"
    ),
    methods = list(
        initialize = function(design, theta = seq(-1, 1, 0.05), nMax = 100L, ...) {
            callSuper(.design = design, theta = theta, nMax = nMax, ...)
            theta <<- .assertIsValidThetaRange(thetaRange = theta, thetaAutoSeqEnabled = FALSE)
            .initPowerAndAverageSampleNumber()
        },
        clone = function() {
            return(PowerAndAverageSampleNumberResult(design = .self$.design, theta = .self$theta, nMax = .self$nMax))
        },
        show = function(showType = 1, digits = NA_integer_) {
            .show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing a power and average sample size (ASN) result"
            .resetCat()
            if (showType == 2) {
                callSuper(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                .cat("Power and average sample size (ASN):\n\n",
                    heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                .showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showParametersOfOneGroup(.getGeneratedParameters(), "Output",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                .showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
                if (.design$kMax > 1) {
                    .cat("Legend:\n",
                        heading = 2,
                        consoleOutputEnabled = consoleOutputEnabled
                    )
                    if (.design$kMax > 1) {
                        .cat("  [k]: values at stage k\n", consoleOutputEnabled = consoleOutputEnabled)
                    }
                    .cat("\n", consoleOutputEnabled = consoleOutputEnabled)
                }
            }
        },
        .toString = function(startWithUpperCase = FALSE) {
            s <- "power and average sample size (ASN)"
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
        },
        .initPowerAndAverageSampleNumber = function() {
            .assertIsTrialDesignInverseNormalOrGroupSequential(.design)
            .assertIsValidSidedParameter(.design$sided)

            if (nMax <= 0) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'nMax' must be an integer > 0")
            }

            .setParameterType("nMax", ifelse(nMax == C_NA_MAX_DEFAULT, C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))

            thetaIsDefault <- length(theta) == length(C_POWER_ASN_THETA_DEFAULT) &&
                sum(theta == C_POWER_ASN_THETA_DEFAULT) == length(theta)
            .setParameterType("theta", ifelse(thetaIsDefault, C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))

            kMax <- .design$kMax

            # initialization
            numberOfThetas <- length(theta)

            averageSampleNumber <<- rep(NA_real_, numberOfThetas)
            .setParameterType("averageSampleNumber", C_PARAM_GENERATED)

            calculatedPower <<- rep(NA_real_, numberOfThetas)
            .setParameterType("calculatedPower", C_PARAM_GENERATED)

            earlyStop <<- matrix(NA_real_, kMax, numberOfThetas)
            .setParameterType("earlyStop", C_PARAM_GENERATED)

            rejectPerStage <<- matrix(NA_real_, kMax, numberOfThetas)
            .setParameterType("rejectPerStage", C_PARAM_GENERATED)

            futilityPerStage <<- matrix(NA_real_, kMax - 1, numberOfThetas)
            .setParameterType("futilityPerStage", C_PARAM_GENERATED)

            rowNames <- paste("stage =", c(1:kMax))
            rownames(earlyStop) <<- rowNames
            rownames(rejectPerStage) <<- rowNames
            if (kMax > 1) {
                rownames(futilityPerStage) <<- rowNames[1:(kMax - 1)]
            }

            for (i in 1:numberOfThetas) {
                result <- .getPowerAndAverageSampleNumber(theta = theta[i])

                averageSampleNumber[i] <<- result$averageSampleNumber
                calculatedPower[i] <<- result$calculatedPower
                earlyStop[1:(kMax - 1), i] <<- result$earlyStop[1:(kMax - 1)]
                rejectPerStage[, i] <<- result$rejectPerStage[1:kMax]
                futilityPerStage[, i] <<- result$futilityPerStage[1:(kMax - 1)]
            }

            overallEarlyStop <<- .getOverallParameter(earlyStop)
            .setParameterType("overallEarlyStop", C_PARAM_GENERATED)

            overallReject <<- .getOverallParameter(rejectPerStage)
            .setParameterType("overallReject", C_PARAM_GENERATED)

            overallFutility <<- .getOverallParameter(futilityPerStage)
            .setParameterType("overallFutility", C_PARAM_GENERATED)
        },
        .getPowerAndAverageSampleNumber = function(theta) {
            kMax <- .design$kMax
            futilityBounds <- .design$futilityBounds
            informationRates <- .design$informationRates
            criticalValues <- .design$criticalValues
            sided <- .design$sided
            delayedInformation <- .design$delayedInformation

            .earlyStop <- rep(NA_real_, kMax)
            .futilityPerStage <- rep(NA_real_, kMax)

            if (!any(is.na(delayedInformation))) {
                contRegionLower <- futilityBounds
                contRegionUpper <- criticalValues
                decisionCriticalValues <- .design$decisionCriticalValues
                probs <- .calculateDecisionProbabilities(
                    sqrtShift = sqrt(nMax) * theta,
                    informationRates, delayedInformation, contRegionUpper, contRegionLower, decisionCriticalValues
                )

                .averageSampleNumber <- nMax - sum(probs$stoppingProbabilities *
                    (informationRates[kMax] - delayedInformation - informationRates[1:(kMax - 1)]) * nMax)
                .calculatedPower <- probs$power[kMax]
                .rejectPerStage <- probs$rejectionProbabilities
                .earlyStop <- probs$stoppingProbabilities
                .futilityPerStage <- probs$futilityProbabilities
            } else {
                if (sided == 2) {
                    if (.design$typeOfDesign == C_TYPE_OF_DESIGN_PT || !is.null(.design$typeBetaSpending) && .design$typeBetaSpending != "none") {
                        futilityBounds[is.na(futilityBounds)] <- 0
                        decisionMatrix <- matrix(c(
                            -criticalValues - theta * sqrt(nMax * informationRates),
                            c(-futilityBounds - theta * sqrt(nMax * informationRates[1:(kMax - 1)]), 0),
                            c(futilityBounds - theta * sqrt(nMax * informationRates[1:(kMax - 1)]), 0),
                            criticalValues - theta * sqrt(nMax * informationRates)
                        ), nrow = 4, byrow = TRUE)
                    } else {
                        decisionMatrix <- matrix(c(
                            -criticalValues - theta * sqrt(nMax * informationRates),
                            criticalValues - theta * sqrt(nMax * informationRates)
                        ), nrow = 2, byrow = TRUE)
                    }
                } else {
                    shiftedFutilityBounds <- futilityBounds - theta * sqrt(nMax * informationRates[1:(kMax - 1)])
                    shiftedFutilityBounds[futilityBounds <= C_FUTILITY_BOUNDS_DEFAULT] <- C_FUTILITY_BOUNDS_DEFAULT
                    decisionMatrix <- matrix(c(
                        shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT,
                        criticalValues - theta * sqrt(nMax * informationRates)
                    ), nrow = 2, byrow = TRUE)
                }

                probs <- .getGroupSequentialProbabilities(decisionMatrix, informationRates)

                if (nrow(probs) == 3) {
                    .averageSampleNumber <- nMax - sum((probs[3, 1:(kMax - 1)] - probs[2, 1:(kMax - 1)] + probs[1, 1:(kMax - 1)]) *
                        (informationRates[kMax] - informationRates[1:(kMax - 1)]) * nMax)
                } else {
                    .averageSampleNumber <- nMax - sum((probs[5, 1:(kMax - 1)] -
                        probs[4, 1:(kMax - 1)] + probs[3, 1:(kMax - 1)] - probs[2, 1:(kMax - 1)] + probs[1, 1:(kMax - 1)]) *
                        (informationRates[kMax] - informationRates[1:(kMax - 1)]) * nMax)
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
                        .rejectPerStage <- .getNoEarlyEfficacyZeroCorrectedValues(.design, .rejectPerStage)
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
                return(rep(NA_real_, length(theta)))
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
#' data <- as.data.frame(getPowerAndAverageSampleNumber(getDesignGroupSequential()))
#' head(data)
#' dim(data)
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
