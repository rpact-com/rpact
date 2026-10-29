## |
## |  *Time classes*
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

#' @include class_time.R
#' @include f_piecewise_survival_utilities.R
NULL

#'
#' @title
#' Piecewise Survival Times for Multiple Groups
#'
#' @description
#' Common time definition for multi-arm and enrichment survival specifications.
#' Groups, intervals, and situations occupy the first, second, and third hazard-ratio
#' dimensions. Use the specialized constructors to specify their statistical roles.
#'
#' @name PiecewiseSurvivalTimeMultiGroup
#' @keywords internal
#'
PiecewiseSurvivalTimeMultiGroup <- R6::R6Class("PiecewiseSurvivalTimeMultiGroup",
    inherit = TimeDefinition,
    public = list(
        piecewiseSurvivalTime = NULL,
        lambdaControls = NULL,
        hazardRatios = NULL,
        groupType = NULL,
        initialize = function(piecewiseSurvivalTime = NULL, lambdaControls = NULL,
                hazardRatios = NULL, groupType = NULL) {
            super$initialize()
            inputs <- .validatePiecewiseSurvivalInputs(piecewiseSurvivalTime, lambdaControls,
                hazardRatios, groupType, .getClassName(self))
            for (parameter in names(inputs)) {
                self[[parameter]] <- inputs[[parameter]]
                self$.setParameterType(parameter, C_PARAM_USER_DEFINED)
            }
            self$.setParameterType("groupType", C_PARAM_DERIVED)
        },
        .validate = function(functionName = .getClassName(self)) {
            inputs <- .validatePiecewiseSurvivalInputs(self$piecewiseSurvivalTime, self$lambdaControls,
                self$hazardRatios, self$groupType, functionName)
            expectedType <- if (inherits(self, "PiecewiseSurvivalTimeMultiArm")) {
                "multi-arm"
            } else if (inherits(self, "PiecewiseSurvivalTimeEnrichment")) {
                "enrichment"
            } else {
                self$groupType
            }
            if (!identical(self$groupType, expectedType)) {
                stopConflictingArguments("the piecewise survival group type must match its class",
                    functionName = functionName, parameter = "groupType", value = self$groupType,
                    constraint = "group type matches the statistical role of the class",
                    relatedParameter = "class", relatedValue = class(self),
                    diagnosticId = "survival.piecewise_class_role")
            }
            if (inherits(self, "PiecewiseSurvivalTimeMultiArm")) {
                .assertPiecewiseNumeric(self$lambdaControl, "lambdaControl", functionName)
                if (!identical(as.numeric(self$lambdaControl), as.numeric(self$lambdaControls[1, ]))) {
                    stopConflictingArguments("'lambdaControl' must agree with the common control hazards",
                        functionName = functionName, parameter = "lambdaControl", value = self$lambdaControl,
                        constraint = "one common control hazard per interval",
                        relatedParameter = "lambdaControls", relatedValue = self$lambdaControls,
                        diagnosticId = "survival.piecewise_common_control")
                }
            }
            # Normalized objects are passed to C++ without further dimensional checks.
            # Reject public-field edits that invalidate the canonical representation.
            for (parameter in names(inputs)) {
                if (!identical(self[[parameter]], inputs[[parameter]])) {
                    stopIllegalArgument("'", parameter, "' no longer has its validated representation; recreate the specification",
                        functionName = functionName, parameter = parameter, value = self[[parameter]],
                        constraint = "canonical validated piecewise survival representation",
                        diagnosticId = "survival.piecewise_modified_specification")
                }
            }
            return(invisible(self))
        },
        getActiveHazards = function() {
            self$.validate()
            return(sweep(self$hazardRatios, c(1, 2), self$lambdaControls, `*`))
        },
        getNumberOfSituations = function() {
            return(dim(self$hazardRatios)[3])
        },
        getNumberOfGroups = function() {
            return(dim(self$hazardRatios)[1])
        },
        asDataFrame = function() {
            self$.validate()
            dimensions <- dim(self$hazardRatios)
            dimensionNames <- dimnames(self$hazardRatios)
            result <- expand.grid(
                group = dimensionNames[[1]], interval = dimensionNames[[2]], situation = dimensionNames[[3]],
                KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
            result$startTime <- rep(self$piecewiseSurvivalTime, each = dimensions[1], times = dimensions[3])
            result$endTime <- rep(c(self$piecewiseSurvivalTime[-1], Inf),
                each = dimensions[1], times = dimensions[3])
            result$lambdaControl <- rep(as.vector(self$lambdaControls), dimensions[3])
            result$hazardRatio <- as.vector(self$hazardRatios)
            result$lambdaActive <- result$lambdaControl * result$hazardRatio
            return(result)
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
            return(invisible(self))
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            self$.validate()
            self$.resetCat()
            if (showType == 2) {
                super$.show(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
                return(invisible(self))
            }
            self$.cat("Piecewise exponential ", self$groupType, " survival time\n\n",
                heading = 1, consoleOutputEnabled = consoleOutputEnabled)
            controls <- self$lambdaControls
            if (self$groupType == "multi-arm") {
                self$.cat("Control hazards (common to all treatment arms):\n",
                    heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                controls <- controls[1, , drop = FALSE]
                rownames(controls) <- "Control"
            } else {
                self$.cat("Control hazards:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
            }
            self$.showMatrix(controls, digits, consoleOutputEnabled)
            for (i in seq_len(self$getNumberOfSituations())) {
                self$.cat("\nHazard ratios - ", dimnames(self$hazardRatios)[[3]][i], ":\n",
                    heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                self$.showMatrix(matrix(self$hazardRatios[, , i], nrow = self$getNumberOfGroups(),
                    dimnames = dimnames(self$hazardRatios)[1:2]), digits, consoleOutputEnabled)
            }
            return(invisible(self))
        },
        .showMatrix = function(values, digits, consoleOutputEnabled) {
            if (is.na(digits)) {
                digits <- getOption("digits")
            }
            for (line in capture.output(print(values, digits = digits))) {
                self$.cat(line, "\n", consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        .printAsDataFrame = function(parameterNames = NULL, niceColumnNamesEnabled = FALSE,
                includeAllParameters = FALSE, handleParameterNamesAsToBeExcluded = FALSE,
                lineBreakEnabled = FALSE) {
            for (line in capture.output(print(self$asDataFrame(), row.names = FALSE))) {
                self$.cat(line, "\n")
            }
        }
    )
)

PiecewiseSurvivalTimeMultiArm <- R6::R6Class("PiecewiseSurvivalTimeMultiArm",
    inherit = PiecewiseSurvivalTimeMultiGroup,
    public = list(
        lambdaControl = NULL,
        initialize = function(piecewiseSurvivalTime = NULL, lambdaControl = NULL, hazardRatios = NULL) {
            functionName <- "getPiecewiseSurvivalTimeMultiArm"
            times <- .validatePiecewiseStartTimes(piecewiseSurvivalTime, functionName)
            .assertPiecewiseNumeric(lambdaControl, "lambdaControl", functionName)
            if (length(lambdaControl) != length(times)) {
                stopArgumentLengthOutOfBounds("'lambdaControl' must contain one numeric value per interval",
                    functionName = functionName, parameter = "lambdaControl", value = lambdaControl,
                    constraint = "one numeric value per interval", relatedParameter = "piecewiseSurvivalTime",
                    relatedValue = times, expectedLength = length(times), actualLength = length(lambdaControl),
                    diagnosticId = "survival.piecewise_control_length")
            }
            ratios <- .normalizePiecewiseHazardRatios(hazardRatios, functionName = functionName)
            controls <- matrix(rep(lambdaControl, each = dim(ratios)[1]), nrow = dim(ratios)[1],
                dimnames = list(dimnames(ratios)[[1]], NULL))
            super$initialize(times, controls, ratios, "multi-arm")
            self$lambdaControl <- as.numeric(lambdaControl)
            self$.setParameterType("lambdaControl", C_PARAM_USER_DEFINED)
            self$.setParameterType("lambdaControls", C_PARAM_DERIVED)
        }
    )
)

PiecewiseSurvivalTimeEnrichment <- R6::R6Class("PiecewiseSurvivalTimeEnrichment",
    inherit = PiecewiseSurvivalTimeMultiGroup,
    public = list(
        initialize = function(piecewiseSurvivalTime = NULL, lambdaControls = NULL, hazardRatios = NULL) {
            super$initialize(piecewiseSurvivalTime, lambdaControls, hazardRatios, "enrichment")
        }
    )
)

#'
#' @title
#' Coerce a Multi-Group Piecewise Survival Specification to a Data Frame
#'
#' @description
#' Returns one row per group, interval, and situation, including control and active hazards.
#'
#' @param x A multi-arm or enrichment piecewise survival specification.
#' @param row.names Optional row names.
#' @param optional Unused; provided for compatibility with [as.data.frame()].
#' @param ... Further arguments, checked for unknown arguments.
#' @param niceColumnNamesEnabled Whether descriptive column names should be used.
#' @param includeAllParameters Unused; all specification rows are always included.
#'
#' @return A data frame in group-by-interval-by-situation order.
#'
#' @export
#'
as.data.frame.PiecewiseSurvivalTimeMultiGroup <- function(x, row.names = NULL, optional = FALSE,
        ..., niceColumnNamesEnabled = FALSE, includeAllParameters = FALSE) {
    .warnInCaseOfUnknownArguments(functionName = "as.data.frame", ...)
    result <- x$asDataFrame()
    if (niceColumnNamesEnabled) {
        names(result) <- c("Group", "Interval", "Situation", "Start time", "End time",
            "Control hazard", "Hazard ratio", "Active hazard")
    }
    if (!is.null(row.names)) {
        rownames(result) <- row.names
    }
    return(result)
}
