## |
## |  *Time classes piecewise exponential survival multi-group simulations*
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
#' @include f_piecewise_survival_time_utilities.R
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
#' @template field_piecewiseSurvivalTime
#' @template field_lambdaControls
#' @template field_hazardRatios
#' @template field_medianTreatments
#' @template field_groupType
#'
#' @name PiecewiseSurvivalTimeMultiGroup
#'
#' @keywords internal
#'
PiecewiseSurvivalTimeMultiGroup <- R6::R6Class("PiecewiseSurvivalTimeMultiGroup",
    inherit = TimeDefinition,
    public = list(
        piecewiseSurvivalTime = NULL,
        lambdaControls = NULL,
        hazardRatios = NULL,
        medianTreatments = NULL,
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
            self$.setParameterType("medianTreatments", C_PARAM_NOT_APPLICABLE)
        },
        .initMedianParameters = function(medianTreatments) {
            if (!is.null(medianTreatments)) {
                dimnames(medianTreatments) <- dimnames(self$hazardRatios)
                self$medianTreatments <- medianTreatments
                self$.setParameterType("medianTreatments", C_PARAM_USER_DEFINED)
                self$.setParameterType("hazardRatios", C_PARAM_DERIVED)
            }
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
            controlMedianName <- if (self$groupType == "multi-arm") {
                "medianControl"
            } else {
                "medianControls"
            }
            controlMedians <- self[[controlMedianName]]
            if (!is.null(controlMedians)) {
                expectedControls <- if (self$groupType == "multi-arm") {
                    .getPiecewiseMedianHazards(controlMedians, controlMedianName, functionName)
                } else {
                    .getPiecewiseMedianHazards(controlMedians, controlMedianName, functionName, dimension = 2)
                }
                actualControls <- if (self$groupType == "multi-arm") {
                    self$lambdaControl
                } else {
                    self$lambdaControls
                }
                if (!identical(expectedControls, actualControls)) {
                    stopConflictingArguments("control medians must agree with their derived hazards; recreate the specification",
                        functionName = functionName, parameter = controlMedianName, value = controlMedians,
                        constraint = "control hazards derived from the supplied interval medians",
                        relatedParameter = "lambdaControls", relatedValue = self$lambdaControls,
                        diagnosticId = "survival.piecewise_median_hazard_consistency")
                }
            }
            if (!is.null(self$medianTreatments)) {
                expectedRatios <- .getPiecewiseMedianRatios(self$medianTreatments, self$lambdaControls, functionName)
                if (!identical(dimnames(expectedRatios), dimnames(self$hazardRatios)) ||
                        !isTRUE(all.equal(expectedRatios, self$hazardRatios, tolerance = 1e-12))) {
                    stopConflictingArguments("treatment medians must agree with their derived hazard ratios; recreate the specification",
                        functionName = functionName, parameter = "medianTreatments", value = self$medianTreatments,
                        constraint = "hazard ratios derived from treatment medians and control hazards",
                        relatedParameter = "hazardRatios", relatedValue = self$hazardRatios,
                        diagnosticId = "survival.piecewise_median_hazard_consistency")
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
            
            for (i in seq_len(self$getNumberOfSituations())) {
                if (!is.null(self$medianTreatments)) {
                    self$.cat("\nTreatment medians - ", dimnames(self$hazardRatios)[[3]][i],
                        " (exponential per interval):\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                    self$.showMatrix(matrix(self$medianTreatments[, , i], nrow = self$getNumberOfGroups(),
                        dimnames = dimnames(self$medianTreatments)[1:2]), digits, consoleOutputEnabled)
                }
                self$.cat("\nHazard ratios - ", dimnames(self$hazardRatios)[[3]][i], ":\n",
                    heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                self$.showMatrix(matrix(self$hazardRatios[, , i], nrow = self$getNumberOfGroups(),
                    dimnames = dimnames(self$hazardRatios)[1:2]), digits, consoleOutputEnabled)
            }
            
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
            controlMedians <- self[["medianControls"]]
            if (self$groupType == "multi-arm" && !is.null(self[["medianControl"]])) {
                controlMedians <- matrix(self$medianControl, nrow = 1,
                    dimnames = list("Control", colnames(controls)))
            }
            if (!is.null(controlMedians)) {
                self$.cat("\nControl medians (exponential per interval):\n",
                    heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                self$.showMatrix(controlMedians, digits, consoleOutputEnabled)
            }
            
            return(invisible(self))
        },
        .showMatrix = function(values, digits, consoleOutputEnabled) {
            if (is.na(digits)) {
                digits <- getOption("digits")
            }
            if (!consoleOutputEnabled) {
                tableColumns <- ncol(values) + 1L
                self$.cat(c("Group", colnames(values)), tableColumns = tableColumns,
                    consoleOutputEnabled = FALSE)
                self$.cat(rep("-----", tableColumns), tableColumns = tableColumns,
                    consoleOutputEnabled = FALSE)
                formattedValues <- format(values, digits = digits, trim = TRUE)
                for (i in seq_len(nrow(values))) {
                    self$.cat(c(rownames(values)[i], formattedValues[i, ]),
                        tableColumns = tableColumns, consoleOutputEnabled = FALSE)
                }
                self$.cat("\n", consoleOutputEnabled = FALSE)
                return(invisible())
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

#'
#' @name PiecewiseSurvivalTimeMultiArm
#'
#' @title
#' Piecewise Exponential Survival Time for Multiple Treatment Arms
#'
#' @description
#' Class for piecewise exponential survival times in patient-wise multi-arm
#' simulations, with a common control hazard for all active treatment arms.
#'
#' @template field_piecewiseSurvivalTime
#' @template field_lambdaControl_piecewise
#' @template field_lambdaControls
#' @template field_hazardRatios
#' @template field_medianControl_piecewise
#' @template field_medianTreatments
#' @template field_groupType
#'
#' @details
#' Create objects with [getPiecewiseSurvivalTimeMultiArm()]. The class inherits
#' from \code{PiecewiseSurvivalTimeMultiGroup} and \code{TimeDefinition}.
#' Rows of \code{lambdaControls} repeat the common control hazards for each
#' active arm. The dimensions of \code{hazardRatios} and \code{medianTreatments}
#' are active arm, interval, and situation, in that order.
#' Interval starts begin at zero and the last interval is open-ended.
#' Supplied interval medians parameterize exponential hazards through
#' \code{log(2) / median}; they are not the medians of the complete piecewise
#' survival distributions. Derived hazards and hazard ratios are retained
#' alongside the supplied medians.
#'
#' @seealso [getPiecewiseSurvivalTimeMultiArm()], [PiecewiseSurvivalTimeEnrichment]
#'
#' @keywords internal
#'
PiecewiseSurvivalTimeMultiArm <- R6::R6Class("PiecewiseSurvivalTimeMultiArm",
    inherit = PiecewiseSurvivalTimeMultiGroup,
    public = list(
        lambdaControl = NULL,
        medianControl = NULL,
        initialize = function(piecewiseSurvivalTime = NULL, lambdaControl = NULL, hazardRatios = NULL,
                medianControl = NULL, medianTreatments = NULL) {
            functionName <- "getPiecewiseSurvivalTimeMultiArm"
            times <- .validatePiecewiseStartTimes(piecewiseSurvivalTime, functionName)
            lambdaControl <- .getPiecewiseControlInput(times, lambdaControl, medianControl,
                "lambdaControl", "medianControl", functionName)
            .assertPiecewiseNumeric(lambdaControl, "lambdaControl", functionName)
            if (length(lambdaControl) != length(times)) {
                stopArgumentLengthOutOfBounds("'lambdaControl' must contain one numeric value per interval",
                    functionName = functionName, parameter = "lambdaControl", value = lambdaControl,
                    constraint = "one numeric value per interval", relatedParameter = "piecewiseSurvivalTime",
                    relatedValue = times, expectedLength = length(times), actualLength = length(lambdaControl),
                    diagnosticId = "survival.piecewise_control_length")
            }
            ratios <- .getPiecewiseEffectInput(hazardRatios, medianTreatments, functionName)
            medians <- NULL
            if (!is.null(medianTreatments)) {
                medians <- ratios
            }
            controls <- matrix(rep(lambdaControl, each = dim(ratios)[1]), nrow = dim(ratios)[1],
                dimnames = list(dimnames(ratios)[[1]], NULL))
            if (!is.null(medians)) {
                ratios <- .getPiecewiseMedianRatios(medians, controls, functionName)
            }
            super$initialize(times, controls, ratios, "multi-arm")
            self$lambdaControl <- as.numeric(lambdaControl)
            self$.setParameterType("lambdaControl", C_PARAM_USER_DEFINED)
            self$.setParameterType("lambdaControls", C_PARAM_DERIVED)
            self$.setParameterType("medianControl", C_PARAM_NOT_APPLICABLE)
            if (!is.null(medianControl)) {
                self$medianControl <- as.numeric(medianControl)
                self$.setParameterType("medianControl", C_PARAM_USER_DEFINED)
                self$.setParameterType("lambdaControl", C_PARAM_DERIVED)
            }
            self$.initMedianParameters(medians)
        }
    )
)

#'
#' @name PiecewiseSurvivalTimeEnrichment
#'
#' @title
#' Piecewise Exponential Survival Time for Population Enrichment
#'
#' @description
#' Class for piecewise exponential survival times in patient-wise population-
#' enrichment simulations, with control hazards and treatment effects specified
#' for disjoint population subsets.
#'
#' @template field_piecewiseSurvivalTime
#' @template field_lambdaControls
#' @template field_hazardRatios
#' @template field_medianControls
#' @template field_medianTreatments
#' @template field_groupType
#'
#' @details
#' Create objects with [getPiecewiseSurvivalTimeEnrichment()]. The class inherits
#' from \code{PiecewiseSurvivalTimeMultiGroup} and \code{TimeDefinition}.
#' Rows of \code{lambdaControls} and \code{medianControls} correspond to disjoint
#' population subsets. The dimensions of \code{hazardRatios} and
#' \code{medianTreatments} are subset, interval, and situation, in that order.
#' Interval starts begin at zero and the last interval is open-ended.
#' Supplied interval medians parameterize exponential hazards through
#' \code{log(2) / median}; they are not the medians of the complete piecewise
#' survival distributions. Derived hazards and hazard ratios are retained
#' alongside the supplied medians.
#'
#' @seealso [getPiecewiseSurvivalTimeEnrichment()], [PiecewiseSurvivalTimeMultiArm]
#'
#' @keywords internal
#'
PiecewiseSurvivalTimeEnrichment <- R6::R6Class("PiecewiseSurvivalTimeEnrichment",
    inherit = PiecewiseSurvivalTimeMultiGroup,
    public = list(
        medianControls = NULL,
        initialize = function(piecewiseSurvivalTime = NULL, lambdaControls = NULL, hazardRatios = NULL,
                medianControls = NULL, medianTreatments = NULL) {
            functionName <- "getPiecewiseSurvivalTimeEnrichment"
            times <- .validatePiecewiseStartTimes(piecewiseSurvivalTime, functionName)
            lambdaControls <- .getPiecewiseControlInput(times, lambdaControls, medianControls,
                "lambdaControls", "medianControls", functionName, dimension = 2)
            .assertPiecewiseNumeric(lambdaControls, "lambdaControls", functionName, dimension = 2)
            ratios <- .getPiecewiseEffectInput(hazardRatios, medianTreatments, functionName,
                groupNames = rownames(lambdaControls))
            medians <- NULL
            if (!is.null(medianTreatments)) {
                medians <- ratios
                ratios <- .getPiecewiseMedianRatios(medians, lambdaControls, functionName)
            }
            super$initialize(times, lambdaControls, ratios, "enrichment")
            self$.setParameterType("medianControls", C_PARAM_NOT_APPLICABLE)
            if (!is.null(medianControls)) {
                self$medianControls <- medianControls
                storage.mode(self$medianControls) <- "double"
                dimnames(self$medianControls) <- dimnames(self$lambdaControls)
                self$.setParameterType("medianControls", C_PARAM_USER_DEFINED)
                self$.setParameterType("lambdaControls", C_PARAM_DERIVED)
            }
            self$.initMedianParameters(medians)
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
