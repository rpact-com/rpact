## |
## |  *Time classes for piecewise exponential survival multi-group simulations*
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

            controls <- self$lambdaControls
            if (self$groupType == "multi-arm") {
                self$.cat("Control hazards (common to all comparisons):\n",
                    heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                controls <- controls[1, , drop = FALSE]
                rownames(controls) <- "Control"
                controlParameter <- "lambdaControl"
            } else {
                self$.cat("Control hazards:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                controlParameter <- "lambdaControls"
            }
            self$.showMatrix(controls, controlParameter, digits, consoleOutputEnabled)
            self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
            self$.cat("Details:\n\n", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
            self$.showPiecewiseParameterGroup(self$.getUserDefinedParameters(),
                "User defined parameters", digits, consoleOutputEnabled)
            self$.showPiecewiseParameterGroup(self$.getDefaultParameters(),
                "Default parameters", digits, consoleOutputEnabled)
            self$.showPiecewiseParameterGroup(self$.getDerivedParameters(),
                "Generated parameters", digits, consoleOutputEnabled)
            self$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)

            return(invisible(self))
        },
        .getNChar = function(includeAllParameters = FALSE) {
            if (includeAllParameters) {
                return(super$.getNChar(includeAllParameters = TRUE))
            }
            # Only parameter lines determine the colon column, never table headings
            # or captions of inactive fields. Long captions remain complete.
            parameters <- self$.getVisibleFieldNames()
            parameters <- setdiff(parameters, c("lambdaControls", "medianControls",
                "hazardRatios", "medianTreatments"))
            captions <- character()
            for (parameter in parameters) {
                if (self$isApplicableParameter(parameter)) {
                    captions <- c(captions, .getParameterCaption(parameter, self))
                }
            }
            return(min(40, max(c(1, nchar(captions)))))
        },
        .showPiecewiseParameterGroup = function(parameters, title, digits, consoleOutputEnabled) {
            # The common control matrix repeats lambdaControl in the multi-arm class.
            # Retain its metadata and technical output, but avoid duplicate normal output.
            if (self$groupType == "multi-arm") {
                parameters <- setdiff(parameters, "lambdaControls")
            }
            if (length(parameters) == 0) {
                self$.showParametersOfOneGroup(parameters, title,
                    consoleOutputEnabled = consoleOutputEnabled)
                return(invisible(NULL))
            }
            parameterOrder <- c("piecewiseSurvivalTime", "lambdaControl", "lambdaControls",
                "medianControl", "medianControls", "medianTreatments", "hazardRatios", "groupType")
            parameters <- c(intersect(parameterOrder, parameters), setdiff(parameters, parameterOrder))
            self$.cat(title, ":\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
            for (parameter in parameters) {
                values <- self[[parameter]]
                if (parameter %in% c("hazardRatios", "medianTreatments")) {
                    caption <- if (parameter == "hazardRatios") {
                        "Hazard ratios"
                    } else {
                        "Treatment medians"
                    }
                    for (i in seq_len(self$getNumberOfSituations())) {
                        suffix <- if (parameter == "medianTreatments") {
                            " (exponential per interval)"
                        } else {
                            ""
                        }
                        self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
                        self$.cat(caption, " - ", dimnames(values)[[3]][i], suffix, ":\n",
                            heading = 3, consoleOutputEnabled = consoleOutputEnabled)
                        self$.showMatrix(matrix(values[, , i], nrow = self$getNumberOfGroups(),
                            dimnames = dimnames(values)[1:2]), parameter, digits, consoleOutputEnabled)
                    }
                } else if (parameter %in% c("lambdaControls", "medianControls")) {
                    caption <- if (parameter == "lambdaControls") {
                        "Control hazards"
                    } else {
                        "Control medians (exponential per interval)"
                    }
                    self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
                    self$.cat(caption, ":\n", heading = 3,
                        consoleOutputEnabled = consoleOutputEnabled)
                    self$.showMatrix(values, parameter, digits, consoleOutputEnabled)
                } else {
                    self$.showParameter(parameter, consoleOutputEnabled = consoleOutputEnabled)
                }
            }
            self$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
            return(invisible(NULL))
        },
        .showMatrix = function(values, parameterName, digits, consoleOutputEnabled) {
            formatFunction <- .getParameterFormatFunction(parameterName, self)
            formattedValues <- .getParameterValueFormattedByFormatFunctionName(
                formatFunction, as.vector(values), self)
            # Only transpose the formatted view; the simulation representation stays unchanged.
            formattedValues <- t(matrix(formattedValues, nrow = nrow(values), dimnames = dimnames(values)))
            names(dimnames(formattedValues)) <- c("Interval", "Group")
            if (consoleOutputEnabled) {
                for (line in capture.output(print(noquote(formattedValues)))) {
                    self$.cat(line, "\n", consoleOutputEnabled = TRUE)
                }
            } else {
                .showMatrix(self, formattedValues, digits, consoleOutputEnabled = FALSE,
                    rowLabel = "Interval", escapeMarkdown = TRUE)
            }
            return(invisible(NULL))
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
