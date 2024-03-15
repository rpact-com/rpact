## |
## |  *Trial design set classes*
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

#' @include f_core_plot.R
#' @include f_core_utilities.R
NULL

#' @title
#' Get Design Set
#'
#' @description
#' Creates a trial design set object and returns it.
#'
#' @param ... \code{designs} or \code{design} and one or more design parameters, e.g., \code{deltaWT = c(0.1, 0.3, 0.4)}.
#' \itemize{
#'   \item \code{design} The master design (optional, you need to specify an
#'         additional parameter that shall be varied).
#'   \item \code{designs} The designs to compare (optional, you need to specify the variable \code{variedParameters}).
#' }
#'
#' @details
#' Specify a master design and one or more design parameters or a list of designs.
#'
#' @return Returns a \code{\link{TrialDesignSet}} object.
#' The following generics (R generic functions) are available for this result object:
#' \itemize{
#'   \item \code{\link[=names.TrialDesignSet]{names}} to obtain the field names,
#'   \item \code{\link[=length.TrialDesignSet]{length}} to obtain the number of design,
#'   \item \code{\link[=print.FieldSet]{print()}} to print the object,
#'   \item \code{\link[=summary.TrialDesignSet]{summary()}} to display a summary of the object,
#'   \item \code{\link[=plot.TrialDesignSet]{plot()}} to plot the object,
#'   \item \code{\link[=as.data.frame.TrialDesignSet]{as.data.frame()}} to coerce the object to a \code{\link[base]{data.frame}},
#'   \item \code{\link[=as.matrix.FieldSet]{as.matrix()}} to coerce the object to a \code{\link[base]{matrix}}.
#' }
#' @template how_to_get_help_for_generics
#'
#' @examples
#' # Example 1
#' design <- getDesignGroupSequential(
#'     alpha = 0.05, kMax = 6,
#'     sided = 2, typeOfDesign = "WT", deltaWT = 0.1
#' )
#' designSet <- getDesignSet()
#' designSet$add(design = design, deltaWT = c(0.3, 0.4))
#' \dontrun{
#' if (require(ggplot2)) plot(designSet, type = 1)
#' }
#'
#' # Example 2 (shorter script)
#' design <- getDesignGroupSequential(
#'     alpha = 0.05, kMax = 6,
#'     sided = 2, typeOfDesign = "WT", deltaWT = 0.1
#' )
#' designSet <- getDesignSet(design = design, deltaWT = c(0.3, 0.4))
#' \dontrun{
#' if (require(ggplot2)) plot(designSet, type = 1)
#' }
#'
#' # Example 3 (use of designs instead of design)
#' d1 <- getDesignGroupSequential(
#'     alpha = 0.05, kMax = 2,
#'     sided = 1, beta = 0.2, typeOfDesign = "asHSD",
#'     gammaA = 0.5, typeBetaSpending = "bsHSD", gammaB = 0.5
#' )
#' d2 <- getDesignGroupSequential(
#'     alpha = 0.05, kMax = 4,
#'     sided = 1, beta = 0.2, typeOfDesign = "asP",
#'     typeBetaSpending = "bsP"
#' )
#' designSet <- getDesignSet(
#'     designs = c(d1, d2),
#'     variedParameters = c("typeOfDesign", "kMax")
#' )
#' \dontrun{
#' if (require(ggplot2)) plot(designSet, type = 8, nMax = 20)
#' }
#'
#' @export
#'
getDesignSet <- function(...) {
    return(TrialDesignSet$new(...))
}

#'
#' @title
#' Trial Design Set Summary
#'
#' @description
#' Displays a summary of \code{\link{ParameterSet}} object.
#'
#' @param object A \code{\link{ParameterSet}} object.
#' @inheritParams param_digits
#' @inheritParams param_three_dots
#'
#' @details
#' Summarizes the trial designs.
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
summary.TrialDesignSet <- function(object, ..., type = 1, digits = NA_integer_) {
    .warnInCaseOfUnknownArguments(functionName = "summary.TrialDesignSet", ...)

    .assertIsTrialDesignSet(object)
    if (object$isEmpty()) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "cannot create summary because the design set is empty")
    }

    summaries <- list()
    for (design in object$designs) {
        s <- .createSummary(design, digits = digits)
        summaries <- c(summaries, s)
    }
    return(summaries)
}

#'
#' @name TrialDesignSet
#'
#' @title
#' Class for trial design sets.
#'
#' @description
#' \code{TrialDesignSet} is a class for creating a collection of different trial designs.
#'
#' @template field_designs
#' @template field_design
#' @template field_variedParameters
#'
#' @details
#' This object cannot be created directly; better use \code{\link[=getDesignSet]{getDesignSet()}}
#' with suitable arguments to create a set of designs.
#'
#' @seealso \code{\link[=getDesignSet]{getDesignSet()}}
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include f_core_plot.R
#' @include f_logger.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
TrialDesignSet <- R6::R6Class("TrialDesignSet",
    inherit = FieldSet,
    public = list(
        .plotSettings = NULL,
        designs = NULL,
        variedParameters = NULL,
        #
        # @param ... 'designs' OR 'design' and one or more design parameters, e.g., deltaWT = c(0.1, 0.3, 0.4)
        #
        initialize = function(...) {
            self$.plotSettings <- PlotSettings$new()
            self$designs <- list()
            self$variedParameters <- character()
            if (length(list(...)) > 0) {
                self$add(...)
            }
            if (length(self$designs) > 0) {
                masterDesign <- self$designs[[1]]
                if (inherits(masterDesign, "ParameterSet")) {
                    self$.plotSettings <- masterDesign$.plotSettings
                }
            }
        },
        getPlotSettings = function() {
            return(self$.plotSettings)
        },
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing trial design sets"
            self$.resetCat()
            self$.cat("Trial design set with ", length(self$designs), " designs\n\n",
                heading = 1,
                consoleOutputEnabled = consoleOutputEnabled
            )
            for (design in self$designs) {
                design$.show(showType = showType, consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        isEmpty = function() {
            return(length(self$designs) == 0)
        },
        getSize = function() {
            return(length(self$designs))
        },
        getDesignMaster = function() {
            if (length(self$designs) == 0) {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "no design master defined")
            }

            return(self$designs[[1]])
        },
        .validateDesignsArgument = function(designsToAdd, args) {
            if (!is.list(designsToAdd)) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'designsToAdd' must be a list")
            }

            if (length(designsToAdd) == 0) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'designsToAdd' must be not empty")
            }

            designsToAddValidated <- list()
            for (d in designsToAdd) {
                if (.isTrialDesign(d)) {
                    designsToAddValidated <- c(designsToAddValidated, d)
                } else {
                    parentDesign <- d[[".design"]]
                    if (is.null(parentDesign)) {
                        stop(
                            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                            "'designsToAdd' must be a list of trial designs (found '", .getClassName(d), "')"
                        )
                    }

                    warning("Only the parent design of ", .getClassName(d),
                        " was added to trial design set",
                        call. = FALSE
                    )
                    designsToAddValidated <- c(designsToAddValidated, parentDesign)
                }
            }

            varPar <- args[["variedParameters"]]
            if (!is.null(varPar) && length(varPar) > 0) {
                self$variedParameters <- c(self$variedParameters, varPar)
            }

            args <- args[!(names(args) %in% c("designs", "variedParameters"))]
            if (length(args) > 0) {
                warning("Argument", ifelse(length(args) > 1, "s", ""), " ",
                    .arrayToString(args, encapsulate = TRUE), " will be ignored ",
                    "because for 'designs' only argument 'variedParameters' will be respected",
                    call. = FALSE
                )
            }

            self$designs <- c(self$designs, designsToAddValidated)
        },
        addVariedParameters = function(varPar) {
            if (is.null(varPar) || !is.character(varPar)) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'varPar' must be a valid character vector")
            }

            self$variedParameters <- c(self$variedParameters, varPar)
        },
        .validateOptionalArguments = function(...) {
            args <- list(...)
            designsToAdd <- .getOptionalArgument(optionalArgumentName = "designs", ...)
            if (!is.null(designsToAdd)) {
                self$.validateDesignsArgument(designsToAdd = designsToAdd, args = args)
                return(NULL)
            }

            design <- .getOptionalArgument(optionalArgumentName = "design", ...)
            optionalArgumentsDefined <- (length(args) > 0)
            if (is.null(design) && !optionalArgumentsDefined) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "please specify a 'design' to add and/or a design parameter, ",
                    "e.g., deltaWT = c(0.1, 0.3, 0.4)"
                )
            }

            if (is.null(design) && optionalArgumentsDefined && length(self$designs) == 0) {
                stop(
                    C_EXCEPTION_TYPE_INCOMPLETE_ARGUMENTS,
                    "at least one design (master) must be defined in this ",
                    "design set to respect any design parameters"
                )
            }

            if (!is.null(design)) {
                self$designs <- c(self$designs, design)
            } else if (length(self$designs) > 0) {
                design <- self$designs[[1]] # use design master
            }

            if (!.isTrialDesign(design)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'design' (", .getClassName(design), ") must be an instance of class 'TrialDesign'"
                )
            }

            self$.getArgumentNames(validatedDesign = design, ...)

            invisible(design)
        },
        .getArgumentNames = function(validatedDesign, ...) {
            args <- list(...)
            if (length(args) == 0) {
                return(character())
            }

            argumentNames <- names(args)
            if (length(argumentNames) == 0) {
                warning("No argument names available for ", paste(args, collapse = ", "), call. = FALSE)
                return(character())
            }

            argumentNames <- argumentNames[nchar(argumentNames) != 0]
            argumentNames <- argumentNames[!(argumentNames %in% c("design", "designs", "singleDesign"))]

            visibleFieldNames <- validatedDesign$.getVisibleFieldNames()
            for (arg in argumentNames) {
                if (!(arg %in% visibleFieldNames)) {
                    stop(sprintf(paste0(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                        "'%s' does not contain a field with name '%s'"
                    ), .getClassName(validatedDesign), arg))
                }
            }

            invisible(argumentNames)
        },
        add = function(...) {
            "Adds 'designs' OR a 'design' and/or a design parameter, e.g., deltaWT = c(0.1, 0.3, 0.4)"
            design <- self$.validateOptionalArguments(...)

            args <- list(...)
            singleDesign <- args[["singleDesign"]]
            if (!is.null(singleDesign) && is.logical(singleDesign) && singleDesign) {
                return(invisible())
            }

            if (!is.null(design)) {
                d <- self$.createDesignVariants(validatedDesign = design, ...)
                self$designs <- c(self$designs, d)
            }
        },
        assertHaveEqualSidedValues = function() {
            if (length(self$designs) == 0) {
                return(invisible())
            }

            sided <- self$getDesignMaster()$sided
            for (design in self$designs) {
                if (sided != design$sided) {
                    stop(
                        C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                        "designs have different directions of alternative (design master is ",
                        ifelse(sided == 1, "one", "two"), " sided)"
                    )
                }
            }
        },
        .createDesignVariants = function(validatedDesign, ...) {
            .assertIsTrialDesign(validatedDesign)
            argumentNames <- self$.getArgumentNames(validatedDesign = validatedDesign, ...)

            if (length(argumentNames) == 0) {
                warning("Creation of design variants stopped: no valid design parameters found", call. = FALSE)
                return(list())
            }

            if (length(argumentNames) > 2) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "too many arguments (", .arrayToString(argumentNames, encapsulate = TRUE),
                    "): up to 2 design parameters are allowed"
                )
            }

            designVariants <- self$.createDesignVariantsRecursive(
                designMaster = validatedDesign,
                args = list(...), argumentIndex = 1, argumentNames = argumentNames
            )

            return(designVariants)
        },
        .designSettingExists = function(parameterName, parameterValue, numberOfArguments = 1,
                parameterNameBefore = NULL, parameterValueBefore = NULL) {
            if (length(self$designs) == 0) {
                return(FALSE)
            }

            for (design in self$designs) {
                if (!is.null(parameterNameBefore) && !is.null(parameterValueBefore)) {
                    if (design[[parameterNameBefore]] == parameterValueBefore &&
                            design[[parameterName]] == parameterValue) {
                        return(TRUE)
                    }
                } else if (numberOfArguments == 1) {
                    if (design[[parameterName]] == parameterValue) {
                        return(TRUE)
                    }
                }
            }
            return(FALSE)
        },
        .createDesignVariantsRecursive = function(designMaster, args, argumentIndex, argumentNames,
                parameterNameBefore = NULL, parameterValueBefore = NULL) {
            if (argumentIndex > length(argumentNames)) {
                return(list())
            }

            designVariants <- list()
            argumentName <- argumentNames[argumentIndex]
            self$variedParameters <- unique(c(self$variedParameters, argumentName))
            argumentValues <- args[[argumentName]]

            for (argumentValue in argumentValues) {
                if (self$.designSettingExists(argumentName, argumentValue,
                        numberOfArguments = length(argumentNames),
                        parameterNameBefore, parameterValueBefore
                    )) {
                    if (!is.null(parameterNameBefore) && !is.null(parameterValueBefore)) {
                        warning(sprintf(
                            "Argument ignored: there exists already a design with %s = %s (%s = %s)",
                            argumentName, argumentValue, parameterNameBefore, parameterValueBefore
                        ), call. = FALSE)
                    } else {
                        warning(sprintf(
                            "Argument ignored: there exists already a design with %s = %s",
                            argumentName, argumentValue
                        ), call. = FALSE)
                    }
                } else {
                    designMaster2 <- self$.createDesignVariant(
                        designMaster = designMaster,
                        argumentName = argumentName, argumentValue = argumentValue
                    )
                    if (argumentIndex == length(argumentNames)) {
                        if (is.null(parameterNameBefore) || is.null(parameterValueBefore)) {
                            .logDebug("Create design variant %s = %s", argumentName, argumentValue)
                        } else {
                            .logDebug(
                                "Create design variant %s = %s (%s = %s)", argumentName, argumentValue,
                                parameterNameBefore, parameterValueBefore
                            )
                        }
                        designVariants <- c(designVariants, designMaster2)
                    }
                    designCopies2 <- self$.createDesignVariantsRecursive(
                        designMaster = designMaster2,
                        args = args, argumentIndex = argumentIndex + 1, argumentNames = argumentNames,
                        parameterNameBefore = argumentName, parameterValueBefore = argumentValue
                    )
                    if (length(designCopies2) > 0) {
                        designVariants <- c(designVariants, designCopies2)
                    }
                }
            }

            return(designVariants)
        },
        .createDesignVariant = function(designMaster, argumentName, argumentValue) {
            if (.isTrialDesignGroupSequential(designMaster)) {
                defaultValues <- .getDesignGroupSequentialDefaultValues()
            } else if (.isTrialDesignInverseNormal(designMaster)) {
                defaultValues <- .getDesignInverseNormalDefaultValues()
            } else if (.isTrialDesignFisher(designMaster)) {
                defaultValues <- .getDesignFisherDefaultValues()
            }

            for (userDefinedParamName in designMaster$.getUserDefinedParameters()) {
                defaultValues[[userDefinedParamName]] <- designMaster[[userDefinedParamName]]
            }
            defaultValues[[argumentName]] <- argumentValue

            if (.isTrialDesignGroupSequential(designMaster)) {
                result <- getDesignGroupSequential(
                    kMax = defaultValues$kMax,
                    alpha = defaultValues$alpha,
                    beta = defaultValues$beta,
                    sided = defaultValues$sided,
                    informationRates = defaultValues$informationRates,
                    futilityBounds = defaultValues$futilityBounds,
                    typeOfDesign = defaultValues$typeOfDesign,
                    deltaWT = defaultValues$deltaWT,
                    optimizationCriterion = defaultValues$optimizationCriterion,
                    gammaA = defaultValues$gammaA,
                    typeBetaSpending = defaultValues$typeBetaSpending,
                    userAlphaSpending = defaultValues$userAlphaSpending,
                    userBetaSpending = defaultValues$userBetaSpending,
                    gammaB = defaultValues$gammaB,
                    tolerance = defaultValues$tolerance
                )
            } else if (.isTrialDesignInverseNormal(designMaster)) {
                result <- getDesignInverseNormal(
                    kMax = defaultValues$kMax,
                    alpha = defaultValues$alpha,
                    beta = defaultValues$beta,
                    sided = defaultValues$sided,
                    informationRates = defaultValues$informationRates,
                    futilityBounds = defaultValues$futilityBounds,
                    typeOfDesign = defaultValues$typeOfDesign,
                    deltaWT = defaultValues$deltaWT,
                    optimizationCriterion = defaultValues$optimizationCriterion,
                    gammaA = defaultValues$gammaA,
                    typeBetaSpending = defaultValues$typeBetaSpending,
                    userAlphaSpending = defaultValues$userAlphaSpending,
                    userBetaSpending = defaultValues$userBetaSpending,
                    gammaB = defaultValues$gammaB,
                    tolerance = defaultValues$tolerance
                )
            } else if (.isTrialDesignFisher(designMaster)) {
                result <- getDesignFisher(
                    kMax = defaultValues$kMax,
                    alpha = defaultValues$alpha,
                    method = defaultValues$method,
                    userAlphaSpending = defaultValues$userAlphaSpending,
                    informationRates = defaultValues$informationRates,
                    alpha0Vec = defaultValues$alpha0Vec,
                    sided = defaultValues$sided,
                    tolerance = defaultValues$tolerance,
                    iterations = defaultValues$iterations,
                    seed = defaultValues$seed
                )
            }
            result$.plotSettings <- designMaster$.plotSettings
            return(result)
        }
    )
)

#'
#' @title
#' Names of a Trial Design Set Object
#'
#' @description
#' Function to get the names of a \code{\link{TrialDesignSet}} object.
#'
#' @param x A \code{\link{TrialDesignSet}} object.
#'
#' @details
#' Returns the names of a design set that can be accessed by the user.
#'
#' @template return_names
#'
#' @examples
#' designSet <- getDesignSet(design = getDesignGroupSequential(), alpha = c(0.01, 0.05))
#' names(designSet)
#'
#' @export
#'
#' @keywords internal
#'
names.TrialDesignSet <- function(x) {
    return(x$.getVisibleFieldNames())
}

#'
#' @title
#' Length of Trial Design Set
#'
#' @description
#' Returns the number of designs in a \code{TrialDesignSet}.
#'
#' @param x A \code{\link{TrialDesignSet}} object.
#'
#' @details
#' Is helpful for iteration over all designs in a design set.
#'
#' @return Returns a non-negative \code{\link[base]{integer}} of length 1
#' representing the number of design in the \code{TrialDesignSet}.
#'
#' @examples
#' designSet <- getDesignSet(design = getDesignGroupSequential(), alpha = c(0.01, 0.05))
#' length(designSet)
#'
#' @export
#'
#' @keywords internal
#'
length.TrialDesignSet <- function(x) {
    return(length(x$designs))
}

.getHarmonizedColumnNames <- function(df1, df2) {
    colNames1 <- colnames(df1)
    colNames2 <- colnames(df2)
    if (length(colNames1) != length(colNames2)) {
        stop(
            C_EXCEPTION_TYPE_RUNTIME_ISSUE,
            "cannot harmonize column names of two data frames if number of columns is unequal (",
            length(colNames1), " != ", length(colNames2), ")"
        )
    }

    colNames <- character()
    for (i in 1:length(colNames1)) {
        colName1 <- colNames1[i]
        colName2 <- colNames2[i]
        if (!identical(colName1, colName2)) {
            vec1 <- unlist(strsplit(colName1, " "))
            vec2 <- unlist(strsplit(colName2, " "))
            colNames <- c(colNames, paste(base::intersect(vec1, vec2), collapse = " "))
        } else {
            colNames <- c(colNames, colName1)
        }
    }
    return(colNames)
}

.insertColumnName <- function(df, colName, colNames, at) {
    naCol <- rep(NA, nrow(df))
    if (at == 1) {
        colNames <- c(colName, colNames)
        df <- cbind(naCol, df)
    } else if (at <= length(colNames)) {
        colNames <- c(colNames[1:(at - 1)], colName, colNames[at:length(colNames)])
        df <- cbind(df[, 1:(at - 1)], naCol, df[, at:ncol(df)])
    } else {
        colNames <- c(colNames, colName)
        df <- cbind(df, naCol)
    }
    colnames(df) <- colNames
    return(list(df = df, colNames = colNames))
}

.getHarmonizedDataFrames <- function(df1, df2) {
    if (ncol(df1) == ncol(df2)) {
        return(list(df1 = df1, df2 = df2))
    }

    colNames1 <- colnames(df1)
    colNames2 <- colnames(df2)
    difference <- c(colNames1[!(colNames1 %in% colNames2)], colNames2[!(colNames2 %in% colNames1)])
    for (colName in difference) {
        if (colName %in% colNames1) {
            result <- .insertColumnName(df2, colName, colNames2, at = which(colNames1 == colName))
            colNames2 <- result$colNames
            df2 <- result$df
        } else {
            result <- .insertColumnName(df1, colName, colNames1, at = which(colNames2 == colName))
            colNames1 <- result$colNames
            df1 <- result$df
        }
    }
    return(list(df1 = df1, df2 = df2))
}

#'
#' @title
#' Coerce Trial Design Set to a Data Frame
#'
#' @description
#' Returns the \code{TrialDesignSet} as data frame.
#'
#' @param x A \code{\link{TrialDesignSet}} object.
#' @inheritParams param_niceColumnNamesEnabled
#' @inheritParams param_includeAllParameters
#' @param addPowerAndAverageSampleNumber If \code{TRUE}, power and average sample size will
#'        be added to data frame, default is \code{FALSE}.
#' @inheritParams param_theta
#' @inheritParams param_nMax
#' @inheritParams param_three_dots
#'
#' @details
#' Coerces the design set to a data frame.
#'
#' @template return_dataframe
#'
#' @examples
#' designSet <- getDesignSet(design = getDesignGroupSequential(), alpha = c(0.01, 0.05))
#' as.data.frame(designSet)
#'
#' @export
#'
#' @keywords internal
#'
as.data.frame.TrialDesignSet <- function(x,
        row.names = NULL,
        optional = FALSE,
        niceColumnNamesEnabled = FALSE,
        includeAllParameters = FALSE,
        addPowerAndAverageSampleNumber = FALSE,
        theta = seq(-1, 1, 0.02),
        nMax = NA_integer_, ...) {
    .assertIsTrialDesignSet(x)
    if (x$isEmpty()) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "cannot create data.frame because the design set is empty")
    }

    fCall <- match.call(expand.dots = FALSE)
    theta <- .assertIsValidThetaRange(thetaRange = theta, thetaAutoSeqEnabled = (as.character(fCall$theta)[1] != "seq"))

    if (addPowerAndAverageSampleNumber) {
        .assertAssociatedArgumentsAreDefined(
            addPowerAndAverageSampleNumber = addPowerAndAverageSampleNumber,
            theta = theta, nMax = nMax
        )
    }

    fisherDesignEnabled <- .isTrialDesignFisher(x$getDesignMaster())
    dataFrame <- NULL
    for (design in x$designs) {
        if (fisherDesignEnabled != .isTrialDesignFisher(design)) {
            stop(
                C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, "all trial designs must be from the same type ",
                "('", .getClassName(x$designs[[1]]), "' != '", .getClassName(design), ")'"
            )
        }

        suppressWarnings(df <- as.data.frame(design,
            niceColumnNamesEnabled = niceColumnNamesEnabled,
            includeAllParameters = includeAllParameters
        ))

        if (.isTrialDesignWithValidFutilityBounds(design)) {
            futilityBoundsName <- "futilityBounds"
            if (niceColumnNamesEnabled) {
                futilityBoundsName <- .getParameterCaption("futilityBounds", design, tableOutputEnabled = TRUE)
            }

            kMax <- design$kMax
            df[[futilityBoundsName]][kMax] <- design$criticalValues[kMax]
        }
        if (.isTrialDesignWithValidAlpha0Vec(design)) {
            alpha0VecName <- "alpha0Vec"
            if (niceColumnNamesEnabled) {
                alpha0VecName <- .getParameterCaption("alpha0Vec", design, tableOutputEnabled = TRUE)
            }

            kMax <- design$kMax
            df[[alpha0VecName]][kMax] <- design$criticalValues[kMax]
        }

        if (addPowerAndAverageSampleNumber) {
            results <- PowerAndAverageSampleNumberResult$new(design, theta = theta, nMax = nMax)
            suppressWarnings(df2 <- as.data.frame(results,
                niceColumnNamesEnabled = niceColumnNamesEnabled,
                includeAllParameters = FALSE # includeAllParameters
            ))
            df <- merge(df, df2, all.y = TRUE)
        }

        if (is.null(dataFrame)) {
            if (niceColumnNamesEnabled) {
                dataFrame <- cbind("Design number" = rep(1, nrow(df)), df)
            } else {
                dataFrame <- cbind(designNumber = rep(1, nrow(df)), df)
            }
        } else {
            if (niceColumnNamesEnabled) {
                df <- cbind("Design number" = rep(max(dataFrame$"Design number") + 1, nrow(df)), df)
            } else {
                df <- cbind(designNumber = rep(max(dataFrame$designNumber) + 1, nrow(df)), df)
            }

            result <- .getHarmonizedDataFrames(dataFrame, df)
            dataFrame <- result$df1
            df <- result$df2

            colNames <- .getHarmonizedColumnNames(dataFrame, df)
            colnames(dataFrame) <- colNames
            colnames(df) <- colNames

            dataFrame <- rbind(dataFrame, df)
        }
    }

    return(dataFrame)
}

#'
#' @title
#' Trial Design Set Plotting
#'
#' @description
#' Plots a trial design set.
#'
#' @param x The trial design set, obtained from \code{\link[=getDesignSet]{getDesignSet()}}.
#' @param y Not available for this kind of plot (is only defined to be compatible to the generic plot function).
#' @param main The main title.
#' @param xlab The x-axis label.
#' @param ylab The y-axis label.
#' @inheritParams param_palette
#' @inheritParams param_theta
#' @inheritParams param_nMax
#' @inheritParams param_plotPointsEnabled
#' @inheritParams param_showSource
#' @inheritParams param_plotSettings
#' @inheritParams param_legendPosition
#' @inheritParams param_grid
#' @param type The plot type (default = \code{1}). The following plot types are available:
#' \itemize{
#'   \item \code{1}: creates a 'Boundaries' plot
#'   \item \code{3}: creates a 'Stage Levels' plot
#'   \item \code{4}: creates a 'Error Spending' plot
#'   \item \code{5}: creates a 'Power and Early Stopping' plot
#'   \item \code{6}: creates an 'Average Sample Size and Power / Early Stop' plot
#'   \item \code{7}: creates an 'Power' plot
#'   \item \code{8}: creates an 'Early Stopping' plot
#'   \item \code{9}: creates an 'Average Sample Size' plot
#'   \item \code{"all"}: creates all available plots and returns it as a grid plot or list
#' }
#' @inheritParams param_three_dots_plot
#'
#' @details
#' Generic function to plot a trial design set.
#' Is, e.g., useful to compare different designs or design parameters visual.
#'
#' @template return_object_ggplot
#'
#' @examples
#' \dontrun{
#' design <- getDesignInverseNormal(
#'     kMax = 3, alpha = 0.025,
#'     typeOfDesign = "asKD", gammaA = 2,
#'     informationRates = c(0.2, 0.7, 1), typeBetaSpending = "bsOF"
#' )
#'
#' # Create a set of designs based on the master design defined above
#' # and varied parameter 'gammaA'
#' designSet <- getDesignSet(design = design, gammaA = 4)
#'
#' if (require(ggplot2)) plot(designSet, type = 1, legendPosition = 6)
#' }
#'
#' @export
#'
plot.TrialDesignSet <- function(x, y, ..., type = 1L, main = NA_character_,
        xlab = NA_character_, ylab = NA_character_, palette = "Set1",
        theta = seq(-1, 1, 0.02), nMax = NA_integer_, plotPointsEnabled = NA,
        legendPosition = NA_integer_, showSource = FALSE,
        grid = 1, plotSettings = NULL) {
    fCall <- match.call(expand.dots = FALSE)
    designSetName <- deparse(fCall$x)
    .assertGgplotIsInstalled()
    .assertIsSingleInteger(grid, "grid", validateType = FALSE)
    typeNumbers <- .getPlotTypeNumber(type, x)
    if (is.null(plotSettings)) {
        plotSettings <- .getGridPlotSettings(x, typeNumbers, grid)
    }
    p <- NULL
    plotList <- list()
    for (typeNumber in typeNumbers) {
        p <- .plotTrialDesignSet(
            x = x, y = y, type = typeNumber, main = main,
            xlab = xlab, ylab = ylab, palette = palette,
            theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
            legendPosition = .getGridLegendPosition(legendPosition, typeNumbers, grid),
            showSource = showSource, designSetName = designSetName,
            plotSettings = plotSettings, ...
        )
        .printPlotShowSourceSeparator(showSource, typeNumber, typeNumbers)
        if (length(typeNumbers) > 1) {
            caption <- .getPlotCaption(x, typeNumber, stopIfNotFound = TRUE)
            plotList[[caption]] <- p
        }
    }
    if (length(typeNumbers) == 1) {
        return(p)
    }

    return(.createPlotResultObject(plotList, grid))
}

.plotTrialDesignSet <- function(..., x, y, type = 1L, main = NA_character_,
        xlab = NA_character_, ylab = NA_character_, palette = "Set1",
        theta = seq(-1, 1, 0.02), nMax = NA_integer_, plotPointsEnabled = NA,
        legendPosition = NA_integer_, showSource = FALSE,
        designSetName = NA_character_, plotSettings = NULL) {
    .assertGgplotIsInstalled()
    if (!is.call(main) && !isS4(main) && !R6::is.R6(main)) { # TODO is.R6 added
        .assertIsSingleCharacter(main, "main", naAllowed = TRUE)
    }
    .assertIsSingleCharacter(xlab, "xlab", naAllowed = TRUE)
    .assertIsSingleCharacter(ylab, "ylab", naAllowed = TRUE)
    .assertIsSingleCharacter(palette, "palette", naAllowed = TRUE)
    theta <- .assertIsValidThetaRange(thetaRange = theta)
    .assertIsSingleNumber(nMax, "nMax", naAllowed = TRUE)
    .assertIsInClosedInterval(nMax, "nMax", lower = 1L, upper = 1e10)
    .assertIsSingleLogical(plotPointsEnabled, "plotPointsEnabled", naAllowed = TRUE)
    .assertIsValidLegendPosition(legendPosition)
    .assertIsSingleInteger(type, "type", naAllowed = FALSE, validateType = FALSE)

    parameterSet <- x
    designMaster <- parameterSet$getDesignMaster()
    .assertIsTrialDesign(designMaster)

    if (type == 1) {
        main <- if (!is.call(main) && !isS4(main) && !R6::is.R6(main) && is.na(main)) "Boundaries" else main
        xParameterName <- "informationRates"
        yParameterNames <- "criticalValues"

        if (designMaster$sided == 1 || (.isTrialDesignInverseNormalOrGroupSequential(designMaster) &&
                (designMaster$typeOfDesign == C_TYPE_OF_DESIGN_PT || grepl("^bs", designMaster$typeBetaSpending)))) {
            if (.isTrialDesignWithValidFutilityBounds(designMaster)) {
                yParameterNames <- c("futilityBounds", yParameterNames)
            }
            if (.isTrialDesignWithValidAlpha0Vec(designMaster)) {
                yParameterNames <- c("alpha0Vec", yParameterNames)
            }
        }
    } else if (type == 2) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "designs with undefined endpoint do not support plot type 2")
    } else if (type == 3) {
        main <- if (!is.call(main) && !isS4(main) && !R6::is.R6(main) && is.na(main)) "Stage Levels" else main
        xParameterName <- "informationRates"
        yParameterNames <- "stageLevels"
    } else if (type == 4) {
        main <- if (!is.call(main) && !isS4(main) && !R6::is.R6(main) && is.na(main)) "Error Spending" else main
        xParameterName <- "informationRates"
        yParameterNames <- c("alphaSpent")
        if (!.isTrialDesignFisher(designMaster) &&
                designMaster$typeBetaSpending != C_TYPE_OF_DESIGN_BS_NONE) {
            yParameterNames <- c(yParameterNames, "betaSpent")
            palette <- "Paired"
        }
        plotPointsEnabled <- ifelse(is.na(plotPointsEnabled), FALSE, plotPointsEnabled)
    } else if (type == 5) {
        if (!is.call(main) && !isS4(main) && !R6::is.R6(main) && is.na(main)) {
            main <- PlotSubTitleItems$new(title = "Power and Early Stopping")
            main$add("N", nMax, "max")
        }
        xParameterName <- "theta"
        yParameterNames <- c("overallEarlyStop", "calculatedPower")
    } else if (type == 6) {
        if (!is.call(main) && !isS4(main) && !R6::is.R6(main) && is.na(main)) {
            main <- PlotSubTitleItems$new(title = "Average Sample Size and Power / Early Stop")
            main$add("N", nMax, "max")
        }
        xParameterName <- "theta"
        yParameterNames <- c("averageSampleNumber", "overallEarlyStop", "calculatedPower")
    } else if (type == 7) {
        if (!is.call(main) && !isS4(main) && !R6::is.R6(main) && is.na(main)) {
            main <- PlotSubTitleItems$new(title = "Power")
            main$add("N", nMax, "max")
        }
        xParameterName <- "theta"
        yParameterNames <- "calculatedPower"
    } else if (type == 8) {
        if (!is.call(main) && !isS4(main) && !R6::is.R6(main) && is.na(main)) {
            main <- PlotSubTitleItems$new(title = "Early Stopping")
            main$add("N", nMax, "max")
        }
        xParameterName <- "theta"
        yParameterNames <- "overallEarlyStop"
    } else if (type == 9) {
        if (!is.call(main) && !isS4(main) && !R6::is.R6(main) && is.na(main)) {
            main <- PlotSubTitleItems$new(title = "Average Sample Size")
            main$add("N", nMax, "max")
        }
        xParameterName <- "theta"
        yParameterNames <- "averageSampleNumber"
    } else {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, ") is not allowed; must be 1, 2, ..., 9")
    }

    if (type >= 5 && type <= 9) {
        designSetName <- paste0(
            "getPowerAndAverageSampleNumber(", designSetName,
            ", theta = ", .reconstructSequenceCommand(theta), ", nMax = ", nMax, ")"
        )
    }

    xValues <- NA_real_
    if (xParameterName == "theta") {
        xValues <- theta
    }
    srcCmd <- .showPlotSourceInformation(
        objectName = designSetName,
        xParameterName = xParameterName,
        yParameterNames = yParameterNames,
        nMax = nMax,
        type = type,
        showSource = showSource,
        xValues = xValues
    )
    if (!is.null(srcCmd)) {
        if (.isSpecialPlotShowSourceArgument(showSource)) {
            return(invisible(srcCmd))
        }
        return(srcCmd)
    }

    p <- .plotParameterSet(
        parameterSet = parameterSet, designMaster = designMaster,
        xParameterName = xParameterName,
        yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
        palette = palette, theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
        legendPosition = legendPosition, plotSettings = plotSettings # , ...
    )

    p <- .addDecistionCriticalValuesToPlot(p = p, designMaster = designMaster, type = type, nMax = nMax)

    return(p)
}

.addDecistionCriticalValuesToPlot <- function(p, designMaster, type, nMax = NA_integer_) {
    if (type != 1 || !.isTrialDesignInverseNormalOrGroupSequential(designMaster)) {
        return(p)
    }

    data <- as.data.frame(designMaster)
    xyNames <- c("delayedInformationRates", "decisionCriticalValues")
    if (!all(xyNames %in% colnames(data))) {
        return(p)
    }

    data <- unique(na.omit(data[, xyNames]))
    data$legend <- rep("Decision critical value", nrow(data))

    if (!is.na(nMax) && nMax > 1) {
        data$delayedInformationRates <- data$delayedInformationRates * nMax
        tryCatch(
            {
                data$delayedInformationRates <- as.numeric(.formatSampleSizes(data$delayedInformationRates))
            },
            error = function(e) {
                warning("Failed to format delayed information rates on x-axis: ", e$message)
            }
        )
    }

    plotSettings <- designMaster$.plotSettings
    p <- p + ggplot2::geom_point(
        data = data,
        mapping = ggplot2::aes(
            x = .data[["delayedInformationRates"]],
            y = .data[["decisionCriticalValues"]],
            colour = .data[["legend"]]
        ),
        size = plotSettings$scaleSize(plotSettings$pointSize, TRUE),
        shape = 4, stroke = 1.25, show.legend = FALSE
    )

    for (i in 1:nrow(data)) {
        label <- paste0("(", round(data[i, 1], 3), ", ", round(data[i, 2], 3), ")")
        p <- p + ggplot2::annotate("text",
            x = data[i, 1], y = data[i, 2],
            label = label, vjust = plotSettings$scaleSize(3.0),
            size = plotSettings$scaleSize(2.5)
        )
    }

    try(suppressWarnings(suppressMessages(p <- p + ggplot2::scale_color_manual(values = c("#4daf4a", "#377eb8", "#e41a1c")))))
    return(p)
}
