## |
## |  *Fisher information results*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Licensed under "GNU Lesser General Public License" version 3
## |

#'
#' @title Fisher Information
#'
#' @description
#' An R6 result class containing Fisher information calculated by
#' \code{\link[=getFisherInformation]{getFisherInformation()}} together with
#' its analysis stages, information type, and planning situations.
#'
#' @details
#' Objects of this class cannot be created directly. Use
#' \code{\link[=getFisherInformation]{getFisherInformation()}} instead.
#'
#' The public fields \code{information}, \code{type}, \code{stage}, and
#' \code{situations} describe the calculated values. The originating design
#' plan is retained privately so that the complete object can be piped into
#' \code{\link[=getFutilityBounds]{getFutilityBounds()}} without exposing or
#' duplicating that potentially large object in the printed result.
#'
#' Use \code{as.numeric()} to obtain the information values as a plain numeric
#' vector (column by column, i.e., all stages within each situation),
#' \code{as.matrix()} to obtain a stage-by-situation matrix, and
#' \code{as.data.frame()} to obtain a long-format data frame;
#' \code{is.finite()}, \code{is.infinite()}, \code{is.nan()}, and \code{is.na()} 
#' to check the information values for finiteness, infiniteness, NaN, and NA,
#' respectively, and \code{length()} to obtain the number of information values.
#'
#' @field information A numeric value, vector, or matrix with the calculated
#'   Fisher information.
#' @field type Either \code{"cumulative"} or \code{"stageWise"}.
#' @field stage Integer vector identifying the represented analysis stages.
#' @field situations Optional character vector identifying the represented
#'   planning situations.
#' @param x A \code{FisherInformation} object.
#' @param ... Additional arguments passed to the respective method.
#' @param row.names Ignored.
#' @param optional Ignored.
#'
#' @include class_core_parameter_set.R
#' @include f_core_utilities.R
#'
#' @keywords internal
#'
FisherInformation <- R6::R6Class(
    "FisherInformation",
    inherit = ParameterSet,
    private = list(
        designPlan = NULL
    ),
    public = list(
        information = NULL,
        type = NULL,
        stage = NULL,
        situations = NULL,
        initialize = function(information, type, stage, situations = NULL, designPlan = NULL) {
            self$information <- information
            self$type <- type
            self$stage <- stage
            self$situations <- situations
            private$designPlan <- designPlan

            super$initialize()
            self$.setParameterType("information", C_PARAM_GENERATED)
            self$.setParameterType("type", C_PARAM_DERIVED)
            self$.setParameterType("stage", C_PARAM_DERIVED)
            self$.setParameterType("situations", C_PARAM_DERIVED)
        },
        .getDesignPlan = function() {
            return(private$designPlan)
        },
        show = function(...) {
            print.FisherInformation(self, ...)
        },
        .toString = function(startWithUpperCase = FALSE) {
            return("Fisher information")
        }
    )
)

.getFisherInformationMatrix <- function(x) {
    values <- x$information
    stage <- x$stage

    if (!is.numeric(values) || length(values) == 0L) {
        stopIllegalArgument(
            .pQuote("information"), " field of ", .pQuote("x"), " must contain numeric values",
            functionName = "as.matrix.FisherInformation",
            parameter = "information",
            value = values
        )
    }
    if (!is.numeric(stage) || length(stage) == 0L || anyNA(stage)) {
        stopIllegalArgument(
            .pQuote("stage"), " field of ", .pQuote("x"), " must identify at least one stage",
            functionName = "as.matrix.FisherInformation",
            parameter = "stage",
            value = stage
        )
    }

    if (!is.null(dim(values))) {
        if (nrow(values) != length(stage)) {
            stopIllegalArgument(
                "number of rows in the ", .pQuote("information"), " field must equal the length of ",
                .pQuote("stage"),
                functionName = "as.matrix.FisherInformation",
                parameter = "information",
                value = values,
                relatedParameter = "stage",
                relatedValue = stage
            )
        }
        result <- matrix(
            as.numeric(values),
            nrow = nrow(values),
            ncol = ncol(values),
            dimnames = dimnames(values)
        )
    } else if (length(stage) == 1L) {
        result <- matrix(as.numeric(values), nrow = 1L)
    } else {
        if (length(values) != length(stage)) {
            stopIllegalArgument(
                "length of the ", .pQuote("information"), " field must equal the length of ",
                .pQuote("stage"), " for a single planning situation",
                functionName = "as.matrix.FisherInformation",
                parameter = "information",
                value = values,
                relatedParameter = "stage",
                relatedValue = stage
            )
        }
        result <- matrix(as.numeric(values), ncol = 1L)
    }

    if (length(stage) == nrow(result)) {
        rownames(result) <- paste0("Stage ", stage)
    }
    if (!is.null(x$situations)) {
        if (length(x$situations) != ncol(result)) {
            stopIllegalArgument(
                "length of the ", .pQuote("situations"), " field must equal the number of information columns",
                functionName = "as.matrix.FisherInformation",
                parameter = "situations",
                value = x$situations
            )
        }
        colnames(result) <- x$situations
    }
    return(result)
}

#' @rdname FisherInformation
#' @export
as.matrix.FisherInformation <- function(x, ...) {
    .warnInCaseOfUnknownArguments(functionName = "as.matrix", ...)
    return(.getFisherInformationMatrix(x))
}

#' @rdname FisherInformation
#' @export
as.double.FisherInformation <- function(x, ...) {
    .warnInCaseOfUnknownArguments(functionName = "as.numeric", ...)
    return(as.numeric(x$information))
}

#' @rdname FisherInformation
#' @export
is.finite.FisherInformation <- function(x) {
    return(is.finite(x$information))
}

#' @rdname FisherInformation
#' @export
is.infinite.FisherInformation <- function(x) {
    return(is.infinite(x$information))
}

#' @rdname FisherInformation
#' @export
is.nan.FisherInformation <- function(x) {
    return(is.nan(x$information))
}

#' @rdname FisherInformation
#' @export
is.na.FisherInformation <- function(x) {
    return(is.na(x$information))
}

#' @rdname FisherInformation
#' @export
as.data.frame.FisherInformation <- function(x, row.names = NULL, optional = FALSE, ...) {
    .warnInCaseOfUnknownArguments(functionName = "as.data.frame", ...)
    informationMatrix <- as.matrix(x)
    situations <- colnames(informationMatrix)
    if (is.null(situations)) {
        situations <- paste0("situation ", seq_len(ncol(informationMatrix)))
    }

    result <- data.frame(
        Stage = rep(x$stage, times = ncol(informationMatrix)),
        Situation = rep(situations, each = nrow(informationMatrix)),
        information = as.numeric(informationMatrix),
        check.names = FALSE,
        stringsAsFactors = FALSE
    )
    names(result)[3] <- "Fisher information"
    return(result)
}

#' @rdname FisherInformation
#' @export
length.FisherInformation <- function(x) {
    return(length(x$information))
}

#' @rdname FisherInformation
#' @export
dim.FisherInformation <- function(x) {
    return(dim(x$information))
}

#' @rdname FisherInformation
#' @export
rownames.FisherInformation <- function(x) {
    return(rownames(x$information))
}

#' @rdname FisherInformation
#' @export
print.FisherInformation <- function(x, ...) {
    typeDescription <- paste(
        .firstCharacterToUpperCase(.formatCamelCaseSingleWord(x$type, sep = "-")),
        "Fisher information"
    )
    informationMatrix <- as.matrix(x)
    nStages <- length(x$stage)
    nSituations <- ncol(informationMatrix)

    if (nStages == 1L) {
        heading <- paste0(typeDescription, " at stage ", x$stage)
        if (nSituations == 1L && !is.null(x$situations)) {
            heading <- paste0(heading, " (", x$situations, ")")
        } else if (nSituations > 1L) {
            heading <- paste0(heading, " by situation")
        }
    } else {
        heading <- paste0(typeDescription, " by stage")
        if (nSituations == 1L && !is.null(x$situations)) {
            heading <- paste0(heading, " (", x$situations, ")")
        } else if (nSituations > 1L) {
            heading <- paste0(heading, " and situation")
        }
    }
    cat(heading, ":\n", sep = "")

    if (nStages == 1L && nSituations > 1L) {
        output <- data.frame(
            Situation = colnames(informationMatrix),
            information = as.numeric(informationMatrix[1L, ]),
            check.names = FALSE
        )
        names(output)[2] <- "Fisher information"
        print(output, row.names = FALSE, ...)
    } else if (nStages > 1L && nSituations == 1L) {
        output <- data.frame(
            Stage = x$stage,
            information = as.numeric(informationMatrix[, 1L]),
            check.names = FALSE
        )
        names(output)[2] <- "Fisher information"
        print(output, row.names = FALSE, ...)
    } else {
        print.default(informationMatrix, ...)
    }

    return(invisible(x))
}

#' @rdname FisherInformation
#' @export
`[.FisherInformation` <- function(x, i, j, ..., drop = TRUE) {
    values <- as.matrix(x)

    if (missing(j)) {
        if (missing(i)) {
            return(values[])
        }
        return(values[i])
    }

    if (missing(i)) {
        return(values[, j, drop = drop])
    }

    return(values[i, j, drop = drop])
}
