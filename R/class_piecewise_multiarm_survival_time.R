## |
## |  *Piecewise survival specifications for patient-wise simulations*
## |

.getPiecewiseSurvivalScenarios <- function(piecewiseSurvivalTime) {
    if (is.null(piecewiseSurvivalTime)) {
        return(NULL)
    }
    activeHazards <- piecewiseSurvivalTime$getActiveHazards()
    lapply(seq_len(dim(activeHazards)[3]), function(i) {
        list(
            intervalStarts = piecewiseSurvivalTime$piecewiseSurvivalTime,
            controlHazards = piecewiseSurvivalTime$lambdaControls,
            activeHazards = matrix(activeHazards[, , i],
                nrow = dim(activeHazards)[1], ncol = dim(activeHazards)[2])
        )
    })
}

.getPiecewiseIntervalNames <- function(piecewiseSurvivalTime) {
    n <- length(piecewiseSurvivalTime)
    vapply(seq_len(n), function(i) {
        if (i < n) {
            paste0(piecewiseSurvivalTime[i], " - <", piecewiseSurvivalTime[i + 1])
        } else {
            paste0(">=", piecewiseSurvivalTime[i])
        }
    }, character(1))
}

.validatePiecewiseStartTimes <- function(piecewiseSurvivalTime) {
    piecewiseSurvivalTime <- .assertIsNumericVector(
        piecewiseSurvivalTime, "piecewiseSurvivalTime", naAllowed = FALSE
    )
    if (length(piecewiseSurvivalTime) < 2) {
        stopIllegalArgument("'piecewiseSurvivalTime' must contain at least two interval start times",
            functionName = ".validatePiecewiseStartTimes", parameter = "piecewiseSurvivalTime")
    }
    if (piecewiseSurvivalTime[1] != 0) {
        stopIllegalArgument("the first value of 'piecewiseSurvivalTime' must be 0",
            functionName = ".validatePiecewiseStartTimes", parameter = "piecewiseSurvivalTime")
    }
    .assertValuesAreStrictlyIncreasing(piecewiseSurvivalTime, "piecewiseSurvivalTime")
    piecewiseSurvivalTime
}

#' Create an Array of Piecewise Hazard Ratios
#'
#' Combines one or more group-by-interval hazard-ratio matrices into an array
#' with dimensions group by interval by situation.
#'
#' @param ... Matrices, or one named list of matrices. Each matrix contains groups
#'   in rows and piecewise survival intervals in columns.
#' @param groupNames Optional group names.
#' @param intervalNames Optional interval names.
#' @return A numeric array with dimensions group by interval by situation.
#' @export
getPiecewiseHazardRatioArray <- function(..., groupNames = NULL, intervalNames = NULL) {
    situations <- list(...)
    if (length(situations) == 1 && is.list(situations[[1]]) && !is.matrix(situations[[1]]) &&
            !is.array(situations[[1]])) {
        situations <- situations[[1]]
    }
    if (length(situations) == 1 && is.array(situations[[1]]) && length(dim(situations[[1]])) == 3) {
        ans <- situations[[1]]
        if (any(!is.finite(ans)) || any(ans <= 0)) {
            stopIllegalArgument("all hazard ratios must be finite and greater than 0",
                functionName = "getPiecewiseHazardRatioArray", parameter = "hazardRatios")
        }
        if ((!is.null(groupNames) && length(groupNames) != dim(ans)[1]) ||
                (!is.null(intervalNames) && length(intervalNames) != dim(ans)[2])) {
            stopIllegalArgument("the numbers of group and interval names must match the array dimensions",
                functionName = "getPiecewiseHazardRatioArray", parameter = "groupNames")
        }
        dn <- dimnames(ans)
        if (is.null(dn)) dn <- vector("list", 3)
        if (!is.null(groupNames)) dn[[1]] <- groupNames
        if (!is.null(intervalNames)) dn[[2]] <- intervalNames
        if (is.null(dn[[1]])) dn[[1]] <- paste("Group", seq_len(dim(ans)[1]))
        if (is.null(dn[[2]])) dn[[2]] <- paste("Interval", seq_len(dim(ans)[2]))
        if (is.null(dn[[3]])) dn[[3]] <- paste("Situation", seq_len(dim(ans)[3]))
        names(dn) <- c("group", "interval", "situation")
        dimnames(ans) <- dn
        storage.mode(ans) <- "double"
        return(ans)
    }
    if (length(situations) == 0) {
        stopMissingArgument("at least one hazard-ratio matrix must be specified",
            functionName = "getPiecewiseHazardRatioArray", parameter = "...")
    }
    situations <- lapply(situations, function(x) {
        x <- as.matrix(x)
        storage.mode(x) <- "double"
        x
    })
    referenceDim <- dim(situations[[1]])
    if (length(referenceDim) != 2 || any(referenceDim == 0) ||
            any(vapply(situations, function(x) !identical(dim(x), referenceDim), logical(1)))) {
        stopIllegalArgument("all hazard-ratio matrices must have identical non-zero dimensions",
            functionName = "getPiecewiseHazardRatioArray", parameter = "hazardRatios")
    }
    values <- unlist(situations, use.names = FALSE)
    if (any(!is.finite(values)) || any(values <= 0)) {
        stopIllegalArgument("all hazard ratios must be finite and greater than 0",
            functionName = "getPiecewiseHazardRatioArray", parameter = "hazardRatios")
    }
    situationNames <- names(situations)
    if (is.null(situationNames) || any(!nzchar(situationNames))) {
        situationNames <- paste("Situation", seq_along(situations))
    }
    if (is.null(groupNames)) groupNames <- rownames(situations[[1]])
    if (is.null(groupNames)) groupNames <- paste("Group", seq_len(referenceDim[1]))
    if (is.null(intervalNames)) intervalNames <- colnames(situations[[1]])
    if (is.null(intervalNames)) intervalNames <- paste("Interval", seq_len(referenceDim[2]))
    if (length(groupNames) != referenceDim[1] || length(intervalNames) != referenceDim[2]) {
        stopIllegalArgument("the numbers of group and interval names must match the matrix dimensions",
            functionName = "getPiecewiseHazardRatioArray", parameter = "groupNames")
    }
    array(values,
        dim = c(referenceDim, length(situations)),
        dimnames = list(group = groupNames, interval = intervalNames, situation = situationNames))
}

.PiecewisePatientWiseSurvivalTime <- R6::R6Class(
    "PiecewisePatientWiseSurvivalTime",
    public = list(
        piecewiseSurvivalTime = NULL,
        lambdaControls = NULL,
        hazardRatios = NULL,
        groupType = NULL,
        initialize = function(piecewiseSurvivalTime, lambdaControls, hazardRatios, groupType) {
            self$piecewiseSurvivalTime <- .validatePiecewiseStartTimes(piecewiseSurvivalTime)
            intervalNames <- .getPiecewiseIntervalNames(self$piecewiseSurvivalTime)
            self$hazardRatios <- getPiecewiseHazardRatioArray(
                hazardRatios, intervalNames = intervalNames
            )
            self$lambdaControls <- as.matrix(lambdaControls)
            storage.mode(self$lambdaControls) <- "double"
            if (ncol(self$lambdaControls) != length(self$piecewiseSurvivalTime)) {
                stopIllegalArgument("the number of control-hazard columns must equal the number of intervals",
                    functionName = "PiecewisePatientWiseSurvivalTime", parameter = "lambdaControls")
            }
            if (nrow(self$lambdaControls) != dim(self$hazardRatios)[1]) {
                stopIllegalArgument("the numbers of control-hazard and hazard-ratio groups must agree",
                    functionName = "PiecewisePatientWiseSurvivalTime", parameter = "lambdaControls")
            }
            if (any(!is.finite(self$lambdaControls)) || any(self$lambdaControls <= 0)) {
                stopIllegalArgument("all control hazards must be finite and greater than 0",
                    functionName = "PiecewisePatientWiseSurvivalTime", parameter = "lambdaControls")
            }
            colnames(self$lambdaControls) <- intervalNames
            if (is.null(rownames(self$lambdaControls))) {
                rownames(self$lambdaControls) <- dimnames(self$hazardRatios)[[1]]
            }
            dimnames(self$hazardRatios)[[1]] <- rownames(self$lambdaControls)
            dimnames(self$hazardRatios)[[2]] <- intervalNames
            self$groupType <- groupType
        },
        getActiveHazards = function() {
            sweep(self$hazardRatios, c(1, 2), self$lambdaControls, `*`)
        },
        getNumberOfSituations = function() dim(self$hazardRatios)[3],
        getNumberOfGroups = function() dim(self$hazardRatios)[1],
        asDataFrame = function() {
            d <- dim(self$hazardRatios)
            dn <- dimnames(self$hazardRatios)
            out <- expand.grid(
                group = dn[[1]], interval = dn[[2]], situation = dn[[3]],
                KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
            )
            out$startTime <- rep(self$piecewiseSurvivalTime, each = d[1], times = d[3])
            out$endTime <- rep(c(self$piecewiseSurvivalTime[-1], Inf), each = d[1], times = d[3])
            out$lambdaControl <- as.vector(array(rep(self$lambdaControls, d[3]), dim = d))
            out$hazardRatio <- as.vector(self$hazardRatios)
            out$lambdaActive <- out$lambdaControl * out$hazardRatio
            out
        },
        show = function() {
            cat("Piecewise exponential ", self$groupType, " survival time\n\n", sep = "")
            cat("Control hazards:\n")
            print(self$lambdaControls)
            for (i in seq_len(dim(self$hazardRatios)[3])) {
                cat("\nHazard ratios - ", dimnames(self$hazardRatios)[[3]][i], ":\n", sep = "")
                print(matrix(
                    self$hazardRatios[, , i],
                    nrow = dim(self$hazardRatios)[1],
                    dimnames = dimnames(self$hazardRatios)[1:2]
                ))
            }
            invisible(self)
        },
        print = function(...) self$show()
    )
)

#' Piecewise Exponential Multi-Arm Survival Time
#'
#' Creates a validated piecewise exponential survival specification for a
#' patient-wise multi-arm simulation.
#'
#' @param piecewiseSurvivalTime Numeric interval start times beginning with 0.
#' @param lambdaControl Positive control hazards, one per interval.
#' @param hazardRatios One active-arm-by-interval matrix, a named list of such
#'   matrices (one per situation), or an active-arm-by-interval-by-situation
#'   array. In the array, situation is the slowest-changing dimension.
#' @return A \code{PiecewiseMultiArmSurvivalTime} object.
#' @examples
#' hazardRatios <- getPiecewiseHazardRatioArray(
#'     favorable = rbind(lowDose = c(0.9, 0.8), highDose = c(0.8, 0.6)),
#'     conservative = rbind(lowDose = c(1, 0.9), highDose = c(0.9, 0.75))
#' )
#' piecewiseTime <- getPiecewiseMultiArmSurvivalTime(
#'     piecewiseSurvivalTime = c(0, 6),
#'     lambdaControl = c(0.08, 0.12),
#'     hazardRatios = hazardRatios
#' )
#' piecewiseTime
#' @export
getPiecewiseMultiArmSurvivalTime <- function(piecewiseSurvivalTime, lambdaControl, hazardRatios) {
    if (!is.numeric(lambdaControl) || length(lambdaControl) != length(piecewiseSurvivalTime)) {
        stopIllegalArgument("'lambdaControl' must contain one numeric value per interval",
            functionName = "getPiecewiseMultiArmSurvivalTime", parameter = "lambdaControl")
    }
    hr <- getPiecewiseHazardRatioArray(hazardRatios,
        intervalNames = .getPiecewiseIntervalNames(.validatePiecewiseStartTimes(piecewiseSurvivalTime)))
    controls <- matrix(rep(lambdaControl, each = dim(hr)[1]), nrow = dim(hr)[1])
    rownames(controls) <- dimnames(hr)[[1]]
    obj <- .PiecewisePatientWiseSurvivalTime$new(
        piecewiseSurvivalTime, controls, hr, "multi-arm"
    )
    class(obj) <- c("PiecewiseMultiArmSurvivalTime", class(obj))
    obj
}

#' Piecewise Exponential Enrichment Survival Time
#'
#' Creates a validated piecewise exponential survival specification for a
#' patient-wise population-enrichment simulation.
#'
#' @param piecewiseSurvivalTime Numeric interval start times beginning with 0.
#' @param lambdaControls Positive subset-by-interval control-hazard matrix.
#' @param hazardRatios One subset-by-interval matrix, a named list of such
#'   matrices (one per situation), or a subset-by-interval-by-situation array.
#'   In the array, situation is the slowest-changing dimension.
#' @return A \code{PiecewiseEnrichmentSurvivalTime} object.
#' @examples
#' piecewiseTime <- getPiecewiseEnrichmentSurvivalTime(
#'     piecewiseSurvivalTime = c(0, 6),
#'     lambdaControls = rbind(markerPositive = c(0.08, 0.12),
#'         markerNegative = c(0.05, 0.09)),
#'     hazardRatios = list(
#'         favorable = rbind(markerPositive = c(0.65, 0.55),
#'             markerNegative = c(0.9, 0.8)),
#'         conservative = rbind(markerPositive = c(0.8, 0.7),
#'             markerNegative = c(1, 0.9))
#'     )
#' )
#' as.data.frame(piecewiseTime)
#' @export
getPiecewiseEnrichmentSurvivalTime <- function(piecewiseSurvivalTime, lambdaControls, hazardRatios) {
    obj <- .PiecewisePatientWiseSurvivalTime$new(
        piecewiseSurvivalTime, lambdaControls, hazardRatios, "enrichment"
    )
    class(obj) <- c("PiecewiseEnrichmentSurvivalTime", class(obj))
    obj
}

#' @export
as.data.frame.PiecewiseMultiArmSurvivalTime <- function(x, ...) x$asDataFrame()

#' @export
as.data.frame.PiecewiseEnrichmentSurvivalTime <- function(x, ...) x$asDataFrame()
