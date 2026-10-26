## |
## |  *Result comparison*
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

#'
#' @title
#' Get Comparison Data
#'
#' @description
#' Combines existing designs, sample size or power plans, and simulation results
#' into a data frame without recalculating or modifying the objects.
#'
#' @param objects A non-empty list of objects inheriting from `TrialDesign`,
#'   `TrialDesignPlan`, or `SimulationResults`. Optional names are retained in
#'   the column `objectName`.
#' @param ... Further arguments. Currently unused.
#' @param niceColumnNamesEnabled If `TRUE`, use descriptive column names.
#' @param includeAllParameters If `TRUE`, include all parameters supported by
#'   the existing data-frame methods of the individual objects.
#'
#' @details
#' Each object receives an `objectNumber`, repeated for all its rows. Plans and
#' simulation results are supplemented with their associated design parameters,
#' matched by analysis stage. The situation column numbers rows within each stage. Columns are matched by technical names; missing
#' values are filled with `NA`. `sampleSizes` is harmonized to `numberOfSubjects`
#' when this does not overwrite another column. Conflicting non-numeric column
#' types are represented as character values. No master design or automatic
#' parameter variation is implied. Original objects are not stored in the result.
#'
#' @return An ordinary `data.frame` with identifiers, object type, endpoint,
#'   calculation target, and input and result parameters.
#'
#' @examples
#' \dontrun{
#' objects <- list(
#'     fixed = getDesignFixed(),
#'     sequential = getDesignGroupSequential(kMax = 2)
#' )
#' head(getComparisonData(objects))
#' }
#'
#' @export
#'
getComparisonData <- function(
        objects,
        ...,
        niceColumnNamesEnabled = FALSE,
        includeAllParameters = FALSE) {
    .warnInCaseOfUnknownArguments(functionName = "getComparisonData", ...)
    .assertIsSingleLogical(niceColumnNamesEnabled, "niceColumnNamesEnabled")
    .assertIsSingleLogical(includeAllParameters, "includeAllParameters")
    if (!is.list(objects) || is.data.frame(objects) || length(objects) == 0) {
        stop("'objects' must be a non-empty list of rpact designs, plans, or simulation results")
    }
    frames <- lapply(seq_along(objects), function(i) {
        obj <- objects[[i]]
        if (!inherits(obj, c("TrialDesign", "TrialDesignPlan", "SimulationResults"))) {
            stop("Unsupported object at position ", i, ": ", paste(class(obj), collapse = "/"))
        }
        df <- as.data.frame(obj,
            niceColumnNamesEnabled = FALSE,
            includeAllParameters = includeAllParameters
        )
        if (!inherits(obj, "TrialDesign")) {
            designData <- as.data.frame(obj$.design,
                niceColumnNamesEnabled = FALSE,
                includeAllParameters = includeAllParameters
            )
            missing <- setdiff(names(designData), names(df))
            if (nrow(designData) == 1L) {
                indices <- rep(1L, nrow(df))
            } else if ("stages" %in% names(df) && "stages" %in% names(designData)) {
                indices <- match(df$stages, designData$stages)
                if (anyNA(indices)) stop("Cannot match analysis stages of object ", i)
            } else {
                stop("Cannot match the associated design to the rows of object ", i)
            }
            df[missing] <- designData[indices, missing, drop = FALSE]
        }
        if ("sampleSizes" %in% names(df) && !"numberOfSubjects" %in% names(df)) {
            names(df)[names(df) == "sampleSizes"] <- "numberOfSubjects"
        }
        className <- class(obj)[1L]
        endpoint <- if (inherits(obj, "TrialDesign")) {
            "Undefined"
        } else {
            if (grepl("Means", className)) "Means" 
            else if (grepl("Rates", className)) "Rates" 
            else if (grepl("Survival", className)) "Survival" 
            else if (grepl("Count", className)) "Counts" 
            else "Undefined"
        }
        calculationTarget <- if (inherits(obj, "TrialDesign")) {
            "Design"
        } else {
            if (inherits(obj, "SimulationResults")) {
                if (grepl("MultiArm", className)) "Multi-arm power simulation" 
                else "Power simulation"
            } else if (obj$.isSampleSizeObject()) "Sample size" 
            else "Power"
        }
        info <- data.frame(
            objectNumber = rep(i, nrow(df)),
            objectType = rep(className, nrow(df)), endpoint = rep(endpoint, nrow(df)),
            calculationTarget = rep(calculationTarget, nrow(df)), stringsAsFactors = FALSE
        )
        if (!is.null(names(objects))) {
            label <- names(objects)[i]
            if (is.na(label) || !nzchar(label)) label <- paste("Object", i)
            info$objectName <- rep(label, nrow(df))
        }
        stageColumn <- intersect(c("stages", "stage"), names(df))
        info$situation <- if (length(stageColumn)) {
            as.integer(ave(seq_len(nrow(df)), df[[stageColumn[1L]]], FUN = seq_along))
        } else {
            seq_len(nrow(df))
        }
        cbind(info, df, row.names = NULL)
    })
    columnNames <- unique(unlist(lapply(frames, names), use.names = FALSE))
    frames <- lapply(frames, function(df) {
        for (name in setdiff(columnNames, names(df))) df[[name]] <- rep(NA, nrow(df))
        df[columnNames]
    })
    for (name in columnNames) {
        columns <- lapply(frames, `[[`, name)
        numericCompatible <- all(vapply(columns, function(x) is.numeric(x) || is.logical(x), logical(1)))
        sameType <- length(unique(vapply(columns, function(x) paste(class(x), collapse = "/"), character(1)))) == 1L
        if (!numericCompatible && !sameType) {
            frames <- lapply(frames, function(df) {
                df[[name]] <- as.character(df[[name]])
                df
            })
        }
    }
    result <- do.call(rbind, frames)
    rownames(result) <- NULL
    if (niceColumnNamesEnabled) {
        labels <- as.list(C_TABLE_COLUMN_NAMES)
        labels[c("objectNumber", "objectName", "objectType", "endpoint", "calculationTarget")] <-
            c("Object number", "Object name", "Object type", "Endpoint", "Calculation target")
        names(result) <- vapply(names(result), function(name) {
            label <- labels[[name]]
            if (is.null(label)) name else label
        }, character(1))
        names(result) <- make.unique(names(result))
    }
    result
}
