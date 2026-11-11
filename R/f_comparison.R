## |
## |  rpact: Confirmatory Adaptive Clinical Trial Design and Analysis
## |  This file is part of the rpact R package.
## |
## |  RPACT — Statistical Tools for Drug Development
## |
## |  Authors: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  SPDX-FileCopyrightText: RPACT GmbH, Germany <info@rpact.com>
## |  SPDX-License-Identifier: LGPL-3.0-only
## |
## |  Licensed under the GNU Lesser General Public License, version 3.
## |  License text: https://www.gnu.org/licenses/lgpl-3.0.html
## |
## |  Package documentation: https://www.rpact.org
## |  Resources and customer portal: https://connect.rpact.app
## |  Consulting and services: https://www.rpact.com
## |  Contact: info@rpact.com
## |

.getComparisonColumnMapping <- function(object, hasInterimAnalyses = FALSE) {
    mapping <- c(sampleSizes = "numberOfSubjects")
    design <- if (.isComparisonObject(object)) {
        .getTrialDesign(object)
    } else {
        NULL
    }
    if (hasInterimAnalyses && inherits(object, "TrialDesignPlan") &&
            !is.null(design) && design$kMax == 1L) {
        # Extend this named mapping for further equivalent fixed-plan columns.
        mapping <- c(mapping, nFixed = "numberOfSubjects",
            nFixed1 = "numberOfSubjects1", nFixed2 = "numberOfSubjects2")
    }
    return(mapping)
}

.harmonizeComparisonColumns <- function(data, mapping) {
    for (source in intersect(names(mapping), names(data))) {
        target <- mapping[[source]]
        if (identical(source, target)) {
            next
        }
        if (!target %in% names(data)) {
            names(data)[names(data) == source] <- target
        } else {
            sourceValues <- data[[source]]
            targetValues <- data[[target]]
            shared <- !is.na(sourceValues) & !is.na(targetValues)
            if (!any(shared) || identical(sourceValues[shared], targetValues[shared])) {
                missing <- is.na(targetValues)
                targetValues[missing] <- sourceValues[missing]
                data[[target]] <- targetValues
                data[[source]] <- NULL
            }
        }
        # Conflicting non-missing targets retain both columns without overwriting values.
    }
    return(data)
}

.isComparisonObject <- function(object) {
    return(inherits(object, c("TrialDesign", "TrialDesignCharacteristics", "TrialDesignPlan", "SimulationResults")))
}

#'
#' @title
#' Get Comparison Data
#'
#' @description
#' Combines existing designs, design characteristics, sample size or power plans,
#' and simulation results into a data frame without recalculating or modifying
#' the objects.
#'
#' @param objects A non-empty list of objects inheriting from `TrialDesign`,
#'   `TrialDesignCharacteristics`, `TrialDesignPlan`, or `SimulationResults`.
#'   Optional names are retained in the column `objectName`.
#' @param ... Further arguments. Currently unused.
#' @param niceColumnNamesEnabled If `TRUE`, use descriptive column names.
#' @param includeAllParameters If `TRUE`, include all parameters supported by
#'   the existing data-frame methods of the individual objects.
#'
#' @details
#' Each object receives an `objectNumber`, repeated for all its rows. Design
#' characteristics, plans, and simulation results are supplemented with their
#' associated design parameters, matched by analysis stage. For fixed designs
#' and results based on them, `kMax` and `stages` identify the single stage as 1, even when omitted by the
#' individual data-frame methods. The situation column numbers rows within each
#' stage. Columns are matched by technical names; missing
#' values are filled with `NA`. `sampleSizes` is harmonized to `numberOfSubjects`.
#' In comparisons containing interim analyses, fixed plans' `nFixed`, `nFixed1`,
#' and `nFixed2` use `numberOfSubjects`, `numberOfSubjects1`, and `numberOfSubjects2`.
#' Existing distinct target columns are preserved. Conflicting non-numeric column
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
#' head(.getComparisonData(objects))
#' }
#'
#' @noRd 
#'
.getComparisonData <- function(
        objects,
        ...,
        niceColumnNamesEnabled = FALSE,
        includeAllParameters = FALSE) {
    .warnInCaseOfUnknownArguments(functionName = ".getComparisonData", ...)
    .assertIsSingleLogical(niceColumnNamesEnabled, "niceColumnNamesEnabled")
    .assertIsSingleLogical(includeAllParameters, "includeAllParameters")
    if (!is.list(objects) || is.data.frame(objects) || length(objects) == 0) {
        stopIllegalArgument("'objects' must be a non-empty list of rpact designs, design characteristics, plans, or simulation results",
            functionName = ".getComparisonData", parameter = "objects", value = objects,
            constraint = "a non-empty list, excluding data.frames",
            diagnosticId = "comparison.objects_invalid"
        )
    }
    hasInterimAnalyses <- any(vapply(objects, function(object) {
        if (!.isComparisonObject(object)) {
            return(FALSE)
        }
        design <- .getTrialDesign(object)
        return(!is.null(design) && design$kMax > 1L)
    }, logical(1)))
    mappings <- lapply(objects, .getComparisonColumnMapping, hasInterimAnalyses = hasInterimAnalyses)
    fixedMappings <- lapply(mappings, function(mapping) {
        return(mapping[setdiff(names(mapping), "sampleSizes")])
    })
    requiredColumns <- unique(unlist(lapply(fixedMappings, unname), use.names = FALSE))
    frames <- lapply(seq_along(objects), function(i) {
        obj <- objects[[i]]
        if (!.isComparisonObject(obj)) {
            stopIllegalArgument("Unsupported object at position ", i, ": ", paste(class(obj), collapse = "/"),
                functionName = ".getComparisonData", parameter = "objects", value = obj,
                constraint = "each object must inherit from TrialDesign, TrialDesignCharacteristics, TrialDesignPlan, or SimulationResults",
                diagnosticId = "comparison.object_class_unsupported",
                context = list(objectPosition = i, objectClass = class(obj))
            )
        }
        df <- as.data.frame(obj,
            niceColumnNamesEnabled = FALSE,
            includeAllParameters = includeAllParameters
        )
        df <- .harmonizeComparisonColumns(df, mappings[[i]])
        missingColumns <- setdiff(intersect(requiredColumns, names(obj)), names(df))
        if (length(missingColumns)) {
            fullData <- as.data.frame(obj, niceColumnNamesEnabled = FALSE, includeAllParameters = TRUE)
            missingColumns <- intersect(missingColumns, names(fullData))
            df[missingColumns] <- fullData[missingColumns]
        }
        design <- if (inherits(obj, "TrialDesign")) {
            obj
        } else {
            obj$.design
        }
        if (inherits(design, "TrialDesignFixed")) {
            # Fixed-design data-frame methods omit these known design coordinates.
            df$kMax <- rep(design$kMax, nrow(df))
            df$stages <- rep(1L, nrow(df))
        }
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
                if (anyNA(indices)) {
                    stopIllegalDataInput("Cannot match analysis stages of object ", i,
                        functionName = ".getComparisonData", parameter = "stages", value = df$stages,
                        relatedParameter = "design$stages", relatedValue = designData$stages,
                        constraint = "each result stage must match an associated design stage",
                        diagnosticId = "comparison.stages_unmatched",
                        context = list(objectPosition = i, unmatchedStages = df$stages[is.na(indices)])
                    )
                }
            } else {
                stopIllegalDataInput("Cannot match the associated design to the rows of object ", i,
                    functionName = ".getComparisonData", parameter = "objects", value = obj,
                    constraint = "a multi-row associated design requires stages in both data frames",
                    diagnosticId = "comparison.design_rows_unmatched",
                    context = list(objectPosition = i, resultColumns = names(df),
                        designColumns = names(designData), designRows = nrow(designData))
                )
            }
            df[missing] <- designData[indices, missing, drop = FALSE]
        }
        className <- class(obj)[1L]
        isDesign <- inherits(obj, c("TrialDesign", "TrialDesignCharacteristics"))
        endpoint <- if (isDesign) {
            "Undefined"
        } else {
            if (grepl("Means", className)) {
                "Means"
            } else if (grepl("Rates", className)) {
                "Rates"
            } else if (grepl("Survival", className)) {
                "Survival"
            } else if (grepl("Count", className)) {
                "Counts"
            } else {
                "Undefined"
            }
        }
        calculationTarget <- if (isDesign) {
            "Design"
        } else {
            if (inherits(obj, "SimulationResults")) {
                if (grepl("MultiArm", className)) {
                    "Multi-arm power simulation"
                } else {
                    "Power simulation"
                }
            } else if (obj$.isSampleSizeObject()) {
                "Sample size"
            } else {
                "Power"
            }
        }
        info <- data.frame(
            objectNumber = rep(i, nrow(df)),
            objectType = rep(className, nrow(df)), endpoint = rep(endpoint, nrow(df)),
            calculationTarget = rep(calculationTarget, nrow(df)), stringsAsFactors = FALSE
        )
        if (!is.null(names(objects))) {
            label <- names(objects)[i]
            if (is.na(label) || !nzchar(label)) {
                label <- paste("Object", i)
            }
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
        for (name in setdiff(columnNames, names(df))) {
            df[[name]] <- rep(NA, nrow(df))
        }
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
            if (is.null(label)) {
                name
            } else {
                label
            }
        }, character(1))
        names(result) <- make.unique(names(result))
    }
    return(result)
}
