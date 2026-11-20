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

.comparisonConversionConflict <- function(parameter, value, strict, constraint, context = list(),
        diagnosticId = "comparison.conversion_conflict", functionName = "as.data.frame.TrialComparison",
        relatedParameter = NULL, relatedValue = NULL) {
    arguments <- list(
        paste0("Cannot align comparison parameter '", parameter, "': ", constraint,
            ". Use strict = FALSE to retain the complete values as character."),
        functionName = functionName, parameter = parameter, value = value,
        relatedParameter = relatedParameter, relatedValue = relatedValue,
        constraint = constraint, diagnosticId = diagnosticId, context = context
    )
    if (strict) {
        do.call(stopIllegalDataInput, arguments)
    } else {
        arguments[[1L]] <- paste0("Cannot align comparison parameter '", parameter, "': ", constraint,
            ". Retaining the complete values as character (use strict = TRUE to fail).")
        do.call(warnDataIssue, arguments)
    }
}

.comparisonCompleteText <- function(value) {
    return(paste(capture.output(dput(value, control = "all")), collapse = " "))
}

.comparisonRowCoordinates <- function(data) {
    stage <- intersect(c("stages", "stage"), names(data))
    stages <- if (length(stage)) {
        data[[stage[1L]]]
    } else {
        rep(1L, nrow(data))
    }
    situations <- if ("situation" %in% names(data)) {
        data$situation
    } else {
        as.integer(ave(seq_len(nrow(data)), stages, FUN = seq_along))
    }
    return(paste(stages, situations, sep = ":"))
}

.getComparisonSourceData <- function(object, includeAllParameters, strict) {
    arguments <- list(object, niceColumnNamesEnabled = FALSE, includeAllParameters = includeAllParameters)
    if (inherits(object, c("TrialDesign", "TrialDesignCharacteristics", "TrialDesignPlan",
            "SimulationResults", "PowerAndAverageSampleNumberResult"))) {
        arguments$strict <- strict
    }
    return(do.call(as.data.frame, arguments))
}

# Expand independent component domains separately, never as a Cartesian product.
# The original scenario coordinate is retained when a row has several components.
.comparisonComponentDomain <- function(parameter, columns) {
    if (parameter %in% c("accrualTime", "accrualIntensity", "accrualIntensityRelative") &&
            any(c("accrualTime", "accrual") %in% columns)) {
        return("accrual")
    }
    if (parameter %in% c("piecewiseSurvivalTime", "lambda1", "lambda2", "median1", "median2", "hazardRatio") &&
            any(c("piecewiseSurvivalTime", "survival") %in% columns)) {
        return("survival")
    }
    if (startsWith(parameter, "observedData_") || parameter == "observedData") {
        return("observedData")
    }
    return(parameter)
}

.expandDataFrameComponents <- function(data, strict = FALSE) {
    columns <- names(data)[vapply(data, is.list, logical(1))]
    if (!length(columns) || !nrow(data)) {
        return(data)
    }
    for (parameter in columns) {
        cells <- data[[parameter]]
        atomicCells <- vapply(cells, function(value) {
            return(is.atomic(value) && !is.object(value))
        }, logical(1))
        numericCells <- vapply(cells, function(value) {
            return(is.numeric(value) || is.logical(value) || !length(value))
        }, logical(1))
        definedCells <- cells[!vapply(cells, function(value) {
            return(!length(value) || (is.logical(value) && all(is.na(value))))
        }, logical(1))]
        types <- unique(vapply(definedCells, typeof, character(1)))
        if (!all(atomicCells) || (!all(numericCells) && length(types) > 1L)) {
            .comparisonConversionConflict(parameter, cells, strict,
                "component cells must have compatible atomic types", functionName = ".expandDataFrameComponents")
            data[[parameter]] <- vapply(cells, .comparisonCompleteText, character(1))
        }
    }
    columns <- names(data)[vapply(data, is.list, logical(1))]
    if (!length(columns)) {
        return(data)
    }
    reserved <- c("component", "componentIndex",
        grep("^componentDimension[0-9]+$", names(data), value = TRUE))
    if (any(reserved %in% names(data))) {
        for (parameter in columns) {
            .comparisonConversionConflict(parameter, data[[parameter]], strict,
                "component coordinates must not collide with existing columns", functionName = ".expandDataFrameComponents")
            data[[parameter]] <- vapply(data[[parameter]], .comparisonCompleteText, character(1))
        }
        return(data)
    }
    if (!"situation" %in% names(data)) {
        stage <- intersect(c("stages", "stage"), names(data))
        data$situation <- if (length(stage)) {
            as.integer(ave(seq_len(nrow(data)), data[[stage[1L]]], FUN = seq_along))
        } else {
            seq_len(nrow(data))
        }
    }
    dimensionCount <- max(0L, unlist(lapply(data[columns], function(cells) {
        return(vapply(cells, function(value) {
            return(length(dim(value)))
        }, integer(1)))
    })))
    dimensionNames <- if (dimensionCount > 0L) {
        paste0("componentDimension", seq_len(dimensionCount))
    } else {
        character()
    }
    domains <- vapply(columns, .comparisonComponentDomain, character(1), columns = columns)
    for (domain in unique(domains)) {
        members <- columns[domains == domain]
        sizes <- lapply(data[members], lengths)
        maximum <- Reduce(pmax, sizes)
        for (parameter in members) {
            allowed <- sizes[[parameter]] == maximum | sizes[[parameter]] == 0L
            if (domain %in% c("accrual", "survival")) {
                timeParameter <- if (domain == "accrual") {
                    "accrualTime"
                } else {
                    "piecewiseSurvivalTime"
                }
                if (timeParameter %in% members && parameter != timeParameter) {
                    allowed <- allowed | (sizes[[timeParameter]] == maximum & sizes[[parameter]] == maximum - 1L)
                }
            }
            if (!all(allowed)) {
                .comparisonConversionConflict(parameter, data[[parameter]], strict,
                    "values in one component domain must have matching indices",
                    functionName = ".expandDataFrameComponents",
                    context = list(domain = domain, componentLengths = sizes))
                data[[parameter]] <- vapply(data[[parameter]], .comparisonCompleteText, character(1))
            }
        }
    }
    columns <- names(data)[vapply(data, is.list, logical(1))]
    if (!length(columns)) {
        return(data)
    }
    domains <- vapply(columns, .comparisonComponentDomain, character(1), columns = columns)
    blocks <- list()
    for (domain in unique(domains)) {
        members <- columns[domains == domain]
        sizes <- pmax(1L, Reduce(pmax, lapply(data[members], lengths)))
        indices <- rep(seq_len(nrow(data)), sizes)
        block <- data[indices, setdiff(names(data), columns), drop = FALSE]
        block$component <- domain
        block$componentIndex <- sequence(sizes)
        for (name in columns) {
            prototype <- unlist(data[[name]], use.names = FALSE)
            if (is.null(prototype)) {
                prototype <- logical()
            }
            block[[name]] <- prototype[rep(NA_integer_, length(indices))]
        }
        for (name in dimensionNames) {
            block[[name]] <- NA_integer_
        }
        for (parameter in members) {
            cells <- data[[parameter]]
            block[[parameter]] <- unlist(lapply(seq_along(cells), function(i) {
                value <- as.vector(cells[[i]])
                if (!length(value)) {
                    return(rep(NA, sizes[i]))
                }
                return(value[seq_len(sizes[i])])
            }), use.names = FALSE)
            offset <- 0L
            for (i in seq_along(cells)) {
                dimensions <- dim(cells[[i]])
                if (length(dimensions) && length(cells[[i]])) {
                    coordinates <- arrayInd(seq_along(cells[[i]]), dimensions)
                    block[offset + seq_along(cells[[i]]), dimensionNames[seq_along(dimensions)]] <- coordinates
                }
                offset <- offset + sizes[i]
            }
        }
        blocks[[domain]] <- block
    }
    result <- do.call(rbind, blocks)
    rownames(result) <- NULL
    return(result)
}

.getComparisonColumnMapping <- function(object, hasInterimAnalyses = FALSE) {
    mapping <- c(sampleSizes = "numberOfSubjects")
    design <- if (.isComparisonObject(object)) {
        .getComparisonDesign(object)
    } else {
        NULL
    }
    if (hasInterimAnalyses && inherits(object, "TrialDesignPlan") &&
            !is.null(design) && design$kMax == 1L) {
        # Extend this named mapping for further equivalent fixed-plan columns.
        mapping <- c(mapping,
            nFixed = "numberOfSubjects",
            nFixed1 = "numberOfSubjects1",
            nFixed2 = "numberOfSubjects2"
        )
        if (inherits(object, "TrialDesignPlanSurvival")) {
            mapping <- c(mapping, eventsFixed = "cumulativeEventsPerStage")
        }
    }
    if (inherits(object, "TrialDesignPlan") && object$.isSampleSizeObject()) {
        # H1 is the alternative supplied for the current sample-size scenario.
        expectationMapping <- c(expectedNumberOfSubjectsH1 = "expectedNumberOfSubjects")
        if (inherits(object, "TrialDesignPlanSurvival")) {
            expectationMapping <- c(expectationMapping, expectedEventsH1 = "expectedNumberOfEvents")
        }
        for (source in intersect(names(expectationMapping), names(object))) {
            value <- object[[source]]
            if (length(value) && !all(is.na(value))) {
                mapping <- c(mapping, expectationMapping[source])
            }
        }
    }
    if (inherits(object, "PowerAndAverageSampleNumberResult")) {
        # Preserve stage-wise stopping before mapping the overall probability.
        mapping <- c(mapping,
            earlyStop = "earlyStopPerStage",
            overallEarlyStop = "earlyStop",
            overallFutility = "futilityStop",
            calculatedPower = "overallReject"
        )
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
    return(inherits(object, c("TrialDesign", "TrialDesignCharacteristics", "TrialDesignPlan",
        "SimulationResults", "PowerAndAverageSampleNumberResult")) || !is.null(.getComparisonAdapter(object)))
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
#'   `TrialDesignCharacteristics`, `TrialDesignPlan`, `SimulationResults`, or
#'   `PowerAndAverageSampleNumberResult`, `EventProbabilities`, `NumberOfSubjects`,
#'   `ConditionalPowerResults`, `PerformanceScore`, `FisherInformation`, or
#'   `TrialDesignOptimalConditionalErrorCharacteristics`.
#'   Optional names are retained in the column `objectName`.
#' @param ... Further arguments. Currently unused.
#' @param niceColumnNamesEnabled If `TRUE`, use descriptive column names.
#' @param includeAllParameters If `TRUE`, include all parameters supported by
#'   the existing data-frame methods of the individual objects.
#'
#' @param harmonizeColumnNames If `TRUE`, apply class-specific column mappings
#'   and include available equivalent fields. If `FALSE`, retain original names.
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
#' Fixed survival plans additionally use `cumulativeEventsPerStage` for `eventsFixed`.
#' Available H1 expectations in sample size plans use `expectedNumberOfSubjects`
#' and `expectedNumberOfEvents`; H0 and intermediate-hypothesis expectations remain separate.
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
        includeAllParameters = FALSE,
        harmonizeColumnNames = TRUE,
        strict = FALSE) {
    .warnInCaseOfUnknownArguments(functionName = ".getComparisonData", ...)
    .assertIsSingleLogical(niceColumnNamesEnabled, "niceColumnNamesEnabled")
    .assertIsSingleLogical(includeAllParameters, "includeAllParameters")
    .assertIsSingleLogical(harmonizeColumnNames, "harmonizeColumnNames")
    .assertIsSingleLogical(strict, "strict")
    if (!is.list(objects) || is.data.frame(objects) || length(objects) == 0) {
        stopIllegalArgument("'objects' must be a non-empty list of supported rpact comparison objects",
            functionName = ".getComparisonData", parameter = "objects", value = objects,
            constraint = "a non-empty list, excluding data.frames",
            diagnosticId = "comparison.objects_invalid"
        )
    }
    hasInterimAnalyses <- harmonizeColumnNames && any(vapply(objects, function(object) {
        if (!.isComparisonObject(object)) {
            return(FALSE)
        }
        design <- .getComparisonDesign(object)
        return(!is.null(design) && isTRUE(design$kMax > 1L))
    }, logical(1)))
    mappings <- lapply(objects, function(object) {
        if (harmonizeColumnNames) {
            return(.getComparisonColumnMapping(object, hasInterimAnalyses = hasInterimAnalyses))
        }
        return(character())
    })
    resultMappings <- lapply(mappings, function(mapping) {
        return(mapping[setdiff(names(mapping), "sampleSizes")])
    })
    requiredColumns <- unique(unlist(lapply(resultMappings, unname), use.names = FALSE))
    frames <- lapply(seq_along(objects), function(i) {
        obj <- objects[[i]]
        if (!.isComparisonObject(obj)) {
            stopIllegalArgument("Unsupported object at position ", i, ": ", paste(class(obj), collapse = "/"),
                functionName = ".getComparisonData", parameter = "objects", value = obj,
                constraint = "each object must be a supported comparison result or planning object",
                diagnosticId = "comparison.object_class_unsupported",
                context = list(objectPosition = i, objectClass = class(obj))
            )
        }
        df <- .getComparisonAdapterData(obj, includeAllParameters = includeAllParameters, strict = strict)
        if (is.null(df)) {
            df <- .getComparisonSourceData(obj, includeAllParameters, strict)
        }
        if (inherits(obj, "PowerAndAverageSampleNumberResult")) {
            # nMax is an essential input even when the compact source export omits it.
            df$nMax <- rep(obj$nMax, nrow(df))
            # The full source export can collapse theta into vector-valued cells.
            df$theta <- rep(obj$theta, times = obj$.design$kMax)
        }
        # Some equivalent fields are omitted by the object's compact export.
        mappedColumns <- unique(c(requiredColumns, names(mappings[[i]])))
        missingColumns <- setdiff(intersect(mappedColumns, names(obj)), names(df))
        if (length(missingColumns) && is.null(.getComparisonAdapter(obj))) {
            fullData <- .getComparisonSourceData(obj, TRUE, strict)
            missingColumns <- intersect(missingColumns, names(fullData))
            indices <- match(.comparisonRowCoordinates(df), .comparisonRowCoordinates(fullData))
            if (anyNA(indices)) {
                .comparisonConversionConflict("objects", obj, strict,
                    "compact and full exports must share stage and situation coordinates",
                    diagnosticId = "comparison.source_rows_unmatched",
                    context = list(objectPosition = i, columns = missingColumns,
                        unmatchedRows = which(is.na(indices))))
                for (parameter in missingColumns) {
                    df[[parameter]] <- rep(.comparisonCompleteText(fullData[[parameter]]), nrow(df))
                }
            } else {
                df[missingColumns] <- fullData[indices, missingColumns, drop = FALSE]
            }
        }
        df <- .harmonizeComparisonColumns(df, mappings[[i]])
        design <- .getComparisonDesign(obj)
        if (inherits(design, "TrialDesignFixed")) {
            # Fixed-design data-frame methods omit these known design coordinates.
            df$kMax <- rep(design$kMax, nrow(df))
            if ("stage" %in% names(df) && !"stages" %in% names(df)) {
                df$stage <- rep(1L, nrow(df))
            } else {
                df$stages <- rep(1L, nrow(df))
            }
        }
        if (!inherits(obj, c("TrialDesign", "PerformanceScore")) && inherits(design, "TrialDesign")) {
            designData <- .getComparisonSourceData(design, includeAllParameters, strict)
            missing <- setdiff(names(designData), names(df))
            stageColumn <- intersect(c("stages", "stage"), names(df))
            if ("stage" %in% stageColumn) {
                missing <- setdiff(missing, "stages")
            }
            if (nrow(designData) == 1L) {
                indices <- rep(1L, nrow(df))
            } else if (length(stageColumn) && "stages" %in% names(designData)) {
                indices <- match(df[[stageColumn[1L]]], designData$stages)
                if (anyNA(indices)) {
                    .comparisonConversionConflict(stageColumn[1L], df[[stageColumn[1L]]], strict,
                        "each result stage must match an associated design stage",
                        relatedParameter = "design$stages", relatedValue = designData$stages,
                        diagnosticId = "comparison.stages_unmatched",
                        context = list(objectPosition = i, designStages = designData$stages,
                            unmatchedStages = df[[stageColumn[1L]]][is.na(indices)]))
                    indices <- NULL
                }
            } else {
                .comparisonConversionConflict("objects", obj, strict,
                    "a multi-row associated design requires stages in both data frames",
                    diagnosticId = "comparison.design_rows_unmatched",
                    context = list(objectPosition = i, resultColumns = names(df),
                        designColumns = names(designData), designRows = nrow(designData)))
                indices <- NULL
            }
            if (is.null(indices)) {
                for (parameter in missing) {
                    df[[parameter]] <- rep(.comparisonCompleteText(designData[[parameter]]), nrow(df))
                }
            } else {
                df[missing] <- designData[indices, missing, drop = FALSE]
            }
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
        adapterContext <- .getComparisonAdapterContext(obj)
        calculationTarget <- if (!is.null(adapterContext)) {
            endpoint <- adapterContext$endpoint
            adapterContext$target
        } else if (isDesign) {
            "Design"
        } else {
            if (inherits(obj, "SimulationResults")) {
                if (grepl("MultiArm", className)) {
                    "Multi-arm power simulation"
                } else {
                    "Power simulation"
                }
            } else if (inherits(obj, "PowerAndAverageSampleNumberResult")) {
                "Power and ASN"
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
        info$situation <- if ("situation" %in% names(df)) {
            df$situation
        } else if (length(stageColumn)) {
            as.integer(ave(seq_len(nrow(df)), df[[stageColumn[1L]]], FUN = seq_along))
        } else {
            seq_len(nrow(df))
        }
        df$situation <- NULL
        return(.expandDataFrameComponents(
            .expandComparisonSurvivalCategories(obj, cbind(info, df, row.names = NULL),
                includeAllParameters, strict = strict),
            strict = strict))
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
        defined <- columns[!vapply(columns, function(column) {
            return(is.logical(column) && all(is.na(column)))
        }, logical(1))]
        numericCompatible <- all(vapply(columns, function(x) {
            return(is.numeric(x) || is.logical(x))
        }, logical(1)))
        sameType <- length(unique(vapply(defined, function(x) {
            return(paste(class(x), collapse = "/"))
        }, character(1)))) <= 1L
        if (!numericCompatible && !sameType) {
            .comparisonConversionConflict(name, columns, strict,
                "columns from different objects must have compatible types",
                context = list(objectPositions = seq_along(frames)))
            frames <- lapply(frames, function(df) {
                values <- df[[name]]
                df[[name]] <- vapply(seq_along(values), function(i) {
                    if (is.na(values[i]) && !is.nan(values[i])) {
                        return(NA_character_)
                    }
                    if (is.character(values)) {
                        return(values[i])
                    }
                    return(.comparisonCompleteText(values[i]))
                }, character(1))
                return(df)
            })
        }
    }
    result <- do.call(rbind, frames)
    rownames(result) <- NULL
    if (niceColumnNamesEnabled) {
        labels <- as.list(C_TABLE_COLUMN_NAMES)
        labels[c("objectNumber", "objectName", "objectType", "endpoint", "calculationTarget", "categoryType", "category")] <-
            c("Object number", "Object name", "Object type", "Endpoint", "Calculation target", "Category type", "Category")
        names(result) <- vapply(names(result), function(name) {
            if (grepl("^(observedDropoutProportion|dropoutObservationTime)[12](\\.|$)", name) ||
                    (name %in% c("singleEventsPerStage", "cumulativeEventsPerStage", "singleEventsPerArmAndStage") &&
                        any(vapply(objects, inherits, logical(1), "SimulationResultsMultiArmSurvival")))) {
                captions <- vapply(objects, function(object) {
                    return(.comparisonCaption(name, object, tableOutputEnabled = TRUE))
                }, character(1))
                return(.comparisonCaptionLabel(name, captions, tableOutputEnabled = TRUE))
            }
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
