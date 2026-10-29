#' @include f_core_assertions.R
#' @include f_error_handling.R
NULL

.assertPiecewiseNumeric <- function(value, parameter, functionName, dimension = NULL) {
    if (is.null(value) || length(value) == 0) {
        stopMissingArgument("'", parameter, "' must contain numeric values",
            functionName = functionName, parameter = parameter, value = value,
            constraint = "non-empty numeric input",
            diagnosticId = "survival.piecewise_input_required")
    }
    validDimension <- if (is.null(dimension)) {
        is.null(dim(value))
    } else {
        length(dim(value)) == dimension && all(dim(value) > 0)
    }
    if (!is.numeric(value) || is.complex(value) || !validDimension) {
        stopIllegalArgument("'", parameter, "' must be numeric with the required dimensions",
            functionName = functionName, parameter = parameter, value = value,
            constraint = "real numeric input with non-zero dimensions and no type coercion",
            diagnosticId = "survival.piecewise_numeric_dimensions",
            context = list(expectedDimensions = dimension, actualDimensions = dim(value)))
    }
    if (any(!is.finite(value)) || any(value <= 0)) {
        stopArgumentOutOfRange("all ", parameter, " must be finite and greater than 0",
            functionName = functionName, parameter = parameter, value = value,
            constraint = "finite values in (0, Inf)", lowerBound = 0,
            diagnosticId = if (startsWith(parameter, "median")) {
                "survival.piecewise_positive_medians"
            } else {
                "survival.piecewise_positive_hazards"
            })
    }
}

.assertPiecewiseNames <- function(value, expectedLength, parameter, functionName) {
    if (is.null(value)) {
        return(invisible(NULL))
    }
    if (length(value) != expectedLength) {
        stopArgumentLengthOutOfBounds("the numbers of group and interval names must match the dimensions",
            functionName = functionName, parameter = parameter, value = value,
            constraint = "one name per dimension entry",
            expectedLength = expectedLength, actualLength = length(value),
            diagnosticId = "survival.piecewise_names_length")
    }
    if (!is.character(value) || anyNA(value) || any(!nzchar(trimws(value))) || anyDuplicated(value) > 0) {
        stopIllegalArgument("'", parameter, "' must contain unique, non-empty names without missing values",
            functionName = functionName, parameter = parameter, value = value,
            constraint = "unique non-empty character names without NA",
            diagnosticId = "survival.piecewise_unique_names")
    }
}

.assertPiecewiseGroupNames <- function(actual, expected, functionName) {
    if (!is.null(actual) && !is.null(expected) && !identical(actual, expected)) {
        stopConflictingArguments(
            "group names and their order must agree across all piecewise hazard matrices",
            functionName = functionName, parameter = "groupNames", value = actual,
            constraint = "identical group names in identical order",
            relatedParameter = "expectedGroupNames", relatedValue = expected,
            diagnosticId = "survival.piecewise_group_identity")
    }
}

.validatePiecewiseStartTimes <- function(piecewiseSurvivalTime, functionName) {
    if (is.null(piecewiseSurvivalTime) || length(piecewiseSurvivalTime) == 0) {
        stopMissingArgument("'piecewiseSurvivalTime' must be specified",
            functionName = functionName, parameter = "piecewiseSurvivalTime", value = piecewiseSurvivalTime,
            constraint = "at least two finite interval start times beginning with 0",
            diagnosticId = "survival.piecewise_times_required")
    }
    if (!is.numeric(piecewiseSurvivalTime) || !is.null(dim(piecewiseSurvivalTime))) {
        stopIllegalArgument("'piecewiseSurvivalTime' must be a numeric vector",
            functionName = functionName, parameter = "piecewiseSurvivalTime", value = piecewiseSurvivalTime,
            constraint = "numeric vector", diagnosticId = "survival.piecewise_times_type")
    }
    if (length(piecewiseSurvivalTime) < 2) {
        stopArgumentLengthOutOfBounds("'piecewiseSurvivalTime' must contain at least two interval start times",
            functionName = functionName, parameter = "piecewiseSurvivalTime", value = piecewiseSurvivalTime,
            constraint = "at least two interval start times", actualLength = length(piecewiseSurvivalTime),
            diagnosticId = "survival.piecewise_times_length")
    }
    if (any(!is.finite(piecewiseSurvivalTime)) || piecewiseSurvivalTime[1] != 0 ||
            any(diff(piecewiseSurvivalTime) <= 0)) {
        stopIllegalArgument("'piecewiseSurvivalTime' must be finite, strictly increasing, and begin with 0",
            functionName = functionName, parameter = "piecewiseSurvivalTime", value = piecewiseSurvivalTime,
            constraint = "finite start times with 0 = t1 < t2 < ...; last interval open-ended",
            diagnosticId = "survival.piecewise_times_order")
    }
    return(as.numeric(piecewiseSurvivalTime))
}

.getPiecewiseIntervalNames <- function(piecewiseSurvivalTime) {
    n <- length(piecewiseSurvivalTime)
    return(vapply(seq_len(n), function(i) {
        if (i < n) {
            return(paste0(piecewiseSurvivalTime[i], " - <", piecewiseSurvivalTime[i + 1]))
        } else {
            return(paste0(">=", piecewiseSurvivalTime[i]))
        }
    }, character(1)))
}

.assertPiecewiseAlternativeInputs <- function(value, median, parameter, medianParameter, functionName) {
    if (!is.null(value) && !is.null(median)) {
        stopConflictingArguments("'", parameter, "' and '", medianParameter, "' cannot both be specified",
            functionName = functionName, parameter = medianParameter, value = median,
            constraint = "specify exactly one hazard or median parameterization",
            relatedParameter = parameter, relatedValue = value,
            diagnosticId = "survival.piecewise_conflicting_parameterizations")
    }
}

.getPiecewiseMedianHazards <- function(medians, parameter, functionName, dimension = NULL) {
    .assertPiecewiseNumeric(medians, parameter, functionName, dimension = dimension)
    hazards <- getLambdaByMedian(as.numeric(medians), kappa = 1)
    if (!is.null(dim(medians))) {
        dim(hazards) <- dim(medians)
        dimnames(hazards) <- dimnames(medians)
    } else {
        names(hazards) <- names(medians)
    }
    return(hazards)
}

.getPiecewiseControlInput <- function(times, hazards, medians, parameter, medianParameter,
        functionName, dimension = NULL) {
    .assertPiecewiseAlternativeInputs(hazards, medians, parameter, medianParameter, functionName)
    if (is.null(medians)) {
        return(hazards)
    }
    converted <- .getPiecewiseMedianHazards(medians, medianParameter, functionName, dimension)
    actualLength <- if (is.null(dimension)) {
        length(medians)
    } else {
        ncol(medians)
    }
    if (actualLength != length(times)) {
        stopArgumentLengthOutOfBounds("'", medianParameter, "' must contain one median per interval",
            functionName = functionName, parameter = medianParameter, value = medians,
            constraint = "one median per interval; no recycling", relatedParameter = "piecewiseSurvivalTime",
            relatedValue = times, expectedLength = length(times), actualLength = actualLength,
            diagnosticId = "survival.piecewise_median_interval_count")
    }
    return(converted)
}

.getPiecewiseEffectInput <- function(hazardRatios, medianTreatments, functionName, groupNames = NULL) {
    .assertPiecewiseAlternativeInputs(hazardRatios, medianTreatments,
        "hazardRatios", "medianTreatments", functionName)
    if (is.null(medianTreatments)) {
        return(.normalizePiecewiseHazardRatios(hazardRatios,
            groupNames = groupNames, functionName = functionName))
    }
    return(.normalizePiecewiseHazardRatios(medianTreatments,
        groupNames = groupNames, functionName = functionName, parameter = "medianTreatments"))
}

.getPiecewiseMedianRatios <- function(medians, controls, functionName) {
    if (!identical(dim(medians)[1:2], dim(controls))) {
        stopIllegalArgument("'medianTreatments' must match the control groups and intervals",
            functionName = functionName, parameter = "medianTreatments", value = medians,
            constraint = "one treatment median per control group and interval in every situation",
            relatedParameter = "lambdaControls", relatedValue = controls,
            diagnosticId = "survival.piecewise_median_dimensions",
            context = list(expectedDimensions = dim(controls), actualDimensions = dim(medians)))
    }
    return(sweep(.getPiecewiseMedianHazards(medians, "medianTreatments", functionName, dimension = 3),
        c(1, 2), controls, `/`))
}

.normalizePiecewiseHazardRatios <- function(hazardRatios, groupNames = NULL,
        intervalNames = NULL, functionName, parameter = "hazardRatios") {
    matrixType <- if (parameter == "medianTreatments") {
        "treatment-median"
    } else {
        "hazard-ratio"
    }
    if (is.array(hazardRatios) && length(dim(hazardRatios)) == 3) {
        .assertPiecewiseNumeric(hazardRatios, parameter, functionName, dimension = 3)
        result <- hazardRatios
    } else {
        situations <- if (is.list(hazardRatios) && !is.data.frame(hazardRatios)) {
            hazardRatios
        } else {
            list(hazardRatios)
        }
        if (length(situations) == 0) {
            stopMissingArgument("at least one ", matrixType, " matrix must be specified",
                functionName = functionName, parameter = parameter, value = hazardRatios,
                constraint = "at least one group-by-interval matrix",
                diagnosticId = "survival.piecewise_input_required")
        }
        for (situation in situations) {
            .assertPiecewiseNumeric(situation, parameter, functionName, dimension = 2)
        }
        referenceDim <- dim(situations[[1]])
        if (any(vapply(situations, function(situation) {
            return(!identical(dim(situation), referenceDim))
        }, logical(1)))) {
            stopIllegalArgument("all ", matrixType, " matrices must have identical non-zero dimensions",
                functionName = functionName, parameter = parameter, value = hazardRatios,
                constraint = "identical non-zero group-by-interval dimensions",
                diagnosticId = "survival.piecewise_situation_dimensions",
                context = list(dimensions = lapply(situations, dim)))
        }
        for (situation in situations) {
            .assertPiecewiseNames(rownames(situation), referenceDim[1], "groupNames", functionName)
            .assertPiecewiseNames(colnames(situation), referenceDim[2], "intervalNames", functionName)
            if (is.null(groupNames)) {
                groupNames <- rownames(situation)
            }
            .assertPiecewiseGroupNames(rownames(situation), groupNames, functionName)
        }
        if (is.null(intervalNames)) {
            intervalNames <- colnames(situations[[1]])
        }
        result <- array(unlist(situations, use.names = FALSE),
            dim = c(referenceDim, length(situations)),
            dimnames = list(NULL, NULL, names(situations)))
    }
    dimensions <- dim(result)
    dimensionNames <- dimnames(result)
    if (is.null(dimensionNames)) {
        dimensionNames <- vector("list", 3)
    }
    for (i in seq_len(3)) {
        .assertPiecewiseNames(dimensionNames[[i]], dimensions[i],
            c("groupNames", "intervalNames", "situationNames")[i], functionName)
    }
    .assertPiecewiseNames(groupNames, dimensions[1], "groupNames", functionName)
    .assertPiecewiseNames(intervalNames, dimensions[2], "intervalNames", functionName)
    .assertPiecewiseGroupNames(dimensionNames[[1]], groupNames, functionName)
    if (!is.null(groupNames)) {
        dimensionNames[[1]] <- groupNames
    }
    if (!is.null(intervalNames)) {
        dimensionNames[[2]] <- intervalNames
    }
    for (i in seq_len(3)) {
        if (is.null(dimensionNames[[i]])) {
            dimensionNames[[i]] <- paste(c("Group", "Interval", "Situation")[i], seq_len(dimensions[i]))
        }
    }
    names(dimensionNames) <- c("group", "interval", "situation")
    dimnames(result) <- dimensionNames
    storage.mode(result) <- "double"
    return(result)
}

.validatePiecewiseSurvivalInputs <- function(piecewiseSurvivalTime, lambdaControls,
        hazardRatios, groupType, functionName) {
    times <- .validatePiecewiseStartTimes(piecewiseSurvivalTime, functionName)
    if (!is.character(groupType) || length(groupType) != 1 || is.na(groupType) ||
            !groupType %in% c("multi-arm", "enrichment")) {
        stopIllegalArgument("'groupType' must be 'multi-arm' or 'enrichment'",
            functionName = functionName, parameter = "groupType", value = groupType,
            constraint = "one supported group type", diagnosticId = "survival.piecewise_group_type",
            context = list(allowedValues = c("multi-arm", "enrichment")))
    }
    .assertPiecewiseNumeric(lambdaControls, "lambdaControls", functionName, dimension = 2)
    .assertPiecewiseNames(rownames(lambdaControls), nrow(lambdaControls), "groupNames", functionName)
    .assertPiecewiseNames(colnames(lambdaControls), ncol(lambdaControls), "intervalNames", functionName)
    ratios <- .normalizePiecewiseHazardRatios(hazardRatios,
        groupNames = rownames(lambdaControls), functionName = functionName)
    if (ncol(lambdaControls) != length(times) || dim(ratios)[2] != length(times) ||
            nrow(lambdaControls) != dim(ratios)[1]) {
        stopIllegalArgument("control hazards and hazard ratios must have one column per interval and matching groups",
            functionName = functionName, parameter = "lambdaControls", value = lambdaControls,
            constraint = "matching groups and one column per interval",
            relatedParameter = "hazardRatios", relatedValue = hazardRatios,
            diagnosticId = "survival.piecewise_hazard_dimensions",
            context = list(intervalStarts = times, controlDimensions = dim(lambdaControls),
                hazardRatioDimensions = dim(ratios)))
    }
    if (groupType == "multi-arm" && any(lambdaControls !=
            matrix(rep(lambdaControls[1, ], each = nrow(lambdaControls)), nrow = nrow(lambdaControls)))) {
        stopConflictingArguments("multi-arm survival requires common control hazards for all treatment arms",
            functionName = functionName, parameter = "lambdaControls", value = lambdaControls,
            constraint = "identical control hazards across arms", relatedParameter = "groupType",
            relatedValue = groupType, diagnosticId = "survival.piecewise_common_control")
    }
    intervalNames <- .getPiecewiseIntervalNames(times)
    colnames(lambdaControls) <- intervalNames
    rownames(lambdaControls) <- dimnames(ratios)[[1]]
    dimnames(ratios)[[2]] <- intervalNames
    storage.mode(lambdaControls) <- "double"
    activeHazards <- sweep(ratios, c(1, 2), lambdaControls, `*`)
    .assertPiecewiseNumeric(activeHazards, "activeHazards", functionName, dimension = 3)
    return(list(piecewiseSurvivalTime = times, lambdaControls = lambdaControls,
        hazardRatios = ratios, groupType = groupType))
}

#'
#' @title
#' Create an Array of Piecewise Hazard Ratios
#'
#' @description
#' Combines one or more group-by-interval hazard-ratio matrices into an array
#' with dimensions group by interval by situation.
#'
#' @param ... Matrices, or one named list of matrices. Each matrix contains groups
#'   in rows and piecewise survival intervals in columns.
#' @param groupNames Optional group names. Existing row names must agree in all
#'   matrices and with these names, including their order. Unnamed matrices are
#'   interpreted positionally.
#' @param intervalNames Optional interval names.
#'
#' @return A numeric array with dimensions group by interval by situation.
#'
#' @export
#'
getPiecewiseHazardRatioArray <- function(..., groupNames = NULL, intervalNames = NULL) {
    situations <- list(...)
    if (length(situations) == 1) {
        situations <- situations[[1]]
    }
    return(.normalizePiecewiseHazardRatios(situations, groupNames, intervalNames,
        functionName = "getPiecewiseHazardRatioArray"))
}

.getPiecewiseSurvivalRCode <- function(object, leadingArguments, newArgumentValues,
        prefix, postfix, stringWrapPrefix, pipeOperator, pipeOperatorPostfix, output, explicitPrint) {
    object$.validate(functionName = "getObjectRCode")
    arguments <- list(piecewiseSurvivalTime = object$piecewiseSurvivalTime)
    functionName <- if (object$groupType == "multi-arm") {
        if (!is.null(object[["medianControl"]])) {
            arguments$medianControl <- object$medianControl
        } else {
            arguments$lambdaControl <- as.numeric(object$lambdaControls[1, ])
        }
        "getPiecewiseSurvivalTimeMultiArm"
    } else {
        if (!is.null(object[["medianControls"]])) {
            arguments$medianControls <- object$medianControls
        } else {
            arguments$lambdaControls <- object$lambdaControls
        }
        "getPiecewiseSurvivalTimeEnrichment"
    }
    if (!is.null(object$medianTreatments)) {
        arguments$medianTreatments <- object$medianTreatments
    } else {
        arguments$hazardRatios <- object$hazardRatios
    }
    controlArguments <- if (object$groupType == "multi-arm") {
        c("lambdaControl", "medianControl")
    } else {
        c("lambdaControls", "medianControls")
    }
    allowedArguments <- c("piecewiseSurvivalTime", controlArguments, "hazardRatios", "medianTreatments")
    if (!is.list(newArgumentValues) || (length(newArgumentValues) > 0 &&
            (is.null(names(newArgumentValues)) || any(!names(newArgumentValues) %in% allowedArguments)))) {
        stopIllegalArgument("piecewise survival argument overrides must be a named list of constructor arguments",
            functionName = "getObjectRCode", parameter = "newArgumentValues", value = newArgumentValues,
            constraint = "named constructor argument overrides",
            diagnosticId = "survival.piecewise_r_code_arguments",
            context = list(allowedArguments = allowedArguments))
    }
    for (alternatives in list(controlArguments, c("hazardRatios", "medianTreatments"))) {
        overridden <- intersect(alternatives, names(newArgumentValues))
        if (length(overridden) == 1) {
            arguments[setdiff(alternatives, overridden)] <- NULL
        }
    }
    for (argumentName in names(newArgumentValues)) {
        arguments[argumentName] <- newArgumentValues[argumentName]
    }
    expressions <- vapply(names(arguments), function(argumentName) {
        return(paste0(argumentName, " = ",
            paste(deparse(arguments[[argumentName]], width.cutoff = 500L,
                control = c("keepNA", "keepInteger", "niceNames", "showAttributes", "digits17")),
                collapse = "\n")))
    }, character(1))
    code <- paste0(prefix, functionName, "(", paste(c(leadingArguments, expressions), collapse = ", "), ")")
    # Preserve quoted names and full numeric precision; a word wrapper is not syntax-aware.
    return(.formatRCode(code, precondition = character(), stringWrapParagraphWidth = NULL,
        postfix = postfix, stringWrapPrefix = stringWrapPrefix, pipeOperator = pipeOperator,
        pipeOperatorPostfix = pipeOperatorPostfix, output = output, explicitPrint = explicitPrint))
}
