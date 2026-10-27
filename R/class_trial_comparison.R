## |
## |  Trial comparisons
## |  This file is part of the R package rpact.
## |  Licensed under the GNU Lesser General Public License, version 3.
## |

#' @include class_core_parameter_set.R
#' @include class_summary.R
NULL

#' Compare Existing Trial Planning Objects
#'
#' @description Creates a comparison of existing designs, sample size or power
#' plans, and simulation results without recalculating them.
#' @param objects A non-empty, optionally named list accepted by
#'   [getComparisonData()].
#' @param ... Further arguments. Currently unused.
#' @return An R6 `TrialComparison` inheriting from `ParameterSet`.
#' @details
#' The comparison retains references to its objects. Changes to an original
#' object are reflected in subsequent comparisons; no comparison data are cached.
#' `$getObjects()` returns the named list of references. Replacing entries in
#' that returned list does not change the comparison. `length()` is the number
#' of objects and `names()` returns their supplied names (or `NULL`). Use
#' `$getObjects()[[i]]` to retrieve an object; the comparison itself is not a list.
#' Shallow cloning shares the contained objects. `clone(deep = TRUE)` is unsupported.
#'
#' `print()` describes the collection and selected observed ranges. `summary()`
#' groups differences by object scope, inputs/design settings and results, with
#' equal profiles grouped by object number. Constants are compared independently
#' of the number of rows. Nonconstant profiles retain stage/situation positions;
#' repeated values across the other dimension are collapsed. Missing values and
#' their positions participate in exact comparisons. No rounding or numerical
#' tolerance is used to decide whether profiles differ.
#'
#' Console and Markdown output are bounded. Limits and display rounding do not
#' change the underlying data. [as.data.frame.TrialComparison()] returns the full
#' comparison table. [plot.TrialComparison()] selects axes only when unambiguous.
#' [getObjectRCode()] reconstructs the comparison and its contained objects;
#' executing generated simulation code reruns the corresponding simulations.
#' @examples
#' comparison <- getComparison(list(
#'     fixed = getDesignFixed(), sequential = getDesignGroupSequential(kMax = 2)
#' ))
#' print(comparison)
#' summary(comparison)
#' head(as.data.frame(comparison))
#' \dontrun{
#' plot(comparison, xParamName = "objectNumber", yParamName = "criticalValues", plotLineEnabled = FALSE)
#' }
#' @export
getComparison <- function(objects, ...) {
    .warnInCaseOfUnknownArguments(functionName = "getComparison", ...)
    if (missing(objects)) {
        stopMissingArgument("'objects' must be supplied", functionName = "getComparison",
            parameter = "objects", constraint = "a non-empty list of comparison objects",
            diagnosticId = "comparison.objects_missing")
    }
    TrialComparison$new(objects)
}

#' Trial Comparison
#' @description R6 container for existing trial planning objects, inheriting from
#'   `ParameterSet`. Create instances using [getComparison()].
#' @name TrialComparison
#' @seealso [getComparison()]
NULL

TrialComparison <- R6::R6Class("TrialComparison",
    inherit = ParameterSet,
    public = list(
        initialize = function(objects) {
            # Reuse the comparison validation, without storing a table.
            getComparisonData(objects)
            super$initialize()
            private$.objects <- objects
        },
        getObjects = function() private$.objects,
        show = function(...) {
            self$.show(..., consoleOutputEnabled = TRUE)
            invisible(self)
        },
        .show = function(..., consoleOutputEnabled = FALSE) {
            .showTrialComparison(self, ..., consoleOutputEnabled = consoleOutputEnabled)
            invisible(self)
        }
    ),
    private = list(
        .objects = NULL,
        deep_clone = function(name, value) {
            stopIllegalArgument("Deep cloning of TrialComparison is not supported; use clone(deep = FALSE)",
                functionName = "TrialComparison$clone", parameter = "deep", value = TRUE,
                constraint = "deep must be FALSE", diagnosticId = "comparison.deep_clone_unsupported")
        }
    )
)

#' @rdname getComparison
#' @param x A `TrialComparison`.
#' @export
length.TrialComparison <- function(x) length(x$getObjects())

#' @rdname getComparison
#' @export
names.TrialComparison <- function(x) names(x$getObjects())

#' Trial Comparison Data Frame
#' @param x A `TrialComparison`.
#' @param row.names Optional row names for the result.
#' @param optional Unused; included for compatibility with `as.data.frame()`.
#' @param ... Further arguments. Currently unused.
#' @inheritParams getComparisonData
#' @param removeConstantParameters If TRUE, retain identifiers and only parameters
#'   with different object profiles, using the same rules as `summary()`.
#' @return An unrounded data.frame. All columns are returned by default.
#' @export
as.data.frame.TrialComparison <- function(x, row.names = NULL, optional = FALSE, ...,
        niceColumnNamesEnabled = FALSE, includeAllParameters = FALSE, removeConstantParameters = FALSE) {
    .warnInCaseOfUnknownArguments(functionName = "as.data.frame.TrialComparison", ...)
    .assertIsSingleLogical(removeConstantParameters, "removeConstantParameters")
    .assertIsSingleLogical(niceColumnNamesEnabled, "niceColumnNamesEnabled")
    data <- getComparisonData(x$getObjects(), includeAllParameters = includeAllParameters)
    allNames <- names(data)
    if (removeConstantParameters) {
        info <- .getTrialComparisonInfo(x, data)
        selected <- getComparisonParameterNames(data, varyingOnly = FALSE)
        selected <- selected[selected %in% c(.comparisonIdentifiers, info$paramName[info$nProfiles > 1L])]
        data <- data[selected]
    }
    if (niceColumnNamesEnabled) {
        labels <- names(getComparisonData(x$getObjects(), niceColumnNamesEnabled = TRUE,
            includeAllParameters = includeAllParameters))
        names(data) <- labels[match(names(data), allNames)]
    }
    if (!is.null(row.names)) rownames(data) <- row.names
    data
}

.comparisonIdentifiers <- c("objectNumber", "objectName", "stages", "stage", "situation")
.comparisonScope <- c("objectType", "endpoint", "calculationTarget")

.assertComparisonLimit <- function(value, parameter, functionName) {
    if (!is.numeric(value) || is.complex(value) || length(value) != 1L || is.na(value) ||
            !is.finite(value) || value < 1 || value != trunc(value)) {
        stopIllegalArgument("'", parameter, "' must be a positive finite integer",
            functionName = functionName, parameter = parameter, value = value,
            constraint = "one finite integer >= 1", diagnosticId = "comparison.display_limit_invalid")
    }
}

.comparisonText <- function(text, width = 100L) {
    text <- gsub("[[:cntrl:]]", " ", as.character(text))
    ifelse(nchar(text) > width, paste0(substr(text, 1L, width - 3L), "..."), text)
}

.comparisonPreview <- function(values, maxValues = 4L, digits = NA_integer_, reference = NULL) {
    shown <- head(values, maxValues)
    text <- .formatComparisonParameterValues(shown, digits)
    if (is.na(digits) && !is.null(reference) && is.numeric(values)) {
        labels <- .formatComparisonParameterValues(reference)
        text <- labels[match(shown, reference)]
    }
    text[is.na(text)] <- "NA"
    if (is.numeric(shown)) text[is.nan(shown)] <- "NaN"
    text[!nzchar(trimws(text))] <- '""'
    text <- paste(.comparisonText(text, if (length(shown) == 1L) 55L else 20L), collapse = ", ")
    if (length(values) > maxValues) text <- paste0(text, ", ... (", length(values), " values)")
    text
}

.comparisonRole <- function(objects, parameter) {
    if (parameter %in% .comparisonScope) return("Object types and scope")
    types <- unlist(lapply(objects, function(object) {
        c(object$.parameterTypes[[parameter]], if (!is.null(object[[".design"]])) {
            object$.design$.parameterTypes[[parameter]]
        })
    }), use.names = FALSE)
    if (any(types %in% c(C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE))) return("Inputs and design settings")
    "Results and derived parameters"
}

.comparisonProfile <- function(data, parameter) {
    values <- data[[parameter]]
    if (length(unique(values)) <= 1L) return(data.frame(value = values[1L]))
    keys <- intersect(c("stages", "stage", "situation"), names(data))
    profile <- data[c(keys, parameter)]
    names(profile)[ncol(profile)] <- "value"
    # Collapse a repeated dimension, retaining the positions of missing values.
    for (key in keys) {
        groups <- split(seq_len(nrow(profile)), profile[[key]])
        if (length(groups) && all(vapply(groups, function(i) length(unique(values[i])) == 1L, logical(1)))) {
            profile <- unique(profile[c(key, "value")])
            break
        }
    }
    keys <- setdiff(names(profile), "value")
    if (length(keys)) profile <- profile[do.call(order, profile[keys]), , drop = FALSE]
    rownames(profile) <- NULL
    profile
}

.getTrialComparisonInfo <- function(object, data = getComparisonData(object$getObjects())) {
    info <- getComparisonParameterInfo(data)
    # Use the common metadata API for the full ordered candidate set; row-wise
    # variation alone cannot identify equal stage profiles of different objects.
    candidates <- getComparisonParameterNames(data, varyingOnly = FALSE, exclude = .comparisonIdentifiers)
    info <- info[match(candidates, info$paramName), , drop = FALSE]
    objects <- object$getObjects()
    frames <- split(data, factor(data$objectNumber, levels = seq_along(objects)))
    profiles <- lapply(info$paramName, function(parameter) lapply(frames, .comparisonProfile, parameter = parameter))
    # Exact serialized keys avoid display rounding and quadratic pairwise comparisons.
    groups <- lapply(profiles, function(items) {
        keys <- vapply(items, function(item) paste(as.character(serialize(item, NULL, version = 2)), collapse = ""), "")
        match(keys, unique(keys))
    })
    info$nProfiles <- vapply(groups, function(group) length(unique(group)), integer(1))
    info$role <- vapply(info$paramName, function(parameter) .comparisonRole(objects, parameter), "")
    info$profiles <- profiles
    info$profileGroups <- groups
    rownames(info) <- NULL
    info
}

#' Print a Trial Comparison
#' @param x A `TrialComparison`.
#' @param ... Further arguments. Currently unused.
#' @param markdown Whether to return Markdown output. NA uses rpact settings.
#' @param digits Display rounding only, as in [getComparisonParameterDescriptions()].
#' @param maxTypes,maxMetrics Maximum numbers of object types and selected metrics
#'   to display. These limits do not affect [as.data.frame.TrialComparison()].
#' @return The object invisibly, or knitr Markdown output when requested.
#' @export
print.TrialComparison <- function(x, ..., markdown = NA, digits = NA_integer_, maxTypes = 5L, maxMetrics = 6L) {
    .warnInCaseOfUnknownArguments(functionName = "print.TrialComparison", ...)
    .assertIsSingleLogical(markdown, "markdown", naAllowed = TRUE)
    if (is.na(markdown)) markdown <- .isMarkdownEnabled("print")
    if (markdown && .comparisonHasConsumerCall(sys.calls())) {
        .addObjectToPipeOperatorQueue(x)
        return(invisible(x))
    }
    if (markdown) {
        return(knitr::asis_output(paste(utils::capture.output(x$.catMarkdownText(
            digits = digits, maxTypes = maxTypes, maxMetrics = maxMetrics)), collapse = "\n")))
    }
    x$.show(digits = digits, maxTypes = maxTypes, maxMetrics = maxMetrics, consoleOutputEnabled = TRUE)
    invisible(x)
}

.showTrialComparison <- function(object, ..., digits = NA_integer_, maxTypes = 5L, maxMetrics = 6L,
        consoleOutputEnabled = TRUE) {
    .warnInCaseOfUnknownArguments(functionName = "TrialComparison$show", ...)
    .assertComparisonLimit(maxTypes, "maxTypes", "print.TrialComparison")
    .assertComparisonLimit(maxMetrics, "maxMetrics", "print.TrialComparison")
    data <- getComparisonData(object$getObjects())
    info <- getComparisonParameterInfo(data)
    getComparisonParameterDescriptions(info[FALSE, ], digits = digits) # Validate display precision.
    object$.resetCat()
    emit <- function(...) object$.cat(..., consoleOutputEnabled = consoleOutputEnabled)
    object$.cat("Trial comparison\n", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
    emit(length(object), " objects; ", nrow(data), " comparison rows; ", ncol(data), " columns.\n")
    types <- table(vapply(object$getObjects(), function(item) class(item)[1L], ""))
    for (i in seq_len(min(length(types), maxTypes))) {
        emit("  ", .comparisonText(names(types)[i], 55L), ": ", types[i], "\n")
    }
    if (length(types) > maxTypes) emit("  ... ", length(types) - maxTypes, " more object types.\n")
    metrics <- intersect(c("kMax", "alpha", "beta", "maxNumberOfSubjects", "maxNumberOfEvents",
        "nFixed", "overallReject", "expectedNumberOfSubjects", "expectedNumberOfEvents", "studyDuration"), info$paramName[!info$isEmpty])
    if (length(metrics)) emit("Observed metrics (across objects, stages and situations):\n")
    for (parameter in head(metrics, maxMetrics)) {
        row <- info[info$paramName == parameter, , drop = FALSE]
        values <- row$values[[1L]]
        if (is.numeric(values) && length(values) > 1L) values <- range(values)
        emit("  ", parameter, ": ", .comparisonPreview(values, digits = digits),
            if (row$hasMissingValues) " (missing values present)" else "", "\n")
    }
    if (length(metrics) > maxMetrics) emit("  ... ", length(metrics) - maxMetrics, " more observed metrics.\n")
    emit("Use summary(x) for differences; as.data.frame(x) for all data;\n",
        "plot(x, xParamName = ..., yParamName = ...) for plots; x$getObjects() for objects.\n")
    invisible(object)
}

#' Summarize Differences Between Trial Planning Objects
#' @param object A `TrialComparison`.
#' @param ... Further arguments. Currently unused.
#' @param digits Display rounding only; exact values determine differences.
#' @param maxParameters Maximum number of differing parameters shown, across all
#'   sections. Increase this to inspect further differences.
#' @param maxProfiles Maximum number of distinct profiles shown per parameter.
#' @param maxObjects Maximum object numbers/names listed per profile.
#' @param maxValues Maximum profile values shown per preview.
#' @param output One of "all", "title", "overview", or "body".
#' @return A `TrialComparisonSummary` inheriting from `SummaryFactory`. Its
#'   `parameterInfo` contains metadata, full exact profiles and object group IDs.
#'   The displayed summary is a snapshot; call `summary()` again after changing
#'   an underlying object. Input roles take precedence when roles differ across
#'   the compared object types.
#' @export
summary.TrialComparison <- function(object, ..., digits = NA_integer_, maxParameters = 12L,
        maxProfiles = 3L, maxObjects = 6L, maxValues = 4L, output = c("all", "title", "overview", "body")) {
    .warnInCaseOfUnknownArguments(functionName = "summary.TrialComparison", ...)
    limits <- list(maxParameters = maxParameters, maxProfiles = maxProfiles, maxObjects = maxObjects, maxValues = maxValues)
    for (parameter in names(limits)) .assertComparisonLimit(limits[[parameter]], parameter, "summary.TrialComparison")
    info <- .getTrialComparisonInfo(object)
    getComparisonParameterDescriptions(info[FALSE, ], digits = digits)
    TrialComparisonSummary$new(object, info, limits, digits, match.arg(output))
}

TrialComparisonSummary <- R6::R6Class("TrialComparisonSummary",
    inherit = SummaryFactory,
    public = list(
        parameterInfo = NULL,
        .limits = NULL,
        .digits = NULL,
        .objectNames = NULL,
        initialize = function(object, info, limits, digits, output) {
            super$initialize(object = object, output = output)
            self$parameterInfo <- info
            self$.limits <- limits
            self$.digits <- digits
            self$.objectNames <- names(object)
        },
        .show = function(..., consoleOutputEnabled = TRUE) {
            .showTrialComparisonSummary(self, consoleOutputEnabled)
            invisible(self)
        }
    )
)

.showTrialComparisonSummary <- function(summary, consoleOutputEnabled) {
    summary$.resetCat()
    emit <- function(...) summary$.cat(..., consoleOutputEnabled = consoleOutputEnabled)
    info <- summary$parameterInfo
    limits <- summary$.limits
    different <- which(info$nProfiles > 1L)
    common <- which(info$nProfiles == 1L & !info$isEmpty)
    empty <- which(info$nProfiles == 1L & info$isEmpty)
    if (summary$output %in% c("all", "title")) {
        summary$.cat("Trial comparison summary\n", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
    }
    if (summary$output %in% c("all", "overview")) {
        emit(length(summary$object), " objects: ", length(different), " differing parameters; ",
            length(common), " shared parameters/profiles; ", length(empty), " empty parameters.\n")
        emit("Stage/situation profiles are compared exactly; NA means missing/unavailable.\n")
        if (length(common)) {
            emit("Shared (examples): ", paste(head(info$paramName[common], 6L), collapse = ", "),
                if (length(common) > 6L) ", ..." else ".", "\n")
        }
    }
    if (!summary$output %in% c("all", "body")) return(invisible(summary))
    if (!length(different)) emit("No parameter differences between the objects.\n")
    roles <- c("Object types and scope", "Inputs and design settings", "Results and derived parameters")
    ordered <- unlist(lapply(roles, function(role) different[info$role[different] == role]), use.names = FALSE)
    # Share the display budget across sections so a long input list cannot hide
    # every result difference. Within each section preserve source-column order.
    sections <- lapply(roles, function(role) different[info$role[different] == role])
    interleaved <- unlist(lapply(seq_len(max(c(0L, lengths(sections)))), function(position) {
        unlist(lapply(sections, function(indices) indices[position][!is.na(indices[position])]), use.names = FALSE)
    }), use.names = FALSE)
    shown <- head(interleaved, limits$maxParameters)
    for (role in roles) {
        indices <- shown[info$role[shown] == role]
        if (!length(indices)) next
        summary$.cat(paste0(role, "\n"), heading = 2, consoleOutputEnabled = consoleOutputEnabled)
        for (i in indices) {
            row <- info[i, , drop = FALSE]
            # The common descriptions supply type/role labels, without unbounded values.
            labelInfo <- row
            labelInfo$isBinary <- labelInfo$isSingleValue <- FALSE
            labelInfo$values <- list(character())
            description <- getComparisonParameterDescriptions(labelInfo, digits = summary$.digits)
            emit(.comparisonText(description, 75L), " [", row$nProfiles, " profiles]\n")
            groups <- row$profileGroups[[1L]]
            for (group in seq_len(min(row$nProfiles, limits$maxProfiles))) {
                members <- which(groups == group)
                ids <- head(members, limits$maxObjects)
                labels <- as.character(ids)
                if (!is.null(summary$.objectNames)) {
                    names <- summary$.objectNames[ids]
                    named <- !is.na(names) & nzchar(names)
                    labels[named] <- paste0(ids[named], " (", .comparisonText(names[named], 18L), ")")
                }
                membersText <- paste(labels, collapse = ", ")
                if (length(members) > limits$maxObjects) membersText <- paste0(membersText,
                    ", ... (+", length(members) - limits$maxObjects, " objects)")
                profile <- row$profiles[[1L]][[members[1L]]]
                keys <- setdiff(names(profile), "value")
                values <- .comparisonPreview(profile$value, limits$maxValues, summary$.digits, row$values[[1L]])
                if (length(keys)) {
                    positions <- vapply(keys, function(key) paste0(key, "=",
                        .comparisonPreview(profile[[key]], limits$maxValues)), "")
                    values <- paste0("[", paste(positions, collapse = "; "), "] ", values)
                }
                emit("  ", .comparisonText(values, 85L), " -> objects ", membersText, "\n")
            }
            if (row$nProfiles > limits$maxProfiles) emit("  ... ", row$nProfiles - limits$maxProfiles, " more profiles.\n")
        }
    }
    if (length(ordered) > length(shown)) emit("... ", length(ordered) - length(shown),
        " more differing parameters; increase maxParameters.\n")
    if (!is.na(summary$.digits)) emit("Display values are rounded; profile comparisons use exact values.\n")
    emit("Full data: as.data.frame(x). Exact profiles: summary(x)$parameterInfo.\n")
    invisible(summary)
}

#' Plot a Trial Comparison
#' @param x A `TrialComparison` or its summary.
#' @param xParamName,yParamName Optional axis column names. Omitted axes are
#'   selected only when there is exactly one candidate: inputs/design settings
#'   are preferred for x, results for y, then other varying numeric columns.
#'   Identifier columns are excluded; stages remain eligible axes.
#' @param ... Arguments passed to [plotComparison()], including categories,
#'   grouping, titles and line/point settings. Explicit axis selections prevail.
#' @return A ggplot object. Ambiguous selections raise an error listing candidate
#'   descriptions and a concrete example.
#' @export
plot.TrialComparison <- function(x, xParamName = NULL, yParamName = NULL, ...) {
    data <- as.data.frame(x)
    available <- getComparisonParameterNames(data, type = "continuous",
        exclude = c("objectNumber", "situation"))
    available <- available[vapply(data[available], is.numeric, logical(1))]
    roles <- vapply(available, function(parameter) .comparisonRole(x$getObjects(), parameter), "")
    select <- function(selection, parameter, other, preferredRole) {
        if (!is.null(selection)) return(selection)
        candidates <- setdiff(available, other)
        preferred <- candidates[roles[candidates] == preferredRole]
        if (length(preferred)) candidates <- preferred
        if (length(candidates) == 1L) return(candidates)
        info <- getComparisonParameterInfo(data[candidates])
        descriptions <- getComparisonParameterDescriptions(info)
        exampleX <- if (!is.null(xParamName)) xParamName else head(available, 1L)
        exampleY <- if (!is.null(yParamName)) yParamName else head(setdiff(available, exampleX), 1L)
        example <- if (length(exampleX) == 1L && length(exampleY) == 1L) {
            paste0("plot(x, xParamName = ", encodeString(exampleX, quote = '"'),
                ", yParamName = ", encodeString(exampleY, quote = '"'), ")")
        } else "Choose numeric axis columns from as.data.frame(x)."
        stopIllegalArgument("Cannot select '", parameter, "' unambiguously. Candidates: ",
            paste(head(.comparisonText(descriptions, 85L), 8L), collapse = "; "),
            if (length(candidates) > 8L) "; ..." else "", ". ", example,
            functionName = "plot.TrialComparison", parameter = parameter, value = selection,
            constraint = "supply an axis name when there is not exactly one candidate",
            diagnosticId = "comparison.plot_axis_ambiguous",
            context = list(candidates = candidates, descriptions = descriptions, example = example))
    }
    xParamName <- select(xParamName, "xParamName", yParamName, "Inputs and design settings")
    yParamName <- select(yParamName, "yParamName", xParamName, "Results and derived parameters")
    plotComparison(data, xParamName, yParamName, ...)
}

#' @rdname plot.TrialComparison
#' @param showSummary If TRUE, also print the summary before creating its plot.
#' @export
plot.TrialComparisonSummary <- function(x, xParamName = NULL, yParamName = NULL, ..., showSummary = FALSE) {
    .assertIsSingleLogical(showSummary, "showSummary")
    if (showSummary) print(x)
    plot(x$object, xParamName = xParamName, yParamName = yParamName, ...)
}

.comparisonHasConsumerCall <- function(calls) {
    any(vapply(calls, function(call) {
        if (!is.call(call)) return(FALSE)
        name <- call[[1L]]
        if (is.call(name) && as.character(name[[1L]]) %in% c("::", ":::")) name <- name[[3L]]
        is.symbol(name) && as.character(name) %in%
            c("summary", "summary.TrialComparison", "plot", "plot.TrialComparison")
    }, logical(1)))
}
