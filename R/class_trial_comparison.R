## |
## |  Trial comparisons
## |  This file is part of the R package rpact.
## |  Licensed under the GNU Lesser General Public License, version 3.
## |

#'
#' @include class_core_parameter_set.R
#' @include class_summary.R
#'
NULL

#'
#' @title
#' Compare Existing Trial Planning Objects
#'
#' @description
#' Creates a comparison of existing designs, design characteristics, sample size
#' or power plans, and simulation results without recalculating them.
#'
#' @param objects Optional objects, lists of objects or `TrialComparison` objects.
#'   NULL is treated as an empty list.
#' @param ... Designs, design characteristics, plans, simulations, lists thereof, or `TrialComparison`
#'   objects. Entries from `...` precede entries from `objects`.
#'
#' @return An R6 `TrialComparison` inheriting from `ParameterSet`.
#'
#' @details
#' The comparison retains references to its objects. Changes to an original
#' object are reflected in subsequent comparisons; no comparison data are cached.
#' `$getObjects()` and [getObjects()] return the named list of references. Replacing entries in
#' that returned list does not change the comparison. `length()` is the number
#' of objects and `names()` returns their supplied names (or `NULL`). Retrieve
#' contained objects using `comparison[[i]]`, `comparison[["name"]]` or
#' `comparison$name`. R6 members take precedence for `$`; use `[[` to retrieve
#' an object whose name coincides with a member such as `add`. Unknown names
#' return NULL. The [fetch.TrialComparison()]
#' method provides the same object access through `fetch(comparison, name)` or
#' `fetch(comparison, position)`.
#' Lists and comparisons are recursively expanded in their original order.
#' Names on individual objects are retained; container names are discarded.
#' Supplied non-empty names must be unique across the expanded collection;
#' duplicate names raise an error. Unnamed entries and repeated references to
#' the same object are allowed. getComparison() creates an empty comparison.
#' Its data frame has zero rows and columns, and plotting requires
#' numeric data to be added first.
#'
#' `comparison$add(..., objects = NULL)` appends objects with the same input
#' rules and returns the comparison invisibly, supporting chaining. All new
#' entries are validated before the stored list is changed, including name
#' collisions with existing objects. Adding another
#' comparison copies its current list of references; subsequent additions to that
#' source comparison do not affect the target. Existing summary objects remain
#' snapshots; call `summary()` again after changing the collection.
#'
#' `comparison$delete(index)` removes one object by its positive integer position
#' or exact name and returns the comparison invisibly. Invalid
#' positions or unknown names raise an error without changing the collection.
#' Removing the last object leaves an empty comparison, which can be filled again.
#' Names and order of remaining objects are retained; positions are renumbered.
#' The S3 generics [add()] and [delete()] delegate to these methods and support
#' pipelines such as `comparison |> add(other) |> delete(1)`.
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
#'
#' @examples
#' comparison <- getComparison(list(
#'     fixed = getDesignFixed(), sequential = getDesignGroupSequential(kMax = 2)
#' ))
#' print(comparison)
#' summary(comparison)
#' head(as.data.frame(comparison))
#' withCharacteristics <- getComparison(
#'     getDesignGroupSequential(kMax = 2),
#'     getDesignCharacteristics(getDesignGroupSequential(kMax = 3)))
#' combined <- getComparison(comparison, other = getDesignFixed(alpha = 0.05))
#' combined$add(list(getDesignFixed(alpha = 0.01)))
#' empty <- getComparison()
#' empty$add(getDesignFixed())
#' combined$delete(1)
#' combined$delete("sequential")
#' \dontrun{
#' plot(comparison, xParamName = "objectNumber", yParamName = "criticalValues", plotLineEnabled = FALSE)
#' }
#'
#' @export
#'
getComparison <- function(..., objects = NULL) {
    return(TrialComparison$new(.collectComparisonObjects(list(...), objects)))
}

.collectComparisonObjects <- function(arguments, objects = NULL) {
    if (!is.null(objects)) {
        arguments <- c(arguments, list(objects))
    }
    result <- list()
    for (i in seq_along(arguments)) {
        object <- arguments[[i]]
        if (inherits(object, "TrialComparison")) {
            entries <- object$getObjects()
        } else if (is.list(object) && !is.data.frame(object)) {
            entries <- .collectComparisonObjects(object)
        } else {
            # Subsetting keeps an individual object's supplied name, including
            # empty names. Container names do not rename its leaves.
            entries <- arguments[i]
        }
        result <- c(result, entries)
    }
    result
}

.assertComparisonObjectNames <- function(objects, functionName) {
    objectNames <- names(objects)
    suppliedNames <- objectNames[!is.na(objectNames) & nzchar(objectNames)]
    duplicateNames <- unique(suppliedNames[duplicated(suppliedNames)])
    if (length(duplicateNames) == 0) {
        return(invisible())
    }

    stopConflictingArguments(
        "Object names must be unique; duplicate names: ",
        paste(.sQuote(duplicateNames), collapse = ", "), ".",
        functionName = functionName,
        parameter = "objects",
        value = objectNames,
        constraint = "non-empty, non-missing object names must be unique",
        diagnosticId = "comparison.object_names_duplicated",
        context = list(
            duplicateNames = duplicateNames,
            objectPositions = which(objectNames %in% duplicateNames)
        )
    )
}

.getTrialComparisonData <- function(objects, niceColumnNamesEnabled = FALSE, includeAllParameters = FALSE) {
    .assertIsSingleLogical(niceColumnNamesEnabled, "niceColumnNamesEnabled")
    .assertIsSingleLogical(includeAllParameters, "includeAllParameters")
    if (!length(objects)) {
        return(data.frame())
    }
    
    .getComparisonData(objects, niceColumnNamesEnabled = niceColumnNamesEnabled,
        includeAllParameters = includeAllParameters)
}

#'
#' @title
#' Get Objects from a Collection
#'
#' @description
#' S3 generic for retrieving the objects contained in a collection.
#' The `TrialComparison` method delegates to `$getObjects()`.
#'
#' @param x An object supporting a `getObjects()` method, such as a `TrialComparison`.
#' @param ... Further arguments. Currently unused for `TrialComparison`.
#'
#' @return For `TrialComparison`, the list of contained objects in their stored
#'   order, retaining supplied names, or an empty list for an empty comparison.
#'
#' @details
#' The returned list contains references to the original R6 objects. Changing
#' a contained object affects subsequent comparisons. Replacing or removing
#' entries in the returned list does not change the comparison's membership;
#' use [add()] and [delete()] to modify the collection.
#'
#' @seealso
#' [getComparison()], [fetch.TrialComparison()]
#'
#' @examples
#' comparison <- getComparison(fixed = getDesignFixed(),
#'     sequential = getDesignGroupSequential(kMax = 2))
#' objects <- comparison |> getObjects()
#' names(objects)
#' identical(objects$fixed, comparison$fixed)
#'
#' @export
#'
getObjects <- function(x, ...) {
    UseMethod("getObjects")
}

#'
#' @title
#' Get Objects from a Collection
#'
#' @description
#' The `TrialComparison` method returns the list of contained objects.
#'
#' @rdname getObjects
#'
#' @export
#'
getObjects.TrialComparison <- function(x, ...) {
    .warnInCaseOfUnknownArguments(functionName = "getObjects.TrialComparison", ...)
    return(x$getObjects())
}

#'
#' @title
#' Add Objects to a Collection
#'
#' @description
#' S3 generic for adding objects. The `TrialComparison` method
#' delegates to `$add()` and modifies the existing comparison by reference.
#'
#' @param x An object supporting an `add()` method, such as a `TrialComparison`.
#' @param ... For `TrialComparison`, designs, plans, simulations, lists or
#'   comparisons to append. The optional named argument `objects` is appended
#'   after the other arguments, with the same rules as [getComparison()].
#'
#' @return The modified comparison invisibly for `TrialComparison`.
#'
#' @seealso
#' [getComparison()], [delete()]
#'
#' @examples
#' comparison <- getComparison(fixed = getDesignFixed())
#' comparison |> add(sequential = getDesignGroupSequential(kMax = 2))
#' comparison |> add(objects = list(other = getDesignFixed(alpha = 0.05)))
#'
#' @export
#'
add <- function(x, ...) UseMethod("add")

#'
#' @title
#' Add Objects to a Collection
#'
#' @description
#' The `TrialComparison` method appends objects to the comparison.
#'
#' @rdname add
#'
#' @export
#'
add.TrialComparison <- function(x, ...) x$add(...)

#'
#' @title
#' Delete an Object from a Collection
#'
#' @description
#' S3 generic for removing objects. The `TrialComparison` method
#' delegates to `$delete()` and modifies the existing comparison by reference.
#'
#' @param x An object supporting a `delete()` method, such as a `TrialComparison`.
#' @param ... For `TrialComparison`, exactly one `index`, supplied by position
#'   or name: a positive integer position or an exact object name.
#'   Invalid selections leave the comparison
#'   unchanged. Removing the last object leaves an empty comparison.
#'
#' @return The modified comparison invisibly for `TrialComparison`.
#'
#' @seealso
#' [getComparison()], [add()]
#'
#' @examples
#' comparison <- getComparison(fixed = getDesignFixed(),
#'     sequential = getDesignGroupSequential(kMax = 2))
#' comparison |> delete("fixed")
#' comparison |> delete(index = 1)
#' comparison |> add(getDesignFixed()) |> delete(1)
#'
#' @export
#'
delete <- function(x, ...) UseMethod("delete")

#'
#' @title
#' Delete an Object from a Collection
#'
#' @rdname delete
#'
#' @description
#' The `TrialComparison` method deletes one contained object by position or name.
#'
#' @export
#'
delete.TrialComparison <- function(x, ...) x$delete(...)

#'
#' @title
#' Trial Comparison
#'
#' @description
#' R6 container for existing trial planning objects, inheriting from
#'   `ParameterSet`. Create instances using [getComparison()].
#'
#' @name TrialComparison
#'
#' @seealso
#' [getComparison()]
#'
NULL

TrialComparison <- R6::R6Class("TrialComparison",
    inherit = ParameterSet,
    public = list(
        initialize = function(objects = list()) {
            objects <- .collectComparisonObjects(list(objects))
            .assertComparisonObjectNames(objects, functionName = "TrialComparison$initialize")
            # Reuse the comparison validation, without storing a table.
            .getTrialComparisonData(objects)
            super$initialize()
            private$.objects <- objects
        },
        getObjects = function() private$.objects,
        add = function(..., objects = NULL) {
            additions <- .collectComparisonObjects(list(...), objects)
            if (!length(additions)) {
                return(invisible(self))
            }
            combined <- c(private$.objects, additions)
            # Validate the complete candidate before changing the stored list.
            .assertComparisonObjectNames(combined, functionName = "TrialComparison$add")
            .getTrialComparisonData(combined)
            private$.objects <- combined
            invisible(self)
        },
        delete = function(index) {
            if (missing(index)) {
                stopMissingArgument("Supply the name or position of the object to delete",
                    functionName = "TrialComparison$delete", parameter = "index",
                    constraint = "one object name or position", diagnosticId = "comparison.object_index_missing")
            }
            if (length(index) != 1L) {
                stopArgumentLengthOutOfBounds("Supply exactly one object name or position",
                    functionName = "TrialComparison$delete", parameter = "index", value = index,
                    constraint = "exactly one index", expectedLength = 1L, actualLength = length(index),
                    diagnosticId = "comparison.object_index_length_invalid")
            }
            if (is.character(index) && !is.na(index) && nzchar(index)) {
                position <- match(index, names(private$.objects))
                if (is.na(position)) {
                    stopIllegalArgument("No object named '", index, "' exists in the comparison",
                        functionName = "TrialComparison$delete", parameter = "index", value = index,
                        constraint = "an exact name of a contained object",
                        diagnosticId = "comparison.object_name_unknown",
                        context = list(availableNames = names(private$.objects)))
                }
            } else {
                if (!is.numeric(index) || is.complex(index) || is.na(index) ||
                        !is.finite(index) || index != trunc(index)) {
                    stopIllegalArgument("The object index must be a non-empty name or an integer position",
                        functionName = "TrialComparison$delete", parameter = "index", value = index,
                        constraint = "one non-missing, non-empty character name or finite integer position",
                        diagnosticId = "comparison.object_index_invalid")
                }
                if (index < 1 || index > length(private$.objects)) {
                    stopIndexOutOfBounds("Object position is outside the comparison",
                        functionName = "TrialComparison$delete", parameter = "index", value = index, index = index,
                        constraint = "a position in [1, number of objects]",
                        lowerBound = 1L, upperBound = length(private$.objects),
                        diagnosticId = "comparison.object_index_out_of_bounds")
                }
                position <- index
            }
            remaining <- private$.objects[-position]
            private$.objects <- if (length(remaining)) {
                remaining
            } else {
                list()
            }
            invisible(self)
        },
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

#'
#' @title
#' Compare Existing Trial Planning Objects
#'
#' @description
#' `length()` returns the number of objects in the comparison.
#'
#' @rdname getComparison
#'
#' @param x A `TrialComparison`.
#'
#' @export
#'
length.TrialComparison <- function(x) {
    return(length(x$getObjects()))
}

#'
#' @title
#' Compare Existing Trial Planning Objects
#'
#' @description
#' `names()` returns the supplied names of the comparison objects.
#'
#' @rdname getComparison
#'
#' @export
#'
names.TrialComparison <- function(x) {
    return(names(x$getObjects()))
}

#'
#' @title
#' Access Objects in a Trial Comparison
#'
#' @description
#' Extracts a contained comparison object by name or position.
#'
#' @rdname extract.TrialComparison
#'
#' @param x A `TrialComparison`.
#' @param i A single object name or a positive integer position.
#' @param name An object name, supplied literally after `$`.
#' @param ... Further indices are not supported.
#' @param exact Whether character indices must match exactly, as for list `[[`.
#'   `$` always matches exactly.
#'
#' @return The contained object by reference, or NULL for an unknown name.
#'
#' @details
#' Object names are unique. Numeric positions refer to the
#' stored object order. R6 members take precedence for `$`: `x$add` is the method
#' even when an object is named "add"; use `x[["add"]]` to select that object.
#' `[[` selects contained objects first and falls back to existing R6 members for
#' other character names. This also preserves existing internal field access.
#' These methods provide extraction; replacing collection entries is not supported.
#' Use [getComparison()] or `$add()` to build or extend the collection.
#'
#' @examples
#' comparison <- getComparison(fixed = getDesignFixed(),
#'     sequential = getDesignGroupSequential(kMax = 2))
#' identical(comparison$fixed, comparison[["fixed"]])
#' identical(comparison[[1]], comparison$fixed)
#'
#' @export
#'
`[[.TrialComparison` <- function(x, i, ..., exact = TRUE) {
    if (missing(i)) {
        stopMissingArgument("An object name or position must be supplied",
            functionName = "[[.TrialComparison", parameter = "i",
            constraint = "one object name or position", diagnosticId = "comparison.object_index_missing")
    }
    extra <- list(...)
    if (length(extra)) {
        stopIllegalArgument("Only one object index is supported",
            functionName = "[[.TrialComparison", parameter = "...", value = extra,
            constraint = "no additional indices", diagnosticId = "comparison.multiple_indices_unsupported")
    }
    .assertIsSingleLogical(exact, "exact")
    if (length(i) != 1L) {
        stopArgumentLengthOutOfBounds("Supply exactly one object name or position",
            functionName = "[[.TrialComparison", parameter = "i", value = i,
            constraint = "exactly one index", expectedLength = 1L, actualLength = length(i),
            diagnosticId = "comparison.object_index_length_invalid")
    }
    objects <- .subset2(x, "getObjects")()
    if (is.character(i)) {
        if (!is.na(i) && nzchar(i) && i %in% names(objects)) {
            return(objects[[i, exact = TRUE]])
        }
        if (!is.na(i) && nzchar(i) && exists(i, envir = x, inherits = FALSE)) {
            return(.subset2(x, i))
        }
        return(objects[[i, exact = exact]])
    }
    if (!is.numeric(i) || is.complex(i) || is.na(i) || !is.finite(i) || i != trunc(i)) {
        stopIllegalArgument("The object index must be a name or an integer position",
            functionName = "[[.TrialComparison", parameter = "i", value = i,
            constraint = "one character name or finite integer position",
            diagnosticId = "comparison.object_index_invalid")
    }
    if (i < 1 || i > length(objects)) {
        stopIndexOutOfBounds("Object position is outside the comparison",
            functionName = "[[.TrialComparison", parameter = "i", value = i, index = i,
            constraint = "a position in [1, number of objects]", lowerBound = 1L, upperBound = length(objects),
            diagnosticId = "comparison.object_index_out_of_bounds")
    }
    objects[[i]]
}

#'
#' @title
#' Access Objects in a Trial Comparison
#'
#' @description
#' `$` accesses named comparison objects while preserving public R6 members.
#'
#' @rdname extract.TrialComparison
#'
#' @export
#'
`$.TrialComparison` <- function(x, name) {
    # Resolve actual bindings without dispatch, keeping R6 methods and fields safe.
    if (exists(name, envir = x, inherits = FALSE)) {
        return(.subset2(x, name))
    }
    .subset2(x, "getObjects")()[[name, exact = TRUE]]
}

#'
#' @title
#' Fetch Objects from a Trial Comparison
#'
#' @description
#' Extracts contained objects by name or position without modifying the comparison
#' or copying its objects. A single selection returns the object itself by default.
#'
#' @param x A `TrialComparison`.
#' @param ... Object names or integer positions, including vectors and multiple
#'   selections. Names may be quoted or unquoted. Positive positions count from
#'   the beginning and negative positions from the end. With no selection, the
#'   last object is returned. Quasi-quotation and programmatic selectors are
#'   supported; an unquoted name matching a contained object takes precedence
#'   over a variable of the same name in the calling environment.
#' @param output One of "value" (default), "named", "labeled", or "list".
#'   "value" returns a single selected object directly and multiple selections
#'   as a named list. The other modes always return a named list. Collection
#'   labels are the supplied object names, with positions for unnamed objects;
#'   consequently "labeled" and "named" are equivalent for comparisons.
#'
#' @return A contained object by reference or a list of objects. An unknown name
#'   returns NULL (retained as a NULL entry in list output).
#'   Empty selector vectors return an empty list.
#'
#' @details
#' `fetch(x, "fixed")`, `fetch(x, fixed)` and `fetch(x, 1)` provide the named and
#' positional object access supported by `[[`. Only contained objects are selected;
#' R6 members are never returned as a fallback. Numeric positions must be finite,
#' nonzero integers within the collection bounds. Selecting the last object of
#' an empty comparison raises an index error.
#'
#' @seealso
#' [fetch()] and [getComparison()].
#'
#' @examples
#' comparison <- getComparison(fixed = getDesignFixed(),
#'     sequential = getDesignGroupSequential(kMax = 2))
#' identical(comparison |> fetch("fixed"), comparison[["fixed"]])
#' identical(comparison |> fetch(fixed), comparison$fixed)
#' identical(comparison |> fetch(1), comparison[[1]])
#' comparison |> fetch(-1)
#' comparison |> fetch(1:2, output = "list")
#'
#' @export
#'
fetch.TrialComparison <- function(x, ..., output = c("value", "named", "labeled", "list")) {
    output <- match.arg(output)
    selectors <- rlang::enquos(...)
    if (!length(selectors)) {
        selectors <- list(rlang::new_quosure(-1L))
    }
    objects <- x$getObjects()
    objectNames <- names(objects)
    positions <- list()
    labels <- list()
    for (i in seq_along(selectors)) {
        selector <- selectors[[i]]
        expression <- rlang::quo_get_expr(selector)
        if (is.symbol(expression) && (as.character(expression) %in% objectNames ||
                !rlang::env_has(rlang::quo_get_env(selector), as.character(expression), inherit = TRUE))) {
            value <- as.character(expression)
        } else {
            value <- rlang::eval_tidy(selector)
        }
        index <- .getComparisonFetchPositions(value, objects, selectorNumber = i)
        label <- if (is.character(value)) {
            value
        } else {
            if (is.null(objectNames)) {
                as.character(index)
            } else {
                objectNames[index]
            }
        }
        unnamed <- is.na(label) | !nzchar(label)
        label[unnamed] <- as.character(index[unnamed])
        positions[[i]] <- index
        labels[[i]] <- label
    }
    positions <- unlist(positions, use.names = FALSE)
    labels <- unlist(labels, use.names = FALSE)
    if (!length(positions)) {
        return(list())
    }
    result <- lapply(positions, function(position) if (is.na(position)) {
        NULL
    } else {
        objects[[position]]
    })
    names(result) <- labels
    if (output == "value" && length(result) == 1L) {
        return(result[[1L]])
    }
    result
}

.getComparisonFetchPositions <- function(selector, objects, selectorNumber) {
    if (is.character(selector) && is.null(dim(selector))) {
        positions <- match(selector, names(objects))
        positions[is.na(selector) | !nzchar(selector)] <- NA_integer_
        return(positions)
    }
    if (!is.numeric(selector) || is.complex(selector) || !is.null(dim(selector)) ||
            anyNA(selector) || any(!is.finite(selector)) || any(selector != trunc(selector))) {
        stopIllegalArgument("Select comparison objects by character names or finite integer positions",
            functionName = "fetch.TrialComparison", parameter = "...", value = selector,
            constraint = "character names or finite integer positions",
            diagnosticId = "comparison.fetch_selector_invalid", context = list(selectorNumber = selectorNumber))
    }
    size <- length(objects)
    invalid <- which(selector == 0 | selector < -size | selector > size)
    if (length(invalid)) {
        stopIndexOutOfBounds("Object position is outside the comparison",
            functionName = "fetch.TrialComparison", parameter = "...", value = selector,
            index = selector[invalid[1L]], lowerBound = -size, upperBound = size,
            constraint = "nonzero integer positions in [-number of objects, number of objects]",
            diagnosticId = "comparison.fetch_index_out_of_bounds",
            context = list(selectorNumber = selectorNumber))
    }
    ifelse(selector < 0, size + selector + 1L, selector)
}

#'
#' @title
#' Trial Comparison Data Frame
#'
#' @description
#' Converts a trial comparison to an unrounded data frame.
#'
#' @param x A `TrialComparison`.
#' @param row.names Optional row names for the result.
#' @param optional Unused; included for compatibility with `as.data.frame()`.
#' @param ... Further arguments. Currently unused.
#' @inheritParams .getComparisonData
#' @param removeConstantParameters If TRUE, retain identifiers and only parameters
#'   with different object profiles, using the same rules as `summary()`.
#'
#' @return An unrounded data.frame. All columns are returned by default.
#'
#' @export
#'
as.data.frame.TrialComparison <- function(x, row.names = NULL, optional = FALSE, ...,
        niceColumnNamesEnabled = FALSE, includeAllParameters = FALSE, removeConstantParameters = FALSE) {
    .warnInCaseOfUnknownArguments(functionName = "as.data.frame.TrialComparison", ...)
    .assertIsSingleLogical(removeConstantParameters, "removeConstantParameters")
    .assertIsSingleLogical(niceColumnNamesEnabled, "niceColumnNamesEnabled")
    data <- .getTrialComparisonData(x$getObjects(), includeAllParameters = includeAllParameters)
    allNames <- names(data)
    if (removeConstantParameters) {
        info <- .getTrialComparisonInfo(x, data)
        selected <- getComparisonParameterNames(data, varyingOnly = FALSE)
        selected <- selected[selected %in% c(.comparisonIdentifiers, info$paramName[info$nProfiles > 1L])]
        data <- data[selected]
    }
    if (niceColumnNamesEnabled) {
        labels <- names(.getTrialComparisonData(x$getObjects(), niceColumnNamesEnabled = TRUE,
            includeAllParameters = includeAllParameters))
        names(data) <- labels[match(names(data), allNames)]
    }
    if (!is.null(row.names)) {
        rownames(data) <- row.names
    }
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
    if (is.numeric(shown)) {
        text[is.nan(shown)] <- "NaN"
    }
    text[!nzchar(trimws(text))] <- '""'
    text <- paste(.comparisonText(text, if (length(shown) == 1L) {
        55L
    } else {
        20L
    }), collapse = ", ")
    if (length(values) > maxValues) {
        text <- paste0(text, ", ... (", length(values), " values)")
    }
    text
}

.comparisonRole <- function(objects, parameter) {
    if (parameter %in% .comparisonScope) {
        return("Object types and scope")
    }
    types <- unlist(lapply(objects, function(object) {
        c(object$.parameterTypes[[parameter]], if (!is.null(object[[".design"]])) {
            object$.design$.parameterTypes[[parameter]]
        })
    }), use.names = FALSE)
    if (any(types %in% c(C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE))) {
        return("Inputs and design settings")
    }
    "Results and derived parameters"
}

.getTrialComparisonTitle <- function(objects, summaryEnabled = FALSE) {
    categories <- vapply(objects, function(object) {
        if (inherits(object, "SimulationResults")) {
            return("Power simulation")
        }
        
        if (inherits(object, c("TrialDesign", "TrialDesignCharacteristics"))) {
            return("Design")
        }
        
        if (inherits(object, "TrialDesignPlan")) {
            if (object$.isSampleSizeObject()) {
                return("Sample size calculation")
            }
            
            if (object$.isPowerObject()) {
                return("Power calculation")
            }
        }
        
        return("Design plan")
    }, character(1))

    categories <- unique(categories)
    category <- if (!length(categories)) {
        "Trial"
    } else if (length(categories) == 1L) {
        categories[[1L]]
    } else {
        "Design plan"
    }
    title <- paste(category, "comparison")
    if (summaryEnabled) {
        title <- paste(title, "summary")
    }
    return(title)
}

.comparisonObjectType <- function(object, startWithUpperCase = TRUE) {
    label <- object$.toString(startWithUpperCase = FALSE)
    if (inherits(object, "TrialDesignPlan") && !inherits(object, "SimulationResults")) {
        label <- paste(.formatCamelCase(object$.objectType), label)
    }
    if (startWithUpperCase) {
        label <- .firstCharacterToUpperCase(label)
    }
    return(label)
}

.comparisonOverview <- function(objects) {
    if (!length(objects)) {
        return("Empty trial comparison.")
    }
    types <- vapply(objects, .comparisonObjectType, character(1), startWithUpperCase = FALSE)
    if (length(unique(types)) == 1L &&
            all(vapply(objects, inherits, logical(1), "TrialDesignPlan")) &&
            !any(vapply(objects, inherits, logical(1), "SimulationResults"))) {
        return(paste0("Comparison of ", length(objects), " ", types[1L],
            if (length(objects) == 1L) {
                " calculation."
            } else {
                " calculations."
            }))
    }
    distinct <- unique(types)
    description <- ""
    for (type in head(distinct, 3L)) {
        description <- .concatenateSummaryText(description, .comparisonText(type, 65L))
    }
    if (length(distinct) > 3L) {
        description <- .concatenateSummaryText(description, "...")
    }
    return(paste0("Comparison of ", length(objects), " trial planning ",
        if (length(objects) == 1L) {
            "result: "
        } else {
            "results: "
        }, description, "."))
}

.comparisonCaption <- function(parameter, object = NULL) {
    if (!is.null(object) && !parameter %in% c(.comparisonScope, .comparisonIdentifiers)) {
        if (!parameter %in% names(object)) {
            design <- .getTrialDesign(object)
            if (is.null(design) || !parameter %in% names(design)) {
                return(NA_character_)
            }
            object <- design
        }
    }
    caption <- .getParameterCaption(parameter, obj = object, tableOutputEnabled = FALSE)
    if (is.null(caption) || length(caption) != 1L || is.na(caption) || !nzchar(caption)) {
        caption <- C_TABLE_COLUMN_NAMES[[parameter]]
    }
    if (is.null(caption) || length(caption) != 1L || is.na(caption) || !nzchar(caption)) {
        caption <- switch(parameter,
            objectType = "Object type", endpoint = "Endpoint", calculationTarget = "Calculation target",
            situation = "Situation", stage = "Stage", parameter)
    }
    return(caption)
}

.comparisonCaptionLabel <- function(parameter, captions) {
    captions <- unique(captions[!is.na(captions) & nzchar(captions)])
    if (!length(captions)) {
        return(.comparisonCaption(parameter))
    }
    return(paste(captions, collapse = " / "))
}

.comparisonProfile <- function(data, parameter) {
    values <- data[[parameter]]
    if (length(unique(values)) <= 1L) {
        return(data.frame(value = values[1L]))
    }
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
    if (length(keys)) {
        profile <- profile[do.call(order, profile[keys]), , drop = FALSE]
    }
    rownames(profile) <- NULL
    profile
}

.getTrialComparisonInfo <- function(object, data = .getTrialComparisonData(object$getObjects())) {
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
    # Capture object-specific captions separately: captions must not participate
    # in exact value-profile grouping or change when a summary is printed later.
    info$paramCaptions <- lapply(info$paramName, function(parameter) {
        vapply(objects, function(item) {
            .comparisonCaption(parameter, item)
        }, character(1))
    })
    info$paramCaption <- vapply(seq_len(nrow(info)), function(i) {
        .comparisonCaptionLabel(info$paramName[i], info$paramCaptions[[i]])
    }, character(1))
    info$profiles <- profiles
    info$profileGroups <- groups
    rownames(info) <- NULL
    info
}

#'
#' @title
#' Print a Trial Comparison
#'
#' @description
#' Prints a compact overview of a trial comparison using descriptive parameter captions.
#'
#' @param x A `TrialComparison`.
#' @param ... Further arguments. Currently unused.
#' @param markdown Whether to return Markdown output. NA uses rpact settings.
#' @param digits Display rounding only, as in [getComparisonParameterDescriptions()].
#' @param maxTypes,maxMetrics Maximum numbers of object types and selected metrics
#'   to display. These limits do not affect [as.data.frame.TrialComparison()].
#'
#' @return The object invisibly, or knitr Markdown output when requested.
#'
#' @export
#'
print.TrialComparison <- function(x, ..., markdown = NA, digits = NA_integer_, maxTypes = 5L, maxMetrics = 6L) {
    .warnInCaseOfUnknownArguments(functionName = "print.TrialComparison", ...)
    .assertIsSingleLogical(markdown, "markdown", naAllowed = TRUE)
    if (is.na(markdown)) {
        markdown <- .isMarkdownEnabled("print")
    }
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
    data <- .getTrialComparisonData(object$getObjects())
    info <- getComparisonParameterInfo(data)
    getComparisonParameterDescriptions(info[FALSE, ], digits = digits) # Validate display precision.
    object$.resetCat()
    emit <- function(...) {
        object$.cat(..., consoleOutputEnabled = consoleOutputEnabled)
    }
    indent <- if (consoleOutputEnabled) {
        "    "
    } else {
        "  "
    }

    object$.cat(.getTrialComparisonTitle(object$getObjects()), "\n\n",
        heading = 1, consoleOutputEnabled = consoleOutputEnabled)
    emit(length(object), " objects; ", nrow(data), " comparison rows; ", ncol(data), " columns.\n")

    types <- table(vapply(object$getObjects(), function(item) {
        .comparisonObjectType(item)
    }, ""))
    typeLabels <- .comparisonText(head(names(types), maxTypes), 55L)
    typeWidth <- max(c(0L, nchar(typeLabels)))
    if (length(types)) {
        emit("\n")
        object$.cat("Object types:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
    }
    for (i in seq_along(typeLabels)) {
        emit(indent, sprintf("%-*s : %s", typeWidth, typeLabels[i], types[i]), "\n")
    }
    if (length(types) > maxTypes) {
        emit(indent, "... ", length(types) - maxTypes, " more object types.\n")
    }

    metrics <- intersect(c("kMax", "alpha", "beta", "maxNumberOfSubjects", "maxNumberOfEvents",
        "nFixed", "overallReject", "expectedNumberOfSubjects", "expectedNumberOfEvents", "studyDuration"), info$paramName[!info$isEmpty])
    shownMetrics <- head(metrics, maxMetrics)
    metricCaptions <- vapply(shownMetrics, function(parameter) {
        captions <- vapply(object$getObjects(), function(item) {
            .comparisonCaption(parameter, item)
        }, character(1))
        .comparisonText(.comparisonCaptionLabel(parameter, captions), 65L)
    }, character(1))
    metricWidth <- max(c(0L, nchar(metricCaptions)))
    if (length(metrics)) {
        emit("\n")
        object$.cat("Observed metrics (across objects, stages and situations):\n",
            heading = 2, consoleOutputEnabled = consoleOutputEnabled)
    }
    for (parameter in shownMetrics) {
        row <- info[info$paramName == parameter, , drop = FALSE]
        values <- row$values[[1L]]
        if (is.numeric(values) && length(values) > 1L) {
            values <- range(values)
        }
        emit(indent, sprintf("%-*s : %s", metricWidth, metricCaptions[[parameter]], .comparisonPreview(values, digits = digits)),
            if (row$hasMissingValues) {
                " (missing values present)"
            } else {
                ""
            }, "\n")
    }
    if (length(metrics) > maxMetrics) {
        emit(indent, "... ", length(metrics) - maxMetrics, " more observed metrics.\n")
    }

    emit("\n")
    object$.cat("Available operations:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
    operations <- c("summary(x)", "as.data.frame(x)", "plot(x, xParamName = ..., yParamName = ...)", "getObjects(x)")
    descriptions <- c("Compare differences", "Access all data", "Plot selected parameters", "Access contained objects")
    operationWidth <- max(nchar(operations))
    for (i in seq_along(operations)) {
        emit(indent, sprintf("%-*s : %s", operationWidth, operations[i], descriptions[i]), "\n")
    }
    return(invisible(object))
}

#'
#' @title
#' Summarize Differences Between Trial Planning Objects
#'
#' @description
#' Summarizes differences between the object profiles in a trial comparison.
#'
#' @param object A `TrialComparison`.
#' @param ... Further arguments. Currently unused.
#' @param digits Display rounding only; exact values determine differences.
#' @param maxParameters Maximum number of differing parameters shown, across all
#'   sections. Increase this to inspect further differences.
#' @param maxProfiles Maximum number of distinct profiles shown per parameter.
#' @param maxObjects Maximum object numbers/names listed per profile.
#' @param maxValues Maximum profile values shown per preview.
#' @param output One of "all", "title", "overview", or "body".
#'
#' @return A `TrialComparisonSummary` inheriting from `SummaryFactory`. Its
#'   `parameterInfo` contains metadata, full exact profiles and object group IDs.
#'   The displayed summary is a snapshot; call `summary()` again after changing
#'   an underlying object. Input roles take precedence when roles differ across
#'   the compared object types.
#'
#' @details
#' Parameter captions are resolved separately for each original object, including
#' its design when a parameter comes from that design. `parameterInfo$paramName`
#' retains the original column names; `paramCaption` contains the display label
#' and `paramCaptions` contains captions in object order (NA for absent parameters).
#' These captions are captured with the summary. Distinct object-specific captions
#' are shown together and identified alongside the corresponding object labels;
#' they do not change exact value-profile grouping. Unnamed entries are labelled
#' by their calculation or design type and collection position. The overview and
#' these type labels are captured with the summary. The legend explains exact
#' profile comparison and mentions NA only when displayed profiles contain
#' missing values. Titles distinguish designs, sample size calculations, power
#' calculations, and power simulations. Mixed comparisons use a design plan
#' comparison title; empty comparisons retain the general trial comparison title.
#'
#' @export
#'
summary.TrialComparison <- function(object, ..., digits = NA_integer_, maxParameters = 12L,
        maxProfiles = 3L, maxObjects = 6L, maxValues = 4L, output = c("all", "title", "overview", "body")) {
    .warnInCaseOfUnknownArguments(functionName = "summary.TrialComparison", ...)
    limits <- list(maxParameters = maxParameters, maxProfiles = maxProfiles, maxObjects = maxObjects, maxValues = maxValues)
    for (parameter in names(limits)) {
        .assertComparisonLimit(limits[[parameter]], parameter, "summary.TrialComparison")
    }
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
        .objectTypes = NULL,
        .overview = NULL,
        .numberOfObjects = NULL,
        initialize = function(object, info, limits, digits, output) {
            super$initialize(object = object, output = output)
            self$title <- .getTrialComparisonTitle(object$getObjects(), summaryEnabled = TRUE)
            self$parameterInfo <- info
            self$.limits <- limits
            self$.digits <- digits
            self$.objectNames <- names(object)
            self$.objectTypes <- vapply(object$getObjects(), .comparisonObjectType, character(1))
            self$.overview <- .comparisonOverview(object$getObjects())
            self$.numberOfObjects <- length(object)
        },
        .show = function(..., consoleOutputEnabled = TRUE) {
            .showTrialComparisonSummary(self, consoleOutputEnabled)
            invisible(self)
        }
    )
)

.showTrialComparisonSummary <- function(summary, consoleOutputEnabled) {
    summary$.resetCat()
    emit <- function(...) {
        summary$.cat(..., consoleOutputEnabled = consoleOutputEnabled)
    }
    parameterIndent <- if (consoleOutputEnabled) {
        "    "
    } else {
        ""
    }
    profileIndent <- if (consoleOutputEnabled) {
        "        "
    } else {
        "  "
    }
    info <- summary$parameterInfo
    limits <- summary$.limits
    different <- which(info$nProfiles > 1L)
    common <- which(info$nProfiles == 1L & !info$isEmpty)
    empty <- which(info$nProfiles == 1L & info$isEmpty)
    if (summary$output %in% c("all", "title")) {
        summary$.cat(summary$title, "\n\n", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
    }
    if (summary$output %in% c("all", "overview")) {
        emit(paste(strwrap(summary$.overview, width = 100L), collapse = "\n"), "\n")
        overview <- .concatenateSummaryText(
            paste("The comparison identifies", length(different), if (length(different) == 1L) {
                "differing parameter"
            } else {
                "differing parameters"
            }),
            paste(length(common), if (length(common) == 1L) {
                "shared parameter or profile"
            } else {
                "shared parameters or profiles"
            }), sep = " and ")
        if (length(empty)) {
            overview <- paste0(overview, ". ", length(empty), if (length(empty) == 1L) {
                " parameter is empty"
            } else {
                " parameters are empty"
            })
        }
        emit(paste(strwrap(paste0(overview, "."), width = 100L), collapse = "\n"), "\n")
        if (length(common)) {
            emit("Shared (examples): ", .comparisonText(paste(head(info$paramCaption[common], 6L), collapse = ", "), 130L),
                if (length(common) > 6L) {
                    ", ..."
                } else {
                    "."
                }, "\n")
        }
    }
    if (!summary$output %in% c("all", "body")) {
        return(invisible(summary))
    }
    if (!length(different)) {
        emit("\nNo parameter differences between the objects.\n")
    }
    roles <- c("Object types and scope", "Inputs and design settings", "Results and derived parameters")
    ordered <- unlist(lapply(roles, function(role) {
        different[info$role[different] == role]
    }), use.names = FALSE)
    # Share the display budget across sections so a long input list cannot hide
    # every result difference. Within each section preserve source-column order.
    sections <- lapply(roles, function(role) {
        different[info$role[different] == role]
    })
    interleaved <- unlist(lapply(seq_len(max(c(0L, lengths(sections)))), function(position) {
        unlist(lapply(sections, function(indices) {
            indices[position][!is.na(indices[position])]
        }), use.names = FALSE)
    }), use.names = FALSE)
    shown <- head(interleaved, limits$maxParameters)
    shownNA <- FALSE
    sectionStarted <- summary$output == "all"
    for (role in roles) {
        indices <- shown[info$role[shown] == role]
        if (!length(indices)) {
            next
        }
        if (sectionStarted) {
            emit("\n")
        }
        summary$.cat(paste0(role, ":\n"), heading = 2, consoleOutputEnabled = consoleOutputEnabled)
        sectionStarted <- TRUE
        for (i in indices) {
            row <- info[i, , drop = FALSE]
            # The common descriptions supply type/role labels, without unbounded values.
            labelInfo <- row
            labelInfo$isBinary <- labelInfo$isSingleValue <- FALSE
            labelInfo$values <- list(character())
            labelInfo$paramName <- row$paramCaption
            description <- getComparisonParameterDescriptions(labelInfo, digits = summary$.digits)
            if (!consoleOutputEnabled) {
                description <- sub(": ", " - ", description, fixed = TRUE)
            }
            emit("\n")
            summary$.cat(parameterIndent, .comparisonText(description, 75L), " [", row$nProfiles, " profiles]\n",
                heading = 3, consoleOutputEnabled = consoleOutputEnabled)
            groups <- row$profileGroups[[1L]]
            for (group in seq_len(min(row$nProfiles, limits$maxProfiles))) {
                members <- which(groups == group)
                ids <- head(members, limits$maxObjects)
                labels <- paste(.comparisonText(summary$.objectTypes[ids], 40L), ids)
                named <- rep(FALSE, length(ids))
                if (!is.null(summary$.objectNames)) {
                    objectNames <- summary$.objectNames[ids]
                    named <- !is.na(objectNames) & nzchar(objectNames)
                    labels[named] <- .comparisonText(objectNames[named], 18L)
                }
                captions <- row$paramCaptions[[1L]]
                variedCaptions <- length(unique(stats::na.omit(captions))) > 1L
                if (variedCaptions) {
                    hasCaption <- !is.na(captions[ids]) & nzchar(captions[ids])
                    labels[hasCaption] <- paste0(labels[hasCaption], " (", captions[ids[hasCaption]], ")")
                }
                membersText <- paste(labels, collapse = ", ")
                if (!variedCaptions && !any(named) && length(members) > 1L) {
                    if (length(unique(summary$.objectTypes[ids])) == 1L) {
                        membersText <- paste(.comparisonText(summary$.objectTypes[ids[1L]], 40L),
                            paste(ids, collapse = ", "))
                    }
                }
                if (length(members) > limits$maxObjects) {
                    omitted <- length(members) - limits$maxObjects
                    omittedLabel <- if (omitted == 1L) {
                        "object"
                    } else {
                        "objects"
                    }
                    membersText <- paste0(membersText, ", ... (+", omitted, " ", omittedLabel, ")")
                }
                if (variedCaptions) {
                    membersText <- .comparisonText(membersText, 80L)
                }
                profile <- row$profiles[[1L]][[members[1L]]]
                keys <- setdiff(names(profile), "value")
                shownNA <- shownNA || anyNA(head(profile$value, limits$maxValues)) ||
                    any(vapply(keys, function(key) {
                        anyNA(head(profile[[key]], limits$maxValues))
                    }, logical(1)))
                values <- .comparisonPreview(profile$value, limits$maxValues, summary$.digits, row$values[[1L]])
                if (length(keys)) {
                    positions <- vapply(keys, function(key) {
                        paste0(.comparisonCaption(key), " = ", .comparisonPreview(profile[[key]], limits$maxValues))
                    }, "")
                    values <- paste0("[", paste(positions, collapse = "; "), "] ", values)
                }
                emit(profileIndent, membersText, ": ", .comparisonText(values, 85L), "\n")
            }
            if (row$nProfiles > limits$maxProfiles) {
                emit(profileIndent, "... ", row$nProfiles - limits$maxProfiles, " more profiles.\n")
            }
        }
    }
    emit("\n")
    if (length(ordered) > length(shown)) {
        emit(
            "... ", length(ordered) - length(shown),
            " more differing parameters; increase maxParameters.\n"
        )
    }
    if (length(shown)) {
        summary$.cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
        legendIndent <- if (consoleOutputEnabled) {
            parameterIndent
        } else {
            profileIndent
        }
        emit(legendIndent, "Profiles compare exact values at each stage and situation.\n")
        if (shownNA) {
            emit(legendIndent, "NA: missing/unavailable.\n")
        }
        if (!is.na(summary$.digits)) {
            emit(legendIndent, "Display values are rounded.\n")
        }
        emit("\n")
    }
    emit("Full data: as.data.frame(x). Exact profiles: summary(x)$parameterInfo.\n")
    return(invisible(summary))
}

#'
#' @title
#' Plot a Trial Comparison
#'
#' @description
#' Plots selected variables from a trial comparison.
#'
#' @param x A `TrialComparison` or its summary.
#' @param xParamName,yParamName Optional axis column names. Omitted axes are
#'   selected only when there is exactly one candidate: inputs/design settings
#'   are preferred for x, results for y, then other varying numeric columns.
#'   Identifier columns are excluded; stages remain eligible axes. `yParamName`
#'   may also explicitly select multiple numeric parameters. Their axis sides
#'   can be controlled through `yAxisSide` passed to [plot.TrialComparison()].
#' @param ... Arguments passed to [plot.TrialComparison()], including categories,
#'   grouping, titles and line/point settings. Explicit axis selections prevail.
#'
#' @return A ggplot object. Ambiguous selections raise an error listing candidate
#'   descriptions and a concrete example.
#'
#' @export
#'
plot.TrialComparison <- function(x, xParamName = NULL, yParamName = NULL, ...) {
    return(.plotComparison(x, xParamName = xParamName, yParamName = yParamName, ...))
}

#'
#' @title
#' Plot a Trial Comparison
#'
#' @description
#' The summary method plots the objects contained in the comparison.
#'
#' @rdname plot.TrialComparison
#'
#' @param showSummary If TRUE, also print the summary before creating its plot.
#'
#' @export
#'
plot.TrialComparisonSummary <- function(x, xParamName = NULL, yParamName = NULL, ..., showSummary = FALSE) {
    .assertIsSingleLogical(showSummary, "showSummary")
    if (showSummary) {
        print(x)
    }
    plot(x$object, xParamName = xParamName, yParamName = yParamName, ...)
}

.comparisonHasConsumerCall <- function(calls) {
    any(vapply(calls, function(call) {
        if (!is.call(call)) {
            return(FALSE)
        }
        name <- call[[1L]]
        if (is.call(name) && as.character(name[[1L]]) %in% c("::", ":::")) {
            name <- name[[3L]]
        }
        is.symbol(name) && as.character(name) %in%
            c("summary", "summary.TrialComparison", "plot", "plot.TrialComparison")
    }, logical(1)))
}
