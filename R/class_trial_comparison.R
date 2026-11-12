## |
## |  rpact: Confirmatory Adaptive Clinical Trial Design and Analysis
## |  This file is part of the rpact R package.
## |
## |  RPACT <U+2014> Statistical Tools for Drug Development
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
#' or power plans, simulation results, and power/ASN results without recalculating
#' them. A single
#' comparison summary returns its underlying comparison.
#'
#' @param objects Optional objects, lists of objects, `TrialComparison` objects,
#'   or `TrialComparisonSummary` objects.
#'   NULL is treated as an empty list.
#' @param ... Designs, design characteristics, plans, simulations,
#'   `PowerAndAverageSampleNumberResult`, `EventProbabilities`, `NumberOfSubjects`,
#'   `ConditionalPowerResults` (including subclasses), `PerformanceScore`,
#'   `FisherInformation`, or `TrialDesignOptimalConditionalErrorCharacteristics`
#'   objects, lists thereof,
#'   `TrialComparison` objects, or `TrialComparisonSummary` objects. Entries
#'   from `...` precede entries from `objects`.
#'
#' @return An R6 `TrialComparison` inheriting from `ParameterSet`. For a single
#'   `TrialComparisonSummary`, its underlying comparison is returned by reference;
#'   otherwise a new collection is created.
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
#' A single summary can be unwrapped with `summaryObject |> getComparison()`,
#' returning exactly `summaryObject$object`. Changes to the returned collection
#' therefore affect that underlying comparison. In combinations or nested lists,
#' summaries contribute their underlying comparisons' current objects; a new
#' collection is created without changing the source collections. Summary display
#' settings are not transferred to the comparison.
#'
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
#' of the number of rows. Constant numeric vector cells, such as `1, 1`,
#' compare equal to the corresponding scalar. Nonconstant profiles retain stage/situation positions;
#' repeated values across the other dimension are collapsed. Missing values and
#' their positions participate in exact comparisons. No rounding or numerical
#' tolerance is used to decide whether profiles differ.
#'
#' For comparisons containing interim analyses, fixed plans' `nFixed`, `nFixed1`,
#' and `nFixed2` are harmonized to `numberOfSubjects`, `numberOfSubjects1`, and
#' `numberOfSubjects2`. The corresponding stored group sizes are included for
#' sequential plans. Comparisons of fixed plans alone retain the fixed names.
#' With `includeAllParameters = TRUE`, sequential plans' `nFixed` reference values
#' remain separate from their stage-wise sample sizes. Fixed survival plans also
#' use `cumulativeEventsPerStage` for `eventsFixed` when interim analyses occur
#' in the comparison; sequential plans retain their fixed-design event reference.
#'
#' Available sample-size expectations `expectedNumberOfSubjectsH1` and
#' `expectedEventsH1` use `expectedNumberOfSubjects` and `expectedNumberOfEvents`.
#' These expectations refer to each scenario's supplied alternative. Harmonization
#' does not change alternatives or stopping rules, combine H0/H0-H1 expectations,
#' or reinterpret simulation assumptions used for conditional power.
#' Conflicting non-missing target values remain in separate columns.
#' Power/ASN results from [getPowerAndAverageSampleNumber()] retain their generic
#' `theta`, `nMax`, and `averageSampleNumber` scales. Individual `theta` scenarios
#' retain their stage coordinates even in full exports. The comparison includes `nMax`
#' even when the compact source data frame omits it; no endpoint or conversion to
#' subjects or events is inferred. With column harmonization enabled, their
#' stage-wise `earlyStop` becomes `earlyStopPerStage`, `overallEarlyStop` becomes
#' `earlyStop`, and `overallFutility` becomes `futilityStop`. `calculatedPower`
#' merges into `overallReject` when the values agree. Original names remain
#' available with `harmonizeColumnNames = FALSE` in data frames and summaries.
#' Original objects are unchanged.
#'
#' Event probabilities and recruitment results retain their time coordinates and
#' assumptions. Conditional-power results retain planned stages, treatment arms
#' or populations, and observed stage and data context; no analysis is rerun.
#' Performance scores retain scenario alternatives, simulation inputs and design
#' settings. Fisher information retains selected stages, situations, the source
#' plan when available, and its cumulative or stage-wise type; these scales are
#' not harmonized. Profiles and plot groups respect time, arm, population and
#' situation coordinates.
#'
#' Optimal conditional-error characteristics are supported internally, including
#' their theta-by-stage results and atomic design settings. Their constructor and
#' characteristic functions remain unexported. Function-valued settings are not
#' compared as scalar parameters. Deprecated `TrialDesignSet` objects and standalone
#' optimal conditional-error designs are not supported.
#'
#' Console and Markdown output are bounded. Limits and display rounding do not
#' change the underlying data. [as.data.frame.TrialComparison()] returns the full
#' comparison table. [plot.TrialComparison()] selects axes only when unambiguous.
#' [getObjectRCode()] reconstructs the comparison and its contained objects;
#' executing generated simulation code reruns the corresponding simulations.
#'
#' @examples
#' asn <- getPowerAndAverageSampleNumber(getDesignGroupSequential(kMax = 2),
#'     theta = c(0, 0.2), nMax = 100
#' )
#' getComparison(asn, asn)
#'
#' comparison <- getComparison(list(
#'     fixed = getDesignFixed(), sequential = getDesignGroupSequential(kMax = 2)
#' ))
#' print(comparison)
#' comparisonSummary <- summary(comparison)
#' identical(comparisonSummary |> getComparison(), comparison)
#' head(as.data.frame(comparison))
#' withCharacteristics <- getComparison(
#'     getDesignGroupSequential(kMax = 2),
#'     getDesignCharacteristics(getDesignGroupSequential(kMax = 3))
#' )
#' combined <- getComparison(comparison, other = getDesignFixed(alpha = 0.05))
#' combined$add(list(getDesignFixed(alpha = 0.01)))
#' empty <- getComparison()
#' empty$add(getDesignFixed())
#' combined$delete(1)
#' combined$delete("sequential")
#' \dontrun{
#' plot(comparison,
#'     xAxis = "objectNumber",
#'     yAxis = "criticalValues",
#'     plotLineEnabled = FALSE
#' )
#' }
#'
#' @export
#'
getComparison <- function(..., objects = NULL) {
    arguments <- list(...)
    if (!is.null(objects)) {
        arguments <- c(arguments, list(objects))
    }
    if (length(arguments) == 1L && inherits(arguments[[1L]], "TrialComparisonSummary")) {
        return(arguments[[1L]]$object)
    }
    return(TrialComparison$new(.collectComparisonObjects(arguments)))
}

#'
#' @title
#' Start or Extend a Trial Comparison
#'
#' @description
#' S3 generic for comparing existing trial planning objects. The `ParameterSet`
#' method creates a comparison or appends to an existing `TrialComparison`.
#'
#' @param x A supported trial planning object or a `TrialComparison`.
#' @param y A trial design, design characteristics, sample size or power plan,
#'   simulation result, list of supported objects, or `TrialComparison` to add.
#'   May be omitted to create a comparison containing only `x`.
#'
#' @return A `TrialComparison`. When `x` is already a comparison, it is modified
#'   by reference if `y` is supplied. Without `y`, a new comparison is returned.
#'
#' @details
#' For supported `ParameterSet` objects, `compare(x, y)` calls
#' [getComparison()] with `x` and `y`. Supported classes are `TrialDesign`,
#' `TrialDesignCharacteristics`, `TrialDesignPlan`, `SimulationResults`,
#' `PowerAndAverageSampleNumberResult`, `EventProbabilities`, `NumberOfSubjects`,
#' `ConditionalPowerResults` (including subclasses), `PerformanceScore`,
#' `FisherInformation`, and `TrialDesignOptimalConditionalErrorCharacteristics`.
#' If `x` is a `TrialComparison`, it calls `x$add(y)` instead. The input
#' validation and collection rules of [getComparison()] apply in both cases.
#' Objects are retained by reference without recalculating them.
#'
#' @seealso
#' [getComparison()], [add()]
#'
#' @examples
#' comp <- getDesignInverseNormal(kMax = 2) |>
#'     compare(getSampleSizeMeans(alternative = 0.2)) |>
#'     compare(getSampleSizeMeans(alternative = 0.59))
#' length(comp)
#'
#' @export
#'
compare <- function(x, y) {
    UseMethod("compare")
}

#'
#' @title
#' Start or Extend a Trial Comparison
#'
#' @rdname compare
#'
#' @description
#' The `ParameterSet` method starts or extends a trial comparison.
#'
#' @export
#'
compare.ParameterSet <- function(x, y) {
    if (missing(y)) {
        return(getComparison(x))
    }

    if (inherits(x, "TrialComparison")) {
        return(x$add(y))
    }

    return(getComparison(x, y))
}

#'
#' @title
#' Compare and Summarize Trial Planning Objects
#'
#' @description
#' Combines two operands with [getComparison()] and returns the summary of the
#' resulting comparison. Repeated use extends the summarized comparison.
#'
#' @param x,y Supported trial planning objects, their `SummaryFactory` summaries,
#'   `TrialComparison` objects, or `TrialComparisonSummary` objects.
#'
#' @return A visible `TrialComparisonSummary`, as returned by
#'   `summary(getComparison(x, y))` after unwrapping summary operands.
#'
#' @details
#' Summary operands contribute their underlying `object`. For `ParameterSet`
#' operands, the supported classes are those accepted by [getComparison()]:
#' designs, design characteristics, sample size or power plans, simulations, and
#' power/ASN results, event probabilities, recruitment results, conditional power,
#' performance scores, Fisher information, and optimal conditional-error
#' characteristics.
#' Existing comparisons contribute their contained objects in order, retaining
#' names and references. The operands' collection membership is not modified.
#' The validation and unique-name rules of [getComparison()] apply.
#'
#' The result can be assigned, extended with another `%vs%`, or passed directly
#' to [plot.TrialComparison()]. Chained operations do not print intermediate
#' summaries. Plot axes must still be specified when their selection is ambiguous.
#'
#' @seealso
#' [getComparison()], [compare()], [summary.TrialComparison()]
#'
#' @examples
#' comparison <- getSampleSizeMeans(alternative = 0.22, stDev = 0.5) %vs%
#'     getSampleSizeMeans(alternative = 0.44, stDev = 1) %vs%
#'     getSampleSizeMeans(alternative = 0.59, stDev = 1)
#' comparison
#'
#' comp1 <- getSampleSizeMeans(alternative = 0.22) %vs%
#'     compare(getSampleSizeMeans(alternative = 0.44))
#' comp2 <- comp1 %vs% compare(getSampleSizeMeans(alternative = 0.59))
#' comp2
#' comp2 |> getComparison()
#'
#' \dontrun{
#' getSampleSizeMeans(alternative = 0.22, stDev = 0.5) %vs%
#'     getSampleSizeMeans(alternative = 0.44, stDev = 1) %vs%
#'     getSampleSizeMeans(alternative = 0.59, stDev = 1) |>
#'     plot(
#'         xAxis = "alternative",
#'         yAxis = c("nFixed", "criticalValuesEffectScale"), category = "stDev"
#'     )
#' }
#'
#' @rdname versus
#'
#' @export
#'
`%vs%` <- function(x, y) {
    if (inherits(x, "SummaryFactory")) {
        x <- x$object
    }
    if (inherits(y, "SummaryFactory")) {
        y <- y$object
    }
    return(summary(getComparison(x, y)))
}

.collectComparisonObjects <- function(arguments, objects = NULL) {
    if (!is.null(objects)) {
        arguments <- c(arguments, list(objects))
    }
    result <- list()
    for (i in seq_along(arguments)) {
        object <- arguments[[i]]
        if (inherits(object, "TrialComparisonSummary")) {
            object <- object$object
        }
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
    return(result)
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

.getTrialComparisonData <- function(
        objects,
        niceColumnNamesEnabled = FALSE,
        includeAllParameters = FALSE,
        harmonizeColumnNames = TRUE) {
    .assertIsSingleLogical(niceColumnNamesEnabled, "niceColumnNamesEnabled")
    .assertIsSingleLogical(includeAllParameters, "includeAllParameters")
    .assertIsSingleLogical(harmonizeColumnNames, "harmonizeColumnNames")
    if (!length(objects)) {
        return(data.frame())
    }

    return(.getComparisonData(objects,
        niceColumnNamesEnabled = niceColumnNamesEnabled,
        includeAllParameters = includeAllParameters,
        harmonizeColumnNames = harmonizeColumnNames
    ))
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
#' comparison <- getComparison(
#'     fixed = getDesignFixed(),
#'     sequential = getDesignGroupSequential(kMax = 2)
#' )
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
#' @return The modified comparison for `TrialComparison`.
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
add <- function(x, ...) {
    UseMethod("add")
}

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
add.TrialComparison <- function(x, ...) {
    return(x$add(...))
}

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
#' @return The modified comparison for `TrialComparison`.
#'
#' @seealso
#' [getComparison()], [add()]
#'
#' @examples
#' comparison <- getComparison(
#'     fixed = getDesignFixed(),
#'     sequential = getDesignGroupSequential(kMax = 2)
#' )
#' comparison |> delete("fixed")
#' comparison |> delete(index = 1)
#' comparison |>
#'     add(getDesignFixed()) |>
#'     delete(1)
#'
#' @export
#'
delete <- function(x, ...) {
    UseMethod("delete")
}

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
delete.TrialComparison <- function(x, ...) {
    return(x$delete(...))
}

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
        getObjects = function() {
            return(private$.objects)
        },
        add = function(..., objects = NULL) {
            additions <- .collectComparisonObjects(list(...), objects)
            if (!length(additions)) {
                return(self)
            }

            combined <- c(private$.objects, additions)
            # Validate the complete candidate before changing the stored list.
            .assertComparisonObjectNames(combined, functionName = "TrialComparison$add")
            .getTrialComparisonData(combined)
            private$.objects <- combined
            return(self)
        },
        delete = function(index) {
            if (missing(index)) {
                stopMissingArgument("Supply the name or position of the object to delete",
                    functionName = "TrialComparison$delete", parameter = "index",
                    constraint = "one object name or position", diagnosticId = "comparison.object_index_missing"
                )
            }
            if (length(index) != 1L) {
                stopArgumentLengthOutOfBounds("Supply exactly one object name or position",
                    functionName = "TrialComparison$delete", parameter = "index", value = index,
                    constraint = "exactly one index", expectedLength = 1L, actualLength = length(index),
                    diagnosticId = "comparison.object_index_length_invalid"
                )
            }
            if (is.character(index) && !is.na(index) && nzchar(index)) {
                position <- match(index, names(private$.objects))
                if (is.na(position)) {
                    stopIllegalArgument("No object named '", index, "' exists in the comparison",
                        functionName = "TrialComparison$delete", parameter = "index", value = index,
                        constraint = "an exact name of a contained object",
                        diagnosticId = "comparison.object_name_unknown",
                        context = list(availableNames = names(private$.objects))
                    )
                }
            } else {
                if (!is.numeric(index) || is.complex(index) || is.na(index) ||
                        !is.finite(index) || index != trunc(index)) {
                    stopIllegalArgument("The object index must be a non-empty name or an integer position",
                        functionName = "TrialComparison$delete", parameter = "index", value = index,
                        constraint = "one non-missing, non-empty character name or finite integer position",
                        diagnosticId = "comparison.object_index_invalid"
                    )
                }
                if (index < 1 || index > length(private$.objects)) {
                    stopIndexOutOfBounds("Object position is outside the comparison",
                        functionName = "TrialComparison$delete", parameter = "index", value = index, index = index,
                        constraint = "a position in [1, number of objects]",
                        lowerBound = 1L, upperBound = length(private$.objects),
                        diagnosticId = "comparison.object_index_out_of_bounds"
                    )
                }
                position <- index
            }
            remaining <- private$.objects[-position]
            private$.objects <- if (length(remaining)) {
                remaining
            } else {
                list()
            }
            return(self)
        },
        show = function(...) {
            self$.show(..., consoleOutputEnabled = TRUE)
            return(invisible(self))
        },
        .show = function(..., consoleOutputEnabled = FALSE) {
            .showTrialComparison(self, ..., consoleOutputEnabled = consoleOutputEnabled)
            return(invisible(self))
        }
    ),
    private = list(
        .objects = NULL,
        deep_clone = function(name, value) {
            stopIllegalArgument("Deep cloning of TrialComparison is not supported; use clone(deep = FALSE)",
                functionName = "TrialComparison$clone", parameter = "deep", value = TRUE,
                constraint = "deep must be FALSE", diagnosticId = "comparison.deep_clone_unsupported"
            )
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
#' comparison <- getComparison(
#'     fixed = getDesignFixed(),
#'     sequential = getDesignGroupSequential(kMax = 2)
#' )
#' identical(comparison$fixed, comparison[["fixed"]])
#' identical(comparison[[1]], comparison$fixed)
#'
#' @export
#'
`[[.TrialComparison` <- function(x, i, ..., exact = TRUE) {
    if (missing(i)) {
        stopMissingArgument("An object name or position must be supplied",
            functionName = "[[.TrialComparison", parameter = "i",
            constraint = "one object name or position", diagnosticId = "comparison.object_index_missing"
        )
    }
    extra <- list(...)
    if (length(extra)) {
        stopIllegalArgument("Only one object index is supported",
            functionName = "[[.TrialComparison", parameter = "...", value = extra,
            constraint = "no additional indices", diagnosticId = "comparison.multiple_indices_unsupported"
        )
    }
    .assertIsSingleLogical(exact, "exact")
    if (length(i) != 1L) {
        stopArgumentLengthOutOfBounds("Supply exactly one object name or position",
            functionName = "[[.TrialComparison", parameter = "i", value = i,
            constraint = "exactly one index", expectedLength = 1L, actualLength = length(i),
            diagnosticId = "comparison.object_index_length_invalid"
        )
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
            diagnosticId = "comparison.object_index_invalid"
        )
    }
    if (i < 1 || i > length(objects)) {
        stopIndexOutOfBounds("Object position is outside the comparison",
            functionName = "[[.TrialComparison", parameter = "i", value = i, index = i,
            constraint = "a position in [1, number of objects]", lowerBound = 1L, upperBound = length(objects),
            diagnosticId = "comparison.object_index_out_of_bounds"
        )
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
#' comparison <- getComparison(
#'     fixed = getDesignFixed(),
#'     sequential = getDesignGroupSequential(kMax = 2)
#' )
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
    result <- lapply(positions, function(position) {
        if (is.na(position)) {
            NULL
        } else {
            objects[[position]]
        }
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
            diagnosticId = "comparison.fetch_selector_invalid", context = list(selectorNumber = selectorNumber)
        )
    }
    size <- length(objects)
    invalid <- which(selector == 0 | selector < -size | selector > size)
    if (length(invalid)) {
        stopIndexOutOfBounds("Object position is outside the comparison",
            functionName = "fetch.TrialComparison", parameter = "...", value = selector,
            index = selector[invalid[1L]], lowerBound = -size, upperBound = size,
            constraint = "nonzero integer positions in [-number of objects, number of objects]",
            diagnosticId = "comparison.fetch_index_out_of_bounds",
            context = list(selectorNumber = selectorNumber)
        )
    }
    ifelse(selector < 0, size + selector + 1L, selector)
}

#'
#' @title
#' Trial Comparison Data Frame
#'
#' @description
#' Converts a trial comparison to a data frame, optionally prepared for reporting.
#'
#' @param x A `TrialComparison`.
#' @param row.names Optional row names for the result.
#' @param optional Unused; included for compatibility with `as.data.frame()`.
#' @param ... Further arguments. Currently unused.
#' @inheritParams param_niceColumnNamesEnabled
#' @param includeAllParameters If `TRUE`, include all parameters supported by
#'   the existing data-frame methods of the individual objects.
#' @param harmonizeColumnNames If `TRUE` (default), apply the class-specific
#'   mappings described below. If `FALSE`, retain original parameter names and
#'   omit extra fields that are included only to support these mappings.
#'   Readable labels remain controlled by `niceColumnNamesEnabled`.
#' @param removeConstantParameters If `TRUE`, retain identifiers and only parameters
#'   with different object profiles, using the same rules as `summary()`.
#' @param transposed If `TRUE`, return parameters as rows and comparison records
#'   as columns, labelled by object and available stage/situation coordinates.
#'   Shared scalar profiles use `Value`; duplicate labels receive unique suffixes.
#'   If exactly one supplied or default input vector matches all situations across
#'   stages, its caption replaces `Situation` (for example, `Alternative 1`).
#'   Ambiguous axes retain `Situation`; shared profiles require agreement across objects.
#'   Mixed column types are converted to a common type by transposition.
#' @param removeVariableParameters If `TRUE`, return shared parameter profiles once,
#'   with stage and situation coordinates where needed, without object identifiers.
#'   Cannot be combined with `removeConstantParameters = TRUE`.
#' @param digits Integer number of decimal places for numeric parameters, or `NA`
#'   to retain full precision. Parameter profiles are compared before rounding.
#' @param ceilingEnabled If `TRUE`, round sample-size and event-count parameters
#'   upwards to integers, using rpact parameter metadata. Variances are excluded.
#' @param output Output mode: `"data.frame"` (default), `"auto"`, `"html"`, or `"pdf"`.
#'   Reporting modes add class `TrialComparisonTable` and rendering attributes;
#'   they still return a data frame. `"auto"` selects HTML or LaTeX during knitting.
#'   `"pdf"` produces LaTeX tables for a PDF document, not a PDF file.
#'
#' @return A data.frame, unrounded and with all columns by default. Reporting modes
#'   additionally inherit from `TrialComparisonTable` for automatic knitr output.
#'
#' @details
#' With `harmonizeColumnNames = TRUE`, fixed-plan sample size columns are harmonized when the comparison includes
#' interim analyses: `nFixed`, `nFixed1`, and `nFixed2` become `numberOfSubjects`,
#' `numberOfSubjects1`, and `numberOfSubjects2`. Fixed survival events use
#' `cumulativeEventsPerStage` in mixed comparisons with interim analyses.
#' Available sample-size H1 expectations use `expectedNumberOfSubjects` and
#' `expectedNumberOfEvents`, referring to each scenario's supplied alternative.
#' See [getComparison()] for the class-specific rules and reference values.
#'
#' Shared profiles use the same exact comparison as [summary.TrialComparison()],
#' applied to the selected harmonized or original columns. The summary's
#' `harmonizeColumnNames` argument can be set independently of this data-frame option.
#' Filtering precedes rounding, readable labels and transposition. Common scalar
#' parameters are returned once; common stage or situation profiles retain their
#' coordinates. Reporting modes render HTML in a horizontally scrollable container
#' or LaTeX longtables split into column blocks with repeated identifier columns.
#' LaTeX output requires the suggested rmarkdown package and registers the longtable
#' dependency through knitr metadata. Other knitting targets use ordinary data-frame
#' output. Reporting does not modify the comparison or its contained objects.
#'
#' Multi-arm survival simulations use `categoryType` and `category` coordinates.
#' `comparison` rows (for example, `Treatment 1 vs. Control`) contain stage-wise
#' and cumulative event counts for treatment plus control. `arm` rows contain
#' individual-arm events, selections, rejections and drop-out summaries, including
#' a separate `Control` category. Arm-independent simulation results occur on
#' `overall` / `All arms` rows. Input and design settings remain available for
#' grouping. Result cells outside their applicable category are `NA`.
#' Categories and situations are retained in profile comparisons and transposed
#' headings. Shared scalar-only output can omit these coordinates.
#'
#' @export
#'
as.data.frame.TrialComparison <- function(
        x,
        row.names = NULL,
        optional = FALSE,
        ...,
        niceColumnNamesEnabled = FALSE,
        includeAllParameters = FALSE,
        harmonizeColumnNames = TRUE,
        removeConstantParameters = FALSE,
        transposed = FALSE,
        removeVariableParameters = FALSE,
        digits = NA_integer_,
        ceilingEnabled = FALSE,
        output = "data.frame") {
    .warnInCaseOfUnknownArguments(functionName = "as.data.frame.TrialComparison", ...)
    .assertIsSingleLogical(removeConstantParameters, "removeConstantParameters")
    .assertIsSingleLogical(niceColumnNamesEnabled, "niceColumnNamesEnabled")
    .assertIsSingleLogical(harmonizeColumnNames, "harmonizeColumnNames")
    .assertIsSingleLogical(transposed, "transposed")
    .assertIsSingleLogical(removeVariableParameters, "removeVariableParameters")
    .assertIsSingleLogical(ceilingEnabled, "ceilingEnabled")
    if (length(digits) != 1L || !(is.numeric(digits) || identical(digits, NA)) ||
            is.complex(digits) || !is.null(dim(digits)) ||
            (!is.na(digits) && (!is.finite(digits) || digits != trunc(digits)))) {
        stopIllegalArgument("'digits' must be a finite integer or NA",
            functionName = "as.data.frame.TrialComparison", parameter = "digits", value = digits,
            constraint = "one finite integer or NA", diagnosticId = "comparison.table_digits_invalid"
        )
    }
    .assertIsSingleCharacter(output, "output")
    if (!output %in% c("data.frame", "auto", "html", "pdf")) {
        stopIllegalArgument("Unknown comparison table output mode",
            functionName = "as.data.frame.TrialComparison", parameter = "output", value = output,
            constraint = "one of data.frame, auto, html, pdf",
            diagnosticId = "comparison.table_output_invalid"
        )
    }
    if (removeConstantParameters && removeVariableParameters) {
        stopConflictingArguments("Cannot remove both constant and variable parameters",
            functionName = "as.data.frame.TrialComparison",
            parameter = "removeVariableParameters", value = removeVariableParameters,
            relatedParameter = "removeConstantParameters", relatedValue = removeConstantParameters,
            constraint = "at most one parameter filter may be TRUE",
            diagnosticId = "comparison.table_filters_conflict"
        )
    }
    objects <- x$getObjects()
    data <- .getTrialComparisonData(objects,
        includeAllParameters = includeAllParameters,
        harmonizeColumnNames = harmonizeColumnNames
    )
    objectNumbers <- data$objectNumber
    columnNameData <- data
    situationCaptions <- if (transposed) {
        vapply(seq_along(objects), function(i) {
            parameter <- .getComparisonSituationParameter(objects[[i]], data[data$objectNumber == i, , drop = FALSE])
            if (is.null(parameter)) {
                return("Situation")
            }
            return(.comparisonCaption(parameter, objects[[i]], tableOutputEnabled = TRUE))
        }, character(1))
    } else {
        character()
    }
    allNames <- names(data)
    if (removeConstantParameters) {
        info <- .getTrialComparisonInfo(x, data)
        selected <- getComparisonParameterNames(data, varyingOnly = FALSE)
        selected <- selected[selected %in% c(.comparisonIdentifiers, info$paramName[info$nProfiles > 1L])]
        data <- data[selected]
    }
    if (removeVariableParameters && nrow(data) > 0L) {
        info <- .getTrialComparisonInfo(x, data)
        common <- info$paramName[info$nProfiles == 1L]
        coordinates <- unique(c(.getComparisonCoordinateNames(data), intersect("situation", names(data))))
        varyingProfiles <- vapply(data[common], function(values) {
            return(length(unique(values)) > 1L)
        }, logical(1))
        if (!any(varyingProfiles)) {
            coordinates <- character()
        }
        selected <- names(data)[names(data) %in% c(coordinates, common)]
        data <- unique(data[data$objectNumber == objectNumbers[1L], selected, drop = FALSE])
        if (!length(common)) {
            data <- data.frame()
        }
        objectNumbers <- rep(1L, nrow(data))
        rownames(data) <- NULL
        columnNameData <- data
    }
    countParameters <- c(
        .getOutputFormatParameterNames("rpact.output.format.sample.size"),
        .getOutputFormatParameterNames("rpact.output.format.event")
    )
    countParameters <- countParameters[!grepl("variance", countParameters, ignore.case = TRUE)]
    for (parameter in setdiff(names(data), .comparisonIdentifiers)) {
        if (is.numeric(data[[parameter]]) && !is.object(data[[parameter]])) {
            originalValues <- data[[parameter]]
            if (!is.na(digits)) {
                data[[parameter]] <- round(originalValues, digits = digits)
            }
            if (ceilingEnabled && parameter %in% countParameters) {
                for (number in unique(objectNumbers)) {
                    formatFunction <- .getParameterFormatFunction(parameter, objects[[number]])
                    if (formatFunction %in% c(".formatSampleSizes", ".formatEvents")) {
                        rows <- objectNumbers == number
                        data[[parameter]][rows] <- ceiling(originalValues[rows])
                    }
                }
            }
        }
    }
    identifiers <- intersect(.comparisonIdentifiers, names(data))
    if (niceColumnNamesEnabled) {
        labels <- names(.getTrialComparisonData(x$getObjects(),
            niceColumnNamesEnabled = TRUE,
            includeAllParameters = includeAllParameters,
            harmonizeColumnNames = harmonizeColumnNames
        ))
        names(data) <- labels[match(names(data), allNames)]
        identifiers <- labels[match(identifiers, allNames)]
    }
    if (transposed) {
        rowCaptions <- if (removeVariableParameters) {
            if (length(unique(situationCaptions)) == 1L) {
                rep(situationCaptions[1L], nrow(columnNameData))
            } else {
                rep("Situation", nrow(columnNameData))
            }
        } else {
            situationCaptions[columnNameData$objectNumber]
        }
        situationNames <- .getComparisonTransposedColumnNames(columnNameData, rowCaptions)
        values <- as.data.frame(t(as.matrix(data)), stringsAsFactors = FALSE)
        names(values) <- situationNames
        data <- cbind(data.frame(Parameter = names(data), stringsAsFactors = FALSE), values)
        rownames(data) <- NULL
        identifiers <- "Parameter"
    }
    if (!is.null(row.names)) {
        rownames(data) <- row.names
    }
    if (output != "data.frame") {
        class(data) <- c("TrialComparisonTable", "data.frame")
        attr(data, "comparisonTableOutput") <- output
        attr(data, "comparisonTableIdentifiers") <- identifiers
        attr(data, "comparisonTableCaption") <- if (removeVariableParameters) {
            "Common parameters"
        } else {
            "Comparison"
        }
    }
    return(data)
}

.comparisonIdentifiers <- c("objectNumber", "objectName", "stages", "stage", "situation", "categoryType", "category")
.comparisonScope <- c("objectType", "endpoint", "calculationTarget")

.getComparisonSituationParameter <- function(object, data) {
    if (!inherits(object, "ParameterSet") || !"situation" %in% names(data)) {
        return(NULL)
    }
    situations <- data$situation
    if (!length(situations) || anyNA(situations)) {
        return(NULL)
    }
    nSituations <- max(situations)
    if (nSituations <= 1L || !setequal(situations, seq_len(nSituations))) {
        return(NULL)
    }
    # Only supplied/default inputs can explain a scenario axis. Derived results
    # and vectors indexed by stage, arm or population must not name that axis.
    candidates <- intersect(c(object$.getUserDefinedParameters(), object$.getDefaultParameters()), names(data))
    candidates <- setdiff(candidates, c(.comparisonIdentifiers, .getComparisonCoordinateNames(data)))
    candidates <- candidates[vapply(candidates, function(parameter) {
        values <- object[[parameter]]
        if (!is.atomic(values) || !is.null(dim(values)) || length(values) != nSituations ||
                anyNA(values) || length(unique(values)) <= 1L) {
            return(FALSE)
        }
        # Check the unrounded values at every stage, not merely vector lengths.
        return(isTRUE(all.equal(unname(data[[parameter]]), unname(values[situations]),
            check.attributes = FALSE, tolerance = 0)))
    }, logical(1))]
    if (length(candidates) != 1L) {
        return(NULL)
    }
    return(candidates[[1L]])
}

.getComparisonTransposedColumnNames <- function(data, situationCaptions = rep("Situation", nrow(data))) {
    coordinates <- unique(c(.getComparisonCoordinateNames(data), intersect("situation", names(data))))
    # The generated situation index is redundant for a single situation with
    # semantic coordinates, but distinguishes multiple scenarios at each stage.
    if (length(setdiff(coordinates, "situation")) &&
            "situation" %in% coordinates && length(unique(data$situation)) <= 1L) {
        coordinates <- setdiff(coordinates, "situation")
    }
    captions <- c(stages = "Stage", stage = "Stage", time = "Time",
        treatmentArm = "Treatment arm", populations = "Population",
        situations = "Situation", situation = "Situation", categoryType = "Category type", category = "Category")
    labels <- vapply(seq_len(nrow(data)), function(i) {
        parts <- character()
        if ("objectNumber" %in% names(data)) {
            label <- data$objectName[i]
            if (is.null(label) || is.na(label) || !nzchar(trimws(label))) {
                label <- paste("Object", data$objectNumber[i])
            }
            parts <- label
        }
        for (coordinate in coordinates) {
            value <- data[[coordinate]][i]
            if (!is.na(value) && nzchar(trimws(as.character(value)))) {
                caption <- if (coordinate == "situation") {
                    situationCaptions[i]
                } else {
                    captions[[coordinate]]
                }
                parts <- c(parts, paste(caption, value))
            }
        }
        if (!length(parts)) {
            return("Value")
        }
        return(paste(unique(parts), collapse = " / "))
    }, character(1))
    return(make.unique(labels))
}

.assertComparisonLimit <- function(value, parameter, functionName) {
    if (!is.numeric(value) || is.complex(value) || length(value) != 1L || is.na(value) ||
            !is.finite(value) || value < 1 || value != trunc(value)) {
        stopIllegalArgument("'", parameter, "' must be a positive finite integer",
            functionName = functionName, parameter = parameter, value = value,
            constraint = "one finite integer >= 1", diagnosticId = "comparison.display_limit_invalid"
        )
    }
}

.comparisonText <- function(text, width = 100L) {
    text <- gsub("[[:cntrl:]]", " ", as.character(text))
    ifelse(nchar(text) > width, paste0(substr(text, 1L, width - 3L), "..."), text)
}

.comparisonPreview <- function(values, maxValues = 4L, digits = NA_integer_, reference = NULL, truncate = TRUE) {
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
    text <- paste(.comparisonText(text, if (!truncate) {
        Inf
    } else if (length(shown) == 1L) {
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
    if (grepl("^observed", parameter) || parameter %in% c(
            "time", "treatmentArm", "populations", "situations", "type", "nPlanned"
        )) {
        return("Inputs and design settings")
    }
    sourceParameter <- sub("^(optimal_|design_|simulation_)", "", parameter)
    types <- unlist(lapply(objects, function(object) {
        return(unlist(lapply(.getComparisonMetadataSources(object), function(source) {
            return(source$.parameterTypes[[sourceParameter]])
        }), use.names = FALSE))
    }), use.names = FALSE)
    if (any(types %in% c(C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE))) {
        return("Inputs and design settings")
    }
    "Results and derived parameters"
}

.getTrialComparisonTitle <- function(objects, summaryEnabled = FALSE) {
    categories <- vapply(objects, function(object) {
        context <- .getComparisonAdapterContext(object)
        if (!is.null(context)) {
            return(context$target)
        }
        if (inherits(object, "SimulationResults")) {
            return("Power simulation")
        }

        if (inherits(object, c("TrialDesign", "TrialDesignCharacteristics"))) {
            return("Design")
        }

        if (inherits(object, "PowerAndAverageSampleNumberResult")) {
            return("Power and average sample size (ASN)")
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
    context <- .getComparisonAdapterContext(object)
    if (!is.null(context)) {
        return(if (startWithUpperCase) {
            .firstCharacterToUpperCase(context$target)
        } else {
            tolower(context$target)
        })
    }
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
        return(paste0(
            "Comparison of ", length(objects), " ", types[1L],
            if (length(objects) == 1L) {
                " calculation."
            } else {
                " calculations."
            }
        ))
    }
    distinct <- unique(types)
    description <- ""
    for (type in distinct) {
        description <- .concatenateSummaryText(description, type)
    }
    return(paste0(
        "Comparison of ", length(objects), " trial planning ",
        if (length(objects) == 1L) {
            "result: "
        } else {
            "results: "
        }, description, "."
    ))
}

.comparisonCaption <- function(parameter, object = NULL, tableOutputEnabled = FALSE) {
    if (!is.null(object) && !parameter %in% c(.comparisonScope, .comparisonIdentifiers)) {
        sources <- .getComparisonMetadataSources(object)
        matching <- vapply(sources, function(source) {
            return(parameter %in% names(source))
        }, logical(1))
        if (!any(matching)) {
            baseParameter <- sub("\\.[^.]+$", "", parameter)
            if (grepl("^(observedDropoutProportion|dropoutObservationTime)[12]$", baseParameter)) {
                for (source in sources) {
                    values <- source[[baseParameter]]
                    if (!is.matrix(values)) {
                        next
                    }
                    for (g in seq_len(nrow(values))) {
                        group <- if (inherits(source, "SimulationResultsEnrichmentSurvival")) {
                            .getCategoryCaptionEnrichment(source, baseParameter, g)
                        } else {
                            paste0("arm", g)
                        }
                        if (identical(parameter, paste0(baseParameter, ".", group))) {
                            caption <- .getParameterCaption(baseParameter, source,
                                tableOutputEnabled = tableOutputEnabled)
                            if (is.null(caption) || !nzchar(caption)) {
                                caption <- .getParameterCaption(baseParameter, source)
                            }
                            return(.getParameterSetParameterCaptionPerRow(source, baseParameter, caption, g))
                        }
                    }
                }
            }
            return(NA_character_)
        }
        object <- sources[[which(matching)[1L]]]
    }
    if (inherits(object, "SimulationResultsMultiArmSurvival") &&
            grepl("^(observedDropoutProportion|dropoutObservationTime)1$", parameter)) {
        return(paste0(sub(" \\(1\\)$", "", .getParameterCaption(parameter, object)), " (treatment arm)"))
    }
    if (inherits(object, "SimulationResultsMultiArmSurvival") &&
            parameter %in% c("singleEventsPerStage", "cumulativeEventsPerStage", "singleEventsPerArmAndStage")) {
        scope <- if (parameter == "singleEventsPerArmAndStage") {
            "individual arm"
        } else {
            "treatment plus control"
        }
        return(paste0(.getParameterCaption(parameter, object), " (", scope, ")"))
    }
    caption <- .getParameterCaption(parameter, obj = object, tableOutputEnabled = tableOutputEnabled)
    if (is.null(caption) || length(caption) != 1L || is.na(caption) || !nzchar(caption)) {
        caption <- C_TABLE_COLUMN_NAMES[[parameter]]
    }
    if (is.null(caption) || length(caption) != 1L || is.na(caption) || !nzchar(caption)) {
        caption <- switch(parameter,
            objectType = "Object type",
            endpoint = "Endpoint",
            calculationTarget = "Calculation target",
            situation = "Situation",
            stage = "Stage",
            parameter
        )
    }
    return(caption)
}

.comparisonCaptionLabel <- function(parameter, captions, tableOutputEnabled = FALSE) {
    captions <- unique(captions[!is.na(captions) & nzchar(captions)])
    if (!length(captions)) {
        return(.comparisonCaption(parameter, tableOutputEnabled = tableOutputEnabled))
    }
    return(paste(captions, collapse = " / "))
}

.comparisonConstantCell <- function(value) {
    if (is.na(value)) {
        return(value)
    }
    tokens <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
    numericPattern <- "^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?$"
    if (!length(tokens) || grepl(",[[:space:]]*$", value) || !all(grepl(numericPattern, tokens))) {
        return(value)
    }
    numbers <- as.numeric(tokens)
    if (!all(is.finite(numbers)) || length(unique(numbers)) != 1L) {
        return(value)
    }
    # Use a compact scalar representation that round-trips without precision loss.
    number <- numbers[[1L]] + 0
    for (precision in 1:17) {
        text <- sprintf("%.*g", precision, number)
        if (identical(as.numeric(text), number)) {
            return(text)
        }
    }
    return(value)
}

.comparisonProfile <- function(data, parameter, object = NULL) {
    if (inherits(object, "SimulationResultsMultiArmSurvival") && "categoryType" %in% names(data)) {
        inputs <- c(object$.getUserDefinedParameters(), object$.getDefaultParameters(),
            names(.getComparisonDesign(object)), .comparisonIdentifiers, .comparisonScope)
        if (parameter %in% names(object) && !parameter %in% inputs) {
            categoryType <- if (parameter %in% c("singleEventsPerStage", "cumulativeEventsPerStage")) {
                "comparison"
            } else if (parameter %in% c("singleEventsPerArmAndStage", "selectedArms", "rejectedArmsPerStage",
                    "lambdaTreatment", "medianTreatment", "effectMatrix") ||
                    grepl("^(observedDropoutProportion|dropoutObservationTime)[12]$", parameter)) {
                "arm"
            } else {
                "overall"
            }
            data <- data[data$categoryType == categoryType, , drop = FALSE]
            if (grepl("^(observedDropoutProportion|dropoutObservationTime)[12]$", parameter)) {
                control <- endsWith(parameter, "2")
                data <- data[(data$category == "Control") == control, , drop = FALSE]
            }
        }
    }
    values <- data[[parameter]]
    if (is.character(values)) {
        values <- vapply(values, .comparisonConstantCell, character(1), USE.NAMES = FALSE)
        data[[parameter]] <- values
    }
    if (length(unique(values)) <= 1L) {
        return(data.frame(value = values[1L]))
    }
    coordinates <- .getComparisonCoordinateNames(data)
    semanticCoordinates <- setdiff(coordinates, c("stages", "stage"))
    keys <- if (length(semanticCoordinates)) {
        coordinates
    } else {
        intersect(c("stages", "stage", "situation"), names(data))
    }
    profile <- data[keys]
    profile$value <- values
    # Collapse a repeated dimension, retaining the positions of missing values.
    collapseKeys <- if (length(semanticCoordinates)) {
        character()
    } else {
        keys
    }
    for (key in collapseKeys) {
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
    return(profile)
}

.getTrialComparisonInfo <- function(object, data = .getTrialComparisonData(object$getObjects())) {
    info <- .getComparisonParameterInfo(data)
    # Use the common metadata API for the full ordered candidate set; row-wise
    # variation alone cannot identify equal stage profiles of different objects.
    candidates <- getComparisonParameterNames(data, varyingOnly = FALSE, exclude = .comparisonIdentifiers)
    info <- info[match(candidates, info$paramName), , drop = FALSE]
    objects <- object$getObjects()
    frames <- split(data, factor(data$objectNumber, levels = seq_along(objects)))
    profiles <- lapply(info$paramName, function(parameter) {
        return(lapply(seq_along(frames), function(i) {
            return(.comparisonProfile(frames[[i]], parameter, objects[[i]]))
        }))
    })
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
print.TrialComparison <- function(
        x,
        ...,
        markdown = NA,
        digits = NA_integer_,
        maxTypes = 5L,
        maxMetrics = 6L) {
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
            digits = digits, maxTypes = maxTypes, maxMetrics = maxMetrics
        )), collapse = "\n")))
    }
    x$.show(
        digits = digits, 
        maxTypes = maxTypes, 
        maxMetrics = maxMetrics, 
        consoleOutputEnabled = TRUE
    )
    invisible(x)
}

.showTrialComparison <- function(
        object,
        ...,
        digits = NA_integer_,
        maxTypes = 5L,
        maxMetrics = 6L,
        consoleOutputEnabled = TRUE) {
    .warnInCaseOfUnknownArguments(functionName = "TrialComparison$show", ...)
    .assertComparisonLimit(maxTypes, "maxTypes", "print.TrialComparison")
    .assertComparisonLimit(maxMetrics, "maxMetrics", "print.TrialComparison")
    data <- .getTrialComparisonData(object$getObjects())
    info <- .getComparisonParameterInfo(data)
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
        heading = 1, consoleOutputEnabled = consoleOutputEnabled
    )
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

    metrics <- intersect(c(
        "kMax", "alpha", "beta", "nMax", "averageSampleNumber", "maxNumberOfSubjects", "maxNumberOfEvents",
        "nFixed", "overallReject", "expectedNumberOfSubjects", "expectedNumberOfEvents", "studyDuration"
    ), info$paramName[!info$isEmpty])
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
            heading = 2, consoleOutputEnabled = consoleOutputEnabled
        )
    }
    for (parameter in shownMetrics) {
        row <- info[info$paramName == parameter, , drop = FALSE]
        values <- row$values[[1L]]
        if (is.numeric(values) && length(values) > 1L) {
            values <- range(values)
        }
        emit(
            indent, sprintf("%-*s : %s", metricWidth, metricCaptions[[parameter]], 
                .comparisonPreview(values, digits = digits)),
            if (row$hasMissingValues) {
                " (missing values present)"
            } else {
                ""
            }, "\n"
        )
    }
    if (length(metrics) > maxMetrics) {
        emit(indent, "... ", length(metrics) - maxMetrics, " more observed metrics.\n")
    }

    emit("\n")
    object$.cat("Available operations:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
    operations <- c(
        "summary(x)", "as.data.frame(x)", 
        "plot(x, xAxis = ..., yAxis = ...)", 
        "getObjects(x)")
    descriptions <- c(
        "Compare differences", 
        "Access all data", "Plot selected parameters", 
        "Access contained objects")
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
#' @param textMode Console text handling: `"wrap"` (default) wraps complete text,
#'   `"truncate"` shows compact previews with an explicit shortening notice,
#'   and `"full"` writes complete text without automatic wrapping. Markdown
#'   always retains complete text in its profile tables.
#' @param width Console line width, at least 20 characters. Defaults to
#'   `getOption("width", 80L)`. Long unbroken words can exceed this width.
#' @param showAll If `TRUE`, lift all four `max*` limits and disable text truncation.
#'   Wrapping remains active when `textMode = "wrap"`. Defaults to `FALSE`.
#' @param harmonizeColumnNames If `TRUE` (default), compare the harmonized
#'   columns described in [getComparison()]. If `FALSE`, compare the original
#'   parameter names, as in `as.data.frame(object, harmonizeColumnNames = FALSE)`.
#' @param output One of "all", "title", "overview", or "body".
#'
#' @return A `TrialComparisonSummary` inheriting from `SummaryFactory`. Its
#'   `parameterInfo` contains metadata, full exact profiles and object group IDs.
#'   The displayed summary is a snapshot; call `summary()` again after changing
#'   an underlying object. The `harmonizeColumnNames` attribute of `parameterInfo`
#'   records the selected naming mode for plotting and R-code reconstruction.
#'   Input roles take precedence when roles differ across the compared object types.
#'
#' @details
#' Constant numeric vector cells compare equal to their scalar value regardless
#' of vector length. Nonconstant vectors, missing values, and their positions
#' remain part of the exact comparison. The stored comparison data are unchanged.
#' This value normalization also applies when column-name harmonization is disabled.
#' Plotting a summary retains its selected column-name mode unless custom data
#' are supplied.
#'
#' Parameter captions are resolved separately for each original object, including
#' its design when a parameter comes from that design. `parameterInfo$paramName`
#' retains the column names selected by `harmonizeColumnNames`; `paramCaption`
#' contains the display label
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
#' Markdown output presents each differing parameter as a table with one row per
#' displayed profile and columns labelled by stage and situation. Scalar profiles
#' use a Value column. Blank cells indicate coordinates absent from a profile;
#' explicit missing values are shown as NA. The display limits also apply to
#' these tables, with omitted values and profiles indicated explicitly.
#'
#' A `NULL` display argument reads its setting through `.getEnvironmentVariable()`.
#' Explicit arguments take precedence over environment variables, which take
#' precedence over R options and defaults. The four default limits are 12
#' parameters, 3 profiles, 6 objects and 4 values. R options use the prefix
#' `rpact.comparison.summary.` followed by the argument name: `maxParameters`,
#' `maxProfiles`, `maxObjects`, `maxValues`, `textMode`, `width` or `showAll`.
#' The corresponding environment variables are `RPACT_COMPARISON_SUMMARY_MAX_PARAMETERS`,
#' `RPACT_COMPARISON_SUMMARY_MAX_PROFILES`, `RPACT_COMPARISON_SUMMARY_MAX_OBJECTS`,
#' `RPACT_COMPARISON_SUMMARY_MAX_VALUES`, `RPACT_COMPARISON_SUMMARY_TEXT_MODE`,
#' `RPACT_COMPARISON_SUMMARY_WIDTH` and `RPACT_COMPARISON_SUMMARY_SHOW_ALL`.
#' Settings are captured when the summary is created. For a complete display use
#' `summary(x, showAll = TRUE)`; for a compact display use `textMode = "truncate"`.
#'
#' @export
#'
summary.TrialComparison <- function(
        object,
        ...,
        digits = NA_integer_,
        maxParameters = NULL,
        maxProfiles = NULL,
        maxObjects = NULL,
        maxValues = NULL,
        output = c("all", "title", "overview", "body"),
        harmonizeColumnNames = TRUE,
        textMode = NULL,
        width = NULL,
        showAll = NULL) {
    .warnInCaseOfUnknownArguments(functionName = "summary.TrialComparison", ...)
    .assertIsSingleLogical(harmonizeColumnNames, "harmonizeColumnNames")
    setting <- function(value, parameter, default, type) {
        if (!is.null(value)) {
            return(value)
        }
        suffix <- gsub("([a-z])([A-Z])", "\\1_\\2", parameter)
        return(.getEnvironmentVariable(paste0("RPACT_COMPARISON_SUMMARY_", toupper(suffix)),
            paste0("rpact.comparison.summary.", parameter), default = default, type = type))
    }
    textMode <- setting(textMode, "textMode", "wrap", "character")
    .assertIsSingleCharacter(textMode, "textMode")
    if (!textMode %in% c("wrap", "truncate", "full")) {
        stopIllegalArgument("'textMode' must be wrap, truncate or full",
            functionName = "summary.TrialComparison", parameter = "textMode", value = textMode,
            constraint = "one of wrap, truncate, full", diagnosticId = "comparison.summary_text_mode_invalid")
    }
    width <- setting(width, "width", getOption("width", 80L), "numeric")
    .assertComparisonLimit(width, "width", "summary.TrialComparison")
    if (width < 20L) {
        stopArgumentOutOfRange("'width' must be at least 20 characters",
            functionName = "summary.TrialComparison", parameter = "width", value = width,
            lowerBound = 20L, constraint = "width >= 20", diagnosticId = "comparison.summary_width_invalid")
    }
    showAll <- setting(showAll, "showAll", FALSE, "logical")
    .assertIsSingleLogical(showAll, "showAll")
    limits <- list(
        maxParameters = maxParameters, 
        maxProfiles = maxProfiles, 
        maxObjects = maxObjects, 
        maxValues = maxValues)
    defaults <- c(maxParameters = 12L, maxProfiles = 3L, maxObjects = 6L, maxValues = 4L)
    for (parameter in names(limits)) {
        limits[[parameter]] <- setting(limits[[parameter]], parameter, defaults[[parameter]], "numeric")
        .assertComparisonLimit(limits[[parameter]], parameter, "summary.TrialComparison")
    }
    if (showAll) {
        limits[] <- .Machine$integer.max
        if (textMode == "truncate") {
            textMode <- "full"
        }
    }
    limits$textMode <- textMode
    limits$width <- width
    limits$showAll <- showAll
    data <- .getTrialComparisonData(object$getObjects(), 
        harmonizeColumnNames = harmonizeColumnNames)
    info <- .getTrialComparisonInfo(object, data)
    attr(info, "harmonizeColumnNames") <- harmonizeColumnNames
    getComparisonParameterDescriptions(info[FALSE, ], digits = digits)
    return(TrialComparisonSummary$new(object, info, limits, digits, match.arg(output)))
}

.getComparisonSummaryHarmonization <- function(object) {
    return(!isFALSE(attr(object$parameterInfo, "harmonizeColumnNames", exact = TRUE)))
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

.comparisonProfileMatrix <- function(
        profiles,
        labels,
        maxValues,
        digits,
        reference,
        wrapColumnLabels = FALSE) {
    coordinates <- lapply(profiles, function(profile) {
        keys <- setdiff(names(profile), "value")
        if (length(keys) == 0) {
            return(rep(list("Value"), min(nrow(profile), maxValues)))
        }
        return(lapply(seq_len(min(nrow(profile), maxValues)), function(i) {
            return(vapply(keys, function(key) {
                return(paste(
                    .comparisonCaption(key, tableOutputEnabled = TRUE),
                    .comparisonPreview(profile[[key]][i], truncate = FALSE)
                ))
            }, character(1)))
        }))
    })
    positions <- lapply(coordinates, function(profile) {
        return(vapply(profile, function(parts) {
            return(paste(parts, collapse = "; "))
        }, character(1)))
    })
    columns <- unique(unlist(positions, use.names = FALSE))
    omitted <- vapply(profiles, function(profile) {
        return(max(0L, nrow(profile) - maxValues))
    }, numeric(1))
    if (any(omitted > 0L)) {
        columns <- c(columns, "...")
    }
    values <- matrix("",
        nrow = length(profiles), ncol = length(columns),
        dimnames = list(labels, columns)
    )
    for (i in seq_along(profiles)) {
        profile <- profiles[[i]]
        values[i, match(positions[[i]], columns)] <- vapply(head(profile$value, maxValues), function(value) {
            return(.comparisonPreview(value, digits = digits, reference = reference, truncate = FALSE))
        }, character(1))
        if (omitted[i] > 0L) {
            values[i, ncol(values)] <- paste0("... (+", omitted[i], " values; increase maxValues)")
        }
    }
    if (wrapColumnLabels) {
        displayLabels <- unlist(lapply(coordinates, function(profile) {
            return(vapply(profile, function(parts) {
                return(paste(gsub(" ", "\u00a0", parts, fixed = TRUE), collapse = " "))
            }, character(1)))
        }), use.names = FALSE)
        # Match by the original coordinates before replacing display labels.
        # Spaces within each dimension are protected; dimensions can wrap freely.
        indices <- match(columns, unlist(positions, use.names = FALSE))
        matched <- !is.na(indices)
        columns[matched] <- displayLabels[indices[matched]]
        colnames(values) <- columns
    }
    return(values)
}

.showTrialComparisonSummary <- function(summary, consoleOutputEnabled) {
    summary$.resetCat()
    limits <- summary$.limits
    shortened <- FALSE
    text <- function(value, width) {
        if (!consoleOutputEnabled || limits$textMode != "truncate") {
            return(value)
        }
        shortened <<- shortened || any(nchar(value) > width)
        return(.comparisonText(value, width))
    }
    preview <- function(values, maxValues = 4L, digits = NA_integer_, reference = NULL) {
        return(.comparisonPreview(values, maxValues, digits, reference, truncate = FALSE))
    }
    emit <- function(...) {
        value <- paste0(..., collapse = "")
        if (consoleOutputEnabled && limits$textMode == "wrap") {
            lines <- strsplit(value, "\n", fixed = TRUE)[[1L]]
            lines <- vapply(lines, function(line) {
                indent <- nchar(line) - nchar(sub("^ *", "", line))
                return(paste(strwrap(line, width = limits$width, indent = indent,
                    exdent = indent + 4L), collapse = "\n"))
            }, character(1))
            value <- paste(lines, collapse = "\n")
            value <- paste0(value, "\n")
        }
        summary$.cat(value, consoleOutputEnabled = consoleOutputEnabled)
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
        emit(summary$.overview, "\n")
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
            }),
            sep = " and "
        )
        if (length(empty)) {
            overview <- paste0(overview, ". ", length(empty), if (length(empty) == 1L) {
                " parameter is empty"
            } else {
                " parameters are empty"
            })
        }
        emit(overview, ".\n")
        if (length(common)) {
            emit(
                "Shared (examples): ", text(
                    paste(head(info$paramCaption[common], if (limits$showAll) {
                        length(common)
                    } else {
                        6L
                    }), collapse = ", "), 130L),
                if (length(common) > 6L && !limits$showAll) {
                    ", ... (all shared captions: showAll = TRUE)"
                } else {
                    "."
                }, "\n"
            )
        }
    }
    if (!summary$output %in% c("all", "body")) {
        if (shortened) {
            emit("Text shortened; use textMode = \"wrap\" or \"full\", or increase width.\n")
        }
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
            if (consoleOutputEnabled) {
                emit(parameterIndent, text(description, 75L), " [", row$nProfiles, " profiles]\n")
            } else {
                summary$.cat(parameterIndent, description, " [", row$nProfiles, " profiles]\n",
                    heading = 3, consoleOutputEnabled = FALSE)
            }
            groups <- row$profileGroups[[1L]]
            profileLabels <- character()
            displayedProfiles <- list()
            for (group in seq_len(min(row$nProfiles, limits$maxProfiles))) {
                members <- which(groups == group)
                ids <- head(members, limits$maxObjects)
                labels <- paste(text(summary$.objectTypes[ids], 40L), ids)
                named <- rep(FALSE, length(ids))
                if (!is.null(summary$.objectNames)) {
                    objectNames <- summary$.objectNames[ids]
                    named <- !is.na(objectNames) & nzchar(objectNames)
                    labels[named] <- text(objectNames[named], 18L)
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
                        membersText <- paste(
                            text(summary$.objectTypes[ids[1L]], 40L),
                            paste(ids, collapse = ", ")
                        )
                    }
                }
                if (length(members) > limits$maxObjects) {
                    omitted <- length(members) - limits$maxObjects
                    omittedLabel <- if (omitted == 1L) {
                        "object"
                    } else {
                        "objects"
                    }
                    membersText <- paste0(membersText, ", ... (+", omitted, " ", omittedLabel, "; increase maxObjects)")
                }
                if (variedCaptions) {
                    membersText <- text(membersText, 80L)
                }
                profile <- row$profiles[[1L]][[members[1L]]]
                keys <- setdiff(names(profile), "value")
                shownNA <- shownNA || anyNA(head(profile$value, limits$maxValues)) ||
                    any(vapply(keys, function(key) {
                        anyNA(head(profile[[key]], limits$maxValues))
                    }, logical(1)))
                if (!consoleOutputEnabled) {
                    profileLabels <- c(profileLabels, membersText)
                    displayedProfiles[[length(displayedProfiles) + 1L]] <- profile
                    next
                }
                values <- preview(profile$value, limits$maxValues, summary$.digits, row$values[[1L]])
                if (length(keys)) {
                    positions <- vapply(keys, function(key) {
                        paste0(.comparisonCaption(key), " = ", preview(profile[[key]], limits$maxValues))
                    }, "")
                    values <- paste0("[", paste(positions, collapse = "; "), "] ", values)
                }
                emit(profileIndent, text(paste0(membersText, ": ", values), limits$width - nchar(profileIndent)), "\n")
                if (nrow(profile) > limits$maxValues) {
                    emit(profileIndent, "More values available; increase maxValues or use showAll = TRUE.\n")
                }
            }
            if (!consoleOutputEnabled) {
                values <- .comparisonProfileMatrix(displayedProfiles, profileLabels,
                    limits$maxValues, summary$.digits, row$values[[1L]],
                    wrapColumnLabels = TRUE
                )
                .showMatrix(summary, values, summary$.digits,
                    consoleOutputEnabled = FALSE,
                    rowLabel = "Parameter", escapeMarkdown = TRUE
                )
            }
            if (row$nProfiles > limits$maxProfiles) {
                emit(profileIndent, "... ", row$nProfiles - limits$maxProfiles, " more profiles; increase maxProfiles or use showAll = TRUE.\n")
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
    if (shortened) {
        emit("Text shortened; use textMode = \"wrap\" or \"full\", or increase width.\n")
    }
    emit("Complete display: summary(x, showAll = TRUE).\n")
    if (.getComparisonSummaryHarmonization(summary)) {
        emit("Full data: as.data.frame(x). Exact profiles: summary(x)$parameterInfo.\n")
    } else {
        emit("Full data: as.data.frame(x, harmonizeColumnNames = FALSE).\n")
        emit("Exact profiles: summary(x, harmonizeColumnNames = FALSE)$parameterInfo.\n")
    }
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
#' @param xAxis,yAxis Optional axis column names or captions. Omitted axes are
#'   selected only when there is exactly one candidate: inputs/design settings
#'   are preferred for x, results for y, then other varying numeric columns.
#'   Identifier columns are excluded; stages remain eligible axes. `yAxis`
#'   may also explicitly select multiple numeric parameters. Their axis sides
#'   can be controlled through `yAxisSide` passed to [plot.TrialComparison()].
#' @param main Plot title. When omitted, generated from the original objects.
#'   NULL or NA hides the title.
#' @param xlab,ylab Axis labels. NA uses automatic parameter captions; NULL hides
#'   the label. `ylab` overrides the primary y-axis label, including a single
#'   right-positioned axis. A secondary y axis retains its automatic caption.
#' @param category Optional column names defining categories. Multiple columns
#'   define joint categories. NULL selects automatically; NA_character_ or
#'   character(0) disables categories.
#' @param group Optional column name defining an additional grouping.
#' @param ... Arguments passed to [plot.TrialComparison()], including categories,
#'   grouping, titles and line/point settings. Explicit axis selections prevail.
#' @param data Optional comparison data frame with additional annotation columns.
#'   Rows must follow the order of `as.data.frame(x)`. Without custom data,
#'   summary plots retain the summary's column-name harmonization setting.
#'
#' @return A ggplot object. Ambiguous selections raise an error listing candidate
#'   descriptions and a concrete example.
#'
#' @details
#' `xAxis`, `yAxis`, `category`, and `group` accept technical column names,
#' captions from the parameter dictionary, and object-specific captions as in
#' [fetch()]. For example, `"Alternative"` and `"Alternatives"` both select
#' `alternative`. Names may also be unquoted, including inside `c(...)`.
#' Defined variables and other expressions are evaluated in the calling environment.
#' Exact column names take precedence over captions. Other names must resolve
#' unambiguously to a column present in the comparison export; harmonized names
#' are taken into account. With fixed plans alone, `"Number of subjects"` can
#' select `nFixed`. Unknown names, ambiguous captions and duplicate selections
#' (including two aliases for one column) raise selection errors.
#'
#' @examples
#' comparison <- getSampleSizeMeans(alternative = 0.22, stDev = 0.5) |>
#'     compare(getSampleSizeMeans(alternative = 0.44, stDev = 1)) |>
#'     compare(getSampleSizeMeans(alternative = 0.59, stDev = 1))
#' plot(comparison,
#'     xAxis = "Alternative",
#'     yAxis = c("Number of subjects", "Critical values (treatment effect scale)"),
#'     category = "Standard deviation"
#' )
#' plot(comparison, xAxis = alternative, yAxis = numberOfSubjects, group = stDev)
#'
#' @export
#'
plot.TrialComparison <- function(
        x,
        xAxis = NULL,
        yAxis = NULL,
        ...,
        data = NULL,
        category = NULL,
        group = NULL,
        main = NULL,
        xlab = NA_character_,
        ylab = NA_character_) {
    xAxis <- .comparisonPlotColumnArgument(substitute(xAxis), parent.frame())
    yAxis <- .comparisonPlotColumnArgument(substitute(yAxis), parent.frame())
    category <- .comparisonPlotColumnArgument(substitute(category), parent.frame())
    group <- .comparisonPlotColumnArgument(substitute(group), parent.frame())
    if (missing(main)) {
        main <- .toCapitalizedSingleCharacter(.getTrialComparisonTitle(x$getObjects()))
    }
    if (!is.null(data)) {
        expectedRows <- nrow(as.data.frame(x))
        if (!is.data.frame(data) || nrow(data) != expectedRows) {
            stopIllegalDataInput("'data' must be a data frame with one row per comparison situation",
                functionName = "plot.TrialComparison", parameter = "data", value = data,
                constraint = "a data frame with one row per comparison situation",
                diagnosticId = "comparison.plot_data_rows_invalid",
                context = list(expectedRows = expectedRows, actualRows = NROW(data))
            )
        }
        return(.plotComparison(data,
            xAxis = xAxis,
            yAxis = yAxis,
            category = category, group = group,
            main = main, xlab = xlab, ylab = ylab,
            objects = x$getObjects(),
            ...
        ))
    }
    return(.plotComparison(x,
        xAxis = xAxis, yAxis = yAxis,
        category = category, group = group,
        main = main, xlab = xlab, ylab = ylab, ...
    ))
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
plot.TrialComparisonSummary <- function(
        x,
        xAxis = NULL,
        yAxis = NULL,
        ...,
        showSummary = FALSE,
        category = NULL,
        group = NULL,
        data = NULL) {
    xAxis <- .comparisonPlotColumnArgument(substitute(xAxis), parent.frame())
    yAxis <- .comparisonPlotColumnArgument(substitute(yAxis), parent.frame())
    category <- .comparisonPlotColumnArgument(substitute(category), parent.frame())
    group <- .comparisonPlotColumnArgument(substitute(group), parent.frame())
    .assertIsSingleLogical(showSummary, "showSummary")
    if (showSummary) {
        print(x)
    }
    if (is.null(data) && !.getComparisonSummaryHarmonization(x)) {
        data <- as.data.frame(x$object, harmonizeColumnNames = FALSE)
    }
    return(plot(x$object,
        xAxis = xAxis, yAxis = yAxis,
        category = category, group = group, data = data, ...
    ))
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
