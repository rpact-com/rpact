# Compare Existing Trial Planning Objects

Creates a comparison of existing designs, design characteristics, sample
size or power plans, simulation results, and power/ASN results without
recalculating them. A single comparison summary returns its underlying
comparison.

[`length()`](https://rdrr.io/r/base/length.html) returns the number of
objects in the comparison.

[`names()`](https://rdrr.io/r/base/names.html) returns the supplied
names of the comparison objects.

## Usage

``` r
getComparison(..., objects = NULL)

# S3 method for class 'TrialComparison'
length(x)

# S3 method for class 'TrialComparison'
names(x)
```

## Arguments

- ...:

  Designs, design characteristics, plans, simulations,
  `PowerAndAverageSampleNumberResult`, `EventProbabilities`,
  `NumberOfSubjects`, `ConditionalPowerResults` (including subclasses),
  `PerformanceScore`, `FisherInformation`, or
  `TrialDesignOptimalConditionalErrorCharacteristics` objects, lists
  thereof, `TrialComparison` objects, or `TrialComparisonSummary`
  objects. Entries from `...` precede entries from `objects`.

- objects:

  Optional objects, lists of objects, `TrialComparison` objects, or
  `TrialComparisonSummary` objects. NULL is treated as an empty list.

- x:

  A `TrialComparison`.

## Value

An R6 `TrialComparison` inheriting from `ParameterSet`. For a single
`TrialComparisonSummary`, its underlying comparison is returned by
reference; otherwise a new collection is created.

## Details

The comparison retains references to its objects. Changes to an original
object are reflected in subsequent comparisons; no comparison data are
cached. `$getObjects()` and
[`getObjects()`](https://docs.rpact.org/qtd/reference/getObjects.md)
return the named list of references. Replacing entries in that returned
list does not change the comparison.
[`length()`](https://rdrr.io/r/base/length.html) is the number of
objects and [`names()`](https://rdrr.io/r/base/names.html) returns their
supplied names (or `NULL`). Retrieve contained objects using
`comparison[[i]]`, `comparison[["name"]]` or `comparison$name`. R6
members take precedence for `$`; use `[[` to retrieve an object whose
name coincides with a member such as `add`. Unknown names return NULL.
The
[`fetch.TrialComparison()`](https://docs.rpact.org/qtd/reference/fetch.TrialComparison.md)
method provides the same object access through `fetch(comparison, name)`
or `fetch(comparison, position)`. A single summary can be unwrapped with
`summaryObject |> getComparison()`, returning exactly
`summaryObject$object`. Changes to the returned collection therefore
affect that underlying comparison. In combinations or nested lists,
summaries contribute their underlying comparisons' current objects; a
new collection is created without changing the source collections.
Summary display settings are not transferred to the comparison.

Lists and comparisons are recursively expanded in their original order.
Names on individual objects are retained; container names are discarded.
Supplied non-empty names must be unique across the expanded collection;
duplicate names raise an error. Unnamed entries and repeated references
to the same object are allowed. getComparison() creates an empty
comparison. Its data frame has zero rows and columns, and plotting
requires numeric data to be added first.

`comparison$add(..., objects = NULL)` appends objects with the same
input rules and returns the comparison invisibly, supporting chaining.
All new entries are validated before the stored list is changed,
including name collisions with existing objects. Adding another
comparison copies its current list of references; subsequent additions
to that source comparison do not affect the target. Existing summary
objects remain snapshots; call
[`summary()`](https://rdrr.io/r/base/summary.html) again after changing
the collection.

`comparison$delete(index)` removes one object by its positive integer
position or exact name and returns the comparison invisibly. Invalid
positions or unknown names raise an error without changing the
collection. Removing the last object leaves an empty comparison, which
can be filled again. Names and order of remaining objects are retained;
positions are renumbered. The S3 generics
[`add()`](https://docs.rpact.org/qtd/reference/add.md) and
[`delete()`](https://docs.rpact.org/qtd/reference/delete.md) delegate to
these methods and support pipelines such as
`comparison |> add(other) |> delete(1)`. Shallow cloning shares the
contained objects. `clone(deep = TRUE)` is unsupported.

[`print()`](https://rdrr.io/r/base/print.html) describes the collection
and selected observed ranges.
[`summary()`](https://rdrr.io/r/base/summary.html) groups differences by
object scope, inputs/design settings and results, with equal profiles
grouped by object number. Constants are compared independently of the
number of rows. Constant numeric vector cells, such as `1, 1`, compare
equal to the corresponding scalar. Nonconstant profiles retain
stage/situation positions; repeated values across the other dimension
are collapsed. Missing values and their positions participate in exact
comparisons. No rounding or numerical tolerance is used to decide
whether profiles differ.

For comparisons containing interim analyses, fixed plans' `nFixed`,
`nFixed1`, and `nFixed2` are harmonized to `numberOfSubjects`,
`numberOfSubjects1`, and `numberOfSubjects2`. The corresponding stored
group sizes are included for sequential plans. Comparisons of fixed
plans alone retain the fixed names. With `includeAllParameters = TRUE`,
sequential plans' `nFixed` reference values remain separate from their
stage-wise sample sizes. Fixed survival plans also use
`cumulativeEventsPerStage` for `eventsFixed` when interim analyses occur
in the comparison; sequential plans retain their fixed-design event
reference.

Available sample-size expectations `expectedNumberOfSubjectsH1` and
`expectedEventsH1` use `expectedNumberOfSubjects` and
`expectedNumberOfEvents`. These expectations refer to each scenario's
supplied alternative. Harmonization does not change alternatives or
stopping rules, combine H0/H0-H1 expectations, or reinterpret simulation
assumptions used for conditional power. Conflicting non-missing target
values remain in separate columns. Power/ASN results from
[`getPowerAndAverageSampleNumber()`](https://docs.rpact.org/qtd/reference/getPowerAndAverageSampleNumber.md)
retain their generic `theta`, `nMax`, and `averageSampleNumber` scales.
Individual `theta` scenarios retain their stage coordinates even in full
exports. The comparison includes `nMax` even when the compact source
data frame omits it; no endpoint or conversion to subjects or events is
inferred. With column harmonization enabled, their stage-wise
`earlyStop` becomes `earlyStopPerStage`, `overallEarlyStop` becomes
`earlyStop`, and `overallFutility` becomes `futilityStop`.
`calculatedPower` merges into `overallReject` when the values agree.
Original names remain available with `harmonizeColumnNames = FALSE` in
data frames and summaries. Original objects are unchanged.

Event probabilities and recruitment results retain their time
coordinates and assumptions. Conditional-power results retain planned
stages, treatment arms or populations, and observed stage and data
context; no analysis is rerun. Performance scores retain scenario
alternatives, simulation inputs and design settings. Fisher information
retains selected stages, situations, the source plan when available, and
its cumulative or stage-wise type; these scales are not harmonized.
Profiles and plot groups respect time, arm, population and situation
coordinates.

Optimal conditional-error characteristics are supported internally,
including their theta-by-stage results and atomic design settings. Their
constructor and characteristic functions remain unexported.
Function-valued settings are not compared as scalar parameters.
Deprecated `TrialDesignSet` objects and standalone optimal
conditional-error designs are not supported.

Console and Markdown output are bounded. Limits and display rounding do
not change the underlying data.
[`as.data.frame.TrialComparison()`](https://docs.rpact.org/qtd/reference/as.data.frame.TrialComparison.md)
returns the full comparison table.
[`plot.TrialComparison()`](https://docs.rpact.org/qtd/reference/plot.TrialComparison.md)
selects axes only when unambiguous.
[`getObjectRCode()`](https://docs.rpact.org/qtd/reference/getObjectRCode.md)
reconstructs the comparison and its contained objects; executing
generated simulation code reruns the corresponding simulations.

## Examples

``` r
asn <- getPowerAndAverageSampleNumber(getDesignGroupSequential(kMax = 2),
    theta = c(0, 0.2), nMax = 100
)
getComparison(asn, asn)
#> Power and average sample size (ASN) comparison
#> 
#> 2 objects; 8 comparison rows; 27 columns.
#> 
#> Object types:
#>     Power and average sample size (ASN) : 2
#> 
#> Observed metrics (across objects, stages and situations):
#>     Maximum number of stages   : 2
#>     Significance level         : 0.025
#>     Type II error rate         : 0.2
#>     N_max                      : 100
#>     Average sample sizes (ASN) : 95.82798, 99.87086
#>     Overall reject             : 0.0250000, 0.5122623
#> 
#> Available operations:
#>     summary(x)                        : Compare differences
#>     as.data.frame(x)                  : Access all data
#>     plot(x, xAxis = ..., yAxis = ...) : Plot selected parameters
#>     getObjects(x)                     : Access contained objects

comparison <- getComparison(list(
    fixed = getDesignFixed(), sequential = getDesignGroupSequential(kMax = 2)
))
print(comparison)
#> Design comparison
#> 
#> 2 objects; 3 comparison rows; 19 columns.
#> 
#> Object types:
#>     Fixed sample size design : 1
#>     Group sequential design  : 1
#> 
#> Observed metrics (across objects, stages and situations):
#>     Maximum number of stages : 1, 2
#>     Significance level       : 0.025
#>     Type II error rate       : 0.2
#> 
#> Available operations:
#>     summary(x)                        : Compare differences
#>     as.data.frame(x)                  : Access all data
#>     plot(x, xAxis = ..., yAxis = ...) : Plot selected parameters
#>     getObjects(x)                     : Access contained objects
comparisonSummary <- summary(comparison)
identical(comparisonSummary |> getComparison(), comparison)
#> [1] TRUE
head(as.data.frame(comparison))
#>   objectNumber objectName stages situation                 objectType  endpoint
#> 1            1      fixed      1         1           TrialDesignFixed Undefined
#> 2            2 sequential      1         1 TrialDesignGroupSequential Undefined
#> 3            2 sequential      2         1 TrialDesignGroupSequential Undefined
#>   calculationTarget kMax typeOfDesign alpha beta bindingFutility sided
#> 1            Design    1         <NA> 0.025  0.2              NA     1
#> 2            Design    2           OF 0.025  0.2           FALSE     1
#> 3            Design    2           OF 0.025  0.2           FALSE     1
#>   tolerance  alphaSpent criticalValues stageLevels informationRates
#> 1        NA          NA       1.959964 0.025000000               NA
#> 2     1e-08 0.002582893       2.796510 0.002582893              0.5
#> 3     1e-08 0.024999990       1.977431 0.023996469              1.0
#>   futilityBounds
#> 1             NA
#> 2           -Inf
#> 3           -Inf
withCharacteristics <- getComparison(
    getDesignGroupSequential(kMax = 2),
    getDesignCharacteristics(getDesignGroupSequential(kMax = 3))
)
combined <- getComparison(comparison, other = getDesignFixed(alpha = 0.05))
combined$add(list(getDesignFixed(alpha = 0.01)))
#> Design comparison
#> 
#> 4 objects; 5 comparison rows; 19 columns.
#> 
#> Object types:
#>     Fixed sample size design : 3
#>     Group sequential design  : 1
#> 
#> Observed metrics (across objects, stages and situations):
#>     Maximum number of stages : 1, 2
#>     Significance level       : 0.01, 0.05
#>     Type II error rate       : 0.2
#> 
#> Available operations:
#>     summary(x)                        : Compare differences
#>     as.data.frame(x)                  : Access all data
#>     plot(x, xAxis = ..., yAxis = ...) : Plot selected parameters
#>     getObjects(x)                     : Access contained objects
empty <- getComparison()
empty$add(getDesignFixed())
#> Design comparison
#> 
#> 1 objects; 1 comparison rows; 12 columns.
#> 
#> Object types:
#>     Fixed sample size design : 1
#> 
#> Observed metrics (across objects, stages and situations):
#>     Maximum number of stages : 1
#>     Significance level       : 0.025
#>     Type II error rate       : 0.2
#> 
#> Available operations:
#>     summary(x)                        : Compare differences
#>     as.data.frame(x)                  : Access all data
#>     plot(x, xAxis = ..., yAxis = ...) : Plot selected parameters
#>     getObjects(x)                     : Access contained objects
combined$delete(1)
#> Design comparison
#> 
#> 3 objects; 4 comparison rows; 19 columns.
#> 
#> Object types:
#>     Fixed sample size design : 2
#>     Group sequential design  : 1
#> 
#> Observed metrics (across objects, stages and situations):
#>     Maximum number of stages : 1, 2
#>     Significance level       : 0.01, 0.05
#>     Type II error rate       : 0.2
#> 
#> Available operations:
#>     summary(x)                        : Compare differences
#>     as.data.frame(x)                  : Access all data
#>     plot(x, xAxis = ..., yAxis = ...) : Plot selected parameters
#>     getObjects(x)                     : Access contained objects
combined$delete("sequential")
#> Design comparison
#> 
#> 2 objects; 2 comparison rows; 13 columns.
#> 
#> Object types:
#>     Fixed sample size design : 2
#> 
#> Observed metrics (across objects, stages and situations):
#>     Maximum number of stages : 1
#>     Significance level       : 0.01, 0.05
#>     Type II error rate       : 0.2
#> 
#> Available operations:
#>     summary(x)                        : Compare differences
#>     as.data.frame(x)                  : Access all data
#>     plot(x, xAxis = ..., yAxis = ...) : Plot selected parameters
#>     getObjects(x)                     : Access contained objects
if (FALSE) { # \dontrun{
plot(comparison,
    xAxis = "objectNumber",
    yAxis = "criticalValues",
    plotLineEnabled = FALSE
)
} # }
```
