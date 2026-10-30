# Compare Existing Trial Planning Objects

Creates a comparison of existing designs, design characteristics, sample
size or power plans, and simulation results without recalculating them.

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

  Designs, design characteristics, plans, simulations, lists thereof, or
  `TrialComparison` objects. Entries from `...` precede entries from
  `objects`.

- objects:

  Optional objects, lists of objects or `TrialComparison` objects. NULL
  is treated as an empty list.

- x:

  A `TrialComparison`.

## Value

An R6 `TrialComparison` inheriting from `ParameterSet`.

## Details

The comparison retains references to its objects. Changes to an original
object are reflected in subsequent comparisons; no comparison data are
cached. `$getObjects()` and
[`getObjects()`](https://docs.rpact.org/reference/getObjects.md) return
the named list of references. Replacing entries in that returned list
does not change the comparison.
[`length()`](https://rdrr.io/r/base/length.html) is the number of
objects and [`names()`](https://rdrr.io/r/base/names.html) returns their
supplied names (or `NULL`). Retrieve contained objects using
`comparison[[i]]`, `comparison[["name"]]` or `comparison$name`. R6
members take precedence for `$`; use `[[` to retrieve an object whose
name coincides with a member such as `add`. Unknown names return NULL.
The
[`fetch.TrialComparison()`](https://docs.rpact.org/reference/fetch.TrialComparison.md)
method provides the same object access through `fetch(comparison, name)`
or `fetch(comparison, position)`. Lists and comparisons are recursively
expanded in their original order. Names on individual objects are
retained; container names are discarded. Supplied non-empty names must
be unique across the expanded collection; duplicate names raise an
error. Unnamed entries and repeated references to the same object are
allowed. getComparison() creates an empty comparison. Its data frame has
zero rows and columns, and plotting requires numeric data to be added
first.

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
[`add()`](https://docs.rpact.org/reference/add.md) and
[`delete()`](https://docs.rpact.org/reference/delete.md) delegate to
these methods and support pipelines such as
`comparison |> add(other) |> delete(1)`. Shallow cloning shares the
contained objects. `clone(deep = TRUE)` is unsupported.

[`print()`](https://rdrr.io/r/base/print.html) describes the collection
and selected observed ranges.
[`summary()`](https://rdrr.io/r/base/summary.html) groups differences by
object scope, inputs/design settings and results, with equal profiles
grouped by object number. Constants are compared independently of the
number of rows. Nonconstant profiles retain stage/situation positions;
repeated values across the other dimension are collapsed. Missing values
and their positions participate in exact comparisons. No rounding or
numerical tolerance is used to decide whether profiles differ.

Console and Markdown output are bounded. Limits and display rounding do
not change the underlying data.
[`as.data.frame.TrialComparison()`](https://docs.rpact.org/reference/as.data.frame.TrialComparison.md)
returns the full comparison table.
[`plot.TrialComparison()`](https://docs.rpact.org/reference/plot.TrialComparison.md)
selects axes only when unambiguous.
[`getObjectRCode()`](https://docs.rpact.org/reference/getObjectRCode.md)
reconstructs the comparison and its contained objects; executing
generated simulation code reruns the corresponding simulations.

## Examples

``` r
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
#>     summary(x)                                  : Compare differences
#>     as.data.frame(x)                            : Access all data
#>     plot(x, xParamName = ..., yParamName = ...) : Plot selected parameters
#>     getObjects(x)                               : Access contained objects
summary(comparison)
#> Design comparison summary
#> 
#> Comparison of 2 trial planning results: fixed sample size design, group sequential design.
#> The comparison identifies 10 differing parameters and 5 shared parameters or profiles.
#> Shared (examples): Endpoint, Calculation target, Significance level, Type II error rate, Test.
#> 
#> Object types and scope:
#> 
#>     Object type: character, factor [2 profiles]
#>         fixed: TrialDesignFixed
#>         sequential: TrialDesignGroupSequential
#> 
#> Inputs and design settings:
#> 
#>     Maximum number of stages: integer, factor [2 profiles]
#>         fixed: 1
#>         sequential: 2
#> 
#>     Type of design: character, factor [2 profiles]
#>         fixed: NA
#>         sequential: OF
#> 
#>     Binding futility: logical, factor [2 profiles]
#>         fixed: NA
#>         sequential: FALSE
#> 
#>     Tolerance: numeric, continuous [2 profiles]
#>         fixed: NA
#>         sequential: 1e-08
#> 
#> Results and derived parameters:
#> 
#>     Critical values: numeric, continuous [2 profiles]
#>         fixed: 1.959964
#>         sequential: [Stages = 1, 2] 2.796510, 1.977431
#> 
#>     Stage levels (one-sided): numeric, continuous [2 profiles]
#>         fixed: 0.025000000
#>         sequential: [Stages = 1, 2] 0.002582893, 0.023996469
#> 
#>     Information rates: numeric, continuous [2 profiles]
#>         fixed: NA
#>         sequential: [Stages = 1, 2] 0.5, 1.0
#> 
#>     Futility bounds (binding) / Futility bounds (non-binding): numeric, cont... [2 profiles]
#>         fixed (Futility bounds (binding)): NA
#>         sequential (Futility bounds (non-binding)): -Inf
#> 
#>     Cumulative alpha spending: numeric, continuous [2 profiles]
#>         fixed: NA
#>         sequential: [Stages = 1, 2] 0.002582893, 0.024999990
#> 
#> Legend:
#>     Profiles compare exact values at each stage and situation.
#>     NA: missing/unavailable.
#> 
#> Full data: as.data.frame(x). Exact profiles: summary(x)$parameterInfo.
head(as.data.frame(comparison))
#>   objectNumber                 objectType  endpoint calculationTarget
#> 1            1           TrialDesignFixed Undefined            Design
#> 2            2 TrialDesignGroupSequential Undefined            Design
#> 3            2 TrialDesignGroupSequential Undefined            Design
#>   objectName situation alpha beta sided criticalValues stageLevels kMax stages
#> 1      fixed         1 0.025  0.2     1       1.959964 0.025000000    1      1
#> 2 sequential         1 0.025  0.2     1       2.796510 0.002582893    2      1
#> 3 sequential         1 0.025  0.2     1       1.977431 0.023996469    2      2
#>   typeOfDesign informationRates futilityBounds bindingFutility tolerance
#> 1         <NA>               NA             NA              NA        NA
#> 2           OF              0.5           -Inf           FALSE     1e-08
#> 3           OF              1.0           -Inf           FALSE     1e-08
#>    alphaSpent
#> 1          NA
#> 2 0.002582893
#> 3 0.024999990
withCharacteristics <- getComparison(
    getDesignGroupSequential(kMax = 2),
    getDesignCharacteristics(getDesignGroupSequential(kMax = 3)))
combined <- getComparison(comparison, other = getDesignFixed(alpha = 0.05))
combined$add(list(getDesignFixed(alpha = 0.01)))
empty <- getComparison()
empty$add(getDesignFixed())
combined$delete(1)
combined$delete("sequential")
if (FALSE) { # \dontrun{
plot(comparison, xParamName = "objectNumber", yParamName = "criticalValues", plotLineEnabled = FALSE)
} # }
```
