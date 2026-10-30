# Select Parameters for a Comparison Plot

Proposes axes and an object category without creating a plot. Selection
problems are returned as text so applications can display them before
plotting.

## Usage

``` r
getComparisonPlotParameters(
  data,
  xParamName = NULL,
  yParamName = NULL,
  ...,
  categoryParamName = NULL,
  groupParamName = NULL,
  objects = NULL,
  yAxisSide = "auto"
)
```

## Arguments

- data:

  A comparison data frame, a `TrialComparison`, or its summary.

- xParamName, yParamName:

  Optional explicit axis column names. `yParamName` may be a vector of
  distinct numeric columns. Automatic axes must be varying numeric
  columns. Every y column must be numeric; an explicitly selected x-axis
  may also be categorical.

- ...:

  Further arguments. Currently unused.

- categoryParamName:

  Optional character vector of category column names. All selected
  columns jointly define the categories. NULL selects `objectName`,
  `objectNumber`, or `designNumber`, in that order, when it identifies
  multiple objects and is not selected as an axis or group.
  NA_character\_ disables automatic categories, as does character(0). No
  category is needed for a single object.

- groupParamName:

  Optional explicit grouping column.

- objects:

  Optional original objects for role-aware selection when `data` is a
  data frame. A `TrialComparison` supplies its own objects
  automatically.

- yAxisSide:

  Axis assignment: "auto" (default), "left", or "right". Supply one
  value for all y parameters, a vector in their order, or a named vector
  assigning individual parameters. Unspecified named entries remain
  automatic. The same argument is accepted by
  [`plotComparison()`](https://docs.rpact.org/reference/plotComparison.md).

## Value

A list containing `xParamName`, `yParamName`, `categoryParamName`,
`groupParamName`, `yAxisSide`, `yAxisTransformation`, `yAxisRanges`,
`message`, `candidates`, and `issues`. `yAxisSide` is named by the
selected parameters; the right-axis plotting transformation is
`value * scale + offset` from `yAxisTransformation`. `yAxisRanges`
contains the reference ranges used for scaling, not ggplot's expanded
panel limits. Unresolved selections are NULL. `message` is NULL on
success, otherwise contains all selection problems. `candidates` lists
eligible columns for each selection; `issues` contains diagnostic
metadata used by
[`plotComparison()`](https://docs.rpact.org/reference/plotComparison.md).

## Details

Explicit selections prevail. When original objects are available, inputs
and design settings are preferred for x and results for y, as in the
comparison plot method. Without original objects, only varying numeric
columns determine automatic axes. Identifiers are excluded, but stages
remain eligible. An omitted axis is selected only when exactly one
eligible candidate remains. Ambiguity, invalid column selections, and
unsupported data structures are returned without raising a selection
error. A category may remain NULL without an error when no object
category is available or required. The data and the original objects are
not changed, and no ggplot2 installation is required.

Automatic y-axis assignment uses the largest gap between finite nonzero
parameter magnitudes (maximum absolute values). A gap of at least a
factor of ten separates the smaller scale onto the right axis. Otherwise
all parameters share the left axis. Consistent explicit reversals also
reverse automatic assignments; explicit assignments always prevail.
There are at most two shared scales, with no separate normalization
within either side. Dual-axis reference ranges include zero.
Probability-like ranges within numerical tolerance of `[0, 1]`, with
maximum at least 0.1, use `[0, 1]`. Smaller magnitudes retain their
observed range. Constant zero or entirely missing ranges use `[0, 1]` to
avoid a degenerate transformation.

## Examples

``` r
comparison <- getComparison(
    first = getDesignFixed(alpha = 0.025),
    second = getDesignFixed(alpha = 0.05)
)
selection <- getComparisonPlotParameters(comparison, yParamName = "criticalValues")
selection[c("xParamName", "yParamName", "categoryParamName", "message")]
#> $xParamName
#> [1] "alpha"
#> 
#> $yParamName
#> [1] "criticalValues"
#> 
#> $categoryParamName
#> [1] "objectName"
#> 
#> $message
#> NULL
#> 
```
