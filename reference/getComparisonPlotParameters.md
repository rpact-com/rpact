# Select Parameters for a Comparison Plot

Proposes axes and an object category without creating a plot. Selection
problems are returned as text so applications can display them before
plotting. The four column selectors accept technical names, dictionary
captions, and object-specific captions, as in
[`fetch()`](https://docs.rpact.org/reference/fetch.ParameterSet.md).
Unbound names may be supplied without quotes; defined variables and
expressions are evaluated normally.

## Usage

``` r
getComparisonPlotParameters(
  data,
  xAxis = NULL,
  yAxis = NULL,
  ...,
  category = NULL,
  group = NULL,
  objects = NULL,
  yAxisSide = "auto"
)
```

## Arguments

- data:

  A comparison data frame, a `TrialComparison`, or its summary.

- xAxis, yAxis:

  Optional explicit axis names or captions. `yAxis` may be a vector of
  distinct numeric columns. Automatic axes must be varying numeric
  columns. Every y column must be numeric; an explicitly selected x-axis
  may also be categorical.

- ...:

  Further arguments. Currently unused.

- category:

  Optional character vector of category column names. All selected
  columns jointly define the categories. NULL selects `objectName`,
  `objectNumber`, or `designNumber`, in that order, when it identifies
  multiple objects and is not selected as an axis or group.
  NA_character\_ disables automatic categories, as does character(0). No
  category is needed for a single object.

- group:

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
  [`plot.TrialComparison()`](https://docs.rpact.org/reference/plot.TrialComparison.md).

## Value

A list containing `xAxis`, `yAxis`, `category`, `group`,
`connectAcrossObjects`, `yAxisSide`, `yAxisTransformation`,
`yAxisRanges`, `message`, `candidates`, and `issues`. `yAxisSide` is
named by the selected parameters; the right-axis plotting transformation
is `value * scale + offset` from `yAxisTransformation`. `yAxisRanges`
contains the reference ranges used for scaling, not ggplot's expanded
panel limits. Unresolved selections are NULL. `message` is NULL on
success, otherwise summarizes selection problems. `candidates` lists
eligible columns for each selection; `issues` contains diagnostic
metadata used by
[`plot.TrialComparison()`](https://docs.rpact.org/reference/plot.TrialComparison.md).

## Details

With original objects available, comparisons in which each object
contributes one point at a distinct finite numeric x value are connected
across objects when only that input varies and the object scope is
shared. Automatic object categories are omitted in this case and
`connectAcrossObjects` is TRUE. Explicit categories or groups retain
their separate curves.

Axis-selection messages use a short problem statement, separate
candidate lists with names and captions, and one example using eligible
axes. Text wraps to the console width (up to 100 characters). Candidate
lists show at most five entries per axis and may be shortened further to
keep the message compact. Full candidates and parameter descriptions
remain available in `issues`.

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
selection <- getComparisonPlotParameters(comparison, yAxis = "criticalValues")
selection[c("xAxis", "yAxis", "category", "message")]
#> $xAxis
#> [1] "alpha"
#> 
#> $yAxis
#> [1] "criticalValues"
#> 
#> $category
#> NULL
#> 
#> $message
#> NULL
#> 
```
