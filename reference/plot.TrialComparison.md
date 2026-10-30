# Plot a Trial Comparison

Plots selected variables from a trial comparison.

The summary method plots the objects contained in the comparison.

## Usage

``` r
# S3 method for class 'TrialComparison'
plot(x, xParamName = NULL, yParamName = NULL, ...)

# S3 method for class 'TrialComparisonSummary'
plot(x, xParamName = NULL, yParamName = NULL, ..., showSummary = FALSE)
```

## Arguments

- x:

  A `TrialComparison` or its summary.

- xParamName, yParamName:

  Optional axis column names. Omitted axes are selected only when there
  is exactly one candidate: inputs/design settings are preferred for x,
  results for y, then other varying numeric columns. Identifier columns
  are excluded; stages remain eligible axes. `yParamName` may also
  explicitly select multiple numeric parameters. Their axis sides can be
  controlled through `yAxisSide` passed to
  [`plotComparison()`](https://docs.rpact.org/reference/plotComparison.md).

- ...:

  Arguments passed to
  [`plotComparison()`](https://docs.rpact.org/reference/plotComparison.md),
  including categories, grouping, titles and line/point settings.
  Explicit axis selections prevail.

- showSummary:

  If TRUE, also print the summary before creating its plot.

## Value

A ggplot object. Ambiguous selections raise an error listing candidate
descriptions and a concrete example.
