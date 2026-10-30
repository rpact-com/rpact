# Print a Trial Comparison

Prints a compact overview of a trial comparison using descriptive
parameter captions.

## Usage

``` r
# S3 method for class 'TrialComparison'
print(
  x,
  ...,
  markdown = NA,
  digits = NA_integer_,
  maxTypes = 5L,
  maxMetrics = 6L
)
```

## Arguments

- x:

  A `TrialComparison`.

- ...:

  Further arguments. Currently unused.

- markdown:

  Whether to return Markdown output. NA uses rpact settings.

- digits:

  Display rounding only, as in
  [`getComparisonParameterDescriptions()`](https://docs.rpact.org/reference/getComparisonParameterDescriptions.md).

- maxTypes, maxMetrics:

  Maximum numbers of object types and selected metrics to display. These
  limits do not affect
  [`as.data.frame.TrialComparison()`](https://docs.rpact.org/reference/as.data.frame.TrialComparison.md).

## Value

The object invisibly, or knitr Markdown output when requested.
