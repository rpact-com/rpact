# Coerce AnalysisResults to a Data Frame

Returns the
[`AnalysisResults`](https://rpact-com.github.io/rpact/reference/AnalysisResults.md)
object as data frame.

## Usage

``` r
# S3 method for class 'AnalysisResults'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  niceColumnNamesEnabled = FALSE
)
```

## Arguments

- x:

  An
  [`AnalysisResults`](https://rpact-com.github.io/rpact/reference/AnalysisResults.md)
  object created by
  [`getAnalysisResults()`](https://rpact-com.github.io/rpact/reference/getAnalysisResults.md).

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- niceColumnNamesEnabled:

  Logical. If `TRUE`, nice looking column names will be used; syntactic
  names (variable names) otherwise (see
  [`make.names`](https://rdrr.io/r/base/make.names.html)).

## Value

Returns a [`data.frame`](https://rdrr.io/r/base/data.frame.html).

## Details

Coerces the analysis results to a data frame.
