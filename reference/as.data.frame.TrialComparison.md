# Trial Comparison Data Frame

Converts a trial comparison to an unrounded data frame.

## Usage

``` r
# S3 method for class 'TrialComparison'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  niceColumnNamesEnabled = FALSE,
  includeAllParameters = FALSE,
  removeConstantParameters = FALSE,
  transposed = FALSE
)
```

## Arguments

- x:

  A `TrialComparison`.

- row.names:

  Optional row names for the result.

- optional:

  Unused; included for compatibility with
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- ...:

  Further arguments. Currently unused.

- niceColumnNamesEnabled:

  Logical. If `TRUE`, nice looking column names will be used; syntactic
  names (variable names) otherwise (see
  [`make.names`](https://rdrr.io/r/base/make.names.html)).

- includeAllParameters:

  If `TRUE`, include all parameters supported by the existing data-frame
  methods of the individual objects.

- removeConstantParameters:

  If `TRUE`, retain identifiers and only parameters with different
  object profiles, using the same rules as
  [`summary()`](https://rdrr.io/r/base/summary.html).

- transposed:

  If `TRUE`, return parameters as rows and comparison situations as
  columns. Mixed column types are converted to a common type by
  transposition.

## Value

An unrounded data.frame. All columns are returned by default.
