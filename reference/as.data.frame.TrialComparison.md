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
  removeConstantParameters = FALSE
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

  If `TRUE`, use descriptive column names.

- includeAllParameters:

  If `TRUE`, include all parameters supported by the existing data-frame
  methods of the individual objects.

- removeConstantParameters:

  If TRUE, retain identifiers and only parameters with different object
  profiles, using the same rules as
  [`summary()`](https://rdrr.io/r/base/summary.html).

## Value

An unrounded data.frame. All columns are returned by default.
