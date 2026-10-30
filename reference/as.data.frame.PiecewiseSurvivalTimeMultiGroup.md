# Coerce a Multi-Group Piecewise Survival Specification to a Data Frame

Returns one row per group, interval, and situation, including control
and active hazards.

## Usage

``` r
# S3 method for class 'PiecewiseSurvivalTimeMultiGroup'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  niceColumnNamesEnabled = FALSE,
  includeAllParameters = FALSE
)
```

## Arguments

- x:

  A multi-arm or enrichment piecewise survival specification.

- row.names:

  Optional row names.

- optional:

  Unused; provided for compatibility with
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- ...:

  Further arguments, checked for unknown arguments.

- niceColumnNamesEnabled:

  Whether descriptive column names should be used.

- includeAllParameters:

  Unused; all specification rows are always included.

## Value

A data frame in group-by-interval-by-situation order.
