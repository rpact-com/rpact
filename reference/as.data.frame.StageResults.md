# Coerce Stage Results to a Data Frame

Returns the `StageResults` as data frame.

## Usage

``` r
# S3 method for class 'StageResults'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  niceColumnNamesEnabled = FALSE,
  includeAllParameters = FALSE,
  type = 1,
  ...
)
```

## Arguments

- x:

  A [`StageResults`](https://docs.rpact.org/reference/StageResults.md)
  object.

- niceColumnNamesEnabled:

  Logical. If `TRUE`, nice looking column names will be used; syntactic
  names (variable names) otherwise (see
  [`make.names`](https://rdrr.io/r/base/make.names.html)).

- includeAllParameters:

  Logical. If `TRUE`, all available parameters will be included in the
  data frame; a meaningful parameter selection otherwise, default is
  `FALSE`.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

## Value

Returns a [`data.frame`](https://rdrr.io/r/base/data.frame.html).

## Details

Coerces the stage results to a data frame.
