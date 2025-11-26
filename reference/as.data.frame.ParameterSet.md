# Coerce Parameter Set to a Data Frame

Returns the `ParameterSet` as data frame.

## Usage

``` r
# S3 method for class 'ParameterSet'
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

  A [`FieldSet`](https://docs.rpact.org/reference/FieldSet.md) object.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- niceColumnNamesEnabled:

  Logical. If `TRUE`, nice looking column names will be used; syntactic
  names (variable names) otherwise (see
  [`make.names`](https://rdrr.io/r/base/make.names.html)).

- includeAllParameters:

  Logical. If `TRUE`, all available parameters will be included in the
  data frame; a meaningful parameter selection otherwise, default is
  `FALSE`.

## Value

Returns a [`data.frame`](https://rdrr.io/r/base/data.frame.html).

## Details

Coerces the parameter set to a data frame.
