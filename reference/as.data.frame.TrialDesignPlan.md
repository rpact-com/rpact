# Coerce Trial Design Plan to a Data Frame

Returns the
[`TrialDesignPlan`](https://docs.rpact.org/reference/TrialDesignPlan.md)
as data frame.

## Usage

``` r
# S3 method for class 'TrialDesignPlan'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  niceColumnNamesEnabled = FALSE,
  includeAllParameters = FALSE,
  ...
)
```

## Arguments

- x:

  A
  [`TrialDesignPlan`](https://docs.rpact.org/reference/TrialDesignPlan.md)
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

Coerces the design plan to a data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
as.data.frame(getSampleSizeMeans())
} # }
```
