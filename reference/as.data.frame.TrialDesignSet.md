# Coerce Trial Design Set to a Data Frame

Returns the `TrialDesignSet` as data frame.

## Usage

``` r
# S3 method for class 'TrialDesignSet'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  niceColumnNamesEnabled = FALSE,
  includeAllParameters = FALSE,
  addPowerAndAverageSampleNumber = FALSE,
  theta = seq(-1, 1, 0.02),
  nMax = NA_integer_,
  ...
)
```

## Arguments

- x:

  A
  [`TrialDesignSet`](https://docs.rpact.org/reference/TrialDesignSet.md)
  object.

- niceColumnNamesEnabled:

  Logical. If `TRUE`, nice looking column names will be used; syntactic
  names (variable names) otherwise (see
  [`make.names`](https://rdrr.io/r/base/make.names.html)).

- includeAllParameters:

  Logical. If `TRUE`, all available parameters will be included in the
  data frame; a meaningful parameter selection otherwise, default is
  `FALSE`.

- addPowerAndAverageSampleNumber:

  If `TRUE`, power and average sample size will be added to data frame,
  default is `FALSE`.

- theta:

  A vector of standardized effect sizes (theta values), default is a
  sequence from -1 to 1.

- nMax:

  The maximum sample size. Must be a positive integer of length 1.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

## Value

Returns a [`data.frame`](https://rdrr.io/r/base/data.frame.html).

## Details

Coerces the design set to a data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
designSet <- getDesignSet(design = getDesignGroupSequential(), alpha = c(0.01, 0.05))
as.data.frame(designSet)
} # }
```
