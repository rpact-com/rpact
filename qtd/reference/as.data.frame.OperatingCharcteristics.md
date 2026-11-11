# Extract Quantitative Decision Probability Tables

Returns the stored probability table of frequentist or Bayesian
quantitative OCs.

## Usage

``` r
# S3 method for class 'OperatingCharcteristics'
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

  An OperatingCharcteristicsMeans or OperatingCharcteristicsRates
  object.

- row.names:

  Optional row names for the resulting data frame.

- optional:

  Reserved for compatibility with as.data.frame.

- ...:

  Additional arguments are checked for unknown names.

- niceColumnNamesEnabled:

  Replace column identifiers with available captions.

## Value

A data frame of sampling probabilities on the scale zero to one.
Assumptions, including priors, remain on the originating result object.
