# Describe Comparison Parameters

Formats comparison parameter metadata for console output, summaries, and
UI labels. Formatting does not alter the metadata or comparison data.

## Usage

``` r
getComparisonParameterDescriptions(
  parameterInfo,
  ...,
  digits = NA_integer_,
  showDetails = TRUE
)
```

## Arguments

- parameterInfo:

  A table returned by getComparisonParameterInfo(), or a row subset of
  such a table.

- ...:

  Further arguments. Currently unused.

- digits:

  Optional number of decimal places for displayed values.

- showDetails:

  Whether to include the inferred type and plotting role.

## Value

A character vector named by parameter names. Empty input returns
character(). Empty columns are described as empty, not as single-valued.

## Examples

``` r
info <- getComparisonParameterInfo(data.frame(effect = c(0.2, 0.3)))
getComparisonParameterDescriptions(info)
#>                                           effect 
#> "effect: numeric, continuous, binary (0.2, 0.3)" 
```
