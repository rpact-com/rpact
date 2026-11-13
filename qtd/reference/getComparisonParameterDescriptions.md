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

  A comparison parameter metadata table, as available in
  `summary(comparison)$parameterInfo`, or a row subset of such a table.

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
comparison <- getComparison(getDesignFixed(), getDesignGroupSequential(kMax = 2))
info <- summary(comparison)$parameterInfo
getComparisonParameterDescriptions(info)
#>                                                                             objectType 
#> "objectType: character, factor, binary (TrialDesignFixed, TrialDesignGroupSequential)" 
#>                                                                               endpoint 
#>                                "endpoint: character, factor, single value (Undefined)" 
#>                                                                      calculationTarget 
#>                          "calculationTarget: character, factor, single value (Design)" 
#>                                                                                   kMax 
#>                                                 "kMax: integer, factor, binary (1, 2)" 
#>                                                                           typeOfDesign 
#>                                   "typeOfDesign: character, factor, single value (OF)" 
#>                                                                                  alpha 
#>                                     "alpha: numeric, continuous, single value (0.025)" 
#>                                                                                   beta 
#>                                        "beta: numeric, continuous, single value (0.2)" 
#>                                                                        bindingFutility 
#>                               "bindingFutility: logical, factor, single value (FALSE)" 
#>                                                                                  sided 
#>                                         "sided: integer, continuous, single value (1)" 
#>                                                                              tolerance 
#>                                 "tolerance: numeric, continuous, single value (1e-08)" 
#>                                                                             alphaSpent 
#>                   "alphaSpent: numeric, continuous, binary (0.002582893, 0.024999990)" 
#>                                                                         criticalValues 
#>                                                  "criticalValues: numeric, continuous" 
#>                                                                            stageLevels 
#>                                                     "stageLevels: numeric, continuous" 
#>                                                                       informationRates 
#>                             "informationRates: numeric, continuous, binary (0.5, 1.0)" 
#>                                                                         futilityBounds 
#>                             "futilityBounds: numeric, continuous, single value (-Inf)" 
```
