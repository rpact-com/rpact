# Create a quantitative fixed-design decision rule

getDesignQuantitative constructs a decision-rule object for a fixed
(non-adaptive) trial with a quantitative endpoint. The returned object
contains the target value (TV), a lower reference value (LRV) and two
percentiles that define the decision boundaries used by the graphicalQTD
methods.

## Usage

``` r
getDesignQuantitative(
  targetValue,
  lowerReferenceValue,
  upperPercentile = 0.9,
  lowerPercentile = 0.2
)
```

## Arguments

- targetValue:

  Numeric scalar. The target value (TV) for the endpoint.

- lowerReferenceValue:

  Numeric scalar. The lower reference value (LRV).

- upperPercentile:

  Numeric scalar in (0, 1). The upper percentile used to define the
  upper decision boundary. Default is 0.9.

- lowerPercentile:

  Numeric scalar in (0, upperPercentile). The lower percentile used to
  define the lower decision boundary. Default is 0.2.

## Value

An object of class `TrialDesignPlanQuantitative` containing the supplied
design parameters.

## Details

The function performs basic input validation and returns an object of
class `TrialDesignPlanQuantitative`. This object is used by helper
functions that compute operating characteristics and make decisions for
fixed designs with normally approximated endpoints (see
`getOperatingCharacteristicsMeans`).

## Examples

``` r
# Basic usage with default percentiles
decisionRule <- getDesignQuantitative(
    targetValue = 100,
    lowerReferenceValue = 0
)
decisionRule
#> Design parameters and output of quantitative trial design (decision rule):
#> 
#> User defined parameters:
#>   Target value          : 100 
#>   Lower reference value : 0 
#> 
#> Default parameters:
#>   Upper percentile      : 0.9000 
#>   Lower percentile      : 0.2000 
#> 

# Custom percentiles
decisionRule2 <- getDesignQuantitative(
    targetValue = 5.5,
    lowerReferenceValue = 0,
    upperPercentile = 0.95,
    lowerPercentile = 0.1
)
decisionRule2
#> Design parameters and output of quantitative trial design (decision rule):
#> 
#> User defined parameters:
#>   Target value          : 5.5 
#>   Lower reference value : 0 
#>   Upper percentile      : 0.9500 
#>   Lower percentile      : 0.1000 
#> 
#> Default parameters: not available
#> 
```
