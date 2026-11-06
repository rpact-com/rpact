# TrialDesignPlanQuantitative R6 class

An R6 class that represents a decision-rule (design) for a fixed
quantitative trial. The object stores the target value (TV), a lower
reference value (LRV), and two percentiles that define the decision
boundaries used by the QTD operating-characteristics and decision
helpers.

## Value

An R6 object of class `TrialDesignPlanQuantitative`.

## Fields

- `targetValue`:

  Numeric scalar. The target value (TV) used by the rule.

- `lowerReferenceValue`:

  Numeric scalar. The lower reference value (LRV).

- `upperPercentile`:

  Numeric scalar in (0,1). Upper percentile for the decision boundary.

- `lowerPercentile`:

  Numeric scalar in (0, upperPercentile). Lower percentile for the
  decision boundary.

## Examples

``` r
# Create a simple decision rule
rule <- getDesignQuantitative(targetValue = 2, lowerReferenceValue = 0)
rule$show()
#> Design parameters and output of quantitative trial design (decision rule):
#> 
#> User defined parameters:
#>   Target value          : 2 
#>   Lower reference value : 0 
#> 
#> Default parameters:
#>   Upper percentile      : 0.9000 
#>   Lower percentile      : 0.2000 
#> 
```
