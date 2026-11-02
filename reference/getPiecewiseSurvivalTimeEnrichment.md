# Piecewise Exponential Enrichment Survival Time

Creates a validated piecewise exponential survival specification for a
patient-wise population-enrichment simulation.

## Usage

``` r
getPiecewiseSurvivalTimeEnrichment(
  piecewiseSurvivalTime = NULL,
  lambdaControls = NULL,
  hazardRatios = NULL,
  medianControls = NULL,
  medianTreatments = NULL
)
```

## Arguments

- piecewiseSurvivalTime:

  Numeric interval start times beginning with 0.

- lambdaControls:

  Positive subset-by-interval control-hazard matrix. If both control
  hazards and hazard ratios have row names, their names and order must
  agree. Specify either `lambdaControls` or `medianControls`.

- hazardRatios:

  One subset-by-interval matrix, a named list of such matrices (one per
  situation), or a subset-by-interval-by-situation array. In the array,
  situation is the slowest-changing dimension. Specify either
  `hazardRatios` or `medianTreatments`.

- medianControls:

  Positive, finite exponential control medians in a subset-by-interval
  matrix, as an alternative to `lambdaControls`. Names and dimensions
  follow the same rules as for `lambdaControls`.

- medianTreatments:

  Positive, finite exponential treatment medians, in the same matrix,
  list, or array format as `hazardRatios`, as an alternative to it.

## Value

A `PiecewiseSurvivalTimeEnrichment` object.

## Details

At least two finite, strictly increasing interval starts are required.
The last interval is open-ended. Hazards must be finite and strictly
positive. Group and situation names must be unique and non-empty.
Unnamed groups are positional; named groups must agree in order.
Interval columns are positional and receive labels derived from the
interval starts. Only piecewise exponential event times are supported;
simulation requires `kappa = 1`. The returned time definition inherits
from `ParameterSet` and supports
[`names()`](https://rdrr.io/r/base/names.html),
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html),
[`as.matrix()`](https://rdrr.io/r/base/matrix.html), and
[`getObjectRCode()`](https://docs.rpact.org/reference/getObjectRCode.md).
Hazard ratios specify the data-generating distribution, rather than
separate interval-specific analysis estimands. Median inputs
parameterize each interval's exponential hazard through
`getLambdaByMedian(median, kappa = 1)`. They are not the actual medians
of the complete piecewise survival distributions. The original interval
medians are retained as user-defined parameters; hazards and hazard
ratios calculated from them are marked as derived. No automatic
recycling across intervals or groups is performed.

## Examples

``` r
piecewiseTime <- getPiecewiseSurvivalTimeEnrichment(
    piecewiseSurvivalTime = c(0, 6),
    lambdaControls = rbind(markerPositive = c(0.08, 0.12),
        markerNegative = c(0.05, 0.09)),
    hazardRatios = list(
        favorable = rbind(markerPositive = c(0.65, 0.55),
            markerNegative = c(0.9, 0.8)),
        conservative = rbind(markerPositive = c(0.8, 0.7),
            markerNegative = c(1, 0.9))
    )
)
as.data.frame(piecewiseTime)
#>            group interval    situation startTime endTime lambdaControl
#> 1 markerPositive   0 - <6    favorable         0       6          0.08
#> 2 markerNegative   0 - <6    favorable         0       6          0.05
#> 3 markerPositive      >=6    favorable         6     Inf          0.12
#> 4 markerNegative      >=6    favorable         6     Inf          0.09
#> 5 markerPositive   0 - <6 conservative         0       6          0.08
#> 6 markerNegative   0 - <6 conservative         0       6          0.05
#> 7 markerPositive      >=6 conservative         6     Inf          0.12
#> 8 markerNegative      >=6 conservative         6     Inf          0.09
#>   hazardRatio lambdaActive
#> 1        0.65        0.052
#> 2        0.90        0.045
#> 3        0.55        0.066
#> 4        0.80        0.072
#> 5        0.80        0.064
#> 6        1.00        0.050
#> 7        0.70        0.084
#> 8        0.90        0.081

getPiecewiseSurvivalTimeEnrichment(
    piecewiseSurvivalTime = c(0, 6),
    medianControls = rbind(markerPositive = c(12, 24), markerNegative = c(18, 30)),
    medianTreatments = rbind(markerPositive = c(18, 36), markerNegative = c(20, 32))
)
#> Piecewise exponential enrichment survival time
#> 
#> Control hazards:
#>         Group
#> Interval markerPositive markerNegative
#>   0 - <6 0.0578         0.0385        
#>   >=6    0.0289         0.0231        
#> 
#> Details:
#> 
#> User defined parameters:
#>   Piecewise survival times : 0.00, 6.00 
#> 
#> Control medians (exponential per interval):
#>         Group
#> Interval markerPositive markerNegative
#>   0 - <6 12.000         18.000        
#>   >=6    24.000         30.000        
#> 
#> Treatment medians - Situation 1 (exponential per interval):
#>         Group
#> Interval markerPositive markerNegative
#>   0 - <6 18.000         20.000        
#>   >=6    36.000         32.000        
#> 
#> Default parameters: not available
#> 
#> Generated parameters:
#> 
#> Control hazards:
#>         Group
#> Interval markerPositive markerNegative
#>   0 - <6 0.0578         0.0385        
#>   >=6    0.0289         0.0231        
#> 
#> Hazard ratios - Situation 1:
#>         Group
#> Interval markerPositive markerNegative
#>   0 - <6 0.667          0.900         
#>   >=6    0.667          0.938         
#>   Group type               : enrichment 
#> 
```
