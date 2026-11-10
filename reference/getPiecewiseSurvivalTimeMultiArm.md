# Piecewise Exponential Multi-Arm Survival Time

Creates a validated piecewise exponential survival specification for a
patient-wise multi-arm simulation.

## Usage

``` r
getPiecewiseSurvivalTimeMultiArm(
  piecewiseSurvivalTime = NULL,
  lambdaControl = NULL,
  hazardRatio = NULL,
  medianControl = NULL,
  medianTreatments = NULL
)
```

## Arguments

- piecewiseSurvivalTime:

  Numeric interval start times beginning with 0.

- lambdaControl:

  Non-negative, finite control hazards, one per interval. Specify either
  `lambdaControl` or `medianControl`.

- hazardRatio:

  One active-arm-by-interval matrix, a named list of such matrices (one
  per situation), or an active-arm-by-interval-by-situation array. In
  the array, situation is the slowest-changing dimension. Specify either
  `hazardRatio` or `medianTreatments`.

- medianControl:

  Positive, finite exponential control medians, one per interval, as an
  alternative to `lambdaControl`. Values are not recycled.

- medianTreatments:

  Positive, finite exponential treatment medians, in the same matrix,
  list, or array format as `hazardRatio`, as an alternative to it.

## Value

A `PiecewiseSurvivalTimeMultiArm` object.

## Details

At least two finite, strictly increasing interval starts are required.
The last interval is open-ended. Hazards must be finite and
non-negative; zero denotes an interval without events. Hazard ratios
must be finite and strictly positive. Group and situation names must be
unique and non-empty. Unnamed groups are positional; named groups must
agree in order. Interval columns are positional and receive labels
derived from the interval starts. Only piecewise exponential event times
are supported; simulation requires `kappa = 1`. The returned time
definition inherits from `ParameterSet` and supports
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
hazardRatio <- getPiecewiseHazardRatioArray(
    favorable = rbind(lowDose = c(0.9, 0.8), highDose = c(0.8, 0.6)),
    conservative = rbind(lowDose = c(1, 0.9), highDose = c(0.9, 0.75))
)
piecewiseTime <- getPiecewiseSurvivalTimeMultiArm(
    piecewiseSurvivalTime = c(0, 6),
    lambdaControl = c(0.08, 0.12),
    hazardRatio = hazardRatio
)
piecewiseTime
#> Piecewise exponential multi-arm survival time
#> 
#> Control hazards (common to all comparisons):
#>         Group
#> Interval Control
#>   0 - <6 0.080  
#>   >=6    0.120  
#> 
#> Details:
#> 
#> User defined parameters:
#>   Piecewise survival times : 0.00, 6.00 
#>   Control hazards (common) : 0.080, 0.120 
#> 
#> Hazard ratios - favorable:
#>         Group
#> Interval lowDose highDose
#>   0 - <6 0.900   0.800   
#>   >=6    0.800   0.600   
#> 
#> Hazard ratios - conservative:
#>         Group
#> Interval lowDose highDose
#>   0 - <6 1.000   0.900   
#>   >=6    0.900   0.750   
#> 
#> Default parameters: not available
#> 
#> Generated parameters:
#>   Group type               : multi-arm 
#> 

getPiecewiseSurvivalTimeMultiArm(
    piecewiseSurvivalTime = c(0, 6),
    medianControl = c(12, 24),
    medianTreatments = rbind(lowDose = c(15, 30), highDose = c(18, 36))
)
#> Piecewise exponential multi-arm survival time
#> 
#> Control hazards (common to all comparisons):
#>         Group
#> Interval Control
#>   0 - <6 0.0578 
#>   >=6    0.0289 
#> 
#> Details:
#> 
#> User defined parameters:
#>   Piecewise survival times                 : 0.00, 6.00 
#>   Control medians (exponential per interval) : 12.000, 24.000 
#> 
#> Treatment medians - Situation 1 (exponential per interval):
#>         Group
#> Interval lowDose highDose
#>   0 - <6 15.000  18.000  
#>   >=6    30.000  36.000  
#> 
#> Default parameters: not available
#> 
#> Generated parameters:
#>   Control hazards (common)                 : 0.0578, 0.0289 
#> 
#> Hazard ratios - Situation 1:
#>         Group
#> Interval lowDose highDose
#>   0 - <6 0.800   0.667   
#>   >=6    0.800   0.667   
#>   Group type                               : multi-arm 
#> 
```
