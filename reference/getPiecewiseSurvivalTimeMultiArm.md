# Piecewise Exponential Multi-Arm Survival Time

Creates a validated piecewise exponential survival specification for a
patient-wise multi-arm simulation.

## Usage

``` r
getPiecewiseSurvivalTimeMultiArm(
  piecewiseSurvivalTime = NULL,
  lambdaControl = NULL,
  hazardRatios = NULL,
  medianControl = NULL,
  medianTreatments = NULL
)
```

## Arguments

- piecewiseSurvivalTime:

  Numeric interval start times beginning with 0.

- lambdaControl:

  Positive control hazards, one per interval. Specify either
  `lambdaControl` or `medianControl`.

- hazardRatios:

  One active-arm-by-interval matrix, a named list of such matrices (one
  per situation), or an active-arm-by-interval-by-situation array. In
  the array, situation is the slowest-changing dimension. Specify either
  `hazardRatios` or `medianTreatments`.

- medianControl:

  Positive, finite exponential control medians, one per interval, as an
  alternative to `lambdaControl`. Values are not recycled.

- medianTreatments:

  Positive, finite exponential treatment medians, in the same matrix,
  list, or array format as `hazardRatios`, as an alternative to it.

## Value

A `PiecewiseSurvivalTimeMultiArm` object.

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
hazardRatios <- getPiecewiseHazardRatioArray(
    favorable = rbind(lowDose = c(0.9, 0.8), highDose = c(0.8, 0.6)),
    conservative = rbind(lowDose = c(1, 0.9), highDose = c(0.9, 0.75))
)
piecewiseTime <- getPiecewiseSurvivalTimeMultiArm(
    piecewiseSurvivalTime = c(0, 6),
    lambdaControl = c(0.08, 0.12),
    hazardRatios = hazardRatios
)
piecewiseTime
#> Piecewise exponential multi-arm survival time
#> 
#> 
#> Hazard ratios - favorable:
#>           interval
#> group      0 - <6 >=6
#>   lowDose     0.9 0.8
#>   highDose    0.8 0.6
#> 
#> Hazard ratios - conservative:
#>           interval
#> group      0 - <6  >=6
#>   lowDose     1.0 0.90
#>   highDose    0.9 0.75
#> Control hazards (common to all treatment arms):
#>         0 - <6  >=6
#> Control   0.08 0.12

getPiecewiseSurvivalTimeMultiArm(
    piecewiseSurvivalTime = c(0, 6),
    medianControl = c(12, 24),
    medianTreatments = rbind(lowDose = c(15, 30), highDose = c(18, 36))
)
#> Piecewise exponential multi-arm survival time
#> 
#> 
#> Treatment medians - Situation 1 (exponential per interval):
#>           interval
#> group      0 - <6 >=6
#>   lowDose      15  30
#>   highDose     18  36
#> 
#> Hazard ratios - Situation 1:
#>           interval
#> group         0 - <6       >=6
#>   lowDose  0.8000000 0.8000000
#>   highDose 0.6666667 0.6666667
#> Control hazards (common to all treatment arms):
#>             0 - <6        >=6
#> Control 0.05776227 0.02888113
#> 
#> Control medians (exponential per interval):
#>         0 - <6 >=6
#> Control     12  24
```
