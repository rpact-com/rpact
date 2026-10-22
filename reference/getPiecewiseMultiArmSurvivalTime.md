# Piecewise Exponential Multi-Arm Survival Time

Creates a validated piecewise exponential survival specification for a
patient-wise multi-arm simulation.

## Usage

``` r
getPiecewiseMultiArmSurvivalTime(
  piecewiseSurvivalTime,
  lambdaControl,
  hazardRatios
)
```

## Arguments

- piecewiseSurvivalTime:

  Numeric interval start times beginning with 0.

- lambdaControl:

  Positive control hazards, one per interval.

- hazardRatios:

  One active-arm-by-interval matrix, a named list of such matrices (one
  per situation), or an active-arm-by-interval-by-situation array. In
  the array, situation is the slowest-changing dimension.

## Value

A `PiecewiseMultiArmSurvivalTime` object.

## Examples

``` r
hazardRatios <- getPiecewiseHazardRatioArray(
    favorable = rbind(lowDose = c(0.9, 0.8), highDose = c(0.8, 0.6)),
    conservative = rbind(lowDose = c(1, 0.9), highDose = c(0.9, 0.75))
)
piecewiseTime <- getPiecewiseMultiArmSurvivalTime(
    piecewiseSurvivalTime = c(0, 6),
    lambdaControl = c(0.08, 0.12),
    hazardRatios = hazardRatios
)
piecewiseTime
#> Piecewise exponential multi-arm survival time
#> 
#> Control hazards (common to all treatment arms):
#>         0 - <6  >=6
#> Control   0.08 0.12
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
```
