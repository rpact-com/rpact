# Piecewise Exponential Enrichment Survival Time

Creates a validated piecewise exponential survival specification for a
patient-wise population-enrichment simulation.

## Usage

``` r
getPiecewiseEnrichmentSurvivalTime(
  piecewiseSurvivalTime,
  lambdaControls,
  hazardRatios
)
```

## Arguments

- piecewiseSurvivalTime:

  Numeric interval start times beginning with 0.

- lambdaControls:

  Positive subset-by-interval control-hazard matrix. If both control
  hazards and hazard ratios have row names, their names and order must
  agree.

- hazardRatios:

  One subset-by-interval matrix, a named list of such matrices (one per
  situation), or a subset-by-interval-by-situation array. In the array,
  situation is the slowest-changing dimension.

## Value

A `PiecewiseEnrichmentSurvivalTime` object.

## Examples

``` r
piecewiseTime <- getPiecewiseEnrichmentSurvivalTime(
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
```
