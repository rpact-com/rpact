# Get Lambda Step Function

Calculates the lambda step values for a given time vector.

## Usage

``` r
getLambdaStepFunction(timeValues, ..., piecewiseSurvivalTime, piecewiseLambda)
```

## Arguments

- timeValues:

  A numeric vector that specifies the time values for which the lambda
  step values shall be calculated.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- piecewiseSurvivalTime:

  A numeric vector that specifies the time intervals for the piecewise
  definition of the exponential survival time cumulative distribution
  function (see details).

- piecewiseLambda:

  A numeric vector that specifies the assumed hazard rate in the
  treatment group.

## Value

A numeric vector containing the lambda step values that corresponds to
the specified time values.

## Details

The first element of the vector `piecewiseSurvivalTime` must be equal to
`0`. This function is used for plotting of sample size survival results
(cf.,
[`plot`](https://rpact-com.github.io/rpact/reference/plot.TrialDesignPlan.md),
`type = 13` and `type = 14`).
