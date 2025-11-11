# The Piecewise Exponential Distribution

Distribution function, quantile function and random number generation
for the piecewise exponential distribution.

## Usage

``` r
getPiecewiseExponentialDistribution(
  time,
  ...,
  piecewiseSurvivalTime = NA_real_,
  piecewiseLambda = NA_real_,
  kappa = 1
)

ppwexp(t, ..., s = NA_real_, lambda = NA_real_, kappa = 1)

getPiecewiseExponentialQuantile(
  quantile,
  ...,
  piecewiseSurvivalTime = NA_real_,
  piecewiseLambda = NA_real_,
  kappa = 1
)

qpwexp(q, ..., s = NA_real_, lambda = NA_real_, kappa = 1)

getPiecewiseExponentialRandomNumbers(
  n,
  ...,
  piecewiseSurvivalTime = NA_real_,
  piecewiseLambda = NA_real_,
  kappa = 1
)

rpwexp(n, ..., s = NA_real_, lambda = NA_real_, kappa = 1)
```

## Arguments

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- kappa:

  A numeric value \> 0. A `kappa != 1` will be used for the
  specification of the shape of the Weibull distribution. Default is
  `1`, i.e., the exponential survival distribution is used instead of
  the Weibull distribution. Note that the Weibull distribution cannot be
  used for the piecewise definition of the survival time distribution,
  i.e., only `piecewiselambda` (as a single value) and `kappa` can be
  specified. This function is equivalent to
  `pweibull(t, shape = kappa, scale = 1 / lambda)` of the `stats`
  package, i.e., the scale parameter is `1 / 'hazard rate'`.  
  For example,
  `getPiecewiseExponentialDistribution(time = 130, piecewiseLambda = 0.01, kappa = 4.2)`
  and `pweibull(q = 130, shape = 4.2, scale = 1 / 0.01)` provide the
  same result.

- t, time:

  Vector of time values.

- s, piecewiseSurvivalTime:

  Vector of start times defining the "time pieces".

- lambda, piecewiseLambda:

  Vector of lambda values (hazard rates) corresponding to the start
  times.

- q, quantile:

  Vector of quantiles.

- n:

  Number of observations.

## Value

A [`numeric`](https://rdrr.io/r/base/numeric.html) value or vector will
be returned.

## Details

`getPiecewiseExponentialDistribution()` (short: `ppwexp()`),
`getPiecewiseExponentialQuantile()` (short: `qpwexp()`), and
`getPiecewiseExponentialRandomNumbers()` (short: `rpwexp()`) provide
probabilities, quantiles, and random numbers according to a piecewise
exponential or a Weibull distribution. The piecewise definition is
performed through a vector of starting times (`piecewiseSurvivalTime`)
and a vector of hazard rates (`piecewiseLambda`). You can also use a
list that defines the starting times and piecewise lambdas together and
define piecewiseSurvivalTime as this list. The list needs to have the
form, e.g., piecewiseSurvivalTime \<- list( "0 - \<6" = 0.025, "6 - \<9"
= 0.04, "9 - \<15" = 0.015, "\>=15" = 0.007) . For the Weibull case, you
can also specify a shape parameter kappa in order to calculate
probabilities, quantiles, or random numbers. In this case, no piecewise
definition is possible, i.e., only piecewiseLambda (as a single value)
and kappa need to be specified.

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate probabilties for a range of time values for a
# piecewise exponential distribution with hazard rates
# 0.025, 0.04, 0.015, and 0.007 in the intervals
# [0, 6), [6, 9), [9, 15), [15, Inf), respectively,
# and re-return the time values:
piecewiseSurvivalTime <- list(
    "0 - <6"   = 0.025,
    "6 - <9"   = 0.04,
    "9 - <15"  = 0.015,
    ">=15"     = 0.01
)
y <- getPiecewiseExponentialDistribution(seq(0, 150, 15),
    piecewiseSurvivalTime = piecewiseSurvivalTime
)
getPiecewiseExponentialQuantile(y,
    piecewiseSurvivalTime = piecewiseSurvivalTime
)
} # }
```
