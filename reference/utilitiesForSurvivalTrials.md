# Survival Helper Functions for Conversion of Pi, Lambda, Median

Functions to convert pi, lambda and median values into each other.

## Usage

``` r
getLambdaByPi(piValue, eventTime = 12, kappa = 1)

getLambdaByMedian(median, kappa = 1)

getHazardRatioByPi(pi1, pi2, eventTime = 12, kappa = 1)

getPiByLambda(lambda, eventTime = 12, kappa = 1)

getPiByMedian(median, eventTime = 12, kappa = 1)

getMedianByLambda(lambda, kappa = 1)

getMedianByPi(piValue, eventTime = 12, kappa = 1)
```

## Arguments

- piValue, pi1, pi2, lambda, median:

  Value that shall be converted.

- eventTime:

  The assumed time under which the event rates are calculated, default
  is `12`.

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

## Value

Returns a [`numeric`](https://rdrr.io/r/base/numeric.html) value or
vector will be returned.

## Details

Can be used, e.g., to convert median values into pi or lambda values for
usage in
[`getSampleSizeSurvival()`](https://docs.rpact.org/reference/getSampleSizeSurvival.md)
or
[`getPowerSurvival()`](https://docs.rpact.org/reference/getPowerSurvival.md).
