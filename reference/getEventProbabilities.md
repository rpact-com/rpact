# Get Event Probabilities

Returns the event probabilities for specified parameters at given time
vector.

## Usage

``` r
getEventProbabilities(
  time,
  ...,
  accrualTime = c(0, 12),
  accrualIntensity = 0.1,
  accrualIntensityType = c("auto", "absolute", "relative"),
  kappa = 1,
  piecewiseSurvivalTime = NA_real_,
  lambda2 = NA_real_,
  lambda1 = NA_real_,
  allocationRatioPlanned = 1,
  hazardRatio = NA_real_,
  dropoutRate1 = 0,
  dropoutRate2 = 0,
  dropoutTime = 12,
  maxNumberOfSubjects = NA_real_
)
```

## Arguments

- time:

  A numeric vector with time values.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- accrualTime:

  The assumed accrual time intervals for the study, default is
  `c(0, 12)` (for details see
  [`getAccrualTime()`](https://docs.rpact.org/reference/getAccrualTime.md)).

- accrualIntensity:

  A numeric vector of accrual intensities, default is the relative
  intensity `0.1` (for details see
  [`getAccrualTime()`](https://docs.rpact.org/reference/getAccrualTime.md)).

- accrualIntensityType:

  A character value specifying the accrual intensity input type. Must be
  one of `"auto"`, `"absolute"`, or `"relative"`; default is `"auto"`,
  i.e., if all values are \< 1 the type is `"relative"`, otherwise it is
  `"absolute"`.

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

- piecewiseSurvivalTime:

  A vector that specifies the time intervals for the piecewise
  definition of the exponential survival time cumulative distribution
  function  
  (for details see
  [`getPiecewiseSurvivalTime()`](https://docs.rpact.org/reference/getPiecewiseSurvivalTime.md)).

- lambda2:

  The assumed hazard rate in the reference group, there is no default.
  `lambda2` can also be used to define piecewise exponentially
  distributed survival times (see details). Must be a positive numeric
  of length 1.

- lambda1:

  The assumed hazard rate in the treatment group, there is no default.
  `lambda1` can also be used to define piecewise exponentially
  distributed survival times (see details). Must be a positive numeric
  of length 1.

- allocationRatioPlanned:

  The planned allocation ratio `n1 / n2` for a two treatment groups
  design, default is `1`. If `allocationRatioPlanned = 0` is entered,
  the optimal allocation ratio yielding the smallest overall sample size
  is determined.

- hazardRatio:

  The vector of hazard ratios under consideration. If the event or
  hazard rates in both treatment groups are defined, the hazard ratio
  needs not to be specified as it is calculated, there is no default.
  Must be a positive numeric of length 1.

- dropoutRate1:

  The assumed drop-out rate in the treatment group, default is `0`.

- dropoutRate2:

  The assumed drop-out rate in the control group, default is `0`.

- dropoutTime:

  The assumed time for drop-out rates in the control and the treatment
  group, default is `12`.

- maxNumberOfSubjects:

  If `maxNumberOfSubjects > 0` is specified, the end of accrual at
  specified `accrualIntensity` for the specified number of subjects is
  determined or `accrualIntensity` is calculated at fixed end of
  accrual.

## Value

Returns a
[`EventProbabilities`](https://docs.rpact.org/reference/EventProbabilities.md)
object. The following generics (R generic functions) are available for
this result object:

- [`names()`](https://docs.rpact.org/reference/names.FieldSet.md) to
  obtain the field names,

- [`print()`](https://docs.rpact.org/reference/print.FieldSet.md) to
  print the object,

- [`summary()`](https://docs.rpact.org/reference/summary.ParameterSet.md)
  to display a summary of the object,

- [`plot()`](https://docs.rpact.org/reference/plot.EventProbabilities.md)
  to plot the object,

- [`as.data.frame()`](https://docs.rpact.org/reference/as.data.frame.ParameterSet.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://docs.rpact.org/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

The function computes the overall event probabilities in a two treatment
groups design. For details of the parameters see
[`getSampleSizeSurvival()`](https://docs.rpact.org/reference/getSampleSizeSurvival.md).

## How to get help for generic functions

Click on the link of a generic in the list above to go directly to the
help documentation of the `rpact` specific implementation of the
generic. Note that you can use the R function
[`methods`](https://rdrr.io/r/utils/methods.html) to get all the methods
of a generic and to identify the object specific name of it, e.g., use
`methods("plot")` to get all the methods for the `plot` generic. There
you can find, e.g., `plot.AnalysisResults` and obtain the specific help
documentation linked above by typing
[`?plot.AnalysisResults`](https://docs.rpact.org/reference/plot.AnalysisResults.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate event probabilities for staggered subjects' entry, piecewisely defined
# survival time and hazards, and plot it.
timeVector <- seq(0, 100, 1)
y <- getEventProbabilities(timeVector, accrualTime = c(0, 20, 60), 
    accrualIntensity = c(5, 20), 
    piecewiseSurvivalTime = c(0, 20, 80),
    lambda2 = c(0.02, 0.06, 0.1), 
    hazardRatio = 2
)
plot(timeVector, y$cumulativeEventProbabilities, type = 'l')
} # }
```
