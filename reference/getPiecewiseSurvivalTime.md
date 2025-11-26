# Get Piecewise Survival Time

Returns a `PiecewiseSurvivalTime` object that contains the all relevant
parameters of an exponential survival time cumulative distribution
function. Use [`names`](https://rdrr.io/r/base/names.html) to obtain the
field names.

## Usage

``` r
getPiecewiseSurvivalTime(
  piecewiseSurvivalTime = NA_real_,
  ...,
  lambda1 = NA_real_,
  lambda2 = NA_real_,
  hazardRatio = NA_real_,
  pi1 = NA_real_,
  pi2 = NA_real_,
  median1 = NA_real_,
  median2 = NA_real_,
  eventTime = 12,
  kappa = 1,
  delayedResponseAllowed = FALSE
)
```

## Arguments

- piecewiseSurvivalTime:

  A vector that specifies the time intervals for the piecewise
  definition of the exponential survival time cumulative distribution
  function (see details).

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- lambda1:

  The assumed hazard rate in the treatment group, there is no default.
  `lambda1` can also be used to define piecewise exponentially
  distributed survival times (see details). Must be a positive numeric
  of length 1.

- lambda2:

  The assumed hazard rate in the reference group, there is no default.
  `lambda2` can also be used to define piecewise exponentially
  distributed survival times (see details). Must be a positive numeric
  of length 1.

- hazardRatio:

  The vector of hazard ratios under consideration. If the event or
  hazard rates in both treatment groups are defined, the hazard ratio
  needs not to be specified as it is calculated, there is no default.
  Must be a positive numeric of length 1.

- pi1:

  A numeric value or vector that represents the assumed event rate in
  the treatment group, default is `seq(0.2, 0.5, 0.1)` (power
  calculations and simulations) or `seq(0.4, 0.6, 0.1)` (sample size
  calculations).

- pi2:

  A numeric value that represents the assumed event rate in the control
  group, default is `0.2`.

- median1:

  The assumed median survival time in the treatment group, there is no
  default.

- median2:

  The assumed median survival time in the reference group, there is no
  default. Must be a positive numeric of length 1.

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

- delayedResponseAllowed:

  If `TRUE`, delayed response is allowed; otherwise it will be validated
  that the response is not delayed, default is `FALSE`.

## Value

Returns a
[`PiecewiseSurvivalTime`](https://docs.rpact.org/reference/PiecewiseSurvivalTime.md)
object. The following generics (R generic functions) are available for
this result object:

- [`names()`](https://docs.rpact.org/reference/names.FieldSet.md) to
  obtain the field names,

- [`print()`](https://docs.rpact.org/reference/print.FieldSet.md) to
  print the object,

- [`summary()`](https://docs.rpact.org/reference/summary.ParameterSet.md)
  to display a summary of the object,

- [`plot()`](https://docs.rpact.org/reference/plot.ParameterSet.md) to
  plot the object,

- [`as.data.frame()`](https://docs.rpact.org/reference/as.data.frame.ParameterSet.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://docs.rpact.org/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Piecewise survival time

The first element of the vector `piecewiseSurvivalTime` must be equal to
`0`. `piecewiseSurvivalTime` can also be a list that combines the
definition of the time intervals and hazard rates in the reference
group. The definition of the survival time in the treatment group is
obtained by the specification of the hazard ratio (see examples for
details).

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
getPiecewiseSurvivalTime(lambda2 = 0.5, hazardRatio = 0.8)

getPiecewiseSurvivalTime(lambda2 = 0.5, lambda1 = 0.4)

getPiecewiseSurvivalTime(pi2 = 0.5, hazardRatio = 0.8)

getPiecewiseSurvivalTime(pi2 = 0.5, pi1 = 0.4)

getPiecewiseSurvivalTime(pi1 = 0.3)

getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), lambda2 = 0.4)

getPiecewiseSurvivalTime(piecewiseSurvivalTime = c(0, 6, 9), 
    lambda2 = c(0.025, 0.04, 0.015), hazardRatio = 0.8)

getPiecewiseSurvivalTime(piecewiseSurvivalTime = c(0, 6, 9), 
    lambda2 = c(0.025, 0.04, 0.015), 
    lambda1 = c(0.025, 0.04, 0.015) * 0.8)

pwst <- getPiecewiseSurvivalTime(list(
    "0 - <6"   = 0.025, 
    "6 - <9"   = 0.04, 
    "9 - <15"  = 0.015, 
    "15 - <21" = 0.01, 
    ">=21"     = 0.007), hazardRatio = 0.75)
pwst

# The object created by getPiecewiseSurvivalTime() can be used directly in 
# getSampleSizeSurvival():
getSampleSizeSurvival(piecewiseSurvivalTime = pwst)

# The object created by getPiecewiseSurvivalTime() can be used directly in 
# getPowerSurvival():
getPowerSurvival(piecewiseSurvivalTime = pwst, directionUpper = FALSE, 
    maxNumberOfEvents = 40, maxNumberOfSubjects = 100)

# The object created by getPiecewiseSurvivalTime() can be used directly in 
# getSimulationSurvival():
getSimulationSurvival(piecewiseSurvivalTime = pwst, directionUpper = FALSE,
    plannedEvents = 40, maxNumberOfSubjects = 100)
} # }
 
```
