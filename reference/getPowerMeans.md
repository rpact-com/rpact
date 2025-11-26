# Get Power Means

Returns the power, stopping probabilities, and expected sample size for
testing means in one or two samples at given maximum sample size.

## Usage

``` r
getPowerMeans(
  design = NULL,
  ...,
  groups = 2L,
  normalApproximation = FALSE,
  meanRatio = FALSE,
  thetaH0 = ifelse(meanRatio, 1, 0),
  alternative = seq(0, 1, 0.2),
  stDev = 1,
  directionUpper = NA,
  maxNumberOfSubjects = NA_real_,
  allocationRatioPlanned = NA_real_
)
```

## Arguments

- design:

  The trial design. If no trial design is specified, a fixed sample size
  design is used. In this case, Type I error rate `alpha`, Type II error
  rate `beta`, `twoSidedPower`, and `sided` can be directly entered as
  argument where necessary.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- groups:

  The number of treatment groups (1 or 2), default is `2`.

- normalApproximation:

  The type of computation of the p-values. If `TRUE`, the variance is
  assumed to be known, default is `FALSE`, i.e., the calculations are
  performed with the t distribution.

- meanRatio:

  If `TRUE`, the sample size for one-sided testing of H0:
  `mu1 / mu2 = thetaH0` is calculated, default is `FALSE`.

- thetaH0:

  The null hypothesis value, default is `0` for the normal and the
  binary case (testing means and rates, respectively), it is `1` for the
  survival case (testing the hazard ratio).  
    
  For non-inferiority designs, `thetaH0` is the non-inferiority bound.
  That is, in case of (one-sided) testing of

  - *means*: a value `!= 0` (or a value `!= 1` for testing the mean
    ratio) can be specified.

  - *rates*: a value `!= 0` (or a value `!= 1` for testing the risk
    ratio `pi1 / pi2`) can be specified.

  - *survival data*: a bound for testing H0:
    `hazard ratio = thetaH0 != 1` can be specified.

  - *count data*: a bound for testing H0:
    `lambda1 / lambda2 = thetaH0 != 1` can be specified.

  For testing a rate in one sample, a value `thetaH0` in (0, 1) has to
  be specified for defining the null hypothesis H0: `pi = thetaH0`.

- alternative:

  The alternative hypothesis value for testing means. This can be a
  vector of assumed alternatives, default is `seq(0, 1, 0.2)` (power
  calculations) or `seq(0.2, 1, 0.2)` (sample size calculations).

- stDev:

  The standard deviation under which the sample size or power
  calculation is performed, default is `1`. For two-armed trials, it is
  allowed to specify the standard deviations separately, i.e., as vector
  with two elements. If `meanRatio = TRUE` is specified, `stDev` defines
  the coefficient of variation `sigma / mu2`.

- directionUpper:

  Logical. Specifies the direction of the alternative, only applicable
  for one-sided testing; default is `TRUE` which means that larger
  values of the test statistics yield smaller p-values.

- maxNumberOfSubjects:

  `maxNumberOfSubjects > 0` needs to be specified for power calculations
  or calculation of necessary follow-up (count data). For two treatment
  arms, it is the maximum number of subjects for both treatment arms.

- allocationRatioPlanned:

  The planned allocation ratio `n1 / n2` for a two treatment groups
  design, default is `1`. For multi-arm designs, it is the allocation
  ratio relating the active arm(s) to the control. For simulating means
  and rates for a two treatment groups design, it can be a vector of
  length `kMax`, the number of stages. It can be a vector of length
  `kMax`, too, for multi-arm and enrichment designs. In these cases, a
  change of allocating subjects to treatment groups over the stages can
  be assessed. Note that internally `allocationRatioPlanned` is treated
  as a vector of length `kMax`, not a scalar.

## Value

Returns a
[`TrialDesignPlan`](https://docs.rpact.org/reference/TrialDesignPlan.md)
object. The following generics (R generic functions) are available for
this result object:

- [`names()`](https://docs.rpact.org/reference/names.FieldSet.md) to
  obtain the field names,

- [`print()`](https://docs.rpact.org/reference/print.FieldSet.md) to
  print the object,

- [`summary()`](https://docs.rpact.org/reference/summary.TrialDesignSet.md)
  to display a summary of the object,

- [`plot()`](https://docs.rpact.org/reference/plot.TrialDesignPlan.md)
  to plot the object,

- [`as.data.frame()`](https://docs.rpact.org/reference/as.data.frame.TrialDesignPlan.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://docs.rpact.org/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

At given design the function calculates the power, stopping
probabilities, and expected sample size for testing means at given
sample size. In a two treatment groups design, additionally, an
allocation ratio = `n1 / n2` can be specified where `n1` and `n2` are
the number of subjects in the two treatment groups. A null hypothesis
value thetaH0 != 0 for testing the difference of two means or
`thetaH0 != 1` for testing the ratio of two means can be specified. For
the specified sample size, critical bounds and stopping for futility
bounds are provided at the effect scale (mean, mean difference, or mean
ratio, respectively)

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

## See also

Other power functions:
[`getPowerCounts()`](https://docs.rpact.org/reference/getPowerCounts.md),
[`getPowerRates()`](https://docs.rpact.org/reference/getPowerRates.md),
[`getPowerSurvival()`](https://docs.rpact.org/reference/getPowerSurvival.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate the power, stopping probabilities, and expected sample size 
# for testing H0: mu1 - mu2 = 0 in a two-armed design against a range of 
# alternatives H1: mu1 - m2 = delta, delta = (0, 1, 2, 3, 4, 5), 
# standard deviation sigma = 8, maximum sample size N = 80 (both treatment 
# arms), and an allocation ratio n1/n2 = 2. The design is a three stage 
# O'Brien & Fleming design with non-binding futility bounds (-0.5, 0.5) 
# for the two interims. The computation takes into account that the t test 
# is used (normalApproximation = FALSE). 
getPowerMeans(getDesignGroupSequential(alpha = 0.025, 
    sided = 1, futilityBounds = c(-0.5, 0.5)), 
    groups = 2, alternative = c(0:5), stDev = 8,
    normalApproximation = FALSE, maxNumberOfSubjects = 80, 
    allocationRatioPlanned = 2)
} # }
```
