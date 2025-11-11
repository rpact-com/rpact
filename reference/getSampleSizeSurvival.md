# Get Sample Size Survival

Returns the sample size for testing the hazard ratio in a two treatment
groups survival design.

## Usage

``` r
getSampleSizeSurvival(
  design = NULL,
  ...,
  typeOfComputation = c("Schoenfeld", "Freedman", "HsiehFreedman"),
  thetaH0 = 1,
  pi1 = NA_real_,
  pi2 = NA_real_,
  lambda1 = NA_real_,
  lambda2 = NA_real_,
  median1 = NA_real_,
  median2 = NA_real_,
  kappa = 1,
  hazardRatio = NA_real_,
  piecewiseSurvivalTime = NA_real_,
  allocationRatioPlanned = NA_real_,
  eventTime = 12,
  accrualTime = c(0, 12),
  accrualIntensity = 0.1,
  accrualIntensityType = c("auto", "absolute", "relative"),
  followUpTime = NA_real_,
  maxNumberOfSubjects = NA_real_,
  dropoutRate1 = 0,
  dropoutRate2 = 0,
  dropoutTime = 12
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

- typeOfComputation:

  Three options are available: `"Schoenfeld"`, `"Freedman"`,
  `"HsiehFreedman"`, the default is `"Schoenfeld"`. For details, see
  Hsieh (Statistics in Medicine, 1992). For non-inferiority testing
  (i.e., `thetaH0 != 1`), only Schoenfeld's formula can be used.

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

- pi1:

  A numeric value or vector that represents the assumed event rate in
  the treatment group, default is `seq(0.2, 0.5, 0.1)` (power
  calculations and simulations) or `seq(0.4, 0.6, 0.1)` (sample size
  calculations).

- pi2:

  A numeric value that represents the assumed event rate in the control
  group, default is `0.2`.

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

- median1:

  The assumed median survival time in the treatment group, there is no
  default.

- median2:

  The assumed median survival time in the reference group, there is no
  default. Must be a positive numeric of length 1.

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

- hazardRatio:

  The vector of hazard ratios under consideration. If the event or
  hazard rates in both treatment groups are defined, the hazard ratio
  needs not to be specified as it is calculated, there is no default.
  Must be a positive numeric of length 1.

- piecewiseSurvivalTime:

  A vector that specifies the time intervals for the piecewise
  definition of the exponential survival time cumulative distribution
  function  
  (for details see
  [`getPiecewiseSurvivalTime()`](https://rpact-com.github.io/rpact/reference/getPiecewiseSurvivalTime.md)).

- allocationRatioPlanned:

  The planned allocation ratio `n1 / n2` for a two treatment groups
  design, default is `1`. If `allocationRatioPlanned = 0` is entered,
  the optimal allocation ratio yielding the smallest overall sample size
  is determined.

- eventTime:

  The assumed time under which the event rates are calculated, default
  is `12`.

- accrualTime:

  The assumed accrual time intervals for the study, default is
  `c(0, 12)` (for details see
  [`getAccrualTime()`](https://rpact-com.github.io/rpact/reference/getAccrualTime.md)).

- accrualIntensity:

  A numeric vector of accrual intensities, default is the relative
  intensity `0.1` (for details see
  [`getAccrualTime()`](https://rpact-com.github.io/rpact/reference/getAccrualTime.md)).

- accrualIntensityType:

  A character value specifying the accrual intensity input type. Must be
  one of `"auto"`, `"absolute"`, or `"relative"`; default is `"auto"`,
  i.e., if all values are \< 1 the type is `"relative"`, otherwise it is
  `"absolute"`.

- followUpTime:

  The assumed (additional) follow-up time for the study, default is `6`.
  The total study duration is `accrualTime + followUpTime`.

- maxNumberOfSubjects:

  If `maxNumberOfSubjects > 0` is specified, the follow-up time for the
  required number of events is determined.

- dropoutRate1:

  The assumed drop-out rate in the treatment group, default is `0`.

- dropoutRate2:

  The assumed drop-out rate in the control group, default is `0`.

- dropoutTime:

  The assumed time for drop-out rates in the control and the treatment
  group, default is `12`.

## Value

Returns a
[`TrialDesignPlan`](https://rpact-com.github.io/rpact/reference/TrialDesignPlan.md)
object. The following generics (R generic functions) are available for
this result object:

- [`names()`](https://rpact-com.github.io/rpact/reference/names.FieldSet.md)
  to obtain the field names,

- [`print()`](https://rpact-com.github.io/rpact/reference/print.FieldSet.md)
  to print the object,

- [`summary()`](https://rpact-com.github.io/rpact/reference/summary.TrialDesignSet.md)
  to display a summary of the object,

- [`plot()`](https://rpact-com.github.io/rpact/reference/plot.TrialDesignPlan.md)
  to plot the object,

- [`as.data.frame()`](https://rpact-com.github.io/rpact/reference/as.data.frame.TrialDesignPlan.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://rpact-com.github.io/rpact/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

At given design the function calculates the number of events and an
estimate for the necessary number of subjects for testing the hazard
ratio in a survival design. It also calculates the time when the
required events are expected under the given assumptions (exponentially,
piecewise exponentially, or Weibull distributed survival times and
constant or non-constant piecewise accrual). Additionally, an allocation
ratio = `n1 / n2` can be specified where `n1` and `n2` are the number of
subjects in the two treatment groups.

Optional argument `accountForObservationTimes`: if
`accountForObservationTimes = TRUE`, the number of subjects is
calculated assuming specific accrual and follow-up time, default is
`TRUE`.

The formula of Kim & Tsiatis (Biometrics, 1990) is used to calculate the
expected number of events under the alternative (see also Lakatos & Lan,
Statistics in Medicine, 1992). These formulas are generalized to
piecewise survival times and non-constant piecewise accrual over time.  

Optional argument `accountForObservationTimes`: if
`accountForObservationTimes = FALSE`, only the event rates are used for
the calculation of the maximum number of subjects.

## Piecewise survival time

The first element of the vector `piecewiseSurvivalTime` must be equal to
`0`. `piecewiseSurvivalTime` can also be a list that combines the
definition of the time intervals and hazard rates in the reference
group. The definition of the survival time in the treatment group is
obtained by the specification of the hazard ratio (see examples for
details).

## Staggered patient entry

`accrualTime` is the time period of subjects' accrual in a study. It can
be a value that defines the end of accrual or a vector. In this case,
`accrualTime` can be used to define a non-constant accrual over time.
For this, `accrualTime` is a vector that defines the accrual intervals.
The first element of `accrualTime` must be equal to `0` and,
additionally, `accrualIntensity` needs to be specified.
`accrualIntensity` itself is a value or a vector (depending on the
length of `accrualTime`) that defines the intensity how subjects enter
the trial in the intervals defined through `accrualTime`.

`accrualTime` can also be a list that combines the definition of the
accrual time and accrual intensity (see below and examples for details).

If the length of `accrualTime` and the length of `accrualIntensity` are
the same (i.e., the end of accrual is undefined),
`maxNumberOfSubjects > 0` needs to be specified and the end of accrual
is calculated. In that case, `accrualIntensity` is the number of
subjects per time unit, i.e., the absolute accrual intensity.

If the length of `accrualTime` equals the length of
`accrualIntensity - 1` (i.e., the end of accrual is defined),
`maxNumberOfSubjects` is calculated if the absolute accrual intensity is
given. If all elements in `accrualIntensity` are smaller than 1,
`accrualIntensity` defines the *relative* intensity how subjects enter
the trial. For example, `accrualIntensity = c(0.1, 0.2)` specifies that
in the second accrual interval the intensity is doubled as compared to
the first accrual interval. The actual (absolute) accrual intensity is
calculated for the calculated or given `maxNumberOfSubjects`. Note that
the default is `accrualIntensity = 0.1` meaning that the *absolute*
accrual intensity will be calculated.

## How to get help for generic functions

Click on the link of a generic in the list above to go directly to the
help documentation of the `rpact` specific implementation of the
generic. Note that you can use the R function
[`methods`](https://rdrr.io/r/utils/methods.html) to get all the methods
of a generic and to identify the object specific name of it, e.g., use
`methods("plot")` to get all the methods for the `plot` generic. There
you can find, e.g., `plot.AnalysisResults` and obtain the specific help
documentation linked above by typing
[`?plot.AnalysisResults`](https://rpact-com.github.io/rpact/reference/plot.AnalysisResults.md).

## See also

Other sample size functions:
[`getSampleSizeCounts()`](https://rpact-com.github.io/rpact/reference/getSampleSizeCounts.md),
[`getSampleSizeMeans()`](https://rpact-com.github.io/rpact/reference/getSampleSizeMeans.md),
[`getSampleSizeRates()`](https://rpact-com.github.io/rpact/reference/getSampleSizeRates.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Fixed sample size trial with median survival 20 vs. 30 months in treatment and 
# reference group, respectively, alpha = 0.05 (two-sided), and power 1 - beta = 90%.
# 20 subjects will be recruited per month up to 400 subjects, i.e., accrual time 
# is 20 months.  
getSampleSizeSurvival(alpha = 0.05, sided = 2, beta = 0.1, lambda1 = log(2) / 20, 
    lambda2 = log(2) / 30, accrualTime = c(0,20), accrualIntensity = 20)

# Fixed sample size with minimum required definitions, pi1 = c(0.4,0.5,0.6) and 
# pi2 = 0.2 at event time 12, accrual time 12 and follow-up time 6 as default, 
# only alpha = 0.01 is specified  
getSampleSizeSurvival(alpha = 0.01)

# Four stage O'Brien & Fleming group sequential design with minimum required 
# definitions, pi1 = c(0.4,0.5,0.6) and pi2 = 0.2 at event time 12, 
# accrual time 12 and follow-up time 6 as default  
getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 4))

# For fixed sample design, determine necessary accrual time if 200 subjects and 
# 30 subjects per time unit can be recruited 
getSampleSizeSurvival(accrualTime = c(0), accrualIntensity = c(30), 
    maxNumberOfSubjects = 200)

# Determine necessary accrual time if 200 subjects and if the first 6 time units 
# 20 subjects per time unit can be recruited, then 30 subjects per time unit 
getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(20, 30), 
    maxNumberOfSubjects = 200)

# Determine maximum number of Subjects if the first 6 time units 20 subjects 
# per time unit can be recruited, and after 10 time units 30 subjects per time unit
getSampleSizeSurvival(accrualTime = c(0, 6, 10), accrualIntensity = c(20, 30))

# Specify accrual time as a list
at <- list(
    "0 - <6"  = 20,
    "6 - Inf" = 30)
getSampleSizeSurvival(accrualTime = at, maxNumberOfSubjects = 200)

# Specify accrual time as a list, if maximum number of subjects need to be calculated
at <- list(
    "0 - <6"   = 20,
    "6 - <=10" = 30)
getSampleSizeSurvival(accrualTime = at)

# Specify effect size for a two-stage group design with O'Brien & Fleming boundaries
# Effect size is based on event rates at specified event time 
# needs to be specified because it should be shown that hazard ratio < 1
getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), 
    pi1 = 0.2, pi2 = 0.3, eventTime = 24)

# Effect size is based on event rate at specified event 
# time for the reference group and hazard ratio 
getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), 
    hazardRatio = 0.5, pi2 = 0.3, eventTime = 24)

# Effect size is based on hazard rate for the reference group and hazard ratio
getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), 
    hazardRatio = 0.5, lambda2 = 0.02) 

# Specification of piecewise exponential survival time and hazard ratios  
getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), 
    piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), 
    hazardRatio = c(1.5, 1.8, 2))

# Specification of piecewise exponential survival time as a list and hazard ratios 
pws <- list(
    "0 - <5"  = 0.01,
    "5 - <10" = 0.02,
    ">=10"    = 0.04)
getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), 
    piecewiseSurvivalTime = pws, hazardRatio = c(1.5, 1.8, 2))

# Specification of piecewise exponential survival time for both treatment arms
getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), 
    piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), 
    lambda1 = c(0.015, 0.03, 0.06))

# Specification of piecewise exponential survival time as a list
pws <- list(
    "0 - <5"  = 0.01,
    "5 - <10" = 0.02,
    ">=10"    = 0.04)
getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), 
    piecewiseSurvivalTime = pws, hazardRatio = c(1.5, 1.8, 2))

# Specify effect size based on median survival times
getSampleSizeSurvival(median1 = 5, median2 = 3)

# Specify effect size based on median survival times of Weibull distribtion with 
# kappa = 2
getSampleSizeSurvival(median1 = 5, median2 = 3, kappa = 2)

# Identify minimal and maximal required subjects to 
# reach the required events in spite of dropouts
getSampleSizeSurvival(accrualTime = c(0, 18), accrualIntensity = c(20, 30), 
    lambda2 = 0.4, lambda1 = 0.3, followUpTime = Inf, dropoutRate1 = 0.001, 
    dropoutRate2 = 0.005)
getSampleSizeSurvival(accrualTime = c(0, 18), accrualIntensity = c(20, 30), 
    lambda2 = 0.4, lambda1 = 0.3, followUpTime = 0, dropoutRate1 = 0.001, 
    dropoutRate2 = 0.005)
} # }
```
