# Get Simulation Survival

Returns the analysis times, power, stopping probabilities, conditional
power, and expected sample size for testing the hazard ratio in a two
treatment groups survival design.

## Usage

``` r
getSimulationSurvival(
  design = NULL,
  ...,
  thetaH0 = 1,
  directionUpper = NA,
  pi1 = NA_real_,
  pi2 = NA_real_,
  lambda1 = NA_real_,
  lambda2 = NA_real_,
  median1 = NA_real_,
  median2 = NA_real_,
  hazardRatio = NA_real_,
  kappa = 1,
  piecewiseSurvivalTime = NA_real_,
  allocation1 = 1,
  allocation2 = 1,
  eventTime = 12,
  accrualTime = c(0, 12),
  accrualIntensity = 0.1,
  accrualIntensityType = c("auto", "absolute", "relative"),
  dropoutRate1 = 0,
  dropoutRate2 = 0,
  dropoutTime = 12,
  maxNumberOfSubjects = NA_real_,
  plannedEvents = NA_real_,
  minNumberOfEventsPerStage = NA_real_,
  maxNumberOfEventsPerStage = NA_real_,
  conditionalPower = NA_real_,
  thetaH1 = NA_real_,
  maxNumberOfIterations = 1000L,
  maxNumberOfRawDatasetsPerStage = 0,
  longTimeSimulationAllowed = FALSE,
  seed = NA_real_,
  calcEventsFunction = NULL,
  showStatistics = FALSE
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

- directionUpper:

  Logical. Specifies the direction of the alternative, only applicable
  for one-sided testing; default is `TRUE` which means that larger
  values of the test statistics yield smaller p-values.

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

- hazardRatio:

  The vector of hazard ratios under consideration. If the event or
  hazard rates in both treatment groups are defined, the hazard ratio
  needs not to be specified as it is calculated, there is no default.
  Must be a positive numeric of length 1.

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
  [`getPiecewiseSurvivalTime()`](https://rpact-com.github.io/rpact/reference/getPiecewiseSurvivalTime.md)).

- allocation1:

  The number how many subjects are assigned to treatment 1 in a
  subsequent order, default is `1`

- allocation2:

  The number how many subjects are assigned to treatment 2 in a
  subsequent order, default is `1`

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

- dropoutRate1:

  The assumed drop-out rate in the treatment group, default is `0`.

- dropoutRate2:

  The assumed drop-out rate in the control group, default is `0`.

- dropoutTime:

  The assumed time for drop-out rates in the control and the treatment
  group, default is `12`.

- maxNumberOfSubjects:

  `maxNumberOfSubjects > 0` needs to be specified. If accrual time and
  accrual intensity are specified, this will be calculated. Must be a
  positive integer of length 1.

- plannedEvents:

  `plannedEvents` is a numeric vector of length `kMax` (the number of
  stages of the design) that determines the number of cumulated
  (overall) events in survival designs when the interim stages are
  planned. For two treatment arms, it is the number of events for both
  treatment arms. For multi-arm designs, `plannedEvents` refers to the
  overall number of events for the selected arms plus control.

- minNumberOfEventsPerStage:

  When performing a data driven sample size recalculation, the numeric
  vector `minNumberOfEventsPerStage` with length kMax determines the
  minimum number of events per stage (i.e., not cumulated), the first
  element is not taken into account.

- maxNumberOfEventsPerStage:

  When performing a data driven sample size recalculation, the numeric
  vector `maxNumberOfEventsPerStage` with length `kMax` determines the
  maximum number of events per stage (i.e., not cumulated), the first
  element is not taken into account.

- conditionalPower:

  If `conditionalPower` together with `minNumberOfSubjectsPerStage` and
  `maxNumberOfSubjectsPerStage` (or `minNumberOfEventsPerStage` and
  `maxNumberOfEventsPerStage` for survival designs) is specified, a
  sample size recalculation based on the specified conditional power is
  performed. It is defined as the power for the subsequent stage given
  the current data. By default, the conditional power will be calculated
  under the observed effect size. Optionally, you can also specify
  `thetaH1` and `stDevH1` (for simulating means), `pi1H1` and `pi2H1`
  (for simulating rates), or `thetaH1` (for simulating hazard ratios) as
  parameters under which it is calculated and the sample size
  recalculation is performed.

- thetaH1:

  If specified, the value of the alternative under which the conditional
  power or sample size recalculation calculation is performed. Must be a
  numeric of length 1.

- maxNumberOfIterations:

  The number of simulation iterations, default is `1000`. Must be a
  positive integer of length 1.

- maxNumberOfRawDatasetsPerStage:

  The number of raw datasets per stage that shall be extracted and saved
  as [`data.frame`](https://rdrr.io/r/base/data.frame.html), default is
  `0`.
  [`getRawData()`](https://rpact-com.github.io/rpact/reference/getRawData.md)
  can be used to get the extracted raw data from the object.

- longTimeSimulationAllowed:

  Logical that indicates whether long time simulations that consumes
  more than 30 seconds are allowed or not, default is `FALSE`.

- seed:

  The seed to reproduce the simulation, default is a random seed.

- calcEventsFunction:

  Optionally, a function can be entered that defines the way of
  performing the sample size recalculation. By default, event number
  recalculation is performed with conditional power and specified
  `minNumberOfEventsPerStage` and `maxNumberOfEventsPerStage` (see
  details and examples).

- showStatistics:

  Logical. If `TRUE`, summary statistics of the simulated data are
  displayed for the `print` command, otherwise the output is suppressed,
  default is `FALSE`.

## Value

Returns a
[`SimulationResults`](https://rpact-com.github.io/rpact/reference/SimulationResults.md)
object. The following generics (R generic functions) are available for
this object:

- [`names()`](https://rpact-com.github.io/rpact/reference/names.FieldSet.md)
  to obtain the field names,

- [`print()`](https://rpact-com.github.io/rpact/reference/print.FieldSet.md)
  to print the object,

- [`summary()`](https://rpact-com.github.io/rpact/reference/summary.ParameterSet.md)
  to display a summary of the object,

- [`plot()`](https://rpact-com.github.io/rpact/reference/plot.SimulationResults.md)
  to plot the object,

- [`as.data.frame()`](https://rpact-com.github.io/rpact/reference/as.data.frame.ParameterSet.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://rpact-com.github.io/rpact/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

At given design the function simulates the power, stopping
probabilities, conditional power, and expected sample size at given
number of events, number of subjects, and parameter configuration. It
also simulates the time when the required events are expected under the
given assumptions (exponentially, piecewise exponentially, or Weibull
distributed survival times and constant or non-constant piecewise
accrual). Additionally, integers `allocation1` and `allocation2` can be
specified that determine the number allocated to treatment group 1 and
treatment group 2, respectively. More precisely, unequal randomization
ratios must be specified via the two integer arguments `allocation1` and
`allocation2` which describe how many subjects are consecutively
enrolled in each group, respectively, before a subject is assigned to
the other group. For example, the arguments `allocation1 = 2`,
`allocation2 = 1`, `maxNumberOfSubjects = 300` specify 2:1 randomization
with 200 subjects randomized to intervention and 100 to control.
(Caveat: Do not use `allocation1 = 200`, `allocation2 = 100`,
`maxNumberOfSubjects = 300` as this would imply that the 200
intervention subjects are enrolled prior to enrollment of any control
subjects.)

`conditionalPower`  
The definition of `thetaH1` makes only sense if `kMax` \> 1 and if
`conditionalPower`, `minNumberOfEventsPerStage`, and
`maxNumberOfEventsPerStage` are defined.

Note that `numberOfSubjects`, `numberOfSubjects1`, and
`numberOfSubjects2` in the output are the expected number of subjects.

`calcEventsFunction`  
This function returns the number of events at given conditional power
and conditional critical value for specified testing situation. The
function might depend on variables `stage`, `conditionalPower`,
`thetaH0`, `plannedEvents`, `singleEventsPerStage`,
`minNumberOfEventsPerStage`, `maxNumberOfEventsPerStage`,
`allocationRatioPlanned`, `conditionalCriticalValue`, The function has
to contain the three-dots argument '...' (see examples).

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

## Simulation Data

The summary statistics "Simulated data" contains the following
parameters: median [range](https://rdrr.io/r/base/range.html); mean
+/-sd  

`$show(showStatistics = FALSE)` or `$setShowStatistics(FALSE)` can be
used to disable the output of the aggregated simulated data.  

Example 1:  
`simulationResults <- getSimulationSurvival(maxNumberOfSubjects = 100, plannedEvents = 30)`  
`simulationResults$show(showStatistics = FALSE)`  

Example 2:  
`simulationResults <- getSimulationSurvival(maxNumberOfSubjects = 100, plannedEvents = 30)`  
`simulationResults$setShowStatistics(FALSE)`  
`simulationResults`  

[`getData()`](https://rpact-com.github.io/rpact/reference/getData.md)
can be used to get the aggregated simulated data from the object as
[`data.frame`](https://rdrr.io/r/base/data.frame.html). The data frame
contains the following columns:

1.  `iterationNumber`: The number of the simulation iteration.

2.  `stageNumber`: The stage.

3.  `pi1`: The assumed or derived event rate in the treatment group.

4.  `pi2`: The assumed or derived event rate in the control group.

5.  `hazardRatio`: The hazard ratio under consideration (if available).

6.  `analysisTime`: The analysis time.

7.  `numberOfSubjects`: The number of subjects under consideration when
    the (interim) analysis takes place.

8.  `eventsPerStage1`: The observed number of events per stage in
    treatment group 1.

9.  `eventsPerStage2`: The observed number of events per stage in
    treatment group 2.

10. `singleEventsPerStage`: The observed number of events per stage in
    both treatment groups.

11. `rejectPerStage`: 1 if null hypothesis can be rejected, 0 otherwise.

12. `futilityPerStage`: 1 if study should be stopped for futility, 0
    otherwise.

13. `eventsNotAchieved`: 1 if number of events could not be reached with
    observed number of subjects, 0 otherwise.

14. `testStatistic`: The test statistic that is used for the test
    decision, depends on which design was chosen (group sequential,
    inverse normal, or Fisher combination test)'

15. `logRankStatistic`: Z-score statistic which corresponds to a
    one-sided log-rank test at considered stage.

16. `hazardRatioEstimateLR`: The estimated hazard ratio, derived from
    the log-rank statistic.

17. `trialStop`: `TRUE` if study should be stopped for efficacy or
    futility or final stage, `FALSE` otherwise.

18. `conditionalPowerAchieved`: The conditional power for the subsequent
    stage of the trial for selected sample size and effect. The effect
    is either estimated from the data or can be user defined with
    `thetaH1`.

## Raw Data

[`getRawData()`](https://rpact-com.github.io/rpact/reference/getRawData.md)
can be used to get the simulated raw data from the object as
[`data.frame`](https://rdrr.io/r/base/data.frame.html). Note that
`getSimulationSurvival()` must called before with
`maxNumberOfRawDatasetsPerStage` \> 0.

**What `maxNumberOfRawDatasetsPerStage` does**

When `maxNumberOfRawDatasetsPerStage = 0` (the default), simulations run
as usual but *no* patient-level ("raw") data are kept - only summary
results.

If you set `maxNumberOfRawDatasetsPerStage > 0`, rpact will **save up to
that many full patient-level datasets *per stage*** (i.e., per
interim/final look). Each saved dataset corresponds to one simulated
iteration and contains all subject-wise records accrued up to the stage
at which that iteration stopped. You can later retrieve these datasets
with
[`getRawData()`](https://rpact-com.github.io/rpact/reference/getRawData.md).

**Why "max" and not `numberOfRawDatasetsPerStage`?**

The value is an *upper bound per stage*, not a fixed count. The actual
number of datasets stored can be smaller because:

- Some iterations stop early (e.g., at stage 1), so later stages receive
  fewer datasets.

- The simulation might finish before reaching the cap due to other
  stopping or iteration limits.

As a result, the number specified is the **maximum possible** datasets
saved *per stage*, not the exact number.

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

## Examples

``` r
if (FALSE) { # \dontrun{
# Fixed sample size with minimum required definitions, pi1 = (0.3,0.4,0.5,0.6) and
# pi2 = 0.3 at event time 12, and accrual time 24
getSimulationSurvival(
    pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, eventTime = 12,
    accrualTime = 24, plannedEvents = 40, maxNumberOfSubjects = 200,
    maxNumberOfIterations = 10
)

# Increase number of simulation iterations
getSimulationSurvival(
    pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, eventTime = 12,
    accrualTime = 24, plannedEvents = 40, maxNumberOfSubjects = 200,
    maxNumberOfIterations = 50
)

# Determine necessary accrual time with default settings if 200 subjects and
# 30 subjects per time unit can be recruited
getSimulationSurvival(
    plannedEvents = 40, accrualTime = 0,
    accrualIntensity = 30, maxNumberOfSubjects = 200, maxNumberOfIterations = 50
)

# Determine necessary accrual time with default settings if 200 subjects and
# if the first 6 time units 20 subjects per time unit can be recruited,
# then 30 subjects per time unit
getSimulationSurvival(
    plannedEvents = 40, accrualTime = c(0, 6),
    accrualIntensity = c(20, 30), maxNumberOfSubjects = 200,
    maxNumberOfIterations = 50
)

# Determine maximum number of Subjects with default settings if the first
# 6 time units 20 subjects per time unit can be recruited, and after
# 10 time units 30 subjects per time unit
getSimulationSurvival(
    plannedEvents = 40, accrualTime = c(0, 6, 10),
    accrualIntensity = c(20, 30), maxNumberOfIterations = 50
)

# Specify accrual time as a list
at <- list(
    "0 - <6"  = 20,
    "6 - Inf" = 30
)
getSimulationSurvival(
    plannedEvents = 40, accrualTime = at,
    maxNumberOfSubjects = 200, maxNumberOfIterations = 50
)

# Specify accrual time as a list, if maximum number of subjects need to be calculated
at <- list(
    "0 - <6"   = 20,
    "6 - <=10" = 30
)
getSimulationSurvival(plannedEvents = 40, accrualTime = at, maxNumberOfIterations = 50)

# Specify effect size for a two-stage group sequential design with
# O'Brien & Fleming boundaries. Effect size is based on event rates
# at specified event time, directionUpper = FALSE needs to be specified
# because it should be shown that hazard ratio < 1
designGS <- getDesignGroupSequential(kMax = 2)
getSimulationSurvival(
    design = designGS,
    pi1 = 0.2, pi2 = 0.3, eventTime = 24, plannedEvents = c(20, 40),
    maxNumberOfSubjects = 200, directionUpper = FALSE, maxNumberOfIterations = 50
)

# As above, but with a three-stage O'Brien and Fleming design with
# specified information rates, note that planned events consists of integer values
designGS2 <- getDesignGroupSequential(informationRates = c(0.4, 0.7, 1))
getSimulationSurvival(
    design = designGS2, 
    pi1 = 0.2, pi2 = 0.3, eventTime = 24,
    plannedEvents = round(designGS2$informationRates * 40),
    maxNumberOfSubjects = 200, directionUpper = FALSE,
    maxNumberOfIterations = 50
)

# Effect size is based on event rate at specified event time for the reference
# group and hazard ratio, directionUpper = FALSE needs to be specified because
# it should be shown that hazard ratio < 1
getSimulationSurvival(
    design = designGS, hazardRatio = 0.5,
    pi2 = 0.3, eventTime = 24, plannedEvents = c(20, 40), maxNumberOfSubjects = 200,
    directionUpper = FALSE, maxNumberOfIterations = 50
)

# Effect size is based on hazard rate for the reference group and
# hazard ratio, directionUpper = FALSE needs to be specified because
# it should be shown that hazard ratio < 1
getSimulationSurvival(
    design = designGS,
    hazardRatio = 0.5, lambda2 = 0.02, plannedEvents = c(20, 40),
    maxNumberOfSubjects = 200, directionUpper = FALSE,
    maxNumberOfIterations = 50
)

# Specification of piecewise exponential survival time and hazard ratios,
# note that in getSimulationSurvival only on hazard ratio is used
# in the case that the survival time is piecewise expoential
getSimulationSurvival(
    design = designGS,
    piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04),
    hazardRatio = 1.5, plannedEvents = c(20, 40), maxNumberOfSubjects = 200,
    maxNumberOfIterations = 50
)

pws <- list(
    "0 - <5"  = 0.01,
    "5 - <10" = 0.02,
    ">=10"    = 0.04
)
getSimulationSurvival(
    design = designGS,
    piecewiseSurvivalTime = pws, hazardRatio = c(1.5),
    plannedEvents = c(20, 40), maxNumberOfSubjects = 200,
    maxNumberOfIterations = 50
)

# Specification of piecewise exponential survival time for both treatment arms
getSimulationSurvival(
    design = designGS,
    piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04),
    lambda1 = c(0.015, 0.03, 0.06), plannedEvents = c(20, 40),
    maxNumberOfSubjects = 200, maxNumberOfIterations = 50
)

# Specification of piecewise exponential survival time as a list,
# note that in getSimulationSurvival only on hazard ratio
# (not a vector) can be used
pws <- list(
    "0 - <5"  = 0.01,
    "5 - <10" = 0.02,
    ">=10"    = 0.04
)
getSimulationSurvival(
    design = designGS,
    piecewiseSurvivalTime = pws, hazardRatio = 1.5,
    plannedEvents = c(20, 40), maxNumberOfSubjects = 200,
    maxNumberOfIterations = 50
)

# Specification of piecewise exponential survival time and delayed effect
# (response after 5 time units)
getSimulationSurvival(
    design = designGS,
    piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04),
    lambda1 = c(0.01, 0.02, 0.06), plannedEvents = c(20, 40),
    maxNumberOfSubjects = 200, maxNumberOfIterations = 50
)

# Specify effect size based on median survival times
getSimulationSurvival(
    median1 = 5, median2 = 3, plannedEvents = 40,
    maxNumberOfSubjects = 200, directionUpper = FALSE,
    maxNumberOfIterations = 50
)

# Specify effect size based on median survival
# times of Weibull distribtion with kappa = 2
getSimulationSurvival(
    median1 = 5, median2 = 3, kappa = 2,
    plannedEvents = 40, maxNumberOfSubjects = 200,
    directionUpper = FALSE, maxNumberOfIterations = 50
)

# Perform recalculation of number of events based on conditional power for a
# three-stage design with inverse normal combination test, where the conditional power
# is calculated under the specified effect size thetaH1 = 1.3 and up to a four-fold
# increase in originally planned sample size (number of events) is allowed.
# Note that the first value in minNumberOfEventsPerStage and
# maxNumberOfEventsPerStage is arbitrary, i.e., it has no effect.
designIN <- getDesignInverseNormal(informationRates = c(0.4, 0.7, 1))

resultsWithSSR1 <- getSimulationSurvival(
    design = designIN,
    hazardRatio = seq(1, 1.6, 0.1),
    pi2 = 0.3, conditionalPower = 0.8, thetaH1 = 1.3,
    plannedEvents = c(58, 102, 146),
    minNumberOfEventsPerStage = c(NA, 44, 44),
    maxNumberOfEventsPerStage = 4 * c(NA, 44, 44),
    maxNumberOfSubjects = 800, maxNumberOfIterations = 50
)
resultsWithSSR1

# If thetaH1 is unspecified, the observed hazard ratio estimate
# (calculated from the log-rank statistic) is used for performing the
# recalculation of the number of events
resultsWithSSR2 <- getSimulationSurvival(
    design = designIN,
    hazardRatio = seq(1, 1.6, 0.1),
    pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 146),
    minNumberOfEventsPerStage = c(NA, 44, 44),
    maxNumberOfEventsPerStage = 4 * c(NA, 44, 44),
    maxNumberOfSubjects = 800, maxNumberOfIterations = 50
)
resultsWithSSR2

# Compare it with design without event size recalculation
resultsWithoutSSR <- getSimulationSurvival(
    design = designIN,
    hazardRatio = seq(1, 1.6, 0.1), pi2 = 0.3,
    plannedEvents = c(58, 102, 145), maxNumberOfSubjects = 800,
    maxNumberOfIterations = 50
)
resultsWithoutSSR$overallReject
resultsWithSSR1$overallReject
resultsWithSSR2$overallReject

# Confirm that event size racalcuation increases the Type I error rate,
# i.e., you have to use the combination test
resultsWithSSRGS <- getSimulationSurvival(
    design = designGS2, 
    hazardRatio = seq(1),
    pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 145),
    minNumberOfEventsPerStage = c(NA, 44, 44),
    maxNumberOfEventsPerStage = 4 * c(NA, 44, 44),
    maxNumberOfSubjects = 800, maxNumberOfIterations = 50
)
resultsWithSSRGS$overallReject

# Set seed to get reproducable results
identical(
    getSimulationSurvival(
        plannedEvents = 40, maxNumberOfSubjects = 200,
        seed = 99
    )$analysisTime,
    getSimulationSurvival(
        plannedEvents = 40, maxNumberOfSubjects = 200,
        seed = 99
    )$analysisTime
)

# Perform recalculation of number of events based on conditional power as above.
# The number of events is recalculated only in the first interim, the recalculated number
# is also used for the final stage. Here, we use the user defind calcEventsFunction as
# follows (note that the last stage value in minNumberOfEventsPerStage and maxNumberOfEventsPerStage
# has no effect):
myCalcEventsFunction <- function(...,
        stage, conditionalPower, estimatedTheta,
        plannedEvents, eventsOverStages,
        minNumberOfEventsPerStage, maxNumberOfEventsPerStage,
        conditionalCriticalValue) {
    theta <- max(1 + 1e-12, estimatedTheta)
    if (stage == 2) {
        requiredStageEvents <-
            max(0, conditionalCriticalValue + qnorm(conditionalPower))^2 * 4 / log(theta)^2
        requiredOverallStageEvents <- min(
            max(minNumberOfEventsPerStage[stage], requiredStageEvents),
            maxNumberOfEventsPerStage[stage]
        ) + eventsOverStages[stage - 1]
    } else {
        requiredOverallStageEvents <- 2 * eventsOverStages[stage - 1] - eventsOverStages[1]
    }
    return(requiredOverallStageEvents)
}
resultsWithSSR <- getSimulationSurvival(
    design = designIN,
    hazardRatio = seq(1, 2.6, 0.5),
    pi2 = 0.3,
    conditionalPower = 0.8,
    plannedEvents = c(58, 102, 146),
    minNumberOfEventsPerStage = c(NA, 44, 4),
    maxNumberOfEventsPerStage = 4 * c(NA, 44, 4),
    maxNumberOfSubjects = 800,
    calcEventsFunction = myCalcEventsFunction,
    seed = 1234,
    maxNumberOfIterations = 50
)
} # }
```
