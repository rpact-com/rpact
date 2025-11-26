# Get Simulation Means

Returns the simulated power, stopping probabilities, conditional power,
and expected sample size for testing means in a one or two treatment
groups testing situation.

## Usage

``` r
getSimulationMeans(
  design = NULL,
  ...,
  groups = 2L,
  normalApproximation = TRUE,
  meanRatio = FALSE,
  thetaH0 = ifelse(meanRatio, 1, 0),
  alternative = seq(0, 1, 0.2),
  stDev = 1,
  plannedSubjects = NA_real_,
  directionUpper = NA,
  allocationRatioPlanned = NA_real_,
  minNumberOfSubjectsPerStage = NA_real_,
  maxNumberOfSubjectsPerStage = NA_real_,
  conditionalPower = NA_real_,
  thetaH1 = NA_real_,
  stDevH1 = NA_real_,
  maxNumberOfIterations = 1000L,
  seed = NA_real_,
  calcSubjectsFunction = NULL,
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

- groups:

  The number of treatment groups (1 or 2), default is `2`.

- normalApproximation:

  The type of computation of the p-values. Default is `TRUE`, i.e.,
  normally distributed test statistics are generated. If `FALSE`, the t
  test is used for calculating the p-values, i.e., t distributed test
  statistics are generated.

- meanRatio:

  If `TRUE`, the design characteristics for one-sided testing of H0:
  `mu1 / mu2 = thetaH0` are simulated, default is `FALSE`.

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

  The alternative hypothesis value for testing means under which the
  data is simulated. This can be a vector of assumed alternatives,
  default is `seq(0, 1, 0.2)`.

- stDev:

  The standard deviation under which the data is simulated, default is
  `1`. For two-armed trials, it is allowed to specify the standard
  deviations separately, i.e., as vector with two elements. If
  `meanRatio = TRUE` is specified, `stDev` defines the coefficient of
  variation `sigma / mu2`.

- plannedSubjects:

  `plannedSubjects` is a numeric vector of length `kMax` (the number of
  stages of the design) that determines the number of cumulated
  (overall) subjects when the interim stages are planned. For two
  treatment arms, it is the number of subjects for both treatment arms.
  For multi-arm designs, `plannedSubjects` refers to the number of
  subjects per selected active arm.

- directionUpper:

  Logical. Specifies the direction of the alternative, only applicable
  for one-sided testing; default is `TRUE` which means that larger
  values of the test statistics yield smaller p-values.

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

- minNumberOfSubjectsPerStage:

  When performing a data driven sample size recalculation, the numeric
  vector `minNumberOfSubjectsPerStage` with length `kMax` determines the
  minimum number of subjects per stage (i.e., not cumulated), the first
  element is not taken into account. For two treatment arms, it is the
  number of subjects for both treatment arms. For multi-arm designs
  `minNumberOfSubjectsPerStage` refers to the minimum number of subjects
  per selected active arm.

- maxNumberOfSubjectsPerStage:

  When performing a data driven sample size recalculation, the numeric
  vector `maxNumberOfSubjectsPerStage` with length `kMax` determines the
  maximum number of subjects per stage (i.e., not cumulated), the first
  element is not taken into account. For two treatment arms, it is the
  number of subjects for both treatment arms. For multi-arm designs
  `maxNumberOfSubjectsPerStage` refers to the maximum number of subjects
  per selected active arm.

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

- stDevH1:

  If specified, the value of the standard deviation under which the
  conditional power or sample size recalculation calculation is
  performed, default is the value of `stDev`.

- maxNumberOfIterations:

  The number of simulation iterations, default is `1000`. Must be a
  positive integer of length 1.

- seed:

  The seed to reproduce the simulation, default is a random seed.

- calcSubjectsFunction:

  Optionally, a function can be entered that defines the way of
  performing the sample size recalculation. By default, sample size
  recalculation is performed with conditional power and specified
  `minNumberOfSubjectsPerStage` and `maxNumberOfSubjectsPerStage` (see
  details and examples).

- showStatistics:

  Logical. If `TRUE`, summary statistics of the simulated data are
  displayed for the `print` command, otherwise the output is suppressed,
  default is `FALSE`.

## Value

Returns a
[`SimulationResults`](https://docs.rpact.org/reference/SimulationResults.md)
object. The following generics (R generic functions) are available for
this object:

- [`names()`](https://docs.rpact.org/reference/names.FieldSet.md) to
  obtain the field names,

- [`print()`](https://docs.rpact.org/reference/print.FieldSet.md) to
  print the object,

- [`summary()`](https://docs.rpact.org/reference/summary.ParameterSet.md)
  to display a summary of the object,

- [`plot()`](https://docs.rpact.org/reference/plot.SimulationResults.md)
  to plot the object,

- [`as.data.frame()`](https://docs.rpact.org/reference/as.data.frame.ParameterSet.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://docs.rpact.org/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

At given design the function simulates the power, stopping
probabilities, conditional power, and expected sample size at given
number of subjects and parameter configuration. Additionally, an
allocation ratio = n1/n2 can be specified where n1 and n2 are the number
of subjects in the two treatment groups.

The definition of `thetaH1` makes only sense if `kMax` \> 1 and if
`conditionalPower`, `minNumberOfSubjectsPerStage`, and
`maxNumberOfSubjectsPerStage` (or `calcSubjectsFunction`) are defined.

`calcSubjectsFunction`  
This function returns the number of subjects at given conditional power
and conditional critical value for specified testing situation. The
function might depend on variables `stage`, `meanRatio`, `thetaH0`,
`groups`, `plannedSubjects`, `sampleSizesPerStage`, `directionUpper`,
`allocationRatioPlanned`, `minNumberOfSubjectsPerStage`,
`maxNumberOfSubjectsPerStage`, `conditionalPower`,
`conditionalCriticalValue`, `thetaH1`, and `stDevH1`. The function has
to contain the three-dots argument '...' (see examples).

## Simulation Data

The summary statistics "Simulated data" contains the following
parameters: median [range](https://rdrr.io/r/base/range.html); mean
+/-sd  

`$show(showStatistics = FALSE)` or `$setShowStatistics(FALSE)` can be
used to disable the output of the aggregated simulated data.  

Example 1:  
`simulationResults <- getSimulationMeans(plannedSubjects = 40)`  
`simulationResults$show(showStatistics = FALSE)`  

Example 2:  
`simulationResults <- getSimulationMeans(plannedSubjects = 40)`  
`simulationResults$setShowStatistics(FALSE)`  
`simulationResults`  

[`getData()`](https://docs.rpact.org/reference/getData.md) can be used
to get the aggregated simulated data from the object as
[`data.frame`](https://rdrr.io/r/base/data.frame.html). The data frame
contains the following columns:

1.  `iterationNumber`: The number of the simulation iteration.

2.  `stageNumber`: The stage.

3.  `alternative`: The alternative hypothesis value.

4.  `numberOfSubjects`: The number of subjects under consideration when
    the (interim) analysis takes place.

5.  `rejectPerStage`: 1 if null hypothesis can be rejected, 0 otherwise.

6.  `futilityPerStage`: 1 if study should be stopped for futility, 0
    otherwise.

7.  `testStatistic`: The test statistic that is used for the test
    decision, depends on which design was chosen (group sequential,
    inverse normal, or Fisher's combination test).

8.  `testStatisticsPerStage`: The test statistic for each stage if only
    data from the considered stage is taken into account.

9.  `effectEstimate`: Overall simulated standardized effect estimate.

10. `trialStop`: `TRUE` if study should be stopped for efficacy or
    futility or final stage, `FALSE` otherwise.

11. `conditionalPowerAchieved`: The conditional power for the subsequent
    stage of the trial for selected sample size and effect. The effect
    is either estimated from the data or can be user defined with
    `thetaH1`.

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
# Fixed sample size design with two groups, total sample size 40,
# alternative = c(0, 0.2, 0.4, 0.8, 1), and standard deviation = 1 (the default)
getSimulationMeans(plannedSubjects = 40, maxNumberOfIterations = 10)

# Increase number of simulation iterations and compare results
# with power calculator using normal approximation
getSimulationMeans(
    alternative = 0:4, stDev = 5,
    plannedSubjects = 40, maxNumberOfIterations = 1000
)
getPowerMeans(
    alternative = 0:4, stDev = 5,
    maxNumberOfSubjects = 40, normalApproximation = TRUE
)

# Do the same for a three-stage O'Brien&Fleming inverse
# normal group sequential design with non-binding futility stops
designIN <- getDesignInverseNormal(typeOfDesign = "OF", futilityBounds = c(0, 0))
x <- getSimulationMeans(designIN,
    alternative = c(0:4), stDev = 5,
    plannedSubjects = c(20, 40, 60), maxNumberOfIterations = 1000
)
getPowerMeans(designIN,
    alternative = 0:4, stDev = 5,
    maxNumberOfSubjects = 60, normalApproximation = TRUE
)

# Assess power and average sample size if a sample size increase is foreseen
# at conditional power 80% for each subsequent stage based on observed overall
# effect and specified minNumberOfSubjectsPerStage and
# maxNumberOfSubjectsPerStage
getSimulationMeans(designIN,
    alternative = 0:4, stDev = 5,
    plannedSubjects = c(20, 40, 60),
    minNumberOfSubjectsPerStage = c(NA, 20, 20),
    maxNumberOfSubjectsPerStage = c(NA, 80, 80),
    conditionalPower = 0.8,
    maxNumberOfIterations = 50
)

# Do the same under the assumption that a sample size increase only takes
# place at the first interim. The sample size for the third stage is set equal
# to the second stage sample size.
mySampleSizeCalculationFunction <- function(..., stage,
        minNumberOfSubjectsPerStage,
        maxNumberOfSubjectsPerStage,
        sampleSizesPerStage,
        conditionalPower,
        conditionalCriticalValue,
        allocationRatioPlanned,
        thetaH1,
        stDevH1) {
    if (stage <= 2) {
        # Note that allocationRatioPlanned is as a vector of length kMax
        stageSubjects <- (1 + allocationRatioPlanned[stage])^2 / 
            allocationRatioPlanned[stage] *
            (max(0, conditionalCriticalValue + stats::qnorm(conditionalPower)))^2 /
            (max(1e-12, thetaH1 / stDevH1))^2
        stageSubjects <- min(max(
            minNumberOfSubjectsPerStage[stage],
            stageSubjects
        ), maxNumberOfSubjectsPerStage[stage])
    } else {
        stageSubjects <- sampleSizesPerStage[stage - 1]
    }
    return(stageSubjects)
}
getSimulationMeans(designIN,
    alternative = 0:4, stDev = 5,
    plannedSubjects = c(20, 40, 60),
    minNumberOfSubjectsPerStage = c(NA, 20, 20),
    maxNumberOfSubjectsPerStage = c(NA, 80, 80),
    conditionalPower = 0.8,
    calcSubjectsFunction = mySampleSizeCalculationFunction,
    maxNumberOfIterations = 50
)
} # }
```
