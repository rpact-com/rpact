# Get Simulation Rates

Returns the simulated power, stopping probabilities, conditional power,
and expected sample size for testing rates in a one or two treatment
groups testing situation.

## Usage

``` r
getSimulationRates(
  design = NULL,
  ...,
  groups = 2L,
  normalApproximation = TRUE,
  riskRatio = FALSE,
  thetaH0 = ifelse(riskRatio, 1, 0),
  pi1 = seq(0.2, 0.5, 0.1),
  pi2 = NA_real_,
  plannedSubjects = NA_real_,
  directionUpper = NA,
  allocationRatioPlanned = NA_real_,
  minNumberOfSubjectsPerStage = NA_real_,
  maxNumberOfSubjectsPerStage = NA_real_,
  conditionalPower = NA_real_,
  pi1H1 = NA_real_,
  pi2H1 = NA_real_,
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

  The type of computation of the p-values. Default is `FALSE` for
  testing means (i.e., the t test is used) and `TRUE` for testing rates
  and the hazard ratio. For testing rates, if
  `normalApproximation = FALSE` is specified, the binomial test (one
  sample) or the exact test of Fisher (two samples) is used for
  calculating the p-values. In the survival setting
  `normalApproximation = FALSE` has no effect.

- riskRatio:

  If `TRUE`, the design characteristics for one-sided testing of H0:
  `pi1 / pi2 = thetaH0` are simulated, default is `FALSE`.

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

  A numeric value or vector that represents the assumed probability in
  the active treatment group if two treatment groups are considered, or
  the alternative probability for a one treatment group design, default
  is `seq(0.2, 0.5, 0.1)` (power calculations and simulations) or
  `seq(0.4, 0.6, 0.1)` (sample size calculations).

- pi2:

  A numeric value that represents the assumed probability in the
  reference group if two treatment groups are considered, default is
  `0.2`.

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

- pi1H1:

  If specified, the assumed probability in the active treatment group if
  two treatment groups are considered, or the assumed probability for a
  one treatment group design, for which the conditional power was
  calculated.

- pi2H1:

  If specified, the assumed probability in the reference group if two
  treatment groups are considered, for which the conditional power was
  calculated.

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

The definition of `pi1H1` and/or `pi2H1` makes only sense if `kMax` \> 1
and if `conditionalPower`, `minNumberOfSubjectsPerStage`, and
`maxNumberOfSubjectsPerStage` (or `calcSubjectsFunction`) are defined.

`calcSubjectsFunction`  
This function returns the number of subjects at given conditional power
and conditional critical value for specified testing situation. The
function might depend on variables `stage`, `riskRatio`, `thetaH0`,
`groups`, `plannedSubjects`, `sampleSizesPerStage`, `directionUpper`,
`allocationRatioPlanned`, `minNumberOfSubjectsPerStage`,
`maxNumberOfSubjectsPerStage`, `conditionalPower`,
`conditionalCriticalValue`, `overallRate`, `farringtonManningValue1`,
and `farringtonManningValue2`. The function has to contain the
three-dots argument '...' (see examples).

## Simulation Data

The summary statistics "Simulated data" contains the following
parameters: median [range](https://rdrr.io/r/base/range.html); mean
+/-sd  

`$show(showStatistics = FALSE)` or `$setShowStatistics(FALSE)` can be
used to disable the output of the aggregated simulated data.  

Example 1:  
`simulationResults <- getSimulationRates(plannedSubjects = 40)`  
`simulationResults$show(showStatistics = FALSE)`  

Example 2:  
`simulationResults <- getSimulationRates(plannedSubjects = 40)`  
`simulationResults$setShowStatistics(FALSE)`  
`simulationResults`  

[`getData()`](https://docs.rpact.org/reference/getData.md) can be used
to get the aggregated simulated data from the object as
[`data.frame`](https://rdrr.io/r/base/data.frame.html). The data frame
contains the following columns:

1.  `iterationNumber`: The number of the simulation iteration.

2.  `stageNumber`: The stage.

3.  `pi1`: The assumed or derived event rate in the treatment group (if
    available).

4.  `pi2`: The assumed or derived event rate in the control group (if
    available).

5.  `numberOfSubjects`: The number of subjects under consideration when
    the (interim) analysis takes place.

6.  `rejectPerStage`: 1 if null hypothesis can be rejected, 0 otherwise.

7.  `futilityPerStage`: 1 if study should be stopped for futility, 0
    otherwise.

8.  `testStatistic`: The test statistic that is used for the test
    decision, depends on which design was chosen (group sequential,
    inverse normal, or Fisher combination test)'

9.  `testStatisticsPerStage`: The test statistic for each stage if only
    data from the considered stage is taken into account.

10. `overallRate1`: The cumulative rate in treatment group 1.

11. `overallRate2`: The cumulative rate in treatment group 2.

12. `stagewiseRates1`: The stage-wise rate in treatment group 1.

13. `stagewiseRates2`: The stage-wise rate in treatment group 2.

14. `sampleSizesPerStage1`: The stage-wise sample size in treatment
    group 1.

15. `sampleSizesPerStage2`: The stage-wise sample size in treatment
    group 2.

16. `trialStop`: `TRUE` if study should be stopped for efficacy or
    futility or final stage, `FALSE` otherwise.

17. `conditionalPowerAchieved`: The conditional power for the subsequent
    stage of the trial for selected sample size and effect. The effect
    is either estimated from the data or can be user defined with
    `pi1H1` and `pi2H1`.

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
# Fixed sample size design (two groups) with total sample 
# size 120, pi1 = (0.3,0.4,0.5,0.6) and pi2 = 0.3
getSimulationRates(pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, 
    plannedSubjects = 120, maxNumberOfIterations = 10)

# Increase number of simulation iterations and compare results with power calculator
getSimulationRates(pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, 
    plannedSubjects = 120, maxNumberOfIterations = 50)
getPowerRates(pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, maxNumberOfSubjects = 120)

# Do the same for a two-stage Pocock inverse normal group sequential 
# design with non-binding futility stops
designIN <- getDesignInverseNormal(typeOfDesign = "P", futilityBounds = c(0))
getSimulationRates(designIN, pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, 
    plannedSubjects = c(40, 80), maxNumberOfIterations = 50)
getPowerRates(designIN, pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, maxNumberOfSubjects = 80)

# Assess power and average sample size if a sample size reassessment is 
# foreseen at conditional power 80% for the subsequent stage (decrease and increase) 
# based on observed overall (cumulative) rates and specified minNumberOfSubjectsPerStage 
# and maxNumberOfSubjectsPerStage

# Do the same under the assumption that a sample size increase only takes place 
# if the rate difference exceeds the value 0.1 at interim. For this, the sample 
# size recalculation method needs to be redefined:  
mySampleSizeCalculationFunction <- function(..., stage,
        plannedSubjects,
        minNumberOfSubjectsPerStage,
        maxNumberOfSubjectsPerStage,
        conditionalPower,
        conditionalCriticalValue,
        overallRate) {
    if (overallRate[1] - overallRate[2] < 0.1) {
        return(plannedSubjects[stage] - plannedSubjects[stage - 1]) 
    } else {
        rateUnderH0 <- (overallRate[1] + overallRate[2]) / 2 
        stageSubjects <- 2 * (max(0, conditionalCriticalValue * 
            sqrt(2 * rateUnderH0 * (1 - rateUnderH0)) + 
            stats::qnorm(conditionalPower) * sqrt(overallRate[1] * 
            (1 - overallRate[1]) + overallRate[2] * (1 - overallRate[2]))))^2 /
            (max(1e-12, (overallRate[1] - overallRate[2])))^2
        stageSubjects <- ceiling(min(max(
            minNumberOfSubjectsPerStage[stage], 
            stageSubjects), maxNumberOfSubjectsPerStage[stage]))
        return(stageSubjects)
    }
}
getSimulationRates(designIN, pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, 
    plannedSubjects = c(40, 80), minNumberOfSubjectsPerStage = c(40, 20), 
    maxNumberOfSubjectsPerStage = c(40, 160), conditionalPower = 0.8, 
    calcSubjectsFunction = mySampleSizeCalculationFunction, maxNumberOfIterations = 50)
} # }
```
