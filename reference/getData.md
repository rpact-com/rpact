# Get Simulation Data

Returns the aggregated simulation data.

## Usage

``` r
getData(x)

getData.SimulationResults(x)
```

## Arguments

- x:

  A
  [`SimulationResults`](https://docs.rpact.org/reference/SimulationResults.md)
  object created by
  [`getSimulationMeans()`](https://docs.rpact.org/reference/getSimulationMeans.md),  
  [`getSimulationRates()`](https://docs.rpact.org/reference/getSimulationRates.md),
  [`getSimulationSurvival()`](https://docs.rpact.org/reference/getSimulationSurvival.md),
  [`getSimulationMultiArmMeans()`](https://docs.rpact.org/reference/getSimulationMultiArmMeans.md),  
  [`getSimulationMultiArmRates()`](https://docs.rpact.org/reference/getSimulationMultiArmRates.md),
  or
  [`getSimulationMultiArmSurvival()`](https://docs.rpact.org/reference/getSimulationMultiArmSurvival.md).

## Value

Returns a [`data.frame`](https://rdrr.io/r/base/data.frame.html).

## Details

This function can be used to get the aggregated simulated data from an
simulation results object, for example, obtained by
[`getSimulationSurvival()`](https://docs.rpact.org/reference/getSimulationSurvival.md).
In this case, the data frame contains the following columns:

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

10. `eventsPerStage`: The observed number of events per stage in both
    treatment groups.

11. `rejectPerStage`: 1 if null hypothesis can be rejected, 0 otherwise.

12. `eventsNotAchieved`: 1 if number of events could not be reached with
    observed number of subjects, 0 otherwise.

13. `futilityPerStage`: 1 if study should be stopped for futility, 0
    otherwise.

14. `testStatistic`: The test statistic that is used for the test
    decision, depends on which design was chosen (group sequential,
    inverse normal, or Fisher combination test)'

15. `logRankStatistic`: Z-score statistic which corresponds to a
    one-sided log-rank test at considered stage.

16. `conditionalPowerAchieved`: The conditional power for the subsequent
    stage of the trial for selected sample size and effect. The effect
    is either estimated from the data or can be user defined with
    `thetaH1` or `pi1H1` and `pi2H1`.

17. `trialStop`: `TRUE` if study should be stopped for efficacy or
    futility or final stage, `FALSE` otherwise.

18. `hazardRatioEstimateLR`: The estimated hazard ratio, derived from
    the log-rank statistic.

A subset of variables is provided for
[`getSimulationMeans()`](https://docs.rpact.org/reference/getSimulationMeans.md),
[`getSimulationRates()`](https://docs.rpact.org/reference/getSimulationRates.md),
[`getSimulationMultiArmMeans()`](https://docs.rpact.org/reference/getSimulationMultiArmMeans.md),  
[`getSimulationMultiArmRates()`](https://docs.rpact.org/reference/getSimulationMultiArmRates.md),
or
[`getSimulationMultiArmSurvival()`](https://docs.rpact.org/reference/getSimulationMultiArmSurvival.md).

## Examples

``` r
if (FALSE) { # \dontrun{
results <- getSimulationSurvival(
    pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, eventTime = 12,
    accrualTime = 24, plannedEvents = 40, maxNumberOfSubjects = 200,
    maxNumberOfIterations = 50
)
data <- getData(results)
head(data)
dim(data)
} # }
```
