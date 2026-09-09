# Get Simulation Raw Data for Survival

Returns the raw survival data which was generated for simulation.

## Usage

``` r
getRawData(x, aggregate = FALSE)
```

## Arguments

- x:

  A survival
  [`SimulationResults`](https://docs.rpact.org/reference/SimulationResults.md)
  object created by
  [`getSimulationSurvival()`](https://docs.rpact.org/reference/getSimulationSurvival.md),
  [`getSimulationMultiArmSurvival()`](https://docs.rpact.org/reference/getSimulationMultiArmSurvival.md),
  or
  [`getSimulationEnrichmentSurvival()`](https://docs.rpact.org/reference/getSimulationEnrichmentSurvival.md).

- aggregate:

  Logical. If `TRUE` the raw data will be aggregated similar to the
  result of [`getData()`](https://docs.rpact.org/reference/getData.md),
  default is `FALSE`.

## Value

Returns a [`data.frame`](https://rdrr.io/r/base/data.frame.html).

## Details

This function works only if the simulation function was called with
`maxNumberOfRawDatasetsPerStage` \> 0 (default is `0`). Multi-arm and
enrichment simulations must use a patient-wise simulation type.

This function can be used to get the simulated raw data from a
simulation results object obtained from an ordinary, multi-arm, or
enrichment survival simulation. The data frame contains the following
columns:

1.  `iterationNumber`: The number of the simulation iteration.

2.  `stopStage`: The stage of stopping.

3.  `subjectId`: The subject id (increasing number 1, 2, 3, ...)

4.  `accrualTime`: The accrual time, i.e., the time when the subject
    entered the trial.

5.  `treatmentGroup`: The treatment group number (1 or 2).

6.  `survivalTime`: The survival time of the subject.

7.  `dropoutTime`: The dropout time of the subject (may be `NA`).

8.  `timeUnderObservation`: The time under observation is defined as
    follows:

        if (event == TRUE) {
            timeUnderObservation <- survivalTime
        } else if (dropoutEvent == TRUE) {
            timeUnderObservation <- dropoutTime
        } else {
            timeUnderObservation <- analysisTime - accrualTime
        }

    where `analysisTime` is available from the corresponding row of
    [`getData()`](https://docs.rpact.org/reference/getData.md).

9.  `event`: `TRUE` if an event occurred; `FALSE` otherwise.

10. `dropoutEvent`: `TRUE` if an dropout event occurred; `FALSE`
    otherwise.

Multi-arm and enrichment raw data additionally contain a stable integer
`scenario` identifier. Enrichment raw data also contain the subject's
`subGroup`. For these simulation types, `aggregate = TRUE` aggregates
multi-arm data by scenario and iteration and enrichment data by
scenario, iteration, and subgroup.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- getSimulationSurvival(
    pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, eventTime = 12,
    accrualTime = 24, plannedEvents = 40, maxNumberOfSubjects = 200,
    maxNumberOfIterations = 50, maxNumberOfRawDatasetsPerStage = 5
)
rawData <- getRawData(results)
head(rawData)
dim(rawData)
} # }
```
