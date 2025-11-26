# Get Simulation Raw Data for Survival

Returns the raw survival data which was generated for simulation.

## Usage

``` r
getRawData(x, aggregate = FALSE)
```

## Arguments

- x:

  A
  [`SimulationResults`](https://docs.rpact.org/reference/SimulationResults.md)
  object created by
  [`getSimulationSurvival()`](https://docs.rpact.org/reference/getSimulationSurvival.md).

- aggregate:

  Logical. If `TRUE` the raw data will be aggregated similar to the
  result of [`getData()`](https://docs.rpact.org/reference/getData.md),
  default is `FALSE`.

## Value

Returns a [`data.frame`](https://rdrr.io/r/base/data.frame.html).

## Details

This function works only if
[`getSimulationSurvival()`](https://docs.rpact.org/reference/getSimulationSurvival.md)
was called with a  
`maxNumberOfRawDatasetsPerStage` \> 0 (default is `0`).

This function can be used to get the simulated raw data from a
simulation results object obtained by
[`getSimulationSurvival()`](https://docs.rpact.org/reference/getSimulationSurvival.md).
Note that
[`getSimulationSurvival()`](https://docs.rpact.org/reference/getSimulationSurvival.md)
must called before with `maxNumberOfRawDatasetsPerStage` \> 0. The data
frame contains the following columns:

1.  `iterationNumber`: The number of the simulation iteration.

2.  `stopStage`: The stage of stopping.

3.  `subjectId`: The subject id (increasing number 1, 2, 3, ...)

4.  `accrualTime`: The accrual time, i.e., the time when the subject
    entered the trial.

5.  `treatmentGroup`: The treatment group number (1 or 2).

6.  `survivalTime`: The survival time of the subject.

7.  `dropoutTime`: The dropout time of the subject (may be `NA`).

8.  `lastObservationTime`: The specific observation time.

9.  `timeUnderObservation`: The time under observation is defined as
    follows:

        if (event == TRUE) {
            timeUnderObservation <- survivalTime
        } else if (dropoutEvent == TRUE) {
            timeUnderObservation <- dropoutTime
        } else {
            timeUnderObservation <- lastObservationTime - accrualTime
        }

10. `event`: `TRUE` if an event occurred; `FALSE` otherwise.

11. `dropoutEvent`: `TRUE` if an dropout event occurred; `FALSE`
    otherwise.

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
