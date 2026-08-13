# Get Fisher Information From a Design Plan or Simulation Results

Calculates the Fisher information at a planned analysis stage for a
design plan or simulation results object for means, rates, survival, or
count data endpoints.

## Usage

``` r
getFisherInformation(designPlan, stage = NA_integer_)
```

## Arguments

- designPlan:

  A trial design plan or simulation results object as returned by
  functions such as
  [`getSampleSizeMeans()`](https://docs.rpact.org/reference/getSampleSizeMeans.md),
  [`getPowerMeans()`](https://docs.rpact.org/reference/getPowerMeans.md),
  [`getSampleSizeRates()`](https://docs.rpact.org/reference/getSampleSizeRates.md),
  [`getPowerRates()`](https://docs.rpact.org/reference/getPowerRates.md),
  [`getSampleSizeSurvival()`](https://docs.rpact.org/reference/getSampleSizeSurvival.md),
  [`getPowerSurvival()`](https://docs.rpact.org/reference/getPowerSurvival.md),
  [`getSampleSizeCounts()`](https://docs.rpact.org/reference/getSampleSizeCounts.md),
  [`getPowerCounts()`](https://docs.rpact.org/reference/getPowerCounts.md),
  [`getSimulationMeans()`](https://docs.rpact.org/reference/getSimulationMeans.md),
  [`getSimulationRates()`](https://docs.rpact.org/reference/getSimulationRates.md),
  [`getSimulationSurvival()`](https://docs.rpact.org/reference/getSimulationSurvival.md),
  [`getSimulationCounts()`](https://docs.rpact.org/reference/getSimulationCounts.md),
  or the corresponding multi-arm simulation functions.

- stage:

  Integer. The analysis stage for which the Fisher information is
  requested. If `NA` (default), the first stage is used.

## Value

A numeric value, vector, or matrix containing the requested-stage Fisher
information. A vector or matrix can be returned if the object contains
several planning alternatives, arms, or sample size values. `NA_real_`
is returned if the endpoint type is not supported by this helper.

## Details

The returned information is the Fisher information used at the requested
analysis stage of the design plan or simulation setup. If `stage = NA`,
the first analysis stage is used.

For means, the information is based on the planned sample size, standard
deviations, allocation ratio, and, if applicable, the mean-ratio null
value. For rates, it is based on the planned sample size and the
binomial variance under the corresponding planning assumptions. For
survival endpoints, it is based on the planned number of events and the
allocation ratio. For count data, it is based on the planned exposure
times, event rates, allocation ratio, and overdispersion of the negative
binomial model.

## See also

[`getFutilityBounds()`](https://docs.rpact.org/reference/getFutilityBounds.md)

## Examples

``` r
if (FALSE) { # \dontrun{
designPlan <- getSampleSizeMeans(alternative = 0.4)
getFisherInformation(designPlan)

design <- getDesignGroupSequential(kMax = 3)
designPlan <- getPowerMeans(design,
    alternative = c(0.3, 0.4), maxNumberOfSubjects = 100
)
getFisherInformation(designPlan)

simulationResults <- getSimulationMeans(design,
    plannedSubjects = c(20, 40, 60), alternative = 0.4,
    maxNumberOfIterations = 10
)
getFisherInformation(simulationResults)
} # }
```
