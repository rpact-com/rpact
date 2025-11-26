# Names of a Simulation Results Object

Function to get the names of a
[`SimulationResults`](https://docs.rpact.org/reference/SimulationResults.md)
object.

## Usage

``` r
# S3 method for class 'SimulationResults'
names(x)
```

## Arguments

- x:

  A
  [`SimulationResults`](https://docs.rpact.org/reference/SimulationResults.md)
  object created by
  `getSimulationResults[MultiArm/Enrichment][Means/Rates/Survival]`.

## Value

Returns a [`character`](https://rdrr.io/r/base/character.html) vector
containing the names of the
[`AnalysisResults`](https://docs.rpact.org/reference/AnalysisResults.md)
object.

## Details

Returns the names of a simulation results that can be accessed by the
user.
