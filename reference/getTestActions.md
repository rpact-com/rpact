# Get Test Actions

Returns test actions.

## Usage

``` r
getTestActions(stageResults, ...)
```

## Arguments

- stageResults:

  The results at given stage, obtained from
  [`getStageResults()`](https://docs.rpact.org/reference/getStageResults.md).

- ...:

  Only available for backward compatibility.

## Value

Returns a [`character`](https://rdrr.io/r/base/character.html) vector of
length `kMax` Returns a [`numeric`](https://rdrr.io/r/base/numeric.html)
vector of length `kMax` containing the test actions of each stage.

## Details

Returns the test actions of the specified design and stage results at
the specified stage.

## See also

Other analysis functions:
[`getAnalysisResults()`](https://docs.rpact.org/reference/getAnalysisResults.md),
[`getClosedCombinationTestResults()`](https://docs.rpact.org/reference/getClosedCombinationTestResults.md),
[`getClosedConditionalDunnettTestResults()`](https://docs.rpact.org/reference/getClosedConditionalDunnettTestResults.md),
[`getConditionalPower()`](https://docs.rpact.org/reference/getConditionalPower.md),
[`getConditionalRejectionProbabilities()`](https://docs.rpact.org/reference/getConditionalRejectionProbabilities.md),
[`getFinalConfidenceInterval()`](https://docs.rpact.org/reference/getFinalConfidenceInterval.md),
[`getFinalPValue()`](https://docs.rpact.org/reference/getFinalPValue.md),
[`getRepeatedConfidenceIntervals()`](https://docs.rpact.org/reference/getRepeatedConfidenceIntervals.md),
[`getRepeatedPValues()`](https://docs.rpact.org/reference/getRepeatedPValues.md),
[`getStageResults()`](https://docs.rpact.org/reference/getStageResults.md)

## Examples

``` r
if (FALSE) { # \dontrun{
design <- getDesignInverseNormal(kMax = 2)
data <- getDataset(
    n      = c( 20,  30),
    means  = c( 50,  51),
    stDevs = c(130, 140)
)
getTestActions(getStageResults(design, dataInput = data))
} # }
```
