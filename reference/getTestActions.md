# Get Test Actions

Returns test actions.

## Usage

``` r
getTestActions(stageResults, ...)
```

## Arguments

- stageResults:

  The results at given stage, obtained from
  [`getStageResults()`](https://rpact-com.github.io/rpact/reference/getStageResults.md).

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
[`getAnalysisResults()`](https://rpact-com.github.io/rpact/reference/getAnalysisResults.md),
[`getClosedCombinationTestResults()`](https://rpact-com.github.io/rpact/reference/getClosedCombinationTestResults.md),
[`getClosedConditionalDunnettTestResults()`](https://rpact-com.github.io/rpact/reference/getClosedConditionalDunnettTestResults.md),
[`getConditionalPower()`](https://rpact-com.github.io/rpact/reference/getConditionalPower.md),
[`getConditionalRejectionProbabilities()`](https://rpact-com.github.io/rpact/reference/getConditionalRejectionProbabilities.md),
[`getFinalConfidenceInterval()`](https://rpact-com.github.io/rpact/reference/getFinalConfidenceInterval.md),
[`getFinalPValue()`](https://rpact-com.github.io/rpact/reference/getFinalPValue.md),
[`getRepeatedConfidenceIntervals()`](https://rpact-com.github.io/rpact/reference/getRepeatedConfidenceIntervals.md),
[`getRepeatedPValues()`](https://rpact-com.github.io/rpact/reference/getRepeatedPValues.md),
[`getStageResults()`](https://rpact-com.github.io/rpact/reference/getStageResults.md)

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
