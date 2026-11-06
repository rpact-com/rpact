# Get Final P Value

Returns the final p-value for given stage results.

## Usage

``` r
getFinalPValue(stageResults, ...)
```

## Arguments

- stageResults:

  The results at given stage, obtained from
  [`getStageResults()`](https://docs.rpact.org/qtd/reference/getStageResults.md).

- ...:

  Only available for backward compatibility.

## Value

Returns a [`list`](https://rdrr.io/r/base/list.html) containing

- `finalStage`,

- `pFinal`.

## Details

The calculation of the final p-value is based on the stage-wise ordering
of the sample space. This enables the calculation for both the
non-adaptive and the adaptive case. For Fisher's combination test, it is
available for `kMax = 2` only.

## See also

Other analysis functions:
[`getAnalysisResults()`](https://docs.rpact.org/qtd/reference/getAnalysisResults.md),
[`getClosedCombinationTestResults()`](https://docs.rpact.org/qtd/reference/getClosedCombinationTestResults.md),
[`getClosedConditionalDunnettTestResults()`](https://docs.rpact.org/qtd/reference/getClosedConditionalDunnettTestResults.md),
[`getConditionalPower()`](https://docs.rpact.org/qtd/reference/getConditionalPower.md),
[`getConditionalRejectionProbabilities()`](https://docs.rpact.org/qtd/reference/getConditionalRejectionProbabilities.md),
[`getFinalConfidenceInterval()`](https://docs.rpact.org/qtd/reference/getFinalConfidenceInterval.md),
[`getRepeatedConfidenceIntervals()`](https://docs.rpact.org/qtd/reference/getRepeatedConfidenceIntervals.md),
[`getRepeatedPValues()`](https://docs.rpact.org/qtd/reference/getRepeatedPValues.md),
[`getStageResults()`](https://docs.rpact.org/qtd/reference/getStageResults.md),
[`getTestActions()`](https://docs.rpact.org/qtd/reference/getTestActions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
design <- getDesignInverseNormal(kMax = 2)
data <- getDataset(
    n      = c( 20,  30),
    means  = c( 50,  51),
    stDevs = c(130, 140)
)
getFinalPValue(getStageResults(design, dataInput = data))
} # }
```
