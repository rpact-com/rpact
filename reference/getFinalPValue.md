# Get Final P Value

Returns the final p-value for given stage results.

## Usage

``` r
getFinalPValue(stageResults, ...)
```

## Arguments

- stageResults:

  The results at given stage, obtained from
  [`getStageResults()`](https://rpact-com.github.io/rpact/reference/getStageResults.md).

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
[`getAnalysisResults()`](https://rpact-com.github.io/rpact/reference/getAnalysisResults.md),
[`getClosedCombinationTestResults()`](https://rpact-com.github.io/rpact/reference/getClosedCombinationTestResults.md),
[`getClosedConditionalDunnettTestResults()`](https://rpact-com.github.io/rpact/reference/getClosedConditionalDunnettTestResults.md),
[`getConditionalPower()`](https://rpact-com.github.io/rpact/reference/getConditionalPower.md),
[`getConditionalRejectionProbabilities()`](https://rpact-com.github.io/rpact/reference/getConditionalRejectionProbabilities.md),
[`getFinalConfidenceInterval()`](https://rpact-com.github.io/rpact/reference/getFinalConfidenceInterval.md),
[`getRepeatedConfidenceIntervals()`](https://rpact-com.github.io/rpact/reference/getRepeatedConfidenceIntervals.md),
[`getRepeatedPValues()`](https://rpact-com.github.io/rpact/reference/getRepeatedPValues.md),
[`getStageResults()`](https://rpact-com.github.io/rpact/reference/getStageResults.md),
[`getTestActions()`](https://rpact-com.github.io/rpact/reference/getTestActions.md)

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
