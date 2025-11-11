# Get Repeated P Values

Calculates the repeated p-values for a given test results.

## Usage

``` r
getRepeatedPValues(stageResults, ..., tolerance = 1e-06)
```

## Arguments

- stageResults:

  The results at given stage, obtained from
  [`getStageResults()`](https://rpact-com.github.io/rpact/reference/getStageResults.md).

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- tolerance:

  The numerical tolerance, default is `1e-06`. Must be a positive
  numeric of length 1.

## Value

Returns a [`numeric`](https://rdrr.io/r/base/numeric.html) vector of
length `kMax` or in case of multi-arm stage results a
[`matrix`](https://rdrr.io/r/base/matrix.html) (each column represents a
stage, each row a comparison) containing the repeated p values.

## Details

The repeated p-value at a given stage of the trial is defined as the
smallest significance level under which at given test design the test
results obtain rejection of the null hypothesis. It can be calculated at
each stage of the trial and can thus be used as a monitoring tool.

The repeated p-values are provided up to the specified stage.

In multi-arm trials, the repeated p-values are defined separately for
each treatment comparison within the closed testing procedure.

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
getRepeatedPValues(getStageResults(design, dataInput = data))
} # }
```
