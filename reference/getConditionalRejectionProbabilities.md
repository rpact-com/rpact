# Get Conditional Rejection Probabilities

Calculates the conditional rejection probabilities (CRP) for given test
results.

## Usage

``` r
getConditionalRejectionProbabilities(stageResults, ...)
```

## Arguments

- stageResults:

  The results at given stage, obtained from
  [`getStageResults()`](https://rpact-com.github.io/rpact/reference/getStageResults.md).

- ...:

  Further (optional) arguments to be passed:

  `iterations`

  :   Iterations for simulating the conditional rejection probabilities
      for Fisher's combination test. For checking purposes, it can be
      estimated via simulation with specified `iterations`.

  `seed`

  :   Seed for simulating the conditional rejection probabilities for
      Fisher's combination test. See above, default is a random seed.

## Value

Returns a [`numeric`](https://rdrr.io/r/base/numeric.html) vector of
length `kMax` or in case of multi-arm stage results a
[`matrix`](https://rdrr.io/r/base/matrix.html) (each column represents a
stage, each row a comparison) containing the conditional rejection
probabilities.

## Details

The conditional rejection probability is the probability, under H0, to
reject H0 in one of the subsequent (remaining) stages. The probability
is calculated using the specified design. For testing rates and the
survival design, the normal approximation is used, i.e., it is
calculated with the use of the prototype case testing a mean for
normally distributed data with known variance.

The conditional rejection probabilities are provided up to the specified
stage.

For Fisher's combination test, you can check the validity of the CRP
calculation via simulation.

## See also

Other analysis functions:
[`getAnalysisResults()`](https://rpact-com.github.io/rpact/reference/getAnalysisResults.md),
[`getClosedCombinationTestResults()`](https://rpact-com.github.io/rpact/reference/getClosedCombinationTestResults.md),
[`getClosedConditionalDunnettTestResults()`](https://rpact-com.github.io/rpact/reference/getClosedConditionalDunnettTestResults.md),
[`getConditionalPower()`](https://rpact-com.github.io/rpact/reference/getConditionalPower.md),
[`getFinalConfidenceInterval()`](https://rpact-com.github.io/rpact/reference/getFinalConfidenceInterval.md),
[`getFinalPValue()`](https://rpact-com.github.io/rpact/reference/getFinalPValue.md),
[`getRepeatedConfidenceIntervals()`](https://rpact-com.github.io/rpact/reference/getRepeatedConfidenceIntervals.md),
[`getRepeatedPValues()`](https://rpact-com.github.io/rpact/reference/getRepeatedPValues.md),
[`getStageResults()`](https://rpact-com.github.io/rpact/reference/getStageResults.md),
[`getTestActions()`](https://rpact-com.github.io/rpact/reference/getTestActions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate CRP for a Fisher's combination test design with
# two remaining stages and check the results by simulation.
design <- getDesignFisher(
    kMax = 4, alpha = 0.01,
    informationRates = c(0.1, 0.3, 0.8, 1)
)
data <- getDataset(n = c(40, 40), events = c(20, 22))
sr <- getStageResults(design, data, thetaH0 = 0.4)
getConditionalRejectionProbabilities(sr)
getConditionalRejectionProbabilities(sr,
    simulateCRP = TRUE,
    seed = 12345, iterations = 10000
)
} # }
```
