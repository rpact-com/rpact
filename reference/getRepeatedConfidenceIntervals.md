# Get Repeated Confidence Intervals

Calculates and returns the lower and upper limit of the repeated
confidence intervals of the trial.

## Usage

``` r
getRepeatedConfidenceIntervals(
  design,
  dataInput,
  ...,
  directionUpper = NA,
  tolerance = 1e-06,
  stage = NA_integer_
)
```

## Arguments

- design:

  The trial design.

- dataInput:

  The summary data used for calculating the test results. This is either
  an element of `DatasetMeans`, of `DatasetRates`, or of
  `DatasetSurvival` and should be created with the function
  [`getDataset()`](https://docs.rpact.org/reference/getDataset.md). For
  more information see
  [`getDataset()`](https://docs.rpact.org/reference/getDataset.md).

- ...:

  Further arguments to be passed to methods (cf., separate functions in
  "See Also" below), e.g.,

  `normalApproximation`

  :   The type of computation of the p-values. Default is `FALSE` for
      testing means (i.e., the t test is used) and `TRUE` for testing
      rates and the hazard ratio. For testing rates, if
      `normalApproximation = FALSE` is specified, the binomial test (one
      sample) or the exact test of Fisher (two samples) is used for
      calculating the p-values. In the survival setting,
      `normalApproximation = FALSE` has no effect.

  `equalVariances`

  :   The type of t test. For testing means in two treatment groups,
      either the t test assuming that the variances are equal or the t
      test without assuming this, i.e., the test of Welch-Satterthwaite
      is calculated, default is `TRUE`.

  `intersectionTest`

  :   Defines the multiple test for the intersection hypotheses in the
      closed system of hypotheses when testing multiple hypotheses. Five
      options are available in multi-arm designs: `"Dunnett"`,
      `"Bonferroni"`, `"Simes"`, `"Sidak"`, and `"Hierarchical"`,
      default is `"Dunnett"`. Four options are available in population
      enrichment designs: `"SpiessensDebois"` (one subset only),
      `"Bonferroni"`, `"Simes"`, and `"Sidak"`, default is `"Simes"`.

  `varianceOption`

  :   Defines the way to calculate the variance in multiple treatment
      arms (\> 2) or population enrichment designs for testing means.
      For multiple arms, three options are available: `"overallPooled"`,
      `"pairwisePooled"`, and `"notPooled"`, default is
      `"overallPooled"`. For enrichment designs, the options are:
      `"pooled"`, `"pooledFromFull"` (one subset only), and
      `"notPooled"`, default is `"pooled"`.

  `stratifiedAnalysis`

  :   For enrichment designs, typically a stratified analysis should be
      chosen. For testing means and rates, also a non-stratified
      analysis based on overall data can be performed. For survival
      data, only a stratified analysis is possible (see Brannath et al.,
      2009), default is `TRUE`.

- directionUpper:

  Logical. Specifies the direction of the alternative, only applicable
  for one-sided testing; default is `TRUE` which means that larger
  values of the test statistics yield smaller p-values.

- tolerance:

  The numerical tolerance, default is `1e-06`. Must be a positive
  numeric of length 1.

- stage:

  The stage number (optional). Default: total number of existing stages
  in the data input.

## Value

Returns a [`matrix`](https://rdrr.io/r/base/matrix.html) with `2` rows
and `kMax` columns containing the lower RCI limits in the first row and
the upper RCI limits in the second row, where each column represents a
stage.

## Details

The repeated confidence interval at a given stage of the trial contains
the parameter values that are not rejected using the specified
sequential design. It can be calculated at each stage of the trial and
can thus be used as a monitoring tool.

The repeated confidence intervals are provided up to the specified
stage.

## See also

Other analysis functions:
[`getAnalysisResults()`](https://docs.rpact.org/reference/getAnalysisResults.md),
[`getClosedCombinationTestResults()`](https://docs.rpact.org/reference/getClosedCombinationTestResults.md),
[`getClosedConditionalDunnettTestResults()`](https://docs.rpact.org/reference/getClosedConditionalDunnettTestResults.md),
[`getConditionalPower()`](https://docs.rpact.org/reference/getConditionalPower.md),
[`getConditionalRejectionProbabilities()`](https://docs.rpact.org/reference/getConditionalRejectionProbabilities.md),
[`getFinalConfidenceInterval()`](https://docs.rpact.org/reference/getFinalConfidenceInterval.md),
[`getFinalPValue()`](https://docs.rpact.org/reference/getFinalPValue.md),
[`getRepeatedPValues()`](https://docs.rpact.org/reference/getRepeatedPValues.md),
[`getStageResults()`](https://docs.rpact.org/reference/getStageResults.md),
[`getTestActions()`](https://docs.rpact.org/reference/getTestActions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
design <- getDesignInverseNormal(kMax = 2)
data <- getDataset(
    n      = c( 20,  30),
    means  = c( 50,  51),
    stDevs = c(130, 140)
)
getRepeatedConfidenceIntervals(design, dataInput = data)
} # }
```
