# Get Stage Results

Returns summary statistics and p-values for a given data set and a given
design.

## Usage

``` r
getStageResults(
  design,
  dataInput,
  ...,
  stage = NA_integer_,
  directionUpper = NA
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

  Further (optional) arguments to be passed:

  `thetaH0`

  :   The null hypothesis value, default is `0` for the normal and the
      binary case (testing means and rates, respectively), it is `1` for
      the survival case (testing the hazard ratio).  
        
      For non-inferiority designs, `thetaH0` is the non-inferiority
      bound. That is, in case of (one-sided) testing of

      - *means*: a value `!= 0` (or a value `!= 1` for testing the mean
        ratio) can be specified.

      - *rates*: a value `!= 0` (or a value `!= 1` for testing the risk
        ratio `pi1 / pi2`) can be specified.

      - *survival data*: a bound for testing H0:
        `hazard ratio = thetaH0 != 1` can be specified.

      For testing a rate in one sample, a value `thetaH0` in (0, 1) has
      to be specified for defining the null hypothesis H0:
      `pi = thetaH0`.

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

- stage:

  The stage number (optional). Default: total number of existing stages
  in the data input.

- directionUpper:

  Logical. Specifies the direction of the alternative, only applicable
  for one-sided testing; default is `TRUE` which means that larger
  values of the test statistics yield smaller p-values.

## Value

Returns a
[`StageResults`](https://docs.rpact.org/reference/StageResults.md)
object.

- [`names`](https://docs.rpact.org/reference/names.StageResults.md) to
  obtain the field names,

- [`print()`](https://docs.rpact.org/reference/print.FieldSet.md) to
  print the object,

- [`summary()`](https://docs.rpact.org/reference/summary.ParameterSet.md)
  to display a summary of the object,

- [`plot()`](https://docs.rpact.org/reference/plot.StageResults.md) to
  plot the object,

- [`as.data.frame()`](https://docs.rpact.org/reference/as.data.frame.StageResults.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://docs.rpact.org/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

Calculates and returns the stage results of the specified design and
data input at the specified stage.

## How to get help for generic functions

Click on the link of a generic in the list above to go directly to the
help documentation of the `rpact` specific implementation of the
generic. Note that you can use the R function
[`methods`](https://rdrr.io/r/utils/methods.html) to get all the methods
of a generic and to identify the object specific name of it, e.g., use
`methods("plot")` to get all the methods for the `plot` generic. There
you can find, e.g., `plot.AnalysisResults` and obtain the specific help
documentation linked above by typing
[`?plot.AnalysisResults`](https://docs.rpact.org/reference/plot.AnalysisResults.md).

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
[`getTestActions()`](https://docs.rpact.org/reference/getTestActions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
design <- getDesignInverseNormal()
dataRates <- getDataset(
    n1      = c(10, 10),
    n2      = c(20, 20),
    events1 = c( 8, 10),
    events2 = c(10, 16))
getStageResults(design, dataRates)
} # }
```
