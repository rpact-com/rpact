# Get Final Confidence Interval

Returns the final confidence interval for the parameter of interest. It
is based on the prototype case, i.e., the test for testing a mean for
normally distributed variables.

## Usage

``` r
getFinalConfidenceInterval(
  design,
  dataInput,
  ...,
  directionUpper = NA,
  thetaH0 = NA_real_,
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

  Further (optional) arguments to be passed:

  `normalApproximation`

  :   The type of computation of the p-values. Default is `FALSE` for
      testing means (i.e., the t test is used) and TRUE for testing
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

  `stdErrorEstimate`

  :   Estimate of standard error for calculation of final confidence
      intervals for comparing rates in two treatment groups, default is
      `"pooled"`.

- directionUpper:

  Logical. Specifies the direction of the alternative, only applicable
  for one-sided testing; default is `TRUE` which means that larger
  values of the test statistics yield smaller p-values.

- thetaH0:

  The null hypothesis value, default is `0` for the normal and the
  binary case (testing means and rates, respectively), it is `1` for the
  survival case (testing the hazard ratio).  
    
  For non-inferiority designs, `thetaH0` is the non-inferiority bound.
  That is, in case of (one-sided) testing of

  - *means*: a value `!= 0` (or a value `!= 1` for testing the mean
    ratio) can be specified.

  - *rates*: a value `!= 0` (or a value `!= 1` for testing the risk
    ratio `pi1 / pi2`) can be specified.

  - *survival data*: a bound for testing H0:
    `hazard ratio = thetaH0 != 1` can be specified.

  - *count data*: a bound for testing H0:
    `lambda1 / lambda2 = thetaH0 != 1` can be specified.

  For testing a rate in one sample, a value `thetaH0` in (0, 1) has to
  be specified for defining the null hypothesis H0: `pi = thetaH0`.

- tolerance:

  The numerical tolerance, default is `1e-06`. Must be a positive
  numeric of length 1.

- stage:

  The stage number (optional). Default: total number of existing stages
  in the data input.

## Value

Returns a [`list`](https://rdrr.io/r/base/list.html) containing

- `finalStage`,

- `medianUnbiased`,

- `finalConfidenceInterval`,

- `medianUnbiasedGeneral`, and

- `finalConfidenceIntervalGeneral`.

## Details

Depending on `design` and `dataInput` the final confidence interval and
median unbiased estimate that is based on the stage-wise ordering of the
sample space will be calculated and returned. Additionally, a
non-standardized ("general") version is provided, the estimated standard
deviation must be used to obtain the confidence interval for the
parameter of interest.

For the inverse normal combination test design with more than two
stages, a warning informs that the validity of the confidence interval
is theoretically shown only if no sample size change was performed.

## See also

Other analysis functions:
[`getAnalysisResults()`](https://docs.rpact.org/reference/getAnalysisResults.md),
[`getClosedCombinationTestResults()`](https://docs.rpact.org/reference/getClosedCombinationTestResults.md),
[`getClosedConditionalDunnettTestResults()`](https://docs.rpact.org/reference/getClosedConditionalDunnettTestResults.md),
[`getConditionalPower()`](https://docs.rpact.org/reference/getConditionalPower.md),
[`getConditionalRejectionProbabilities()`](https://docs.rpact.org/reference/getConditionalRejectionProbabilities.md),
[`getFinalPValue()`](https://docs.rpact.org/reference/getFinalPValue.md),
[`getRepeatedConfidenceIntervals()`](https://docs.rpact.org/reference/getRepeatedConfidenceIntervals.md),
[`getRepeatedPValues()`](https://docs.rpact.org/reference/getRepeatedPValues.md),
[`getStageResults()`](https://docs.rpact.org/reference/getStageResults.md),
[`getTestActions()`](https://docs.rpact.org/reference/getTestActions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
design <- getDesignInverseNormal(kMax = 2)
data <- getDataset(
    n = c(20, 30),
    means = c(50, 51),
    stDevs = c(130, 140)
)
getFinalConfidenceInterval(design, dataInput = data)
} # }
```
