# Stage Results Enrichment Means

Class for stage results of enrichment means data

## Details

This object cannot be created directly; use `getStageResults` with
suitable arguments to create the stage results of a dataset of
enrichment means.

## Fields

- `stages`:

  The stage numbers of the trial. Is a numeric vector of length `kMax`
  containing whole numbers.

- `thetaH0`:

  The difference or assumed effect under H0. Is a numeric vector of
  length 1.

- `direction`:

  Specifies the direction of the alternative, is either "upper" or
  "lower". Only applicable for one-sided testing.

- `normalApproximation`:

  Describes if a normal approximation was used when calculating
  p-values. Default for means is `FALSE` and `TRUE` for rates and hazard
  ratio. Is a logical vector of length 1.

- `directionUpper`:

  Specifies the direction of the alternative, only applicable for
  one-sided testing. Default is `TRUE` which means that larger values of
  the test statistics yield smaller p-values. Is a logical vector of
  length 1.

- `varianceOption`:

  Defines the way to calculate the variance in multiple (i.e., \>2)
  treatment arms or population enrichment designs when testing means.
  Available options for multiple arms:
  `"overallPooled", "pairwisePooled", "notPooled"`. Available options
  for enrichment designs: `"pooled", "pooledFromFull", "notPooled"`.

- `intersectionTest`:

  The multiple test used for intersection hypotheses in closed systems
  of hypotheses. Is a character vector of length 1.

- `testStatistics`:

  The stage-wise test statistics. Is a numeric vector of length `kMax`.

- `overallTestStatistics`:

  The overall, i.e., cumulated test statistics. Is a numeric vector of
  length `kMax`.

- `pValues`:

  The stage-wise p-values. Is a numeric vector of length `kMax`
  containing values between 0 and 1.

- `overallPValues`:

  The overall, i.e., cumulated p-values. Is a numeric vector of length
  `kMax` containing values between 0 and 1.

- `overallStDevs`:

  The overall, i.e., cumulative standard deviations. Is a numeric vector
  of length number of stages times number of groups.

- `overallPooledStDevs`:

  The overall pooled standard deviations. Is a numeric matrix.

- `separatePValues`:

  The p-values from the separate stages. Is a numeric matrix.

- `effectSizes`:

  The stage-wise effect sizes. Is a numeric vector of length `kMax`.

- `singleStepAdjustedPValues`:

  The adjusted p-value for testing multiple hypotheses per stage of the
  trial.

- `stratifiedAnalysis`:

  For enrichment designs, typically a stratified analysis should be
  chosen. When testing means and rates, a non-stratified analysis can be
  performed on overall data. For survival data, only a stratified
  analysis is possible. Is a logical vector of length 1.

- `combInverseNormal`:

  The test statistics over stages for the inverse normal test. Is a
  numeric vector of length `kMax`.

- `combFisher`:

  The test statistics over stages for Fisher's combination test. Is a
  numeric vector of length `kMax` containing values between 0 and 1.

- `weightsFisher`:

  The weights for Fisher's combination test. Is a numeric vector of
  length `kMax`.

- `weightsInverseNormal`:

  The weights for the inverse normal statistic. Is a numeric vector of
  length `kMax`.
