# Analysis Results Enrichment Inverse Normal

Class for enrichment analysis results based on a inverse normal design.

## Details

This object cannot be created directly; use
[`getAnalysisResults`](https://rpact-com.github.io/rpact/reference/getAnalysisResults.md)
with suitable arguments to create the enrichment analysis results of an
inverse normal design.

## Fields

- `normalApproximation`:

  Describes if a normal approximation was used when calculating
  p-values. Default for means is `FALSE` and `TRUE` for rates and hazard
  ratio. Is a logical vector of length 1.

- `directionUpper`:

  Specifies the direction of the alternative, only applicable for
  one-sided testing. Default is `TRUE` which means that larger values of
  the test statistics yield smaller p-values. Is a logical vector of
  length 1.

- `thetaH0`:

  The difference or assumed effect under H0. Is a numeric vector of
  length 1.

- `pi1`:

  The assumed probability or probabilities in the active treatment group
  in two-group designs, or the alternative probability for a one-group
  design.

- `pi2`:

  The assumed probability in the reference group for two-group designs.
  Is a numeric vector of length 1 containing a value between 0 and 1.

- `nPlanned`:

  The sample size planned for each of the subsequent stages. Is a
  numeric vector of length `kMax` containing whole numbers.

- `allocationRatioPlanned`:

  The planned allocation ratio (`n1 / n2`) for the groups. For multi-arm
  designs, it is the allocation ratio relating the active arm(s) to the
  control. Is a positive numeric vector of length 1.

- `thetaH1`:

  The assumed effect under the alternative hypothesis. For survival
  designs, refers to the hazard ratio. Is a numeric vector.

- `assumedStDevs`:

  Assumed standard deviations to calculate conditional power in
  multi-arm trials or enrichment designs. Is a numeric vector.

- `piTreatments`:

  The assumed rates in the treatment groups for multi-arm and enrichment
  designs, i.e., designs with multiple subsets.

- `intersectionTest`:

  The multiple test used for intersection hypotheses in closed systems
  of hypotheses. Is a character vector of length 1.

- `varianceOption`:

  Defines the way to calculate the variance in multiple (i.e., \>2)
  treatment arms or population enrichment designs when testing means.
  Available options for multiple arms:
  `"overallPooled", "pairwisePooled", "notPooled"`. Available options
  for enrichment designs: `"pooled", "pooledFromFull", "notPooled"`.

- `conditionalRejectionProbabilities`:

  The probabilities of rejecting the null hypothesis at each stage,
  given the stage has been reached. Is a numeric vector of length `kMax`
  containing values between 0 and 1.

- `conditionalPower`:

  The conditional power at each stage of the trial. Is a numeric vector
  of length 1 containing a value between 0 and 1.

- `repeatedConfidenceIntervalLowerBounds`:

  The lower bound of the confidence intervals that are calculated at any
  stage of the trial. Is a numeric vector of length `kMax`.

- `repeatedConfidenceIntervalUpperBounds`:

  The upper bound of the confidence interval that are calculated at any
  stage of the trial. Is a numeric vector of length `kMax`.

- `repeatedPValues`:

  The p-values that are calculated at any stage of the trial. Is a
  numeric vector of length `kMax` containing values between 0 and 1.

- `piControls`:

  The assumed rates in the control group for enrichment designs, i.e.,
  designs with multiple subsets.

- `stratifiedAnalysis`:

  For enrichment designs, typically a stratified analysis should be
  chosen. When testing means and rates, a non-stratified analysis can be
  performed on overall data. For survival data, only a stratified
  analysis is possible. Is a logical vector of length 1.
