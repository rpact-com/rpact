# Analysis Results Inverse Normal

Class for analysis results results based on an inverse normal design.

## Details

This object cannot be created directly; use
[`getAnalysisResults`](https://rpact-com.github.io/rpact/reference/getAnalysisResults.md)
with suitable arguments to create the analysis results of a inverse
normal design.

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

- `assumedStDev`:

  The assumed standard deviation(s) for means analysis. Is a numeric
  vector.

- `equalVariances`:

  Describes if the variances in two treatment groups are assumed to be
  the same. Is a logical vector of length 1.

- `testActions`:

  The test decisions at each stage of the trial. Is a character vector
  of length `kMax`.

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

- `finalStage`:

  The stage at which the trial ends, either with acceptance or rejection
  of the null hypothesis. Is a numeric vector of length 1.

- `finalPValues`:

  The final p-value that is based on the stage-wise ordering. Is a
  numeric vector of length `kMax` containing values between 0 and 1.

- `finalConfidenceIntervalLowerBounds`:

  The lower bound of the confidence interval that is based on the
  stage-wise ordering. Is a numeric vector of length `kMax`.

- `finalConfidenceIntervalUpperBounds`:

  The upper bound of the confidence interval that is based on the
  stage-wise ordering. Is a numeric vector of length `kMax`.

- `medianUnbiasedEstimates`:

  The calculated median unbiased estimates that are based on the
  stage-wise ordering. Is a numeric vector of length `kMax`.
