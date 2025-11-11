# Trial Design Plan Means

Trial design plan for means.

## Details

This object cannot be created directly; use
[`getSampleSizeMeans()`](https://rpact-com.github.io/rpact/reference/getSampleSizeMeans.md)
with suitable arguments to create a design plan for a dataset of means.

## Fields

- `meanRatio`:

  Specifies if the sample size for one-sided testing of H0:
  `mu1/mu2 = thetaH0` has been calculated. Is a logical vector of length
  1.

- `thetaH0`:

  The difference or assumed effect under H0. Is a numeric vector of
  length 1.

- `normalApproximation`:

  Describes if a normal approximation was used when calculating
  p-values. Default for means is `FALSE` and `TRUE` for rates and hazard
  ratio. Is a logical vector of length 1.

- `alternative`:

  The alternative hypothesis value(s) for testing means. Is a numeric
  vector.

- `stDev`:

  The standard deviation used for sample size and power calculation. Is
  a numeric vector of length 1.

- `groups`:

  The group numbers. Is a numeric vector.

- `allocationRatioPlanned`:

  The planned allocation ratio (`n1 / n2`) for the groups. For multi-arm
  designs, it is the allocation ratio relating the active arm(s) to the
  control. Is a positive numeric vector of length 1.

- `optimumAllocationRatio`:

  The allocation ratio that is optimum with respect to the overall
  sample size at given power. Is a logical vector of length 1.

- `directionUpper`:

  Specifies the direction of the alternative, only applicable for
  one-sided testing. Default is `TRUE` which means that larger values of
  the test statistics yield smaller p-values. Is a logical vector of
  length 1.

- `effect`:

  The effect for randomly creating normally distributed responses. Is a
  numeric vector of length `kMax`.

- `overallReject`:

  The overall rejection probability. Is a numeric vector.

- `rejectPerStage`:

  The probability to reject a hypothesis per stage of the trial. Is a
  numeric matrix.

- `futilityStop`:

  In simulation results data set: indicates whether trial is stopped for
  futility or not.

- `futilityPerStage`:

  The per-stage probabilities of stopping the trial for futility. Is a
  numeric matrix.

- `earlyStop`:

  The probability to stopping the trial either for efficacy or futility.
  Is a numeric vector.

- `expectedNumberOfSubjects`:

  The expected number of subjects under specified alternative.

- `nFixed`:

  The sample size in a fixed (one-stage) design. Is a positive numeric
  vector.

- `nFixed1`:

  The sample size in treatment arm 1 in a fixed (one-stage) design. Is a
  positive numeric vector.

- `nFixed2`:

  The sample size in treatment arm 2 in a fixed (one-stage) design. Is a
  positive numeric vector.

- `informationRates`:

  The information rates (that must be fixed prior to the trial), default
  is `(1:kMax) / kMax`. Is a numeric vector of length `kMax` containing
  values between 0 and 1.

- `maxNumberOfSubjects`:

  The maximum number of subjects for power calculations. Is a numeric
  vector.

- `maxNumberOfSubjects1`:

  The maximum number of subjects in treatment arm 1. Is a numeric
  vector.

- `maxNumberOfSubjects2`:

  The maximum number of subjects in treatment arm 2. Is a numeric
  vector.

- `numberOfSubjects`:

  In simulation results data set: The number of subjects under
  consideration when the interim analysis takes place.

- `numberOfSubjects1`:

  In simulation results data set: The number of subjects under
  consideration in treatment arm 1 when the interim analysis takes
  place.

- `numberOfSubjects2`:

  In simulation results data set: The number of subjects under
  consideration in treatment arm 2 when the interim analysis takes
  place.

- `expectedNumberOfSubjectsH0`:

  The expected number of subjects under H0. Is a numeric vector.

- `expectedNumberOfSubjectsH01`:

  The expected number of subjects under a value between H0 and H1. Is a
  numeric vector.

- `expectedNumberOfSubjectsH1`:

  The expected number of subjects under H1. Is a numeric vector.

- `criticalValuesEffectScale`:

  The critical values for each stage of the trial on the effect size
  scale.

- `criticalValuesEffectScaleLower`:

  The lower critical values for each stage of the trial on the effect
  size scale. Is a numeric matrix.

- `criticalValuesEffectScaleUpper`:

  The upper critical values for each stage of the trial on the effect
  size scale. Is a numeric matrix.

- `criticalValuesPValueScale`:

  The critical values for each stage of the trial on the p-value scale.

- `futilityBoundsEffectScale`:

  The futility bounds for each stage of the trial on the effect size
  scale. Is a numeric matrix.

- `futilityBoundsEffectScaleLower`:

  The lower futility bounds for each stage of the trial on the effect
  size scale. Is a numeric matrix.

- `futilityBoundsEffectScaleUpper`:

  The upper futility bounds for each stage of the trial on the effect
  size scale. Is a numeric matrix.

- `futilityBoundsPValueScale`:

  The futility bounds for each stage of the trial on the p-value scale.
  Is a numeric matrix.
