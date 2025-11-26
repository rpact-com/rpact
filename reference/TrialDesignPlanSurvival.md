# Trial Design Plan Survival

Trial design plan for survival data.

## Details

This object cannot be created directly; use
[`getSampleSizeSurvival()`](https://docs.rpact.org/reference/getSampleSizeSurvival.md)
with suitable arguments to create a design plan for a dataset of
survival data.

## Fields

- `thetaH0`:

  The difference or assumed effect under H0. Is a numeric vector of
  length 1.

- `typeOfComputation`:

  The type of computation used, either `"Schoenfeld", "Freedman"`, or
  `"HsiehFreedman"`.

- `directionUpper`:

  Specifies the direction of the alternative, only applicable for
  one-sided testing. Default is `TRUE` which means that larger values of
  the test statistics yield smaller p-values. Is a logical vector of
  length 1.

- `pi1`:

  The assumed event rate in the treatment group. Is a numeric vector of
  length `kMax` containing values between 0 and 1.

- `pi2`:

  The assumed event rate in the control group. Is a numeric vector of
  length 1 containing a value between 0 and 1.

- `median1`:

  The assumed median survival time in the treatment group. Is a numeric
  vector.

- `median2`:

  The assumed median survival time in the reference group. Is a numeric
  vector of length 1.

- `lambda1`:

  The assumed hazard rate in the treatment group. Is a numeric vector of
  length `kMax`.

- `lambda2`:

  The assumed hazard rate in the reference group. Is a numeric vector of
  length 1.

- `hazardRatio`:

  The hazard ratios under consideration. Is a numeric vector of length
  `kMax`.

- `maxNumberOfSubjects`:

  The maximum number of subjects for power calculations. Is a numeric
  vector.

- `maxNumberOfSubjects1`:

  The maximum number of subjects in treatment arm 1. Is a numeric
  vector.

- `maxNumberOfSubjects2`:

  The maximum number of subjects in treatment arm 2. Is a numeric
  vector.

- `maxNumberOfEvents`:

  The maximum number of events for power calculations. Is a positive
  numeric vector of length `kMax`.

- `allocationRatioPlanned`:

  The planned allocation ratio (`n1 / n2`) for the groups. For multi-arm
  designs, it is the allocation ratio relating the active arm(s) to the
  control. Is a positive numeric vector of length 1.

- `optimumAllocationRatio`:

  The allocation ratio that is optimum with respect to the overall
  sample size at given power. Is a logical vector of length 1.

- `accountForObservationTimes`:

  If `FALSE`, only the event rates are used for the calculation of the
  maximum number of subjects. Is a logical vector of length 1.

- `eventTime`:

  The assumed time under which the event rates are calculated. Is a
  numeric vector of length 1.

- `accrualTime`:

  The assumed accrual time intervals for the study. Is a numeric vector.

- `totalAccrualTime`:

  The total accrual time, i.e., the maximum of `accrualTime`. Is a
  positive numeric vector of length 1.

- `accrualIntensity`:

  The absolute accrual intensities. Is a numeric vector of length
  `kMax`.

- `accrualIntensityRelative`:

  The relative accrual intensities.

- `kappa`:

  The shape of the Weibull distribution if `kappa!=1`. Is a numeric
  vector of length 1.

- `piecewiseSurvivalTime`:

  The time intervals for the piecewise definition of the exponential
  survival time cumulative distribution function. Is a numeric vector.

- `followUpTime`:

  The assumed follow-up time for the study. Is a numeric vector of
  length 1.

- `dropoutRate1`:

  The assumed drop-out rate in the treatment group. Is a numeric vector
  of length 1 containing a value between 0 and 1.

- `dropoutRate2`:

  The assumed drop-out rate in the control group. Is a numeric vector of
  length 1 containing a value between 0 and 1.

- `dropoutTime`:

  The assumed time for drop-out rates in the control and treatment
  group. Is a numeric vector of length 1.

- `chi`:

  The calculated event probability at end of trial. Is a numeric vector.

- `expectedNumberOfEvents`:

  The expected number of events under specified alternative. Is a
  numeric vector.

- `eventsFixed`:

  The number of events in a fixed sample size design. Is a numeric
  vector.

- `nFixed`:

  The sample size in a fixed (one-stage) design. Is a positive numeric
  vector.

- `nFixed1`:

  The sample size in treatment arm 1 in a fixed (one-stage) design. Is a
  positive numeric vector.

- `nFixed2`:

  The sample size in treatment arm 2 in a fixed (one-stage) design. Is a
  positive numeric vector.

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

- `informationRates`:

  The information rates (that must be fixed prior to the trial), default
  is `(1:kMax) / kMax`. Is a numeric vector of length `kMax` containing
  values between 0 and 1.

- `analysisTime`:

  The estimated time of analysis. Is a numeric matrix.

- `studyDurationH1`:

  The study duration under the alternative hypothesis. Is a positive
  numeric vector.

- `studyDuration`:

  The study duration for specified effect size. Is a positive numeric
  vector.

- `maxStudyDuration`:

  The maximum study duration in survival designs. Is a numeric vector.

- `eventsPerStage`:

  Deprecated: use `singleEventsPerStage` or `cumulativeEventsPerStage`
  instead Is a numeric matrix.

- `singleEventsPerStage`:

  The single number of events per stage. Is a numeric matrix.

- `cumulativeEventsPerStage`:

  The cumulative number of events per stage. Is a numeric matrix.

- `expectedEventsH0`:

  The expected number of events under H0. Is a numeric vector.

- `expectedEventsH01`:

  The expected number of events under a value between H0 and H1. Is a
  numeric vector.

- `expectedEventsH1`:

  The expected number of events under H1. Is a numeric vector.

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

- `expectedNumberOfSubjectsH1`:

  The expected number of subjects under H1. Is a numeric vector.

- `expectedNumberOfSubjects`:

  The expected number of subjects under specified alternative.

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
