# Trial Design Plan Count Data

Trial design plan for count data.

## Details

This object cannot be created directly; use
[`getSampleSizeCounts()`](https://docs.rpact.org/reference/getSampleSizeCounts.md)
with suitable arguments to create a design plan for a dataset of rates.

## Fields

- `thetaH0`:

  The difference or assumed effect under H0. Is a numeric vector of
  length 1.

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

- `lambda1`:

  The assumed hazard rate in the treatment group. Is a numeric vector of
  length `kMax`.

- `lambda2`:

  The assumed hazard rate in the reference group. Is a numeric vector of
  length 1.

- `lambda`:

  A numeric value or vector that represents the assumed rate of a
  homogeneous Poisson process in the pooled treatment groups Is a
  numeric vector.

- `theta`:

  A vector of standardized effect sizes (theta values). Is a numeric
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

- `maxNumberOfSubjects`:

  The maximum number of subjects for power calculations. Is a numeric
  vector.

- `maxNumberOfSubjects1`:

  The maximum number of subjects in treatment arm 1. Is a numeric
  vector.

- `maxNumberOfSubjects2`:

  The maximum number of subjects in treatment arm 2. Is a numeric
  vector.

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

- `overdispersion`:

  A numeric value that represents the assumed overdispersion of the
  negative binomial distribution Is a numeric vector.

- `fixedExposureTime`:

  If specified, the fixed time of exposure per subject for count data Is
  a numeric vector.

- `accrualTime`:

  The assumed accrual time intervals for the study. Is a numeric vector.

- `accrualIntensity`:

  The absolute accrual intensities. Is a numeric vector of length
  `kMax`.

- `followUpTime`:

  The assumed follow-up time for the study. Is a numeric vector of
  length 1.

- `calendarTime`:

  The calendar time Is a numeric vector.

- `expectedStudyDurationH1`:

  The expected study duration under H1 Is a numeric vector.

- `studyTime`:

  The study time Is a numeric vector.

- `numberOfSubjects`:

  In simulation results data set: The number of subjects under
  consideration when the interim analysis takes place.

- `expectedNumberOfSubjectsH1`:

  The expected number of subjects under H1. Is a numeric vector.

- `informationOverStages`:

  The information over stages Is a numeric vector.

- `expectedInformationH0`:

  The expected information under H0 Is a numeric vector.

- `expectedInformationH01`:

  The expected information under H0/H1 Is a numeric vector.

- `expectedInformationH1`:

  The expected information under H1 Is a numeric vector.

- `maxInformation`:

  The maximum information. Is a numeric vector of length 1 containing a
  whole number.

- `futilityBoundsPValueScale`:

  The futility bounds for each stage of the trial on the p-value scale.
  Is a numeric matrix.
