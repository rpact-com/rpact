# Stage Results Multi Arm Survival

Class for stage results of multi arm survival data

## Details

This object cannot be created directly; use `getStageResults` with
suitable arguments to create the stage results of a dataset of multi arm
survival.

## Fields

- `stages`:

  The stage numbers of the trial. Is a numeric vector of length `kMax`
  containing whole numbers.

- `testStatistics`:

  The stage-wise test statistics. Is a numeric vector of length `kMax`.

- `pValues`:

  The stage-wise p-values. Is a numeric vector of length `kMax`
  containing values between 0 and 1.

- `combInverseNormal`:

  The test statistics over stages for the inverse normal test. Is a
  numeric vector of length `kMax`.

- `combFisher`:

  The test statistics over stages for Fisher's combination test. Is a
  numeric vector of length `kMax` containing values between 0 and 1.

- `effectSizes`:

  The stage-wise effect sizes. Is a numeric vector of length `kMax`.

- `testActions`:

  The test decisions at each stage of the trial. Is a character vector
  of length `kMax`.

- `weightsFisher`:

  The weights for Fisher's combination test. Is a numeric vector of
  length `kMax`.

- `weightsInverseNormal`:

  The weights for the inverse normal statistic. Is a numeric vector of
  length `kMax`.

- `combInverseNormal`:

  The test statistics over stages for the inverse normal test. Is a
  numeric vector of length `kMax`.

- `combFisher`:

  The test statistics over stages for Fisher's combination test. Is a
  numeric vector of length `kMax` containing values between 0 and 1.

- `overallTestStatistics`:

  The overall, i.e., cumulated test statistics. Is a numeric vector of
  length `kMax`.

- `overallPValues`:

  The overall, i.e., cumulated p-values. Is a numeric vector of length
  `kMax` containing values between 0 and 1.

- `testStatistics`:

  The stage-wise test statistics. Is a numeric vector of length `kMax`.

- `separatePValues`:

  The p-values from the separate stages. Is a numeric matrix.

- `effectSizes`:

  The stage-wise effect sizes. Is a numeric vector of length `kMax`.

- `singleStepAdjustedPValues`:

  The adjusted p-value for testing multiple hypotheses per stage of the
  trial.

- `intersectionTest`:

  The multiple test used for intersection hypotheses in closed systems
  of hypotheses. Is a character vector of length 1.

- `directionUpper`:

  Specifies the direction of the alternative, only applicable for
  one-sided testing. Default is `TRUE` which means that larger values of
  the test statistics yield smaller p-values. Is a logical vector of
  length 1.
