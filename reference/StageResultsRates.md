# Stage Results of Rates

Class for stage results of rates.

## Details

This object cannot be created directly; use `getStageResults` with
suitable arguments to create the stage results of a dataset of rates.

## Fields

- `stages`:

  The stage numbers of the trial. Is a numeric vector of length `kMax`
  containing whole numbers.

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

- `effectSizes`:

  The stage-wise effect sizes. Is a numeric vector of length `kMax`.

- `direction`:

  Specifies the direction of the alternative, is either "upper" or
  "lower". Only applicable for one-sided testing.

- `testActions`:

  The test decisions at each stage of the trial. Is a character vector
  of length `kMax`.

- `thetaH0`:

  The difference or assumed effect under H0. Is a numeric vector of
  length 1.

- `normalApproximation`:

  Describes if a normal approximation was used when calculating
  p-values. Default for means is `FALSE` and `TRUE` for rates and hazard
  ratio. Is a logical vector of length 1.

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

- `...`:

  Names of `dataInput`.
