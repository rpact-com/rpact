# Stage Results Enrichment Rates

Class for stage results of enrichment rates data.

## Details

This object cannot be created directly; use `getStageResults` with
suitable arguments to create the stage results of a dataset of
enrichment rates.

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
