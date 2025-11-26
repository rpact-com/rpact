# Basic Stage Results

Basic class for stage results.

## Details

`StageResults` is the basic class for

- [`StageResultsMeans`](https://docs.rpact.org/reference/StageResultsMeans.md),

- [`StageResultsRates`](https://docs.rpact.org/reference/StageResultsRates.md),

- [`StageResultsSurvival`](https://docs.rpact.org/reference/StageResultsSurvival.md),

- [`StageResultsMultiArmMeans`](https://docs.rpact.org/reference/StageResultsMultiArmMeans.md),

- [`StageResultsMultiArmRates`](https://docs.rpact.org/reference/StageResultsMultiArmRates.md),

- [`StageResultsMultiArmSurvival`](https://docs.rpact.org/reference/StageResultsMultiArmSurvival.md),

- [`StageResultsEnrichmentMeans`](https://docs.rpact.org/reference/StageResultsEnrichmentMeans.md),

- [`StageResultsEnrichmentRates`](https://docs.rpact.org/reference/StageResultsEnrichmentRates.md),
  and

- [`StageResultsEnrichmentSurvival`](https://docs.rpact.org/reference/StageResultsEnrichmentSurvival.md).

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
