# Dataset

Basic class for datasets.

## Details

`Dataset` is the basic class for

- [`DatasetMeans`](https://rpact-com.github.io/rpact/reference/DatasetMeans.md),

- [`DatasetRates`](https://rpact-com.github.io/rpact/reference/DatasetRates.md),

- [`DatasetSurvival`](https://rpact-com.github.io/rpact/reference/DatasetSurvival.md),
  and

- [`DatasetEnrichmentSurvival`](https://rpact-com.github.io/rpact/reference/DatasetSurvival.md).

This basic class contains the fields `stages` and `groups` and several
commonly used functions.

## Fields

- `stages`:

  The stage numbers of the trial. Is a numeric vector of length `kMax`
  containing whole numbers.

- `groups`:

  The group numbers. Is a numeric vector.
