# Dataset

Basic class for datasets.

## Details

`Dataset` is the basic class for

- [`DatasetMeans`](https://docs.rpact.org/reference/DatasetMeans.md),

- [`DatasetRates`](https://docs.rpact.org/reference/DatasetRates.md),

- [`DatasetSurvival`](https://docs.rpact.org/reference/DatasetSurvival.md),
  and

- [`DatasetEnrichmentSurvival`](https://docs.rpact.org/reference/DatasetSurvival.md).

This basic class contains the fields `stages` and `groups` and several
commonly used functions.

## Fields

- `stages`:

  The stage numbers of the trial. Is a numeric vector of length `kMax`
  containing whole numbers.

- `groups`:

  The group numbers. Is a numeric vector.
