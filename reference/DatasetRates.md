# Dataset of Rates

Class for a dataset of rates.

## Details

This object cannot be created directly; better use
[`getDataset`](https://docs.rpact.org/reference/getDataset.md) with
suitable arguments to create a dataset of rates.

## Fields

- `groups`:

  The group numbers. Is a numeric vector.

- `stages`:

  The stage numbers of the trial. Is a numeric vector of length `kMax`
  containing whole numbers.

- `sampleSizes`:

  The sample sizes for each group and stage. Is a numeric vector of
  length number of stages times number of groups containing whole
  numbers.

- `overallSampleSizes`:

  The overall, i.e., cumulative sample sizes. Is a numeric vector of
  length number of stages times number of groups.

- `events`:

  The number of events in each group at each stage. Is a numeric vector
  of length number of stages times number of groups.

- `overallEvents`:

  The overall, i.e., cumulative events. Is a numeric vector of length
  number of stages times number of groups containing whole numbers.
