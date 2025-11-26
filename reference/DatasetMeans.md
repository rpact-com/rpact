# Dataset of Means

Class for a dataset of means.

## Details

This object cannot be created directly; better use
[`getDataset`](https://docs.rpact.org/reference/getDataset.md) with
suitable arguments to create a dataset of means.

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

- `means`:

  The means. Is a numeric vector of length number of stages times number
  of groups.

- `stDevs`:

  The standard deviations. Is a numeric vector of length number of
  stages times number of groups.

- `overallSampleSizes`:

  The overall, i.e., cumulative sample sizes. Is a numeric vector of
  length number of stages times number of groups.

- `overallMeans`:

  The overall, i.e., cumulative means. Is a numeric vector of length
  number of stages times number of groups.

- `overallStDevs`:

  The overall, i.e., cumulative standard deviations. Is a numeric vector
  of length number of stages times number of groups.
