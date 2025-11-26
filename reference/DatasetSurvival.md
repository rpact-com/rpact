# Dataset of Survival Data

Class for a dataset of survival data.

## Details

This object cannot be created directly; better use
[`getDataset`](https://docs.rpact.org/reference/getDataset.md) with
suitable arguments to create a dataset of survival data.

## Fields

- `groups`:

  The group numbers. Is a numeric vector.

- `stages`:

  The stage numbers of the trial. Is a numeric vector of length `kMax`
  containing whole numbers.

- `events`:

  The number of events in each group at each stage. Is a numeric vector
  of length number of stages times number of groups.

- `overallEvents`:

  The overall, i.e., cumulative events. Is a numeric vector of length
  number of stages times number of groups containing whole numbers.

- `allocationRatios`:

  The observed allocation ratios. Is a numeric vector of length number
  of stages times number of groups.

- `overallAllocationRatios`:

  The cumulative allocation ratios. Is a numeric vector of length number
  of stages times number of groups.

- `logRanks`:

  The logrank test statistics at each stage of the trial. Is a numeric
  vector of length number of stages times number of groups.

- `overallLogRanks`:

  The overall, i.e., cumulative logrank test statistics. Is a numeric
  vector of length number of stages times number of groups.
