# Power and Average Sample Number Result

Class for power and average sample number (ASN) results.

## Details

This object cannot be created directly; use
[`getPowerAndAverageSampleNumber()`](https://docs.rpact.org/reference/getPowerAndAverageSampleNumber.md)
with suitable arguments to create it.

## Fields

- `nMax`:

  The maximum sample size. Is a numeric vector of length 1 containing a
  whole number.

- `theta`:

  A vector of standardized effect sizes (theta values). Is a numeric
  vector.

- `averageSampleNumber`:

  The average sample number calculated for each value of `theta` or
  `nMax`, if the specified maximum sample size would be exceeded. Is a
  numeric vector.

- `calculatedPower`:

  The calculated power for the given scenario.

- `overallEarlyStop`:

  The overall early stopping probability. Is a numeric vector.

- `earlyStop`:

  The probability to stopping the trial either for efficacy or futility.
  Is a numeric vector.

- `overallReject`:

  The overall rejection probability. Is a numeric vector.

- `rejectPerStage`:

  The probability to reject a hypothesis per stage of the trial. Is a
  numeric matrix.

- `overallFutility`:

  The overall stopping for futility probability. Is a numeric vector.

- `futilityPerStage`:

  The per-stage probabilities of stopping the trial for futility. Is a
  numeric matrix.
