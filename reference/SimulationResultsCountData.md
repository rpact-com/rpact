# Class for Simulation Results Count Data

A class for simulation results count data.

## Details

Use
[`getSimulationCounts()`](https://docs.rpact.org/reference/getSimulationCounts.md)
to create an object of this type.

## Fields

- `accrualIntensity`:

  The absolute accrual intensities. Is a numeric vector of length
  `kMax`.

- `accrualTime`:

  The assumed accrual time intervals for the study. Is a numeric vector.

- `allocationRatioPlanned`:

  The planned allocation ratio (`n1 / n2`) for the groups. For multi-arm
  designs, it is the allocation ratio relating the active arm(s) to the
  control. Is a positive numeric vector of length 1.

- `conditionalPower`:

  The conditional power at each stage of the trial. Is a numeric vector
  of length 1 containing a value between 0 and 1.

- `directionUpper`:

  Specifies the direction of the alternative, only applicable for
  one-sided testing. Default is `TRUE` which means that larger values of
  the test statistics yield smaller p-values. Is a logical vector of
  length 1.

- `earlyStop`:

  The probability to stopping the trial either for efficacy or futility.
  Is a numeric vector.

- `expectedNumberOfSubjects`:

  The expected number of subjects under specified alternative.

- `futilityPerStage`:

  The per-stage probabilities of stopping the trial for futility. Is a
  numeric matrix.

- `groups`:

  The group numbers. Is a numeric vector.

- `iterations`:

  The number of iterations used for simulations. Is a numeric vector of
  length 1 containing a whole number.

- `maxNumberOfIterations`:

  The number of simulation iterations. Is a numeric vector of length 1
  containing a whole number.

- `overallReject`:

  The overall rejection probability. Is a numeric vector.

- `rejectPerStage`:

  The probability to reject a hypothesis per stage of the trial. Is a
  numeric matrix.

- `sampleSizes`:

  The sample sizes for each group and stage. Is a numeric vector of
  length number of stages times number of groups containing whole
  numbers.

- `seed`:

  The seed used for random number generation. Is a numeric vector of
  length 1.

- `thetaH0`:

  The difference or assumed effect under H0. Is a numeric vector of
  length 1.
