# Class for Simulation Results Rates

A class for simulation results rates.

## Details

Use
[`getSimulationRates()`](https://docs.rpact.org/reference/getSimulationRates.md)
to create an object of this type.

`SimulationResultsRates` is the basic class for

- `SimulationResultsRates`,

- [`SimulationResultsMultiArmRates`](https://docs.rpact.org/reference/SimulationResultsMultiArmRates.md),
  and

- [`SimulationResultsEnrichmentRates`](https://docs.rpact.org/reference/SimulationResultsEnrichmentRates.md).

## Fields

- `allocationRatioPlanned`:

  The planned allocation ratio (`n1 / n2`) for the groups. For multi-arm
  designs, it is the allocation ratio relating the active arm(s) to the
  control. Is a single positive numeric value.

- `calcSubjectsFunction`:

  An optional function that can be entered to define how sample size is
  recalculated. By default, recalculation is performed with conditional
  power with specified `minNumberOfSubjectsPerStage` and
  `maxNumberOfSubjectsPerStage`.

- `conditionalPower`:

  The conditional power at each stage of the trial. Is a single numeric
  value between 0 and 1.

- `conditionalPowerAchieved`:

  The calculated conditional power, under the assumption of observed or
  assumed effect sizes. Is a numeric matrix.

- `directionUpper`:

  Specifies the direction of the alternative, only applicable for
  one-sided testing. Default is `TRUE` which means that larger values of
  the test statistics yield smaller p-values. Is a single logical value.

- `earlyStop`:

  The probability to stopping the trial either for efficacy or futility.
  Is a numeric vector.

- `effect`:

  The effect for randomly creating normally distributed responses. Is a
  numeric vector of length `kMax`.

- `expectedNumberOfSubjects`:

  The expected number of subjects under specified alternative.

- `futilityPerStage`:

  The per-stage probabilities of stopping the trial for futility. Is a
  numeric matrix.

- `futilityStop`:

  In simulation results data set: indicates whether trial is stopped for
  futility or not.

- `groups`:

  The group numbers. Is a numeric vector.

- `iterations`:

  The number of iterations used for simulations. Is a single numeric
  value representing a whole number.

- `maxNumberOfIterations`:

  The number of simulation iterations. Is a single numeric value
  representing a whole number.

- `maxNumberOfSubjects`:

  The maximum number of subjects for power calculations. Is a numeric
  vector.

- `normalApproximation`:

  Describes if a normal approximation was used when calculating
  p-values. Default for means is `FALSE` and `TRUE` for rates and hazard
  ratio. Is a single logical value.

- `overallReject`:

  The overall rejection probability. Is a numeric vector.

- `pi1`:

  The assumed probability or probabilities in the active treatment group
  in two-group designs, or the alternative probability for a one-group
  design.

- `pi1H1`:

  The assumed probability in the active treatment group for two-group
  designs, or the assumed probability for a one treatment group design,
  for which the conditional power was calculated. Is a single numeric
  value between 0 and 1.

- `pi2`:

  The assumed probability in the reference group for two-group designs.
  Is a single numeric value between 0 and 1.

- `pi2H1`:

  The assumed probability in the reference group for two-group designs,
  for which the conditional power was calculated. Is a single numeric
  value between 0 and 1.

- `plannedSubjects`:

  Determines the number of cumulated (overall) subjects when the interim
  stages are planned. For two treatment arms, is the number of subjects
  for both treatment arms. For multi-arm designs, refers to the number
  of subjects per selected active arm. Is a numeric vector of length
  `kMax` containing whole numbers.

- `rejectPerStage`:

  The probability to reject a hypothesis per stage of the trial. Is a
  numeric matrix.

- `riskRatio`:

  Specifies if the sample size for one-sided testing of H0:
  `pi1 / pi2 = thetaH0` has been calculated. Is a single logical value.

- `sampleSizes`:

  The sample sizes for each group and stage. Is a numeric vector of
  length number of stages times number of groups containing whole
  numbers.

- `seed`:

  The seed used for random number generation. Is a single numeric value.

- `thetaH0`:

  The difference or assumed effect under H0. Is a single numeric value.
