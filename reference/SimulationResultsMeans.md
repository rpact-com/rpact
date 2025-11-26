# Class for Simulation Results Means

A class for simulation results means.

## Details

Use
[`getSimulationMeans()`](https://docs.rpact.org/reference/getSimulationMeans.md)
to create an object of this type.

`SimulationResultsMeans` is the basic class for

- `SimulationResultsMeans`,

- [`SimulationResultsMultiArmMeans`](https://docs.rpact.org/reference/SimulationResultsMultiArmMeans.md),
  and

- [`SimulationResultsEnrichmentMeans`](https://docs.rpact.org/reference/SimulationResultsEnrichmentMeans.md).

## Fields

- `allocationRatioPlanned`:

  The planned allocation ratio (`n1 / n2`) for the groups. For multi-arm
  designs, it is the allocation ratio relating the active arm(s) to the
  control. Is a positive numeric vector of length 1.

- `alternative`:

  The alternative hypothesis value(s) for testing means. Is a numeric
  vector.

- `calcSubjectsFunction`:

  An optional function that can be entered to define how sample size is
  recalculated. By default, recalculation is performed with conditional
  power with specified `minNumberOfSubjectsPerStage` and
  `maxNumberOfSubjectsPerStage`.

- `conditionalPower`:

  The conditional power at each stage of the trial. Is a numeric vector
  of length 1 containing a value between 0 and 1.

- `conditionalPowerAchieved`:

  The calculated conditional power, under the assumption of observed or
  assumed effect sizes. Is a numeric matrix.

- `directionUpper`:

  Specifies the direction of the alternative, only applicable for
  one-sided testing. Default is `TRUE` which means that larger values of
  the test statistics yield smaller p-values. Is a logical vector of
  length 1.

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

  The number of iterations used for simulations. Is a numeric vector of
  length 1 containing a whole number.

- `maxNumberOfIterations`:

  The number of simulation iterations. Is a numeric vector of length 1
  containing a whole number.

- `maxNumberOfSubjectsPerStage`:

  Determines the maximum number of subjects per stage for data-driven
  sample size recalculation. For two treatment arms, is the number of
  subjects for both treatment arms. For multi-arm designs, is the
  minimum number of subjects per selected active arm. Is a numeric
  vector of length `kMax` containing whole numbers.

- `meanRatio`:

  Specifies if the sample size for one-sided testing of H0:
  `mu1/mu2 = thetaH0` has been calculated. Is a logical vector of length
  1.

- `minNumberOfSubjectsPerStage`:

  Determines the minimum number of subjects per stage for data-driven
  sample size recalculation. For two treatment arms, is the number of
  subjects for both treatment arms. For multi-arm designs, is the
  minimum number of subjects per selected active arm. Is a numeric
  vector of length `kMax` containing whole numbers.

- `normalApproximation`:

  Describes if a normal approximation was used when calculating
  p-values. Default for means is `FALSE` and `TRUE` for rates and hazard
  ratio. Is a logical vector of length 1.

- `overallReject`:

  The overall rejection probability. Is a numeric vector.

- `plannedSubjects`:

  Determines the number of cumulated (overall) subjects when the interim
  stages are planned. For two treatment arms, is the number of subjects
  for both treatment arms. For multi-arm designs, refers to the number
  of subjects per selected active arm. Is a numeric vector of length
  `kMax` containing whole numbers.

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

- `stDev`:

  The standard deviation used for sample size and power calculation. Is
  a numeric vector of length 1.

- `stDevH1`:

  The standard deviation under which the conditional power or sample
  size recalculation is performed. Is a numeric vector of length 1.

- `thetaH0`:

  The difference or assumed effect under H0. Is a numeric vector of
  length 1.

- `thetaH1`:

  The assumed effect under the alternative hypothesis. For survival
  designs, refers to the hazard ratio. Is a numeric vector.
