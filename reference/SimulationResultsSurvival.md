# Class for Simulation Results Survival

A class for simulation results survival.

## Details

Use
[`getSimulationSurvival()`](https://rpact-com.github.io/rpact/reference/getSimulationSurvival.md)
to create an object of this type.

`SimulationResultsSurvival` is the basic class for

- `SimulationResultsSurvival`,

- [`SimulationResultsMultiArmSurvival`](https://rpact-com.github.io/rpact/reference/SimulationResultsMultiArmSurvival.md),
  and

- [`SimulationResultsEnrichmentSurvival`](https://rpact-com.github.io/rpact/reference/SimulationResultsEnrichmentSurvival.md).

## Fields

- `accrualIntensity`:

  The absolute accrual intensities. Is a numeric vector of length
  `kMax`.

- `accrualTime`:

  The assumed accrual time intervals for the study. Is a numeric vector.

- `allocation1`:

  The number of subjects to be assigned to treatment 1 in subsequent
  order. Is a numeric vector of length 1 containing a whole number.

- `allocation2`:

  The number of subjects to be assigned to treatment 2 in subsequent
  order. Is a numeric vector of length 1 containing a whole number.

- `allocationRatioPlanned`:

  The planned allocation ratio (`n1 / n2`) for the groups. For multi-arm
  designs, it is the allocation ratio relating the active arm(s) to the
  control. Is a positive numeric vector of length 1.

- `calcEventsFunction`:

  An optional function that can be entered to define how event size is
  recalculated. By default, recalculation is performed with conditional
  power with specified `minNumberOfEventsPerStage` and
  `maxNumberOfEventsPerStage`.

- `conditionalPower`:

  The conditional power at each stage of the trial. Is a numeric vector
  of length 1 containing a value between 0 and 1.

- `conditionalPowerAchieved`:

  The calculated conditional power, under the assumption of observed or
  assumed effect sizes. Is a numeric matrix.

- `cumulativeEventsPerStage`:

  The cumulative number of events per stage. Is a numeric matrix.

- `directionUpper`:

  Specifies the direction of the alternative, only applicable for
  one-sided testing. Default is `TRUE` which means that larger values of
  the test statistics yield smaller p-values. Is a logical vector of
  length 1.

- `dropoutRate1`:

  The assumed drop-out rate in the treatment group. Is a numeric vector
  of length 1 containing a value between 0 and 1.

- `dropoutRate2`:

  The assumed drop-out rate in the control group. Is a numeric vector of
  length 1 containing a value between 0 and 1.

- `dropoutTime`:

  The assumed time for drop-out rates in the control and treatment
  group. Is a numeric vector of length 1.

- `earlyStop`:

  The probability to stopping the trial either for efficacy or futility.
  Is a numeric vector.

- `eventsNotAchieved`:

  The simulated number of cases how often the number of events was not
  reached. Is a numeric matrix.

- `eventTime`:

  The assumed time under which the event rates are calculated. Is a
  numeric vector of length 1.

- `expectedNumberOfEvents`:

  The expected number of events under specified alternative. Is a
  numeric vector.

- `expectedNumberOfSubjects`:

  The expected number of subjects under specified alternative.

- `futilityPerStage`:

  The per-stage probabilities of stopping the trial for futility. Is a
  numeric matrix.

- `futilityStop`:

  In simulation results data set: indicates whether trial is stopped for
  futility or not.

- `hazardRatio`:

  The hazard ratios under consideration. Is a numeric vector of length
  `kMax`.

- `iterations`:

  The number of iterations used for simulations. Is a numeric vector of
  length 1 containing a whole number.

- `kappa`:

  The shape of the Weibull distribution if `kappa!=1`. Is a numeric
  vector of length 1.

- `lambda1`:

  The assumed hazard rate in the treatment group. Is a numeric vector of
  length `kMax`.

- `lambda2`:

  The assumed hazard rate in the reference group. Is a numeric vector of
  length 1.

- `maxNumberOfEventsPerStage`:

  Determines the maximum number of events per stage for data-driven
  sample size recalculation. Is a numeric vector of length `kMax`
  containing whole numbers.

- `maxNumberOfIterations`:

  The number of simulation iterations. Is a numeric vector of length 1
  containing a whole number.

- `maxNumberOfSubjects`:

  The maximum number of subjects for power calculations. Is a numeric
  vector.

- `median1`:

  The assumed median survival time in the treatment group. Is a numeric
  vector.

- `median2`:

  The assumed median survival time in the reference group. Is a numeric
  vector of length 1.

- `minNumberOfEventsPerStage`:

  Determines the minimum number of events per stage for data-driven
  sample size recalculation. Is a numeric vector of length `kMax`
  containing whole numbers.

- `numberOfSubjects`:

  In simulation results data set: The number of subjects under
  consideration when the interim analysis takes place.

- `numberOfSubjects1`:

  In simulation results data set: The number of subjects under
  consideration in treatment arm 1 when the interim analysis takes
  place.

- `numberOfSubjects2`:

  In simulation results data set: The number of subjects under
  consideration in treatment arm 2 when the interim analysis takes
  place.

- `overallReject`:

  The overall rejection probability. Is a numeric vector.

- `pi1`:

  The assumed event rate in the treatment group. Is a numeric vector of
  length `kMax` containing values between 0 and 1.

- `pi2`:

  The assumed event rate in the control group. Is a numeric vector of
  length 1 containing a value between 0 and 1.

- `piecewiseSurvivalTime`:

  The time intervals for the piecewise definition of the exponential
  survival time cumulative distribution function. Is a numeric vector.

- `plannedEvents`:

  Determines the number of cumulated (overall) events in survival
  designs when the interim stages are planned. For two treatment arms,
  is the number of events for both treatment arms. For multi-arm
  designs, refers to the overall number of events for the selected arms
  plus control. Is a numeric vector of length `kMax` containing whole
  numbers.

- `rejectPerStage`:

  The probability to reject a hypothesis per stage of the trial. Is a
  numeric matrix.

- `seed`:

  The seed used for random number generation. Is a numeric vector of
  length 1.

- `singleEventsPerStage`:

  The single number of events per stage. Is a numeric matrix.

- `studyDuration`:

  The study duration for specified effect size. Is a positive numeric
  vector.

- `thetaH0`:

  The difference or assumed effect under H0. Is a numeric vector of
  length 1.

- `thetaH1`:

  The assumed effect under the alternative hypothesis. For survival
  designs, refers to the hazard ratio. Is a numeric vector.
