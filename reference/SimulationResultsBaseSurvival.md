# Common Survival Simulation Results

Base class for survival simulation results. Obtain specialized instances
with
[`getSimulationSurvival()`](https://docs.rpact.org/reference/getSimulationSurvival.md),
[`getSimulationMultiArmSurvival()`](https://docs.rpact.org/reference/getSimulationMultiArmSurvival.md),
or
[`getSimulationEnrichmentSurvival()`](https://docs.rpact.org/reference/getSimulationEnrichmentSurvival.md).

## Details

Patient-wise dropout summaries use each arm or population's last
contributing analysis, including discontinued groups. Replication
proportions and finishing horizons are averaged over the same nonempty
enrolled-subject denominators. Test-statistic-based simulations leave
these fields unavailable.

## Fields

- `maxNumberOfIterations`:

  The number of simulation iterations. Is a single numeric value
  representing a whole number.

- `seed`:

  The seed used for random number generation. Is a single numeric value.

- `allocationRatioPlanned`:

  The planned allocation ratio (`n1 / n2`) for the groups. For multi-arm
  designs, it is the allocation ratio relating the active arm(s) to the
  control. Is a single positive numeric value.

- `conditionalPower`:

  The conditional power at each stage of the trial. Is a single numeric
  value between 0 and 1.

- `iterations`:

  The number of iterations used for simulations. Is a single numeric
  value representing a whole number.

- `futilityPerStage`:

  The per-stage probabilities of stopping the trial for futility. Is a
  numeric matrix.

- `futilityStop`:

  In simulation results data set: indicates whether trial is stopped for
  futility or not.

- `calcEventsFunction`:

  An optional function that can be entered to define how event size is
  recalculated. By default, recalculation is performed with conditional
  power with specified `minNumberOfEventsPerStage` and
  `maxNumberOfEventsPerStage`.

- `directionUpper`:

  Specifies the direction of the alternative, only applicable for
  one-sided testing. Default is `TRUE` which means that larger values of
  the test statistics yield smaller p-values. Is a single logical value.

- `expectedNumberOfEvents`:

  The expected number of events under specified alternative. Is a
  numeric vector.

- `maxNumberOfEventsPerStage`:

  Determines the maximum number of events per stage for data-driven
  sample size recalculation. Is a numeric vector of length `kMax`
  containing whole numbers.

- `minNumberOfEventsPerStage`:

  Determines the minimum number of events per stage for data-driven
  sample size recalculation. Is a numeric vector of length `kMax`
  containing whole numbers.

- `plannedEvents`:

  Determines the number of cumulated (overall) events in survival
  designs when the interim stages are planned. For two treatment arms,
  is the number of events for both treatment arms. For multi-arm
  designs, refers to the overall number of events for the selected arms
  plus control. Is a numeric vector of length `kMax` containing whole
  numbers.

- `thetaH0`:

  The difference or assumed effect under H0. Is a single numeric value.

- `thetaH1`:

  The assumed effect under the alternative hypothesis. For survival
  designs, refers to the hazard ratio. Is a numeric vector.

- `studyDuration`:

  The study duration for specified effect size. Is a positive numeric
  vector.

- `maxStudyDuration`:

  The maximum study duration in survival designs. Is a numeric vector.

- `observedDropoutProportion1`:

  Mean replication-specific observed dropout proportion in arm 1,
  counting dropout before the event and by the arm or population's last
  contributing analysis. Only replications with a finite finishing
  horizon and positive enrolled-subject denominator contribute; NA if
  none contribute. Available only for patient-wise simulation. Numeric
  vector over scenarios for two-arm trials; in multi-arm trials, arm 1
  is a matrix of active-arm rows and scenario columns (a vector for one
  active arm), and arm 2 is a vector over scenarios for shared control.
  In enrichment trials, both arms have population rows (subpopulations
  followed by the full population) and scenario columns, or a vector for
  one population. Discontinued arms and populations are retained;
  overlapping populations include their shared subjects. There is no
  stage dimension. NULL for test-statistic-based simulation.

- `observedDropoutProportion2`:

  Mean replication-specific observed dropout proportion in arm 2,
  counting dropout before the event and by the arm or population's last
  contributing analysis. Only replications with a finite finishing
  horizon and positive enrolled-subject denominator contribute; NA if
  none contribute. Available only for patient-wise simulation. Numeric
  vector over scenarios for two-arm trials; in multi-arm trials, arm 1
  is a matrix of active-arm rows and scenario columns (a vector for one
  active arm), and arm 2 is a vector over scenarios for shared control.
  In enrichment trials, both arms have population rows (subpopulations
  followed by the full population) and scenario columns, or a vector for
  one population. Discontinued arms and populations are retained;
  overlapping populations include their shared subjects. There is no
  stage dimension. NULL for test-statistic-based simulation.

- `dropoutObservationTime1`:

  Mean finishing analysis time in arm 1 over exactly the replications
  contributing to the corresponding observed dropout proportion, in the
  same time units as accrual and survival. NA if no replication
  contributes. Available only for patient-wise simulation. Numeric
  vector over scenarios for two-arm trials; in multi-arm trials, arm 1
  is a matrix of active-arm rows and scenario columns (a vector for one
  active arm), and arm 2 is a vector over scenarios for shared control.
  In enrichment trials, both arms have population rows (subpopulations
  followed by the full population) and scenario columns, or a vector for
  one population. Discontinued arms and populations are retained;
  overlapping populations include their shared subjects. There is no
  stage dimension. NULL for test-statistic-based simulation.

- `dropoutObservationTime2`:

  Mean finishing analysis time in arm 2 over exactly the replications
  contributing to the corresponding observed dropout proportion, in the
  same time units as accrual and survival. NA if no replication
  contributes. Available only for patient-wise simulation. Numeric
  vector over scenarios for two-arm trials; in multi-arm trials, arm 1
  is a matrix of active-arm rows and scenario columns (a vector for one
  active arm), and arm 2 is a vector over scenarios for shared control.
  In enrichment trials, both arms have population rows (subpopulations
  followed by the full population) and scenario columns, or a vector for
  one population. Discontinued arms and populations are retained;
  overlapping populations include their shared subjects. There is no
  stage dimension. NULL for test-statistic-based simulation.

## See also

[SimulationResultsSurvival](https://docs.rpact.org/reference/SimulationResultsSurvival.md),
[SimulationResultsMultiArmSurvival](https://docs.rpact.org/reference/SimulationResultsMultiArmSurvival.md),
[SimulationResultsEnrichmentSurvival](https://docs.rpact.org/reference/SimulationResultsEnrichmentSurvival.md)
