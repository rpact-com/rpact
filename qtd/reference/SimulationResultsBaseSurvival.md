# Common Survival Simulation Results

Base class for survival simulation results. Obtain specialized instances
with
[`getSimulationSurvival()`](https://docs.rpact.org/qtd/reference/getSimulationSurvival.md),
[`getSimulationMultiArmSurvival()`](https://docs.rpact.org/qtd/reference/getSimulationMultiArmSurvival.md),
or
[`getSimulationEnrichmentSurvival()`](https://docs.rpact.org/qtd/reference/getSimulationEnrichmentSurvival.md).

## Details

Patient-wise dropout summaries use each stage's analysis time.
Replication proportions are averaged over nonempty enrolled-subject
denominators at that stage. Stopped trials and discontinued groups do
not contribute to later stages. The fields `dropoutObservationTime1` and
`dropoutObservationTime2` remain available for direct access but are
omitted from print and summary output. They average analysis times over
exactly the replications contributing to the corresponding dropout
proportion. They can therefore differ from the trial-level
`analysisTime` after arm or population selection, or when an
enrolled-subject denominator is empty. Contributing arms and populations
share the same analysis time within each replication and stage.
Test-statistic-based simulations leave these fields unavailable.

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

  Mean replication-specific cumulative observed dropout proportion in
  arm 1 at each stage, counting dropout before the event and by that
  stage's analysis time. Only replications contributing at that stage
  with a finite analysis time and positive enrolled-subject denominator
  contribute; NA if none contribute. Stopped trials and discontinued
  groups do not contribute to later stages; overlapping populations
  include their shared subjects. Numeric matrices with stage rows and
  scenario columns for two-arm trials. In multi-arm trials, arm 1 has
  dimensions stage, scenario, and active arm; arm 2 is a
  stage-by-scenario matrix for shared control. In enrichment trials,
  both arms have dimensions stage, scenario, and population
  (subpopulations followed by the full population). A single active arm
  or population uses a stage-by-scenario matrix. Stage and scenario
  dimensions are always retained. NULL for test-statistic-based
  simulation. Both proportions are hidden in print and summary output
  when both input dropout rates are zero, but remain in the object.

- `observedDropoutProportion2`:

  Mean replication-specific cumulative observed dropout proportion in
  arm 2 at each stage, counting dropout before the event and by that
  stage's analysis time. Only replications contributing at that stage
  with a finite analysis time and positive enrolled-subject denominator
  contribute; NA if none contribute. Stopped trials and discontinued
  groups do not contribute to later stages; overlapping populations
  include their shared subjects. Numeric matrices with stage rows and
  scenario columns for two-arm trials. In multi-arm trials, arm 1 has
  dimensions stage, scenario, and active arm; arm 2 is a
  stage-by-scenario matrix for shared control. In enrichment trials,
  both arms have dimensions stage, scenario, and population
  (subpopulations followed by the full population). A single active arm
  or population uses a stage-by-scenario matrix. Stage and scenario
  dimensions are always retained. NULL for test-statistic-based
  simulation. Both proportions are hidden in print and summary output
  when both input dropout rates are zero, but remain in the object.

- `dropoutObservationTime1`:

  Mean stage analysis time in arm 1 over exactly the replications
  contributing to the corresponding stage's observed dropout proportion,
  in the same units as accrual and survival times. NA if no replication
  contributes. Numeric matrices with stage rows and scenario columns for
  two-arm trials. In multi-arm trials, arm 1 has dimensions stage,
  scenario, and active arm; arm 2 is a stage-by-scenario matrix for
  shared control. In enrichment trials, both arms have dimensions stage,
  scenario, and population (subpopulations followed by the full
  population). A single active arm or population uses a
  stage-by-scenario matrix. Stage and scenario dimensions are always
  retained. NULL for test-statistic-based simulation. Retained in the
  object but omitted from print and summary output.

- `dropoutObservationTime2`:

  Mean stage analysis time in arm 2 over exactly the replications
  contributing to the corresponding stage's observed dropout proportion,
  in the same units as accrual and survival times. NA if no replication
  contributes. Numeric matrices with stage rows and scenario columns for
  two-arm trials. In multi-arm trials, arm 1 has dimensions stage,
  scenario, and active arm; arm 2 is a stage-by-scenario matrix for
  shared control. In enrichment trials, both arms have dimensions stage,
  scenario, and population (subpopulations followed by the full
  population). A single active arm or population uses a
  stage-by-scenario matrix. Stage and scenario dimensions are always
  retained. NULL for test-statistic-based simulation. Retained in the
  object but omitted from print and summary output.

## See also

[SimulationResultsSurvival](https://docs.rpact.org/qtd/reference/SimulationResultsSurvival.md),
[SimulationResultsMultiArmSurvival](https://docs.rpact.org/qtd/reference/SimulationResultsMultiArmSurvival.md),
[SimulationResultsEnrichmentSurvival](https://docs.rpact.org/qtd/reference/SimulationResultsEnrichmentSurvival.md)
