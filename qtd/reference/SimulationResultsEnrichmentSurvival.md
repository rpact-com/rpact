# Class for Simulation Results Enrichment Survival

A class for simulation results survival in enrichment designs.

## Details

Use
[`getSimulationEnrichmentSurvival()`](https://docs.rpact.org/qtd/reference/getSimulationEnrichmentSurvival.md)
to create an object of this type.

## Fields

- `accrualIntensity`:

  The absolute accrual intensities. Is a numeric vector of length
  `kMax`.

- `accrualTime`:

  The assumed accrual time intervals for the study. Is a numeric vector.

- `adaptations`:

  Indicates whether or not an adaptation takes place at interim k. Is a
  logical vector of length `kMax` minus 1.

- `allocationRatioPlanned`:

  The planned allocation ratio (`n1 / n2`) for the groups. For multi-arm
  designs, it is the allocation ratio relating the active arm(s) to the
  control. Is a single positive numeric value.

- `calcEventsFunction`:

  An optional function that can be entered to define how event size is
  recalculated. By default, recalculation is performed with conditional
  power with specified `minNumberOfEventsPerStage` and
  `maxNumberOfEventsPerStage`.

- `conditionalPower`:

  The conditional power at each stage of the trial. Is a single numeric
  value between 0 and 1.

- `conditionalPowerAchieved`:

  The calculated conditional power, under the assumption of observed or
  assumed effect sizes. Is a numeric matrix.

- `cumulativeEventsPerStage`:

  The cumulative number of events per stage. Is a numeric matrix.

- `directionUpper`:

  Specifies the direction of the alternative, only applicable for
  one-sided testing. Default is `TRUE` which means that larger values of
  the test statistics yield smaller p-values. Is a single logical value.

- `dropoutRate1`:

  The assumed drop-out rate in the treatment group. Is a single numeric
  value between 0 and 1.

- `dropoutRate2`:

  The assumed drop-out rate in the control group. Is a single numeric
  value between 0 and 1.

- `dropoutTime`:

  The assumed time for drop-out rates in the control and treatment
  group. Is a single numeric value.

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

- `earlyStop`:

  The probability to stopping the trial either for efficacy or futility.
  Is a numeric vector.

- `effectList`:

  The list of subsets, prevalences and effect sizes with columns and
  number of rows reflecting the different situations to be considered.

- `effectMeasure`:

  Criterion for treatment arm/population selection, either based on test
  statistic (`"testStatistic"`) or effect estimate (`"effectEstimate"`).
  Is a single character value.

- `epsilonValue`:

  Needs to be specified if `typeOfSelection = "epsilon"`. Is a single
  numeric value.

- `eventsPerStage`:

  Deprecated: use `singleEventsPerStage` or `cumulativeEventsPerStage`
  instead Is a numeric matrix.

- `eventTime`:

  The assumed time under which the event rates are calculated. Is a
  single numeric value.

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

- `intersectionTest`:

  The multiple test used for intersection hypotheses in closed systems
  of hypotheses. Is a single character value.

- `iterations`:

  The number of iterations used for simulations. Is a single numeric
  value representing a whole number.

- `kappa`:

  The shape of the Weibull distribution if `kappa!=1`. Is a single
  numeric value.

- `maxNumberOfIterations`:

  The number of simulation iterations. Is a single numeric value
  representing a whole number.

- `maxNumberOfSubjects`:

  The maximum number of subjects for power calculations. Is a numeric
  vector.

- `maxNumberOfSubjectsPerStage`:

  Determines the maximum number of subjects per stage for data-driven
  sample size recalculation. For two treatment arms, is the number of
  subjects for both treatment arms. For multi-arm designs, is the
  minimum number of subjects per selected active arm. Is a numeric
  vector of length `kMax` containing whole numbers.

- `minNumberOfSubjectsPerStage`:

  Determines the minimum number of subjects per stage for data-driven
  sample size recalculation. For two treatment arms, is the number of
  subjects for both treatment arms. For multi-arm designs, is the
  minimum number of subjects per selected active arm. Is a numeric
  vector of length `kMax` containing whole numbers.

- `numberOfPopulations`:

  The number of populations in an enrichment design. Is a numeric
  matrix.

- `plannedSubjects`:

  Determines the number of cumulated (overall) subjects when the interim
  stages are planned. For two treatment arms, is the number of subjects
  for both treatment arms. For multi-arm designs, refers to the number
  of subjects per selected active arm. Is a numeric vector of length
  `kMax` containing whole numbers.

- `populations`:

  The number of populations in an enrichment design. Is a single numeric
  value representing a whole number.

- `populationEventsPerStage`:

  Mean number of events within each population and stage. Is a numeric
  array with dimensions stage, scenario, and population.

- `rejectAtLeastOne`:

  The probability to reject at least one of the (multiple) hypotheses.
  Is a numeric vector.

- `rejectedPopulationsPerStage`:

  The simulated number of rejected populations per stage.

- `rValue`:

  Needs to be specified if `typeOfSelection = "rBest"`. Is a single
  numeric value.

- `seed`:

  The seed used for random number generation. Is a single numeric value.

- `selectedPopulations`:

  The selected populations in enrichment designs.

- `selectPopulationsFunction`:

  An optional function that can be entered to define the way of how
  populations are selected.

- `singleEventsPerStage`:

  The single number of events per stage. Is a numeric matrix.

- `singleEventsPerSubsetAndStage`:

  Mean number of events within each subgroup and stage. Is a numeric
  array with dimensions stage, scenario, and subgroup.

- `singleNumberOfEventsPerStage`:

  Deprecated: use `singleEventsPerArmAndStage` or
  `singleEventsPerSubsetAndStage` instead

- `simulationType`:

  The simulation approach used: `"patientWise"` or
  `"testStatisticBased"`.

- `stratifiedAnalysis`:

  For enrichment designs, typically a stratified analysis should be
  chosen. When testing means and rates, a non-stratified analysis can be
  performed on overall data. For survival data, only a stratified
  analysis is possible. Is a single logical value.

- `studyDuration`:

  The study duration for specified effect size. Is a positive numeric
  vector.

- `successCriterion`:

  Defines when the study is stopped for efficacy at interim. `"all"`
  stops the trial if the efficacy criterion has been fulfilled for all
  selected treatment arms/populations, `"atLeastOne"` stops if at least
  one of the selected treatment arms/populations is shown to be superior
  to control at interim. Is a single character value.

- `successPerStage`:

  The simulated success probabilities per stage where success is defined
  by user. Is a numeric matrix.

- `thetaH0`:

  The difference or assumed effect under H0. Is a single numeric value.

- `thetaH1`:

  The assumed effect under the alternative hypothesis. For survival
  designs, refers to the hazard ratio. Is a numeric vector.

- `threshold`:

  The selection criterion: treatment arm/population is only selected if
  `effectMeasure` exceeds `threshold`. Either a single numeric value or
  a numeric vector of length `activeArms` referring to a separate
  threshold condition for each treatment arm.

- `typeOfSelection`:

  The way the treatment arms or populations are selected at interim. Is
  a single character value.

- `analysisTime`:

  The estimated time of analysis. Is a numeric matrix.

- `eventsNotAchieved`:

  The simulated number of cases how often the number of events was not
  reached. Is a numeric matrix.

- `numberOfSubjects`:

  In simulation results data set: The number of subjects under
  consideration when the interim analysis takes place.

- `piecewiseSurvivalTime`:

  The time intervals for the piecewise definition of the exponential
  survival time cumulative distribution function. Is a numeric vector.

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

- `maxStudyDuration`:

  The maximum study duration in survival designs. Is a numeric vector.
