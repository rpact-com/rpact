# Class for Simulation Results Multi-Arm Survival

A class for simulation results survival in multi-arm designs.

## Details

Use
[`getSimulationMultiArmSurvival()`](https://docs.rpact.org/reference/getSimulationMultiArmSurvival.md)
to create an object of this type.

## Fields

- `accrualIntensity`:

  The absolute accrual intensities. Is a numeric vector of length
  `kMax`.

- `accrualTime`:

  The assumed accrual time intervals for the study. Is a numeric vector.

- `activeArms`:

  The number of active treatment arms to be compared with control. Is a
  numeric vector of length 1 containing a whole number.

- `adaptations`:

  Indicates whether or not an adaptation takes place at interim k. Is a
  logical vector of length `kMax` minus 1.

- `allocationRatioPlanned`:

  The planned allocation ratio (`n1 / n2`) for the groups. For multi-arm
  designs, it is the allocation ratio relating the active arm(s) to the
  control. Is a positive numeric vector of length 1.

- `conditionalPower`:

  The conditional power at each stage of the trial. Is a numeric vector
  of length 1 containing a value between 0 and 1.

- `conditionalPowerAchieved`:

  The calculated conditional power, under the assumption of observed or
  assumed effect sizes. Is a numeric matrix.

- `correlationComputation`:

  If `"alternative"`, a correlation matrix according to Deng et al.
  (Biometrics, 2019) accounting for the respective alternative is used
  for simulating log-rank statistics in the many-to-one design. If
  `"null"`, a constant correlation matrix valid under the null
  hypothesis is used.

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

- `effectMatrix`:

  The matrix of effect sizes with `activeArms` columns and number of
  rows reflecting the different situations to consider.

- `epsilonValue`:

  Needs to be specified if `typeOfSelection = "epsilon"`. Is a numeric
  vector of length 1.

- `eventsPerStage`:

  Deprecated: use `singleEventsPerStage` or `cumulativeEventsPerStage`
  instead Is a numeric matrix.

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

- `gED50`:

  The ED50 of the sigmoid Emax model. Only necessary if
  `typeOfShape = "sigmoidEmax"` has been specified. Is a numeric vector
  of length 1.

- `intersectionTest`:

  The multiple test used for intersection hypotheses in closed systems
  of hypotheses. Is a character vector of length 1.

- `iterations`:

  The number of iterations used for simulations. Is a numeric vector of
  length 1 containing a whole number.

- `kappa`:

  The shape of the Weibull distribution if `kappa!=1`. Is a numeric
  vector of length 1.

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

- `minNumberOfEventsPerStage`:

  Determines the minimum number of events per stage for data-driven
  sample size recalculation. Is a numeric vector of length `kMax`
  containing whole numbers.

- `numberOfActiveArms`:

  The number of active arms in a multi-armed design. Is a numeric
  matrix.

- `omegaMaxVector`:

  The range of hazard ratios with highest response for `"linear"` and
  `"sigmoidEmax"` model. Is a numeric vector.

- `plannedEvents`:

  Determines the number of cumulated (overall) events in survival
  designs when the interim stages are planned. For two treatment arms,
  is the number of events for both treatment arms. For multi-arm
  designs, refers to the overall number of events for the selected arms
  plus control. Is a numeric vector of length `kMax` containing whole
  numbers.

- `rejectAtLeastOne`:

  The probability to reject at least one of the (multiple) hypotheses.
  Is a numeric vector.

- `rejectedArmsPerStage`:

  The simulated number of rejected arms per stage.

- `rValue`:

  Needs to be specified if `typeOfSelection = "rBest"`. Is a numeric
  vector of length 1.

- `seed`:

  The seed used for random number generation. Is a numeric vector of
  length 1.

- `selectArmsFunction`:

  An optional function that can be entered to define how treatment arms
  are selected.

- `selectedArms`:

  The selected arms in multi-armed designs.

- `singleEventsPerArmAndStage`:

  The number of events per arm and stage that is used for the analysis.

- `singleEventsPerStage`:

  The single number of events per stage. Is a numeric matrix.

- `singleNumberOfEventsPerStage`:

  Deprecated: use `singleEventsPerArmAndStage` or
  `singleEventsPerSubsetAndStage` instead

- `slope`:

  The slope of the sigmoid Emax model, if `typeOfShape = "sigmoidEmax"`
  Is a numeric vector of length 1.

- `studyDuration`:

  The study duration for specified effect size. Is a positive numeric
  vector.

- `successPerStage`:

  The simulated success probabilities per stage where success is defined
  by user. Is a numeric matrix.

- `threshold`:

  The selection criterion: treatment arm/population is only selected if
  `effectMeasure` exceeds `threshold`. Either a single numeric value or
  a numeric vector of length `activeArms` referring to a separate
  threshold condition for each treatment arm.

- `typeOfShape`:

  The shape of the dose-response relationship over the treatment groups.
  Is a character vector of length 1.
