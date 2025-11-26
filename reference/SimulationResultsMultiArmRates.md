# Class for Simulation Results Multi-Arm Rates

A class for simulation results rates in multi-arm designs.

## Details

Use
[`getSimulationMultiArmRates()`](https://docs.rpact.org/reference/getSimulationMultiArmRates.md)
to create an object of this type.

## Fields

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

- `effectMatrix`:

  The matrix of effect sizes with `activeArms` columns and number of
  rows reflecting the different situations to consider.

- `effectMeasure`:

  Criterion for treatment arm/population selection, either based on test
  statistic (`"testStatistic"`) or effect estimate (`"effectEstimate"`).
  Is a character vector of length 1.

- `epsilonValue`:

  Needs to be specified if `typeOfSelection = "epsilon"`. Is a numeric
  vector of length 1.

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

- `maxNumberOfIterations`:

  The number of simulation iterations. Is a numeric vector of length 1
  containing a whole number.

- `maxNumberOfSubjects`:

  The maximum number of subjects for power calculations. Is a numeric
  vector.

- `numberOfActiveArms`:

  The number of active arms in a multi-armed design. Is a numeric
  matrix.

- `piControl`:

  The assumed probability in the control arm for simulation and under
  which the sample size recalculation is performed. Is a numeric vector
  of length 1 containing a value between 0 and 1.

- `piControlH1`:

  The assumed probability in the reference group, for which the
  conditional power was calculated. Is a numeric vector of length 1
  containing a value between 0 and 1.

- `piH1`:

  The assumed probability in the active treatment arm(s) under which the
  sample size recalculation is performed. Is a numeric vector of length
  1 containing a value between 0 and 1.

- `piMaxVector`:

  The range of assumed probabilities for the treatment group with
  highest response for `"linear"` and `"sigmoidEmax"` model.

- `plannedSubjects`:

  Determines the number of cumulated (overall) subjects when the interim
  stages are planned. For two treatment arms, is the number of subjects
  for both treatment arms. For multi-arm designs, refers to the number
  of subjects per selected active arm. Is a numeric vector of length
  `kMax` containing whole numbers.

- `rejectAtLeastOne`:

  The probability to reject at least one of the (multiple) hypotheses.
  Is a numeric vector.

- `rejectedArmsPerStage`:

  The simulated number of rejected arms per stage.

- `rValue`:

  Needs to be specified if `typeOfSelection = "rBest"`. Is a numeric
  vector of length 1.

- `sampleSizes`:

  The sample sizes for each group and stage. Is a numeric vector of
  length number of stages times number of groups containing whole
  numbers.

- `seed`:

  The seed used for random number generation. Is a numeric vector of
  length 1.

- `selectArmsFunction`:

  An optional function that can be entered to define how treatment arms
  are selected.

- `selectedArms`:

  The selected arms in multi-armed designs.

- `slope`:

  The slope of the sigmoid Emax model, if `typeOfShape = "sigmoidEmax"`
  Is a numeric vector of length 1.

- `successCriterion`:

  Defines when the study is stopped for efficacy at interim. `"all"`
  stops the trial if the efficacy criterion has been fulfilled for all
  selected treatment arms/populations, `"atLeastOne"` stops if at least
  one of the selected treatment arms/populations is shown to be superior
  to control at interim. Is a character vector of length 1.

- `successPerStage`:

  The simulated success probabilities per stage where success is defined
  by user. Is a numeric matrix.

- `threshold`:

  The selection criterion: treatment arm/population is only selected if
  `effectMeasure` exceeds `threshold`. Either a single numeric value or
  a numeric vector of length `activeArms` referring to a separate
  threshold condition for each treatment arm.

- `typeOfSelection`:

  The way the treatment arms or populations are selected at interim. Is
  a character vector of length 1.

- `typeOfShape`:

  The shape of the dose-response relationship over the treatment groups.
  Is a character vector of length 1.
