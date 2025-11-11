# Class for Simulation Results Enrichment Means

A class for simulation results means in enrichment designs.

## Details

Use
[`getSimulationEnrichmentMeans()`](https://rpact-com.github.io/rpact/reference/getSimulationEnrichmentMeans.md)
to create an object of this type.

## Fields

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

- `earlyStop`:

  The probability to stopping the trial either for efficacy or futility.
  Is a numeric vector.

- `effectList`:

  The list of subsets, prevalences and effect sizes with columns and
  number of rows reflecting the different situations to be considered.

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

- `intersectionTest`:

  The multiple test used for intersection hypotheses in closed systems
  of hypotheses. Is a character vector of length 1.

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

  The number of populations in an enrichment design. Is a numeric vector
  of length 1 containing a whole number.

- `rejectAtLeastOne`:

  The probability to reject at least one of the (multiple) hypotheses.
  Is a numeric vector.

- `rejectedPopulationsPerStage`:

  The simulated number of rejected populations per stage.

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

- `selectedPopulations`:

  The selected populations in enrichment designs.

- `selectPopulationsFunction`:

  An optional function that can be entered to define the way of how
  populations are selected.

- `stDev`:

  The standard deviation used for sample size and power calculation. Is
  a numeric vector of length 1.

- `stDevH1`:

  The standard deviation under which the conditional power or sample
  size recalculation is performed. Is a numeric vector of length 1.

- `stratifiedAnalysis`:

  For enrichment designs, typically a stratified analysis should be
  chosen. When testing means and rates, a non-stratified analysis can be
  performed on overall data. For survival data, only a stratified
  analysis is possible. Is a logical vector of length 1.

- `successCriterion`:

  Defines when the study is stopped for efficacy at interim. `"all"`
  stops the trial if the efficacy criterion has been fulfilled for all
  selected treatment arms/populations, `"atLeastOne"` stops if at least
  one of the selected treatment arms/populations is shown to be superior
  to control at interim. Is a character vector of length 1.

- `successPerStage`:

  The simulated success probabilities per stage where success is defined
  by user. Is a numeric matrix.

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
  a character vector of length 1.
