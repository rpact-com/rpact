# Get Simulation Multi-Arm Rates

Returns the simulated power, stopping and selection probabilities,
conditional power, and expected sample size for testing rates in a
multi-arm treatment groups testing situation.

## Usage

``` r
getSimulationMultiArmRates(
  design = NULL,
  ...,
  activeArms = NA_integer_,
  effectMatrix = NULL,
  typeOfShape = c("linear", "sigmoidEmax", "userDefined"),
  piMaxVector = NA_real_,
  piControl = NA_real_,
  gED50 = NA_real_,
  slope = 1,
  doseLevels = NA_real_,
  intersectionTest = c("Dunnett", "Bonferroni", "Simes", "Sidak", "Hierarchical"),
  directionUpper = NA,
  adaptations = NA,
  typeOfSelection = c("best", "rBest", "epsilon", "all", "userDefined"),
  effectMeasure = c("effectEstimate", "testStatistic"),
  successCriterion = c("all", "atLeastOne"),
  epsilonValue = NA_real_,
  rValue = NA_real_,
  threshold = -Inf,
  plannedSubjects = NA_real_,
  allocationRatioPlanned = NA_real_,
  minNumberOfSubjectsPerStage = NA_real_,
  maxNumberOfSubjectsPerStage = NA_real_,
  conditionalPower = NA_real_,
  thetaH0 = C_THETA_H0_MEANS_DEFAULT,
  piTreatmentsH1 = NA_real_,
  piControlH1 = NA_real_,
  maxNumberOfIterations = NA_integer_,
  seed = NA_real_,
  calcSubjectsFunction = NULL,
  selectArmsFunction = NULL,
  showStatistics = FALSE
)
```

## Arguments

- design:

  The trial design. If no trial design is specified, a fixed sample size
  design is used. In this case, Type I error rate `alpha`, Type II error
  rate `beta`, `twoSidedPower`, and `sided` can be directly entered as
  argument where necessary. Must be created with
  [`getDesignInverseNormal()`](https://docs.rpact.org/reference/getDesignInverseNormal.md),
  [`getDesignFisher()`](https://docs.rpact.org/reference/getDesignFisher.md),
  [`getDesignConditionalDunnett()`](https://docs.rpact.org/reference/getDesignConditionalDunnett.md),
  or
  [`getDesignFixed()`](https://docs.rpact.org/reference/getDesignFixed.md);
  designs created with
  [`getDesignGroupSequential()`](https://docs.rpact.org/reference/getDesignGroupSequential.md)
  are not supported.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- activeArms:

  The number of active treatment arms to be compared with control,
  default is `3`. Must not exceed `8`.

- effectMatrix:

  Matrix of effect sizes with `activeArms` columns and one or more rows,
  where each row represents a different situation to consider.

- typeOfShape:

  The shape of the dose-response relationship over the treatment groups.
  This can be either `"linear"`, `"sigmoidEmax"`, or `"userDefined"`,
  default is `"linear"`.  
  For `"linear"`, `piMaxVector` specifies the range of effect sizes for
  the treatment group with highest response. If `"sigmoidEmax"` is
  selected, `gED50` and `slope` has to be entered to specify the ED50
  and the slope of the sigmoid Emax model. For `"sigmoidEmax"`,
  `piMaxVector` specifies the range of effect sizes for the treatment
  group with response according to infinite dose. If `"userDefined"` is
  selected, `effectMatrix` has to be entered.

- piMaxVector:

  Range of assumed probabilities for the treatment group with highest
  response for `"linear"` and `"sigmoidEmax"` model, default is
  `seq(0, 1, 0.2)`.

- piControl:

  If specified, the assumed probability in the control arm for
  simulation and under which the sample size recalculation is performed,
  default is `0.2`.

- gED50:

  If `typeOfShape = "sigmoidEmax"` is selected, `gED50` has to be
  entered to specify the ED50 of the sigmoid Emax model.

- slope:

  If `typeOfShape = "sigmoidEmax"` is selected, `slope` can be entered
  to specify the slope of the sigmoid Emax model, default is 1.

- doseLevels:

  The dose levels for the dose response relationship. If not specified,
  these dose levels are `1,...,activeArms`.

- intersectionTest:

  Defines the multiple test for the intersection hypotheses in the
  closed system of hypotheses. Five options are available in multi-arm
  designs: `"Dunnett"`, `"Bonferroni"`, `"Simes"`, `"Sidak"`, and
  `"Hierarchical"`, default is `"Dunnett"`.

- directionUpper:

  Logical. Specifies the direction of the alternative, only applicable
  for one-sided testing; `TRUE` means that larger values of the test
  statistics yield smaller p-values. The default is `TRUE` for means,
  rates, and count data, and `FALSE` for survival data (power
  calculations, simulations, and analyses). In sample size calculations,
  the direction is derived from the specified alternative; a specified
  `directionUpper` must agree with it.

- adaptations:

  A logical vector of length `kMax - 1` indicating whether or not an
  adaptation takes place at interim k, default is `rep(TRUE, kMax - 1)`.

- typeOfSelection:

  Rule for selecting treatment arms (multi-arm designs) or populations
  (enrichment designs) for continuation after an interim analysis. The
  default `"best"` selects only the single best arm or population;
  `"rBest"` (also accepted as `"rbest"`) selects the best `rValue`;
  `"epsilon"` selects those within `epsilonValue` of the best; `"all"`
  retains all remaining arms or populations; and `"userDefined"` applies
  `selectArmsFunction` or `selectPopulationsFunction`. Selection is
  applied at interims with `adaptations = TRUE`. See the section
  *Treatment arm and population selection* for details and an example of
  gradual elimination.

- effectMeasure:

  Criterion for treatment arm/population selection, either based on test
  statistic (`"testStatistic"`) or effect estimate (difference for means
  and rates or ratio for survival) (`"effectEstimate"`), default is
  `"effectEstimate"`.

- successCriterion:

  Defines when the study is stopped for efficacy at interim. Two options
  are available: `"all"` stops the trial if the efficacy criterion is
  fulfilled for all selected treatment arms/populations, `"atLeastOne"`
  stops if at least one of the selected treatment arms/populations is
  shown to be superior to control at interim, default is `"all"`.

- epsilonValue:

  For `typeOfSelection = "epsilon"` (select treatment arm / population
  not worse than epsilon compared to the best), the parameter
  `epsilonValue` has to be specified. Must be a numeric of length 1.

- rValue:

  For `typeOfSelection = "rbest"` (select the `rValue` best treatment
  arms / populations), the parameter `rValue` has to be specified.

- threshold:

  Selection criterion: treatment arm / population is selected only if
  `effectMeasure` exceeds `threshold`, default is `-Inf`. `threshold`
  can also be a vector of length `activeArms` referring to a separate
  threshold condition over the treatment arms.

- plannedSubjects:

  `plannedSubjects` is a numeric vector of length `kMax` (the number of
  stages of the design) that determines the number of cumulated
  (overall) subjects when the interim stages are planned. For two
  treatment arms, it is the number of subjects for both treatment arms.
  For multi-arm designs, `plannedSubjects` refers to the number of
  subjects per selected active arm.

- allocationRatioPlanned:

  The planned allocation ratio `n1 / n2` for a two treatment groups
  design, default is `1`. For multi-arm designs, it is the allocation
  ratio relating the active arm(s) to the control. For simulating means
  and rates for a two treatment groups design, it can be a vector of
  length `kMax`, the number of stages. It can be a vector of length
  `kMax`, too, for multi-arm and enrichment designs. In these cases, a
  change of allocating subjects to treatment groups over the stages can
  be assessed. Note that internally `allocationRatioPlanned` is treated
  as a vector of length `kMax`, not a scalar.

- minNumberOfSubjectsPerStage:

  When performing a data driven sample size recalculation, the numeric
  vector `minNumberOfSubjectsPerStage` with length `kMax` determines the
  minimum number of subjects per stage (i.e., not cumulated), the first
  element is not taken into account. For two treatment arms, it is the
  number of subjects for both treatment arms. For multi-arm designs
  `minNumberOfSubjectsPerStage` refers to the minimum number of subjects
  per selected active arm.

- maxNumberOfSubjectsPerStage:

  When performing a data driven sample size recalculation, the numeric
  vector `maxNumberOfSubjectsPerStage` with length `kMax` determines the
  maximum number of subjects per stage (i.e., not cumulated), the first
  element is not taken into account. For two treatment arms, it is the
  number of subjects for both treatment arms. For multi-arm designs
  `maxNumberOfSubjectsPerStage` refers to the maximum number of subjects
  per selected active arm.

- conditionalPower:

  If `conditionalPower` together with `minNumberOfSubjectsPerStage` and
  `maxNumberOfSubjectsPerStage` (or `minNumberOfEventsPerStage` and
  `maxNumberOfEventsPerStage` for survival designs) is specified, a
  sample size recalculation based on the specified conditional power is
  performed. It is defined as the power for the subsequent stage given
  the current data. By default, the conditional power will be calculated
  under the observed effect size. Optionally, you can also specify
  `thetaH1` and `stDevH1` (for simulating means), `pi1H1` and `pi2H1`
  (for simulating rates; `piTreatmentsH1` and `piControlH1` in multi-arm
  designs, `piTreatmentH1` and `piControlH1` in enrichment designs), or
  `thetaH1` (for simulating hazard ratios) as parameters under which it
  is calculated and the sample size recalculation is performed.

- thetaH0:

  The null hypothesis value, default is `0` for general estimates, the
  normal case, and the binary case, it is `1` for the survival case
  (testing the hazard ratio).  
    
  For non-inferiority designs, `thetaH0` is the non-inferiority bound.
  That is, in case of (one-sided) testing of

  - *general estimates*: a value on the scale of the supplied estimates
    can be specified.

  - *means*: a value `!= 0` (or a value `!= 1` for testing the mean
    ratio) can be specified.

  - *rates*: a value `!= 0` (or a value `!= 1` for testing the risk
    ratio `pi1 / pi2`) can be specified.

  - *survival data*: a bound for testing H0:
    `hazard ratio = thetaH0 != 1` can be specified.

  - *count data*: a bound for testing H0:
    `lambda1 / lambda2 = thetaH0 != 1` can be specified.

  For testing a rate in one sample, a value `thetaH0` in (0, 1) has to
  be specified for defining the null hypothesis H0: `pi = thetaH0`.

- piTreatmentsH1:

  If specified, the assumed probability in the active treatment arm(s)
  under which the sample size recalculation is performed.

- piControlH1:

  If specified, the assumed probability in the reference group (if
  different from `piControl`) for which the conditional power was
  calculated.

- maxNumberOfIterations:

  The number of simulation iterations, default is `1000`. Must be a
  positive integer of length 1.

- seed:

  The seed to reproduce the simulation, default is a random seed.

- calcSubjectsFunction:

  Optionally, a function can be entered that defines the way of
  performing the sample size recalculation. By default, sample size
  recalculation is performed with conditional power and specified
  `minNumberOfSubjectsPerStage` and `maxNumberOfSubjectsPerStage` (see
  details and examples).

- selectArmsFunction:

  Optionally, a function can be entered that defines the way of how
  treatment arms are selected. This function is allowed to depend on
  `effectVector` with length `activeArms`, `stage`, `conditionalPower`,
  `conditionalCriticalValue`, `plannedSubjects/plannedEvents`,
  `allocationRatioPlanned`, `selectedArms`, `thetaH1` (for means and
  survival), `stDevH1` (for means), `overallEffects`, and for rates
  additionally: `piTreatmentsH1`, `piControlH1`, `overallRates`, and
  `overallRatesControl` (see examples).

- showStatistics:

  Logical. If `TRUE`, summary statistics of the simulated data are
  displayed for the `print` command, otherwise the output is suppressed,
  default is `FALSE`.

## Value

Returns a
[`SimulationResults`](https://docs.rpact.org/reference/SimulationResults.md)
object. The following generics (R generic functions) are available for
this object:

- [`names()`](https://docs.rpact.org/reference/names.FieldSet.md) to
  obtain the field names,

- [`print()`](https://docs.rpact.org/reference/print.FieldSet.md) to
  print the object,

- [`summary()`](https://docs.rpact.org/reference/summary.ParameterSet.md)
  to display a summary of the object,

- [`plot()`](https://docs.rpact.org/reference/plot.SimulationResults.md)
  to plot the object,

- [`as.data.frame()`](https://docs.rpact.org/reference/as.data.frame.ParameterSet.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://docs.rpact.org/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

At given design the function simulates the power, stopping
probabilities, selection probabilities, and expected sample size at
given number of subjects, parameter configuration, and treatment arm
selection rule in the multi-arm situation. An allocation ratio can be
specified referring to the ratio of number of subjects in the active
treatment groups as compared to the control group.

The definition of `piTreatmentsH1` and/or `piControlH1` makes only sense
if `kMax` \> 1 and if `conditionalPower`, `minNumberOfSubjectsPerStage`,
and `maxNumberOfSubjectsPerStage` (or `calcSubjectsFunction`) are
defined.

`calcSubjectsFunction`  
This function returns the number of subjects at given conditional power
and conditional critical value for specified testing situation. The
function might depend on the variables `stage`, `selectedArms`,
`directionUpper`, `plannedSubjects`, `allocationRatioPlanned`,
`minNumberOfSubjectsPerStage`, `maxNumberOfSubjectsPerStage`,
`conditionalPower`, `conditionalCriticalValue`, `overallRates`,
`overallRatesControl`, `piTreatmentsH1`, and `piControlH1`. The function
has to contain the three-dots argument '...' (see examples).

## Treatment arm and population selection

Selection determines which treatment arms or populations continue to the
next stage. It is applied at interim analysis `k` only if
`adaptations[k] = TRUE`; the default enables selection at every interim.
Otherwise, the current selection is retained. Once dropped, an arm or
population cannot be selected again. In multi-arm designs, these rules
apply to the active treatment arms; the control group is retained.

The ranking uses the cumulative interim data and the criterion specified
by `effectMeasure`: the estimated treatment effect for
`"effectEstimate"` (the default), or the test statistic for
`"testStatistic"`. The selection functions use a vector `effectVector`
on a scale where larger values rank better. For survival endpoints with
`effectMeasure = "effectEstimate"`, this is the estimated hazard ratio
for `directionUpper = TRUE` and its reciprocal for
`directionUpper = FALSE`.

- `"best"` (default):

  Selects only the single best remaining arm or population. Thus, with
  three active arms and selection enabled at the first interim, only one
  arm continues to stage 2. It does not drop just the worst arm. If
  several have the same best value, the first in the original order is
  selected.

- `"rBest"`:

  Selects the `rValue` highest-ranked remaining arms or populations,
  subject to availability. Specify `rValue` as a positive integer. For
  example, `rValue = 2` selects the best two; `rValue = 1` gives the
  same selection as `"best"`. The same `rValue` is used at every enabled
  interim, so this option does not mean dropping one arm or population
  at each interim. The spelling `"rbest"` is also accepted.

- `"epsilon"`:

  Selects the best and all other remaining arms or populations whose
  selection value is at most `epsilonValue` below the best value,
  including equality. Specify `epsilonValue` on the scale of
  `effectVector`. The number selected depends on the observed
  differences and can vary between simulation runs. For example,
  selection values `c(0.20, 0.35, 0.40)` and `epsilonValue = 0.10`
  retain the second and third arms or populations. For response-rate
  differences, `0.10` represents 10 percentage points.

- `"all"`:

  Retains all remaining arms or populations without ranking them. This
  disables dropping based on relative performance, but threshold
  conditions and stopping rules still apply.

- `"userDefined"`:

  Uses `selectArmsFunction` in multi-arm designs or
  `selectPopulationsFunction` in enrichment designs. Supply a function
  returning a logical vector in the original arm or population order,
  with `TRUE` indicating selection. Its length must equal the original
  number of active arms or populations. The function can use `stage`,
  `effectVector`, and the other arguments listed in the corresponding
  parameter description. Use this option for rules whose number selected
  changes by stage, such as dropping the worst remaining arm at each
  interim.

For the four built-in rules, selected arms or populations must
additionally have a selection value strictly above `threshold`. The
default `threshold = -Inf` imposes no additional restriction.
Consequently, fewer than the requested number may continue, or none may
continue. For `"userDefined"`, implement any threshold condition in the
supplied function. Efficacy and futility stopping rules apply separately
from selection.

For example, the following multi-arm selection function drops one worst
remaining arm at each enabled interim, while retaining at least one arm.
Pass it as `selectArmsFunction = dropWorstArm` together with
`typeOfSelection = "userDefined"`. If three arms are still available at
the first interim, it retains two; at the next enabled interim it
retains one, provided the trial continues.


    dropWorstArm <- function(effectVector, selectedArms, stage) {
        keep <- selectedArms[seq_along(effectVector), stage] & !is.na(effectVector)
        if (sum(keep) > 1L) {
            remaining <- which(keep)
            worst <- remaining[which.min(effectVector[remaining])]
            keep[worst] <- FALSE
        }
        keep
    }

## How to get help for generic functions

Click on the link of a generic in the list above to go directly to the
help documentation of the `rpact` specific implementation of the
generic. Note that you can use the R function
[`methods`](https://rdrr.io/r/utils/methods.html) to get all the methods
of a generic and to identify the object specific name of it, e.g., use
`methods("plot")` to get all the methods for the `plot` generic. There
you can find, e.g., `plot.AnalysisResults` and obtain the specific help
documentation linked above by typing
[`?plot.AnalysisResults`](https://docs.rpact.org/reference/plot.AnalysisResults.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Simulate the power of the combination test with two interim stages and 
# O'Brien & Fleming boundaries using Dunnett's intersection tests if the 
# best treatment arm is selected at first interim. Selection only take 
# place if a non-negative treatment effect is observed (threshold = 0); 
# 20 subjects per stage and treatment arm, simulation is performed for 
# four parameter configurations.
design <- getDesignInverseNormal(typeOfDesign = "OF")
effectMatrix <- matrix(c(0.2,0.2,0.2,
    0.4,0.4,0.4,
    0.4,0.5,0.5,
    0.4,0.5,0.6),
    byrow = TRUE, nrow = 4, ncol = 3)
x <- getSimulationMultiArmRates(design = design, typeOfShape = "userDefined", 
    effectMatrix = effectMatrix , piControl = 0.2, 
    typeOfSelection = "best", threshold = 0, intersectionTest = "Dunnett", 
    plannedSubjects = c(20, 40, 60), 
    maxNumberOfIterations = 50)
summary(x)
} # }
```
