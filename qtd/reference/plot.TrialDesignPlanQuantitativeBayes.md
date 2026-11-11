# Plot Bayesian Quantitative Decision Regions

Displays decisions based on posterior criteria rather than frequentist
intervals.

## Usage

``` r
# S3 method for class 'TrialDesignPlanQuantitativeBayes'
plot(
  x,
  ...,
  type = 1L,
  estimates = NULL,
  standardErrors = 1,
  priorDist = NULL,
  estimateRange = c(-1, 2),
  standardErrorRange = c(0.1, 1),
  gridPoints = 50L,
  numberOfSubjects = NULL,
  priorDist1 = NULL,
  priorDist2 = NULL,
  directionUpper = TRUE,
  plotCat = 4L,
  main = "Bayesian quantitative decisions",
  plotSettings = NULL
)
```

## Arguments

- x:

  A Bayesian rule from
  [`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md).

- ...:

  Additional arguments are checked for unknown names.

- type:

  1 for observed effect estimates and posterior intervals, 2 for an
  effect-estimate/standard-error decision map, 3 for a binary count map.

- estimates:

  Numeric vector of observed treatment-minus-control estimates for type
  1.

- standardErrors:

  Positive sampling standard errors, one per estimate or a common
  scalar. These describe effect estimates, not arm-level SDs.

- priorDist:

  RBesT normMix effect prior for types 1 and 2. Defaults to mixnorm(c(1,
  0, 100)). Group-mean priors are supported by the OC function; these
  effect-estimate plots require an effect prior.

- estimateRange, standardErrorRange:

  Increasing two-element ranges for type 2.

- gridPoints:

  Integer grid resolution per axis for type 2, at most 200.

- numberOfSubjects:

  Positive integer vector (control, treatment) for type 3.

- priorDist1, priorDist2:

  RBesT betaMix treatment and control priors for type 3.

- directionUpper:

  Whether increasing treatment rates are beneficial.

- plotCat:

  Three or four decision categories.

- main:

  Plot title.

- plotSettings:

  Optional PlotSettings object.

## Value

A ggplot object containing the evaluated decisions in its data.

## Details

Type 1 shows equal-tail 95 percent posterior intervals for orientation;
categories always use the actual posterior criteria, not these display
bounds. Exact posterior probability-threshold ties belong to
Indeterminate 2. Type 3 uses the same full posterior decision grid as
binary OCs. The grid contains at most one million cells; large grids can
be expensive to evaluate.

## See also

[`getOperatingCharacteristicsMeans()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsMeans.md),
[`getOperatingCharacteristicsRates()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsRates.md)
