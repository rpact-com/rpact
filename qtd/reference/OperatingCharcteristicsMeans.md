# Operating Characteristics for Comparing Means

Stores fixed-design quantitative decision probabilities for means.
Obtain instances through
[`getOperatingCharacteristicsMeans()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsMeans.md).

Initializes means OC assumptions and probability output.

Prints the result.

Renders the result.

Returns the result description.

## Arguments

- ...:

  Reserved for ParameterSet compatibility.

- design:

  Originating quantitative rule.

- numberOfSubjects:

  Control and treatment sample sizes.

- normalApproximation:

  Normal sampling model flag.

- groups:

  Number of groups.

- alternative:

  True effect vector.

- stDev:

  Known standard deviations.

- probabilities:

  Category-block probability table.

- showType:

  Output mode.

- digits:

  Display precision.

- consoleOutputEnabled:

  Print or collect output.

- startWithUpperCase:

  Capitalize the description.

## Details

Inherits sample sizes, probabilities, priors and controlMean from
OperatingCharcteristics. The rates priors priorDist1 and priorDist2
remain NULL.

## Fields

- `normalApproximation`:

  TRUE for the normal sampling model used by the frequentist means
  calculation. For Bayesian means the model has known standard
  deviations and this field does not select a t approximation. Is a
  single logical value.

- `groups`:

  Number of independent treatment groups, fixed at 2 (control and
  treatment). Is a single numeric value representing a whole number.

- `alternative`:

  Supplied finite numeric vector of true treatment-minus-control mean
  differences. Order and repeated entries are retained for every
  sample-size scenario.

- `stDev`:

  Supplied known standard deviation for means calculations. A positive
  scalar shared by both arms, or for Bayesian calculations a vector of
  two positive values ordered control, treatment. Bayesian OC plot data
  use the root mean square as the scalar SD facet label and retain both
  arm-specific values in four-category data.

## Inherited fields

- `probabilities`:

  Calculated data frame of decision-category probabilities. Frequentist
  results store six categories; Bayesian results store Go, Indeterminate
  1, Indeterminate 2 and Stop. Within each sample-size scenario,
  categories form blocks retaining supplied effect order and repeats.
  Probabilities are on the scale 0 to 1.

- `priorDist`:

  Supplied normal-mixture effect prior (RBesT normMix) or a two-element
  list of control and treatment mean priors, in that order. NULL for
  frequentist results and rates results. Default Bayesian effect prior
  is mixnorm(c(1, 0, 100)).

- `priorDist1`:

  Supplied RBesT betaMix prior for the treatment event probability. NULL
  for frequentist results and means results. Default Bayesian prior is
  mixbeta(c(1, 1/3, 1/3)).

- `priorDist2`:

  Supplied RBesT betaMix prior for the control event probability. NULL
  for frequentist results and means results. Default Bayesian prior is
  mixbeta(c(1, 1/3, 1/3)).

- `controlMean`:

  Single supplied true control mean when separate Bayesian mean priors
  are used. The true treatment mean is controlMean plus alternative.
  NULL for an effect prior, frequentist results and rates results.

## See also

[`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md),
[`getOperatingCharacteristicsMeans()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsMeans.md)
