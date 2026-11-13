# Operating Characteristics for Quantitative Decisions

Internal ParameterSet container shared by means and rates results.
Obtain concrete instances through the endpoint-specific OC functions.

Initializes the shared quantitative operating-characteristics fields.

## Arguments

- design:

  The originating quantitative decision rule.

- numberOfSubjects:

  Matrix of control and treatment sample sizes.

- probabilities:

  Computed probability data frame.

## Details

The originating rule is stored in the internal .design field. Priors and
the control mean are NULL when not applicable. Bayesian results store
four exclusive categories; frequentist results store six. Sampling
probabilities are conditional on the supplied true effect or response
rates; they are not predictive assurance.

## Fields

- `numberOfSubjects`:

  Supplied sample-size scenarios. Rows correspond to scenarios; the two
  columns contain the positive integer numbers of control and treatment
  subjects, in that order. Repeated scenarios are retained. Is a numeric
  matrix.

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

[`getOperatingCharacteristicsMeans()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsMeans.md),
[`getOperatingCharacteristicsRates()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsRates.md)
