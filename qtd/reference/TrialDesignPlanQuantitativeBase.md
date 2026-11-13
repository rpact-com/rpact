# Common Quantitative Trial Decision Rule

Internal ParameterSet base for frequentist and Bayesian quantitative
decision rules. Obtain instances of its concrete subclasses with
[`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md).

Initializes shared reference parameters.

Prints the quantitative rule.

Renders the quantitative rule.

Returns the object description.

## Arguments

- ...:

  Reserved for ParameterSet compatibility.

- targetValue, lowerReferenceValue:

  Reference effects or NA.

- upperPercentile, lowerPercentile:

  Reference percentiles or NA.

- showType:

  Output mode.

- digits:

  Display precision.

- consoleOutputEnabled:

  Print or collect output.

- startWithUpperCase:

  Capitalize the description.

## Details

The reference values and percentiles are optional for explicit Bayesian
rules. The concrete subclasses are TrialDesignPlanQuantitative and
TrialDesignPlanQuantitativeBayes. ParameterSet supplies shared parameter
and output management; internal dot-prefixed fields are implementation
details.

## Fields

- `targetValue`:

  Optional target effect on the beneficial-effect scale. A supplied
  finite scalar for reference-value rules; NA for explicit Bayesian
  criteria.

- `lowerReferenceValue`:

  Optional lower reference effect on the beneficial-effect scale. A
  supplied finite scalar for reference-value rules; NA for explicit
  Bayesian criteria.

- `upperPercentile`:

  Upper percentile supplied for a reference-value rule, strictly between
  lowerPercentile and 1. NA for explicit Bayesian criteria. For Bayesian
  rules its interpretation is controlled by percentileMapping.

- `lowerPercentile`:

  Lower percentile supplied for a reference-value rule, strictly between
  0 and upperPercentile. NA for explicit Bayesian criteria. For Bayesian
  rules its interpretation is controlled by percentileMapping.

## See also

[`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md)
