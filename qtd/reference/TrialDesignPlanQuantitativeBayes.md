# Bayesian Quantitative Trial Decision Rule

Internal Bayesian decision-rule class. Obtain instances through
[`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md)
with bayesian = TRUE or explicit criteria.

Stores validated Bayesian rules and optional references.

Returns the object description.

## Arguments

- go, stop, indeterminate1:

  Exclusive posterior criteria.

- targetValue, lowerReferenceValue:

  Reference effects or NA.

- upperPercentile, lowerPercentile:

  Reference percentiles or NA.

- percentileMapping:

  Interpretation of reference percentiles.

- startWithUpperCase:

  Capitalize the description.

## Details

Inherits optional reference values and percentiles from
TrialDesignPlanQuantitativeBase. These are NA for freely specified
criteria. Go, Stop and Indeterminate 1 must be provably mutually
exclusive for every continuous posterior CDF; Indeterminate 2 includes
all remaining outcomes, including exact probability-threshold ties.
Priors are supplied to the OC functions, rather than stored in the
decision rule.

## Fields

- `go`:

  Supplied QuantitativeDecisionThresholds object defining the
  conjunction of Go criteria. Stored as a deep copy.

- `stop`:

  Supplied QuantitativeDecisionThresholds object defining the
  conjunction of Stop criteria. Must be provably incompatible with the
  other explicit categories. Stored as a deep copy.

- `indeterminate1`:

  Supplied QuantitativeDecisionThresholds object defining
  Indeterminate 1. Must exclude Go and Stop. Indeterminate 2 is the
  complement of the three explicit categories, including exact
  probability-threshold ties. Stored as a deep copy.

- `percentileMapping`:

  Bayesian mapping for reference-value rules: quantile preserves
  posterior quantile comparisons, whereas posteriorProbability uses the
  percentiles as beneficial-tail thresholds. Has no statistical effect
  for explicit criteria. Is a single character value.

## Inherited fields

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

[`getOperatingCharacteristicsMeans()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsMeans.md),
[`getOperatingCharacteristicsRates()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsRates.md)
