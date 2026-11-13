# Frequentist Quantitative Trial Decision Rule

Represents the reference-value and percentile rule for a fixed two-group
trial. Obtain instances with
[`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md).

## Details

Inherits all public data fields from TrialDesignPlanQuantitativeBase.
Operating characteristics use the frequentist sampling model and six
categories.

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
