# Posterior Probability Criteria for Quantitative Decisions

Internal ParameterSet holding a conjunction of posterior probability
criteria. Obtain instances through
[`getQuantitativeDecisionThresholds()`](https://docs.rpact.org/qtd/reference/getQuantitativeDecisionThresholds.md).

Stores validated posterior criteria.

Returns the object description.

## Arguments

- quantiles:

  Effect thresholds.

- probabilities:

  Posterior probability thresholds.

- lowerTail:

  Directions of the probability inequalities.

- startWithUpperCase:

  Capitalize the description.

## Details

All criteria must hold, using strict probability comparisons as in
RBesT. Thresholds refer to the effect in the beneficial direction.

## Fields

- `quantiles`:

  Supplied finite numeric vector of posterior effect thresholds on the
  beneficial-effect scale, one per criterion. All criteria are combined
  with logical AND.

- `probabilities`:

  Supplied numeric vector of strict posterior probability thresholds in
  (0, 1), one per quantile in the same order.

- `lowerTail`:

  Supplied logical vector, expanded from a scalar if needed, one per
  quantile. TRUE specifies P(effect \<= quantile) \> probability and
  FALSE specifies P(effect \> quantile) \> probability.

## See also

[`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md)
