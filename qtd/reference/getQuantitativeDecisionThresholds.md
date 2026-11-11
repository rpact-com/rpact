# Specify Posterior Probability Criteria

Creates a conjunction of strict posterior probability criteria, as in
RBesT.

## Usage

``` r
getQuantitativeDecisionThresholds(quantiles, probabilities, lowerTail = FALSE)
```

## Arguments

- quantiles:

  Finite effect thresholds on the beneficial-effect scale.

- probabilities:

  Probability thresholds in (0, 1), one per quantile.

- lowerTail:

  Logical scalar or vector. TRUE specifies P(effect \<= quantile) \>
  probability; FALSE specifies P(effect \> quantile) \> probability.

## Value

A QuantitativeDecisionThresholds object.

## See also

[`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md)
