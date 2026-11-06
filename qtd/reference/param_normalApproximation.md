# Parameter Description: Normal Approximation

Parameter Description: Normal Approximation

## Arguments

- normalApproximation:

  The type of computation of the p-values. Default is `FALSE` for
  testing means (i.e., the t test is used) and `TRUE` for testing rates
  and the hazard ratio. For testing rates, if
  `normalApproximation = FALSE` is specified, the binomial test (one
  sample) or the exact test of Fisher (two samples) is used for
  calculating the p-values. In the survival setting
  `normalApproximation = FALSE` has no effect.
