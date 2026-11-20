# Parameter Description: Power

Parameter Description: Power

## Arguments

- power:

  Nominal target power, an alternative to `beta`, defined as `1 - beta`.
  Must be a single numeric value strictly between 0 and 1. If both are
  supplied, `abs(beta + power - 1) <= 1e-12` is required and the
  supplied `beta` is retained. The usual restrictions on `beta` and its
  relationship to `alpha` still apply. The default `NA_real_` leaves the
  existing `beta` default or derivation from user-defined beta spending
  unchanged. This scalar input does not replace the returned
  `design$power`: that field can contain cumulative rejection
  probabilities by stage, whose final value need not equal the nominal
  target, particularly for two-sided designs when only one rejection
  tail is counted.
