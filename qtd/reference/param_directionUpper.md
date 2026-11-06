# Parameter Description: Direction Upper

Parameter Description: Direction Upper

## Arguments

- directionUpper:

  Logical. Specifies the direction of the alternative, only applicable
  for one-sided testing; `TRUE` means that larger values of the test
  statistics yield smaller p-values. The default is `TRUE` for means,
  rates, and count data, and `FALSE` for survival data (power
  calculations, simulations, and analyses). In sample size calculations,
  the direction is derived from the specified alternative; a specified
  `directionUpper` must agree with it.
