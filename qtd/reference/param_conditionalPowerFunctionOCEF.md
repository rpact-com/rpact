# Parameter Description: Conditional Power Function (optimal conditional error design)

Parameter Description: Conditional Power Function (optimal conditional
error design)

## Arguments

- conditionalPowerFunction:

  A function accepting a single first-stage p-value and returning a
  numeric scalar strictly between 0 and 1. The function is evaluated
  separately for each p-value. This function should not be increasing in
  the first-stage p-value, otherwise monotonicity issues may occur. With
  a conditional power function, the resulting conditional error function
  is not necessarily optimal for expected second-stage information.
