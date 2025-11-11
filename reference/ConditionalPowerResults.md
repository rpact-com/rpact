# Conditional Power Results

Class for conditional power calculations

## Details

This object cannot be created directly; use
[`getConditionalPower()`](https://rpact-com.github.io/rpact/reference/getConditionalPower.md)
with suitable arguments to create the results of a group sequential or a
combination test design.

## Fields

- `nPlanned`:

  The sample size planned for each of the subsequent stages. Is a
  numeric vector of length `kMax` containing whole numbers.

- `allocationRatioPlanned`:

  The planned allocation ratio (`n1 / n2`) for the groups. For multi-arm
  designs, it is the allocation ratio relating the active arm(s) to the
  control. Is a positive numeric vector of length 1.

- `iterations`:

  The number of iterations used for simulations. Is a numeric vector of
  length 1 containing a whole number.

- `seed`:

  The seed used for random number generation. Is a numeric vector of
  length 1.

- `simulated`:

  Describes if the power for Fisher's combination test has been
  simulated. Only applicable when using Fisher designs. Is a logical
  vector of length 1.

- `conditionalPower`:

  The conditional power at each stage of the trial. Is a numeric vector
  of length 1 containing a value between 0 and 1.

- `thetaH1`:

  The assumed effect under the alternative hypothesis. For survival
  designs, refers to the hazard ratio. Is a numeric vector.

- `assumedStDev`:

  The assumed standard deviation(s) for means analysis. Is a numeric
  vector.
