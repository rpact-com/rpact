# Fisher Design

Trial design for Fisher's combination test.

## Details

This object should not be created directly; use
[`getDesignFisher`](https://docs.rpact.org/reference/getDesignFisher.md)
with suitable arguments to create a Fisher combination test design.

## Fields

- `kMax`:

  The maximum number of stages `K`. Is a numeric vector of length 1
  containing a whole number.

- `alpha`:

  The significance level alpha, default is 0.025. Is a numeric vector of
  length 1 containing a value between 0 and 1.

- `stages`:

  The stage numbers of the trial. Is a numeric vector of length `kMax`
  containing whole numbers.

- `informationRates`:

  The information rates (that must be fixed prior to the trial), default
  is `(1:kMax) / kMax`. Is a numeric vector of length `kMax` containing
  values between 0 and 1.

- `userAlphaSpending`:

  The user defined alpha spending. Contains the cumulative
  alpha-spending (type I error rate) up to each interim stage. Is a
  numeric vector of length `kMax` containing values between 0 and 1.

- `criticalValues`:

  The critical values for each stage of the trial. Is a numeric vector
  of length `kMax`.

- `stageLevels`:

  The adjusted significance levels to reach significance in a group
  sequential design. Is a numeric vector of length `kMax` containing
  values between 0 and 1.

- `alphaSpent`:

  The cumulative alpha spent at each stage. Is a numeric vector of
  length `kMax` containing values between 0 and 1.

- `bindingFutility`:

  If `TRUE`, the calculation of the critical values is affected by the
  futility bounds and the futility threshold is binding in the sense
  that the study must be stopped if the futility condition was reached
  (default is `FALSE`) Is a logical vector of length 1.

- `tolerance`:

  The numerical tolerance, default is `1e-06`. Is a numeric vector of
  length 1.

- `method`:

  "equalAlpha", "fullAlpha", "noInteraction", or "userDefinedAlpha",
  default is "equalAlpha". For details, see Wassmer, 1999, doi:
  10.1002/(SICI)1521-4036(199906)41:3%3C279::AID-BIMJ279%3E3.0.CO;2-V.

- `alpha0Vec`:

  The stopping for futility bounds for stage-wise p-values in Fisher's
  combination test. Is a numeric vector of length `kMax` minus 1
  containing values between 0 and 1.

- `scale`:

  The scale for Fisher's combination test. Numeric vector of length
  `kMax-1` that applies to Fisher's design with unequally spaced
  information rates. Is a numeric vector of length `kMax` minus 1
  containing values between 0 and 1.

- `nonStochasticCurtailment`:

  If `TRUE`, the stopping rule is based on the phenomenon of
  non-stochastic curtailment rather than stochastic reasoning. Is a
  logical vector of length 1.

- `sided`:

  Describes if the alternative is one-sided (`1`) or two-sided (`2`). Is
  a numeric vector of length 1 containing a whole number.

- `simAlpha`:

  The observed alpha error if simulations have been performed. Is a
  numeric vector of length 1 containing a value between 0 and 1.

- `iterations`:

  The number of iterations used for simulations. Is a numeric vector of
  length 1 containing a whole number.

- `seed`:

  The seed used for random number generation. Is a numeric vector of
  length 1.

## See also

[`getDesignFisher`](https://docs.rpact.org/reference/getDesignFisher.md)
for creating a Fisher combination test design.
