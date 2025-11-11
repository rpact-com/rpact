# Inverse Normal Design

Trial design for inverse normal method.

## Details

This object should not be created directly; use
[`getDesignInverseNormal()`](https://rpact-com.github.io/rpact/reference/getDesignInverseNormal.md)
with suitable arguments to create a inverse normal design.

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

- `typeOfDesign`:

  The type of design. Is a character vector of length 1.

- `beta`:

  The Type II error rate necessary for providing sample size
  calculations (e.g., in `getSampleSizeMeans`), beta spending function
  designs, or optimum designs, default is `0.20`. Is a numeric vector of
  length 1 containing a value between 0 and 1.

- `deltaWT`:

  Delta for Wang & Tsiatis Delta class. Is a numeric vector of length 1.

- `deltaPT1`:

  Delta1 for Pampallona & Tsiatis class rejecting H0 boundaries. Is a
  numeric vector of length 1.

- `deltaPT0`:

  Delta0 for Pampallona & Tsiatis class rejecting H1 (accepting H0)
  boundaries. Is a numeric vector of length 1.

- `futilityBounds`:

  The futility bounds for each stage of the trial. Is a numeric vector
  of length `kMax`.

- `gammaA`:

  The parameter for the alpha spending function. Is a numeric vector of
  length 1.

- `gammaB`:

  The parameter for the beta spending function. Is a numeric vector of
  length 1.

- `optimizationCriterion`:

  The optimization criterion for optimum design within the Wang &
  Tsiatis class (`"ASNH1"`, `"ASNIFH1"`, `"ASNsum"`), default is
  `"ASNH1"`.

- `sided`:

  Describes if the alternative is one-sided (`1`) or two-sided (`2`). Is
  a numeric vector of length 1 containing a whole number.

- `betaSpent`:

  The cumulative beta level spent at each stage of the trial. Only
  applicable for beta-spending designs. Is a numeric vector of length
  `kMax` containing values between 0 and 1.

- `typeBetaSpending`:

  The type of beta spending. Is a character vector of length 1.

- `userBetaSpending`:

  The user defined beta spending. Contains the cumulative beta-spending
  up to each interim stage. Is a numeric vector of length `kMax`
  containing values between 0 and 1.

- `efficacyStops`:

  Logical vector indicating efficacy stops Is a logical vector of length
  `kMax` minus 1.

- `futilityStops`:

  Logical vector indicating futility stops Is a logical vector of length
  `kMax` minus 1.

- `power`:

  The one-sided power at each stage of the trial. Is a numeric vector of
  length `kMax` containing values between 0 and 1.

- `twoSidedPower`:

  Specifies if power is defined two-sided at each stage of the trial. Is
  a logical vector of length 1.

- `constantBoundsHP`:

  The constant bounds up to stage kMax - 1 for the Haybittle & Peto
  design (default is 3). Is a numeric vector of length 1.

- `betaAdjustment`:

  If `TRUE`, beta spending values are linearly adjusted if an
  overlapping of decision regions for futility stopping at earlier
  stages occurs. Only applicable for two-sided beta-spending designs. Is
  a logical vector of length 1.

- `delayedInformation`:

  Delay of information for delayed response designs. Is a numeric vector
  of length `kMax` minus 1 containing values between 0 and 1.

- `decisionCriticalValues`:

  The decision critical values for each stage of the trial in a delayed
  response design. Is a numeric vector of length `kMax`.

- `reversalProbabilities`:

  The probability to switch from stopping the trial for success (or
  futility) and reaching non-rejection (or rejection) in a delayed
  response design. Is a numeric vector of length `kMax` minus 1
  containing values between 0 and 1.

## See also

[`getDesignInverseNormal()`](https://rpact-com.github.io/rpact/reference/getDesignInverseNormal.md)
for creating a inverse normal design.
