# Conditional Dunnett Design

Trial design for conditional Dunnett tests.

## Details

This object should not be created directly; use
[`getDesignConditionalDunnett`](https://rpact-com.github.io/rpact/reference/getDesignConditionalDunnett.md)
with suitable arguments to create a conditional Dunnett test design.

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

- `informationAtInterim`:

  The information to be expected at interim, default is
  informationAtInterim = 0.5. Is a numeric vector of length 1 containing
  a value between 0 and 1.

- `secondStageConditioning`:

  The way the second stage p-values are calculated within the closed
  system of hypotheses. If `FALSE`, the unconditional adjusted p-values
  are used, otherwise conditional adjusted p-values are calculated. Is a
  logical vector of length 1.

- `sided`:

  Describes if the alternative is one-sided (`1`) or two-sided (`2`). Is
  a numeric vector of length 1 containing a whole number.

## See also

[`getDesignConditionalDunnett`](https://rpact-com.github.io/rpact/reference/getDesignConditionalDunnett.md)
for creating a conditional Dunnett test design.
