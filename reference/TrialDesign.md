# Basic Trial Design

Basic class for trial designs.

## Details

`TrialDesign` is the basic class for

- [`TrialDesignFisher`](https://rpact-com.github.io/rpact/reference/TrialDesignFisher.md),

- [`TrialDesignGroupSequential`](https://rpact-com.github.io/rpact/reference/TrialDesignGroupSequential.md),

- [`TrialDesignInverseNormal`](https://rpact-com.github.io/rpact/reference/TrialDesignInverseNormal.md),
  and

- [`TrialDesignConditionalDunnett`](https://rpact-com.github.io/rpact/reference/TrialDesignConditionalDunnett.md).

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
