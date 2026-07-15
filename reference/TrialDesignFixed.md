# Fixed Design

Trial design for fixed (single-stage) trials.

## Details

This object should not be created directly; use
[`getDesignFixed()`](https://docs.rpact.org/reference/getDesignFixed.md)
with suitable arguments to create a fixed design.

## Fields

- `kMax`:

  The maximum number of stages `K`. Is a single numeric value
  representing a whole number.

- `alpha`:

  The significance level alpha, default is 0.025. Is a single numeric
  value between 0 and 1.

- `stages`:

  The stage numbers of the trial. Is a numeric vector of length `kMax`
  containing whole numbers.

- `tolerance`:

  The numerical tolerance, default is `1e-06`. Is a single numeric
  value.

- `sided`:

  Describes if the alternative is one-sided (`1`) or two-sided (`2`). Is
  a single numeric value representing a whole number.

- `twoSidedPower`:

  Specifies if power is defined two-sided at each stage of the trial. Is
  a single logical value.

## See also

[`getDesignFixed()`](https://docs.rpact.org/reference/getDesignFixed.md)
for creating a fixed design.
