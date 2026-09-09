# Dataset of General Estimates

Class for a dataset of general estimates with optional degrees of
freedom and standard errors.

## Details

This object cannot be created directly; use
[`getDataset`](https://docs.rpact.org/reference/getDataset.md) with
`est`, `se`, and optionally `df` to create a dataset of general
estimates. If `df` is omitted, it is internally set to `Inf` for every
stage, corresponding to a normal approximation.

The estimates are endpoint-independent. Test statistics and information
are calculated directly from estimates, standard errors, and degrees of
freedom. Stage-wise test statistics are calculated as
`(estimate - thetaH0) / se`. Cumulative estimates use inverse-variance
weighting, with cumulative standard errors and Satterthwaite degrees of
freedom calculated on the same scale.

## Fields

- `stages`:

  The stage numbers of the trial. Is a numeric vector of length `kMax`
  containing whole numbers.

- `estimates`:

  Stage-wise estimates.

- `degreesOfFreedom`:

  Stage-wise degrees of freedom. If omitted on input, these are set to
  `Inf`.

- `standardErrors`:

  Stage-wise standard errors.
