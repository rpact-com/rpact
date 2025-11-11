# Trial Design Characteristics

Class for trial design characteristics.

## Details

`TrialDesignCharacteristics` contains all fields required to collect the
characteristics of a design. This object should not be created directly;
use `getDesignCharacteristics` with suitable arguments to create it.

## Fields

- `nFixed`:

  The sample size in a fixed (one-stage) design. Is a positive numeric
  vector.

- `shift`:

  The shift value for group sequential test characteristics. Is a
  numeric vector of length 1.

- `inflationFactor`:

  The relative increase of maximum sample size in a group sequential
  design as compared to the fixed sample size case. Is a positive
  numeric vector of length 1.

- `stages`:

  The stage numbers of the trial. Is a numeric vector of length `kMax`
  containing whole numbers.

- `information`:

  The information over stages needed to achieve power of the specified
  design. Is a numeric vector of length `kMax`.

- `power`:

  The one-sided power at each stage of the trial. Is a numeric vector of
  length `kMax` containing values between 0 and 1.

- `rejectionProbabilities`:

  The rejection probabilities over treatments arms or populations and
  stages. Is a numeric vector.

- `futilityProbabilities`:

  The overall probabilities of stopping the trial for futility. Is a
  numeric vector of length `kMax` minus 1 containing values between 0
  and 1.

- `averageSampleNumber1`:

  The expected sample size under H1. Is a positive numeric vector of
  length 1.

- `averageSampleNumber01`:

  The expected sample size for a value between H0 and H1. Is a positive
  numeric vector of length 1.

- `averageSampleNumber0`:

  The expected sample size under H0. Is a positive numeric vector of
  length 1.

## See also

[`getDesignCharacteristics`](https://rpact-com.github.io/rpact/reference/getDesignCharacteristics.md)
for getting the design characteristics.
