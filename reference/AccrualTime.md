# Accrual Time

Class for the definition of accrual time and accrual intensity.

## Details

`AccrualTime` is a class for the definition of accrual time and accrual
intensity.

## Fields

- `endOfAccrualIsUserDefined`:

  If `TRUE`, the end of accrual has to be defined by the user (i.e., the
  length of `accrualTime` is equal to the length of
  `accrualIntensity -1`). Is a single logical value.

- `followUpTimeMustBeUserDefined`:

  Specifies whether follow up time needs to be defined or not. Is a
  single logical value.

- `maxNumberOfSubjectsIsUserDefined`:

  If `TRUE`, the maximum number of subjects has been specified by the
  user, if `FALSE`, it was calculated.

- `maxNumberOfSubjectsCanBeCalculatedDirectly`:

  If `TRUE`, the maximum number of subjects can directly be calculated.
  Is a single logical value.

- `absoluteAccrualIntensityEnabled`:

  If `TRUE`, absolute accrual intensity is enabled. Is a single logical
  value.

- `accrualTime`:

  The assumed accrual time intervals for the study. Is a numeric vector.

- `accrualIntensity`:

  The absolute accrual intensities. Is a numeric vector of length
  `kMax`.

- `accrualIntensityRelative`:

  The relative accrual intensities.

- `maxNumberOfSubjects`:

  The maximum number of subjects for power calculations. Is a numeric
  vector.

- `remainingTime`:

  In survival designs, the remaining time for observation. Is a single
  numeric value.

- `piecewiseAccrualEnabled`:

  Indicates whether piecewise accrual is selected. Is a single logical
  value.
