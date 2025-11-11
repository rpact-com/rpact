# Piecewise Exponential Survival Time

Class for the definition of piecewise survival times.

## Details

`PiecewiseSurvivalTime` is a class for the definition of piecewise
survival times.

## Fields

- `piecewiseSurvivalTime`:

  The time intervals for the piecewise definition of the exponential
  survival time cumulative distribution function. Is a numeric vector.

- `lambda1`:

  The assumed hazard rate in the treatment group. Is a numeric vector of
  length `kMax`.

- `lambda2`:

  The assumed hazard rate in the reference group. Is a numeric vector of
  length 1.

- `hazardRatio`:

  The hazard ratios under consideration. Is a numeric vector of length
  `kMax`.

- `pi1`:

  The assumed event rate in the treatment group. Is a numeric vector of
  length `kMax` containing values between 0 and 1.

- `pi2`:

  The assumed event rate in the control group. Is a numeric vector of
  length 1 containing a value between 0 and 1.

- `median1`:

  The assumed median survival time in the treatment group. Is a numeric
  vector.

- `median2`:

  The assumed median survival time in the reference group. Is a numeric
  vector of length 1.

- `eventTime`:

  The assumed time under which the event rates are calculated. Is a
  numeric vector of length 1.

- `kappa`:

  The shape of the Weibull distribution if `kappa!=1`. Is a numeric
  vector of length 1.

- `piecewiseSurvivalEnabled`:

  Indicates whether specification of piecewise definition of survival
  time is selected. Is a logical vector of length 1.

- `delayedResponseAllowed`:

  If `TRUE`, delayed response is allowed, if `FALSE` the response is not
  delayed.

- `delayedResponseEnabled`:

  If `TRUE`, delayed response is enabled, if `FALSE` delayed response is
  not enabled.
