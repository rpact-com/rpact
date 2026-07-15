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

  The assumed hazard rate in the reference group. Is a single numeric
  value.

- `hazardRatio`:

  The hazard ratios under consideration. Is a numeric vector of length
  `kMax`.

- `pi1`:

  The assumed event rate in the treatment group. Is a numeric vector of
  length `kMax` containing values between 0 and 1.

- `pi2`:

  The assumed event rate in the control group. Is a single numeric value
  between 0 and 1.

- `median1`:

  The assumed median survival time in the treatment group. Is a numeric
  vector.

- `median2`:

  The assumed median survival time in the reference group. Is a single
  numeric value.

- `eventTime`:

  The assumed time under which the event rates are calculated. Is a
  single numeric value.

- `kappa`:

  The shape of the Weibull distribution if `kappa!=1`. Is a single
  numeric value.

- `piecewiseSurvivalEnabled`:

  Indicates whether specification of piecewise definition of survival
  time is selected. Is a single logical value.

- `delayedResponseAllowed`:

  If `TRUE`, delayed response is allowed, if `FALSE` the response is not
  delayed.

- `delayedResponseEnabled`:

  If `TRUE`, delayed response is enabled, if `FALSE` delayed response is
  not enabled.
