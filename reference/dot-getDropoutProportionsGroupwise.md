# Average Dropout Incidence over Accrued Subjects

Calculates group-specific expected dropout proportions among subjects
enrolled by a calendar-time horizon, using piecewise constant accrual.

## Usage

``` r
.getDropoutProportionsGroupwise(
  time,
  accrualTimeVector,
  accrualIntensity,
  lambda1,
  lambda2,
  piecewiseSurvivalTime,
  phi,
  allocationRatioPlanned = 1
)
```

## Arguments

- time:

  Single finite calendar time since recruitment began.

- accrualTimeVector:

  Accrual interval boundaries; a leading zero is inserted if absent.

- accrualIntensity:

  Accrual rates, recycled across accrual intervals.

- lambda1, lambda2:

  Normalized event hazards for the two groups.

- piecewiseSurvivalTime:

  Normalized event-hazard interval start times. Ignored for a group with
  a single event hazard.

- phi:

  Vector of exponential dropout hazards for the two groups.

- allocationRatioPlanned:

  Constant planned treatment-to-control ratio.

## Value

A numeric vector of length two. Unavailable horizons, empty accrued
denominators, and groups without allocation yield missing proportions.

## Details

Averages competing-risk dropout incidence over entry times, dividing by
accrued enrollment rather than total planned enrollment. Constant
allocation cancels within each group. Follow-up intervals are split at
event-hazard changes and integrated analytically. On an interval from v
to w, let h be the total hazard and J(v) the joint survival at its
start. With z = h \* (w - v), the mean incidence is F_D(v) + phi \* J(v)
/ h \* B(z). The factor B(z) equals G_1(z) - G_2(z) / z, where G_k is
the gamma CDF with shape k and rate one. The second term is evaluated
using its log probability to avoid underflow before division. The factor
is zero at z = 0 by continuity. This implements the validation
specification's expected dropout proportion and analytic dropout accrual
integral formulas without numerical quadrature. The survival dropout
integration appendix derives these formulas and maps their mathematical
quantities to the implementation variables.
