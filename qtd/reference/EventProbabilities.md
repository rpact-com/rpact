# Event Probabilities

Class for the definition of event probabilities.

## Details

`EventProbabilities` is a class for the definition of event
probabilities.

## Fields

- `time`:

  The time values. Is a numeric vector.

- `accrualTime`:

  The assumed accrual time intervals for the study. Is a numeric vector.

- `accrualIntensity`:

  The absolute accrual intensities. Is a numeric vector of length
  `kMax`.

- `kappa`:

  The shape of the Weibull distribution if `kappa!=1`. Is a single
  numeric value.

- `piecewiseSurvivalTime`:

  The time intervals for the piecewise definition of the exponential
  survival time cumulative distribution function. Is a numeric vector.

- `lambda1`:

  The assumed hazard rate in the treatment group. Is a numeric vector of
  length `kMax`.

- `lambda2`:

  The assumed hazard rate in the reference group. Is a single numeric
  value.

- `allocationRatioPlanned`:

  The planned allocation ratio (`n1 / n2`) for the groups. For multi-arm
  designs, it is the allocation ratio relating the active arm(s) to the
  control. Is a single positive numeric value.

- `hazardRatio`:

  The hazard ratios under consideration. Is a numeric vector of length
  `kMax`.

- `dropoutRate1`:

  The assumed drop-out rate in the treatment group. Is a single numeric
  value between 0 and 1.

- `dropoutRate2`:

  The assumed drop-out rate in the control group. Is a single numeric
  value between 0 and 1.

- `dropoutTime`:

  The assumed time for drop-out rates in the control and treatment
  group. Is a single numeric value.

- `maxNumberOfSubjects`:

  The maximum number of subjects for power calculations. Is a numeric
  vector.

- `overallEventProbabilities`:

  Deprecated field which will be removed in one of the next releases.
  Use `cumulativeEventProbabilities` instead.

- `cumulativeEventProbabilities`:

  The cumulative event probabilities in survival designs. Is a numeric
  vector.

- `eventProbabilities1`:

  The event probabilities in treatment group 1. Is a numeric vector.

- `eventProbabilities2`:

  The event probabilities in treatment group 2. Is a numeric vector.
