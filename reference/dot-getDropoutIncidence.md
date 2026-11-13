# Calculate Competing-Risk Dropout Incidence

Calculates the probability of dropout before an event by a follow-up
time, for exponential dropout and exponential or piecewise exponential
event times.

## Usage

``` r
.getDropoutIncidence(time, piecewiseLambda, piecewiseSurvivalTime, phi)
```

## Arguments

- time:

  Vector of follow-up times since enrollment. Negative times give zero
  incidence; missing times remain missing.

- piecewiseLambda:

  Normalized nonnegative event hazards, one per interval.

- piecewiseSurvivalTime:

  Normalized interval start times, starting at zero. Ignored when there
  is a single event hazard.

- phi:

  Nonnegative exponential dropout hazard.

## Value

A numeric vector of dropout incidences, with one entry per time.

## Details

Integrates the dropout hazard times joint event-free and dropout-free
survival analytically over the event-hazard intervals. Inputs are
normalized by the survival model before this internal helper is called.
