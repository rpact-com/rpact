# Parameter Description: Lambda (1)

Parameter Description: Lambda (1)

## Arguments

- lambda1:

  The assumed hazard rate in the treatment group, there is no default.
  `lambda1` can also be used to define piecewise exponentially
  distributed survival times (see details). Must be a positive numeric
  value or vector: without `piecewiseSurvivalTime`, a vector defines
  alternative hazard rates to be considered; with
  `piecewiseSurvivalTime`, it contains one hazard rate per time
  interval.
  [`getEventProbabilities()`](https://docs.rpact.org/reference/getEventProbabilities.md)
  uses only the first value of a non-piecewise vector.
