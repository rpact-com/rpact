# Parameter Description: Piecewise Multi-Arm Survival Time

Parameter Description: Piecewise Multi-Arm Survival Time

## Arguments

- piecewiseSurvivalTime:

  A `PiecewiseMultiArmSurvivalTime` object created by
  [`getPiecewiseMultiArmSurvivalTime()`](https://docs.rpact.org/reference/getPiecewiseMultiArmSurvivalTime.md).
  It specifies common interval start times, the control hazard in every
  interval, and an active-arm-by-interval hazard-ratio matrix for every
  situation. It can only be used for patient-wise simulation with
  `kappa = 1`, and cannot be combined with `piControl` or
  `effectMatrix`.
