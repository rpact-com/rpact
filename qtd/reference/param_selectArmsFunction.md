# Parameter Description: Select Arms Function

Parameter Description: Select Arms Function

## Arguments

- selectArmsFunction:

  Optionally, a function can be entered that defines the way of how
  treatment arms are selected. This function is allowed to depend on
  `effectVector` with length `activeArms`, `stage`, `conditionalPower`,
  `conditionalCriticalValue`, `plannedSubjects/plannedEvents`,
  `allocationRatioPlanned`, `selectedArms`, `thetaH1` (for means and
  survival), `stDevH1` (for means), `overallEffects`, and for rates
  additionally: `piTreatmentsH1`, `piControlH1`, `overallRates`, and
  `overallRatesControl` (see examples).
