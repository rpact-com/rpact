# Parameter Description: Select Populations Function

Parameter Description: Select Populations Function

## Arguments

- selectPopulationsFunction:

  Optionally, a function can be entered that defines the way of how
  populations are selected. This function is allowed to depend on
  `effectVector` with length `populations` `stage`, `conditionalPower`,
  `conditionalCriticalValue`, `plannedSubjects/plannedEvents`,
  `allocationRatioPlanned`, `selectedPopulations`, `thetaH1` (for means
  and survival), `stDevH1` (for means), `overallEffects`, and for rates
  additionally: `piTreatmentsH1`, `piControlH1`, `overallRates`, and
  `overallRatesControl` (see examples).
