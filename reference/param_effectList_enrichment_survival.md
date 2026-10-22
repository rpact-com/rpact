# Parameter Description: Effect List for Enrichment Survival

Parameter Description: Effect List for Enrichment Survival

## Arguments

- effectList:

  List of subsets, prevalences, and effect sizes with columns and number
  of rows reflecting the different situations to consider (see
  examples). For a piecewise exponential patient-wise survival
  simulation, specify `subGroups`, `prevalences`, and
  `piecewiseSurvivalTime`, where the latter is a
  `PiecewiseEnrichmentSurvivalTime` object created by
  [`getPiecewiseEnrichmentSurvivalTime()`](https://docs.rpact.org/reference/getPiecewiseEnrichmentSurvivalTime.md).
  Its subgroup names and the order of `subGroups` must agree. In this
  case, do not also specify `piControls`, `piTreatments`, or
  `hazardRatios`.
