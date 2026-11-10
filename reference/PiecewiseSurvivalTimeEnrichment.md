# Piecewise Exponential Survival Time for Population Enrichment

Class for piecewise exponential survival times in patient-wise
population- enrichment simulations, with control hazards and treatment
effects specified for disjoint population subsets.

## Details

Create objects with
[`getPiecewiseSurvivalTimeEnrichment()`](https://docs.rpact.org/reference/getPiecewiseSurvivalTimeEnrichment.md).
The class inherits from `PiecewiseSurvivalTimeMultiGroup` and
`TimeDefinition`. Rows of `lambdaControls` and `medianControls`
correspond to disjoint population subsets. The dimensions of
`hazardRatio` and `medianTreatments` are subset, interval, and
situation, in that order. Interval starts begin at zero and the last
interval is open-ended. Supplied interval medians parameterize
exponential hazards through `log(2) / median`; they are not the medians
of the complete piecewise survival distributions. Derived hazards and
hazard ratios are retained alongside the supplied medians.

## Fields

- `piecewiseSurvivalTime`:

  The time intervals for the piecewise definition of the exponential
  survival time cumulative distribution function. Is a numeric vector.

- `lambdaControls`:

  Positive, finite control hazards, with groups in rows and piecewise
  survival intervals in columns. Groups are active treatment arms
  sharing the same control hazards in multi-arm specifications, or
  disjoint population subsets in enrichment specifications. Is a numeric
  matrix.

- `hazardRatio`:

  Positive, finite treatment-to-control hazard ratios in a
  group-by-interval-by-situation numeric array. Groups are active
  treatment arms or disjoint population subsets. Situations occupy the
  third, slowest-changing dimension. Values are supplied directly or
  derived from the supplied interval medians.

- `medianControls`:

  The supplied positive, finite exponential control medians in a
  subset-by-interval numeric matrix, or NULL when control hazards were
  supplied directly. These parameterize interval hazards as
  `log(2) / medianControls`, rather than the medians of the complete
  piecewise survival distributions.

- `medianTreatments`:

  The supplied positive, finite exponential treatment medians in a
  group-by-interval-by-situation numeric array, or NULL when hazard
  ratios were supplied directly. Dimensions and names match
  `hazardRatio`. These parameterize interval treatment hazards, rather
  than the medians of the complete piecewise survival distributions.

- `groupType`:

  The statistical role of the groups: `"multi-arm"` for active treatment
  arms sharing a common control, or `"enrichment"` for disjoint
  population subsets. Is a single character value.

## See also

[`getPiecewiseSurvivalTimeEnrichment()`](https://docs.rpact.org/reference/getPiecewiseSurvivalTimeEnrichment.md),
[PiecewiseSurvivalTimeMultiArm](https://docs.rpact.org/reference/PiecewiseSurvivalTimeMultiArm.md)
