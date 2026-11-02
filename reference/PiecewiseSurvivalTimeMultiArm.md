# Piecewise Exponential Survival Time for Multiple Treatment Arms

Class for piecewise exponential survival times in patient-wise multi-arm
simulations, with a common control hazard for all active treatment arms.

## Details

Create objects with
[`getPiecewiseSurvivalTimeMultiArm()`](https://docs.rpact.org/reference/getPiecewiseSurvivalTimeMultiArm.md).
The class inherits from `PiecewiseSurvivalTimeMultiGroup` and
`TimeDefinition`. Rows of `lambdaControls` repeat the common control
hazards for each active arm. The dimensions of `hazardRatios` and
`medianTreatments` are active arm, interval, and situation, in that
order. Interval starts begin at zero and the last interval is
open-ended. Supplied interval medians parameterize exponential hazards
through `log(2) / median`; they are not the medians of the complete
piecewise survival distributions. Derived hazards and hazard ratios are
retained alongside the supplied medians.

## Fields

- `piecewiseSurvivalTime`:

  The time intervals for the piecewise definition of the exponential
  survival time cumulative distribution function. Is a numeric vector.

- `lambdaControl`:

  The common control hazards for all active treatment arms, with one
  positive, finite value per piecewise survival interval. Is a numeric
  vector.

- `lambdaControls`:

  Positive, finite control hazards, with groups in rows and piecewise
  survival intervals in columns. Groups are active treatment arms
  sharing the same control hazards in multi-arm specifications, or
  disjoint population subsets in enrichment specifications. Is a numeric
  matrix.

- `hazardRatios`:

  Positive, finite treatment-to-control hazard ratios in a
  group-by-interval-by-situation numeric array. Groups are active
  treatment arms or disjoint population subsets. Situations occupy the
  third, slowest-changing dimension.

- `medianControl`:

  The supplied positive, finite exponential control medians, one per
  interval and common to all active arms, or NULL when control hazards
  were supplied directly. These parameterize interval hazards as
  `log(2) / medianControl`, rather than the median of the complete
  piecewise survival distribution.

- `medianTreatments`:

  The supplied positive, finite exponential treatment medians in a
  group-by-interval-by-situation numeric array, or NULL when hazard
  ratios were supplied directly. Dimensions and names match
  `hazardRatios`. These parameterize interval treatment hazards, rather
  than the medians of the complete piecewise survival distributions.

- `groupType`:

  The statistical role of the groups: `"multi-arm"` for active treatment
  arms sharing a common control, or `"enrichment"` for disjoint
  population subsets. Is a single character value.

## See also

[`getPiecewiseSurvivalTimeMultiArm()`](https://docs.rpact.org/reference/getPiecewiseSurvivalTimeMultiArm.md),
[PiecewiseSurvivalTimeEnrichment](https://docs.rpact.org/reference/PiecewiseSurvivalTimeEnrichment.md)
