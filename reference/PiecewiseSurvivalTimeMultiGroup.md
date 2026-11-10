# Piecewise Survival Times for Multiple Groups

Common time definition for multi-arm and enrichment survival
specifications. Groups, intervals, and situations occupy the first,
second, and third hazard-ratio dimensions. Use the specialized
constructors to specify their statistical roles.

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
