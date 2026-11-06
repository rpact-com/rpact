# Parameter Description: Minimum planning effect (optimal conditional error design)

Parameter Description: Minimum planning effect (optimal conditional
error design)

## Arguments

- minThetaH1:

  The minimum for an interim estimate of the treatment effect, specified
  on the mean difference scale. If the interim estimate (on the mean
  difference scale) yields a value smaller than `minThetaH1`,
  `minThetaH1` is used for it. Is only used if
  `useInterimEstimate=TRUE`. Must be a numeric value.
