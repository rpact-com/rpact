# Parameter Description: Maximum planning effect (optimal conditional error design)

Parameter Description: Maximum planning effect (optimal conditional
error design)

## Arguments

- maxThetaH1:

  The maximum for an interim estimate of the treatment effect, specified
  on the mean difference scale. If the interim estimate (on the mean
  difference scale) yields a value larger than `maxThetaH1`,
  `maxThetaH1` is used for it. Is only used if
  `useInterimEstimate=TRUE`. Must be a numeric value. Default value is
  `Inf`, i.e., no upper restriction.
