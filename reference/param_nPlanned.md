# Parameter Description: N Planned

Parameter Description: N Planned

## Arguments

- nPlanned:

  The additional (i.e., "new" and not cumulative) sample size planned
  for each of the subsequent stages. The argument must be a vector with
  length equal to the number of remaining stages and contain the
  combined sample size from both treatment groups if two groups are
  considered. For survival outcomes, it should contain the planned
  number of additional events. For multi-arm designs, it is the
  per-comparison (combined) sample size. For enrichment designs, it is
  the (combined) sample size for the considered sub-population.
