# Parameter Description: accrualTime for Counts

Parameter Description: accrualTime for Counts

## Arguments

- accrualTime:

  If specified, the assumed accrual time interval(s) for the study,
  there is no default. A single value defines the end of accrual; if a
  vector is specified, its first value must be `0`. If `followUpTime` is
  specified, `accrualTime` can have at most two elements. Required for
  [`getSimulationCounts()`](https://docs.rpact.org/reference/getSimulationCounts.md).
