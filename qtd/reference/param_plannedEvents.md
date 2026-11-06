# Parameter Description: Planned Events

Parameter Description: Planned Events

## Arguments

- plannedEvents:

  `plannedEvents` is a numeric vector of length `kMax` (the number of
  stages of the design) that determines the number of cumulated
  (overall) events in survival designs when the interim stages are
  planned. For two treatment arms, it is the number of events for both
  treatment arms. For multi-arm designs, `plannedEvents` refers to the
  overall number of events for the selected arms plus control.
