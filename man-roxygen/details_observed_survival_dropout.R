#'
#' Patient-wise simulation reports \code{observedDropoutProportion1} and
#' \code{observedDropoutProportion2}, together with
#' \code{dropoutObservationTime1} and \code{dropoutObservationTime2}.
#' For each replication, a dropout must precede the event and occur by that arm or
#' population's finishing horizon, and the denominator counts subjects enrolled
#' by that horizon. The proportion is calculated separately in each replication
#' and then averaged over replications with a finite horizon and nonempty
#' denominator. Counts are not pooled across replications. The corresponding
#' observation time is the mean finishing horizon over exactly those same
#' replications, in the same units as accrual and survival times. Both summaries
#' are NA when no replication contributes; otherwise zero dropouts gives zero.
#'
#' A finishing horizon is the last analysis at which the arm or population
#' contributes. Discontinued arms and populations finish at their discontinuation
#' analysis; continuing groups and shared control finish at their last applicable
#' analysis. Overlapping enrichment populations retain their shared subjects.
#' Multi-arm treatment summaries have active-arm rows and scenario columns, while
#' shared control is a vector over scenarios. Enrichment summaries have population
#' rows (subpopulations followed by the full population) and scenario columns.
#' A single arm or population is stored as a vector; there is no stage dimension.
#' The two-arm outputs are vectors over alternative scenarios.
#'
#' These outputs require patient histories and are unavailable for
#' \code{simulationType = "testStatisticBased"}. Existing recruitment and
#' allocation mechanisms are retained. The input dropout probability at
#' \code{dropoutTime} describes the dropout distribution without competing events;
#' it is not the simulated observed proportion. Analytic expected proportions
#' condition on deterministic planned accrual at a fixed final planned horizon,
#' whereas simulation includes random analysis times, early stopping and selection.
#' Compare them only with matching horizon, enrollment and allocation assumptions.
#'
