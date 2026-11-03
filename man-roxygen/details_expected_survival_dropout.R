#'
#' The derived fields \code{expectedDropoutProportion1} and
#' \code{expectedDropoutProportion2} give expected observed dropout proportions
#' at each scenario's final planned analysis time. There is no stage dimension.
#' Enrollment, event time, and exponential dropout time are assumed independent
#' within each arm. A dropout is observed only before the event and by the analysis
#' horizon. These proportions differ from \code{dropoutRate1} and
#' \code{dropoutRate2}, which parameterize the exponential dropout distribution
#' at \code{dropoutTime} in the absence of competing events.
#'
#' Planning conditions on deterministic piecewise constant accrual and planned
#' allocation, retaining fractional accrued counts without rounding. Within each
#' accrual interval the individual competing-risk dropout incidence is averaged
#' over enrollment dates and weighted by planned enrollment by the horizon.
#' The denominator includes only subjects accrued by that time. The horizon is
#' the final planned analysis, not an average over early stopping. An unavailable
#' final horizon (including calculations without observation times) or no accrued
#' subjects gives NA; otherwise zero dropout gives zero. Exponential and piecewise
#' exponential event times are supported. The existing restriction against
#' Weibull event times with nonzero dropout remains in place.
#'
#' Accrual averages use one-dimensional numerical integration, split at event-hazard
#' boundaries, with relative tolerance \code{1e-9} and absolute tolerance
#' \code{1e-11}. Existing event-count and analysis-time calculations are unchanged.
#'
