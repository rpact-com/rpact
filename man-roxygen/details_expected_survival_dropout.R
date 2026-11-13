#'
#' The derived fields \code{expectedDropoutProportion1} and
#' \code{expectedDropoutProportion2} give expected observed dropout proportions
#' at each stage's planned analysis time in each scenario. Both fields are numeric
#' matrices with stage rows and alternative-scenario columns, including for a
#' single stage or scenario.
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
#' the corresponding stage's planned analysis, without early-stopping weighting.
#' An unavailable horizon (including calculations without observation times) or no accrued
#' subjects gives NA; otherwise zero dropout gives zero. Exponential and piecewise
#' exponential event times are supported. The existing restriction against
#' Weibull event times with nonzero dropout remains in place.
#'
#' Accrual averages use analytic integration, split at event-hazard boundaries.
#' When both input dropout rates are zero, the proportions are retained in the
#' result object but omitted from print and summary output.
#'
