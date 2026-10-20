#' @section Conditional power under piecewise hazards:
#' Conditional power is calculated using a scalar hazard ratio estimated from
#' the interim data or specified through \code{thetaH1}. The calculation uses
#' a proportional-hazards working model, including when survival times are
#' generated from piecewise hazards with interval-specific hazard ratios.
#' The interval-specific hazards are not used to project future test-statistic
#' drift in this calculation.
#'
#' Accordingly, \code{conditionalPowerAchieved} reports conditional power under
#' the scalar effect assumption for the selected number of events. It is not
#' an empirical rejection probability and, when hazard ratios vary over time,
#' need not equal the conditional rejection probability under the specified
#' piecewise survival model. The default event-number recalculation uses the
#' same working model; a custom \code{calcEventsFunction} can define a different
#' recalculation rule.
