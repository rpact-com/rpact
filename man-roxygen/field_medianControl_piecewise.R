#' Field description: median {control, piecewise}
#' @field medianControl The supplied positive, finite exponential control medians, one per interval and common to all active arms, or NULL when control hazards were supplied directly. These parameterize interval hazards as \code{log(2) / medianControl}, rather than the median of the complete piecewise survival distribution.
#' @keywords internal
