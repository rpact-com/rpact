#' @section Treatment arm and population selection:
#' Selection determines which treatment arms or populations continue to the next
#' stage. It is applied at interim analysis \code{k} only if
#' \code{adaptations[k] = TRUE}; the default enables selection at every interim.
#' Otherwise, the current selection is retained. Once dropped, an arm or
#' population cannot be selected again. In multi-arm designs, these rules apply
#' to the active treatment arms; the control group is retained.
#'
#' The ranking uses the cumulative interim data and the criterion specified by
#' \code{effectMeasure}: the estimated treatment effect for
#' \code{"effectEstimate"} (the default), or the test statistic for
#' \code{"testStatistic"}. The selection functions use a vector
#' \code{effectVector} on a scale where larger values rank better. For survival
#' endpoints with \code{effectMeasure = "effectEstimate"}, this is the estimated
#' hazard ratio for \code{directionUpper = TRUE} and its reciprocal for
#' \code{directionUpper = FALSE}.
#'
#' \describe{
#'   \item{\code{"best"} (default)}{Selects only the single best remaining arm
#'   or population. Thus, with three active arms and selection enabled at the
#'   first interim, only one arm continues to stage 2. It does not drop just the
#'   worst arm. If several have the same best value, the first in the original
#'   order is selected.}
#'   \item{\code{"rBest"}}{Selects the \code{rValue} highest-ranked remaining
#'   arms or populations, subject to availability. Specify \code{rValue} as a
#'   positive integer. For example, \code{rValue = 2} selects the best two;
#'   \code{rValue = 1} gives the same selection as \code{"best"}.
#'   The same \code{rValue} is used at every enabled interim, so this option
#'   does not mean dropping one arm or population at each interim.
#'   The spelling \code{"rbest"} is also accepted.}
#'   \item{\code{"epsilon"}}{Selects the best and all other remaining arms or
#'   populations whose selection value is at most \code{epsilonValue} below
#'   the best value, including equality. Specify \code{epsilonValue} on the
#'   scale of \code{effectVector}. The number selected depends on the observed
#'   differences and can vary between simulation runs. For example, selection
#'   values \code{c(0.20, 0.35, 0.40)} and \code{epsilonValue = 0.10} retain
#'   the second and third arms or populations. For response-rate differences,
#'   \code{0.10} represents 10 percentage points.}
#'   \item{\code{"all"}}{Retains all remaining arms or populations without
#'   ranking them. This disables dropping based on relative performance, but
#'   threshold conditions and stopping rules still apply.}
#'   \item{\code{"userDefined"}}{Uses \code{selectArmsFunction} in multi-arm
#'   designs or \code{selectPopulationsFunction} in enrichment designs.
#'   Supply a function returning a logical vector in the original arm or
#'   population order, with \code{TRUE} indicating selection. Its length must
#'   equal the original number of active arms or populations. The function
#'   can use \code{stage}, \code{effectVector}, and the other arguments listed
#'   in the corresponding parameter description. Use this option for rules
#'   whose number selected changes by stage, such as dropping the worst
#'   remaining arm at each interim.}
#' }
#'
#' For the four built-in rules, selected arms or populations must additionally
#' have a selection value strictly above \code{threshold}. The default
#' \code{threshold = -Inf} imposes no additional restriction. Consequently,
#' fewer than the requested number may continue, or none may continue.
#' For \code{"userDefined"}, implement any threshold condition in the supplied
#' function. Efficacy and futility stopping rules apply separately from selection.
#'
#' For example, the following multi-arm selection function drops one worst
#' remaining arm at each enabled interim, while retaining at least one arm.
#' Pass it as \code{selectArmsFunction = dropWorstArm} together with
#' \code{typeOfSelection = "userDefined"}. If three arms are still available
#' at the first interim, it retains two; at the next enabled interim it retains
#' one, provided the trial continues.
#' \preformatted{
#' dropWorstArm <- function(effectVector, selectedArms, stage) {
#'     keep <- selectedArms[seq_along(effectVector), stage] & !is.na(effectVector)
#'     if (sum(keep) > 1L) {
#'         remaining <- which(keep)
#'         worst <- remaining[which.min(effectVector[remaining])]
#'         keep[worst] <- FALSE
#'     }
#'     keep
#' }
#' }
