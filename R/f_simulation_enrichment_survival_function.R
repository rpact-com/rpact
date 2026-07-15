## |
## |  *Simulation of enrichment design with time to event data*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |
## |  RPACT company website: https://www.rpact.com
## |  rpact package website: https://www.rpact.org
## |
## |  Contact us for information about our services: info@rpact.com
## |

#'
#' @title Compute additional events for a stage in enrichment survival simulation
#'
#' @description
#' Internal helper used by enrichment survival simulation to compute the number
#' of events to schedule for the given stage. The function supports conditional
#' sample size reassessment when `conditionalPower` is specified and returns
#' a single numeric value representing new events for the specified stage.
#'
#' @param ... Currently unused; reserved for compatibility with user-supplied callbacks.
#' @param stage Integer stage index (1-based as used by callers). Note: the function
#'        internally converts to 0-based (`stage <- stage - 1`) for calculations.
#' @param directionUpper Logical or `NA`; one-sided test direction.
#' @param conditionalPower Numeric target conditional power (or `NA` to use planned events).
#' @param conditionalCriticalValue Numeric vector of conditional critical values (indexed by stage - 1).
#' @param plannedEvents Numeric vector of cumulative planned events per stage.
#' @param eventsOverStages Numeric vector/matrix with events observed over stages (not extensively checked).
#' @param allocationRatioPlanned Numeric vector of allocation ratios (active : control) per stage.
#' @param selectedPopulations Logical matrix indicating selected populations per stage (rows = populations, cols = stages).
#' @param estimatedTheta Numeric hypothesized effect (hazard ratio) used instead of observed effects when provided.
#' @param overallEffects Numeric matrix of observed overall effect estimates (rows = alternatives/populations, cols = stages).
#' @param minNumberOfEventsPerStage Numeric vector of minimum events allowed per stage.
#' @param maxNumberOfEventsPerStage Numeric vector of maximum events allowed per stage.
#'
#' @details
#' - If `conditionalPower` is `NA` the function returns the planned incremental
#'   events for the stage: `plannedEvents[stage+1] - plannedEvents[stage]`.
#' - Otherwise, if any population is selected at the current stage the function computes a
#'   standardized effect (`thetaStandardized`) using `overallEffects` or
#'   `estimatedTheta` and applies the conditional power formula. If the conditional
#'   critical value is very large (> 8) the upper bound `maxNumberOfEventsPerStage`
#'   for the next stage is returned. The result is clipped to the supplied min/max
#'   event bounds.
#' - The function returns a single numeric value (new events for the stage). Callers
#'   should ensure inputs are valid; this helper does not perform extensive argument checking.
#'
#' @return A single numeric value giving the number of new events to schedule for the stage.
#'
#' @examples
#' \dontrun{
#' # return planned increment when no conditional power is requested
#' .getSimulationSurvivalEnrichmentStageEvents(
#'     stage = 2,
#'     directionUpper = TRUE,
#'     conditionalPower = NA_real_,
#'     conditionalCriticalValue = rep(NA_real_, 1),
#'     plannedEvents = c(100, 200),
#'     eventsOverStages = c(100, 200),
#'     allocationRatioPlanned = c(1, 1),
#'     selectedPopulations = matrix(TRUE, nrow = 3, ncol = 2),
#'     estimatedTheta = NA_real_,
#'     overallEffects = matrix(1, nrow = 3, ncol = 2),
#'     minNumberOfEventsPerStage = c(0, 50),
#'     maxNumberOfEventsPerStage = c(0, 200)
#' )
#' }
#'
#' @keywords internal
#' @noRd
#'
.getSimulationSurvivalEnrichmentStageEvents <- function(
        ...,
        stage,
        directionUpper,
        conditionalPower,
        conditionalCriticalValue,
        plannedEvents,
        eventsOverStages,
        allocationRatioPlanned,
        selectedPopulations,
        estimatedTheta,
        overallEffects,
        minNumberOfEventsPerStage,
        maxNumberOfEventsPerStage) {
    stage <- stage - 1 # to be consistent with non-enrichment situation
    gMax <- nrow(overallEffects)

    if (!is.na(conditionalPower)) {
        if (any(selectedPopulations[1:gMax, stage + 1], na.rm = TRUE)) {
            if (is.na(estimatedTheta)) {
                if (is.na(directionUpper) || isTRUE(directionUpper)) {
                    thetaStandardized <- log(max(
                        min(
                            overallEffects[selectedPopulations[1:gMax, stage + 1], stage],
                            na.rm = TRUE
                        ),
                        1 + 1e-07
                    ))
                } else {
                    thetaStandardized <- log(min(
                        max(
                            overallEffects[selectedPopulations[1:gMax, stage + 1], stage],
                            na.rm = TRUE
                        ),
                        1 - 1e-07
                    ))
                }
            } else {
                thetaStandardized <- log(min(
                    estimatedTheta,
                    1 +
                        ifelse(
                            is.na(directionUpper) ||
                                isTRUE(directionUpper),
                            1e-07,
                            -1e-07
                        )
                ))
            }

            if (conditionalCriticalValue[stage] > 8) {
                newEvents <- maxNumberOfEventsPerStage[stage + 1]
            } else {
                newEvents <- (1 + allocationRatioPlanned[stage])^2 /
                    allocationRatioPlanned[stage] *
                    (max(
                        0,
                        conditionalCriticalValue[stage] +
                            .getQNorm(conditionalPower),
                        na.rm = TRUE
                    ))^2 /
                    thetaStandardized^2
                newEvents <- min(
                    max(minNumberOfEventsPerStage[stage + 1], newEvents),
                    maxNumberOfEventsPerStage[stage + 1]
                )
            }
        } else {
            newEvents <- 0
        }
    } else {
        newEvents <- plannedEvents[stage + 1] - plannedEvents[stage]
    }
    return(newEvents)
}

.getSimulationSurvivalEnrichmentStageEventsBasic <- function(
        ...,
        stage,
        directionUpper,
        conditionalPower,
        conditionalCriticalValue,
        plannedEvents,
        allocationRatioPlanned,
        selectedPopulations,
        thetaH1,
        overallEffects,
        minNumberOfEventsPerStage,
        maxNumberOfEventsPerStage) {
    stage <- stage - 1 # to be consistent with non-enrichment situation
    gMax <- nrow(overallEffects)

    if (!is.na(conditionalPower)) {
        if (any(selectedPopulations[1:gMax, stage + 1], na.rm = TRUE)) {
            if (is.na(thetaH1)) {
                if (is.na(directionUpper) || isTRUE(directionUpper)) {
                    thetaStandardized <- log(max(
                        min(
                            overallEffects[selectedPopulations[1:gMax, stage + 1], stage],
                            na.rm = TRUE
                        ),
                        1 + 1e-07
                    ))
                } else {
                    thetaStandardized <- log(min(
                        max(
                            overallEffects[selectedPopulations[1:gMax, stage + 1], stage],
                            na.rm = TRUE
                        ),
                        1 - 1e-07
                    ))
                }
            } else {
                thetaStandardized <- log(min(
                    thetaH1,
                    1 +
                        ifelse(
                            is.na(directionUpper) ||
                                isTRUE(directionUpper),
                            1e-07,
                            -1e-07
                        )
                ))
            }

            if (conditionalCriticalValue[stage] > 8) {
                newEvents <- maxNumberOfEventsPerStage[stage + 1]
            } else {
                newEvents <- (1 + allocationRatioPlanned[stage])^2 /
                    allocationRatioPlanned[stage] *
                    (max(
                        0,
                        conditionalCriticalValue[stage] +
                            .getQNorm(conditionalPower),
                        na.rm = TRUE
                    ))^2 /
                    thetaStandardized^2
                newEvents <- min(
                    max(minNumberOfEventsPerStage[stage + 1], newEvents),
                    maxNumberOfEventsPerStage[stage + 1]
                )
            }
        } else {
            newEvents <- 0
        }
    } else {
        newEvents <- plannedEvents[stage + 1] - plannedEvents[stage]
    }
    return(newEvents)
}
