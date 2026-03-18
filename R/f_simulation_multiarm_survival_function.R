## |
## |  *Simulation of multi-arm design with time to event data*
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

#' @include f_simulation_multiarm.R
NULL

#'
#' Calculates stage events for specified conditional power
#'
#' @noRd
#'
.getSimulationSurvivalMultiArmStageEvents <- function(
        ...,
        stage,
        directionUpper,
        conditionalPower,
        conditionalCriticalValue,
        plannedEvents,
        allocationRatioPlanned,
        selectedArms,
        thetaH1,
        overallEffects,
        minNumberOfEventsPerStage,
        maxNumberOfEventsPerStage
        ) {
    stage <- stage - 1 # to be consistent with non-multiarm situation
    gMax <- nrow(overallEffects)

    if (!is.na(conditionalPower)) {
        if (any(selectedArms[1:gMax, stage + 1], na.rm = TRUE)) {
            if (is.na(thetaH1)) {
                if (is.na(directionUpper) || isTRUE(directionUpper)) {
                    thetaStandardized <- log(max(
                        min(
                            overallEffects[selectedArms[1:gMax, stage + 1], stage],
                            na.rm = TRUE
                        ),
                        1 + 1e-07
                    ))
                } else {
                    thetaStandardized <- log(min(
                        max(
                            overallEffects[selectedArms[1:gMax, stage + 1], stage],
                            na.rm = TRUE
                        ),
                        1 - 1e-07
                    ))
                }
            } else {
                thetaStandardized <- log(min(
                    thetaH1,
                    1 + ifelse(is.na(directionUpper) || isTRUE(directionUpper), 1e-07, -1e-07)
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
