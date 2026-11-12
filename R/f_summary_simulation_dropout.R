## |
## |  rpact: Confirmatory Adaptive Clinical Trial Design and Analysis
## |  This file is part of the rpact R package.
## |
## |  RPACT — Statistical Tools for Drug Development
## |
## |  Authors: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  SPDX-FileCopyrightText: RPACT GmbH, Germany <info@rpact.com>
## |  SPDX-License-Identifier: LGPL-3.0-only
## |
## |  Licensed under the GNU Lesser General Public License, version 3.
## |  License text: https://www.gnu.org/licenses/lgpl-3.0.html
## |
## |  Package documentation: https://www.rpact.org
## |  Resources and customer portal: https://connect.rpact.app
## |  Consulting and services: https://www.rpact.com
## |  Contact: info@rpact.com
## |

.captureDropoutCounts <- function(data, time, member) {
    enrolled <- member & data$accrualTime <= time
    dropped <- enrolled & !is.na(data$dropoutTime) &
        data$dropoutTime < data$survivalTime & data$accrualTime + data$dropoutTime <= time
    return(c(sum(enrolled), sum(dropped)))
}

.finishDropoutRecords <- function(
        stageResults,
        closedTest,
        scenario,
        iteration,
        groups,
        arms,
        sharedControl = FALSE) {
    selected <- closedTest$selectedArms
    if (is.null(selected)) {
        selected <- closedTest$selectedPopulations
    }
    kMax <- length(stageResults$analysisTime)
    result <- matrix(NA_real_, nrow = groups * arms * kMax, ncol = 8)
    for (g in seq_len(groups)) {
        stopped <- FALSE
        for (k in seq_len(kMax)) {
            available <- !stopped && !isTRUE(stageResults$eventsNotAchieved[k]) &&
                is.finite(stageResults$analysisTime[k])
            contributes <- available && ((sharedControl && g == groups) || isTRUE(selected[g, k]))
            for (a in seq_len(arms)) {
                groupRow <- (g - 1L) * arms + a
                row <- (groupRow - 1L) * kMax + k
                result[row, 1:5] <- c(scenario, iteration, g, a, k)
                if (contributes) {
                    result[row, 6:8] <- c(stageResults$analysisTime[k],
                        stageResults$dropoutEnrolled[groupRow, k], stageResults$dropoutDropped[groupRow, k])
                }
            }
            if (!available || isTRUE(closedTest$successStop[k]) || isTRUE(closedTest$futilityStop[k])) {
                stopped <- TRUE
            }
        }
    }
    return(result)
}

.addObservedDropoutProportions <- function(result, records, scenarios, groups = 1, multiArm = FALSE) {
    records <- as.data.frame(records)
    names(records) <- c("scenario", "iteration", "group", "arm", "stage", "horizon", "enrolled", "dropouts")
    result$.dropoutRecords <- records
    kMax <- result$.design$kMax
    for (a in 1:2) {
        numberOfGroups <- groups
        if (multiArm && a == 2) {
            numberOfGroups <- 1L
        }
        proportions <- horizons <- array(NA_real_, dim = c(kMax, scenarios, numberOfGroups))
        contributors <- array(0L, dim = c(kMax, scenarios, numberOfGroups))
        for (g in seq_len(numberOfGroups)) {
            for (i in seq_len(scenarios)) {
                selectedGroup <- g
                selectedArm <- a
                if (multiArm) {
                    selectedArm <- 1L
                    if (a == 2) {
                        selectedGroup <- groups + 1L
                    }
                }
                for (k in seq_len(kMax)) {
                    rows <- records$scenario == i & records$group == selectedGroup &
                        records$arm == selectedArm & records$stage == k &
                        is.finite(records$horizon) & records$enrolled > 0
                    rows[is.na(rows)] <- FALSE
                    contributors[k, i, g] <- sum(rows)
                    if (any(rows)) {
                        proportions[k, i, g] <- mean(records$dropouts[rows] / records$enrolled[rows])
                        horizons[k, i, g] <- mean(records$horizon[rows])
                    }
                }
            }
        }
        # Keep stage and scenario dimensions, even for a single stage or scenario.
        if (numberOfGroups == 1L) {
            proportions <- matrix(proportions, nrow = kMax)
            horizons <- matrix(horizons, nrow = kMax)
            contributors <- matrix(contributors, nrow = kMax)
        }
        result[[paste0("observedDropoutProportion", a)]] <- proportions
        result[[paste0("dropoutObservationTime", a)]] <- horizons
        result[[paste0(".dropoutContributors", a)]] <- contributors
        result$.setParameterType(paste0("observedDropoutProportion", a), C_PARAM_GENERATED)
        result$.setParameterType(paste0("dropoutObservationTime", a), C_PARAM_GENERATED)
    }
}

.isDropoutOutputHidden <- function(result, field) {
    if (grepl("^dropoutObservationTime[12]$", field)) {
        return(TRUE)
    }
    if (!grepl("^(expected|observed)DropoutProportion[12]$", field)) {
        return(FALSE)
    }
    rates <- c(result[["dropoutRate1"]], result[["dropoutRate2"]])
    return(length(rates) > 0 && all(!is.na(rates) & rates == 0))
}

.addDropoutSummaryItems <- function(result, summaryFactory) {
    if (inherits(result, "SimulationResultsMultiArmSurvival")) {
        .addMultiArmDropoutSummaryItems(result, summaryFactory)
        return(invisible(NULL))
    }
    fields <- c("expectedDropoutProportion1", "expectedDropoutProportion2",
        "observedDropoutProportion1", "observedDropoutProportion2")
    legendEntry <- .getDropoutLegendEntries(result)
    for (field in fields) {
        values <- result[[field]]
        if (is.null(values) || result$isNotApplicableParameter(field) || .isDropoutOutputHidden(result, field)) {
            next
        }
        caption <- .getParameterCaption(field, result)
        if (length(dim(values)) == 3L) {
            .addSimulationArrayToSummary(result, field, caption, summaryFactory,
                digitsSampleSize = 0, legendEntry = legendEntry)
        } else {
            summaryFactory$addParameter(result, parameterName = field,
                parameterCaption = caption, roundDigits = 0, showNA = TRUE,
                legendEntry = legendEntry,
                transpose = inherits(result, "SimulationResultsMultiArmSurvival") ||
                    inherits(result, "SimulationResultsEnrichmentSurvival"))
        }
    }
}

.getDropoutLegendEntries <- function(result) {
    if (!inherits(result, c("SimulationResultsSurvival", "SimulationResultsEnrichmentSurvival"))) {
        return(list())
    }
    for (field in c("observedDropoutProportion1", "observedDropoutProportion2")) {
        if (!is.null(result[[field]]) && !result$isNotApplicableParameter(field) &&
                !.isDropoutOutputHidden(result, field)) {
            return(.addLegendEntry("treatmentArmValues"))
        }
    }
    return(list())
}

.addMultiArmDropoutSummaryItems <- function(result, summaryFactory) {
    treatment <- result$observedDropoutProportion1
    control <- result$observedDropoutProportion2
    if (is.null(treatment) || is.null(control) ||
            result$isNotApplicableParameter("observedDropoutProportion1") ||
            result$isNotApplicableParameter("observedDropoutProportion2") ||
            .isDropoutOutputHidden(result, "observedDropoutProportion1")) {
        return(invisible(NULL))
    }
    numberOfArms <- 1L
    if (length(dim(treatment)) == 3L) {
        numberOfArms <- dim(treatment)[3]
    }
    treatment <- array(treatment, dim = c(nrow(control), ncol(control), numberOfArms))
    listItemPrefix <- .getEnvironmentVariable(
        "RPACT_SUMMARY_LIST_ITEM_PREFIX", "rpact.summary.list.item.prefix",
        default = C_SUMMARY_LIST_ITEM_PREFIX_DEFAULT, type = "character"
    )
    for (i in seq_len(ncol(control))) {
        group <- .getSummaryGroup("Observed drop-out proportion", ncol(control), i, result)
        summaryFactory$addItem(group$groupCaption, "", legendEntry = group$legendEntry)
        for (g in seq_len(numberOfArms)) {
            summaryFactory$addParameter(result,
                parameterName = "observedDropoutProportion1", values = treatment[, i, g],
                parameterCaption = paste0(listItemPrefix, "Treatment arm ", g),
                roundDigits = 0, enforceFirstCase = TRUE
            )
        }
        summaryFactory$addParameter(result,
            parameterName = "observedDropoutProportion2", values = control[, i],
            parameterCaption = paste0(listItemPrefix, "Control"),
            roundDigits = 0, enforceFirstCase = TRUE
        )
    }
}
