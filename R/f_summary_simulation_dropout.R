## |
## |  *Internal dropout summaries shared by the patient-wise survival engines*
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
    result <- matrix(NA_real_, nrow = groups * arms, ncol = 7)
    for (g in seq_len(groups)) {
        last <- NA_integer_
        for (k in seq_along(stageResults$analysisTime)) {
            if (isTRUE(stageResults$eventsNotAchieved[k]) || !is.finite(stageResults$analysisTime[k])) {
                break
            }
            if ((sharedControl && g == groups) || isTRUE(selected[g, k])) {
                last <- k
            }
            if (isTRUE(closedTest$successStop[k]) || isTRUE(closedTest$futilityStop[k])) {
                break
            }
        }
        for (a in seq_len(arms)) {
            row <- (g - 1L) * arms + a
            result[row, 1:4] <- c(scenario, iteration, g, a)
            if (!is.na(last)) {
                result[row, 5:7] <- c(
                    stageResults$analysisTime[last],
                    stageResults$dropoutEnrolled[row, last], stageResults$dropoutDropped[row, last]
                )
            }
        }
    }
    return(result)
}

.addObservedDropoutProportions <- function(result, records, scenarios, groups = 1, multiArm = FALSE) {
    records <- as.data.frame(records)
    names(records) <- c("scenario", "iteration", "group", "arm", "horizon", "enrolled", "dropouts")
    result$.dropoutRecords <- records
    for (a in 1:2) {
        columns <- groups
        if (multiArm && a == 2) {
            columns <- 1L
        }
        proportions <- horizons <- matrix(NA_real_, nrow = columns, ncol = scenarios)
        contributors <- matrix(0L, nrow = columns, ncol = scenarios)
        for (g in seq_len(columns)) {
            for (i in seq_len(scenarios)) {
                selectedGroup <- g
                selectedArm <- a
                if (multiArm) {
                    selectedArm <- 1L
                    if (a == 2) {
                        selectedGroup <- groups + 1L
                    }
                }
                rows <- records$scenario == i & records$group == selectedGroup &
                    records$arm == selectedArm & is.finite(records$horizon) & records$enrolled > 0
                rows[is.na(rows)] <- FALSE
                contributors[g, i] <- sum(rows)
                if (any(rows)) {
                    proportions[g, i] <- mean(records$dropouts[rows] / records$enrolled[rows])
                    horizons[g, i] <- mean(records$horizon[rows])
                }
            }
        }
        # Group/population rows and scenario columns, with no stage dimension.
        if (columns == 1L) {
            proportions <- as.numeric(proportions)
            horizons <- as.numeric(horizons)
            contributors <- as.integer(contributors)
        }
        result[[paste0("observedDropoutProportion", a)]] <- proportions
        result[[paste0("dropoutObservationTime", a)]] <- horizons
        result[[paste0(".dropoutContributors", a)]] <- contributors
        result$.setParameterType(paste0("observedDropoutProportion", a), C_PARAM_GENERATED)
        result$.setParameterType(paste0("dropoutObservationTime", a), C_PARAM_GENERATED)
    }
}

.addDropoutSummaryItems <- function(result, summaryFactory) {
    if (inherits(result, "SimulationResultsEnrichmentSurvival")) {
        .addEnrichmentDropoutSummaryItems(result, summaryFactory)
        return(invisible(NULL))
    }
    if (inherits(result, "SimulationResultsMultiArmSurvival")) {
        .addMultiArmDropoutSummaryItems(result, summaryFactory)
        return(invisible(NULL))
    }
    fields <- c(
        "expectedDropoutProportion1", "expectedDropoutProportion2",
        "observedDropoutProportion1", "observedDropoutProportion2",
        "dropoutObservationTime1", "dropoutObservationTime2"
    )
    for (field in fields) {
        values <- result[[field]]
        if (is.null(values) || result$isNotApplicableParameter(field)) {
            next
        }
        if (is.null(dim(values))) {
            values <- matrix(values, nrow = 1)
        }
        for (g in seq_len(nrow(values))) {
            caption <- .getParameterCaption(field, result)
            if (nrow(values) > 1) {
                caption <- .getParameterSetParameterCaptionPerRow(result, field, caption, g)
            }
            for (i in seq_len(ncol(values))) {
                title <- caption
                if (ncol(values) > 1) {
                    title <- paste0(title, ", scenario ", i)
                }
                formatted <- .formatRates(values[g, i])
                if (startsWith(field, "dropoutObservationTime")) {
                    formatted <- .formatDropoutObservationTime(values[g, i])
                }
                summaryFactory$addItem(title, formatted)
            }
        }
    }
}

.addMultiArmDropoutSummaryItems <- function(result, summaryFactory) {
    fields <- c(
        "observedDropoutProportion1", "observedDropoutProportion2",
        "dropoutObservationTime1", "dropoutObservationTime2"
    )
    values <- lapply(fields, function(field) {
        value <- result[[field]]
        if (is.null(value) || result$isNotApplicableParameter(field)) {
            return(NULL)
        }
        if (is.null(dim(value))) {
            value <- matrix(value, nrow = 1)
        }
        return(value)
    })
    if (any(vapply(values, is.null, logical(1)))) {
        return(invisible(NULL))
    }

    proportions <- rbind(values[[1]], values[[2]])
    times <- rbind(values[[3]], values[[4]])
    armCaptions <- c(paste0("Treatment arm ", seq_len(nrow(values[[1]]))), "Control")
    listItemPrefix <- .getEnvironmentVariable(
        "RPACT_SUMMARY_LIST_ITEM_PREFIX", "rpact.summary.list.item.prefix",
        default = C_SUMMARY_LIST_ITEM_PREFIX_DEFAULT, type = "character"
    )
    for (i in seq_len(ncol(proportions))) {
        caption <- "Observed drop-out by arm"
        if (ncol(proportions) > 1) {
            caption <- paste0(caption, ", scenario ", i)
        }
        summaryFactory$addItem(caption, "")
        for (g in seq_len(nrow(proportions))) {
            summaryFactory$addItem(paste0(listItemPrefix, armCaptions[g]), "")
            summaryFactory$addItem("    Proportion", .formatRates(proportions[g, i]))
            summaryFactory$addItem("    Time", .formatDropoutObservationTime(times[g, i]))
        }
    }
}

.addEnrichmentDropoutSummaryItems <- function(result, summaryFactory) {
    fields <- c("observedDropoutProportion1", "observedDropoutProportion2", "dropoutObservationTime1")
    values <- lapply(fields, function(field) {
        value <- result[[field]]
        if (is.null(value) || result$isNotApplicableParameter(field)) {
            return(NULL)
        }
        if (is.null(dim(value))) {
            value <- matrix(value, nrow = 1)
        }
        return(value)
    })
    if (any(vapply(values, is.null, logical(1)))) {
        return(invisible(NULL))
    }

    for (i in seq_len(ncol(values[[1]]))) {
        caption <- "Observed drop-out by population"
        if (ncol(values[[1]]) > 1) {
            caption <- paste0(caption, ", scenario ", i)
        }
        summaryFactory$addItem(caption, "")
        for (g in seq_len(nrow(values[[1]]))) {
            populationCaption <- .getSummaryGroupCaption(result, fields[1], nrow(values[[1]]), g)
            summaryFactory$addItem(populationCaption, "")
            summaryFactory$addItem("    Proportion (1)", .formatRates(values[[1]][g, i]))
            summaryFactory$addItem("    Proportion (2)", .formatRates(values[[2]][g, i]))
            summaryFactory$addItem("    Time", .formatDropoutObservationTime(values[[3]][g, i]))
        }
    }
}
