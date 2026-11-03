# Internal dropout summaries shared by the patient-wise survival engines.

.captureDropoutCounts <- function(data, time, member) {
    enrolled <- member & data$accrualTime <= time
    dropped <- enrolled & !is.na(data$dropoutTime) &
        data$dropoutTime < data$survivalTime & data$accrualTime + data$dropoutTime <= time
    return(c(sum(enrolled), sum(dropped)))
}

.finishDropoutRecords <- function(stageResults, closedTest, scenario, iteration, groups, arms,
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
                result[row, 5:7] <- c(stageResults$analysisTime[last],
                    stageResults$dropoutEnrolled[row, last], stageResults$dropoutDropped[row, last])
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
    fields <- c("expectedDropoutProportion1", "expectedDropoutProportion2",
        "observedDropoutProportion1", "observedDropoutProportion2",
        "dropoutObservationTime1", "dropoutObservationTime2")
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
                    formatted <- .formatTime(values[g, i])
                }
                summaryFactory$addItem(title, formatted)
            }
        }
    }
}
