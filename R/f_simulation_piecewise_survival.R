## |
## |  *Time classes for patient-wise simulations*
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

.showPiecewiseSimulationParameters <- function(simulationResults, consoleOutputEnabled = TRUE) {
    specification <- simulationResults$piecewiseSurvivalTime
    if (!inherits(specification, "PiecewiseSurvivalTimeMultiGroup")) {
        return(invisible(NULL))
    }
    captions <- "Interval start times"
    values <- list(specification$piecewiseSurvivalTime)
    indent <- if (consoleOutputEnabled) {
        "  "
    } else {
        ""
    }
    if (specification$groupType == "multi-arm") {
        captions <- c(captions, "Control hazards (common)")
        values <- c(values, list(.formatRatios(specification$lambdaControls[1, ])))
    } else {
        for (group in seq_len(specification$getNumberOfGroups())) {
            captions <- c(captions, paste0("Control hazards (", rownames(specification$lambdaControls)[group], ")"))
            values <- c(values, list(.formatRatios(specification$lambdaControls[group, ])))
        }
    }
    groupNames <- dimnames(specification$hazardRatios)[[1]]
    situationNames <- dimnames(specification$hazardRatios)[[3]]
    for (situation in seq_along(situationNames)) {
        for (group in seq_along(groupNames)) {
            captions <- c(captions, paste0("Hazard ratios (", groupNames[group], "), ", situationNames[situation]))
            values <- c(values, list(.formatRatios(specification$hazardRatios[group, , situation])))
        }
    }
    simulationResults$.cat(indent, "Piecewise survival parameters:\n",
        heading = if (consoleOutputEnabled) {
            0
        } else {
            3
        },
        consoleOutputEnabled = consoleOutputEnabled)
    width <- max(simulationResults$.getNChar(), nchar(captions))
    for (i in seq_along(captions)) {
        simulationResults$.cat(indent,
            .getFormattedVariableName(captions[i], n = width), " ",
            paste(values[[i]], collapse = ", "), "\n", consoleOutputEnabled = consoleOutputEnabled)
    }
    simulationResults$.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
    return(invisible(NULL))
}

.getPiecewiseSurvivalSummaryText <- function(specification) {
    formatVector <- function(values) {
        return(.arrayToString(values, digits = 4, vectorLookAndFeelEnabled = TRUE))
    }
    text <- paste0("H1: piecewise exponential survival distribution with interval start times = ",
        formatVector(specification$piecewiseSurvivalTime), " (last interval open-ended)")
    if (specification$groupType == "multi-arm") {
        text <- .concatenateSummaryText(text, paste0("common control hazards = ",
            formatVector(specification$lambdaControls[1, ])))
    } else {
        for (group in seq_len(specification$getNumberOfGroups())) {
            text <- .concatenateSummaryText(text, paste0("control hazards (",
                rownames(specification$lambdaControls)[group], ") = ",
                formatVector(specification$lambdaControls[group, ])))
        }
    }
    groupNames <- dimnames(specification$hazardRatios)[[1]]
    situationNames <- dimnames(specification$hazardRatios)[[3]]
    for (situation in seq_along(situationNames)) {
        ratios <- vapply(seq_along(groupNames), function(group) {
            return(paste0(groupNames[group], " = ", formatVector(specification$hazardRatios[group, , situation])))
        }, character(1))
        text <- .concatenateSummaryText(text, paste0("hazard ratios for ", situationNames[situation],
            " (in interval order): ", paste(ratios, collapse = "; ")))
    }
    return(text)
}

.getPiecewiseInitialHazardRatios <- function(piecewiseSurvivalTime) {
    piecewiseSurvivalTime$.validate()
    # Initial ratios are compatibility inputs, not effects averaged over time.
    hazardRatios <- piecewiseSurvivalTime$hazardRatios
    return(t(matrix(hazardRatios[, 1, , drop = FALSE],
        nrow = dim(hazardRatios)[1], ncol = dim(hazardRatios)[3],
        dimnames = dimnames(hazardRatios)[c(1, 3)])))
}

.alignPiecewiseSurvivalGroups <- function(piecewiseSurvivalTime, groups) {
    if (is.null(piecewiseSurvivalTime) ||
            identical(dimnames(piecewiseSurvivalTime$hazardRatios)[[1]], groups)) {
        return(piecewiseSurvivalTime)
    }
    # Effect-list validation orders subgroups; keep the hazards in that same order
    # without changing the specification supplied by the caller.
    aligned <- piecewiseSurvivalTime$clone()
    aligned$.validate()
    actualGroups <- dimnames(aligned$hazardRatios)[[1]]
    .assertPiecewiseNames(groups, length(actualGroups), "subGroups", ".alignPiecewiseSurvivalGroups")
    if (!setequal(groups, actualGroups)) {
        stopConflictingArguments("subgroup names must match the piecewise survival specification",
            functionName = ".alignPiecewiseSurvivalGroups", parameter = "subGroups", value = groups,
            constraint = "same subgroup identities", relatedParameter = "groupNames",
            relatedValue = actualGroups, diagnosticId = "survival.piecewise_group_identity")
    }
    indices <- match(groups, actualGroups)
    aligned$lambdaControls <- aligned$lambdaControls[indices, , drop = FALSE]
    aligned$hazardRatios <- aligned$hazardRatios[indices, , , drop = FALSE]
    if (!is.null(aligned[["medianControls"]])) {
        aligned$medianControls <- aligned$medianControls[indices, , drop = FALSE]
    }
    if (!is.null(aligned$medianTreatments)) {
        aligned$medianTreatments <- aligned$medianTreatments[indices, , , drop = FALSE]
    }
    return(aligned)
}

.getPiecewiseSurvivalScenarios <- function(piecewiseSurvivalTime) {
    if (is.null(piecewiseSurvivalTime)) {
        return(NULL)
    }
    activeHazards <- piecewiseSurvivalTime$getActiveHazards()
    return(lapply(seq_len(dim(activeHazards)[3]), function(i) {
        return(list(
            intervalStarts = piecewiseSurvivalTime$piecewiseSurvivalTime,
            controlHazards = piecewiseSurvivalTime$lambdaControls,
            activeHazards = matrix(activeHazards[, , i],
                nrow = dim(activeHazards)[1], ncol = dim(activeHazards)[2])
        ))
    }))
}

