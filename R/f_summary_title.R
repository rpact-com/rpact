## |
## |  *Summary classes and functions*
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

#' @include f_core_utilities.R
#' @include f_core_assertions.R
NULL

.createSummaryTitleObject <- function(object) {
    design <- NULL
    designPlan <- NULL
    if (inherits(object, "TrialDesignCharacteristics")) {
        design <- object$.design
    } else if (.isTrialDesignPlan(object) || inherits(object, "SimulationResults")) {
        design <- object$.design
        designPlan <- object
    } else if (inherits(object, "AnalysisResults")) {
        return(.createSummaryTitleAnalysisResults(object$.design, object))
    } else if (.isTrialDesign(object)) {
        design <- object
    }
    if (!is.null(design)) {
        return(.createSummaryTitleDesign(design, designPlan))
    }
    return("")
}

.createSummaryTitleAnalysisResults <- function(design, analysisResults) {
    kMax <- design$kMax

    title <- ""
    if (kMax == 1) {
        title <- paste0(title, "Fixed sample analysis results")
    } else {
        title <- paste0(title, "Sequential analysis results with a maximum of ", kMax, " looks")
    }

    if (!is.null(analysisResults)) {
        if (.isMultiArmAnalysisResults(analysisResults)) {
            title <- "Multi-arm analysis results for a "
        } else if (.isEnrichmentAnalysisResults(analysisResults)) {
            title <- "Enrichment analysis results for a "
        } else {
            title <- "Analysis results for a "
        }

        if (analysisResults$.dataInput$isDatasetEstimates()) {
            title <- paste0(title, "general estimates")
        } else if (analysisResults$.dataInput$isDatasetMeans()) {
            title <- paste0(title, "continuous endpoint")
        } else if (analysisResults$.dataInput$isDatasetRates()) {
            title <- paste0(title, "binary endpoint")
        } else if (analysisResults$.dataInput$isDatasetSurvival()) {
            title <- paste0(title, "survival endpoint")
        }

        if (.isMultiHypothesesAnalysisResults(analysisResults)) {
            gMax <- analysisResults$.stageResults$getGMax()
            if (.isMultiArmAnalysisResults(analysisResults)) {
                title <- paste0(title, " (", gMax, " active arms vs. control)")
            } else if (.isEnrichmentAnalysisResults(analysisResults)) {
                title <- paste0(title, " (", gMax, " populations)")
            }
        }
    } else if (kMax > 1) {
        prefix <- ifelse(design$.isDelayedResponseDesign(), "delayed response ", "")
        title <- .concatenateSummaryText(title,
            paste0("(", prefix, design$.toString(startWithUpperCase = FALSE), ")"),
            sep = " "
        )
    }

    return(title)
}

.createSummaryTitleDesign <- function(design, designPlan) {
    kMax <- design$kMax

    title <- ""
    if (kMax == 1) {
        title <- paste0(title, "Fixed sample analysis")
    } else {
        title <- paste0(title, "Sequential analysis with a maximum of ", kMax, " looks")
    }
    if (!is.null(designPlan)) {
        if (inherits(designPlan, "SimulationResults")) {
            title <- "Simulation of a "
        } else if (designPlan$.isSampleSizeObject()) {
            title <- "Sample size calculation for a "
        } else if (designPlan$.isPowerObject()) {
            title <- "Power calculation for a "
        }

        if (grepl("Means", .getClassName(designPlan))) {
            title <- paste0(title, "continuous endpoint")
        } else if (grepl("Rates", .getClassName(designPlan))) {
            title <- paste0(title, "binary endpoint")
        } else if (grepl("Survival", .getClassName(designPlan))) {
            title <- paste0(title, "survival endpoint")
        } else if (grepl("CountData", .getClassName(designPlan))) {
            title <- paste0(title, "count data endpoint")
        }

        if (grepl("MultiArm", .getClassName(designPlan)) &&
                !is.null(designPlan[["activeArms"]]) && designPlan$activeArms > 1) {
            title <- .concatenateSummaryText(title, "(multi-arm design)", sep = " ")
        } else if (grepl("Enrichment", .getClassName(designPlan))) {
            title <- .concatenateSummaryText(title, "(enrichment design)", sep = " ")
        }
    } else if (kMax > 1) {
        prefix <- ifelse(design$.isDelayedResponseDesign(), "delayed response ", "")
        title <- .concatenateSummaryText(title,
            paste0("(", prefix, design$.toString(startWithUpperCase = FALSE), ")"),
            sep = " "
        )
    }

    return(title)
}
