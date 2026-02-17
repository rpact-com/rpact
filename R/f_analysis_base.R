## |
## |  *Analysis functions*
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
#' @include f_logger.R
#' @include f_object_r_code.R
NULL

.getDesignAndDataInput <- function(..., design, dataInput) {
    if (missing(design) && missing(dataInput)) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, sQuote("dataInput"), " must be specified")
    }

    if (missing(dataInput) && !missing(design) && inherits(design, "Dataset")) {
        dataInput <- design
        if (!is.null(dataInput$.design) && inherits(dataInput$.design, "TrialDesign")) {
            design <- dataInput$.design
        } else {
            design <- .getDefaultDesign(..., type = "analysis")
        }
    } else if (!missing(dataInput) && missing(design)) {
        if (!is.null(dataInput$.design) && inherits(dataInput$.design, "TrialDesign")) {
            design <- dataInput$.design
        } else {
            design <- .getDefaultDesign(..., type = "analysis")
        }
    } else {
        .warnInCaseOfTwoSidedPowerArgument(...)
    }

    .assertIsTrialDesign(design)
    .assertIsDataset(dataInput)
    design <- .resetPipeOperatorQueue(design)

    return(list(
        design = design,
        dataInput = dataInput$clone(deep = TRUE)
    ))
}

#' @title
#' Get Analysis Results
#'
#' @description
#' Calculates and returns the analysis results for the specified design and data.
#'
#' @inheritParams param_design
#' @inheritParams param_dataInput
#' @param ... Further arguments to be passed to methods (cf., separate functions in "See Also" below), e.g.,
#' \describe{
#'   	\item{\code{thetaH1} and \code{stDevH1} (or \code{assumedStDev} / \code{assumedStDevs}),
#' 		 \code{pi1}, \code{pi2}, or \code{piTreatments}, \code{piControl(s)}}{
#'       The assumed effect size, standard deviation or rates to calculate the conditional power if \code{nPlanned}
#' 		 is specified. For survival designs, \code{thetaH1} refers to the hazard ratio.
#' 		 For one-armed trials with binary outcome, only \code{pi1} can be specified, for two-armed trials with binary outcome,
#' 		 \code{pi1} and \code{pi2} can be specified referring to the assumed treatment and control rate, respectively.
#' 		 In multi-armed or enrichment designs, you can
#'   	 specify a value or a vector with elements referring to the treatment arms or the sub-populations,
#'   	 respectively. For testing rates, the parameters to be specified are \code{piTreatments} and \code{piControl}
#' 		 (multi-arm designs) and \code{piTreatments} and \code{piControls} (enrichment designs).\cr
#'   	 If not specified, the conditional power is calculated under the assumption of observed effect sizes,
#'   	 standard deviations, rates, or hazard ratios.}
#'   \item{\code{iterations}}{Iterations for simulating the power for Fisher's combination test.
#'       If the power for more than one remaining stages is to be determined for
#'       Fisher's combination test, it is estimated via simulation with specified \cr
#'       \code{iterations}, the default is \code{1000}.}
#'   \item{\code{seed}}{Seed for simulating the conditional power for Fisher's combination test.
#'       See above, default is a random seed.}
#'   \item{\code{normalApproximation}}{The type of computation of the p-values. Default is \code{FALSE} for
#'       testing means (i.e., the t test is used) and \code{TRUE} for testing rates and the hazard ratio.
#'       For testing rates, if \code{normalApproximation = FALSE} is specified, the binomial test
#'       (one sample) or the exact test of Fisher (two samples) is used for calculating the p-values.
#'       In the survival setting, \code{normalApproximation = FALSE} has no effect.}
#'   \item{\code{equalVariances}}{The type of t test. For testing means in two treatment groups, either
#'       the t test assuming that the variances are equal or the t test without assuming this,
#'       i.e., the test of Welch-Satterthwaite is calculated, default is \code{TRUE}.}
#'   \item{\code{stdErrorEstimate}}{Estimate of standard error for calculation of final confidence intervals for
#'       comparing rates in two treatment groups, default is \code{"pooled"}.}
#'   \item{\code{intersectionTest}}{Defines the multiple test for the intersection
#'       hypotheses in the closed system of hypotheses when testing multiple hypotheses.
#'       Five options are available in multi-arm designs: \code{"Dunnett"}, \code{"Bonferroni"}, \code{"Simes"},
#'       \code{"Sidak"}, and \code{"Hierarchical"}, default is \code{"Dunnett"}.
#'       Four options are available in population enrichment designs: \code{"SpiessensDebois"} (one subset only),
#'       \code{"Bonferroni"}, \code{"Simes"}, and \code{"Sidak"}, default is \code{"Simes"}.}
#'   \item{\code{varianceOption}}{Defines the way to calculate the variance in multiple treatment arms (> 2)
#'   	 or population enrichment designs for testing means. For multiple arms, three options are available:
#'       \code{"overallPooled"}, \code{"pairwisePooled"}, and \code{"notPooled"}, default is \code{"overallPooled"}.
#'       For enrichment designs, the options are: \code{"pooled"}, \code{"pooledFromFull"} (one subset only),
#'       and \code{"notPooled"}, default is \code{"pooled"}.}
#'   \item{\code{stratifiedAnalysis}}{For enrichment designs, typically a stratified analysis should be chosen.
#'       For testing means and rates, also a non-stratified analysis based on overall data can be performed.
#'       For survival data, only a stratified analysis is possible (see Brannath et al., 2009), default is \code{TRUE}.}
#' }
#' @inheritParams param_directionUpper
#' @inheritParams param_thetaH0
#' @inheritParams param_nPlanned
#' @inheritParams param_allocationRatioPlanned
#' @inheritParams param_stage
#' @inheritParams param_maxInformation
#' @inheritParams param_informationEpsilon
#'
#' @details
#' Given a design and a dataset, at given stage the function calculates the test results
#' (effect sizes, stage-wise test statistics and p-values, overall p-values and test statistics,
#' conditional rejection probability (CRP), conditional power, Repeated Confidence Intervals (RCIs),
#' repeated overall p-values, and final stage p-values, median unbiased effect estimates,
#' and final confidence intervals.
#'
#' For designs with more than two treatments arms (multi-arm designs) or enrichment designs
#' a closed combination test is performed.
#' That is, additionally the statistics to be used in a closed testing procedure are provided.
#'
#' The conditional power is calculated if the planned sample size for the subsequent stages (\code{nPlanned})
#' is specified. The conditional power is calculated either under the assumption of the observed effect or
#' under the assumption of an assumed effect, that has to be specified (see above).\cr
#' For testing rates in a two-armed trial, pi1 and pi2 typically refer to the rates in the treatment
#' and the control group, respectively. This is not mandatory, however, and so pi1 and pi2 can be interchanged.
#' In many-to-one multi-armed trials, piTreatments and piControl refer to the rates in the treatment arms and
#' the one control arm, and so they cannot be interchanged. piTreatments and piControls in enrichment designs
#' can principally be interchanged, but we use the plural form to indicate that the rates can be differently
#' specified for the sub-populations.
#'
#' Median unbiased effect estimates and confidence intervals are calculated if
#' a group sequential design or an inverse normal combination test design was chosen, i.e., it is not applicable
#' for Fisher's p-value combination test design.
#' For the inverse normal combination test design with more than two stages, a warning informs that the validity
#' of the confidence interval is theoretically shown only if no sample size change was performed.
#'
#' A final stage p-value for Fisher's combination test is calculated only if a two-stage design was chosen.
#' For Fisher's combination test, the conditional power for more than one remaining stages is estimated via simulation.
#'
#' Final stage p-values, median unbiased effect estimates, and final confidence intervals are not calculated
#' for multi-arm and enrichment designs.
#'
#' @return Returns an \code{\link{AnalysisResults}} object.
#' The following generics (R generic functions) are available for this result object:
#' \itemize{
#'   \item \code{\link[=names.AnalysisResults]{names}} to obtain the field names,
#'   \item \code{\link[=print.ParameterSet]{print()}} to print the object,
#'   \item \code{\link[=summary.AnalysisResults]{summary()}} to display a summary of the object,
#'   \item \code{\link[=plot.AnalysisResults]{plot()}} to plot the object,
#'   \item \code{\link[=as.data.frame.AnalysisResults]{as.data.frame()}} to coerce the object to a \code{\link[base]{data.frame}},
#'   \item \code{\link[=as.matrix.FieldSet]{as.matrix()}} to coerce the object to a \code{\link[base]{matrix}}.
#' }
#' @template how_to_get_help_for_generics
#'
#' @seealso
#' \code{\link[=getObservedInformationRates]{getObservedInformationRates()}}
#'
#' @family analysis functions
#'
#' @template examples_get_analysis_results
#'
#' @export
#'
getAnalysisResults <- function(design,
        dataInput,
        ...,
        directionUpper = NA, # C_DIRECTION_UPPER_DEFAULT
        thetaH0 = NA_real_,
        nPlanned = NA_real_,
        allocationRatioPlanned = 1, # C_ALLOCATION_RATIO_DEFAULT
        stage = NA_integer_,
        maxInformation = NULL,
        informationEpsilon = NULL) {
    on.exit(base::options("rpact.analyis.repeated.p.values.warnings.enabled" = "TRUE"))
    designAndDataInput <- .getDesignAndDataInput(design = design, dataInput = dataInput, ...)
    design <- designAndDataInput$design
    dataInput <- designAndDataInput$dataInput
    directionUpper <- .assertIsValidDirectionUpper(directionUpper, design,
        objectType = "analysis", userFunctionCallEnabled = TRUE
    )

    recalculationResult <- .getDesignWithRecalculatedBoundaries(
        design = design,
        dataInput = dataInput,
        directionUpper = directionUpper,
        thetaH0 = thetaH0,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        stage = stage,
        maxInformation = maxInformation,
        informationEpsilon = informationEpsilon,
        ...
    )
    design <- recalculationResult$design

    args <- list(
        design = design,
        dataInput = dataInput,
        directionUpper = directionUpper,
        thetaH0 = thetaH0,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned,
        stage = stage,
        ...
    )

    result <- NULL
    if (.isEnrichmentDataset(dataInput) || .isMultiArmDataset(dataInput)) {
        if (.isEnrichmentDataset(dataInput)) {
            fun <- .getAnalysisResultsEnrichment
        } else {
            fun <- .getAnalysisResultsMultiArm
        }
        result <- do.call(fun, args)
    } else {
        stage <- .getStageFromOptionalArguments(...,
            dataInput = dataInput,
            design = design,
            stage = stage,
            showWarnings = TRUE
        )
        directionUpper <- .assertIsValidDirectionUpper(directionUpper, design, objectType = "analysis")
        .assertIsValidDataInput(dataInput = dataInput, design = design, stage = stage)
        on.exit(dataInput$.trim())
        .assertIsValidThetaH0DataInput(thetaH0, dataInput)
        if (is.null(maxInformation) || is.na(maxInformation)) {
            .assertAreSuitableInformationRates(design, dataInput, stage = stage)
        }
        .assertIsValidNPlanned(nPlanned, design$kMax, stage, required = FALSE)
        .assertIsValidAllocationRatioPlanned(allocationRatioPlanned,
            numberOfGroups = dataInput$getNumberOfGroups()
        )

        fun <- NULL
        if (dataInput$isDatasetMeans()) {
            fun <- .getAnalysisResultsMeans
        } else if (dataInput$isDatasetRates()) {
            fun <- .getAnalysisResultsRates
        } else if (dataInput$isDatasetSurvival()) {
            fun <- .getAnalysisResultsSurvival
        } else {
            .fireDataInputNotSupportedException(dataInput)
        }
        thetaH0 <- .getDefaultThetaH0(dataInput, thetaH0)

        args$thetaH0 <- thetaH0
        result <- do.call(fun, args)

        if (is.null(result)) {
            .fireDataInputNotSupportedException(dataInput)
        }

        if (isTRUE(recalculationResult$informationRatesRecalculated)) {
            result$.setParameterType("maxInformation", C_PARAM_USER_DEFINED)
            if (!is.null(informationEpsilon) && !is.na(informationEpsilon)) {
                result$informationEpsilon <- informationEpsilon
                result$.setParameterType("informationEpsilon", C_PARAM_USER_DEFINED)
            }
        }
    }

    if (!is.null(result) && !is.null(recalculationResult$repeatedPValues)) {
        result$repeatedPValues <- recalculationResult$repeatedPValues
    }

    if (design$kMax > 1 && .isTrialDesignInverseNormalOrGroupSequential(design) &&
            design$typeOfDesign %in% c(C_TYPE_OF_DESIGN_AS_USER, C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY)) {
        indices <- design$userAlphaSpending == 0
        if (.isEnrichmentDataset(dataInput) || .isMultiArmDataset(dataInput)) {
            result$repeatedConfidenceIntervalLowerBounds[, indices] <- NA_real_
            result$repeatedConfidenceIntervalUpperBounds[, indices] <- NA_real_
            result$repeatedPValues[, indices] <- NA_real_
        } else {
            result$repeatedConfidenceIntervalLowerBounds[indices] <- NA_real_
            result$repeatedConfidenceIntervalUpperBounds[indices] <- NA_real_
            result$repeatedPValues[indices] <- NA_real_
        }
    }

    return(result)
}

#' @title
#' Get Stage Results
#'
#' @description
#' Returns summary statistics and p-values for a given data set and a given design.
#'
#' @inheritParams param_design
#' @inheritParams param_dataInput
#' @inheritParams param_stage
#' @inheritParams param_directionUpper
#' @param ... Further (optional) arguments to be passed:
#' \describe{
#'   \item{\code{thetaH0}}{The null hypothesis value,
#'       default is \code{0} for the normal and the binary case (testing means and rates, respectively),
#'       it is \code{1} for the survival case (testing the hazard ratio).\cr\cr
#'       For non-inferiority designs, \code{thetaH0} is the non-inferiority bound.
#'       That is, in case of (one-sided) testing of
#'       \itemize{
#'         \item \emph{means}: a value \code{!= 0}
#'             (or a value \code{!= 1} for testing the mean ratio) can be specified.
#'         \item \emph{rates}: a value \code{!= 0}
#'             (or a value \code{!= 1} for testing the risk ratio \code{pi1 / pi2}) can be specified.
#'         \item \emph{survival data}: a bound for testing H0:
#'             \code{hazard ratio = thetaH0 != 1} can be specified.
#'       }
#'       For testing a rate in one sample, a value \code{thetaH0} in (0, 1) has to be specified for
#'       defining the null hypothesis H0: \code{pi = thetaH0}.}
#'   \item{\code{normalApproximation}}{The
#'       type of computation of the p-values. Default is \code{FALSE} for
#'       testing means (i.e., the t test is used) and \code{TRUE} for testing rates and the hazard ratio.
#'       For testing rates, if \code{normalApproximation = FALSE} is specified, the binomial test
#'       (one sample) or the exact test of Fisher (two samples) is used for calculating the p-values.
#'       In the survival setting, \code{normalApproximation = FALSE} has no effect.}
#'   \item{\code{equalVariances}}{The type of t test. For testing means in two treatment groups, either
#'       the t test assuming that the variances are equal or the t test without assuming this,
#'       i.e., the test of Welch-Satterthwaite is calculated, default is \code{TRUE}.}
#'   \item{\code{intersectionTest}}{Defines the multiple test for the intersection
#'       hypotheses in the closed system of hypotheses when testing multiple hypotheses.
#'       Five options are available in multi-arm designs: \code{"Dunnett"}, \code{"Bonferroni"}, \code{"Simes"},
#'       \code{"Sidak"}, and \code{"Hierarchical"}, default is \code{"Dunnett"}.
#'       Four options are available in population enrichment designs: \code{"SpiessensDebois"} (one subset only),
#'       \code{"Bonferroni"}, \code{"Simes"}, and \code{"Sidak"}, default is \code{"Simes"}.}
#'   \item{\code{varianceOption}}{Defines the way to calculate the variance in multiple treatment arms (> 2)
#'   	 or population enrichment designs for testing means. For multiple arms, three options are available:
#'       \code{"overallPooled"}, \code{"pairwisePooled"}, and \code{"notPooled"}, default is \code{"overallPooled"}.
#'       For enrichment designs, the options are: \code{"pooled"}, \code{"pooledFromFull"} (one subset only),
#'       and \code{"notPooled"}, default is \code{"pooled"}.}
#'   \item{\code{stratifiedAnalysis}}{For enrichment designs, typically a stratified analysis should be chosen.
#'     For testing means and rates, also a non-stratified analysis based on overall data can be performed.
#'     For survival data, only a stratified analysis is possible (see Brannath et al., 2009), default is \code{TRUE}.}
#' }
#'
#' @details
#' Calculates and returns the stage results of the specified design and data input at the specified stage.
#'
#' @return Returns a \code{\link{StageResults}} object.
#' \itemize{
#'   \item \code{\link[=names.StageResults]{names}} to obtain the field names,
#'   \item \code{\link[=print.FieldSet]{print()}} to print the object,
#'   \item \code{\link[=summary.ParameterSet]{summary()}} to display a summary of the object,
#'   \item \code{\link[=plot.StageResults]{plot()}} to plot the object,
#'   \item \code{\link[=as.data.frame.StageResults]{as.data.frame()}} to coerce the object to a \code{\link[base]{data.frame}},
#'   \item \code{\link[=as.matrix.FieldSet]{as.matrix()}} to coerce the object to a \code{\link[base]{matrix}}.
#' }
#' @template how_to_get_help_for_generics
#'
#' @family analysis functions
#'
#' @template examples_get_stage_results
#'
#' @export
#'
getStageResults <- function(design,
        dataInput,
        ...,
        stage = NA_integer_,
        directionUpper = NA) {
    designAndDataInput <- .getDesignAndDataInput(design = design, dataInput = dataInput, ...)
    design <- designAndDataInput$design
    dataInput <- designAndDataInput$dataInput

    directionUpper <- .assertIsValidDirectionUpper(directionUpper, design,
        objectType = "analysis", userFunctionCallEnabled = TRUE
    )

    if (.isEnrichmentDataset(dataInput)) {
        return(.getStageResultsEnrichment(
            design = design,
            dataInput = dataInput,
            stage = stage,
            directionUpper = directionUpper,
            ...
        ))
    } else if (.isMultiArmDataset(dataInput)) {
        return(.getStageResultsMultiArm(
            design = design,
            dataInput = dataInput,
            stage = stage,
            directionUpper = directionUpper,
            ...
        ))
    } else {
        stage <- .getStageFromOptionalArguments(...,
            dataInput = dataInput, design = design, stage = stage
        )
        .assertIsValidDataInput(dataInput = dataInput, design = design, stage = stage)
        on.exit(dataInput$.trim())

        if (dataInput$isDatasetMeans()) {
            return(.getStageResultsMeans(
                design = design,
                dataInput = dataInput,
                stage = stage,
                directionUpper = directionUpper,
                userFunctionCallEnabled = TRUE,
                ...
            ))
        }

        if (dataInput$isDatasetRates()) {
            return(.getStageResultsRates(
                design = design,
                dataInput = dataInput,
                stage = stage,
                directionUpper = directionUpper,
                userFunctionCallEnabled = TRUE,
                ...
            ))
        }

        if (dataInput$isDatasetSurvival()) {
            return(.getStageResultsSurvival(
                design = design,
                dataInput = dataInput,
                stage = stage,
                directionUpper = directionUpper,
                userFunctionCallEnabled = TRUE,
                ...
            ))
        }
    }

    .fireDataInputNotSupportedException(dataInput)
}

.getStageFromOptionalArguments <- function(..., dataInput, design, showWarnings = FALSE) {
    .assertIsTrialDesign(design)

    stage <- .getOptionalArgument("stage", ...)
    if (!is.null(stage) && !is.na(stage)) {
        .assertIsValidStage(stage, design$kMax)
        if (showWarnings) {
            .assertIsDataset(dataInput)
            if (stage > dataInput$getNumberOfStages()) {
                warning("'stage' (", stage, ") will be ignored because 'dataInput' ",
                    "has only ", dataInput$getNumberOfStages(), " stages defined",
                    call. = FALSE
                )
            }
        }

        return(as.integer(stage))
    }

    .assertIsDataset(dataInput)
    stage <- dataInput$getNumberOfStages()
    stage <- min(stage, design$kMax)
    stage <- as.integer(stage)
    .assertIsValidStage(stage, design$kMax)
    return(stage)
}

#'
#' @title
#' Get Test Actions
#'
#' @description
#' Returns test actions.
#'
#' @inheritParams param_stageResults
#' @param ... Only available for backward compatibility.
#'
#' @details
#' Returns the test actions of the specified design and
#' stage results at the specified stage.
#'
#' @return Returns a \code{\link[base]{character}} vector of length \code{kMax}
#' Returns a \code{\link[base]{numeric}} vector of length \code{kMax}
#' containing the test actions of each stage.
#'
#' @family analysis functions
#'
#' @template examples_get_test_actions
#'
#' @export
#'
getTestActions <- function(stageResults, ...) {
    .warnInCaseOfUnknownArguments(functionName = "getTestActions", ...)

    stageResults <- .getStageResultsObject(stageResults, functionName = "getTestActions", ...)
    .assertIsStageResultsNonMultiHypotheses(stageResults)
    design <- stageResults$.design
    criticalValues <- .getCriticalValues(design)
    testActions <- rep(NA_character_, design$kMax)
    if (.isTrialDesignInverseNormal(design)) {
        for (k in 1:stageResults$stage) {
            if (design$sided == 1) {
                if (k < design$kMax) {
                    if (stageResults$combInverseNormal[k] > criticalValues[k]) {
                        testActions[k] <- "reject and stop"
                    } else if (stageResults$combInverseNormal[k] < design$futilityBounds[k]) {
                        testActions[k] <- "accept and stop"
                    } else {
                        testActions[k] <- "continue"
                    }
                } else {
                    if (stageResults$combInverseNormal[k] > criticalValues[k]) {
                        testActions[k] <- "reject"
                    } else {
                        testActions[k] <- "accept"
                    }
                }
            }
            if (design$sided == 2) {
                if (k < design$kMax) {
                    if (abs(stageResults$combInverseNormal[k]) > criticalValues[k]) {
                        testActions[k] <- "reject and stop"
                    } else {
                        testActions[k] <- "continue"
                    }
                } else {
                    if (abs(stageResults$combInverseNormal[k]) > criticalValues[k]) {
                        testActions[k] <- "reject"
                    } else {
                        testActions[k] <- "accept"
                    }
                }
            }
        }
    } else if (.isTrialDesignGroupSequential(design)) {
        for (k in 1:stageResults$stage) {
            if (design$sided == 1) {
                if (k < design$kMax) {
                    if (.getOneMinusQNorm(stageResults$overallPValues[k]) > criticalValues[k]) {
                        testActions[k] <- "reject and stop"
                    } else if (.getOneMinusQNorm(stageResults$overallPValues[k]) < design$futilityBounds[k]) {
                        testActions[k] <- "accept and stop"
                    } else {
                        testActions[k] <- "continue"
                    }
                } else {
                    if (.getOneMinusQNorm(stageResults$overallPValues[k]) > criticalValues[k]) {
                        testActions[k] <- "reject"
                    } else {
                        testActions[k] <- "accept"
                    }
                }
            }
            if (design$sided == 2) {
                if (k < design$kMax) {
                    if (abs(.getOneMinusQNorm(stageResults$overallPValues[k])) > criticalValues[k]) {
                        testActions[k] <- "reject and stop"
                    } else {
                        testActions[k] <- "continue"
                    }
                } else {
                    if (abs(.getOneMinusQNorm(stageResults$overallPValues[k])) > criticalValues[k]) {
                        testActions[k] <- "reject"
                    } else {
                        testActions[k] <- "accept"
                    }
                }
            }
        }
    } else if (.isTrialDesignFisher(design)) {
        for (k in 1:stageResults$stage) {
            if (design$sided == 1) {
                if (k < design$kMax) {
                    if (stageResults$combFisher[k] < criticalValues[k]) {
                        testActions[k] <- "reject and stop"
                    } else if (stageResults$pValues[k] > design$alpha0Vec[k]) {
                        testActions[k] <- "accept and stop"
                    } else {
                        testActions[k] <- "continue"
                    }
                } else {
                    if (stageResults$combFisher[k] < criticalValues[k]) {
                        testActions[k] <- "reject"
                    } else {
                        testActions[k] <- "accept"
                    }
                }
            }
            if (design$sided == 2) {
                if (k < design$kMax) {
                    if (min(stageResults$combFisher[k], 1 - stageResults$combFisher[k]) < criticalValues[k]) {
                        testActions[k] <- "reject and stop"
                    } else {
                        testActions[k] <- "continue"
                    }
                } else {
                    if (min(stageResults$combFisher[k], 1 - stageResults$combFisher[k]) < criticalValues[k]) {
                        testActions[k] <- "reject"
                    } else {
                        testActions[k] <- "accept"
                    }
                }
            }
        }
    }
    return(testActions)
}

#'
#' @title
#' Get Repeated Confidence Intervals
#'
#' @description
#' Calculates and returns the lower and upper limit of the repeated confidence intervals of the trial.
#'
#' @inheritParams param_design
#' @inheritParams param_dataInput
#' @inheritParams param_directionUpper
#' @inheritParams param_tolerance
#' @inheritParams param_stage
#' @param ... Further arguments to be passed to methods (cf., separate functions in "See Also" below), e.g.,
#' \describe{
#'   \item{\code{normalApproximation}}{The type of computation of the p-values. Default is \code{FALSE} for
#'       testing means (i.e., the t test is used) and \code{TRUE} for testing rates and the hazard ratio.
#'       For testing rates, if \code{normalApproximation = FALSE} is specified, the binomial test
#'       (one sample) or the exact test of Fisher (two samples) is used for calculating the p-values.
#'       In the survival setting, \code{normalApproximation = FALSE} has no effect.}
#'   \item{\code{equalVariances}}{The type of t test. For testing means in two treatment groups, either
#'       the t test assuming that the variances are equal or the t test without assuming this,
#'       i.e., the test of Welch-Satterthwaite is calculated, default is \code{TRUE}.}
#'   \item{\code{intersectionTest}}{Defines the multiple test for the intersection
#'       hypotheses in the closed system of hypotheses when testing multiple hypotheses.
#'       Five options are available in multi-arm designs: \code{"Dunnett"}, \code{"Bonferroni"}, \code{"Simes"},
#'       \code{"Sidak"}, and \code{"Hierarchical"}, default is \code{"Dunnett"}.
#'       Four options are available in population enrichment designs: \code{"SpiessensDebois"} (one subset only),
#'       \code{"Bonferroni"}, \code{"Simes"}, and \code{"Sidak"}, default is \code{"Simes"}.}
#'   \item{\code{varianceOption}}{Defines the way to calculate the variance in multiple treatment arms (> 2)
#'   	 or population enrichment designs for testing means. For multiple arms, three options are available:
#'       \code{"overallPooled"}, \code{"pairwisePooled"}, and \code{"notPooled"}, default is \code{"overallPooled"}.
#'       For enrichment designs, the options are: \code{"pooled"}, \code{"pooledFromFull"} (one subset only),
#'       and \code{"notPooled"}, default is \code{"pooled"}.}
#'   \item{\code{stratifiedAnalysis}}{For enrichment designs, typically a stratified analysis should be chosen.
#'     For testing means and rates, also a non-stratified analysis based on overall data can be performed.
#'     For survival data, only a stratified analysis is possible (see Brannath et al., 2009), default is \code{TRUE}.}
#' }
#'
#' @details
#' The repeated confidence interval at a given stage of the trial contains the
#' parameter values that are not rejected using the specified sequential design.
#' It can be calculated at each stage of the trial and can thus be used as a monitoring tool.
#'
#' The repeated confidence intervals are provided up to the specified stage.
#'
#' @return Returns a \code{\link[base]{matrix}} with \code{2} rows
#' and \code{kMax} columns containing the lower RCI limits in the first row and
#' the upper RCI limits in the second row, where each column represents a stage.
#'
#' @family analysis functions
#'
#' @template examples_get_repeated_confidence_intervals
#'
#' @export
#'
getRepeatedConfidenceIntervals <- function(design,
        dataInput,
        ...,
        directionUpper = NA, # C_DIRECTION_UPPER_DEFAULT
        tolerance = 1e-06, # C_ANALYSIS_TOLERANCE_DEFAULT
        stage = NA_integer_) {
    .assertIsValidTolerance(tolerance)

    directionUpper <- .assertIsValidDirectionUpper(directionUpper, design,
        objectType = "analysis", userFunctionCallEnabled = TRUE
    )

    designAndDataInput <- .getDesignAndDataInput(design = design, dataInput = dataInput, ...)
    design <- designAndDataInput$design
    dataInput <- designAndDataInput$dataInput

    if (.isEnrichmentDataset(dataInput)) {
        return(.getRepeatedConfidenceIntervalsEnrichment(
            design = design,
            dataInput = dataInput,
            stage = stage,
            directionUpper = directionUpper,
            ...
        ))
    }

    if (.isMultiArmDataset(dataInput)) {
        return(.getRepeatedConfidenceIntervalsMultiArm(
            design = design,
            dataInput = dataInput,
            stage = stage,
            directionUpper = directionUpper,
            ...
        ))
    }

    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design, stage = stage)
    .assertIsValidDataInput(dataInput = dataInput, design = design, stage = stage)
    on.exit(dataInput$.trim())

    if (dataInput$isDatasetMeans()) {
        return(.getRepeatedConfidenceIntervalsMeans(
            design = design,
            dataInput = dataInput,
            directionUpper = directionUpper,
            tolerance = tolerance,
            stage = stage,
            ...
        ))
    }

    if (dataInput$isDatasetRates()) {
        return(.getRepeatedConfidenceIntervalsRates(
            design = design,
            dataInput = dataInput,
            directionUpper = directionUpper,
            tolerance = tolerance,
            stage = stage, ...
        ))
    }

    if (dataInput$isDatasetSurvival()) {
        return(.getRepeatedConfidenceIntervalsSurvival(
            design = design,
            dataInput = dataInput,
            directionUpper = directionUpper,
            tolerance = tolerance,
            stage = stage, ...
        ))
    }

    .fireDataInputNotSupportedException(dataInput)
}

.getStageResultsObject <- function(stageResults, ..., functionName) {
    if (missing(stageResults)) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'stageResults' must be defined")
    }

    .stopInCaseOfIllegalStageDefinition(stageResults, ...)

    args <- list(...)
    if (.isTrialDesign(stageResults)) {
        if (length(args) == 0) {
            stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'stageResults' must be defined")
        }

        stageResults <- args[[1]]
        .logDebug(
            "The separate specification of the design in ", functionName, "() is deprecated ",
            "because the 'stageResults' object contains the design already"
        )
    }

    if (.isDataset(stageResults)) {
        stageResults <- getStageResults(dataInput = stageResults, ...)
    }

    if (!.isStageResults(stageResults)) {
        for (arg in args) {
            if (.isStageResults(arg)) {
                return(arg)
            }
        }
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'stageResults' must be defined")
    }

    return(stageResults)
}

#'
#' @title
#' Get Conditional Power
#'
#' @description
#' Calculates and returns the conditional power.
#'
#' @inheritParams param_stageResults
#' @inheritParams param_nPlanned
#' @inheritParams param_allocationRatioPlanned
#' @param ... Further (optional) arguments to be passed:
#' \describe{
#'   \item{\code{thetaH1} and \code{stDevH1} (or \code{assumedStDev} / \code{assumedStDevs}),
#' 		 \code{pi1}, \code{pi2}, or \code{piTreatments}, \code{piControl(s)}}{
#'       The assumed effect size, standard deviation or rates to calculate the conditional power if \code{nPlanned}
#' 		 is specified. For survival designs, \code{thetaH1} refers to the hazard ratio.
#' 		 For one-armed trials with binary outcome, only \code{pi1} can be specified, for two-armed trials with binary outcome,
#' 		 \code{pi1} and \code{pi2} can be specified referring to the assumed treatment and control rate, respectively.
#' 		 In multi-armed or enrichment designs, you can
#'   	 specify a value or a vector with elements referring to the treatment arms or the sub-populations,
#'   	 respectively. For testing rates, the parameters to be specified are \code{piTreatments} and \code{piControl} (multi-arm
#'   	 designs) and \code{piTreatments} and \code{piControls} (enrichment designs).\cr
#'   	 If not specified, the conditional power is calculated under the assumption of observed effect sizes,
#'   	 standard deviations, rates, or hazard ratios.}
#'   \item{\code{iterations}}{Iterations for simulating the power for Fisher's combination test.
#'       If the power for more than one remaining stages is to be determined for
#'       Fisher's combination test, it is estimated via simulation with specified \cr
#'       \code{iterations}, the default is \code{1000}.}
#'   \item{\code{seed}}{Seed for simulating the conditional power for Fisher's combination test.
#'       See above, default is a random seed.}
#' }
#'
#' @details
#' The conditional power is calculated if the planned sample size for the subsequent stages is specified.\cr
#' For testing rates in a two-armed trial, pi1 and pi2 typically refer to the rates in the treatment
#' and the control group, respectively. This is not mandatory, however, and so pi1 and pi2 can be interchanged.
#' In many-to-one multi-armed trials, piTreatments and piControl refer to the rates in the treatment arms and
#' the one control arm, and so they cannot be interchanged. piTreatments and piControls in enrichment designs
#' can principally be interchanged, but we use the plural form to indicate that the rates can be differently
#' specified for the sub-populations.
#'
#' For Fisher's combination test, the conditional power for more than one remaining stages is
#' estimated via simulation.
#'
#' @seealso
#' \code{\link[=plot.StageResults]{plot.StageResults()}} or \code{\link[=plot.AnalysisResults]{plot.AnalysisResults()}}
#' for plotting the conditional power.
#'
#' @return Returns a \code{\link{ConditionalPowerResults}} object.
#' The following generics (R generic functions) are available for this result object:
#' \itemize{
#'   \item \code{\link[=names.FieldSet]{names()}} to obtain the field names,
#'   \item \code{\link[=print.FieldSet]{print()}} to print the object,
#'   \item \code{\link[=summary.ParameterSet]{summary()}} to display a summary of the object,
#'   \item \code{\link[=plot.ParameterSet]{plot()}} to plot the object,
#'   \item \code{\link[=as.data.frame.ParameterSet]{as.data.frame()}} to coerce the object to a \code{\link[base]{data.frame}},
#'   \item \code{\link[=as.matrix.FieldSet]{as.matrix()}} to coerce the object to a \code{\link[base]{matrix}}.
#' }
#' @template how_to_get_help_for_generics
#'
#' @family analysis functions
#'
#' @template examples_get_conditional_power
#'
#' @export
#'
getConditionalPower <- function(stageResults,
        ...,
        nPlanned,
        allocationRatioPlanned = 1 # C_ALLOCATION_RATIO_DEFAULT
        ) {
    stageResults <- .getStageResultsObject(stageResults = stageResults, functionName = "getConditionalPower", ...)
    .assertIsValidAllocationRatioPlanned(allocationRatioPlanned, stageResults$.dataInput$getNumberOfGroups())

    conditionalPower <- NULL
    if (.isEnrichmentStageResults(stageResults)) {
        conditionalPower <- .getConditionalPowerEnrichment(
            stageResults = stageResults,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            ...
        )
    } else if (.isMultiArmStageResults(stageResults)) {
        conditionalPower <- .getConditionalPowerMultiArm(
            stageResults = stageResults,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            ...
        )
    } else {
        .assertIsStageResults(stageResults)
        if (stageResults$isDatasetMeans()) {
            conditionalPower <- .getConditionalPowerMeans(
                stageResults = stageResults,
                nPlanned = nPlanned,
                allocationRatioPlanned = allocationRatioPlanned,
                ...
            )
        } else if (stageResults$isDatasetRates()) {
            conditionalPower <- .getConditionalPowerRates(
                stageResults = stageResults,
                nPlanned = nPlanned,
                allocationRatioPlanned = allocationRatioPlanned,
                ...
            )
        } else if (stageResults$isDatasetSurvival()) {
            conditionalPower <- .getConditionalPowerSurvival(
                stageResults = stageResults,
                nPlanned = nPlanned,
                allocationRatioPlanned = allocationRatioPlanned,
                ...
            )
        }
    }
    if (!is.null(conditionalPower)) {
        addPlotData <- .getOptionalArgument("addPlotData", ...)
        if (!is.null(addPlotData) && isTRUE(addPlotData)) {
            conditionalPower$.plotData <- .getConditionalPowerPlot(
                stageResults = stageResults,
                nPlanned = nPlanned,
                allocationRatioPlanned = allocationRatioPlanned,
                ...
            )
        }

        conditionalPower$.setParameterType("nPlanned", C_PARAM_USER_DEFINED)
        conditionalPower$.setParameterType(
            "allocationRatioPlanned",
            ifelse(allocationRatioPlanned == C_ALLOCATION_RATIO_DEFAULT,
                C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
            )
        )

        return(conditionalPower)
    }

    .fireDataInputNotSupportedException(stageResults$getDataInput())
}

.getConditionalPowerPlot <- function(...,
        stageResults,
        nPlanned,
        allocationRatioPlanned = NA_real_) {
    if (.isMultiArmStageResults(stageResults)) {
        return(.getConditionalPowerPlotMultiArm(
            stageResults = stageResults,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, ...
        ))
    }

    if (.isEnrichmentStageResults(stageResults)) {
        return(.getConditionalPowerPlotEnrichment(
            stageResults = stageResults,
            nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, ...
        ))
    }

    .assertIsStageResults(stageResults)
    .stopInCaseOfIllegalStageDefinition2(...)

    stage <- stageResults$stage
    if (stage == stageResults$.design$kMax && length(nPlanned) > 0) {
        stage <- stageResults$.design$kMax - 1
    }

    .assertIsValidNPlanned(nPlanned, stageResults$.design$kMax, stage)
    if (is.na(allocationRatioPlanned)) {
        allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
    }

    if (stageResults$isDatasetMeans()) {
        return(.getConditionalPowerPlotMeans(
            stageResults = stageResults,
            stage = stage,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            ...
        ))
    }

    if (stageResults$isDatasetRates()) {
        return(.getConditionalPowerPlotRates(
            stageResults = stageResults,
            stage = stage,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            ...
        ))
    }

    if (stageResults$isDatasetSurvival()) {
        return(.getConditionalPowerPlotSurvival(
            stageResults = stageResults,
            stage = stage,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            ...
        ))
    }

    .fireDataInputNotSupportedException(stageResults$getDataInput())
}

#'
#' @title
#' Get Repeated P Values
#'
#' @description
#' Calculates the repeated p-values for a given test results.
#'
#' @inheritParams param_stageResults
#' @inheritParams param_tolerance
#' @inheritParams param_three_dots
#'
#' @details
#' The repeated p-value at a given stage of the trial is defined as the smallest
#' significance level under which at given test design the test results
#' obtain rejection of the null hypothesis. It can be calculated at each
#' stage of the trial and can thus be used as a monitoring tool.
#'
#' The repeated p-values are provided up to the specified stage.
#'
#' In multi-arm trials, the repeated p-values are defined separately for each
#' treatment comparison within the closed testing procedure.
#'
#' @return Returns a \code{\link[base]{numeric}} vector of length \code{kMax} or in case of multi-arm stage results
#' a \code{\link[base]{matrix}} (each column represents a stage, each row a comparison)
#' containing the repeated p values.
#'
#' @family analysis functions
#'
#' @template examples_get_repeated_p_values
#'
#' @export
#'
getRepeatedPValues <- function(stageResults, ...,
        tolerance = 1e-06 # C_ANALYSIS_TOLERANCE_DEFAULT
        ) {
    .assertIsValidTolerance(tolerance)
    .assertIsValidTolerance(tolerance)
    stageResults <- .getStageResultsObject(stageResults, functionName = "getRepeatedPValues", ...)

    if (.isEnrichmentStageResults(stageResults)) {
        return(.getRepeatedPValuesEnrichment(stageResults = stageResults, tolerance = tolerance, ...))
    }

    if (.isMultiArmStageResults(stageResults)) {
        return(.getRepeatedPValuesMultiArm(stageResults = stageResults, tolerance = tolerance, ...))
    }

    .assertIsStageResults(stageResults)
    design <- stageResults$.design

    if (design$kMax == 1) {
        return(ifelse(design$sided == 1, stageResults$pValues[1],
            2 * min(stageResults$pValues[1], 1 - stageResults$pValues[1])
        ))
    }

    if (.isTrialDesignInverseNormalOrGroupSequential(design) &&
            design$typeOfDesign %in% c(C_TYPE_OF_DESIGN_AS_USER, C_TYPE_OF_DESIGN_WT_OPTIMUM)) {
        showWarnings <- as.logical(getOption("rpact.analyis.repeated.p.values.warnings.enabled", "TRUE"))
        if (showWarnings) {
            warning("Repeated p-values not available for 'typeOfDesign' = '",
                design$typeOfDesign, "'",
                call. = FALSE
            )
        }
        return(rep(NA_real_, design$kMax))
    }

    if (.isTrialDesignInverseNormal(design)) {
        return(.getRepeatedPValuesInverseNormal(
            stageResults = stageResults, tolerance = tolerance, ...
        ))
    }

    if (.isTrialDesignGroupSequential(design)) {
        return(.getRepeatedPValuesGroupSequential(
            stageResults = stageResults, tolerance = tolerance, ...
        ))
    }

    if (.isTrialDesignFisher(design)) {
        if (design$method == C_FISHER_METHOD_USER_DEFINED_ALPHA) {
            warning("Repeated p-values not available for 'method' = '",
                C_FISHER_METHOD_USER_DEFINED_ALPHA, "'",
                call. = FALSE
            )
            return(rep(NA_real_, design$kMax))
        }

        return(.getRepeatedPValuesFisher(
            stageResults = stageResults, tolerance = tolerance, ...
        ))
    }

    .stopWithWrongDesignMessage(design, inclusiveConditionalDunnett = FALSE)
}

#
# Get final p-value based on inverse normal method
#
.getFinalPValueInverseNormalOrGroupSequential <- function(stageResults) {
    design <- stageResults$.design
    .assertIsTrialDesignInverseNormalOrGroupSequential(design)

    if (.isTrialDesignInverseNormal(design)) {
        stageInverseNormalOrGroupSequential <- .getStageInverseNormal(
            design = design,
            stageResults = stageResults, stage = stageResults$stage
        )
    } else {
        stageInverseNormalOrGroupSequential <- .getStageGroupSeq(
            design = design,
            stageResults = stageResults, stage = stageResults$stage
        )
    }
    finalStage <- min(stageInverseNormalOrGroupSequential, design$kMax)

    criticalValues <- .getCriticalValues(design)

    # Early stopping or at end of study
    if (stageInverseNormalOrGroupSequential < design$kMax || stageResults$stage == design$kMax) {
        if (stageInverseNormalOrGroupSequential == 1) {
            pFinal <- stageResults$pValues[1]
        } else {
            if (design$bindingFutility) {
                if (.isTrialDesignInverseNormal(design)) {
                    decisionMatrix <- matrix(
                        c(
                            design$futilityBounds[1:(finalStage - 1)], C_FUTILITY_BOUNDS_DEFAULT,
                            c(criticalValues[1:(finalStage - 1)], stageResults$combInverseNormal[finalStage])
                        ),
                        nrow = 2, byrow = TRUE
                    )
                } else {
                    decisionMatrix <- matrix(
                        c(
                            design$futilityBounds[1:(finalStage - 1)], C_FUTILITY_BOUNDS_DEFAULT,
                            c(criticalValues[1:(finalStage - 1)], .getOneMinusQNorm(stageResults$overallPValues[finalStage]))
                        ),
                        nrow = 2, byrow = TRUE
                    )
                }
            } else {
                if (.isTrialDesignInverseNormal(design)) {
                    decisionMatrix <- matrix(
                        c(
                            rep(C_FUTILITY_BOUNDS_DEFAULT, finalStage),
                            c(criticalValues[1:(finalStage - 1)], stageResults$combInverseNormal[finalStage])
                        ),
                        nrow = 2, byrow = TRUE
                    )
                } else {
                    decisionMatrix <- matrix(
                        c(
                            rep(C_FUTILITY_BOUNDS_DEFAULT, finalStage),
                            c(criticalValues[1:(finalStage - 1)], .getOneMinusQNorm(stageResults$overallPValues[finalStage]))
                        ),
                        nrow = 2, byrow = TRUE
                    )
                }
            }

            probs <- .getGroupSequentialProbabilities(
                decisionMatrix = decisionMatrix,
                informationRates = design$informationRates[1:finalStage]
            )
            pFinal <- sum(probs[3, ] - probs[2, ])

            if (design$sided == 2) {
                if (stageInverseNormalOrGroupSequential == 1) {
                    pFinalOtherDirection <- 1 - stageResults$pValues[1]
                } else {
                    if (.isTrialDesignInverseNormal(design)) {
                        decisionMatrix <- matrix(
                            c(
                                rep(C_FUTILITY_BOUNDS_DEFAULT, finalStage),
                                c(criticalValues[1:(finalStage - 1)], -stageResults$combInverseNormal[finalStage])
                            ),
                            nrow = 2, byrow = TRUE
                        )
                    } else {
                        decisionMatrix <- matrix(
                            c(
                                rep(C_FUTILITY_BOUNDS_DEFAULT, finalStage),
                                c(
                                    criticalValues[1:(finalStage - 1)],
                                    -.getOneMinusQNorm(stageResults$overallPValues[finalStage])
                                )
                            ),
                            nrow = 2, byrow = TRUE
                        )
                    }
                    probs <- .getGroupSequentialProbabilities(
                        decisionMatrix = decisionMatrix,
                        informationRates = design$informationRates[1:finalStage]
                    )

                    pFinalOtherDirection <- sum(probs[3, ] - probs[2, ])
                }

                pFinal <- 2 * min(pFinal, pFinalOtherDirection)
            }
        }

        return(list(finalStage = finalStage, pFinal = pFinal))
    }

    return(list(finalStage = NA_integer_, pFinal = NA_real_))
}

.setWeightsToStageResults <- function(design, stageResults) {
    if (.isTrialDesignInverseNormal(design)) {
        stageResults$weightsInverseNormal <- .getWeightsInverseNormal(design)
        stageResults$.setParameterType("weightsInverseNormal", C_PARAM_GENERATED)
    } else if (.isTrialDesignFisher(design)) {
        stageResults$weightsFisher <- .getWeightsFisher(design)
        stageResults$.setParameterType("weightsFisher", C_PARAM_GENERATED)
    }
}

#
# Returns the weights for inverse normal statistic
#
.getWeightsInverseNormal <- function(design) {
    if (design$kMax == 1) {
        return(1)
    }

    weights <- rep(NA, design$kMax)
    weights[1] <- sqrt(design$informationRates[1])
    weights[2:design$kMax] <- sqrt(design$informationRates[2:design$kMax] -
        design$informationRates[1:(design$kMax - 1)])
    return(weights)
}

#
# Returns the weights for Fisher's combination test statistic
#
.getWeightsFisher <- function(design) {
    if (design$kMax == 1) {
        return(1)
    }

    weights <- rep(NA, design$kMax)
    weights[1] <- 1
    weights[2:design$kMax] <- sqrt((design$informationRates[2:design$kMax] -
        design$informationRates[1:(design$kMax - 1)]) / design$informationRates[1])
    return(weights)
}

#
# Returns the stage when using the inverse normal combination test
#
.getStageInverseNormal <- function(..., design, stageResults, stage) {
    criticalValues <- .getCriticalValues(design)
    for (k in 1:stage) {
        if (stageResults$combInverseNormal[k] >= criticalValues[k]) {
            return(k)
        }
        if (design$sided == 2) {
            if (stageResults$combInverseNormal[k] <= -criticalValues[k]) {
                return(k)
            }
        }

        if (design$bindingFutility && k < design$kMax && stageResults$combInverseNormal[k] <=
                design$futilityBounds[k]) {
            return(k)
        }
    }

    # no early stopping
    return(as.integer(stage + design$kMax))
}

#
# Returns the stage when using the group sequential test
#
.getStageGroupSeq <- function(..., design, stageResults, stage) {
    criticalValues <- .getCriticalValues(design)
    for (k in 1:stage) {
        if (.getOneMinusQNorm(stageResults$overallPValues[k]) >= criticalValues[k]) {
            return(k)
        }

        if (design$sided == 2) {
            if (.getOneMinusQNorm(stageResults$overallPValues[k]) <= -criticalValues[k]) {
                return(k)
            }
        }

        if (design$bindingFutility && k < design$kMax &&
                .getQNorm(max(1e-8, 1 - stageResults$overallPValues[k])) <= design$futilityBounds[k]) {
            return(k)
        }
    }

    # no early stopping
    return(as.integer(stage + design$kMax))
}

#
# Returns the stage when using Fisher's combination test
#
.getStageFisher <- function(..., design, stageResults, stage) {
    for (k in 1:stage) {
        if (stageResults$combFisher[k] <= design$criticalValues[k]) {
            return(k)
        }
        if (design$sided == 2) {
            if (1 - stageResults$combFisher[k] <= design$criticalValues[k]) {
                return(k)
            }
        }

        if (design$bindingFutility && k < design$kMax && stageResults$pValues[k] >= design$alpha0Vec[k]) {
            return(k)
        }
    }

    # no early stopping
    return(as.integer(stage + design$kMax))
}

# @title
# q function
#
# @description
# Function for calculating the final p-value for two-stage design with Fisher's combination test
# and its use for calculating confidence intervals, see Wassmer & Brannath, p. 192 and Brannath et al. (2002), p. 241.
# Formula generalized for arbitrary weight in combination test.
#
.getQFunctionResult <- function(..., design, stageResults, theta, infRate) {
    alpha1 <- design$criticalValues[1]
    alpha0 <- design$alpha0Vec[1]
    if (!design$bindingFutility || (design$sided == 2)) {
        alpha0 <- 1
    }
    weightForFisher <- stageResults$weightsFisher[2]

    if (theta != 0) {
        alpha1Adj <- ifelse(alpha1 <= 0, 0,
            1 - stats::pnorm(.getOneMinusQNorm(alpha1) - theta / stageResults$overallStDevs[1] * infRate[1])
        )
    } else {
        alpha1Adj <- alpha1
    }

    if (is.na(alpha1Adj)) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "failed to calculate 'alpha1Adj'")
    }

    if (theta != 0) {
        alpha0Adj <- ifelse(alpha0 >= 1, 1,
            1 - stats::pnorm(.getOneMinusQNorm(alpha0) - theta / stageResults$overallStDevs[1] * infRate[1])
        )
    } else {
        alpha0Adj <- alpha0
    }

    if (is.na(alpha0Adj)) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "failed to calculate 'alpha0Adj'")
    }

    if (stageResults$pValues[1] <= alpha1Adj || stageResults$pValues[1] >= alpha0Adj) {
        return(stageResults$pValues[1])
    }

    if (weightForFisher == 1) {
        return(max(alpha1Adj, stageResults$pValues[1] * stageResults$pValues[2]) + stageResults$pValues[1] *
            stageResults$pValues[2] * (log(alpha0Adj) - log(max(
                alpha1Adj,
                stageResults$pValues[1] * stageResults$pValues[2]
            ))))
    }

    return(max(alpha1Adj, stageResults$pValues[1] * stageResults$pValues[2]^weightForFisher) +
        weightForFisher / (weightForFisher - 1) * stageResults$pValues[1]^(1 / weightForFisher) *
            stageResults$pValues[2] * (alpha0Adj^(1 - 1 / weightForFisher) -
                max(alpha1Adj, stageResults$pValues[1] * stageResults$pValues[2]^weightForFisher)^(1 - 1 /
                    weightForFisher)))
}

#
# Get final p-value based on Fisher combination test
#
.getFinalPValueFisher <- function(stageResults) {
    design <- stageResults$.design
    .assertIsTrialDesignFisher(design)
    stageFisher <- .getStageFisher(design = design, stageResults = stageResults, stage = stageResults$stage)
    finalStage <- min(stageFisher, design$kMax)

    # Early stopping or at end of study
    if (stageFisher < design$kMax || stageResults$stage == design$kMax) {
        if (stageFisher == 1) {
            pFinal <- stageResults$pValues[1]
        } else {
            if (design$kMax > 2) {
                message(
                    "Final p-value cannot be calculated for kMax = ", design$kMax, " ",
                    "because the function for Fisher's design is implemented only for kMax <= 2"
                )
                return(list(finalStage = NA_integer_, pFinal = NA_real_))
            }

            # Final p-value for kMax = 2
            pFinal <- .getQFunctionResult(
                design = design, stageResults = stageResults,
                theta = 0, infRate = 0
            )
        }

        if (design$sided == 2) {
            if (stageFisher == 1) {
                pFinalOtherDirection <- 1 - stageResults$pValues[1]
            } else {
                stageResults$pValues <- 1 - stageResults$pValues
                pFinalOtherDirection <- .getQFunctionResult(
                    design = design, stageResults = stageResults,
                    theta = 0, infRate = 0
                )
                stageResults$pValues <- 1 - stageResults$pValues
            }

            # Final p-value for kMax = 2
            pFinal <- 2 * min(pFinal, pFinalOtherDirection)
        }

        return(list(finalStage = finalStage, pFinal = pFinal))
    }

    return(list(finalStage = NA_integer_, pFinal = NA_real_))
}

#'
#' @title
#' Get Final P Value
#'
#' @description
#' Returns the final p-value for given stage results.
#'
#' @inheritParams param_stageResults
#' @param ... Only available for backward compatibility.
#'
#' @return Returns a \code{\link[base]{list}} containing
#' \itemize{
#'   \item \code{finalStage},
#'   \item \code{pFinal}.
#' }
#'
#' @details
#' The calculation of the final p-value is based on the stage-wise ordering of the sample space.
#' This enables the calculation for both the non-adaptive and the adaptive case.
#' For Fisher's combination test, it is available for \code{kMax = 2} only.
#'
#' @family analysis functions
#'
#' @template examples_get_final_p_value
#'
#' @export
#'
getFinalPValue <- function(stageResults, ...) {
    stageResults <- .getStageResultsObject(stageResults, functionName = "getFinalPValue", ...)

    .assertIsStageResultsNonMultiHypotheses(stageResults)

    if (stageResults$.design$kMax == 1) {
        warning("Final p-value is not available for fixed designs", call. = FALSE)
        return(list(finalStage = NA_integer_, pFinal = NA_real_))
    }

    finalPValue <- NULL
    if (.isTrialDesignInverseNormalOrGroupSequential(stageResults$.design)) {
        finalPValue <- .getFinalPValueInverseNormalOrGroupSequential(stageResults)
    } else if (.isTrialDesignFisher(stageResults$.design)) {
        finalPValue <- .getFinalPValueFisher(stageResults)
    }

    if (is.null(finalPValue)) {
        .stopWithWrongDesignMessage(stageResults$.design,
            inclusiveConditionalDunnett = .isMultiArmStageResults(stageResults)
        )
    }

    if (stageResults$.design$kMax > 1 && is.na(finalPValue$finalStage) &&
            (length(finalPValue$pFinal) == 0 || all(is.na(finalPValue$pFinal)))) {
        if (.getOptionalArgument("showWarnings", optionalArgumentDefaultValue = TRUE, ...)) {
            warning("Final p-value not calculated because final stage not reached", call. = FALSE)
        }
    }

    return(finalPValue)
}

.getVectorWithFinalValueAtFinalStage <- function(..., kMax, finalValue, finalStage) {
    v <- rep(NA_real_, kMax)
    if (is.null(finalValue) || is.na(finalValue) ||
            is.null(finalStage) || is.na(finalStage) ||
            finalStage < 1 || finalStage > kMax) {
        return(v)
    }

    v[finalStage] <- finalValue
    return(v)
}

#' @title
#' Get Final Confidence Interval
#'
#' @description
#' Returns the final confidence interval for the parameter of interest.
#' It is based on the prototype case, i.e., the test for testing a mean for
#' normally distributed variables.
#'
#' @inheritParams param_design
#' @inheritParams param_dataInput
#' @param ... Further (optional) arguments to be passed:
#' \describe{
#'   \item{\code{normalApproximation}}{
#'       The type of computation of the p-values. Default is \code{FALSE} for
#'       testing means (i.e., the t test is used) and TRUE for testing rates and the hazard ratio.
#'       For testing rates, if \code{normalApproximation = FALSE} is specified, the binomial test
#'       (one sample) or the exact test of Fisher (two samples) is used for calculating the p-values.
#'       In the survival setting, \code{normalApproximation = FALSE} has no effect.}
#'   \item{\code{equalVariances}}{The type of t test. For testing means in two treatment groups, either
#'       the t test assuming that the variances are equal or the t test without assuming this,
#'       i.e., the test of Welch-Satterthwaite is calculated, default is \code{TRUE}.}
#'   \item{\code{stdErrorEstimate}}{Estimate of standard error for calculation of final confidence intervals for
#'       comparing rates in two treatment groups, default is \code{"pooled"}.}
#' }
#' @inheritParams param_thetaH0
#' @inheritParams param_directionUpper
#' @inheritParams param_tolerance
#' @inheritParams param_stage
#'
#' @details
#' Depending on \code{design} and \code{dataInput} the final confidence interval and median unbiased estimate
#' that is based on the stage-wise ordering of the sample space will be calculated and returned.
#' Additionally, a non-standardized ("general") version is provided,
#' the estimated standard deviation must be used to obtain
#' the confidence interval for the parameter of interest.
#'
#' For the inverse normal combination test design with more than two
#' stages, a warning informs that the validity of the confidence interval is theoretically shown only if
#' no sample size change was performed.
#'
#' @return Returns a \code{\link[base]{list}} containing
#' \itemize{
#'   \item \code{finalStage},
#'   \item \code{medianUnbiased},
#'   \item \code{finalConfidenceInterval},
#'   \item \code{medianUnbiasedGeneral}, and
#'   \item \code{finalConfidenceIntervalGeneral}.
#' }
#'
#' @family analysis functions
#'
#' @template examples_get_final_confidence_interval
#'
#' @export
#'
getFinalConfidenceInterval <- function(design,
        dataInput,
        ...,
        directionUpper = NA, # C_DIRECTION_UPPER_DEFAULT
        thetaH0 = NA_real_,
        tolerance = 1e-06, # C_ANALYSIS_TOLERANCE_DEFAULT
        stage = NA_integer_) {
    .assertIsValidTolerance(tolerance)

    designAndDataInput <- .getDesignAndDataInput(design = design, dataInput = dataInput, ...)
    design <- designAndDataInput$design
    dataInput <- designAndDataInput$dataInput

    stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design, stage = stage)
    .assertIsValidDataInput(dataInput = dataInput, design = design, stage = stage)

    .assertIsDatasetNonMultiHypotheses(dataInput)
    on.exit(dataInput$.trim())

    if (design$kMax == 1) {
        warning("Final confidence interval is not available for fixed sample designs", call. = FALSE)
    }

    if (design$kMax > 1 && design$bindingFutility && !.isTrialDesignFisher(design)) {
        warning("Two-sided final confidence bounds are not appropriate for binding futility case, ",
            "use one-sided version (i.e., one bound) only",
            call. = FALSE
        )
    }

    directionUpper <- .assertIsValidDirectionUpper(directionUpper, design,
        objectType = "analysis", userFunctionCallEnabled = TRUE
    )

    finalConfidenceInterval <- NULL
    if (dataInput$isDatasetMeans()) {
        finalConfidenceInterval <- .getFinalConfidenceIntervalMeans(
            design = design,
            dataInput = dataInput,
            directionUpper = directionUpper,
            thetaH0 = thetaH0,
            tolerance = tolerance,
            stage = stage,
            ...
        )
    } else if (dataInput$isDatasetRates()) {
        stdErrorEstimate <- .getOptionalArgument("stdErrorEstimate", optionalArgumentDefaultValue = NA_character_, ...)
        if (!.isTrialDesignFisher(design)) {
            stdErrorEstimate <- .assertIsValidStdErrorEstimateRates(stdErrorEstimate, dataInput)
        }

        normalApproximation <- .getOptionalArgument("normalApproximation",
            optionalArgumentDefaultValue = C_NORMAL_APPROXIMATION_RATES_DEFAULT, ...
        )

        finalConfidenceInterval <- .getFinalConfidenceIntervalRates(
            design = design,
            dataInput = dataInput,
            directionUpper = directionUpper,
            thetaH0 = thetaH0,
            tolerance = tolerance,
            stage = stage,
            stdErrorEstimate = stdErrorEstimate,
            normalApproximation = normalApproximation
        )
    } else if (dataInput$isDatasetSurvival()) {
        finalConfidenceInterval <- .getFinalConfidenceIntervalSurvival(
            design = design,
            dataInput = dataInput,
            directionUpper = directionUpper,
            thetaH0 = thetaH0,
            tolerance = tolerance,
            stage = stage
        )
    }

    if (is.null(finalConfidenceInterval)) {
        .fireDataInputNotSupportedException(dataInput)
    }

    if (design$kMax > 1 && is.na(finalConfidenceInterval$finalStage) &&
            (length(finalConfidenceInterval$finalConfidenceInterval) == 0 ||
                all(is.na(finalConfidenceInterval$finalConfidenceInterval)))) {
        warning("Final confidence interval not calculated because final stage not reached", call. = FALSE)
    }

    return(finalConfidenceInterval)
}

#
# Get repeated p-values based on group sequential test
#
.getRepeatedPValuesGroupSequential <- function(..., stageResults, tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(functionName = ".getRepeatedPValuesGroupSequential", ...)

    design <- stageResults$.design

    .assertIsTrialDesignInverseNormalOrGroupSequential(design)

    repeatedPValues <- rep(NA_real_, design$kMax)
    if (design$typeOfDesign == C_TYPE_OF_DESIGN_HP && stageResults$stage == design$kMax) {
        if (!is.na(stageResults$overallPValues[design$kMax]) &&
                .getOneMinusQNorm(stageResults$overallPValues[design$kMax]) == Inf) {
            repeatedPValues[design$kMax] <- tolerance
        } else {
            startTime <- Sys.time()
            lower <- .getDesignGroupSequential(
                kMax = design$kMax,
                sided = design$sided,
                informationRates = design$informationRates,
                typeOfDesign = C_TYPE_OF_DESIGN_HP,
                futilityBounds = design$futilityBounds,
                bindingFutility = design$bindingFutility
            )$alphaSpent[design$kMax - 1] + tolerance
            upper <- 0.5
            repeatedPValues[design$kMax] <- .getOneDimensionalRootBisectionMethod(
                fun = function(level) {
                    y <- .getDesignGroupSequential(
                        kMax = design$kMax, alpha = level,
                        sided = design$sided,
                        informationRates = design$informationRates,
                        typeOfDesign = C_TYPE_OF_DESIGN_HP,
                        futilityBounds = design$futilityBounds,
                        bindingFutility = design$bindingFutility
                    )
                    if (design$sided == 2) {
                        return(y$criticalValues[design$kMax] -
                            abs(.getOneMinusQNorm(stageResults$overallPValues[design$kMax])))
                    }

                    return(y$criticalValues[design$kMax] -
                        .getOneMinusQNorm(stageResults$overallPValues[design$kMax]))
                }, lower = lower, upper = upper,
                tolerance = tolerance, direction = -1,
                acceptResultsOutOfTolerance = TRUE, suppressWarnings = TRUE,
                callingFunctionInformation = ".getRepeatedPValuesGroupSequential"
            )
            .logProgress("Repeated p-values for final stage calculated", startTime = startTime)
        }
    } else {
        typeOfDesign <- design$typeOfDesign
        deltaWT <- design$deltaWT
        typeBetaSpending <- design$typeBetaSpending

        if (!design$bindingFutility) {
            if (design$typeOfDesign == C_TYPE_OF_DESIGN_PT) {
                typeOfDesign <- C_TYPE_OF_DESIGN_WT
                deltaWT <- design$deltaPT1
            }
            if (design$typeBetaSpending != "none") {
                typeBetaSpending <- "none"
            }
        } else if ((design$typeOfDesign == C_TYPE_OF_DESIGN_PT) || (design$typeBetaSpending != "none")) {
            message("Calculation of repeated p-values might take a while for binding case, please wait...")
        }

        for (k in 1:stageResults$stage) {
            if (!is.na(stageResults$overallPValues[k]) && .getOneMinusQNorm(stageResults$overallPValues[k]) == Inf) {
                repeatedPValues[k] <- tolerance
            } else {
                startTime <- Sys.time()
                upper <- 0.5
                repeatedPValues[k] <- .getOneDimensionalRootBisectionMethod(
                    fun = function(level) {
                        y <- .getDesignGroupSequential(
                            kMax = design$kMax, alpha = level,
                            sided = design$sided,
                            informationRates = design$informationRates,
                            typeOfDesign = typeOfDesign,
                            typeBetaSpending = typeBetaSpending,
                            gammaB = design$gammaB,
                            deltaWT = deltaWT,
                            deltaPT0 = design$deltaPT0,
                            deltaPT1 = design$deltaPT1,
                            beta = design$beta,
                            gammaA = design$gammaA,
                            futilityBounds = design$futilityBounds,
                            bindingFutility = design$bindingFutility
                        )
                        if (design$sided == 2) {
                            return(y$criticalValues[k] - abs(.getOneMinusQNorm(stageResults$overallPValues[k])))
                        }

                        return(y$criticalValues[k] - .getOneMinusQNorm(stageResults$overallPValues[k]))
                    }, lower = tolerance, upper = upper,
                    tolerance = tolerance, direction = -1,
                    acceptResultsOutOfTolerance = TRUE, suppressWarnings = TRUE,
                    callingFunctionInformation = ".getRepeatedPValuesGroupSequential"
                )
                .logProgress("Repeated p-values of stage %s calculated", startTime = startTime, k)
            }
        }
    }

    return(repeatedPValues)
}

#
# Get repeated p-values based on inverse normal method
#
.getRepeatedPValuesInverseNormal <- function(..., stageResults, tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    design <- stageResults$.design
    .assertIsTrialDesignInverseNormalOrGroupSequential(design)
    .warnInCaseOfUnknownArguments(functionName = ".getRepeatedPValuesInverseNormal", ...)

    repeatedPValues <- rep(NA_real_, design$kMax)

    if (design$typeOfDesign == C_TYPE_OF_DESIGN_HP && stageResults$stage == design$kMax) {
        if (!is.na(stageResults$combInverseNormal[design$kMax]) &&
                stageResults$combInverseNormal[design$kMax] == Inf) {
            repeatedPValues[design$kMax] <- tolerance
        } else {
            startTime <- Sys.time()
            lower <- .getDesignGroupSequential(
                kMax = design$kMax,
                sided = design$sided,
                informationRates = design$informationRates,
                typeOfDesign = C_TYPE_OF_DESIGN_HP,
                futilityBounds = design$futilityBounds,
                bindingFutility = design$bindingFutility
            )$alphaSpent[design$kMax - 1] + tolerance
            upper <- 0.5
            repeatedPValues[design$kMax] <- .getOneDimensionalRootBisectionMethod(
                fun = function(level) {
                    y <- .getDesignGroupSequential(
                        kMax = design$kMax,
                        alpha = level,
                        sided = design$sided,
                        informationRates = design$informationRates,
                        typeOfDesign = C_TYPE_OF_DESIGN_HP,
                        futilityBounds = design$futilityBounds,
                        bindingFutility = design$bindingFutility
                    )
                    if (design$sided == 2) {
                        return(y$criticalValues[design$kMax] -
                            abs(stageResults$combInverseNormal[design$kMax]))
                    }

                    return(y$criticalValues[design$kMax] - stageResults$combInverseNormal[design$kMax])
                }, lower = lower, upper = upper,
                tolerance = tolerance, direction = -1,
                acceptResultsOutOfTolerance = TRUE, suppressWarnings = TRUE,
                callingFunctionInformation = ".getRepeatedPValuesInverseNormal"
            )
            .logProgress("Repeated p-values for final stage calculated", startTime = startTime)
        }
    } else {
        typeOfDesign <- design$typeOfDesign
        deltaWT <- design$deltaWT
        typeBetaSpending <- design$typeBetaSpending

        if (!design$bindingFutility) {
            if (design$typeOfDesign == C_TYPE_OF_DESIGN_PT) {
                typeOfDesign <- C_TYPE_OF_DESIGN_WT
                deltaWT <- design$deltaPT1
            }
            if (design$typeBetaSpending != "none") {
                typeBetaSpending <- "none"
            }
        } else if ((design$typeOfDesign == C_TYPE_OF_DESIGN_PT) || (design$typeBetaSpending != "none")) {
            message("Calculation of repeated p-values might take a while for binding case, please wait...")
        }

        for (k in 1:stageResults$stage) {
            if (!is.na(stageResults$combInverseNormal[k]) && (stageResults$combInverseNormal[k] == Inf)) {
                repeatedPValues[k] <- tolerance
            } else {
                startTime <- Sys.time()
                upper <- 0.5
                repeatedPValues[k] <- .getOneDimensionalRootBisectionMethod(
                    fun = function(level) {
                        y <- .getDesignGroupSequential(
                            kMax = design$kMax,
                            alpha = level,
                            sided = design$sided,
                            informationRates = design$informationRates,
                            typeOfDesign = typeOfDesign,
                            typeBetaSpending = typeBetaSpending,
                            gammaB = design$gammaB,
                            deltaWT = deltaWT,
                            deltaPT0 = design$deltaPT0,
                            deltaPT1 = design$deltaPT1,
                            beta = design$beta,
                            gammaA = design$gammaA,
                            futilityBounds = design$futilityBounds,
                            bindingFutility = design$bindingFutility
                        )
                        if (design$sided == 2) {
                            return(y$criticalValues[k] - abs(stageResults$combInverseNormal[k]))
                        }

                        return(y$criticalValues[k] - stageResults$combInverseNormal[k])
                    }, lower = tolerance, upper = upper,
                    tolerance = tolerance, direction = -1,
                    acceptResultsOutOfTolerance = TRUE, suppressWarnings = TRUE,
                    callingFunctionInformation = ".getRepeatedPValuesInverseNormal"
                )
                .logProgress("Repeated p-values of stage %s calculated", startTime = startTime, k)
            }
        }
    }

    return(repeatedPValues)
}

#
# Get repeated p-values based on Fisher combination test
#
.getRepeatedPValuesFisher <- function(..., stageResults, tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
    .warnInCaseOfUnknownArguments(functionName = ".getRepeatedPValuesFisher", ...)

    design <- stageResults$.design
    .assertIsTrialDesignFisher(design)

    repeatedPValues <- rep(NA_real_, design$kMax)
    for (k in 1:stageResults$stage) {
        if (!is.na(stageResults$combFisher[k]) && (stageResults$combFisher[k] == 0)) {
            repeatedPValues[k] <- tolerance
        } else {
            startTime <- Sys.time()
            repeatedPValues[k] <- .getOneDimensionalRootBisectionMethod(
                fun = function(level) {
                    y <- .getDesignFisher(
                        kMax = design$kMax,
                        alpha = level,
                        sided = design$sided,
                        informationRates = design$informationRates,
                        alpha0Vec = design$alpha0Vec,
                        bindingFutility = design$bindingFutility,
                        method = design$method
                    )
                    if (design$sided == 2) {
                        combFisherNegStagek <- prod((1 -
                            stageResults$pValues[1:k])^stageResults$weightsFisher[1:k])
                        return(y$criticalValues[k] - min(stageResults$combFisher[k], combFisherNegStagek))
                    }
                    return(y$criticalValues[k] - stageResults$combFisher[k])
                },
                lower = tolerance, upper = 0.5, tolerance = tolerance, direction = 1,
                acceptResultsOutOfTolerance = TRUE, suppressWarnings = TRUE,
                callingFunctionInformation = ".getRepeatedPValuesFisher"
            )
            .logProgress("Repeated p-values of stage %s calculated", startTime = startTime, k)
        }
    }

    return(repeatedPValues)
}

.getRejectValueConditionalPowerFisher <- function(..., kMax, alpha0Vec,
        criticalValues, weightsFisher, pValues, currentKMax, thetaH1, stage, nPlanned) {
    pValues <- c(pValues[1:stage], 1 - stats::pnorm(stats::rnorm(
        kMax - stage,
        thetaH1 * sqrt(nPlanned[(stage + 1):currentKMax])
    )))
    for (j in 1:currentKMax) {
        reject <- .getRejectValueFisherForOneStage(
            kMax = currentKMax,
            alpha0Vec = alpha0Vec, criticalValues = criticalValues,
            weightsFisher = weightsFisher, stage = j, pValues = pValues
        )
        if (reject >= 0) {
            return(reject)
        }
    }
    return(0)
}

.getRejectValueFisherForOneStage <- function(..., kMax, alpha0Vec, criticalValues, weightsFisher, stage, pValues) {
    if (stage < kMax && pValues[stage] >= alpha0Vec[stage]) {
        return(0)
    }

    p <- prod(pValues[1:stage]^weightsFisher[1:stage])
    if (is.na(p)) {
        stop(
            C_EXCEPTION_TYPE_RUNTIME_ISSUE, "calculation of 'p' failed for stage ", stage,
            " ('pValues' = ", .arrayToString(pValues), ", 'weightsFisher' = ", .arrayToString(weightsFisher), ")"
        )
    }
    if (is.na(criticalValues[stage])) {
        stop(
            C_EXCEPTION_TYPE_RUNTIME_ISSUE, "no critical value available for stage ", stage,
            " ('criticalValues' = ", .arrayToString(criticalValues), ")"
        )
    }

    if (p < criticalValues[stage]) {
        return(1)
    }

    return(-1)
}

.getRejectValueCrpFisher <- function(..., kMax, alpha0Vec, criticalValues, weightsFisher, k, stageResults) {
    pValues <- c(stageResults$pValues[1:k], stats::runif(kMax - k))
    for (stage in 1:kMax) {
        reject <- .getRejectValueFisherForOneStage(
            kMax = kMax, alpha0Vec = alpha0Vec,
            criticalValues = criticalValues, weightsFisher = weightsFisher, stage = stage,
            pValues = pValues
        )
        if (reject >= 0) {
            return(reject)
        }
    }
    return(0)
}

#
# Get CRP based on inverse normal or group sequential method
#
.getConditionalRejectionProbabilitiesInverseNormalorGroupSequential <- function(..., stageResults) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getConditionalRejectionProbabilitiesInverseNormalorGroupSequential",
        ignore = c("design"), ...
    )

    design <- stageResults$.design
    .assertIsTrialDesignInverseNormalOrGroupSequential(design)

    criticalValues <- .getCriticalValues(design)
    informationRates <- design$informationRates
    weights <- stageResults$weightsInverseNormal
    futilityBounds <- design$futilityBounds

    kMax <- design$kMax
    conditionalRejectionProbabilities <- rep(NA_real_, kMax)
    if (kMax == 1) {
        return(NA_real_)
    }

    for (k in 1:min(kMax - 1, stageResults$stage)) {
        if (.isTrialDesignInverseNormal(design)) {
            # Shifted decision region for use in getGroupSeqProbs
            upperShiftedDecision <- criticalValues[(k + 1):kMax] * sqrt(sum(weights[1:k]^2) +
                cumsum(weights[(k + 1):kMax]^2)) / sqrt(cumsum(weights[(k + 1):kMax]^2)) -
                as.vector(weights[1:k] %*% .getOneMinusQNorm(stageResults$pValues[1:k])) /
                    sqrt(cumsum(weights[(k + 1):kMax]^2))
            if (design$sided == 2) {
                lowerShiftedDecision <- -criticalValues[(k + 1):kMax] * sqrt(sum(weights[1:k]^2) +
                    cumsum(weights[(k + 1):kMax]^2)) / sqrt(cumsum(weights[(k + 1):kMax]^2)) -
                    as.vector(weights[1:k] %*% .getOneMinusQNorm(stageResults$pValues[1:k])) /
                        sqrt(cumsum(weights[(k + 1):kMax]^2))
            }
            if (k == kMax - 1) {
                shiftedFutilityBounds <- c()
            } else {
                shiftedFutilityBounds <- futilityBounds[(k + 1):(kMax - 1)] *
                    sqrt(sum(weights[1:k]^2) + cumsum(weights[(k + 1):(kMax - 1)]^2)) /
                    sqrt(cumsum(weights[(k + 1):(kMax - 1)]^2)) -
                    as.vector(weights[1:k] %*% .getOneMinusQNorm(stageResults$pValues[1:k])) /
                        sqrt(cumsum(weights[(k + 1):(kMax - 1)]^2))
            }
        } else {
            # Shifted decision region for use in getGroupSeqProbs
            upperShiftedDecision <- criticalValues[(k + 1):kMax] *
                sqrt(sum(weights[1:k]^2) + cumsum(weights[(k + 1):kMax]^2)) /
                sqrt(cumsum(weights[(k + 1):kMax]^2)) -
                .getOneMinusQNorm(stageResults$overallPValues[k]) * sqrt(sum(weights[1:k]^2)) /
                    sqrt(cumsum(weights[(k + 1):kMax]^2))
            if (design$sided == 2) {
                lowerShiftedDecision <- -criticalValues[(k + 1):kMax] *
                    sqrt(sum(weights[1:k]^2) + cumsum(weights[(k + 1):kMax]^2)) /
                    sqrt(cumsum(weights[(k + 1):kMax]^2)) -
                    .getOneMinusQNorm(stageResults$overallPValues[k]) * sqrt(sum(weights[1:k]^2)) /
                        sqrt(cumsum(weights[(k + 1):kMax]^2))
            }
            if (k == kMax - 1) {
                shiftedFutilityBounds <- c()
            } else {
                shiftedFutilityBounds <- futilityBounds[(k + 1):(kMax - 1)] *
                    sqrt(sum(weights[1:k]^2) + cumsum(weights[(k + 1):(kMax - 1)]^2)) /
                    sqrt(cumsum(weights[(k + 1):(kMax - 1)]^2)) -
                    .getOneMinusQNorm(stageResults$overallPValues[k]) * sqrt(sum(weights[1:k]^2)) /
                        sqrt(cumsum(weights[(k + 1):(kMax - 1)]^2))
            }
        }

        # Scaled information for use in getGroupSeqProbs
        scaledInformation <- (informationRates[(k + 1):kMax] - informationRates[k]) /
            (1 - informationRates[k])

        if (design$sided == 2) {
            decisionMatrix <- matrix(c(lowerShiftedDecision, upperShiftedDecision), nrow = 2, byrow = TRUE)
            probs <- .getGroupSequentialProbabilities(decisionMatrix = decisionMatrix, informationRates = scaledInformation)
            crp <- sum(probs[3, ] - probs[2, ] + probs[1, ])
        } else {
            if (design$bindingFutility) {
                decisionMatrix <- matrix(c(shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT, upperShiftedDecision),
                    nrow = 2, byrow = TRUE
                )
            } else {
                decisionMatrix <- matrix(c(rep(C_FUTILITY_BOUNDS_DEFAULT, kMax - k), upperShiftedDecision),
                    nrow = 2, byrow = TRUE
                )
            }
            probs <- .getGroupSequentialProbabilities(
                decisionMatrix = decisionMatrix,
                informationRates = scaledInformation
            )
            crp <- sum(probs[3, ] - probs[2, ])
        }
        conditionalRejectionProbabilities[k] <- crp
    }

    if (design$bindingFutility) {
        for (k in 1:min(kMax - 1, stageResults$stage)) {
            if (!is.na(futilityBounds[k])) {
                if (.isTrialDesignInverseNormal(design)) {
                    if (stageResults$combInverseNormal[k] <= futilityBounds[k]) {
                        conditionalRejectionProbabilities[k:stageResults$stage] <- 0
                    }
                } else {
                    if (.getOneMinusQNorm(stageResults$overallPValues[k]) <= futilityBounds[k]) {
                        conditionalRejectionProbabilities[k:stageResults$stage] <- 0
                    }
                }
            }
        }
    }

    return(conditionalRejectionProbabilities)
}

#
# Get CRP based on Fisher combination test
#
.getConditionalRejectionProbabilitiesFisher <- function(..., stageResults) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getConditionalRejectionProbabilitiesFisher", ignore = c("stage", "design"), ...
    )

    design <- stageResults$.design
    .assertIsTrialDesignFisher(design)

    kMax <- design$kMax
    if (kMax == 1) {
        return(NA_real_)
    }

    criticalValues <- .getCriticalValues(design)
    weights <- stageResults$weightsFisher
    if (design$bindingFutility) {
        alpha0Vec <- design$alpha0Vec
    } else {
        alpha0Vec <- rep(1, kMax - 1)
    }

    conditionalRejectionProbabilities <- rep(NA_real_, kMax)
    for (k in (1:min(kMax - 1, stageResults$stage))) {
        if (prod(stageResults$pValues[1:k]^weights[1:k]) <= criticalValues[k]) {
            conditionalRejectionProbabilities[k] <- 1
        } else {
            if (k < kMax - 1) {
                conditionalRejectionProbabilities[k] <- .getFisherCombinationSize(
                    kMax - k,
                    alpha0Vec[(k + 1):(kMax - 1)], (criticalValues[(k + 1):kMax] /
                        prod(stageResults$pValues[1:k]^weights[1:k]))^(1 / weights[k + 1]),
                    weights[(k + 2):kMax] / weights[k + 1]
                )
            } else {
                conditionalRejectionProbabilities[k] <- (criticalValues[kMax] /
                    prod(stageResults$pValues[1:k]^weights[1:k]))^(1 / weights[kMax])
            }
        }
    }

    if (design$bindingFutility) {
        for (k in (1:min(kMax - 1, stageResults$stage))) {
            if (stageResults$pValues[k] > alpha0Vec[k]) {
                conditionalRejectionProbabilities[k:stageResults$stage] <- 0
            }
        }
    }

    conditionalRejectionProbabilities[conditionalRejectionProbabilities >= 1] <- 1
    conditionalRejectionProbabilities[conditionalRejectionProbabilities < 0] <- NA_real_
    return(conditionalRejectionProbabilities)
}

#
# Get CRP based on Fisher combination test, tested through simulation
#
.getConditionalRejectionProbabilitiesFisherSimulated <- function(...,
        stageResults, iterations = 0, seed = NA_real_) {
    .warnInCaseOfUnknownArguments(
        functionName =
            ".getConditionalRejectionProbabilitiesFisherSimulated", ignore = c("design", "simulateCRP"), ...
    )

    design <- stageResults$.design
    .assertIsTrialDesignFisher(design)
    .assertIsValidIterationsAndSeed(iterations, seed)

    criticalValues <- .getCriticalValues(design)
    alpha0Vec <- design$alpha0Vec
    weightsFisher <- stageResults$weightsFisher

    kMax <- design$kMax
    crpFisherSimulated <- rep(NA_real_, kMax)
    if (iterations > 0) {
        seed <- .setSeed(seed)
        if (kMax >= 2) {
            for (k in 1:min(kMax - 1, stageResults$stage)) {
                reject <- 0
                for (i in 1:iterations) {
                    reject <- reject + .getRejectValueCrpFisher(
                        kMax = kMax,
                        alpha0Vec = alpha0Vec, criticalValues = criticalValues,
                        weightsFisher = weightsFisher, k = k, stageResults = stageResults
                    )
                }
                crpFisherSimulated[k] <- reject / iterations
            }
        } else {
            warning("Simulation of CRP Fisher stopped: 'kMax' must be >= 2", call. = FALSE)
        }
    }

    return(list(
        crpFisherSimulated = crpFisherSimulated,
        iterations = iterations,
        seed = seed
    ))
}

#'
#' @title
#' Get Conditional Rejection Probabilities
#'
#' @description
#' Calculates the conditional rejection probabilities (CRP) for given test results.
#'
#' @inheritParams param_stageResults
#' @param ... Further (optional) arguments to be passed:
#' \describe{
#'   \item{\code{iterations}}{Iterations for simulating the conditional
#'       rejection probabilities for Fisher's combination test.
#'       For checking purposes, it can be estimated via simulation with
#'       specified \code{iterations}.}
#'   \item{\code{seed}}{Seed for simulating the conditional rejection probabilities
#'       for Fisher's combination test. See above, default is a random seed.}
#' }
#'
#' @details
#' The conditional rejection probability is the probability, under H0, to reject H0
#' in one of the subsequent (remaining) stages.
#' The probability is calculated using the specified design. For testing rates and the
#' survival design, the normal approximation is used, i.e., it is calculated with the
#' use of the prototype case testing a mean for normally distributed data with known variance.
#'
#' The conditional rejection probabilities are provided up to the specified stage.
#'
#' For Fisher's combination test, you can check the validity of the CRP calculation via simulation.
#'
#' @return Returns a \code{\link[base]{numeric}} vector of length \code{kMax} or in case of multi-arm stage results
#' a \code{\link[base]{matrix}} (each column represents a stage, each row a comparison)
#' containing the conditional rejection probabilities.
#'
#' @family analysis functions
#'
#' @template examples_get_conditional_rejection_probabilities
#'
#' @export
#'
getConditionalRejectionProbabilities <- function(stageResults, ...) {
    stageResults <- .getStageResultsObject(stageResults,
        functionName = "getConditionalRejectionProbabilities", ...
    )

    if (.isEnrichmentStageResults(stageResults)) {
        return(.getConditionalRejectionProbabilitiesEnrichment(stageResults = stageResults, ...))
    }

    if (.isMultiArmStageResults(stageResults)) {
        return(.getConditionalRejectionProbabilitiesMultiArm(stageResults = stageResults, ...))
    }

    .assertIsStageResults(stageResults)

    if (.isTrialDesignInverseNormalOrGroupSequential(stageResults$.design)) {
        return(.getConditionalRejectionProbabilitiesInverseNormalorGroupSequential(
            stageResults = stageResults, ...
        ))
    }

    if (.isTrialDesignFisher(stageResults$.design)) {
        simulateCRP <- .getOptionalArgument("simulateCRP", ...)
        if (!is.null(simulateCRP) && isTRUE(simulateCRP)) {
            iterations <- .getOptionalArgument("iterations", ...)
            if (!is.null(iterations) && iterations > 0) {
                return(.getConditionalRejectionProbabilitiesFisherSimulated(
                    stageResults = stageResults, ...
                ))
            }
        }
        return(.getConditionalRejectionProbabilitiesFisher(
            stageResults = stageResults, ...
        ))
    }

    .stopWithWrongDesignMessage(stageResults$.design,
        inclusiveConditionalDunnett = .isMultiArmStageResults(stageResults)
    )
}

.getDecisionMatrixRoot <- function(...,
        design,
        stage,
        stageResults,
        tolerance,
        firstParameterName,
        case = c(
            "finalConfidenceIntervalGeneralLower",
            "finalConfidenceIntervalGeneralUpper",
            "medianUnbiasedGeneral"
        )) {
    case <- match.arg(case)
    firstValue <- stageResults[[firstParameterName]][stage]
    if (.isTrialDesignGroupSequential(design)) {
        firstValue <- .getOneMinusQNorm(firstValue)
    }

    if (firstValue >= 8) {
        return(NA_real_)
    }

    result <- .getOneDimensionalRoot(
        function(theta) {
            if (design$bindingFutility) {
                row1part1 <- design$futilityBounds[1:(stage - 1)]
            } else {
                row1part1 <- rep(C_FUTILITY_BOUNDS_DEFAULT, stage - 1)
            }
            row1part2 <- C_FUTILITY_BOUNDS_DEFAULT
            row2part1 <- .getCriticalValues(design, 1:(stage - 1))
            row2part2 <- firstValue

            if (.isTrialDesignGroupSequential(design)) {
                if (stageResults$isDatasetSurvival()) {
                    row1part3 <- theta * sqrt(design$informationRates[1:stage] /
                        design$informationRates[stage]) *
                        sqrt(stageResults$overallEvents[stage])
                } else {
                    if (stageResults$isOneSampleDataset()) {
                        row1part3 <- theta * sqrt(design$informationRates[1:stage] /
                            design$informationRates[stage]) *
                            sqrt(stageResults$overallSampleSizes[stage])
                    }

                    if (stageResults$isTwoSampleDataset()) {
                        row1part3 <- theta * sqrt(design$informationRates[1:stage] /
                            design$informationRates[stage]) /
                            sqrt(1 / stageResults$overallSampleSizes1[stage] + 1 /
                                stageResults$overallSampleSizes2[stage])
                    }
                }
            }

            if (.isTrialDesignInverseNormal(design)) {
                if (stageResults$isDatasetSurvival()) {
                    events <- stageResults$getDataInput()$getEventsUpTo(stage)
                    adjInfRate <- cumsum(stageResults$weightsInverseNormal[1:stage] * sqrt(events[1:stage])) /
                        sqrt(cumsum(stageResults$weightsInverseNormal[1:stage]^2))
                } else {
                    if (stageResults$isOneSampleDataset()) {
                        sampleSizes <- stageResults$getDataInput()$getSampleSizesUpTo(stage)
                        adjInfRate <- cumsum(stageResults$weightsInverseNormal[1:stage] *
                            sqrt(sampleSizes[1:stage])) /
                            sqrt(cumsum(stageResults$weightsInverseNormal[1:stage]^2))
                    }

                    if (stageResults$isTwoSampleDataset()) {
                        sampleSizes1 <- stageResults$getDataInput()$getSampleSizesUpTo(stage, 1)
                        sampleSizes2 <- stageResults$getDataInput()$getSampleSizesUpTo(stage, 2)
                        adjInfRate <- cumsum(stageResults$weightsInverseNormal[1:stage] /
                            sqrt(1 / sampleSizes1[1:stage] + 1 / sampleSizes2[1:stage])) /
                            sqrt(cumsum(stageResults$weightsInverseNormal[1:stage]^2))
                    }
                }
                row1part3 <- theta * adjInfRate
            }
            row2part3 <- row1part3

            row1 <- c(row1part1, row1part2) - row1part3
            row2 <- c(row2part1, row2part2) - row2part3

            decisionMatrix <- matrix(c(row1, row2), nrow = 2, byrow = TRUE)

            probs <- .getGroupSequentialProbabilities(
                decisionMatrix = decisionMatrix,
                informationRates = design$informationRates[1:stage]
            )

            if (case == "finalConfidenceIntervalGeneralLower") {
                return(sum(probs[3, ] - probs[2, ]) - design$alpha / design$sided)
            } else if (case == "finalConfidenceIntervalGeneralUpper") {
                return(1 - sum(probs[3, ] - probs[2, ]) - design$alpha / design$sided)
            } else if (case == "medianUnbiasedGeneral") {
                return(sum(probs[3, ] - probs[2, ]) - 0.50)
            } else {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'case' = '", case, "' is not implemented")
            }
        },
        lower = -8,
        upper = 8,
        tolerance = tolerance,
        callingFunctionInformation = ".getDecisionMatrixRoot"
    )
}
