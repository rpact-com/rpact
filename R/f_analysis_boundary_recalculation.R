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
## |  File version: $Revision: 8385 $
## |  Last changed: $Date: 2024-11-13 11:30:34 +0100 (Mi, 13 Nov 2024) $
## |  Last changed by: $Author: pahlke $
## |

.getRecalculatedInformationRates <- function(dataInput,
        maxInformation,
        stage = NA_integer_) {
    .assertIsSingleInteger(stage, "stage", naAllowed = TRUE, validateType = FALSE)
    stageFromData <- dataInput$getNumberOfStages()
    if (is.null(stage) || is.na(stage) || stage > stageFromData) {
        stage <- stageFromData
    }

    informationRates <- rep(NA_real_, stage)
    absoluteInformations <- rep(NA_real_, stage)
    if (.isDatasetMeans(dataInput) || .isDatasetRates(dataInput)) {
        for (k in 1:stage) {
            sampleSizes <- dataInput$getOverallSampleSizes(stage = k)
            absoluteInformations[k] <- sum(sampleSizes, na.rm = TRUE)
            informationRates[k] <- absoluteInformations[k] / maxInformation
        }
    } else if (.isDatasetSurvival(dataInput)) {
        for (k in 1:stage) {
            events <- dataInput$getOverallEvents(stage = k)
            absoluteInformations[k] <- sum(events, na.rm = TRUE)
            informationRates[k] <- absoluteInformations[k] / maxInformation
        }
    } else {
        stop(
            C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'dataInput' class ",
            .getClassName(dataInput), " is not supported", 
            call. = FALSE
        )
    }

    return(list(
        informationRates = informationRates,
        absoluteInformations = absoluteInformations,
        stage = stage
    ))
}

#' @title
#' Get Observed Information Rates
#'
#' @description
#' Recalculates the observed information rates from the specified dataset.
#'
#' @param dataInput The dataset for which the information rates shall be recalculated.
#' @inheritParams param_maxInformation
#' @inheritParams param_informationEpsilon
#' @inheritParams param_stage
#' @inheritParams param_three_dots
#'
#' @details
#' For means and rates the maximum information is the maximum number of subjects
#' or the relative proportion if \code{informationEpsilon} < 1;
#' for survival data it is the maximum number of events
#' or the relative proportion if \code{informationEpsilon} < 1.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[=getAnalysisResults]{getAnalysisResults()}} for using
#'         \code{getObservedInformationRates()} implicit,
#'   \item \href{https://www.rpact.org/vignettes/planning/rpact_boundary_update_example/}{www.rpact.org/vignettes/planning/rpact_boundary_update_example}
#' }
#'
#' @examples
#' \dontrun{
#' # Absolute information epsilon:
#' # decision rule 45 >= 46 - 1, i.e., under-running
#' data <- getDataset(
#'     overallN = c(22, 45),
#'     overallEvents = c(11, 28)
#' )
#' getObservedInformationRates(data,
#'     maxInformation = 46, informationEpsilon = 1
#' )
#'
#' # Relative information epsilon:
#' # last information rate = 45/46 = 0.9783,
#' # is > 1 - 0.03 = 0.97, i.e., under-running
#' data <- getDataset(
#'     overallN = c(22, 45),
#'     overallEvents = c(11, 28)
#' )
#' getObservedInformationRates(data,
#'     maxInformation = 46, informationEpsilon = 0.03
#' )
#' }
#'
#' @return Returns a list that summarizes the observed information rates.
#'
#' @export
#'
getObservedInformationRates <- function(dataInput,
        ...,
        maxInformation = NULL,
        informationEpsilon = NULL,
        stage = NA_integer_) {
    .assertIsDataset(dataInput)
    .assertIsSingleNumber(maxInformation, "maxInformation")
    .assertIsInOpenInterval(
        maxInformation,
        "maxInformation", 0, NULL
    )

    information <- .getRecalculatedInformationRates(dataInput, maxInformation, stage = stage)
    informationRates <- information$informationRates
    absoluteInformations <- information$absoluteInformations
    stage <- information$stage

    status <- "interim-stage"

    showObservedInformationRatesMessage <- .getOptionalArgument("showObservedInformationRatesMessage", ...)
    if (is.null(showObservedInformationRatesMessage) || 
            !is.logical(showObservedInformationRatesMessage)) {
        showObservedInformationRatesMessage <- TRUE
    }

    # Updates at the final analysis in case the observed information at the final analysis
    # is larger ("over-running") or smaller ("under-running") than the planned maximum information
    if (informationRates[length(informationRates)] < 1) {
        underRunningEnabled <- FALSE

        if (!is.null(informationEpsilon)) {
            .assertIsSingleNumber(informationEpsilon, "informationEpsilon")
            .assertIsInOpenInterval(informationEpsilon, "informationEpsilon", 
                lower = 0, upper = maxInformation)

            lastInformationRate <- informationRates[length(informationRates)]
            lastInformationNumber <- absoluteInformations[length(absoluteInformations)]

            if (informationEpsilon < 1) {
                if (lastInformationRate >= (1 - informationEpsilon)) {
                    message(
                        "Under-running: relative information epsilon ",
                        round(informationEpsilon, 4), " is applicable; ",
                        "use observed information ", lastInformationNumber,
                        " instead of planned information ", maxInformation
                    )
                    information <- .getRecalculatedInformationRates(
                        dataInput, lastInformationNumber,
                        stage = stage
                    )
                    informationRates <- information$informationRates
                    absoluteInformations <- information$absoluteInformations
                    stage <- information$stage
                    underRunningEnabled <- TRUE
                    maxInformation <- lastInformationNumber
                    showObservedInformationRatesMessage <- FALSE
                }
            } else {
                if ((lastInformationNumber + informationEpsilon) >= maxInformation) {
                    message(
                        "Under-running: absolute information epsilon ",
                        round(informationEpsilon, 1), " is applicable; ",
                        "use observed information ", lastInformationNumber,
                        " instead of planned information ", maxInformation
                    )
                    maxInformation <- lastInformationNumber
                    information <- .getRecalculatedInformationRates(
                        dataInput, lastInformationNumber,
                        stage = stage
                    )
                    informationRates <- information$informationRates
                    absoluteInformations <- information$absoluteInformations
                    stage <- information$stage
                    underRunningEnabled <- TRUE
                    showObservedInformationRatesMessage <- FALSE
                }
            }
        }

        if (!underRunningEnabled) {
            informationRates <- c(informationRates, 1)
        } else {
            status <- "under-running"
        }
    } else {
        lastInformationNumber <- absoluteInformations[length(absoluteInformations)]
        if (lastInformationNumber > maxInformation) {
            information <- .getRecalculatedInformationRates(
                dataInput, lastInformationNumber,
                stage = stage
            )
            informationRates <- information$informationRates
            absoluteInformations <- information$absoluteInformations
            stage <- information$stage
            message(
                "Over-running: observed information ", lastInformationNumber,
                " at stage ", length(absoluteInformations),
                " is larger than the maximum planned information ", maxInformation, "; ",
                "information rates will be recalculated"
            )
            status <- "over-running"
            maxInformation <- lastInformationNumber
            showObservedInformationRatesMessage <- FALSE
        }
    }

    if (any(informationRates > 1)) {
        warning("The observed information at stage ",
            .arrayToString(which(informationRates > 1)), " is over-running, ",
            "i.e., the information rate (", .arrayToString(informationRates[informationRates > 1]), ") ",
            "is larger than the planned maximum information rate (1)",
            call. = FALSE
        )
    }

    informationRates[informationRates > 1] <- 1

    end <- min(which(informationRates == 1))
    informationRates <- informationRates[1:end]

    if (showObservedInformationRatesMessage) {
        message(
            "The observed information rates for 'maxInformation' = ", maxInformation,
            " at stage ", stage, " are: ", .arrayToString(informationRates)
        )
    }

    if (status == "interim-stage" && informationRates[length(informationRates)] == 1 &&
            stage == length(informationRates)) {
        status <- "final-stage"
    }

    return(list(
        absoluteInformations = absoluteInformations,
        maxInformation = maxInformation,
        informationEpsilon = informationEpsilon,
        informationRates = informationRates,
        status = status
    ))
}

.getRecalculatedDesign <- function(design, newArgumentValues, ignore = character()) {
    parametersToIgnore <- character()
    if (!("kMax" %in% names(newArgumentValues))) {
        if (design$.getParameterType("kMax") %in% C_PARAM_USER_DEFINED) {
            parametersToIgnore <- c(parametersToIgnore, "kMax")
        }
        newArgumentValues$kMax <- NA_integer_
    }
    for (paramName in c("futilityBounds", "informationRates", ignore)) {
        if (design$.getParameterType(paramName) %in% C_PARAM_USER_DEFINED) {
            parametersToIgnore <- c(parametersToIgnore, paramName)
            if (!(paramName %in% names(newArgumentValues))) {
                newArgumentValues[[paramName]] <- NA_real_
            }
        }
    }
    if (length(parametersToIgnore) > 0) {
        warning("The user-defined parameters ", 
            .arrayToString(sQuote(parametersToIgnore), mode = "and"),
            " will be ignored because they are not applicable ",
            "for automatic recalculation of the boundaries",
            call. = FALSE
        )
    }

    return(eval(parse(text = getObjectRCode(design,
        newArgumentValues = newArgumentValues,
        stringWrapParagraphWidth = NULL
    ))))
}

.getDesignWithRecalculatedBoundaries <- function(...,
        design,
        dataInput,
        directionUpper,
        thetaH0,
        nPlanned,
        allocationRatioPlanned,
        stage,
        maxInformation,
        informationEpsilon) {
    result <- list(
        design = design,
        informationRatesRecalculated = FALSE,
        repeatedPValues = NULL
    )

    if (!.isAlphaSpendingDesign(design) ||
            design$typeBetaSpending != "none" ||
            !.isTrialDesignGroupSequential(design) ||
            .isMultiArmDataset(dataInput) || 
            .isEnrichmentDataset(dataInput)) {
            
        arguments <- character()
        if (!is.null(maxInformation) && !is.na(maxInformation)) {
            arguments <- c(arguments, paste0("'maxInformation' (", .arrayToString(maxInformation), ")"))
        }
        if (!is.null(informationEpsilon) && !is.na(informationEpsilon)) {
            arguments <- c(arguments, paste0("'informationEpsilon' (", .arrayToString(informationEpsilon), ")"))
        }
        if (length(arguments) > 0) {
            warning(.arrayToString(arguments, mode = "and"),
                " will be ignored because ", ifelse(length(arguments) == 1, "it is", "they are"), 
                " only applicable for alpha spending", "\n",
                "group sequential designs with no futility bounds and a single hypothesis",
                call. = FALSE
            )
        }
        return(result)
    }

    if (is.null(maxInformation) || length(maxInformation) == 0 || is.na(maxInformation)) {
        if (!is.null(informationEpsilon) && !all(is.na(informationEpsilon))) {
            warning("'informationEpsilon' (", .arrayToString(informationEpsilon),
                ") will be ignored because 'maxInformation' is undefined",
                call. = FALSE
            )
        }
        return(result)
    }

    if (design$typeOfDesign == "asUser") {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "recalculation of the information rates not possible ",
            "for user-defined alpha spending designs", 
            call. = FALSE
        )
    }

    showObservedInformationRatesMessage <- .getOptionalArgument(
        "showObservedInformationRatesMessage",
        optionalArgumentDefaultValue = TRUE, ...
    )
    observedInformation <- getObservedInformationRates(
        dataInput,
        maxInformation = maxInformation,
        informationEpsilon = informationEpsilon,
        stage = stage,
        showObservedInformationRatesMessage = showObservedInformationRatesMessage
    )

    observedInformationRates <- observedInformation$informationRates
    if (is.null(observedInformationRates)) {
        return(result)
    }

    if (is.null(observedInformation$status) ||
            !(observedInformation$status %in% c("under-running", "over-running")) ||
            length(observedInformationRates) <= 1) {
        design <- .getRecalculatedDesign(design,
            newArgumentValues = list(informationRates = observedInformationRates)
        )
        return(list(
            design = design,
            informationRatesRecalculated = TRUE,
            repeatedPValues = NULL
        ))
    }

    stageFromData <- dataInput$getNumberOfStages()
    if (stageFromData == 1) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "recalculation of the information rates not possible at stage 1", 
            call. = FALSE
        )
    }

    if (!(getLogLevel() %in% c(C_LOG_LEVEL_DISABLED, C_LOG_LEVEL_PROGRESS))) {
        message(
            "Calculate alpha values that have actually been spent ",
            "at earlier interim analyses at stage ", (stageFromData - 1)
        )
    }
    .assertIsSingleInteger(stage, "stage", naAllowed = TRUE, validateType = FALSE)
    observedInformationRatesBefore <- getObservedInformationRates(
        dataInput,
        maxInformation = maxInformation,
        informationEpsilon = informationEpsilon,
        stage = ifelse(!is.na(stage), stage - 1, stageFromData - 1),
        showObservedInformationRatesMessage = FALSE
    )$informationRates
    if (length(observedInformationRatesBefore) < length(design$informationRates)) {
        for (i in (length(observedInformationRatesBefore) + 1):length(design$informationRates)) {
            if (observedInformationRatesBefore[length(observedInformationRatesBefore)] < 1) {
                observedInformationRatesBefore <-
                    c(observedInformationRatesBefore, design$informationRates[i])
            }
        }
    }

    designBefore <- .getRecalculatedDesign(design,
        newArgumentValues = list(informationRates = observedInformationRatesBefore)
    )

    repeatedPValues <- NULL
    if (is.na(stage) || stage == stageFromData) {
        repeatedPValues <- getAnalysisResults(
            design = designBefore,
            dataInput = dataInput,
            directionUpper = directionUpper,
            thetaH0 = thetaH0,
            nPlanned = nPlanned,
            allocationRatioPlanned = allocationRatioPlanned,
            stage = stageFromData - 1,
            maxInformation = maxInformation,
            informationEpsilon = informationEpsilon,
            showObservedInformationRatesMessage = FALSE
        )$repeatedPValues
    }

    userAlphaSpending <- designBefore$alphaSpent
    message(
        "Use alpha values that have actually been spent at earlier stages ",
        "and spend all remaining alpha at the final analysis, ",
        "i.e., userAlphaSpending = (",
        .arrayToString(userAlphaSpending, digits = 6), ") "
    )
    absoluteInformations <- observedInformation$absoluteInformations
    observedInformationRates <- getObservedInformationRates(
        dataInput,
        maxInformation = absoluteInformations[stageFromData],
        informationEpsilon = informationEpsilon,
        stage = stage,
        showObservedInformationRatesMessage = FALSE
    )$informationRates
    design <- .getRecalculatedDesign(design,
        newArgumentValues = list(
            informationRates = observedInformationRates,
            userAlphaSpending = userAlphaSpending,
            typeOfDesign = C_TYPE_OF_DESIGN_AS_USER
        )
    )
    base::options("rpact.analyis.repeated.p.values.warnings.enabled" = "FALSE")
    warning("Repeated p-values not available for automatic ",
        "recalculation of boundaries at final stage",
        call. = FALSE
    )

    return(list(
        design = design,
        informationRatesRecalculated = TRUE,
        repeatedPValues = repeatedPValues
    ))
}
