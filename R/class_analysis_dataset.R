## |
## |  *Dataset classes*
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
## |  File version: $Revision: 7139 $
## |  Last changed: $Date: 2023-06-28 08:15:31 +0200 (Mi, 28 Jun 2023) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_analysis_utilities.R
#' @include f_core_utilities.R
#' @include f_object_r_code.R
NULL

C_KEY_WORDS_GROUPS <- c("group", "groups")

C_KEY_WORDS_STAGES <- c("stage", "stages")

C_KEY_WORDS_SUBSETS <- c("subset", "subsets")

C_KEY_WORDS_SAMPLE_SIZES <- .getAllParameterNameVariants(c("n", "N", "sampleSizes", "sampleSize"))

C_KEY_WORDS_MEANS <- c("means", "mean")

C_KEY_WORDS_ST_DEVS <- .getAllParameterNameVariants(c("stDevs", "stDev", "stds", "sd"))

C_KEY_WORDS_EVENTS <- c("event", "events")

C_KEY_WORDS_OVERALL_EVENTS <- .getAllParameterNameVariants(c("overallEvents", "overallEvent"))

C_KEY_WORDS_EXPECTED_EVENTS <- .getAllParameterNameVariants(c("expectedEvents", "expectedEvent"))

C_KEY_WORDS_VARIANCE_EVENTS <- .getAllParameterNameVariants(c("varianceEvents", "varianceEvent"))

C_KEY_WORDS_OVERALL_EXPECTED_EVENTS <- .getAllParameterNameVariants(c("overallExpectedEvents", "overallExpectedEvent"))

C_KEY_WORDS_OVERALL_VARIANCE_EVENTS <- .getAllParameterNameVariants(c("overallVarianceEvents", "overallVarianceEvent"))

C_KEY_WORDS_OVERALL_SAMPLE_SIZES <- .getAllParameterNameVariants(c(
    "overallN", "overallSampleSizes", "overallSampleSize"
))

C_KEY_WORDS_OVERALL_MEANS <- .getAllParameterNameVariants(c("overallMeans", "overallMean"))

C_KEY_WORDS_OVERALL_ST_DEVS <- .getAllParameterNameVariants(c(
    "overallStDevs", "overallStDev", "overall.sd", "overall_sd"
))

C_KEY_WORDS_ALLOCATION_RATIOS <- .getAllParameterNameVariants(c("ar", "allocationRatios", "allocationRatio"))

C_KEY_WORDS_LOG_RANKS <- .getAllParameterNameVariants(c("logRanks", "logRank", "lr"))

C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS <- .getAllParameterNameVariants(c(
    "oar", "car", "overallAllocationRatios", "overallAllocationRatio"
))

C_KEY_WORDS_OVERALL_LOG_RANKS <- .getAllParameterNameVariants(c("olr", "clr", "overallLogRanks", "overallLogRank"))

C_KEY_WORDS <- c(
    C_KEY_WORDS_GROUPS,
    C_KEY_WORDS_STAGES,
    C_KEY_WORDS_SUBSETS,
    C_KEY_WORDS_SAMPLE_SIZES,
    C_KEY_WORDS_MEANS,
    C_KEY_WORDS_ST_DEVS,
    C_KEY_WORDS_EVENTS,
    C_KEY_WORDS_OVERALL_EVENTS,
    C_KEY_WORDS_OVERALL_SAMPLE_SIZES,
    C_KEY_WORDS_OVERALL_MEANS,
    C_KEY_WORDS_OVERALL_ST_DEVS,
    C_KEY_WORDS_ALLOCATION_RATIOS,
    C_KEY_WORDS_LOG_RANKS,
    C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS,
    C_KEY_WORDS_OVERALL_LOG_RANKS
)

#'
#' @name Dataset
#'
#' @title
#' Dataset
#'
#' @description
#' Basic class for datasets.
#'
#' @template field_stages
#' @template field_groups
#'
#' @details
#' \code{Dataset} is the basic class for
#' \itemize{
#'   \item \code{\link{DatasetMeans}},
#'   \item \code{\link{DatasetRates}},
#'   \item \code{\link{DatasetSurvival}}, and
#'   \item \code{\link{DatasetEnrichmentSurvival}}.
#' }
#' This basic class contains the fields \code{stages} and \code{groups} and several commonly used
#' functions.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include f_core_assertions.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
Dataset <- setRefClass("Dataset",
    contains = "ParameterSet",
    fields = list(
        .data = "data.frame",
        .plotSettings = "ANY",
        .id = "integer",
        .description = "character",
        .floatingPointNumbersEnabled = "logical",
        .kMax = "integer",
        .enrichmentEnabled = "logical",
        .inputType = "character",
        .design = "ANY",
        stages = "integer",
        groups = "integer",
        subsets = "character"
    ),
    methods = list(
        initialize = function(dataFrame, ..., floatingPointNumbersEnabled = FALSE, enrichmentEnabled = FALSE) {
            callSuper(
                .floatingPointNumbersEnabled = floatingPointNumbersEnabled,
                .enrichmentEnabled = enrichmentEnabled, ...
            )
            .plotSettings <<- PlotSettingsR6$new()
            .parameterNames <<- .getParameterNames(dataset = .self)
            .parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS

            .id <<- NA_integer_
            .description <<- NA_character_
            .inputType <<- NA_character_

            if (!missing(dataFrame)) {
                .initByDataFrame(dataFrame)
                .kMax <<- getNumberOfStages()
                if (!.enrichmentEnabled) {
                    .validateDataset()
                }
            }
        },
        getPlotSettings = function() {
            return(.plotSettings)
        },
        show = function(showType = 1, digits = NA_integer_) {
            "Method for automatically printing dataset objects"
            .show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            .resetCat()

            if (!is.null(showType) && length(showType) == 1 && !is.na(showType) &&
                    is.character(showType) && showType == "rcmd") {
                s <- strsplit(getObjectRCode(.self, stringWrapParagraphWidth = NULL), "), *")[[1]]
                s[2:length(s)] <- paste0("\t", s[2:length(s)])
                s <- paste0(s, collapse = "),\n")
                cat(s, "\n")
            } else if (showType == 2) {
                callSuper(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                .showParametersOfOneGroup(.getUserDefinedParameters(),
                    title = .toString(startWithUpperCase = TRUE), orderByParameterName = FALSE,
                    consoleOutputEnabled = consoleOutputEnabled
                )

                .showParametersOfOneGroup(.getGeneratedParameters(),
                    title = "Calculated data", orderByParameterName = FALSE,
                    consoleOutputEnabled = consoleOutputEnabled
                )

                .showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)

                if (!is.na(.description) && nchar(.description) > 0) {
                    .cat("Description: ", .description, "\n\n",
                        consoleOutputEnabled = consoleOutputEnabled
                    )
                }
            }
        },
        .initByDataFrame = function(dataFrame) {
            if (!is.data.frame(dataFrame)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'dataFrame' must be a data.frame (is an instance of class ", .getClassName(dataFrame), ")"
                )
            }

            if (!.paramExists(dataFrame, "stage") && !.paramExists(dataFrame, "stages")) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'dataFrame' must contain parameter 'stages' or 'stage'"
                )
            }

            stages <<- as.integer(.getValuesByParameterName(dataFrame, c("stages", "stage")))
            if (!.enrichmentEnabled && length(unique(stages)) < length(stages)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'stages' (", .arrayToString(stages),
                    ") must be a unique vector of stage numbers"
                )
            }
            groups <<- rep(1L, length(stages))

            .setParameterType("groups", C_PARAM_USER_DEFINED)
            .setParameterType("stages", C_PARAM_USER_DEFINED)

            if (any(grepl("^subsets?\\d*$", colnames(dataFrame)))) {
                numberOfTreatmentGroups <- .getNumberOfGroups(dataFrame, c(C_KEY_WORDS_SAMPLE_SIZES, C_KEY_WORDS_LOG_RANKS))
                subsets <<- character(0)
                for (group in 1:numberOfTreatmentGroups) {
                    suffix <- ifelse(any(grepl("^subsets?\\d+$", colnames(dataFrame))), group, "")
                    subsets <<- c(subsets, .getValuesByParameterName(dataFrame, C_KEY_WORDS_SUBSETS, suffix = suffix))
                }
                .setParameterType("subsets", C_PARAM_USER_DEFINED)
            } else {
                subsets <<- rep(NA_character_, length(stages))
            }
        },
        .validateDataset = function() {
            .assertIsValidKMax(kMax = getNumberOfStages())

            for (var in names(.self)) {
                values <- .self[[var]]
                if (any(is.nan(values)) || any(is.infinite(values))) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'", var, "' (", .arrayToString(values),
                        ") contains illegal values, i.e., something went wrong"
                    )
                }
            }
        },
        .validateValues = function(values, name) {
            if (.enrichmentEnabled) {
                return(invisible())
            }

            l1 <- length(unique(stages))
            l2 <- length(values)
            if (l1 != l2) {
                stop(
                    C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                    "there ", ifelse(l1 == 1, paste("is", l1, "stage"),
                        paste("are", l1, "stages")
                    ), " defined",
                    " (", .arrayToString(unique(stages)), ") and '", name, "' has length ", l2
                )
            }
        },
        .recreateDataFrame = function() {
            .data <<- data.frame(
                stage = factor(stages),
                group = factor(groups),
                subset = factor(subsets)
            )
        },
        .setDataToVariables = function() {
            stages <<- as.integer(.data$stage)
            groups <<- as.integer(.data$group)
            subsets <<- as.character(.data$subset)
        },
        .fillWithNAs = function(kMax) {
            numberOfStages <- getNumberOfStages()
            .kMax <<- numberOfStages
            if (numberOfStages >= kMax) {
                return(invisible())
            }

            numberOfGroups <- getNumberOfGroups(survivalCorrectionEnabled = FALSE)
            if (.enrichmentEnabled) {
                for (stage in (numberOfStages + 1):kMax) {
                    for (group in 1:numberOfGroups) {
                        for (subset in levels(.data$subset)) {
                            stages <<- c(stages, stage)
                            groups <<- c(groups, group)
                            subsets <<- c(subsets, subset)
                        }
                    }
                }
            } else {
                for (stage in (numberOfStages + 1):kMax) {
                    for (group in 1:numberOfGroups) {
                        stages <<- c(stages, stage)
                        groups <<- c(groups, group)
                        subsets <<- c(subsets, NA_character_)
                    }
                }
            }
        },
        .trim = function(kMax) {
            if (is.na(kMax)) {
                kMax <- .kMax
            }
            numberOfStages <- getNumberOfStages(FALSE)
            if (numberOfStages <= kMax) {
                return(invisible(numeric(0)))
            }

            indices <- which(stages <= kMax)

            stages <<- stages[indices]
            groups <<- groups[indices]
            subsets <<- subsets[indices]

            return(indices)
        },
        .orderDataByStageAndGroup = function() {
            if (.enrichmentEnabled) {
                dat <- .data
                dat$char <- gsub("\\d", "", as.character(.data$subset))
                dat$char[dat$char == "R"] <- "Z"
                dat$char[dat$char == "F"] <- "Z"
                dat$num <- as.integer(gsub("\\D", "", as.character(.data$subset)))

                .data <<- .data[order(.data$stage, .data$group, dat$char, dat$num), ]
            } else {
                .data <<- .data[order(.data$stage, .data$group), ]
            }
        },
        .getNumberOfNAsToAdd = function(kMax) {
            n <- kMax - getNumberOfStages()
            if (n <= 0) {
                return(0)
            }

            n <- n * getNumberOfGroups(survivalCorrectionEnabled = FALSE)
            if (.enrichmentEnabled) {
                n <- n * getNumberOfSubsets()
            }
            return(n)
        },
        .paramExists = function(dataFrame, parameterName) {
            for (p in parameterName) {
                value <- dataFrame[[p]]
                if (!is.null(value)) {
                    return(TRUE)
                }
            }
            return(FALSE)
        },
        .getValuesByParameterName = function(dataFrame, parameterNameVariants, ...,
                defaultValues = NULL, suffix = "") {
            for (parameterName in parameterNameVariants) {
                key <- paste0(parameterName, suffix)
                if (.paramExists(dataFrame, key)) {
                    return(dataFrame[[key]])
                }
            }

            if (!is.null(defaultValues)) {
                return(defaultValues)
            }

            stop(
                C_EXCEPTION_TYPE_MISSING_ARGUMENT, "parameter '",
                paste0(parameterNameVariants[1], suffix), "' is missing or not correctly specified"
            )
        },
        .getValueLevels = function(values) {
            if (is.factor(values)) {
                return(levels(values))
            }

            return(sort(unique(na.omit(values))))
        },
        .getValues = function(paramName, paramValues) {
            values <- .data[[paramName]]
            valueLevels <- .getValueLevels(values)
            if (!all(paramValues %in% valueLevels)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", paramName, "' (", .arrayToString(paramValues),
                    ") out of range [", .arrayToString(valueLevels), "]"
                )
            }
            return(values)
        },
        .getIndexValues = function(paramName, paramValues, subset = NA_character_) {
            values <- .getValues(paramName, paramValues)
            if (all(is.na(subset))) {
                return(which(values %in% paramValues))
            }

            .assertIsValidSubset(subset)
            return(which(values %in% paramValues & .data$subset %in% subset))
        },
        .assertIsValidSubset = function(subset) {
            for (s in subset) {
                if (!(s %in% levels(.data$subset))) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'subset' (", s,
                        ") is not a defined value [", .arrayToString(levels(.data$subset)), "]"
                    )
                }
            }
        },
        .getIndices = function(..., stage, group, subset = NA_character_) {
            if (is.null(.data)) {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'.data' must be defined")
            }

            if (!is.null(stage) && !any(is.na(stage)) && all(stage < 0)) {
                index <- 1:getNumberOfStages()
                stage <- index[!(index %in% abs(stage))]
            }

            if (!is.null(group) && !any(is.na(group)) && all(group < 0)) {
                index <- 1:getNumberOfGroups(survivalCorrectionEnabled = FALSE)
                group <- index[!(index %in% abs(group))]
            }

            # stage only and optional subset
            if (!is.null(group) && length(group) == 1 && is.na(group)) {
                return(.getIndexValues("stage", stage, subset))
            }

            # group only and optional subset
            if (!is.null(stage) && length(stage) == 1 && is.na(stage)) {
                return(.getIndexValues("group", group, subset))
            }

            # stage and group and optional subset
            stageValues <- .getValues("stage", stage)
            groupValues <- .getValues("group", group)
            if (all(is.na(subset))) {
                return(which(stageValues %in% stage & groupValues %in% group))
            }

            .assertIsValidSubset(subset)
            return(which(stageValues %in% stage & groupValues %in% group & .data$subset %in% subset))
        },
        .getValidatedFloatingPointNumbers = function(x, parameterName = "Sample sizes") {
            if (.floatingPointNumbersEnabled) {
                return(x)
            }

            nToCheck <- stats::na.omit(x)
            if (any(nToCheck != as.integer(nToCheck))) {
                warning(parameterName, " specified as floating-point numbers were truncated", call. = FALSE)
            }

            x[!is.na(x)] <- as.integer(x[!is.na(x)])
            return(x)
        },
        .keyWordExists = function(dataFrame, keyWords, suffix = "") {
            for (key in keyWords) {
                if (.paramExists(dataFrame, paste0(key, suffix))) {
                    return(TRUE)
                }
            }
            return(FALSE)
        },
        .getNumberOfGroups = function(dataFrame, keyWords) {
            for (group in 2:1000) {
                if (!.keyWordExists(dataFrame, keyWords, group)) {
                    return(group - 1)
                }
            }
            return(1)
        },
        .getValidatedStage = function(stage = NA_integer_) {
            if (all(is.na(stage))) {
                stage <- c(1:getNumberOfStages())
            }
            return(stage)
        },
        getNumberOfGroups = function(survivalCorrectionEnabled = TRUE) {
            data <- stats::na.omit(.data)
            if (!survivalCorrectionEnabled) {
                return(length(levels(data$group)))
            }
            return(length(levels(data$group)) + ifelse(inherits(.self, "DatasetSurvival"), 1, 0))
        },
        getNumberOfStages = function(naOmitEnabled = TRUE) {
            if (naOmitEnabled) {
                colNames <- colnames(.data)
                validColNames <- character(0)
                for (colName in colNames) {
                    colValues <- .data[, colName]
                    if (length(colValues) > 0 && !all(is.na(colValues))) {
                        validColNames <- c(validColNames, colName)
                    }
                }
                subData <- stats::na.omit(.data[, validColNames])
                numberOfStages <- length(unique(as.character(subData$stage)))
                if (numberOfStages == 0) {
                    print(.data[, validColNames])
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                        ".data seems to contain an invalid column"
                    )
                }
                return(numberOfStages)
            }
            return(length(levels(.data$stage)))
        },
        getNumberOfSubsets = function() {
            return(length(levels(.data$subset)))
        },
        isDatasetMeans = function() {
            return(inherits(.self, "DatasetMeans"))
        },
        isDatasetRates = function() {
            return(inherits(.self, "DatasetRates"))
        },
        isDatasetSurvival = function() {
            return(inherits(.self, "DatasetSurvival"))
        },
        isStratified = function() {
            return(.enrichmentEnabled && "R" %in% levels(.data$subset))
        },
        setId = function(id) {
            .id <<- as.integer(id)
        },
        getId = function() {
            return(.id)
        },
        setDescription = function(description) {
            .description <<- description
        },
        getDescription = function() {
            return(.description)
        },
        .toString = function(startWithUpperCase = FALSE) {
            s <- "dataset of "
            if (.enrichmentEnabled) {
                s <- paste0(s, "enrichment ")
            } else if (.self$getNumberOfGroups() > 2) {
                s <- paste0(s, "multi-arm ")
            }

            if (isDatasetMeans()) {
                s <- paste0(s, "means")
            } else if (isDatasetRates()) {
                s <- paste0(s, "rates")
            } else if (isDatasetSurvival()) {
                s <- paste0(s, "survival data")
            } else {
                s <- paste0(s, "unknown endpoint")
            }
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
        }
    )
)

#'
#' @name DatasetMeans
#'
#' @title
#' Dataset of Means
#'
#' @description
#' Class for a dataset of means.
#'
#' @template field_groups
#' @template field_stages
#' @template field_sampleSizes
#' @template field_means
#' @template field_stDevs
#' @template field_overallSampleSizes
#' @template field_overallMeans
#' @template field_overallStDevs
#'
#' @details
#' This object cannot be created directly; better use \code{\link{getDataset}}
#' with suitable arguments to create a dataset of means.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
DatasetMeans <- setRefClass("DatasetMeans",
    contains = "Dataset",
    fields = list(
        sampleSizes = "numeric",
        means = "numeric",
        stDevs = "numeric",
        overallSampleSizes = "numeric",
        overallMeans = "numeric",
        overallStDevs = "numeric"
    ),
    methods = list(
        getSampleSize = function(stage, group = 1, subset = NA_character_) {
            return(.data$sampleSize[.getIndices(stage = stage, group = group, subset = subset)])
        },
        getMean = function(stage, group = 1, subset = NA_character_) {
            return(.data$mean[.getIndices(stage = stage, group = group, subset = subset)])
        },
        getStDev = function(stage, group = 1, subset = NA_character_) {
            return(.data$stDev[.getIndices(stage = stage, group = group, subset = subset)])
        },
        getSampleSizes = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(.data$sampleSize[.getIndices(stage = .getValidatedStage(stage), group = group, subset = subset)])
        },
        getMeans = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(.data$mean[.getIndices(stage = .getValidatedStage(stage), group = group, subset = subset)])
        },
        getStDevs = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(.data$stDev[.getIndices(stage = .getValidatedStage(stage), group = group, subset = subset)])
        },
        getSampleSizesUpTo = function(to, group = 1, subset = NA_character_) {
            return(.data$sampleSize[.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getMeansUpTo = function(to, group = 1, subset = NA_character_) {
            return(.data$mean[.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getStDevsUpTo = function(to, group = 1, subset = NA_character_) {
            return(.data$stDev[.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getOverallSampleSize = function(stage, group = 1, subset = NA_character_) {
            return(.data$overallSampleSize[.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallMean = function(stage, group = 1, subset = NA_character_) {
            return(.data$overallMean[.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallStDev = function(stage, group = 1, subset = NA_character_) {
            return(.data$overallStDev[.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallSampleSizes = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(.data$overallSampleSize[.getIndices(stage = .getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallMeans = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(.data$overallMean[.getIndices(stage = .getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallStDevs = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(.data$overallStDev[.getIndices(stage = .getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallSampleSizesUpTo = function(to, group = 1, subset = NA_character_) {
            return(.data$overallSampleSize[.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getOverallMeansUpTo = function(to, group = 1, subset = NA_character_) {
            return(.data$overallMean[.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getOverallStDevsUpTo = function(to, group = 1, subset = NA_character_) {
            return(.data$overallStDev[.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        .initByDataFrame = function(dataFrame) {
            callSuper(dataFrame)

            # case: one mean - stage wise
            if (.paramExists(dataFrame, C_KEY_WORDS_SAMPLE_SIZES)) {
                .inputType <<- "stagewise"
                sampleSizes <<- .getValidatedFloatingPointNumbers(.getValuesByParameterName(
                    dataFrame,
                    C_KEY_WORDS_SAMPLE_SIZES
                ), parameterName = "Sample sizes")
                .validateValues(sampleSizes, "n")
                if (any(stats::na.omit(sampleSizes) <= 0)) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "all sample sizes must be > 0, but 'n' = ",
                        .arrayToString(sampleSizes, vectorLookAndFeelEnabled = TRUE)
                    )
                }

                means <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_MEANS)
                .validateValues(means, "means")

                stDevs <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_ST_DEVS)
                .validateValues(stDevs, "stDevs")
            }

            # case: one mean - overall
            else if (.paramExists(dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES)) {
                .inputType <<- "overall"
                overallSampleSizes <<- .getValidatedFloatingPointNumbers(.getValuesByParameterName(
                    dataFrame,
                    C_KEY_WORDS_OVERALL_SAMPLE_SIZES
                ), parameterName = "Cumulative sample sizes ")
                .validateValues(overallSampleSizes, "overallSampleSizes")

                overallMeans <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_MEANS)
                .validateValues(overallMeans, "overallMeans")

                overallStDevs <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_ST_DEVS)
                .validateValues(overallStDevs, "overallStDevs")
            }

            # case: two or more means - stage wise
            else if (.paramExists(dataFrame, paste0(C_KEY_WORDS_SAMPLE_SIZES, 1)) &&
                    .paramExists(dataFrame, paste0(C_KEY_WORDS_SAMPLE_SIZES, 2))) {
                .inputType <<- "stagewise"
                numberOfTreatmentGroups <- .getNumberOfGroups(dataFrame, C_KEY_WORDS_SAMPLE_SIZES)
                stages <<- rep(stages, numberOfTreatmentGroups)
                groups <<- integer(0)
                sampleSizes <<- numeric(0)
                means <<- numeric(0)
                stDevs <<- numeric(0)
                for (group in 1:numberOfTreatmentGroups) {
                    sampleSizesTemp <- .getValidatedFloatingPointNumbers(.getValuesByParameterName(
                        dataFrame, C_KEY_WORDS_SAMPLE_SIZES,
                        suffix = group
                    ), parameterName = "Sample sizes")
                    .validateValues(sampleSizesTemp, paste0("n", group))
                    if (any(stats::na.omit(sampleSizesTemp) <= 0)) {
                        stop(
                            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                            "all sample sizes must be > 0, but 'n", group, "' = ",
                            .arrayToString(sampleSizesTemp, vectorLookAndFeelEnabled = TRUE)
                        )
                    }
                    sampleSizes <<- c(sampleSizes, sampleSizesTemp)

                    meansTemp <- .getValuesByParameterName(dataFrame, C_KEY_WORDS_MEANS, suffix = group)
                    .validateValues(meansTemp, paste0("means", group))
                    means <<- c(means, meansTemp)

                    stDevsTemp <- .getValuesByParameterName(dataFrame, C_KEY_WORDS_ST_DEVS, suffix = group)
                    .validateValues(stDevsTemp, paste0("stDevs", group))
                    stDevs <<- c(stDevs, stDevsTemp)

                    groups <<- c(groups, rep(as.integer(group), length(sampleSizesTemp)))
                }
            }

            # case: two or more means - overall
            else if (.paramExists(dataFrame, paste0(C_KEY_WORDS_OVERALL_SAMPLE_SIZES, 1)) &&
                    .paramExists(dataFrame, paste0(C_KEY_WORDS_OVERALL_SAMPLE_SIZES, 2))) {
                .inputType <<- "overall"
                numberOfTreatmentGroups <- .getNumberOfGroups(dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES)
                stages <<- rep(stages, numberOfTreatmentGroups)
                groups <<- integer(0)
                sampleSizes <<- numeric(0)
                means <<- numeric(0)
                stDevs <<- numeric(0)
                overallSampleSizes <<- numeric(0)
                overallMeans <<- numeric(0)
                overallStDevs <<- numeric(0)
                for (group in 1:numberOfTreatmentGroups) {
                    overallSampleSizesTemp <- .getValidatedFloatingPointNumbers(.getValuesByParameterName(
                        dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES,
                        suffix = group
                    ), parameterName = "Cumulative sample sizes")
                    .validateValues(overallSampleSizesTemp, paste0("overallSampleSizes", group))
                    overallSampleSizes <<- c(overallSampleSizes, overallSampleSizesTemp)

                    overallMeansTemp <- .getValuesByParameterName(dataFrame,
                        C_KEY_WORDS_OVERALL_MEANS,
                        suffix = group
                    )
                    .validateValues(overallMeansTemp, paste0("overallMeans", group))
                    overallMeans <<- c(overallMeans, overallMeansTemp)

                    overallStDevsTemp <- .getValuesByParameterName(dataFrame,
                        C_KEY_WORDS_OVERALL_ST_DEVS,
                        suffix = group
                    )
                    .validateValues(overallStDevsTemp, paste0("overallStDevs", group))
                    overallStDevs <<- c(overallStDevs, overallStDevsTemp)

                    groups <<- c(groups, rep(as.integer(group), length(overallSampleSizesTemp)))
                }
            } else {
                stop(
                    C_EXCEPTION_TYPE_MISSING_ARGUMENT,
                    "sample sizes are missing or not correctly specified"
                )
            }

            if (.inputType == "stagewise") {
                n <- length(sampleSizes)
                overallSampleSizes <<- rep(NA_real_, n)
                overallMeans <<- rep(NA_real_, n)
                overallStDevs <<- rep(NA_real_, n)

                .setParameterType("sampleSizes", C_PARAM_USER_DEFINED)
                .setParameterType("means", C_PARAM_USER_DEFINED)
                .setParameterType("stDevs", C_PARAM_USER_DEFINED)

                .setParameterType("overallSampleSizes", C_PARAM_GENERATED)
                .setParameterType("overallMeans", C_PARAM_GENERATED)
                .setParameterType("overallStDevs", C_PARAM_GENERATED)

                .recreateDataFrame()
                .createOverallData()
            } else {
                n <- length(overallSampleSizes)
                sampleSizes <<- rep(NA_real_, n)
                means <<- rep(NA_real_, n)
                stDevs <<- rep(NA_real_, n)

                .setParameterType("sampleSizes", C_PARAM_GENERATED)
                .setParameterType("means", C_PARAM_GENERATED)
                .setParameterType("stDevs", C_PARAM_GENERATED)

                .setParameterType("overallSampleSizes", C_PARAM_USER_DEFINED)
                .setParameterType("overallMeans", C_PARAM_USER_DEFINED)
                .setParameterType("overallStDevs", C_PARAM_USER_DEFINED)

                .recreateDataFrame()
                .createStageWiseData()
            }

            if (sum(stats::na.omit(sampleSizes) < 0) > 0) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all sample sizes must be >= 0")
            }
            if (sum(stats::na.omit(stDevs) < 0) > 0) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all standard deviations must be >= 0")
            }
        },
        .recreateDataFrame = function() {
            callSuper()
            .data <<- cbind(.data, data.frame(
                sampleSize = sampleSizes,
                mean = means,
                stDev = stDevs,
                overallSampleSize = overallSampleSizes,
                overallMean = overallMeans,
                overallStDev = overallStDevs
            ))
            .orderDataByStageAndGroup()
            .setDataToVariables()
        },
        .setDataToVariables = function() {
            callSuper()
            sampleSizes <<- .data$sampleSize
            means <<- .data$mean
            stDevs <<- .data$stDev
            overallSampleSizes <<- .data$overallSampleSize
            overallMeans <<- .data$overallMean
            overallStDevs <<- .data$overallStDev
        },
        .fillWithNAs = function(kMax) {
            callSuper(kMax)
            n <- .getNumberOfNAsToAdd(kMax)

            naRealsToAdd <- rep(NA_real_, n)

            sampleSizes <<- c(sampleSizes, naRealsToAdd)
            means <<- c(means, naRealsToAdd)
            stDevs <<- c(stDevs, naRealsToAdd)

            overallSampleSizes <<- c(overallSampleSizes, naRealsToAdd)
            overallMeans <<- c(overallMeans, naRealsToAdd)
            overallStDevs <<- c(overallStDevs, naRealsToAdd)

            .recreateDataFrame()
        },
        .trim = function(kMax = NA_integer_) {
            indices <- callSuper(kMax)
            if (length(indices) == 0) {
                return(invisible(FALSE))
            }

            sampleSizes <<- sampleSizes[indices]
            means <<- means[indices]
            stDevs <<- stDevs[indices]

            overallSampleSizes <<- overallSampleSizes[indices]
            overallMeans <<- overallMeans[indices]
            overallStDevs <<- overallStDevs[indices]

            .recreateDataFrame()
            return(invisible(TRUE))
        },
        .getOverallMeans = function(sampleSizes, means) {
            return(cumsum(sampleSizes * means) / cumsum(sampleSizes))
        },
        .getOverallStDevs = function(sampleSizes, means, stDevs, overallMeans) {
            kMax <- length(sampleSizes)
            overallStDev <- rep(NA_real_, kMax)
            for (k in 1:kMax) {
                overallStDev[k] <- sqrt((sum((sampleSizes[1:k] - 1) * stDevs[1:k]^2) +
                    sum(sampleSizes[1:k] * (means[1:k] - overallMeans[k])^2)) /
                    (sum(sampleSizes[1:k]) - 1))
            }
            return(overallStDev)
        },
        .createOverallData = function() {
            .data$overallSampleSize <<- rep(NA_real_, nrow(.data))
            .data$overallMean <<- rep(NA_real_, nrow(.data))
            .data$overallStDev <<- rep(NA_real_, nrow(.data))
            subsetLevels <- NA_character_
            if (.enrichmentEnabled) {
                subsetLevels <- levels(.data$subset)
            }
            for (s in subsetLevels) {
                for (g in levels(.data$group)) {
                    if (!is.na(s)) {
                        indices <- which(.data$subset == s & .data$group == g)
                    } else {
                        indices <- which(.data$group == g)
                    }
                    .data$overallSampleSize[indices] <<- cumsum(.data$sampleSize[indices])
                    .data$overallMean[indices] <<- .getOverallMeans(
                        .data$sampleSize[indices], .data$mean[indices]
                    )
                    .data$overallStDev[indices] <<- .getOverallStDevs(
                        .data$sampleSize[indices],
                        .data$mean[indices], .data$stDev[indices], .data$overallMean[indices]
                    )
                }
            }
            .setDataToVariables()
        },
        .getStageWiseSampleSizes = function(overallSampleSizes) {
            result <- overallSampleSizes
            if (length(overallSampleSizes) == 1) {
                return(result)
            }

            kMax <- length(overallSampleSizes)
            result[2:kMax] <- overallSampleSizes[2:kMax] - overallSampleSizes[1:(kMax - 1)]
            return(result)
        },
        .getStageWiseMeans = function(sampleSizes, overallSampleSizes, overallMeans) {
            result <- overallMeans
            if (length(overallMeans) == 1) {
                return(result)
            }

            for (k in 2:length(overallMeans)) {
                result[k] <- (overallSampleSizes[k] * overallMeans[k] -
                    overallSampleSizes[k - 1] * overallMeans[k - 1]) / sampleSizes[k]
            }
            return(result)
        },
        .getStageWiseStDev = function(overallStDevs, sampleSizes, overallSampleSizes, means, overallMeans, k) {
            numBeforeK <- (overallSampleSizes[k - 1] - 1) * overallStDevs[k - 1]^2
            numK <- (overallSampleSizes[k] - 1) * overallStDevs[k]^2
            numSumBeforeK <- sum(sampleSizes[1:(k - 1)] * (means[1:(k - 1)] - overallMeans[k - 1])^2)
            numSumK <- sum(sampleSizes[1:k] * (means[1:k] - overallMeans[k])^2)
            denom <- (sampleSizes[k] - 1)
            value <- (numK - numBeforeK + numSumBeforeK - numSumK) / denom
            if (is.null(value) || length(value) != 1 || is.na(value) || value < 0) {
                warning("No calculation of stage-wise standard deviation from ",
                    "overall standard deviations possible at stage ", k,
                    call. = FALSE
                )
                return(NA_real_)
            }

            return(sqrt(value))
        },
        .getStageWiseStDevs = function(overallStDevs, sampleSizes, overallSampleSizes, means, overallMeans) {
            result <- overallStDevs
            if (length(overallStDevs) == 1) {
                return(result)
            }

            for (k in 2:length(overallStDevs)) {
                result[k] <- .getStageWiseStDev(overallStDevs, sampleSizes, overallSampleSizes, means, overallMeans, k)
            }
            return(result)
        },
        .createStageWiseData = function() {
            "Calculates stage-wise means and standard deviation if cunulative data is available"

            .data$sampleSize <<- rep(NA_real_, nrow(.data))
            .data$mean <<- rep(NA_real_, nrow(.data))
            .data$stDev <<- rep(NA_real_, nrow(.data))

            subsetLevels <- NA_character_
            if (.enrichmentEnabled) {
                subsetLevels <- levels(.data$subset)
            }

            for (s in subsetLevels) {
                for (g in levels(.data$group)) {
                    if (!is.na(s)) {
                        indices <- which(.data$subset == s & .data$group == g)
                    } else {
                        indices <- which(.data$group == g)
                    }

                    .assertValuesAreStrictlyIncreasing(.data$overallSampleSize[indices],
                        paste0("overallSampleSizes", g),
                        endingNasAllowed = TRUE
                    )

                    .data$sampleSize[indices] <<- .getStageWiseSampleSizes(.data$overallSampleSize[indices])
                    .data$mean[indices] <<- .getStageWiseMeans(
                        .data$sampleSize[indices],
                        .data$overallSampleSize[indices], .data$overallMean[indices]
                    )
                    .data$stDev[indices] <<- .getStageWiseStDevs(
                        .data$overallStDev[indices], .data$sampleSize[indices],
                        .data$overallSampleSize[indices], .data$mean[indices], .data$overallMean[indices]
                    )
                }
            }
            .setDataToVariables()
        },
        getRandomData = function() {
            return(.getRandomDataMeans(.self))
        }
    )
)

#'
#' @title
#' Dataset Plotting
#'
#' @description
#' Plots a dataset.
#'
#' @param x The \code{\link{Dataset}} object to plot.
#' @param y Not available for this kind of plot (is only defined to be compatible
#'        to the generic plot function).
#' @param main The main title, default is \code{"Dataset"}.
#' @param xlab The x-axis label, default is \code{"Stage"}.
#' @param ylab The y-axis label.
#' @param legendTitle The legend title, default is \code{"Group"}.
#' @inheritParams param_palette
#' @inheritParams param_showSource
#' @inheritParams param_plotSettings
#' @inheritParams param_three_dots_plot
#'
#' @details
#' Generic function to plot all kinds of datasets.
#'
#' @template return_object_ggplot
#'
#' @examples
#' # Plot a dataset of means
#' dataExample <- getDataset(
#'     n1 = c(22, 11, 22, 11),
#'     n2 = c(22, 13, 22, 13),
#'     means1 = c(1, 1.1, 1, 1),
#'     means2 = c(1.4, 1.5, 3, 2.5),
#'     stDevs1 = c(1, 2, 2, 1.3),
#'     stDevs2 = c(1, 2, 2, 1.3)
#' )
#' \dontrun{
#' if (require(ggplot2)) plot(dataExample, main = "Comparison of Means")
#' }
#'
#' # Plot a dataset of rates
#' dataExample <- getDataset(
#'     n1 = c(8, 10, 9, 11),
#'     n2 = c(11, 13, 12, 13),
#'     events1 = c(3, 5, 5, 6),
#'     events2 = c(8, 10, 12, 12)
#' )
#' \dontrun{
#' if (require(ggplot2)) plot(dataExample, main = "Comparison of Rates")
#' }
#'
#' @export
#'
plot.Dataset <- function(x, y, ..., main = "Dataset", xlab = "Stage", ylab = NA_character_,
        legendTitle = "Group", palette = "Set1", showSource = FALSE, plotSettings = NULL) {
    if (x$.enrichmentEnabled) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "plot of enrichment data is not implemented yet")
    }

    .assertGgplotIsInstalled()

    if (x$isDatasetMeans()) {
        data <- x$getRandomData()
        if (is.na(ylab)) {
            ylab <- "Random data"
        }
    } else if (x$isDatasetRates()) {
        data <- x$.data
        if (is.na(ylab)) {
            ylab <- "Frequency (Events and Sample Size)"
        }
    } else if (x$isDatasetSurvival()) {
        # Open work: implement dataset plot of survival data
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "plot of survival data is not implemented yet")
    }

    if (!is.logical(showSource) || isTRUE(showSource)) {
        warning("'showSource' != FALSE is not implemented yet for class ", .getClassName(x))
    }

    if (is.null(plotSettings)) {
        plotSettings <- x$getPlotSettings()
    }

    if (x$getNumberOfGroups() == 1) {
        if (x$isDatasetMeans()) {
            p <- ggplot2::ggplot(
                data = data,
                ggplot2::aes(y = .data[["randomData"]], x = factor(.data[["stage"]]))
            )
            p <- p + ggplot2::geom_boxplot(ggplot2::aes(fill = .data[["stage"]]))
            p <- p + ggplot2::geom_point(
                colour = "#0e414e", shape = 20,
                position = ggplot2::position_jitter(width = .1),
                size = plotSettings$pointSize
            )
            p <- p + ggplot2::stat_summary(
                fun = "mean", geom = "point",
                shape = 21, position = ggplot2::position_dodge(.75), size = 4, fill = "white",
                colour = "black", show.legend = FALSE
            )
        } else if (x$isDatasetRates()) {
            p <- ggplot2::ggplot(show.legend = FALSE)

            # plot sample size
            p <- p + ggplot2::geom_bar(
                data = data,
                ggplot2::aes(
                    y = .data[["sampleSize"]],
                    x = factor(.data[["stage"]]), fill = factor(.data[["stage"]])
                ),
                position = "dodge", stat = "identity", alpha = 0.4
            )

            # plot events
            p <- p + ggplot2::geom_bar(
                data = data,
                ggplot2::aes(
                    y = .data[["event"]], x = factor(.data[["stage"]]),
                    fill = factor(.data[["stage"]])
                ),
                position = "dodge", stat = "identity"
            )
        } else if (x$isDatasetSurvival()) {
            # implement survival plot here
        }
    } else {
        data$stageGroup <- interaction(data$stage, data$group)

        if (x$isDatasetMeans()) {
            p <- ggplot2::ggplot(ggplot2::aes(
                y = .data[["randomData"]], x = factor(.data[["stage"]]),
                fill = factor(.data[["group"]])
            ), data = data)
            p <- p + ggplot2::geom_point(ggplot2::aes(colour = .data[["group"]]),
                shape = 20,
                position = ggplot2::position_dodge(.75),
                size = plotSettings$pointSize
            )
            p <- p + ggplot2::geom_boxplot()
            p <- p + ggplot2::stat_summary(ggplot2::aes(colour = .data[["group"]]),
                fun = "mean", geom = "point",
                shape = 21, position = ggplot2::position_dodge(.75), size = 4, fill = "white",
                show.legend = FALSE
            )
        } else if (x$isDatasetRates()) {
            p <- ggplot2::ggplot(show.legend = FALSE)

            # plot sample size
            p <- p + ggplot2::geom_bar(
                ggplot2::aes(
                    y = .data[["sampleSize"]],
                    x = factor(.data[["stage"]]), fill = factor(.data[["group"]])
                ),
                data = data, position = "dodge", stat = "identity", alpha = 0.4
            )

            # plot events
            p <- p + ggplot2::geom_bar(
                data = data,
                ggplot2::aes(
                    y = .data[["event"]], x = factor(.data[["stage"]]),
                    fill = factor(.data[["group"]])
                ),
                position = "dodge", stat = "identity"
            )
        } else if (x$isDatasetSurvival()) {
            # implement survival plot here
        }
    }

    # hide second legend
    if (x$getNumberOfGroups() == 1) {
        p <- p + ggplot2::guides(fill = FALSE, colour = FALSE)
    } else {
        p <- p + ggplot2::guides(colour = FALSE)
    }

    # set theme
    p <- plotSettings$setTheme(p)
    # p <- designSet$getPlotSettings()$hideGridLines(p)

    # set main title
    p <- plotSettings$setMainTitle(p, main)

    # set axes labels
    p <- plotSettings$setAxesLabels(p, xlab = xlab, ylab = ylab)

    # set legend
    if (x$getNumberOfGroups() > 1) {
        p <- plotSettings$setLegendPosition(p, legendPosition = C_POSITION_OUTSIDE_PLOT)
        p <- plotSettings$setLegendBorder(p)
        p <- plotSettings$setLegendTitle(p, legendTitle, mode = "fill")
        p <- plotSettings$setLegendLabelSize(p)
    }

    p <- plotSettings$setAxesAppearance(p)
    p <- plotSettings$setColorPalette(p, palette, mode = "all")
    p <- plotSettings$enlargeAxisTicks(p)

    companyAnnotationEnabled <- .getOptionalArgument("companyAnnotationEnabled", ...)
    if (is.null(companyAnnotationEnabled) || !is.logical(companyAnnotationEnabled)) {
        companyAnnotationEnabled <- FALSE
    }
    p <- plotSettings$addCompanyAnnotation(p, enabled = companyAnnotationEnabled)

    p
}

#'
#' @name DatasetRates
#'
#' @title
#' Dataset of Rates
#'
#' @description
#' Class for a dataset of rates.
#'
#' @template field_groups
#' @template field_stages
#' @template field_sampleSizes
#' @template field_overallSampleSizes
#' @template field_events
#' @template field_overallEvents
#'
#' @details
#' This object cannot be created directly; better use \code{\link{getDataset}}
#' with suitable arguments to create a dataset of rates.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
DatasetRates <- setRefClass("DatasetRates",
    contains = "Dataset",
    fields = list(
        sampleSizes = "numeric",
        events = "numeric",
        overallSampleSizes = "numeric",
        overallEvents = "numeric"
    ),
    methods = list(
        getSampleSize = function(stage, group = 1, subset = NA_character_) {
            return(.data$sampleSize[.getIndices(stage = stage, group = group, subset = subset)])
        },
        getSampleSizes = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(.data$sampleSize[.getIndices(stage = .getValidatedStage(stage), group = group, subset = subset)])
        },
        getSampleSizesUpTo = function(to, group = 1, subset = NA_character_) {
            return(.data$sampleSize[.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getEvent = function(stage, group = 1, subset = NA_character_) {
            return(.data$event[.getIndices(stage = stage, group = group, subset = subset)])
        },
        getEvents = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(.data$event[.getIndices(stage = .getValidatedStage(stage), group = group, subset = subset)])
        },
        getEventsUpTo = function(to, group = 1, subset = NA_character_) {
            return(.data$event[.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getOverallSampleSize = function(stage, group = 1, subset = NA_character_) {
            return(.data$overallSampleSize[.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallSampleSizes = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(.data$overallSampleSize[.getIndices(stage = .getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallSampleSizesUpTo = function(to, group = 1, subset = NA_character_) {
            return(.data$overallSampleSize[.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getOverallEvent = function(stage, group = 1, subset = NA_character_) {
            return(.data$overallEvent[.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallEvents = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(.data$overallEvent[.getIndices(stage = .getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallEventsUpTo = function(to, group = 1, subset = NA_character_) {
            return(.data$overallEvent[.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        .initByDataFrame = function(dataFrame) {
            callSuper(dataFrame)

            # case: one rate - stage wise
            if (.paramExists(dataFrame, C_KEY_WORDS_SAMPLE_SIZES)) {
                .inputType <<- "stagewise"

                sampleSizes <<- .getValidatedFloatingPointNumbers(
                    .getValuesByParameterName(dataFrame, C_KEY_WORDS_SAMPLE_SIZES),
                    parameterName = "Sample sizes"
                )
                .validateValues(sampleSizes, "n")
                if (any(stats::na.omit(sampleSizes) <= 0)) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "all sample sizes must be > 0, but 'n' = ",
                        .arrayToString(sampleSizes, vectorLookAndFeelEnabled = TRUE)
                    )
                }

                events <<- .getValidatedFloatingPointNumbers(
                    .getValuesByParameterName(dataFrame, C_KEY_WORDS_EVENTS),
                    parameterName = "Events"
                )
                .validateValues(events, "events")
                if (any(stats::na.omit(events) < 0)) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all events must be >= 0, but 'events' = ",
                        .arrayToString(events, vectorLookAndFeelEnabled = TRUE)
                    )
                }

                kMax <- length(sampleSizes)
                stageNumber <- length(stats::na.omit(sampleSizes))
                dataInput <- data.frame(
                    sampleSizes = sampleSizes,
                    events = events
                )
                dataInput <- .getOverallData(dataInput, kMax, stage = stageNumber)
                overallSampleSizes <<- dataInput$overallSampleSizes
                overallEvents <<- dataInput$overallEvents

                .setParameterType("sampleSizes", C_PARAM_USER_DEFINED)
                .setParameterType("events", C_PARAM_USER_DEFINED)

                .setParameterType("overallSampleSizes", C_PARAM_GENERATED)
                .setParameterType("overallEvents", C_PARAM_GENERATED)
            }

            # case: one rate - overall
            else if (.paramExists(dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES)) {
                .inputType <<- "overall"
                overallSampleSizes <<- .getValidatedFloatingPointNumbers(
                    .getValuesByParameterName(
                        dataFrame,
                        C_KEY_WORDS_OVERALL_SAMPLE_SIZES
                    ),
                    parameterName = "Cumulative sample sizes"
                )
                .validateValues(overallSampleSizes, "overallSampleSizes")
                .assertValuesAreStrictlyIncreasing(overallSampleSizes, "overallSampleSizes", endingNasAllowed = TRUE)

                overallEvents <<- .getValidatedFloatingPointNumbers(
                    .getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_EVENTS),
                    parameterName = "Cumulative events"
                )
                .validateValues(overallEvents, "overallEvents")
                .assertValuesAreMonotoneIncreasing(overallEvents, "overallEvents", endingNasAllowed = TRUE)

                kMax <- length(overallSampleSizes)
                stageNumber <- length(stats::na.omit(overallSampleSizes))
                stageWiseData <- .getStageWiseData(data.frame(
                    overallSampleSizes = overallSampleSizes,
                    overallEvents = overallEvents
                ), kMax, stage = stageNumber)
                sampleSizes <<- stageWiseData$sampleSizes
                events <<- stageWiseData$events

                .setParameterType("sampleSizes", C_PARAM_GENERATED)
                .setParameterType("events", C_PARAM_GENERATED)

                .setParameterType("overallSampleSizes", C_PARAM_USER_DEFINED)
                .setParameterType("overallEvents", C_PARAM_USER_DEFINED)
            }

            # case: two or more rates - stage wise
            else if (.paramExists(dataFrame, paste0(C_KEY_WORDS_SAMPLE_SIZES, 1)) &&
                    .paramExists(dataFrame, paste0(C_KEY_WORDS_SAMPLE_SIZES, 2))) {
                .inputType <<- "stagewise"

                numberOfTreatmentGroups <- .getNumberOfGroups(dataFrame, C_KEY_WORDS_SAMPLE_SIZES)

                stages <<- rep(stages, numberOfTreatmentGroups)

                groups <<- integer(0)
                sampleSizes <<- numeric(0)
                events <<- numeric(0)
                overallSampleSizes <<- numeric(0)
                overallEvents <<- numeric(0)
                for (group in 1:numberOfTreatmentGroups) {
                    sampleSizesTemp <- .getValidatedFloatingPointNumbers(
                        .getValuesByParameterName(
                            dataFrame, C_KEY_WORDS_SAMPLE_SIZES,
                            suffix = group
                        ),
                        parameterName = "Sample sizes"
                    )
                    .validateValues(sampleSizesTemp, paste0("n", group))
                    if (any(stats::na.omit(sampleSizesTemp) <= 0)) {
                        stop(
                            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                            "all sample sizes must be > 0, but 'n", group, "' = ",
                            .arrayToString(sampleSizesTemp, vectorLookAndFeelEnabled = TRUE)
                        )
                    }
                    sampleSizes <<- c(sampleSizes, sampleSizesTemp)

                    eventsTemp <- .getValidatedFloatingPointNumbers(
                        .getValuesByParameterName(dataFrame, C_KEY_WORDS_EVENTS, suffix = group),
                        parameterName = "Events"
                    )
                    .validateValues(eventsTemp, paste0("events", group))
                    if (any(stats::na.omit(eventsTemp) < 0)) {
                        stop(
                            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all events must be >= 0, but 'events", group, "' = ",
                            .arrayToString(eventsTemp, vectorLookAndFeelEnabled = TRUE)
                        )
                    }
                    events <<- c(events, eventsTemp)

                    groups <<- c(groups, rep(as.integer(group), length(sampleSizesTemp)))

                    kMax <- length(sampleSizesTemp)
                    numberOfValidStages <- length(stats::na.omit(sampleSizesTemp))
                    overallData <- .getOverallData(data.frame(
                        sampleSizes = sampleSizesTemp,
                        events = eventsTemp
                    ), kMax, stage = numberOfValidStages)

                    overallSampleSizes <<- c(overallSampleSizes, overallData$overallSampleSizes)
                    overallEvents <<- c(overallEvents, overallData$overallEvents)
                }
                if (sum(stats::na.omit(sampleSizes) < 0) > 0) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all sample sizes must be >= 0")
                }

                .setParameterType("sampleSizes", C_PARAM_USER_DEFINED)
                .setParameterType("events", C_PARAM_USER_DEFINED)

                .setParameterType("overallSampleSizes", C_PARAM_GENERATED)
                .setParameterType("overallEvents", C_PARAM_GENERATED)
            }

            # case: two or more rates - overall
            else if (.paramExists(dataFrame, paste0(C_KEY_WORDS_OVERALL_SAMPLE_SIZES, 1)) &&
                    .paramExists(dataFrame, paste0(C_KEY_WORDS_OVERALL_SAMPLE_SIZES, 2))) {
                .inputType <<- "overall"

                numberOfTreatmentGroups <- .getNumberOfGroups(dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES)

                stages <<- rep(stages, numberOfTreatmentGroups)

                groups <<- integer(0)
                sampleSizes <<- numeric(0)
                events <<- numeric(0)
                overallSampleSizes <<- numeric(0)
                overallEvents <<- numeric(0)
                for (group in 1:numberOfTreatmentGroups) {
                    overallSampleSizesTemp <- .getValidatedFloatingPointNumbers(
                        .getValuesByParameterName(
                            dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES,
                            suffix = group
                        ),
                        parameterName = "Cumulative sample sizes"
                    )
                    .validateValues(overallSampleSizesTemp, paste0("overallSampleSizes", group))
                    .assertValuesAreStrictlyIncreasing(overallSampleSizesTemp,
                        paste0("overallSampleSizes", group),
                        endingNasAllowed = TRUE
                    )
                    overallSampleSizes <<- c(overallSampleSizes, overallSampleSizesTemp)

                    overallEventsTemp <- .getValidatedFloatingPointNumbers(
                        .getValuesByParameterName(dataFrame,
                            C_KEY_WORDS_OVERALL_EVENTS,
                            suffix = group
                        ),
                        parameterName = "Cumulative events"
                    )
                    .validateValues(overallEventsTemp, paste0("overallEvents", group))
                    .assertValuesAreMonotoneIncreasing(overallEventsTemp,
                        paste0("overallEvents", group),
                        endingNasAllowed = TRUE
                    )
                    overallEvents <<- c(overallEvents, overallEventsTemp)

                    groups <<- c(groups, rep(as.integer(group), length(overallSampleSizesTemp)))

                    kMax <- length(overallSampleSizesTemp)
                    numberOfValidStages <- length(stats::na.omit(overallSampleSizesTemp))
                    stageWiseData <- .getStageWiseData(data.frame(
                        overallSampleSizes = overallSampleSizesTemp,
                        overallEvents = overallEventsTemp
                    ), kMax, stage = numberOfValidStages)

                    validatedSampleSizes <- stageWiseData$sampleSizes
                    .validateValues(validatedSampleSizes, paste0("n", group))
                    sampleSizes <<- c(sampleSizes, validatedSampleSizes)
                    events <<- c(events, stageWiseData$events)

                    if (sum(stats::na.omit(sampleSizes) < 0) > 0) {
                        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all sample sizes must be >= 0")
                    }
                }

                .setParameterType("sampleSizes", C_PARAM_GENERATED)
                .setParameterType("events", C_PARAM_GENERATED)

                .setParameterType("overallSampleSizes", C_PARAM_USER_DEFINED)
                .setParameterType("overallEvents", C_PARAM_USER_DEFINED)
            } else {
                stop(
                    C_EXCEPTION_TYPE_MISSING_ARGUMENT,
                    "sample sizes are missing or not correctly specified"
                )
            }

            if (sum(stats::na.omit(events) < 0) > 0) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all events must be >= 0")
            }

            .recreateDataFrame()
            if (.enrichmentEnabled) {
                .createOverallDataEnrichment()
            }
        },
        .recreateDataFrame = function() {
            callSuper()
            .data <<- cbind(.data, data.frame(
                sampleSize = sampleSizes,
                event = events,
                overallSampleSize = overallSampleSizes,
                overallEvent = overallEvents
            ))
            .orderDataByStageAndGroup()
            .setDataToVariables()
        },
        .setDataToVariables = function() {
            callSuper()
            sampleSizes <<- .data$sampleSize
            events <<- .data$event
            overallSampleSizes <<- .data$overallSampleSize
            overallEvents <<- .data$overallEvent
        },
        .fillWithNAs = function(kMax) {
            callSuper(kMax)
            n <- .getNumberOfNAsToAdd(kMax)

            sampleSizes <<- c(sampleSizes, rep(NA_real_, n))
            events <<- c(events, rep(NA_real_, n))

            overallSampleSizes <<- c(overallSampleSizes, rep(NA_real_, n))
            overallEvents <<- c(overallEvents, rep(NA_real_, n))

            .recreateDataFrame()
        },
        .trim = function(kMax = NA_integer_) {
            indices <- callSuper(kMax)
            if (length(indices) == 0) {
                return(invisible(FALSE))
            }

            sampleSizes <<- sampleSizes[indices]
            events <<- events[indices]

            overallSampleSizes <<- overallSampleSizes[indices]
            overallEvents <<- overallEvents[indices]

            .recreateDataFrame()

            return(invisible(TRUE))
        },
        getRandomData = function() {
            data <- NULL
            for (stage in 1:getNumberOfStages()) {
                for (group in 1:getNumberOfGroups()) {
                    if (.enrichmentEnabled) {
                        for (subset in levels(.data$subset)) {
                            n <- getSampleSize(stage = stage, group = group, subset = subset)
                            numberOfEvents <- getEvent(stage = stage, group = group, subset = subset)
                            randomIndizes <- sample(x = c(1:n), size = numberOfEvents, replace = FALSE)
                            randomData <- rep(0, n)
                            randomData[randomIndizes] <- 1

                            row <- data.frame(
                                stage = stage,
                                group = group,
                                subset = subset,
                                randomData = randomData
                            )
                            if (is.null(data)) {
                                data <- row
                            } else {
                                data <- rbind(data, row)
                            }
                        }
                    } else {
                        n <- getSampleSize(stage = stage, group = group)
                        numberOfEvents <- getEvent(stage = stage, group = group)
                        randomIndizes <- sample(x = c(1:n), size = numberOfEvents, replace = FALSE)
                        randomData <- rep(0, n)
                        randomData[randomIndizes] <- 1

                        row <- data.frame(
                            stage = stage,
                            group = group,
                            randomData = randomData
                        )
                        if (is.null(data)) {
                            data <- row
                        } else {
                            data <- rbind(data, row)
                        }
                    }
                }
            }
            data$stage <- factor(data$stage)
            data$group <- factor(data$group, label = paste("Group", c(1:getNumberOfGroups())))
            return(data)
        },
        .createOverallDataEnrichment = function() {
            if (!.enrichmentEnabled) {
                return(invisible())
            }

            .data$overallSampleSize <<- rep(NA_real_, nrow(.data))
            .data$overallEvent <<- rep(NA_real_, nrow(.data))
            for (s in levels(.data$subset)) {
                for (g in levels(.data$group)) {
                    indices <- which(.data$subset == s & .data$group == g)
                    .data$overallSampleSize[indices] <<- cumsum(.data$sampleSize[indices])
                    .data$overallEvent[indices] <<- cumsum(.data$event[indices])
                }
            }

            .setDataToVariables()
        },
        .getOverallData = function(dataInput, kMax, stage) {
            "Calculates cumulative values if stage-wise data is available"
            if (is.null(dataInput[["sampleSizes"]])) {
                stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data input must contain variable 'sampleSizes'")
            }
            if (is.null(dataInput[["events"]])) {
                stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data input must contain variable 'events'")
            }

            dataInput$overallSampleSizes <- c(
                cumsum(dataInput$sampleSizes[1:stage]),
                rep(NA_real_, kMax - stage)
            )

            dataInput$overallEvents <- c(
                cumsum(dataInput$events[1:stage]),
                rep(NA_real_, kMax - stage)
            )

            return(dataInput)
        },
        .getStageWiseData = function(dataInput, kMax, stage) {
            "Calculates stage-wise values if cumulative data is available"
            if (is.null(dataInput[["overallSampleSizes"]])) {
                stop(
                    C_EXCEPTION_TYPE_MISSING_ARGUMENT,
                    "data input must contain variable 'overallSampleSizes'"
                )
            }
            if (is.null(dataInput[["overallEvents"]])) {
                stop(
                    C_EXCEPTION_TYPE_MISSING_ARGUMENT,
                    "data input must contain variable 'overallEvents'"
                )
            }

            dataInput$sampleSizes <- c(dataInput$overallSampleSizes[1:stage], rep(NA_real_, kMax - stage))
            if (stage > 1) {
                dataInput$sampleSizes[2:stage] <- dataInput$overallSampleSizes[2:stage] -
                    dataInput$overallSampleSizes[1:(stage - 1)]
            }

            dataInput$events <- c(dataInput$overallEvents[1:stage], rep(NA_real_, kMax - stage))
            if (stage > 1) {
                dataInput$events[2:stage] <- dataInput$overallEvents[2:stage] -
                    dataInput$overallEvents[1:(stage - 1)]
            }

            return(dataInput)
        }
    )
)

#'
#' @name DatasetSurvival
#'
#' @title
#' Dataset of Survival Data
#'
#' @description
#' Class for a dataset of survival data.
#'
#' @template field_groups
#' @template field_stages
#' @template field_events
#' @template field_overallEvents
#' @template field_allocationRatios
#' @template field_overallAllocationRatios
#' @template field_logRanks
#' @template field_overallLogRanks
#'
#'
#' @details
#' This object cannot be created directly; better use \code{\link{getDataset}}
#' with suitable arguments to create a dataset of survival data.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include f_core_constants.R
#'
#' @keywords internal
#'
#' @importFrom methods new
#'
DatasetSurvival <- setRefClass("DatasetSurvival",
    contains = "Dataset",
    fields = list(
        overallEvents = "numeric",
        overallAllocationRatios = "numeric",
        overallLogRanks = "numeric",
        events = "numeric",
        allocationRatios = "numeric",
        logRanks = "numeric"
    ),
    methods = list(
        getEvent = function(stage, group = 1, subset = NA_character_) {
            return(.data$event[.getIndices(stage = stage, group = group, subset = subset)])
        },
        getEvents = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(.data$event[.getIndices(stage = .getValidatedStage(stage), group = group, subset = subset)])
        },
        getEventsUpTo = function(to, group = 1, subset = NA_character_) {
            return(.data$event[.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getAllocationRatio = function(stage, group = 1, subset = NA_character_) {
            return(.data$allocationRatio[.getIndices(stage = stage, group = group, subset = subset)])
        },
        getAllocationRatios = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(.data$allocationRatio[.getIndices(stage = .getValidatedStage(stage), group = group, subset = subset)])
        },
        getAllocationRatiosUpTo = function(to, group = 1, subset = NA_character_) {
            return(.data$allocationRatio[.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getLogRank = function(stage, group = 1, subset = NA_character_) {
            return(.data$logRank[.getIndices(stage = stage, group = group, subset = subset)])
        },
        getLogRanks = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(.data$logRank[.getIndices(stage = .getValidatedStage(stage), group = group, subset = subset)])
        },
        getLogRanksUpTo = function(to, group = 1, subset = NA_character_) {
            return(.data$logRank[.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getOverallEvent = function(stage, group = 1, subset = NA_character_) {
            return(.data$overallEvent[.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallEvents = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(.data$overallEvent[.getIndices(stage = .getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallEventsUpTo = function(to, group = 1, subset = NA_character_) {
            return(.data$overallEvent[.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getOverallAllocationRatio = function(stage, group = 1, subset = NA_character_) {
            return(.data$overallAllocationRatio[.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallAllocationRatios = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(.data$overallAllocationRatio[.getIndices(stage = .getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallAllocationRatiosUpTo = function(to, group = 1, subset = NA_character_) {
            return(.data$overallAllocationRatio[.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getOverallLogRank = function(stage, group = 1, subset = NA_character_) {
            return(.data$overallLogRank[.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallLogRanks = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(.data$overallLogRank[.getIndices(stage = .getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallLogRanksUpTo = function(to, group = 1, subset = NA_character_) {
            return(.data$overallLogRank[.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        .getAllocationRatioDefaultValues = function(stages, events, logRanks) {
            allocationRatioDefaultValues <- rep(C_ALLOCATION_RATIO_DEFAULT, length(stages))
            indices <- which(is.na(events) | is.na(logRanks))
            allocationRatioDefaultValues[indices] <- NA_real_
            return(allocationRatioDefaultValues)
        },
        .initByDataFrame = function(dataFrame) {
            callSuper(dataFrame)

            if (inherits(.self, "DatasetEnrichmentSurvival")) {
                if (.paramExists(dataFrame, C_KEY_WORDS_EXPECTED_EVENTS) ||
                        .paramExists(dataFrame, C_KEY_WORDS_VARIANCE_EVENTS)) {
                    .inputType <<- "stagewise"

                    events <<- .getValidatedFloatingPointNumbers(
                        .getValuesByParameterName(dataFrame, C_KEY_WORDS_EVENTS),
                        parameterName = "Events"
                    )
                    .validateValues(events, "events")

                    allocationRatios <<- .getValuesByParameterName(
                        dataFrame, C_KEY_WORDS_ALLOCATION_RATIOS,
                        defaultValues = .getAllocationRatioDefaultValues(stages, events, expectedEvents)
                    )
                    .validateValues(allocationRatios, "allocationRatios")
                } else if (.paramExists(dataFrame, C_KEY_WORDS_OVERALL_EXPECTED_EVENTS) ||
                        .paramExists(dataFrame, C_KEY_WORDS_OVERALL_VARIANCE_EVENTS)) {
                    .inputType <<- "overall"

                    overallEvents <<- .getValidatedFloatingPointNumbers(
                        .getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_EVENTS),
                        parameterName = "Cumulative events"
                    )
                    .validateValues(overallEvents, "overallEvents")

                    overallAllocationRatios <<- .getValuesByParameterName(
                        dataFrame,
                        parameterNameVariants = C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS,
                        defaultValues = .getAllocationRatioDefaultValues(stages, overallEvents, overallExpectedEvents)
                    )
                    .validateValues(overallAllocationRatios, "overallAllocationRatios")
                }

                # stratified enrichment: do nothing more here
            }

            # case: survival, two groups - overall
            else if (.paramExists(dataFrame, C_KEY_WORDS_OVERALL_LOG_RANKS)) {
                .inputType <<- "overall"
                overallEvents <<- .getValidatedFloatingPointNumbers(
                    .getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_EVENTS),
                    parameterName = "Cumulative events"
                )
                .validateValues(overallEvents, "overallEvents")
                if (!.enrichmentEnabled) {
                    .assertValuesAreStrictlyIncreasing(overallEvents, "overallEvents", endingNasAllowed = TRUE)
                }

                overallLogRanks <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_LOG_RANKS)
                .validateValues(overallLogRanks, "overallLogRanks")

                overallAllocationRatios <<- .getValuesByParameterName(
                    dataFrame,
                    parameterNameVariants = C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS,
                    defaultValues = .getAllocationRatioDefaultValues(stages, overallEvents, overallLogRanks)
                )
                .validateValues(overallAllocationRatios, "overallAllocationRatios")

                .setParameterType("groups", C_PARAM_NOT_APPLICABLE)
            }

            # case: survival, two groups - stage wise
            else if (.paramExists(dataFrame, C_KEY_WORDS_LOG_RANKS)) {
                .inputType <<- "stagewise"
                events <<- .getValidatedFloatingPointNumbers(.getValuesByParameterName(
                    dataFrame, C_KEY_WORDS_EVENTS
                ), parameterName = "Events")
                .validateValues(events, "events")
                if (any(stats::na.omit(events) < 0)) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all events must be >= 0")
                }

                logRanks <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_LOG_RANKS)
                .validateValues(logRanks, "logRanks")

                allocationRatios <<- .getValuesByParameterName(
                    dataFrame, C_KEY_WORDS_ALLOCATION_RATIOS,
                    defaultValues = .getAllocationRatioDefaultValues(stages, events, logRanks)
                )
                .validateValues(allocationRatios, "allocationRatios")

                .setParameterType("groups", C_PARAM_NOT_APPLICABLE)
            }

            # case: survival, three ore more groups - overall
            else if (.paramExists(dataFrame, paste0(C_KEY_WORDS_OVERALL_LOG_RANKS, 1)) &&
                    .paramExists(dataFrame, paste0(C_KEY_WORDS_OVERALL_LOG_RANKS, 2))) {
                .inputType <<- "overall"

                numberOfTreatmentGroups <- .getNumberOfGroups(dataFrame, C_KEY_WORDS_OVERALL_LOG_RANKS)

                stages <<- rep(stages, numberOfTreatmentGroups)

                groups <<- integer(0)
                overallEvents <<- numeric(0)
                overallAllocationRatios <<- numeric(0)
                overallLogRanks <<- numeric(0)
                for (group in 1:numberOfTreatmentGroups) {
                    overallEventsTemp <- .getValuesByParameterName(dataFrame,
                        C_KEY_WORDS_OVERALL_EVENTS,
                        suffix = group
                    )
                    .validateValues(overallEventsTemp, paste0("overallEvents", group))
                    if (is.null(dataFrame[["subset"]]) || length(unique(dataFrame[["subset"]])) <= 1) {
                        .assertValuesAreStrictlyIncreasing(overallEventsTemp,
                            paste0("overallEvents", group),
                            endingNasAllowed = TRUE
                        )
                    }
                    overallEvents <<- c(overallEvents, overallEventsTemp)

                    overallLogRanksTemp <- .getValuesByParameterName(
                        dataFrame, C_KEY_WORDS_OVERALL_LOG_RANKS,
                        suffix = group
                    )
                    .validateValues(overallLogRanksTemp, paste0("overallLogRanks", group))
                    overallLogRanks <<- c(overallLogRanks, overallLogRanksTemp)

                    overallAllocationRatiosTemp <- .getValuesByParameterName(
                        dataFrame, C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS,
                        suffix = group,
                        defaultValues = .getAllocationRatioDefaultValues(
                            overallEventsTemp,
                            overallEventsTemp, overallLogRanksTemp
                        )
                    )
                    .validateValues(overallAllocationRatiosTemp, paste0("overallAllocationRatios", group))
                    overallAllocationRatios <<- c(overallAllocationRatios, overallAllocationRatiosTemp)

                    groups <<- c(groups, rep(as.integer(group), length(overallLogRanksTemp)))
                }
            }

            # case: survival, three ore more groups - stage wise
            else if (.paramExists(dataFrame, paste0(C_KEY_WORDS_LOG_RANKS, 1)) &&
                    .paramExists(dataFrame, paste0(C_KEY_WORDS_LOG_RANKS, 2))) {
                .inputType <<- "stagewise"
                numberOfTreatmentGroups <- .getNumberOfGroups(dataFrame, C_KEY_WORDS_LOG_RANKS)

                stages <<- rep(stages, numberOfTreatmentGroups)

                groups <<- integer(0)
                events <<- numeric(0)
                allocationRatios <<- numeric(0)
                logRanks <<- numeric(0)
                for (group in 1:numberOfTreatmentGroups) {
                    eventsTemp <- .getValidatedFloatingPointNumbers(.getValuesByParameterName(
                        dataFrame, C_KEY_WORDS_EVENTS,
                        suffix = group
                    ), parameterName = "Events")
                    if (any(stats::na.omit(eventsTemp) < 0)) {
                        stop(
                            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all events must be >= 0, but 'events", group, "' = ",
                            .arrayToString(eventsTemp, vectorLookAndFeelEnabled = TRUE)
                        )
                    }
                    events <<- c(events, eventsTemp)

                    logRanksTemp <- .getValuesByParameterName(
                        dataFrame, C_KEY_WORDS_LOG_RANKS,
                        suffix = group
                    )
                    .validateValues(logRanksTemp, paste0("n", group))
                    logRanks <<- c(logRanks, logRanksTemp)

                    allocationRatiosTemp <- .getValuesByParameterName(
                        dataFrame, C_KEY_WORDS_ALLOCATION_RATIOS,
                        suffix = group,
                        defaultValues = .getAllocationRatioDefaultValues(
                            eventsTemp,
                            eventsTemp, logRanksTemp
                        )
                    )
                    .validateValues(allocationRatiosTemp, paste0("allocationRatios", group))
                    allocationRatios <<- c(allocationRatios, allocationRatiosTemp)

                    groups <<- c(groups, rep(as.integer(group), length(eventsTemp)))
                }
            } else {
                stop(
                    C_EXCEPTION_TYPE_RUNTIME_ISSUE, "unable to identify case for ", .getClassName(.self), " and columns ",
                    .arrayToString(colnames(dataFrame))
                )
            }

            if (.inputType == "stagewise") {
                n <- length(events)
                overallEvents <<- rep(NA_real_, n)
                overallAllocationRatios <<- rep(NA_real_, n)
                overallLogRanks <<- rep(NA_real_, n)

                .setParameterType("events", C_PARAM_USER_DEFINED)
                .setParameterType("allocationRatios", C_PARAM_USER_DEFINED)
                if (!inherits(.self, "DatasetEnrichmentSurvival")) {
                    .setParameterType("logRanks", C_PARAM_USER_DEFINED)
                }

                .setParameterType("overallEvents", C_PARAM_GENERATED)
                .setParameterType("overallAllocationRatios", C_PARAM_GENERATED)
                if (!inherits(.self, "DatasetEnrichmentSurvival")) {
                    .setParameterType("overallLogRanks", C_PARAM_GENERATED)
                }

                if (!inherits(.self, "DatasetEnrichmentSurvival")) {
                    .recreateDataFrame()
                    .createOverallData()
                }
            } else {
                n <- length(overallEvents)
                events <<- rep(NA_real_, n)
                allocationRatios <<- rep(NA_real_, n)
                logRanks <<- rep(NA_real_, n)

                .setParameterType("events", C_PARAM_GENERATED)
                .setParameterType("allocationRatios", C_PARAM_GENERATED)
                if (!inherits(.self, "DatasetEnrichmentSurvival")) {
                    .setParameterType("logRanks", C_PARAM_GENERATED)
                }

                .setParameterType("overallEvents", C_PARAM_USER_DEFINED)
                .setParameterType("overallAllocationRatios", C_PARAM_USER_DEFINED)
                if (!inherits(.self, "DatasetEnrichmentSurvival")) {
                    .setParameterType("overallLogRanks", C_PARAM_USER_DEFINED)
                }

                if (!inherits(.self, "DatasetEnrichmentSurvival")) {
                    .recreateDataFrame()
                    .createStageWiseData()
                }
            }
        },
        .recreateDataFrame = function() {
            callSuper()

            if (inherits(.self, "DatasetEnrichmentSurvival")) {
                .data <<- cbind(.data, data.frame(
                    overallEvent = overallEvents,
                    overallExpectedEvent = overallExpectedEvents,
                    overallVarianceEvent = overallVarianceEvents,
                    overallAllocationRatio = overallAllocationRatios,
                    event = events,
                    expectedEvent = expectedEvents,
                    # varianceEvent = varianceEvents, # maybe implemented later
                    allocationRatio = allocationRatios
                ))
            } else {
                .data <<- cbind(.data, data.frame(
                    overallEvent = overallEvents,
                    overallAllocationRatio = overallAllocationRatios,
                    overallLogRank = overallLogRanks,
                    event = events,
                    allocationRatio = allocationRatios,
                    logRank = logRanks
                ))
            }
            .orderDataByStageAndGroup()
            .setDataToVariables()
        },
        .setDataToVariables = function() {
            callSuper()
            overallEvents <<- .data$overallEvent
            overallAllocationRatios <<- .data$overallAllocationRatio
            events <<- .data$event
            allocationRatios <<- .data$allocationRatio
            if (!inherits(.self, "DatasetEnrichmentSurvival")) {
                overallLogRanks <<- .data$overallLogRank
                logRanks <<- .data$logRank
            }
        },
        .fillWithNAs = function(kMax) {
            callSuper(kMax)
            n <- .getNumberOfNAsToAdd(kMax)

            overallEvents <<- c(overallEvents, rep(NA_real_, n))
            overallAllocationRatios <<- c(overallAllocationRatios, rep(NA_real_, n))
            overallLogRanks <<- c(overallLogRanks, rep(NA_real_, n))

            events <<- c(events, rep(NA_real_, n))
            allocationRatios <<- c(allocationRatios, rep(NA_real_, n))
            logRanks <<- c(logRanks, rep(NA_real_, n))

            .recreateDataFrame()
        },
        .trim = function(kMax = NA_integer_) {
            indices <- callSuper(kMax)
            if (length(indices) == 0) {
                return(invisible(FALSE))
            }

            events <<- events[indices]
            allocationRatios <<- allocationRatios[indices]
            logRanks <<- logRanks[indices]

            overallEvents <<- overallEvents[indices]
            overallAllocationRatios <<- overallAllocationRatios[indices]
            overallLogRanks <<- overallLogRanks[indices]

            .recreateDataFrame()

            return(invisible(TRUE))
        },
        getRandomData = function() {
            stop(
                C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                "the function 'DatasetSurvival.getRandomData()' is not implemented yet"
            )
        },
        .getOverallLogRanks = function(logRanks, events, overallEvents,
                kMax = length(logRanks), stage = length(logRanks)) {
            result <- c(logRanks[1:stage], rep(NA_real_, kMax - stage))
            if (stage == 1) {
                return(result)
            }
            for (k in 2:stage) {
                result[k] <-
                    (sqrt(events[k]) * logRanks[k] +
                        sqrt(overallEvents[k - 1]) *
                            result[k - 1]) / sqrt(overallEvents[k])
            }
            return(result)
        },
        .getOverallAllocationRatios = function(allocationRatios, events, overallEvents,
                kMax = length(allocationRatios), stage = length(allocationRatios)) {
            result <- c(
                allocationRatios[1:stage],
                rep(NA_real_, kMax - stage)
            )
            if (stage == 1) {
                return(result)
            }
            for (k in 2:stage) {
                result[k] <- (events[k] *
                    allocationRatios[k] + overallEvents[k - 1] *
                        result[k - 1]) / overallEvents[k]
            }
            return(result)
        },
        .createOverallData = function() {
            .data$overallEvent <<- rep(NA_real_, nrow(.data))
            if (inherits(.self, "DatasetEnrichmentSurvival")) {
                .data$overallExpectedEvent <<- rep(NA_real_, nrow(.data))
                .data$overallVarianceEvent <<- rep(NA_real_, nrow(.data))
            } else {
                .data$overallLogRank <<- rep(NA_real_, nrow(.data))
            }
            .data$overallAllocationRatio <<- rep(NA_real_, nrow(.data))
            subsetLevels <- NA_character_
            if (.enrichmentEnabled) {
                subsetLevels <- levels(.data$subset)
            }
            for (s in subsetLevels) {
                for (g in levels(.data$group)) {
                    if (!is.na(s)) {
                        indices <- which(.data$subset == s & .data$group == g)
                    } else {
                        indices <- which(.data$group == g)
                    }
                    .data$overallEvent[indices] <<- cumsum(.data$event[indices])
                    .data$overallExpectedEvent[indices] <<- cumsum(.data$expectedEvent[indices])
                    # .data$overallVarianceEvent[indices] <<- # maybe implemented later
                    .data$overallLogRank[indices] <<- .getOverallLogRanks(
                        .data$logRank[indices], .data$event[indices], .data$overallEvent[indices]
                    )
                    .data$overallAllocationRatio[indices] <<- .getOverallAllocationRatios(
                        .data$allocationRatio[indices], .data$event[indices], .data$overallEvent[indices]
                    )
                }
            }
            .setDataToVariables()
        },
        .getStageWiseEvents = function(overallEvents) {
            result <- overallEvents
            if (length(result) == 1) {
                return(result)
            }

            kMax <- length(result)
            result[2:kMax] <- overallEvents[2:kMax] - overallEvents[1:(kMax - 1)]
            return(result)
        },
        .getStageWiseLogRanks = function(overallLogRanks, overallEvents) {
            result <- overallLogRanks
            if (length(result) == 1) {
                return(result)
            }

            kMax <- length(result)
            result[2:kMax] <- (sqrt(overallEvents[2:kMax]) *
                overallLogRanks[2:kMax] -
                sqrt(overallEvents[1:(kMax - 1)]) *
                    overallLogRanks[1:(kMax - 1)]) /
                sqrt(overallEvents[2:kMax] - overallEvents[1:(kMax - 1)])
            return(result)
        },
        .getStageWiseAllocationRatios = function(overallAllocationRatios, events, overallEvents) {
            result <- overallAllocationRatios
            if (length(result) == 1) {
                return(result)
            }

            kMax <- length(result)
            result[2:kMax] <- (
                overallAllocationRatios[2:kMax] -
                    overallAllocationRatios[1:(kMax - 1)] *
                        overallEvents[1:(kMax - 1)] / overallEvents[2:kMax]
            ) / (events[2:kMax] / overallEvents[2:kMax])
            if (any(stats::na.omit(result) <= 0)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "overall allocation ratios not correctly specified: ",
                    "one or more calculated stage-wise allocation ratios <= 0"
                )
            }
            return(result)
        },
        .createStageWiseData = function() {
            "Calculates stage-wise logrank statistics, events, and allocation ratios if cumulative data is available"

            .data$event <<- rep(NA_real_, nrow(.data))
            if (inherits(.self, "DatasetEnrichmentSurvival")) {
                .data$expectedEvent <<- rep(NA_real_, nrow(.data))
                .data$varianceEvent <<- rep(NA_real_, nrow(.data))
            } else {
                .data$logRank <<- rep(NA_real_, nrow(.data))
            }
            .data$allocationRatio <<- rep(NA_real_, nrow(.data))

            subsetLevels <- NA_character_
            if (.enrichmentEnabled) {
                subsetLevels <- levels(.data$subset)
            }

            for (s in subsetLevels) {
                for (g in levels(.data$group)) {
                    if (!is.na(s)) {
                        indices <- which(.data$subset == s & .data$group == g)
                    } else {
                        indices <- which(.data$group == g)
                    }

                    groupNumber <- ifelse(levels(.data$group) > 1, g, "")
                    if (.enrichmentEnabled) {
                        .assertValuesAreStrictlyIncreasing(.data$overallEvent[indices],
                            paste0("overallEvents", groupNumber, "[subset == \"", s, "\"]"),
                            endingNasAllowed = TRUE
                        )
                    } else {
                        .assertValuesAreStrictlyIncreasing(.data$overallEvent[indices],
                            paste0("overallEvents", groupNumber),
                            endingNasAllowed = TRUE
                        )
                    }

                    .data$event[indices] <<- .getStageWiseEvents(.data$overallEvent[indices])
                    if (inherits(.self, "DatasetEnrichmentSurvival")) {
                        .data$expectedEvent[indices] <<- .getStageWiseEvents(.data$overallExpectedEvent[indices])
                        # .data$varianceEvent[indices] <<- # maybe implemented later
                    } else {
                        .data$logRank[indices] <<- .getStageWiseLogRanks(
                            .data$overallLogRank[indices], .data$overallEvent[indices]
                        )
                    }
                    .data$allocationRatio[indices] <<- .getStageWiseAllocationRatios(
                        .data$overallAllocationRatio[indices],
                        .data$event[indices], .data$overallEvent[indices]
                    )
                }
            }
            .setDataToVariables()
        }
    )
)

#'
#' @rdname DatasetSurvival
#'
#' @keywords internal
#'
DatasetEnrichmentSurvival <- setRefClass("DatasetEnrichmentSurvival",
    contains = "DatasetSurvival",
    fields = list(
        expectedEvents = "numeric",
        varianceEvents = "numeric",
        overallExpectedEvents = "numeric",
        overallVarianceEvents = "numeric"
    ),
    methods = list(
        .initByDataFrame = function(dataFrame) {
            callSuper(dataFrame)

            if (.paramExists(dataFrame, C_KEY_WORDS_OVERALL_EXPECTED_EVENTS) ||
                    .paramExists(dataFrame, C_KEY_WORDS_OVERALL_VARIANCE_EVENTS)) {
                if (!.paramExists(dataFrame, C_KEY_WORDS_OVERALL_EXPECTED_EVENTS)) {
                    stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'overallExpectedEvents' is missing")
                }
                if (!.paramExists(dataFrame, C_KEY_WORDS_OVERALL_VARIANCE_EVENTS)) {
                    stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'overallVarianceEvents' is missing")
                }

                .inputType <<- "overall"

                overallEvents <<- .getValidatedFloatingPointNumbers(
                    .getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_EVENTS),
                    parameterName = "Cumulative events"
                )
                .validateValues(overallEvents, "overallEvents")

                overallExpectedEvents <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_EXPECTED_EVENTS)
                .validateValues(overallExpectedEvents, "overallExpectedEvents")

                overallVarianceEvents <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_VARIANCE_EVENTS)
                .validateValues(overallVarianceEvents, "overallVarianceEvents")

                overallAllocationRatios <<- .getValuesByParameterName(
                    dataFrame,
                    parameterNameVariants = C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS,
                    defaultValues = .getAllocationRatioDefaultValues(stages, overallEvents, overallExpectedEvents)
                )
                .validateValues(overallAllocationRatios, "overallAllocationRatios")
            } else if (.paramExists(dataFrame, C_KEY_WORDS_EXPECTED_EVENTS) ||
                    .paramExists(dataFrame, C_KEY_WORDS_VARIANCE_EVENTS)) {
                if (!.paramExists(dataFrame, C_KEY_WORDS_EXPECTED_EVENTS)) {
                    stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'expectedEvents' is missing")
                }
                if (!.paramExists(dataFrame, C_KEY_WORDS_VARIANCE_EVENTS)) {
                    stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'varianceEvents' is missing")
                }

                .inputType <<- "stagewise"

                events <<- .getValidatedFloatingPointNumbers(
                    .getValuesByParameterName(dataFrame, C_KEY_WORDS_EVENTS),
                    parameterName = "Events"
                )
                .validateValues(events, "events")

                expectedEvents <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_EXPECTED_EVENTS)
                .validateValues(expectedEvents, "expectedEvents")

                varianceEvents <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_VARIANCE_EVENTS)
                .validateValues(varianceEvents, "varianceEvents")

                allocationRatios <<- .getValuesByParameterName(
                    dataFrame,
                    parameterNameVariants = C_KEY_WORDS_ALLOCATION_RATIOS,
                    defaultValues = .getAllocationRatioDefaultValues(stages, events, expectedEvents)
                )
                .validateValues(allocationRatios, "allocationRatios")
            }

            .setParameterType("groups", C_PARAM_NOT_APPLICABLE)

            if (.inputType == "stagewise") {
                n <- length(events)
                overallExpectedEvents <<- rep(NA_real_, n)
                overallVarianceEvents <<- rep(NA_real_, n)

                .setParameterType("events", C_PARAM_USER_DEFINED)
                .setParameterType("allocationRatios", C_PARAM_USER_DEFINED)
                .setParameterType("expectedEvents", C_PARAM_USER_DEFINED)
                .setParameterType("varianceEvents", C_PARAM_USER_DEFINED)

                .setParameterType("overallEvents", C_PARAM_GENERATED)
                .setParameterType("overallAllocationRatios", C_PARAM_GENERATED)
                .setParameterType("overallExpectedEvents", C_PARAM_GENERATED)
                .setParameterType("overallVarianceEvents", C_PARAM_GENERATED)

                .recreateDataFrame()
                .createOverallData()
            } else {
                n <- length(overallEvents)
                expectedEvents <<- rep(NA_real_, n)
                varianceEvents <<- rep(NA_real_, n)

                .setParameterType("events", C_PARAM_GENERATED)
                .setParameterType("allocationRatios", C_PARAM_GENERATED)
                .setParameterType("expectedEvents", C_PARAM_GENERATED)
                .setParameterType("varianceEvents", C_PARAM_GENERATED)

                .setParameterType("overallEvents", C_PARAM_USER_DEFINED)
                .setParameterType("overallAllocationRatios", C_PARAM_USER_DEFINED)
                .setParameterType("overallExpectedEvents", C_PARAM_USER_DEFINED)
                .setParameterType("overallVarianceEvents", C_PARAM_USER_DEFINED)

                .recreateDataFrame()
                .createStageWiseData()
            }
        },
        .getVisibleFieldNames = function() {
            visibleFieldNames <- callSuper()
            visibleFieldNames <- visibleFieldNames[!(visibleFieldNames %in% c("logRanks", "overallLogRanks"))]
            return(visibleFieldNames)
        },
        .setDataToVariables = function() {
            callSuper()
            overallExpectedEvents <<- .data$overallExpectedEvent
            overallVarianceEvents <<- .data$overallVarianceEvent
            expectedEvents <<- .data$expectedEvent
        },
        getOverallExpectedEvent = function(stage, group = 1, subset = NA_character_) {
            return(.data$overallExpectedEvent[.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallExpectedEvents = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(.data$overallExpectedEvent[.getIndices(stage = .getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallExpectedEventsUpTo = function(to, group = 1, subset = NA_character_) {
            return(.data$overallExpectedEvent[.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getOverallVarianceEvent = function(stage, group = 1, subset = NA_character_) {
            return(.data$overallVarianceEvent[.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallVarianceEvents = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(.data$overallVarianceEvent[.getIndices(stage = .getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallVarianceEventsUpTo = function(to, group = 1, subset = NA_character_) {
            return(.data$overallVarianceEvent[.getIndices(stage = c(1:to), group = group, subset = subset)])
        }
    )
)

#'
#' @title
#' Dataset Summary
#'
#' @description
#' Displays a summary of \code{\link{Dataset}} object.
#'
#' @param object A \code{\link{Dataset}} object.
#' @inheritParams param_digits
#' @inheritParams param_three_dots
#'
#' @details
#' Summarizes the parameters and results of a dataset.
#'
#' @template details_summary
#'
#' @template return_object_summary
#' @template how_to_get_help_for_generics
#'
#' @export
#'
#' @keywords internal
#'
summary.Dataset <- function(object, ..., type = 1, digits = NA_integer_) {
    .warnInCaseOfUnknownArguments(functionName = "summary", ...)

    if (type == 1 && inherits(object, "SummaryFactory")) {
        return(object)
    }

    if (type != 1) {
        return(summary.ParameterSet(object, type = type, digits = digits, ...))
    }

    intervalFormat <- getOption("rpact.summary.intervalFormat", "[%s; %s]")
    .assertIsValidSummaryIntervalFormat(intervalFormat)

    summaryFactory <- SummaryFactory(object = object, intervalFormat = intervalFormat)

    s <- object$.toString()

    kMax <- object$getNumberOfStages()
    summaryFactory$title <- .firstCharacterToUpperCase(s)

    numberOfGroups <- object$getNumberOfGroups()

    if (numberOfGroups == 1) {
        groups <- "one sample"
    } else if (numberOfGroups == 2) {
        groups <- c("one treatment", "one control group")
        if (object$isDatasetSurvival()) {
            groups <- paste0(groups, c(" (1)", " (2)"))
        }
    } else {
        groups <- c(paste0(
            .integerToWrittenNumber(numberOfGroups - 1),
            " treatment groups"
        ), "one control group")
        if (object$isDatasetSurvival()) {
            groups <- paste0(groups, c(
                paste0(" (", .arrayToString(1:(numberOfGroups - 1)), ")"),
                paste0(" (", numberOfGroups, ")")
            ))
        }
    }

    prefix <- ""
    if (object$isDatasetMeans()) {
        prefix <- "the sample sizes, means, and standard deviations of "
    } else if (object$isDatasetRates()) {
        prefix <- "the sample sizes and events of "
    } else if (object$isDatasetSurvival()) {
        prefix <- "the events and log rank statistics of the comparison of "
    }
    if (numberOfGroups > 1) {
        prefix <- paste0(prefix, "\n")
    }
    header <- paste0(
        "The dataset contains ", prefix,
        paste0(groups, collapse = ifelse(object$isDatasetSurvival(), " with ", " and "))
    )
    if (object$.enrichmentEnabled) {
        header <- paste0(header, ". The data will be analyzed ", ifelse(object$isStratified(), "", "non-"), "stratified")
    }
    if (kMax > 1) {
        header <- paste0(
            header, ".\nThe total number of looks is ", .integerToWrittenNumber(kMax),
            "; stage-wise and cumulative data are included"
        )
    }
    header <- paste0(header, ".")
    summaryFactory$header <- header

    digitSettings <- .getSummaryDigits(digits)
    digits <- digitSettings$digits
    digitsSampleSize <- 0
    digitsGeneral <- digitSettings$digitsGeneral
    digitsProbabilities <- digitSettings$digitsProbabilities

    paramsToCheck <- character(0)
    if (object$isDatasetMeans() || object$isDatasetRates()) {
        paramsToCheck <- c(paramsToCheck, "sampleSizes")
        if (kMax > 1) {
            paramsToCheck <- c(paramsToCheck, "overallSampleSizes")
        }
    } else if (object$isDatasetRates() || object$isDatasetSurvival()) {
        paramsToCheck <- c(paramsToCheck, "events")
        if (kMax > 1) {
            paramsToCheck <- c(paramsToCheck, "overallEvents")
        }
    }
    if (length(paramsToCheck) > 0) {
        for (param in paramsToCheck) {
            if (.isFloatingPointSampleSize(object, param)) {
                digitsSampleSize <- max(digitsSampleSize, .getMaxDigits(object[[param]]))
            }
        }
        digitsSampleSize <- min(digitsSampleSize, digits)
    }

    summaryFactory$addItem("Stage", object$stages)

    if (numberOfGroups > 1) {
        groupNumbers <- object$groups
        if (object$isDatasetSurvival()) {
            groupNumbers <- paste0(object$groups, " vs ", numberOfGroups)
            summaryFactory$addItem("Comparison", groupNumbers)
        } else {
            summaryFactory$addItem("Group", groupNumbers)
        }
    }

    if (object$.enrichmentEnabled) {
        summaryFactory$addItem("Subset", object$subsets)
    }

    parameterCaptionPrefix <- ifelse(kMax == 1, "", "Stage-wise ")

    if (object$isDatasetMeans() || object$isDatasetRates()) {
        summaryFactory$addParameter(object,
            parameterName = "sampleSizes",
            parameterCaption = .firstCharacterToUpperCase(parameterCaptionPrefix, "sample size"),
            roundDigits = digitsSampleSize
        )
        if (kMax > 1) {
            summaryFactory$addParameter(object,
                parameterName = "overallSampleSizes",
                parameterCaption = "Cumulative sample size", roundDigits = digitsSampleSize
            )
        }
    }

    if (object$isDatasetMeans()) {
        summaryFactory$addParameter(object,
            parameterName = "means",
            parameterCaption = .firstCharacterToUpperCase(parameterCaptionPrefix, "mean"),
            roundDigits = digitsGeneral
        )
        if (kMax > 1) {
            summaryFactory$addParameter(object,
                parameterName = "overallMeans",
                parameterCaption = "Cumulative mean", roundDigits = digitsGeneral
            )
        }
        summaryFactory$addParameter(object,
            parameterName = "stDevs",
            parameterCaption = .firstCharacterToUpperCase(parameterCaptionPrefix, "standard deviation"),
            roundDigits = digitsGeneral
        )
        if (kMax > 1) {
            summaryFactory$addParameter(object,
                parameterName = "overallStDevs",
                parameterCaption = "Cumulative standard deviation", roundDigits = digitsGeneral
            )
        }
    } else if (object$isDatasetRates()) {
        summaryFactory$addParameter(object,
            parameterName = "events",
            parameterCaption = .firstCharacterToUpperCase(parameterCaptionPrefix, "number of events"),
            roundDigits = digitsSampleSize
        )
        if (kMax > 1) {
            summaryFactory$addParameter(object,
                parameterName = "overallEvents",
                parameterCaption = "Cumulative number of events", roundDigits = digitsSampleSize
            )
        }
    } else if (object$isDatasetSurvival()) {
        summaryFactory$addParameter(object,
            parameterName = "events",
            parameterCaption = .firstCharacterToUpperCase(parameterCaptionPrefix, "number of events"),
            roundDigits = digitsSampleSize
        )
        if (kMax > 1) {
            summaryFactory$addParameter(object,
                parameterName = "overallEvents",
                parameterCaption = "Cumulative number of events", roundDigits = digitsSampleSize
            )
        }
        summaryFactory$addParameter(object,
            parameterName = "logRanks",
            parameterCaption = .firstCharacterToUpperCase(parameterCaptionPrefix, "log rank statistic"),
            roundDigits = digitsGeneral
        )
        if (kMax > 1) {
            summaryFactory$addParameter(object,
                parameterName = "overallLogRanks",
                parameterCaption = "Cumulative log rank statistic", roundDigits = digitsGeneral
            )
        }
        if (!any(is.na(object$allocationRatios)) && any(object$allocationRatios != 1)) {
            summaryFactory$addParameter(object,
                parameterName = "allocationRatios",
                parameterCaption = .firstCharacterToUpperCase(parameterCaptionPrefix, "allocation ratio"),
                roundDigits = digitsGeneral
            )
            if (kMax > 1) {
                summaryFactory$addParameter(object,
                    parameterName = "overallAllocationRatios",
                    parameterCaption = "Cumulative allocation ratio", roundDigits = digitsGeneral
                )
            }
        }
    }

    return(summaryFactory)
}

#'
#' @title
#' Print Dataset Values
#'
#' @description
#' \code{print} prints its \code{\link{Dataset}} argument and returns it invisibly (via \code{invisible(x)}).
#'
#' @param x A \code{\link{Dataset}} object.
#' @param markdown If \code{TRUE}, the output will be created in Markdown.
#' @param output A character defining the output type, default is "list".
#' @inheritParams param_three_dots
#'
#' @details
#' Prints the dataset.
#'
#' @export
#'
#' @keywords internal
#'
print.Dataset <- function(x, ..., markdown = FALSE, output = c("list", "long", "wide", "r", "rComplete")) {
    fCall <- match.call(expand.dots = FALSE)
    datasetName <- deparse(fCall$x)

    output <- match.arg(output)

    if (markdown) {
        if (output != "list") {
            warning("'output' (\"", output, "\") will be ignored ",
                "because only \"list\" is supported yet if markdown is enabled",
                call. = FALSE
            )
        }

        x$.catMarkdownText()
        return(invisible(x))
    }

    if (output == "long") {
        m <- getLongFormat(x)
        m <- prmatrix(m, rowlab = rep("", nrow(m)))
        print(m, quote = FALSE, right = FALSE)
        return(invisible(x))
    } else if (output == "wide") {
        m <- getWideFormat(x)
        m <- prmatrix(m, rowlab = rep("", nrow(m)))
        print(m, quote = FALSE, right = FALSE)
        return(invisible(x))
    } else if (output %in% c("r", "rComplete")) {
        lines <- .getDatasetArgumentsRCodeLines(x, complete = (output == "rComplete"))
        lines <- paste0("\t", lines)

        if (is.null(datasetName) || length(datasetName) != 1 || is.na(datasetName)) {
            datasetName <- "dataInput"
        }

        cat(datasetName, " <- getDataset(\n", sep = "")
        cat(paste0(lines, collapse = ",\n"), "\n")
        cat(")\n")
        return(invisible(x))
    }

    x$show()
    return(invisible(x))
}
