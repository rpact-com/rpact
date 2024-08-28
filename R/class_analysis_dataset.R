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
## |  File version: $Revision: 8127 $
## |  Last changed: $Date: 2024-08-23 18:00:31 +0200 (Fr, 23 Aug 2024) $
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

#' @title
#' Read Dataset
#'
#' @description
#' Reads a data file and returns it as dataset object.
#'
#' @param file A CSV file (see \code{\link[utils]{read.table}}).
#' @param header A logical value indicating whether the file contains the names of
#'        the variables as its first line.
#' @param sep The field separator character. Values on each line of the file are separated
#'        by this character. If sep = "," (the default for \code{readDataset}) the separator is a comma.
#' @param quote The set of quoting characters. To disable quoting altogether, use
#'        quote = "". See scan for the behavior on quotes embedded in quotes. Quoting is only
#'        considered for columns read as character, which is all of them unless \code{colClasses} is specified.
#' @param dec The character used in the file for decimal points.
#' @param fill logical. If \code{TRUE} then in case the rows have unequal length, blank fields
#'        are implicitly added.
#' @param comment.char character: a character vector of length one containing a single character
#'        or an empty string. Use "" to turn off the interpretation of comments altogether.
#' @param fileEncoding character string: if non-empty declares the encoding used on a file
#'        (not a connection) so the character data can be re-encoded.
#'        See the 'Encoding' section of the help for file, the 'R Data Import/Export Manual' and 'Note'.
#' @param ... Further arguments to be passed to \code{\link[utils]{read.table}}.
#'
#' @details
#' \code{readDataset} is a wrapper function that uses \code{\link[utils]{read.table}} to read the
#' CSV file into a data frame, transfers it from long to wide format with \code{\link[stats]{reshape}}
#' and puts the data to \code{\link[=getDataset]{getDataset()}}.
#'
#' @template return_object_dataset
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[=readDatasets]{readDatasets()}} for reading multiple datasets,
#'   \item \code{\link[=writeDataset]{writeDataset()}} for writing a single dataset,
#'   \item \code{\link[=writeDatasets]{writeDatasets()}} for writing multiple datasets.
#' }
#'
#' @examples
#' \dontrun{
#' dataFileRates <- system.file("extdata",
#'     "dataset_rates.csv",
#'     package = "rpact"
#' )
#' if (dataFileRates != "") {
#'     datasetRates <- readDataset(dataFileRates)
#'     datasetRates
#' }
#'
#' dataFileMeansMultiArm <- system.file("extdata",
#'     "dataset_means_multi-arm.csv",
#'     package = "rpact"
#' )
#' if (dataFileMeansMultiArm != "") {
#'     datasetMeansMultiArm <- readDataset(dataFileMeansMultiArm)
#'     datasetMeansMultiArm
#' }
#'
#' dataFileRatesMultiArm <- system.file("extdata",
#'     "dataset_rates_multi-arm.csv",
#'     package = "rpact"
#' )
#' if (dataFileRatesMultiArm != "") {
#'     datasetRatesMultiArm <- readDataset(dataFileRatesMultiArm)
#'     datasetRatesMultiArm
#' }
#'
#' dataFileSurvivalMultiArm <- system.file("extdata",
#'     "dataset_survival_multi-arm.csv",
#'     package = "rpact"
#' )
#' if (dataFileSurvivalMultiArm != "") {
#'     datasetSurvivalMultiArm <- readDataset(dataFileSurvivalMultiArm)
#'     datasetSurvivalMultiArm
#' }
#' }
#'
#' @export
#'
readDataset <- function(file, ..., header = TRUE, sep = ",", quote = "\"",
        dec = ".", fill = TRUE, comment.char = "", fileEncoding = "UTF-8") {
    if (!file.exists(file)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "the file '", file, "' does not exist")
    }

    data <- utils::read.table(
        file = file, header = header, sep = sep,
        quote = quote, dec = dec, fill = fill, fileEncoding = fileEncoding, ...
    )
    dataWide <- stats::reshape(data = data, direction = "wide", idvar = "stages", timevar = "groups")
    colnames(dataWide) <- gsub("\\.", "", colnames(dataWide))
    return(getDataset(dataWide))
}

#' @title
#' Write Dataset
#'
#' @description
#' Writes a dataset to a CSV file.
#'
#' @param dataset A dataset.
#' @param file The target CSV file.
#' @param append Logical. Only relevant if file is a character string.
#'        If \code{TRUE}, the output is appended to the file. If \code{FALSE}, any existing file of the name is destroyed.
#' @param sep The field separator character. Values on each line of the file are separated
#'        by this character. If sep = "," (the default for \code{writeDataset}) the separator is a comma.
#' @param quote The set of quoting characters. To disable quoting altogether, use
#'        quote = "". See scan for the behavior on quotes embedded in quotes. Quoting is only
#'        considered for columns read as character, which is all of them unless \code{colClasses} is specified.
#' @param dec The character used in the file for decimal points.
#' @param eol The character(s) to print at the end of each line (row).
#' @param na The string to use for missing values in the data.
#' @param row.names Either a logical value indicating whether the row names of \code{dataset} are
#'        to be written along with  \code{dataset}, or a character vector of row names to be written.
#' @param col.names Either a logical value indicating whether the column names of  \code{dataset} are
#'        to be written along with  \code{dataset}, or a character vector of column names to be written.
#'        See the section on 'CSV files' for the meaning of \code{col.names = NA}.
#' @param qmethod A character string specifying how to deal with embedded double quote characters
#'        when quoting strings. Must be one of "double" (default in \code{writeDataset}) or "escape".
#' @param fileEncoding Character string: if non-empty declares the encoding used on a file
#'        (not a connection) so the character data can be re-encoded.
#'        See the 'Encoding' section of the help for file, the 'R Data Import/Export Manual' and 'Note'.
#' @param ... Further arguments to be passed to \code{\link[utils]{write.table}}.
#'
#' @details
#' \code{\link[=writeDataset]{writeDataset()}} is a wrapper function that coerces the dataset to a data frame and uses \cr
#' \code{\link[utils]{write.table}} to write it to a CSV file.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[=writeDatasets]{writeDatasets()}} for writing multiple datasets,
#'   \item \code{\link[=readDataset]{readDataset()}} for reading a single dataset,
#'   \item \code{\link[=readDatasets]{readDatasets()}} for reading multiple datasets.
#' }
#'
#' @examples
#' \dontrun{
#' datasetOfRates <- getDataset(
#'     n1 = c(11, 13, 12, 13),
#'     n2 = c(8, 10, 9, 11),
#'     events1 = c(10, 10, 12, 12),
#'     events2 = c(3, 5, 5, 6)
#' )
#' writeDataset(datasetOfRates, "dataset_rates.csv")
#' }
#'
#' @export
#'
writeDataset <- function(dataset, file, ..., append = FALSE, quote = TRUE, sep = ",",
        eol = "\n", na = "NA", dec = ".", row.names = TRUE,
        col.names = NA, qmethod = "double",
        fileEncoding = "UTF-8") {
    .assertIsDataset(dataset)

    x <- as.data.frame(dataset, niceColumnNamesEnabled = FALSE)

    utils::write.table(
        x = x, file = file, append = append, quote = quote, sep = sep,
        eol = eol, na = na, dec = dec, row.names = FALSE,
        col.names = TRUE, qmethod = qmethod,
        fileEncoding = fileEncoding
    )
}

#' @title
#' Read Multiple Datasets
#'
#' @description
#' Reads a data file and returns it as a list of dataset objects.
#'
#' @param file A CSV file (see \code{\link[utils]{read.table}}).
#' @param header A logical value indicating whether the file contains the names of
#'        the variables as its first line.
#' @param sep The field separator character. Values on each line of the file are separated
#'        by this character. If sep = "," (the default for \code{readDatasets}) the separator is a comma.
#' @param quote The set of quoting characters. To disable quoting altogether, use
#'        quote = "". See scan for the behavior on quotes embedded in quotes. Quoting is only
#'        considered for columns read as character, which is all of them unless \code{colClasses} is specified.
#' @param dec The character used in the file for decimal points.
#' @param fill logical. If \code{TRUE} then in case the rows have unequal length, blank fields
#'        are implicitly added.
#' @param comment.char character: a character vector of length one containing a single character
#'        or an empty string. Use "" to turn off the interpretation of comments altogether.
#' @param fileEncoding character string: if non-empty declares the encoding used on a file
#'        (not a connection) so the character data can be re-encoded.
#'        See the 'Encoding' section of the help for file, the 'R Data Import/Export Manual' and 'Note'.
#' @param ... Further arguments to be passed to \code{\link[utils]{read.table}}.
#'
#' @details
#' Reads a file that was written by \code{\link[=writeDatasets]{writeDatasets()}} before.
#'
#' @return Returns a \code{\link[base]{list}} of \code{\link{Dataset}} objects.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[=readDataset]{readDataset()}} for reading a single dataset,
#'   \item \code{\link[=writeDatasets]{writeDatasets()}} for writing multiple datasets,
#'   \item \code{\link[=writeDataset]{writeDataset()}} for writing a single dataset.
#' }
#'
#' @examples
#' \dontrun{
#' dataFile <- system.file("extdata", "datasets_rates.csv", package = "rpact")
#' if (dataFile != "") {
#'     datasets <- readDatasets(dataFile)
#'     datasets
#' }
#' }
#' @export
#'
readDatasets <- function(file, ..., header = TRUE, sep = ",", quote = "\"",
        dec = ".", fill = TRUE, comment.char = "", fileEncoding = "UTF-8") {
    if (!file.exists(file)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "the file '", file, "' does not exist")
    }

    data <- utils::read.table(
        file = file, header = header, sep = sep,
        quote = quote, dec = dec, fill = fill, fileEncoding = fileEncoding, ...
    )

    if (is.null(data[["datasetId"]])) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "data file must contain the column 'datasetId'")
    }

    datasets <- list()
    for (datasetId in unique(data$datasetId)) {
        subData <- data[data$datasetId == datasetId, ]
        dataFrame <- subset(subData, select = -datasetId)
        description <- NA_character_
        if (!is.null(dataFrame[["description"]])) {
            description <- as.character(dataFrame$description[1])
            dataFrame <- subset(dataFrame, select = -description)
        }
        if (length(unique(subData$groups)) == 2) {
            dataWide <- stats::reshape(dataFrame, direction = "wide", idvar = "stages", timevar = "groups")
            colnames(dataWide) <- gsub("\\.", "", colnames(dataWide))
            dataset <- getDataset(dataWide)
        } else {
            dataset <- getDataset(dataFrame)
        }
        dataset$setDescription(description)
        datasets <- c(datasets, dataset)
    }
    return(datasets)
}

#' @title
#' Write Multiple Datasets
#'
#' @description
#' Writes a list of datasets to a CSV file.
#'
#' @param datasets A list of datasets.
#' @param file The target CSV file.
#' @param append Logical. Only relevant if file is a character string.
#'        If \code{TRUE}, the output is appended to the file. If FALSE, any existing file of the name is destroyed.
#' @param sep The field separator character. Values on each line of the file are separated
#'        by this character. If sep = "," (the default for \code{writeDatasets}) the separator is a comma.
#' @param quote The set of quoting characters. To disable quoting altogether, use
#'        quote = "". See scan for the behavior on quotes embedded in quotes. Quoting is only
#'        considered for columns read as character, which is all of them unless \code{colClasses} is specified.
#' @param dec The character used in the file for decimal points.
#' @param eol The character(s) to print at the end of each line (row).
#' @param na The string to use for missing values in the data.
#' @param row.names Either a logical value indicating whether the row names of \code{dataset} are
#'        to be written along with  \code{dataset}, or a character vector of row names to be written.
#' @param col.names Either a logical value indicating whether the column names of  \code{dataset} are
#'        to be written along with  \code{dataset}, or a character vector of column names to be written.
#'        See the section on 'CSV files' for the meaning of \code{col.names = NA}.
#' @param qmethod A character string specifying how to deal with embedded double quote characters
#'        when quoting strings. Must be one of "double" (default in \code{writeDatasets}) or "escape".
#' @param fileEncoding Character string: if non-empty declares the encoding used on a file
#'        (not a connection) so the character data can be re-encoded.
#'        See the 'Encoding' section of the help for file, the 'R Data Import/Export Manual' and 'Note'.
#' @param ... Further arguments to be passed to \code{\link[utils]{write.table}}.
#'
#' @details
#' The format of the CSV file is optimized for usage of \code{\link[=readDatasets]{readDatasets()}}.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[=writeDataset]{writeDataset()}} for writing a single dataset,
#'   \item \code{\link[=readDatasets]{readDatasets()}} for reading multiple datasets,
#'   \item \code{\link[=readDataset]{readDataset()}} for reading a single dataset.
#' }
#'
#' @examples
#' \dontrun{
#' d1 <- getDataset(
#'     n1 = c(11, 13, 12, 13),
#'     n2 = c(8, 10, 9, 11),
#'     events1 = c(10, 10, 12, 12),
#'     events2 = c(3, 5, 5, 6)
#' )
#' d2 <- getDataset(
#'     n1 = c(9, 13, 12, 13),
#'     n2 = c(6, 10, 9, 11),
#'     events1 = c(10, 10, 12, 12),
#'     events2 = c(4, 5, 5, 6)
#' )
#' datasets <- list(d1, d2)
#' writeDatasets(datasets, "datasets_rates.csv")
#' }
#'
#' @export
#'
writeDatasets <- function(datasets, file, ..., append = FALSE, quote = TRUE, sep = ",",
        eol = "\n", na = "NA", dec = ".", row.names = TRUE,
        col.names = NA, qmethod = "double",
        fileEncoding = "UTF-8") {
    if (!is.list(datasets)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'datasets' must be a list of datasets")
    }

    if (length(datasets) == 0) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'datasets' is empty")
    }

    datasetType <- NA_character_
    dataFrames <- NULL
    for (i in seq_len(length(datasets))) {
        dataset <- datasets[[i]]
        .assertIsDataset(dataset)
        if (is.na(datasetType)) {
            datasetType <- .getClassName(dataset)
        } else if (.getClassName(dataset) != datasetType) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all datasets must have the same type")
        }

        data <- as.data.frame(dataset, niceColumnNamesEnabled = FALSE)
        datasetId <- ifelse(!is.null(dataset$getId()) && !is.na(dataset$getId()), dataset$getId(), i)
        data <- cbind(rep(datasetId, nrow(data)), data)
        colnames(data)[1] <- "datasetId"

        if (!is.null(dataset$getDescription()) && !is.na(dataset$getDescription())) {
            data <- cbind(data, rep(dataset$getDescription(), nrow(data)))
            colnames(data)[ncol(data)] <- "description"
        }

        if (is.null(dataFrames)) {
            dataFrames <- data
        } else {
            dataFrames <- rbind(dataFrames, data)
        }
    }

    if (is.null(dataFrames)) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "failed to bind datasets")
    }

    utils::write.table(
        x = dataFrames, file = file, append = append, quote = quote, sep = sep,
        eol = eol, na = na, dec = dec, row.names = FALSE,
        col.names = TRUE, qmethod = qmethod,
        fileEncoding = fileEncoding
    )
}

.getDataset <- function(..., floatingPointNumbersEnabled = FALSE) {
    args <- list(...)
    if (length(args) == 0) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data.frame, data vectors, or datasets expected")
    }

    if (.optionalArgsContainsDatasets(...)) {
        if (length(args) == 1) {
            return(args[[1]])
        }

        design <- .getDesignFromArgs(...)
        if (length(args) == 2 && !is.null(design)) {
            dataset <- .getDatasetFromArgs(...)
            if (!is.null(dataset)) {
                dataset <- dataset$clone(deep = TRUE) 
                dataset$.design <- design
                return(dataset)
            }
        }

        dataset <- .getEnrichmentDatasetFromArgs(...)
        dataset$.design <- design
        return(dataset)
    }

    exampleType <- args[["example"]]
    if (!is.null(exampleType) && exampleType %in% c("means", "rates", "survival")) {
        return(.getDatasetExample(exampleType = exampleType))
    }

    if (length(args) == 1 && !is.null(args[[1]]) && is.list(args[[1]]) && !is.data.frame(args[[1]])) {
        return(.getDatasetMeansFromModelsByStage(emmeansResults = args[[1]]))
    }

    emmeansResults <- .getDatasetMeansModelObjectsList(args)
    if (!is.null(emmeansResults) && length(emmeansResults) > 0) {
        return(.getDatasetMeansFromModelsByStage(emmeansResults = emmeansResults))
    }

    dataFrame <- .getDataFrameFromArgs(...)

    design <- .getDesignFromArgs(...)

    if (is.null(dataFrame)) {
        args <- .removeDesignFromArgs(args)

        paramNames <- names(args)
        paramNames <- paramNames[paramNames != ""]

        numberOfParameters <- length(args)
        if (numberOfParameters > 0 && names(args)[1] == "" && .isTrialDesign(args[[1]])) {
            numberOfParameters <- numberOfParameters - 1
        }

        if (length(paramNames) != numberOfParameters) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all parameters must be named")
        }

        if (length(paramNames) != length(unique(paramNames))) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "the parameter names must be unique")
        }

        dataFrame <- .createDataFrame(...)
    }

    enrichmentEnabled <- .isDataObjectEnrichment(...)

    if (.isDataObjectMeans(...)) {
        return(DatasetMeans$new(
            dataFrame = dataFrame,
            floatingPointNumbersEnabled = floatingPointNumbersEnabled,
            enrichmentEnabled = enrichmentEnabled,
            .design = design
        ))
    }

    if (.isDataObjectRates(...)) {
        return(DatasetRates$new(
            dataFrame = dataFrame,
            floatingPointNumbersEnabled = floatingPointNumbersEnabled,
            enrichmentEnabled = enrichmentEnabled,
            .design = design
        ))
    }

    if (.isDataObjectNonStratifiedEnrichmentSurvival(...)) {
        return(DatasetEnrichmentSurvival$new(
            dataFrame = dataFrame,
            floatingPointNumbersEnabled = floatingPointNumbersEnabled,
            enrichmentEnabled = enrichmentEnabled,
            .design = design
        ))
    }

    if (.isDataObjectSurvival(...)) {
        return(DatasetSurvival$new(
            dataFrame = dataFrame,
            floatingPointNumbersEnabled = floatingPointNumbersEnabled,
            enrichmentEnabled = enrichmentEnabled,
            .design = design
        ))
    }

    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "failed to identify dataset type")
}

#' @title
#' Get Dataset
#'
#' @description
#' Creates a dataset object and returns it.
#'
#' @param ... A \code{data.frame} or some data vectors defining the dataset.
#' @param floatingPointNumbersEnabled If \code{TRUE},
#'        sample sizes and event numbers can be specified as floating-point numbers
#'        (this make sense, e.g., for theoretical comparisons); \cr
#'        by default \code{floatingPointNumbersEnabled = FALSE}, i.e.,
#'        samples sizes and event numbers defined as floating-point numbers will be truncated.
#'
#' @details
#' The different dataset types \code{DatasetMeans}, of \code{DatasetRates}, or
#' \code{DatasetSurvival} can be created as follows:
#' \itemize{
#'   \item An element of \code{\link{DatasetMeans}} for one sample is created by \cr
#'     \code{getDataset(sampleSizes =, means =, stDevs =)} where \cr
#'     \code{sampleSizes}, \code{means}, \code{stDevs} are vectors with stage-wise sample sizes,
#'     means and standard deviations of length given by the number of available stages.
#'   \item An element of \code{\link{DatasetMeans}} for two samples is created by \cr
#'     \code{getDataset(sampleSizes1 =, sampleSizes2 =, means1 =, means2 =, } \cr
#'     \code{stDevs1 =, stDevs2 =)} where
#'     \code{sampleSizes1}, \code{sampleSizes2}, \code{means1}, \code{means2},
#'     \code{stDevs1}, \code{stDevs2} are vectors with
#'     stage-wise sample sizes, means and standard deviations for the two treatment groups
#'     of length given by the number of available stages.
#'   \item An element of \code{\link{DatasetRates}} for one sample is created by \cr
#'     \code{getDataset(sampleSizes =, events =)} where \code{sampleSizes}, \code{events} are vectors
#'     with stage-wise sample sizes and events of length given by the number of available stages.
#'   \item An element of \code{\link{DatasetRates}} for two samples is created by \cr
#'     \code{getDataset(sampleSizes1 =, sampleSizes2 =, events1 =, events2 =)} where
#'     \code{sampleSizes1}, \code{sampleSizes2}, \code{events1}, \code{events2}
#'     are vectors with stage-wise sample sizes
#'     and events  for the two treatment groups of length given by the number of available stages.
#'   \item An element of \code{\link{DatasetSurvival}} is created by \cr
#'     \code{getDataset(events =, logRanks =, allocationRatios =)} where
#'     \code{events}, \code{logRanks}, and \code{allocation ratios} are the stage-wise events,
#'     (one-sided) logrank statistics, and allocation ratios.
#'   \item An element of \code{\link{DatasetMeans}}, \code{\link{DatasetRates}}, and \code{\link{DatasetSurvival}}
#'     for more than one comparison is created by adding subsequent digits to the variable names.
#'     The system can analyze these data in a multi-arm many-to-one comparison setting where the
#'     group with the highest index represents the control group.
#' }
#' Prefix \code{overall[Capital case of first letter of variable name]...} for the variable
#' names enables entering the overall (cumulative) results and calculates stage-wise statistics.
#' Since rpact version 3.2, the prefix \code{cumulative[Capital case of first letter of variable name]...} or
#' \code{cum[Capital case of first letter of variable name]...} can alternatively be used for this.
#'
#' \code{n} can be used in place of \code{samplesizes}.
#'
#' Note that in survival design usually the overall (cumulative) events and logrank test statistics are provided
#' in the output, so \cr
#' \code{getDataset(cumulativeEvents=, cumulativeLogRanks =, cumulativeAllocationRatios =)} \cr
#' is the usual command for entering survival data. Note also that for \code{cumulativeLogranks} also the
#' z scores from a Cox regression can be used.
#'
#' For multi-arm designs, the index refers to the considered comparison. For example,\cr
#' \code{
#'   getDataset(events1=c(13, 33), logRanks1 = c(1.23, 1.55), events2 = c(16, NA), logRanks2 = c(1.55, NA))
#' } \cr
#' refers to the case where one active arm (1) is considered at both stages whereas active arm 2
#' was dropped at interim. Number of events and logrank statistics are entered for the corresponding
#' comparison to control (see Examples).
#'
#' For enrichment designs, the comparison of two samples is provided for an unstratified
#' (sub-population wise) or stratified data input.\cr
#' For non-stratified (sub-population wise) data input the data sets are defined for the sub-populations
#' S1, S2, ..., F, where F refers to the full populations. Use of \code{getDataset(S1 = , S2, ..., F = )}
#' defines the data set to be used in \code{\link[=getAnalysisResults]{getAnalysisResults()}} (see examples)\cr
#' For stratified data input the data sets are defined for the strata S1, S12, S2, ..., R, where R
#' refers to the remainder of the strata such that the union of all sets is the full population.
#' Use of \code{getDataset(S1 = , S12 = , S2, ..., R = )} defines the data set to be used in
#' \code{\link[=getAnalysisResults]{getAnalysisResults()}} (see examples)\cr
#' For survival data, for enrichment designs the log-rank statistics can only be entered as stratified
#' log-rank statistics in order to provide strong control of Type I error rate. For stratified data input,
#' the variables to be specified in \code{getDataset()} are \code{cumEvents}, \code{cumExpectedEvents},
#' \code{cumVarianceEvents}, and \code{cumAllocationRatios} or \code{overallEvents}, \code{overallExpectedEvents},
#' \code{overallVarianceEvents}, and \code{overallAllocationRatios}. From this, (stratified) log-rank tests and
#' and the independent increments are calculated.
#'
#' @template return_object_dataset
#'
#' @template examples_get_dataset
#'
#' @include f_analysis_base.R
#' @include f_analysis_utilities.R
#'
#' @export
#'
getDataset <- function(..., floatingPointNumbersEnabled = FALSE) {
    dataset <- .getDataset(floatingPointNumbersEnabled = floatingPointNumbersEnabled, ...)
    if (dataset$.enrichmentEnabled && dataset$getNumberOfGroups() != 2) {
        warning("Only population enrichment data with 2 groups can be analyzed but ",
            dataset$getNumberOfGroups(), " group",
            ifelse(dataset$getNumberOfGroups() == 1, " is", "s are"), " defined",
            call. = FALSE
        )
    }
    dataset <- .resetPipeOperatorQueue(dataset)
    return(dataset)
}

#' @rdname getDataset
#' @export
getDataSet <- function(..., floatingPointNumbersEnabled = FALSE) {
    return(getDataset(floatingPointNumbersEnabled = floatingPointNumbersEnabled, ...))
}

.getDatasetMeansModelObjectsList <- function(args) {
    if (is.null(args) || length(args) == 0 || !is.list(args)) {
        return(NULL)
    }

    emmeansResults <- list()
    for (arg in args) {
        if (inherits(arg, "emmGrid")) {
            emmeansResults[[length(emmeansResults) + 1]] <- arg
        }
    }
    if (length(emmeansResults) == 0) {
        return(NULL)
    }

    argNames <- names(args)
    for (i in seq_len(length(args))) {
        arg <- args[[i]]
        if (!inherits(arg, "emmGrid")) {
            argName <- argNames[i]
            argInfo <- ""
            if (length(argName) == 1 && argName != "") {
                argInfo <- paste0(sQuote(argName), " ")
            }
            argInfo <- paste0(argInfo, "(", .arrayToString(arg), ")")
            warning("Argument ", argInfo, " will be ignored because only 'emmGrid' objects will be respected")
        }
    }

    return(emmeansResults)
}

.getStandardDeviationFromStandardError <- function(sampleSize, standardError, ...,
        dfValue = NA_real_, alpha = 0.05, lmEnabled = TRUE, stDevCalcMode = "auto") {
    qtCalcEnablbled <- length(stDevCalcMode) == 1 && !is.na(stDevCalcMode) && stDevCalcMode == "t"
    if ((qtCalcEnablbled || !lmEnabled) && !is.na(dfValue) && !is.infinite(dfValue) && dfValue > 0) {
        qValue <- stats::qt(1 - alpha / 2, df = dfValue)
        stDev <- standardError * 2 / qValue * sqrt(sampleSize)
    } else {
        stDev <- standardError * sqrt(sampleSize)
    }

    return(stDev)
}

.getDatasetMeansFromModelsByStage <- function(emmeansResults, correctGroupOrder = TRUE) {
    if (is.null(emmeansResults)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote(emmeansResults), " must be a non-empty list")
    }
    if (!is.list(emmeansResults)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote(emmeansResults), " must be a list")
    }
    if (length(emmeansResults) == 0) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote(emmeansResults), " must be not empty")
    }

    for (stage in seq_len(length(emmeansResults))) {
        if (!inherits(emmeansResults[[stage]], "emmGrid")) {
            stop(sprintf(
                paste0(
                    "%s%s must contain %s objects created by emmeans(x), ",
                    "where x is a linear model result (one object per stage; class is %s at stage %s)"
                ),
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote("emmeansResults"), sQuote("emmGrid"),
                .getClassName(emmeansResults[[stage]]), stage
            ))
        }
    }

    stages <- integer(0)
    groups <- integer(0)
    means <- numeric(0)
    stDevs <- numeric(0)
    sampleSizes <- numeric(0)

    lmEnabled <- TRUE
    tryCatch(
        {
            modelCall <- emmeansResults[[1]]@model.info$call
            modelFunction <- as.character(modelCall)[1]
            lmEnabled <- modelFunction == "lm"
            if (!grepl(paste0("::", modelFunction), modelFunction)) {
                packageName <- .getPackageName(modelFunction)
                if (!is.na(packageName)) {
                    modelFunction <- paste0(packageName, "::", modelFunction)
                }
            }

            if (lmEnabled) {
                warning("When using ", modelFunction, "() ",
                    "the estimated marginal means and standard deviations can be inaccurate ",
                    "and analysis results based on this values may be imprecise",
                    call. = FALSE
                )
            } else {
                warning("Using ", modelFunction, " emmeans result objects as ",
                    "arguments of getDataset() is experminental in this rpact version and not fully validated",
                    call. = FALSE
                )
            }
        },
        error = function(e) {
            warning("Using emmeans result objects as ",
                "arguments of getDataset() is experminental in this rpact version and not fully validated",
                call. = FALSE
            )
        }
    )

    stDevCalcMode <- getOption("rpact.dataset.stdev.calc.mode", "auto") # auto, sigma, norm, t
    for (stage in seq_len(length(emmeansResults))) {
        emmeansResult <- emmeansResults[[stage]]
        emmeansResultsSummary <- summary(emmeansResult)
        emmeansResultsList <- as.list(emmeansResult)

        if (is.null(emmeansResultsSummary[["emmean"]])) {
            stop(
                C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                "the objects in summary(emmeansResults) must contain the field 'emmean'"
            )
        }
        for (expectedField in c("sigma", "extras")) {
            if (is.null(emmeansResultsList[[expectedField]])) {
                stop(
                    C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                    "the objects in as.list(emmeansResults) must contain the field ", sQuote(expectedField)
                )
            }
        }

        numberOfGroups <- length(emmeansResultsSummary$emmean)
        rpactGroupNumbers <- 1:numberOfGroups
        if (correctGroupOrder) {
            rpactGroupNumbers <- 1
            if (numberOfGroups > 1) {
                rpactGroupNumbers <- c(2:numberOfGroups, rpactGroupNumbers)
            }
        }
        for (group in seq_len(length(emmeansResultsSummary$emmean))) {
            stages <- c(stages, stage)
            groups <- c(groups, group)
            rpactGroupNumber <- rpactGroupNumbers[group]

            standardError <- emmeansResultsSummary$SE[rpactGroupNumber]

            sampleSize <- emmeansResultsList$extras[rpactGroupNumber, ]
            meanValue <- emmeansResultsSummary$emmean[rpactGroupNumber]
            dfValue <- emmeansResultsSummary$df[rpactGroupNumber]
            if (length(stDevCalcMode) == 1 && !is.na(stDevCalcMode) && stDevCalcMode == "sigma") {
                # pooled standard deviation from emmeans
                stDev <- emmeansResultsList$sigma
            } else {
                stDev <- .getStandardDeviationFromStandardError(sampleSize, standardError,
                    dfValue = dfValue, lmEnabled = lmEnabled, stDevCalcMode = stDevCalcMode
                )
            }

            means <- c(means, meanValue)
            stDevs <- c(stDevs, stDev)
            sampleSizes <- c(sampleSizes, sampleSize)
        }
    }

    data <- data.frame(
        stages = stages,
        groups = groups,
        means = means,
        stDevs = stDevs,
        sampleSizes = sampleSizes
    )
    data <- data[order(data$stages, data$groups), ]
    dataWide <- stats::reshape(data = data, direction = "wide", idvar = "stages", timevar = "groups")
    colnames(dataWide) <- gsub("\\.", "", colnames(dataWide))
    return(getDataset(dataWide))
}

.optionalArgsContainsDatasets <- function(...) {
    args <- list(...)
    if (length(args) == 0) {
        return(FALSE)
    }

    for (arg in args) {
        if (inherits(arg, "Dataset")) {
            return(TRUE)
        }
    }
    return(FALSE)
}

.getSubsetsFromArgs <- function(...) {
    args <- list(...)
    if (length(args) == 0) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "one or more subset datasets expected")
    }

    subsetNames <- names(args)
    if (is.null(subsetNames)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all subsets must be named")
    }

    if (!("R" %in% subsetNames) && !("F" %in% subsetNames)) {
        stop(
            C_EXCEPTION_TYPE_MISSING_ARGUMENT,
            '"R" (stratified analysis)" or "F" (non-stratified analysis) must be defined as subset'
        )
    }

    subsetNumbers <- gsub("\\D", "", subsetNames)
    subsetNumbers <- subsetNumbers[subsetNumbers != ""] #  & nchar(subsetNumbers) == 1
    if (length(subsetNumbers) == 0) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all subset names (",
            .arrayToString(subsetNames), ") must be \"S[n]\", \"R\", or \"F\", ",
            "where [n] is a number with increasing digits (starting with 1)"
        )
    }

    stratifiedInput <- "R" %in% subsetNames

    subsetNumbers <- paste0(subsetNumbers, collapse = "")
    subsetNumbers <- strsplit(subsetNumbers, "")[[1]]
    subsetNumbers <- as.integer(subsetNumbers)
    gMax <- max(subsetNumbers) + 1
    validSubsetNames <- .createSubsetsByGMax(gMax, stratifiedInput = stratifiedInput, all = FALSE)
    for (i in seq_len(length(subsetNames))) {
        subsetName <- subsetNames[i]
        if (subsetName == "" && !inherits(args[[i]], "TrialDesign")) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all subsets must be named")
        }

        if (subsetName != "" && !(subsetName %in% validSubsetNames)) {
            suffix <- ifelse(stratifiedInput, " (stratified analysis)", " (non-stratified analysis)")
            if (length(validSubsetNames) < 10) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "invalid subset name (", subsetName, "); ",
                    "valid names are ", .arrayToString(validSubsetNames), suffix
                )
            } else {
                restFull <- ifelse(stratifiedInput, '"R"', '"F"')
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "invalid subset name (", subsetName, "): ",
                    "all subset names must be \"S[n]\" or ", restFull, ", ",
                    "where [n] is a number with increasing digits", suffix
                )
            }
        }
    }
    
    subsetNames <- subsetNames[subsetNames != ""]
    subsets <- NULL
    subsetType <- NA_character_
    emptySubsetNames <- validSubsetNames[!(validSubsetNames %in% subsetNames)]
    for (subsetName in subsetNames) {
        subset <- args[[subsetName]]
        if (is.null(subset) || (!.isResultObjectBaseClass(subset) && is.na(subset))) {
            emptySubsetNames <- c(emptySubsetNames, subsetName)
        } else {
            if (!.isDataset(subset)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "subset ", subsetName, " is not a dataset (is ", .getClassName(subset), ")"
                )
            }
            if (!is.na(subsetType) && subsetType != .getClassName(subset)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "all subsets must have the same type (found ", subsetType, " and ", .getClassName(subset), ")"
                )
            }
            subsetType <- .getClassName(subset)
            if (is.null(subset[[".data"]])) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "subset ", subsetName, " does not contain field '.data'"
                )
            }
            subset <- subset$.data
            subset$subset <- rep(subsetName, nrow(subset))
            if (is.null(subsets)) {
                subsets <- subset
            } else {
                subsets <- rbind(subsets, subset)
            }
        }
    }

    if (length(emptySubsetNames) > 0) {
        emptySubsetNames <- unique(emptySubsetNames)
        template <- subsets[subsets$subset == ifelse(stratifiedInput, "R", "F"), ]
        colNames <- colnames(template)
        colNames <- colNames[!(colNames %in% c("stage", "group", "subset"))]
        for (colName in colNames) {
            template[[colName]] <- rep(NA_real_, nrow(template))
        }

        for (subsetName in emptySubsetNames) {
            template$subset <- rep(subsetName, nrow(template))
            subsets <- rbind(subsets, template)
        }

        if (length(emptySubsetNames) == 1) {
            warning("The undefined subset ", emptySubsetNames,
                " was defined as empty subset",
                call. = FALSE
            )
        } else {
            warning(gettextf(
                "The %s undefined subsets %s were defined as empty subsets",
                length(emptySubsetNames), .arrayToString(emptySubsetNames)
            ), call. = FALSE)
        }
    }

    return(subsets)
}

.validateEnrichmentDataFrameAtFirstStage <- function(dataFrame, params) {
    dataFrameStage1 <- dataFrame[dataFrame$stage == 1, ]
    for (param in params) {
        paramValue <- dataFrameStage1[[param]]
        if (any(is.null(paramValue) || any(is.infinite(paramValue)))) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                gettextf(
                    "all %s values (%s) at first stage must be valid",
                    sQuote(param), .arrayToString(paramValue, maxLength = 10)
                )
            )
        }
        if (any(is.na(paramValue))) {
            subsets <- unique(dataFrame$subset)
            for (s in subsets) {
                subData <- dataFrame[dataFrame$subset == s, ]
                subsetParamValues <- subData[[param]]
                if (!all(is.na(subsetParamValues)) && any(is.na(subsetParamValues[subData$stage == 1]))) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        gettextf(
                            "all %s values (%s) at first stage must be valid (NA is not allowed)",
                            sQuote(param), .arrayToString(paramValue, maxLength = 10)
                        )
                    )
                }
            }
        }
    }
}

.getEndpointSpecificDataFrameParameterNames <- function(dataFrame) {
    paramNames <- colnames(dataFrame)
    paramNames <- paramNames[!(paramNames %in% c("stage", "group", "subset"))]
    return(paramNames)
}

.validateEnrichmentDataFrameDeselection <- function(dataFrame) {
    paramNames <- .getEndpointSpecificDataFrameParameterNames(dataFrame)
    for (i in 1:nrow(dataFrame)) {
        row <- dataFrame[i, paramNames]
        if (any(is.na(row)) && !all(is.na(row))) {
            stop(
                C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                gettextf(
                    paste0(
                        "inconsistent deselection in group %s at stage %s (",
                        "%s: all or none must be NA)"
                    ),
                    dataFrame$group[i], dataFrame$stage[i], .arrayToString(paramNames, maxCharacters = 40)
                )
            )
        }
    }

    subsets <- unique(dataFrame$subset)
    for (s in subsets) {
        deselectedStage <- 0
        for (stage in unique(dataFrame$stage)) {
            subData <- dataFrame[dataFrame$subset == s & dataFrame$stage == stage, paramNames]

            if (deselectedStage > 0 && !all(is.na(subData))) {
                stop(
                    C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                    gettextf(paste0(
                        "%s was deselected at stage %s ",
                        "and therefore must be also deselected in the following stages, ",
                        "but is no longer deselected in stage %s"
                    ), s, deselectedStage, stage)
                )
            }

            if (any(is.na(subData))) {
                deselectedStage <- stage
            }
        }
    }
}

.validateEnrichmentDataFrameMeans <- function(dataFrame) {
    if (any(na.omit(dataFrame$stDev) <= 0) || any(na.omit(dataFrame$overallStDev) <= 0)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all standard deviations must be > 0")
    }
    if (any(na.omit(dataFrame$sampleSize) <= 0) || any(na.omit(dataFrame$overallSampleSize) <= 0)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all sample sizes must be > 0")
    }

    .validateEnrichmentDataFrameAtFirstStage(dataFrame,
        params = c("sampleSize", "overallSampleSize", "mean", "overallMean", "stDev", "overallStDev")
    )

    .validateEnrichmentDataFrameDeselection(dataFrame)

    subsets <- unique(dataFrame$subset)
    if ("F" %in% subsets) {
        subsets <- subsets[subsets != "F"]
        fullData <- dataFrame[dataFrame$subset == "F", ]
        for (s in subsets) {
            for (stage in unique(dataFrame$stage)) {
                for (group in unique(dataFrame$group)) {
                    subData <- dataFrame[dataFrame$subset == s & dataFrame$stage == stage & dataFrame$group == group, ]

                    stDevFull <- na.omit(fullData$stDev[fullData$stage == stage & fullData$group == group])
                    stDevSubset <- na.omit(subData$stDev)
                    if (length(stDevFull) > 0 && length(stDevSubset) > 0 && any(stDevFull <= stDevSubset)) {
                        stop(
                            C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                            gettextf(
                                "'stDev' F (%s) must be > 'stDev' %s (%s) in group %s at stage %s",
                                .arrayToString(stDevFull), s,
                                .arrayToString(stDevSubset), group, stage
                            )
                        )
                    }

                    sampleSizeFull <- na.omit(fullData$sampleSize[fullData$stage == stage & fullData$group == group])
                    sampleSizeSubset <- na.omit(subData$sampleSize)
                    if (length(sampleSizeFull) > 0 && length(sampleSizeSubset) > 0 && any(sampleSizeFull < sampleSizeSubset)) {
                        stop(
                            C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                            gettextf(
                                "'sampleSize' F (%s) must be >= 'sampleSize' %s (%s) in group %s at stage %s",
                                .arrayToString(sampleSizeFull), s,
                                .arrayToString(sampleSizeSubset), group, stage
                            )
                        )
                    }
                }
            }
        }
    }
}

.validateEnrichmentDataFrameSurvival <- function(dataFrame) {
    if (any(na.omit(dataFrame$event) < 0) || any(na.omit(dataFrame$overallEvent) < 0)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all events must be >= 0")
    }

    .validateEnrichmentDataFrameAtFirstStage(dataFrame,
        params = c("event", "overallEvent")
    )

    .validateEnrichmentDataFrameDeselection(dataFrame)

    subsets <- unique(dataFrame$subset)
    if ("F" %in% subsets) {
        subsets <- subsets[subsets != "F"]
        fullData <- dataFrame[dataFrame$subset == "F", ]
        for (s in subsets) {
            for (stage in unique(dataFrame$stage)) {
                for (group in unique(dataFrame$group)) {
                    subData <- dataFrame[dataFrame$subset == s & dataFrame$stage == stage & dataFrame$group == group, ]

                    eventFull <- na.omit(fullData$event[fullData$stage == stage & fullData$group == group])
                    eventSubset <- na.omit(subData$event)
                    if (length(eventFull) > 0 && length(eventSubset) > 0 && any(eventFull < eventSubset)) {
                        stop(
                            C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                            gettextf(
                                "'event' F (%s) must be >= 'event' %s (%s) in group %s at stage %s",
                                .arrayToString(eventFull), s,
                                .arrayToString(eventSubset), group, stage
                            )
                        )
                    }
                }
            }
        }
    }
}

.validateEnrichmentDataFrameRates <- function(dataFrame) {
    if (any(na.omit(dataFrame$sampleSize) <= 0) || any(na.omit(dataFrame$overallSampleSize) <= 0)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all sample sizes must be > 0")
    }

    .validateEnrichmentDataFrameAtFirstStage(dataFrame,
        params = c("sampleSize", "overallSampleSize")
    )

    .validateEnrichmentDataFrameDeselection(dataFrame)

    subsets <- unique(dataFrame$subset)
    if ("F" %in% subsets) {
        subsets <- subsets[subsets != "F"]
        fullData <- dataFrame[dataFrame$subset == "F", ]
        for (s in subsets) {
            for (stage in unique(dataFrame$stage)) {
                for (group in unique(dataFrame$group)) {
                    subData <- dataFrame[dataFrame$subset == s & dataFrame$stage == stage & dataFrame$group == group, ]

                    sampleSizeFull <- na.omit(fullData$sampleSize[fullData$stage == stage & fullData$group == group])
                    sampleSizeSubset <- na.omit(subData$sampleSize)
                    if (length(sampleSizeFull) > 0 && length(sampleSizeSubset) > 0 && any(sampleSizeFull < sampleSizeSubset)) {
                        stop(
                            C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                            gettextf(
                                "'sampleSize' F (%s) must be >= 'sampleSize' %s (%s) in group %s at stage %s",
                                .arrayToString(sampleSizeFull), s,
                                .arrayToString(sampleSizeSubset), group, stage
                            )
                        )
                    }
                }
            }
        }
    }

    .validateEnrichmentDataFrameSurvival(dataFrame)
}

.validateEnrichmentDataFrameHasConsistentNumberOfStages <- function(dataFrame) {
    subsets <- unique(dataFrame$subset)
    kMaxList <- list()
    for (s in subsets) {
        subsetStages <- as.integer(sort(unique(na.omit(as.character(dataFrame$stage[dataFrame$subset == s])))))
        kMax <- max(subsetStages)
        if (!isTRUE(all.equal(1:kMax, subsetStages))) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                gettextf("subset %s has incomplete stages (%s)", s, .arrayToString(subsetStages))
            )
        }

        kMaxList[[s]] <- kMax
    }

    kMax <- unique(unlist(kMaxList))
    if (length(kMax) > 1) {
        stop(
            C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
            "all subsets must have the identical number of stages defined (kMax: ", .listToString(kMaxList), ")"
        )
    }
}

.validateEnrichmentDataFrame <- function(dataFrame) {
    paramNames <- colnames(dataFrame)
    if (any(grepl("(S|s)tDev", paramNames))) {
        .validateEnrichmentDataFrameMeans(dataFrame)
    } else if (any(grepl("(S|s)ampleSize", paramNames)) && any(grepl("(E|e)vent", paramNames))) {
        .validateEnrichmentDataFrameRates(dataFrame)
    } else if (any(grepl("(L|l)ogRank", paramNames)) || any(grepl("(E|e)xpectedEvent", paramNames))) {
        .validateEnrichmentDataFrameSurvival(dataFrame)
    } else {
        print(paramNames)
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "could not identify the endpoint of the specified dataset")
    }

    subsets <- unique(dataFrame$subset)
    if ("R" %in% subsets) {
        paramNames <- .getEndpointSpecificDataFrameParameterNames(dataFrame)
        paramName <- paramNames[1]
        subsets <- subsets[subsets != "R"]
        subsets <- subsets[grepl("^S\\d$", subsets)]
        if (length(subsets) > 0) {
            restData <- dataFrame[dataFrame$subset == "R", ]
            for (s in subsets) {
                stages <- unique(dataFrame$stage)
                stages <- stages[stages != 1]
                if (length(stages) > 0) {
                    for (stage in stages) {
                        for (group in unique(dataFrame$group)) {
                            subData <- dataFrame[dataFrame$subset == s & dataFrame$stage == stage & dataFrame$group == group, ]

                            paramValueRest <- restData[[paramName]][restData$stage == stage & restData$group == group]
                            paramValueSubset <- subData[[paramName]]
                            if (length(paramValueRest) > 0 && length(paramValueSubset) > 0 &&
                                    any(is.na(paramValueSubset)) && !all(is.na(paramValueRest))) {
                                stop(
                                    C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                                    gettextf(
                                        paste0(
                                            "if %s is deselected (NA) then R also must be deselected (NA) but, e.g., ",
                                            "%s R is %s in group %s at stage %s"
                                        ),
                                        s, sQuote(paramName), .arrayToString(paramValueRest, vectorLookAndFeelEnabled = TRUE),
                                        group, stage
                                    )
                                )
                            }
                        }
                    }
                }
            }
        }
    }

    .validateEnrichmentDataFrameHasConsistentNumberOfStages(dataFrame)
}

.getEnrichmentDataFrameFromArgs <- function(...) {
    dataFrame <- .getSubsetsFromArgs(...)

    validColumns <- c()
    for (j in 1:ncol(dataFrame)) {
        if (!all(is.na(dataFrame[, j]))) {
            validColumns <- c(validColumns, j)
        }
    }
    if (length(validColumns) > 0) {
        dataFrame <- dataFrame[, validColumns]
    }

    return(dataFrame)
}

.getEnrichmentDatasetFromArgs <- function(...) {
    dataFrame <- .getEnrichmentDataFrameFromArgs(...)
    .validateEnrichmentDataFrame(dataFrame)
    dataFrame <- .getWideFormat(dataFrame)
    return(.getDataset(dataFrame = dataFrame))
}

.getDatasetExample <- function(exampleType) {
    if (exampleType == "means") {
        return(getDataset(
            n1 = c(13, 25),
            n2 = c(15, NA),
            n3 = c(14, 27),
            n4 = c(12, 29),
            means1 = c(24.2, 22.2),
            means2 = c(18.8, NA),
            means3 = c(26.7, 27.7),
            means4 = c(9.2, 12.2),
            stDevs1 = c(24.4, 22.1),
            stDevs2 = c(21.2, NA),
            stDevs3 = c(25.6, 23.2),
            stDevs4 = c(21.5, 22.7)
        ))
    } else if (exampleType == "rates") {
        return(getDataset(
            n1 = c(23, 25),
            n2 = c(25, NA),
            n3 = c(24, 27),
            n4 = c(22, 29),
            events1 = c(15, 12),
            events2 = c(19, NA),
            events3 = c(18, 22),
            events4 = c(12, 13)
        ))
    } else if (exampleType == "survival") {
        return(getDataset(
            events1   = c(25, 32),
            events2   = c(18, NA),
            events3   = c(22, 36),
            logRanks1 = c(2.2, 1.8),
            logRanks2 = c(1.99, NA),
            logRanks3 = c(2.32, 2.11)
        ))
    }

    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'exampleType' (", exampleType, ") is not allowed")
}

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
Dataset <- R6::R6Class("Dataset",
    inherit = ParameterSet,
    public = list(
        .data = NULL,
        .plotSettings = NULL,
        .id = NULL,
        .description = NULL,
        .floatingPointNumbersEnabled = NULL,
        .kMax = NULL,
        .enrichmentEnabled = NULL,
        .inputType = NULL,
        .design = NULL,
        stages = NULL,
        groups = NULL,
        subsets = NULL,
        initialize = function(dataFrame, ..., floatingPointNumbersEnabled = FALSE, enrichmentEnabled = FALSE, .design = NULL) {
            super$initialize(...)

            self$.floatingPointNumbersEnabled <- floatingPointNumbersEnabled
            self$.enrichmentEnabled <- enrichmentEnabled
            self$.design <- .design

            self$.plotSettings <- PlotSettings$new()

            self$.id <- NA_integer_
            self$.description <- NA_character_
            self$.inputType <- NA_character_

            if (!missing(dataFrame)) {
                self$.initByDataFrame(dataFrame)
                self$.kMax <- self$getNumberOfStages()
                if (!self$.enrichmentEnabled) {
                    self$.validateDataset()
                }
            }
        },
        getPlotSettings = function() {
            return(self$.plotSettings)
        },
        show = function(showType = 1, digits = NA_integer_) {
            "Method for automatically printing dataset objects"
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            self$.resetCat()

            if (!is.null(showType) && length(showType) == 1 && !is.na(showType) &&
                    is.character(showType) && showType == "rcmd") {
                s <- strsplit(getObjectRCode(self, stringWrapParagraphWidth = NULL), "), *")[[1]]
                s[2:length(s)] <- paste0("\t", s[2:length(s)])
                s <- paste0(s, collapse = "),\n")
                cat(s, "\n")
            } else if (showType == 2) {
                super$.show(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                self$.showParametersOfOneGroup(self$.getUserDefinedParameters(),
                    title = self$.toString(startWithUpperCase = TRUE), orderByParameterName = FALSE,
                    consoleOutputEnabled = consoleOutputEnabled
                )

                self$.showParametersOfOneGroup(self$.getGeneratedParameters(),
                    title = "Calculated data", orderByParameterName = FALSE,
                    consoleOutputEnabled = consoleOutputEnabled
                )

                self$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)

                if (!is.na(self$.description) && nchar(self$.description) > 0) {
                    self$.cat("Description: ", self$.description, "\n\n",
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

            if (!self$.paramExists(dataFrame, "stage") && !self$.paramExists(dataFrame, "stages")) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'dataFrame' must contain parameter 'stages' or 'stage'"
                )
            }

            self$stages <- as.integer(self$.getValuesByParameterName(dataFrame, c("stages", "stage")))
            if (!self$.enrichmentEnabled && length(unique(self$stages)) < length(self$stages)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'stages' (", .arrayToString(self$stages),
                    ") must be a unique vector of stage numbers"
                )
            }
            self$groups <- rep(1L, length(self$stages))

            self$.setParameterType("groups", C_PARAM_USER_DEFINED)
            self$.setParameterType("stages", C_PARAM_USER_DEFINED)

            if (any(grepl("^subsets?\\d*$", colnames(dataFrame)))) {
                numberOfTreatmentGroups <- self$.getNumberOfGroups(dataFrame, c(C_KEY_WORDS_SAMPLE_SIZES, C_KEY_WORDS_LOG_RANKS))
                self$subsets <- character()
                for (group in 1:numberOfTreatmentGroups) {
                    suffix <- ifelse(any(grepl("^subsets?\\d+$", colnames(dataFrame))), group, "")
                    self$subsets <- c(self$subsets, self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_SUBSETS, suffix = suffix))
                }
                self$.setParameterType("subsets", C_PARAM_USER_DEFINED)
            } else {
                self$subsets <- rep(NA_character_, length(self$stages))
            }
        },
        .validateDataset = function() {
            .assertIsValidKMax(kMax = self$getNumberOfStages())

            for (var in names(self)) {
                values <- self[[var]]
                if (any(is.nan(values)) || any(is.infinite(values))) {
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'", var, "' (", .arrayToString(values),
                        ") contains illegal values, i.e., something went wrong"
                    )
                }
            }
        },
        .validateValues = function(values, name) {
            if (self$.enrichmentEnabled) {
                return(invisible())
            }

            l1 <- length(unique(self$stages))
            l2 <- length(values)
            if (l1 != l2) {
                stop(
                    C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                    "there ", ifelse(l1 == 1, paste("is", l1, "stage"),
                        paste("are", l1, "stages")
                    ), " defined",
                    " (", .arrayToString(unique(self$stages)), ") and '", name, "' has length ", l2
                )
            }
        },
        .recreateDataFrame = function() {
            self$.data <- data.frame(
                stage = factor(self$stages),
                group = factor(self$groups),
                subset = factor(self$subsets)
            )
        },
        .setDataToVariables = function() {
            self$stages <- as.integer(self$.data$stage)
            self$groups <- as.integer(self$.data$group)
            self$subsets <- as.character(self$.data$subset)
        },
        .fillWithNAs = function(kMax) {
            numberOfStages <- self$getNumberOfStages()
            self$.kMax <- numberOfStages
            if (numberOfStages >= kMax) {
                return(invisible())
            }

            numberOfGroups <- self$getNumberOfGroups(survivalCorrectionEnabled = FALSE)
            if (self$.enrichmentEnabled) {
                for (stage in (numberOfStages + 1):kMax) {
                    for (group in 1:numberOfGroups) {
                        for (subset in levels(self$.data$subset)) {
                            self$stages <- c(self$stages, stage)
                            self$groups <- c(self$groups, group)
                            self$subsets <- c(self$subsets, subset)
                        }
                    }
                }
            } else {
                for (stage in (numberOfStages + 1):kMax) {
                    for (group in 1:numberOfGroups) {
                        self$stages <- c(self$stages, stage)
                        self$groups <- c(self$groups, group)
                        self$subsets <- c(self$subsets, NA_character_)
                    }
                }
            }
        },
        .trim = function(kMax) {
            if (is.na(kMax)) {
                kMax <- self$.kMax
            }
            numberOfStages <- self$getNumberOfStages(FALSE)
            if (numberOfStages <= kMax) {
                return(invisible(numeric(0)))
            }

            indices <- which(self$stages <= kMax)

            self$stages <- self$stages[indices]
            self$groups <- self$groups[indices]
            self$subsets <- self$subsets[indices]

            return(indices)
        },
        .orderDataByStageAndGroup = function() {
            if (self$.enrichmentEnabled) {
                dat <- self$.data
                dat$char <- gsub("\\d", "", as.character(self$.data$subset))
                dat$char[dat$char == "R"] <- "Z"
                dat$char[dat$char == "F"] <- "Z"
                dat$num <- as.integer(gsub("\\D", "", as.character(self$.data$subset)))

                self$.data <- self$.data[order(self$.data$stage, self$.data$group, dat$char, dat$num), ]
            } else {
                self$.data <- self$.data[order(self$.data$stage, self$.data$group), ]
            }
        },
        .getNumberOfNAsToAdd = function(kMax) {
            n <- kMax - self$getNumberOfStages()
            if (n <= 0) {
                return(0)
            }

            n <- n * self$getNumberOfGroups(survivalCorrectionEnabled = FALSE)
            if (self$.enrichmentEnabled) {
                n <- n * self$getNumberOfSubsets()
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
                if (self$.paramExists(dataFrame, key)) {
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
            values <- self$.data[[paramName]]
            valueLevels <- self$.getValueLevels(values)
            if (!all(paramValues %in% valueLevels)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", paramName, "' (", .arrayToString(paramValues),
                    ") out of range [", .arrayToString(valueLevels), "]"
                )
            }
            return(values)
        },
        .getIndexValues = function(paramName, paramValues, subset = NA_character_) {
            values <- self$.getValues(paramName, paramValues)
            if (all(is.na(subset))) {
                return(which(values %in% paramValues))
            }

            self$.assertIsValidSubset(subset)
            return(which(values %in% paramValues & self$.data$subset %in% subset))
        },
        .assertIsValidSubset = function(subset) {
            for (s in subset) {
                if (!(s %in% levels(self$.data$subset))) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'subset' (", s,
                        ") is not a defined value [", .arrayToString(levels(self$.data$subset)), "]"
                    )
                }
            }
        },
        .getIndices = function(..., stage, group, subset = NA_character_) {
            if (is.null(self$.data)) {
                stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'.data' must be defined")
            }

            if (!is.null(stage) && !any(is.na(stage)) && all(stage < 0)) {
                index <- 1:self$getNumberOfStages()
                stage <- index[!(index %in% abs(stage))]
            }

            if (!is.null(group) && !any(is.na(group)) && all(group < 0)) {
                index <- 1:self$getNumberOfGroups(survivalCorrectionEnabled = FALSE)
                group <- index[!(index %in% abs(group))]
            }

            # stage only and optional subset
            if (!is.null(group) && length(group) == 1 && is.na(group)) {
                return(self$.getIndexValues("stage", stage, subset))
            }

            # group only and optional subset
            if (!is.null(stage) && length(stage) == 1 && is.na(stage)) {
                return(self$.getIndexValues("group", group, subset))
            }

            # stage and group and optional subset
            stageValues <- self$.getValues("stage", stage)
            groupValues <- self$.getValues("group", group)
            if (all(is.na(subset))) {
                return(which(stageValues %in% stage & groupValues %in% group))
            }

            self$.assertIsValidSubset(subset)
            return(which(stageValues %in% stage & groupValues %in% group & self$.data$subset %in% subset))
        },
        .getValidatedFloatingPointNumbers = function(x, parameterName = "Sample sizes") {
            if (self$.floatingPointNumbersEnabled) {
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
                if (self$.paramExists(dataFrame, paste0(key, suffix))) {
                    return(TRUE)
                }
            }
            return(FALSE)
        },
        .getNumberOfGroups = function(dataFrame, keyWords) {
            for (group in 2:1000) {
                if (!self$.keyWordExists(dataFrame, keyWords, group)) {
                    return(group - 1)
                }
            }
            return(1)
        },
        .getValidatedStage = function(stage = NA_integer_) {
            if (all(is.na(stage))) {
                stage <- c(1:self$getNumberOfStages())
            }
            return(stage)
        },
        getNumberOfGroups = function(survivalCorrectionEnabled = TRUE) {
            data <- stats::na.omit(self$.data)
            if (!survivalCorrectionEnabled) {
                return(length(levels(data$group)))
            }
            return(length(levels(data$group)) + ifelse(inherits(self, "DatasetSurvival") || inherits(self, "DatasetSurvival"), 1, 0))
        },
        getNumberOfStages = function(naOmitEnabled = TRUE) {
            if (naOmitEnabled) {
                colNames <- colnames(self$.data)
                validColNames <- character()
                for (colName in colNames) {
                    colValues <- self$.data[, colName]
                    if (length(colValues) > 0 && !all(is.na(colValues))) {
                        validColNames <- c(validColNames, colName)
                    }
                }
                subData <- stats::na.omit(self$.data[, validColNames])
                numberOfStages <- length(unique(as.character(subData$stage)))
                if (numberOfStages == 0) {
                    print(self$.data[, validColNames])
                    stop(
                        C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                        ".data seems to contain an invalid column"
                    )
                }
                return(numberOfStages)
            }
            return(length(levels(self$.data$stage)))
        },
        getNumberOfSubsets = function() {
            return(length(levels(self$.data$subset)))
        },
        isDatasetMeans = function() {
            return(inherits(self, "DatasetMeans"))
        },
        isDatasetRates = function() {
            return(inherits(self, "DatasetRates"))
        },
        isDatasetSurvival = function() {
            return(inherits(self, "DatasetSurvival"))
        },
        isStratified = function() {
            return(self$.enrichmentEnabled && "R" %in% levels(self$.data$subset))
        },
        setId = function(id) {
            self$.id <- as.integer(id)
        },
        getId = function() {
            return(self$.id)
        },
        setDescription = function(description) {
            self$.description <- description
        },
        getDescription = function() {
            return(self$.description)
        },
        .toString = function(startWithUpperCase = FALSE) {
            s <- "dataset of "
            if (self$.enrichmentEnabled) {
                s <- paste0(s, "enrichment ")
            } else if (self$getNumberOfGroups() > 2) {
                s <- paste0(s, "multi-arm ")
            }

            if (self$isDatasetMeans()) {
                s <- paste0(s, "means")
            } else if (self$isDatasetRates()) {
                s <- paste0(s, "rates")
            } else if (self$isDatasetSurvival()) {
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
DatasetMeans <- R6::R6Class("DatasetMeans",
    inherit = Dataset,
    public = list(
        sampleSizes = NULL,
        means = NULL,
        stDevs = NULL,
        overallSampleSizes = NULL,
        overallMeans = NULL,
        overallStDevs = NULL,
        getSampleSize = function(stage, group = 1, subset = NA_character_) {
            return(self$.data$sampleSize[self$.getIndices(stage = stage, group = group, subset = subset)])
        },
        getMean = function(stage, group = 1, subset = NA_character_) {
            return(self$.data$mean[self$.getIndices(stage = stage, group = group, subset = subset)])
        },
        getStDev = function(stage, group = 1, subset = NA_character_) {
            return(self$.data$stDev[self$.getIndices(stage = stage, group = group, subset = subset)])
        },
        getSampleSizes = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(self$.data$sampleSize[self$.getIndices(stage = self$.getValidatedStage(stage), group = group, subset = subset)])
        },
        getMeans = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(self$.data$mean[self$.getIndices(stage = self$.getValidatedStage(stage), group = group, subset = subset)])
        },
        getStDevs = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(self$.data$stDev[self$.getIndices(stage = self$.getValidatedStage(stage), group = group, subset = subset)])
        },
        getSampleSizesUpTo = function(to, group = 1, subset = NA_character_) {
            return(self$.data$sampleSize[self$.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getMeansUpTo = function(to, group = 1, subset = NA_character_) {
            return(self$.data$mean[self$.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getStDevsUpTo = function(to, group = 1, subset = NA_character_) {
            return(self$.data$stDev[self$.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getOverallSampleSize = function(stage, group = 1, subset = NA_character_) {
            return(self$.data$overallSampleSize[self$.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallMean = function(stage, group = 1, subset = NA_character_) {
            return(self$.data$overallMean[self$.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallStDev = function(stage, group = 1, subset = NA_character_) {
            return(self$.data$overallStDev[self$.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallSampleSizes = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(self$.data$overallSampleSize[self$.getIndices(stage = self$.getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallMeans = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(self$.data$overallMean[self$.getIndices(stage = self$.getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallStDevs = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(self$.data$overallStDev[self$.getIndices(stage = self$.getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallSampleSizesUpTo = function(to, group = 1, subset = NA_character_) {
            return(self$.data$overallSampleSize[self$.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getOverallMeansUpTo = function(to, group = 1, subset = NA_character_) {
            return(self$.data$overallMean[self$.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getOverallStDevsUpTo = function(to, group = 1, subset = NA_character_) {
            return(self$.data$overallStDev[self$.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        .initByDataFrame = function(dataFrame) {
            super$.initByDataFrame(dataFrame)

            # case: one mean - stage wise
            if (self$.paramExists(dataFrame, C_KEY_WORDS_SAMPLE_SIZES)) {
                self$.inputType <- "stagewise"
                self$sampleSizes <- self$.getValidatedFloatingPointNumbers(self$.getValuesByParameterName(
                    dataFrame,
                    C_KEY_WORDS_SAMPLE_SIZES
                ), parameterName = "Sample sizes")
                self$.validateValues(self$sampleSizes, "n")
                if (any(stats::na.omit(self$sampleSizes) <= 0)) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "all sample sizes must be > 0, but 'n' = ",
                        .arrayToString(self$sampleSizes, vectorLookAndFeelEnabled = TRUE)
                    )
                }

                self$means <- self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_MEANS)
                self$.validateValues(self$means, "means")

                self$stDevs <- self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_ST_DEVS)
                self$.validateValues(self$stDevs, "stDevs")
            }

            # case: one mean - overall
            else if (self$.paramExists(dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES)) {
                self$.inputType <- "overall"
                self$overallSampleSizes <- self$.getValidatedFloatingPointNumbers(self$.getValuesByParameterName(
                    dataFrame,
                    C_KEY_WORDS_OVERALL_SAMPLE_SIZES
                ), parameterName = "Cumulative sample sizes ")
                self$.validateValues(self$overallSampleSizes, "overallSampleSizes")

                self$overallMeans <- self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_MEANS)
                self$.validateValues(self$overallMeans, "overallMeans")

                self$overallStDevs <- self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_ST_DEVS)
                self$.validateValues(self$overallStDevs, "overallStDevs")
            }

            # case: two or more means - stage wise
            else if (self$.paramExists(dataFrame, paste0(C_KEY_WORDS_SAMPLE_SIZES, 1)) &&
                    self$.paramExists(dataFrame, paste0(C_KEY_WORDS_SAMPLE_SIZES, 2))) {
                self$.inputType <- "stagewise"
                numberOfTreatmentGroups <- self$.getNumberOfGroups(dataFrame, C_KEY_WORDS_SAMPLE_SIZES)
                self$stages <- rep(self$stages, numberOfTreatmentGroups)
                self$groups <- integer(0)
                self$sampleSizes <- numeric(0)
                self$means <- numeric(0)
                self$stDevs <- numeric(0)
                for (group in 1:numberOfTreatmentGroups) {
                    sampleSizesTemp <- self$.getValidatedFloatingPointNumbers(self$.getValuesByParameterName(
                        dataFrame, C_KEY_WORDS_SAMPLE_SIZES,
                        suffix = group
                    ), parameterName = "Sample sizes")
                    self$.validateValues(sampleSizesTemp, paste0("n", group))
                    if (any(stats::na.omit(sampleSizesTemp) <= 0)) {
                        stop(
                            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                            "all sample sizes must be > 0, but 'n", group, "' = ",
                            .arrayToString(sampleSizesTemp, vectorLookAndFeelEnabled = TRUE)
                        )
                    }
                    self$sampleSizes <- c(self$sampleSizes, sampleSizesTemp)

                    meansTemp <- self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_MEANS, suffix = group)
                    self$.validateValues(meansTemp, paste0("means", group))
                    self$means <- c(self$means, meansTemp)

                    stDevsTemp <- self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_ST_DEVS, suffix = group)
                    self$.validateValues(stDevsTemp, paste0("stDevs", group))
                    self$stDevs <- c(self$stDevs, stDevsTemp)

                    self$groups <- c(self$groups, rep(as.integer(group), length(sampleSizesTemp)))
                }
            }

            # case: two or more means - overall
            else if (self$.paramExists(dataFrame, paste0(C_KEY_WORDS_OVERALL_SAMPLE_SIZES, 1)) &&
                    self$.paramExists(dataFrame, paste0(C_KEY_WORDS_OVERALL_SAMPLE_SIZES, 2))) {
                self$.inputType <- "overall"
                numberOfTreatmentGroups <- self$.getNumberOfGroups(dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES)
                self$stages <- rep(self$stages, numberOfTreatmentGroups)
                self$groups <- integer(0)
                self$sampleSizes <- numeric(0)
                self$means <- numeric(0)
                self$stDevs <- numeric(0)
                self$overallSampleSizes <- numeric(0)
                self$overallMeans <- numeric(0)
                self$overallStDevs <- numeric(0)
                for (group in 1:numberOfTreatmentGroups) {
                    overallSampleSizesTemp <- self$.getValidatedFloatingPointNumbers(self$.getValuesByParameterName(
                        dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES,
                        suffix = group
                    ), parameterName = "Cumulative sample sizes")
                    self$.validateValues(overallSampleSizesTemp, paste0("overallSampleSizes", group))
                    self$overallSampleSizes <- c(self$overallSampleSizes, overallSampleSizesTemp)

                    overallMeansTemp <- self$.getValuesByParameterName(dataFrame,
                        C_KEY_WORDS_OVERALL_MEANS,
                        suffix = group
                    )
                    self$.validateValues(overallMeansTemp, paste0("overallMeans", group))
                    self$overallMeans <- c(self$overallMeans, overallMeansTemp)

                    overallStDevsTemp <- self$.getValuesByParameterName(dataFrame,
                        C_KEY_WORDS_OVERALL_ST_DEVS,
                        suffix = group
                    )
                    self$.validateValues(overallStDevsTemp, paste0("overallStDevs", group))
                    self$overallStDevs <- c(self$overallStDevs, overallStDevsTemp)

                    self$groups <- c(self$groups, rep(as.integer(group), length(overallSampleSizesTemp)))
                }
            } else {
                stop(
                    C_EXCEPTION_TYPE_MISSING_ARGUMENT,
                    "sample sizes are missing or not correctly specified"
                )
            }

            if (self$.inputType == "stagewise") {
                n <- length(self$sampleSizes)
                self$overallSampleSizes <- rep(NA_real_, n)
                self$overallMeans <- rep(NA_real_, n)
                self$overallStDevs <- rep(NA_real_, n)

                self$.setParameterType("sampleSizes", C_PARAM_USER_DEFINED)
                self$.setParameterType("means", C_PARAM_USER_DEFINED)
                self$.setParameterType("stDevs", C_PARAM_USER_DEFINED)

                self$.setParameterType("overallSampleSizes", C_PARAM_GENERATED)
                self$.setParameterType("overallMeans", C_PARAM_GENERATED)
                self$.setParameterType("overallStDevs", C_PARAM_GENERATED)

                self$.recreateDataFrame()
                self$.createOverallData()
            } else {
                n <- length(self$overallSampleSizes)
                self$sampleSizes <- rep(NA_real_, n)
                self$means <- rep(NA_real_, n)
                self$stDevs <- rep(NA_real_, n)

                self$.setParameterType("sampleSizes", C_PARAM_GENERATED)
                self$.setParameterType("means", C_PARAM_GENERATED)
                self$.setParameterType("stDevs", C_PARAM_GENERATED)

                self$.setParameterType("overallSampleSizes", C_PARAM_USER_DEFINED)
                self$.setParameterType("overallMeans", C_PARAM_USER_DEFINED)
                self$.setParameterType("overallStDevs", C_PARAM_USER_DEFINED)

                self$.recreateDataFrame()
                self$.createStageWiseData()
            }

            if (sum(stats::na.omit(self$sampleSizes) < 0) > 0) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all sample sizes must be >= 0")
            }
            if (sum(stats::na.omit(self$stDevs) < 0) > 0) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all standard deviations must be >= 0")
            }
        },
        .recreateDataFrame = function() {
            super$.recreateDataFrame()
            self$.data <- cbind(self$.data, data.frame(
                sampleSize = self$sampleSizes,
                mean = self$means,
                stDev = self$stDevs,
                overallSampleSize = self$overallSampleSizes,
                overallMean = self$overallMeans,
                overallStDev = self$overallStDevs
            ))
            self$.orderDataByStageAndGroup()
            self$.setDataToVariables()
        },
        .setDataToVariables = function() {
            super$.setDataToVariables()
            self$sampleSizes <- self$.data$sampleSize
            self$means <- self$.data$mean
            self$stDevs <- self$.data$stDev
            self$overallSampleSizes <- self$.data$overallSampleSize
            self$overallMeans <- self$.data$overallMean
            self$overallStDevs <- self$.data$overallStDev
        },
        .fillWithNAs = function(kMax) {
            super$.fillWithNAs(kMax)
            n <- self$.getNumberOfNAsToAdd(kMax)

            naRealsToAdd <- rep(NA_real_, n)

            self$sampleSizes <- c(self$sampleSizes, naRealsToAdd)
            self$means <- c(self$means, naRealsToAdd)
            self$stDevs <- c(self$stDevs, naRealsToAdd)

            self$overallSampleSizes <- c(self$overallSampleSizes, naRealsToAdd)
            self$overallMeans <- c(self$overallMeans, naRealsToAdd)
            self$overallStDevs <- c(self$overallStDevs, naRealsToAdd)

            self$.recreateDataFrame()
        },
        .trim = function(kMax = NA_integer_) {
            indices <- super$.trim(kMax)
            if (length(indices) == 0) {
                return(invisible(FALSE))
            }

            self$sampleSizes <- self$sampleSizes[indices]
            self$means <- self$means[indices]
            self$stDevs <- self$stDevs[indices]

            self$overallSampleSizes <- self$overallSampleSizes[indices]
            self$overallMeans <- self$overallMeans[indices]
            self$overallStDevs <- self$overallStDevs[indices]

            self$.recreateDataFrame()
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
            self$.data$overallSampleSize <- rep(NA_real_, nrow(self$.data))
            self$.data$overallMean <- rep(NA_real_, nrow(self$.data))
            self$.data$overallStDev <- rep(NA_real_, nrow(self$.data))
            subsetLevels <- NA_character_
            if (self$.enrichmentEnabled) {
                subsetLevels <- levels(self$.data$subset)
            }
            for (s in subsetLevels) {
                for (g in levels(self$.data$group)) {
                    if (!is.na(s)) {
                        indices <- which(self$.data$subset == s & self$.data$group == g)
                    } else {
                        indices <- which(self$.data$group == g)
                    }
                    self$.data$overallSampleSize[indices] <- cumsum(self$.data$sampleSize[indices])
                    self$.data$overallMean[indices] <- self$.getOverallMeans(
                        self$.data$sampleSize[indices], self$.data$mean[indices]
                    )
                    self$.data$overallStDev[indices] <- self$.getOverallStDevs(
                        self$.data$sampleSize[indices],
                        self$.data$mean[indices], self$.data$stDev[indices], self$.data$overallMean[indices]
                    )
                }
            }
            self$.setDataToVariables()
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
                result[k] <- self$.getStageWiseStDev(overallStDevs, sampleSizes, overallSampleSizes, means, overallMeans, k)
            }
            return(result)
        },
        .createStageWiseData = function() {
            "Calculates stage-wise means and standard deviation if cunulative data is available"

            self$.data$sampleSize <- rep(NA_real_, nrow(self$.data))
            self$.data$mean <- rep(NA_real_, nrow(self$.data))
            self$.data$stDev <- rep(NA_real_, nrow(self$.data))

            subsetLevels <- NA_character_
            if (self$.enrichmentEnabled) {
                subsetLevels <- levels(self$.data$subset)
            }

            for (s in subsetLevels) {
                for (g in levels(self$.data$group)) {
                    if (!is.na(s)) {
                        indices <- which(self$.data$subset == s & self$.data$group == g)
                    } else {
                        indices <- which(self$.data$group == g)
                    }

                    .assertValuesAreStrictlyIncreasing(self$.data$overallSampleSize[indices],
                        paste0("overallSampleSizes", g),
                        endingNasAllowed = TRUE
                    )

                    self$.data$sampleSize[indices] <- self$.getStageWiseSampleSizes(self$.data$overallSampleSize[indices])
                    self$.data$mean[indices] <- self$.getStageWiseMeans(
                        self$.data$sampleSize[indices],
                        self$.data$overallSampleSize[indices], self$.data$overallMean[indices]
                    )
                    self$.data$stDev[indices] <- self$.getStageWiseStDevs(
                        self$.data$overallStDev[indices], self$.data$sampleSize[indices],
                        self$.data$overallSampleSize[indices], self$.data$mean[indices], self$.data$overallMean[indices]
                    )
                }
            }
            self$.setDataToVariables()
        },
        getRandomData = function() {
            return(.getRandomDataMeans(self))
        }
    )
)

#' @examples
#' \dontrun{
#' datasetExample <- getDataset(
#'     means1 = c(112.3, 105.1, 121.3),
#'     means2 = c(98.1, 99.3, 100.1),
#'     means3 = c(98.1, 99.3, 100.1),
#'     stDevs1 = c(44.4, 42.9, 41.4),
#'     stDevs2 = c(46.7, 41.1, 39.5),
#'     stDevs3 = c(46.7, 41.1, 39.5),
#'     n1 = c(84, 81, 82),
#'     n2 = c(87, 83, 81),
#'     n3 = c(87, 82, 84)
#' )
#' .getRandomDataMeans(datasetExample,
#'     randomDataParamName = "outcome", numberOfVisits = 3,
#'     fixedCovariates = list(gender = c("f", "m"), bmi = c(17, 40))
#' )
#' }
#' 
#' @noRd
#'
.getRandomDataMeans <- function(dataset, ...,
        treatmentName = "Treatment group",
        controlName = "Control group",
        randomDataParamName = "randomData",
        numberOfVisits = 1L,
        fixedCovariates = NULL,
        covariateEffects = NULL,
        seed = NA_real_) {
    if (!is.null(fixedCovariates)) {
        if (!is.list(fixedCovariates)) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote("fixedCovariates"), " must be a named list")
        }
    }
    if (!is.null(covariateEffects)) {
        if (!is.list(covariateEffects)) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote("covariateEffects"), " must be a named list")
        }
    }

    .assertIsSingleCharacter(treatmentName, "treatmentName")
    .assertIsSingleCharacter(controlName, "controlName")
    .assertIsSingleCharacter(randomDataParamName, "randomDataParamName")
    .assertIsSinglePositiveInteger(numberOfVisits, "numberOfVisits", validateType = FALSE)
    .assertIsSingleNumber(seed, "seed", naAllowed = TRUE)

    seed <- .setSeed(seed)

    numberOfGroups <- dataset$getNumberOfGroups()

    sampleSize <- 0
    for (stage in 1:dataset$getNumberOfStages()) {
        for (group in 1:numberOfGroups) {
            if (dataset$.enrichmentEnabled) {
                for (subset in levels(dataset$.data$subset)) {
                    n <- dataset$getSampleSize(stage = stage, group = group, subset = subset)
                    if (n > sampleSize) {
                        sampleSize <- n
                    }
                }
            } else {
                n <- dataset$getSampleSize(stage = stage, group = group)
                n <- round(n / numberOfVisits)
                if (n > sampleSize) {
                    sampleSize <- n
                }
            }
        }
    }

    idFactor <- 10^nchar(as.character(sampleSize))

    data <- NULL
    for (stage in 1:dataset$getNumberOfStages()) {
        for (group in 1:numberOfGroups) {
            for (visit in 1:numberOfVisits) {
                if (dataset$.enrichmentEnabled) {
                    for (subset in levels(dataset$.data$subset)) {
                        n <- dataset$getSampleSize(stage = stage, group = group, subset = subset)
                        randomData <- stats::rnorm(
                            n    = n,
                            mean = dataset$getMean(stage = stage, group = group, subset = subset),
                            sd   = dataset$getStDev(stage = stage, group = group, subset = subset)
                        )
                        row <- data.frame(
                            subject    = idFactor * group + c(1:n),
                            stage      = rep(stage, n),
                            group      = rep(group, n),
                            subset     = rep(subset, n),
                            randomData = randomData
                        )
                        if (is.null(data)) {
                            data <- row
                        } else {
                            data <- rbind(data, row)
                        }
                    }
                } else {
                    n <- dataset$getSampleSize(stage = stage, group = group)
                    n <- floor(n / numberOfVisits)

                    randomData <- stats::rnorm(
                        n    = sampleSize,
                        mean = dataset$getMean(stage = stage, group = group),
                        sd   = dataset$getStDev(stage = stage, group = group)
                    )

                    subjectIds <- (idFactor * 10 * stage) + (idFactor * group) + c(1:sampleSize)
                    indices <- 1:sampleSize
                    randomDataBefore <- NULL
                    numberOfDropOutsBefore <- 0
                    if (visit > 1 && !is.null(data)) {
                        randomDataBefore <- data$randomData[data$stage == visit - 1 & data$subject %in% subjectIds]
                        numberOfDropOutsBefore <- sum(is.na(randomDataBefore))
                        indices <- which(!is.na(randomDataBefore))
                    }
                    sampleSizeBefore <- sampleSize - numberOfDropOutsBefore
                    if (n < sampleSizeBefore) {
                        numberOfDropOuts <- sampleSizeBefore - n
                        numberOfDropOuts <- min(numberOfDropOuts, ceiling(n * 0.2))
                        dropOuts <- sample(c(rep(1, n - numberOfDropOuts), rep(0, numberOfDropOuts)))
                        randomData[indices[dropOuts == 0]] <- NA_real_
                        if (!is.null(randomDataBefore)) {
                            randomData[is.na(randomDataBefore)] <- NA_real_
                        }
                    }

                    row <- data.frame(
                        subject    = subjectIds,
                        stage      = rep(stage, sampleSize),
                        group      = rep(group, sampleSize),
                        visit      = rep(visit - 1, sampleSize),
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
    }
    data$stage <- factor(data$stage)
    groupLevels <- paste(treatmentName, c(1:numberOfGroups))
    if (numberOfGroups > 1) {
        if (numberOfGroups == 2) {
            groupLevels[1] <- treatmentName
        }
        groupLevels[numberOfGroups] <- controlName
    }

    data$group <- factor(data$group, labels = groupLevels)
    if (dataset$.enrichmentEnabled) {
        data$subset <- factor(data$subset)
    }

    if (!is.null(randomDataParamName) && length(randomDataParamName) == 1 && !is.na(randomDataParamName)) {
        colNames <- colnames(data)
        colNames[colNames == "randomData"] <- randomDataParamName
        colnames(data) <- colNames
    }

    if (!is.null(fixedCovariates)) {
        fixedCovariateNames <- names(fixedCovariates)
        if (is.null(fixedCovariateNames) || any(nchar(trimws(fixedCovariateNames)) == 0)) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote("fixedCovariates"), " must be a named list")
        }

        subjects <- sort(unique(data$subject))
        for (fixedCovariateName in fixedCovariateNames) {
            data[[fixedCovariateName]] <- rep(NA, nrow(data))
            values <- fixedCovariates[[fixedCovariateName]]
            if (is.null(values) || length(values) < 2 || any(is.na(values))) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote(paste0("fixedCovariates$", fixedCovariateName)),
                    " (", .arrayToString(values), ") must be a valid numeric or character vector with a minimum of 2 values"
                )
            }

            if (is.character(values)) {
                if (length(unique(values)) < length(values)) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote(paste0("fixedCovariates$", fixedCovariateName)),
                        " (", .arrayToString(values, maxLength = 20), ") must be a unique vector"
                    )
                }

                fixedCovariateSample <- sample(values, length(subjects), replace = TRUE)
                for (i in seq_len(length(subjects))) {
                    data[[fixedCovariateName]][data$subject == subjects[i]] <- fixedCovariateSample[i]
                }
            } else if (is.numeric(values)) {
                if (length(values) == 2) {
                    minValue <- min(values)
                    maxValue <- max(values)
                    covMean <- runif(1, minValue, maxValue)
                    covSD <- covMean * 0.1
                    showMessage <- TRUE
                    for (i in seq_len(length(subjects))) {
                        groupName <- as.character(data$group[data$subject == subjects[i]])[1]
                        covEffect <- 1
                        if (groupName == controlName && !is.null(covariateEffects)) {
                            covEffect <- covariateEffects[[fixedCovariateName]]
                            if (is.null(covEffect)) {
                                covEffect <- 1
                            } else {
                                .assertIsNumericVector(covEffect, paste0("covariateEffects$", fixedCovariateName))
                                if (showMessage) {
                                    message(
                                        "Add effect ", covEffect, " to ",
                                        sQuote(fixedCovariateName), " of ", sQuote(groupName)
                                    )
                                    showMessage <- FALSE
                                }
                            }
                        }
                        continuesExample <- rnorm(sum(data$subject == subjects[i]), covMean * covEffect, covSD)
                        data[[fixedCovariateName]][data$subject == subjects[i]] <- continuesExample
                    }
                }
            }
        }
    }

    data$seed <- rep(seed, nrow(data))

    return(data)
}

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
#' \dontrun{
#' # Plot a dataset of means
#' dataExample <- getDataset(
#'     n1 = c(22, 11, 22, 11),
#'     n2 = c(22, 13, 22, 13),
#'     means1 = c(1, 1.1, 1, 1),
#'     means2 = c(1.4, 1.5, 3, 2.5),
#'     stDevs1 = c(1, 2, 2, 1.3),
#'     stDevs2 = c(1, 2, 2, 1.3)
#' )
#' if (require(ggplot2)) plot(dataExample, main = "Comparison of Means")
#'
#' # Plot a dataset of rates
#' dataExample <- getDataset(
#'     n1 = c(8, 10, 9, 11),
#'     n2 = c(11, 13, 12, 13),
#'     events1 = c(3, 5, 5, 6),
#'     events2 = c(8, 10, 12, 12)
#' )
#' if (require(ggplot2)) plot(dataExample, main = "Comparison of Rates")
#' }
#'
#' @export
#'
plot.Dataset <- function(x, y, ..., 
        main = "Dataset", 
        xlab = "Stage", 
        ylab = NA_character_,
        legendTitle = "Group", 
        palette = "Set1", 
        showSource = FALSE, 
        plotSettings = NULL) {
        
    markdown <- .getOptionalArgument("markdown", ..., optionalArgumentDefaultValue = NA)
    if (is.na(markdown)) {
        markdown <- .isMarkdownEnabled("plot")
    }
    
    args <- list(
        x = x, 
        y = NULL,
        main = main,
        xlab = xlab,
        ylab = ylab,
        legendTitle = legendTitle,
        palette = palette,
        plotSettings = plotSettings, 
        ...)
    
    if (markdown) {
        sep <- .getMarkdownPlotPrintSeparator()
        print(do.call(.plot.Dataset, args))
        return(.knitPrintQueue(x, sep = sep, prefix = sep))
    }
    
    return(do.call(.plot.Dataset, args))
}

.plot.Dataset <- function(x, y, ..., main = "Dataset", xlab = "Stage", ylab = NA_character_,
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
            p <- p + ggplot2::geom_boxplot(
                ggplot2::aes(fill = .data[["stage"]]),
                na.rm = TRUE)
            p <- p + ggplot2::geom_point(
                colour = "#0e414e", shape = 20,
                position = ggplot2::position_jitter(width = .1),
                size = plotSettings$pointSize,
                na.rm = TRUE
            )
            p <- p + ggplot2::stat_summary(
                fun = "mean", geom = "point",
                shape = 21, position = ggplot2::position_dodge(.75), size = 4, fill = "white",
                colour = "black", show.legend = FALSE,
                na.rm = TRUE
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
                position = "dodge", stat = "identity", alpha = 0.4,
                na.rm = TRUE
            )

            # plot events
            p <- p + ggplot2::geom_bar(
                data = data,
                ggplot2::aes(
                    y = .data[["event"]], x = factor(.data[["stage"]]),
                    fill = factor(.data[["stage"]])
                ),
                position = "dodge", stat = "identity",
                na.rm = TRUE
            )
        } else if (x$isDatasetSurvival()) {
            # implement survival plot here
        }
    } else {
        data$stageGroup <- base::interaction(data$stage, data$group)

        if (x$isDatasetMeans()) {
            p <- ggplot2::ggplot(ggplot2::aes(
                y = .data[["randomData"]], x = factor(.data[["stage"]]),
                fill = factor(.data[["group"]])
            ), data = data)
            p <- p + ggplot2::geom_point(
                    ggplot2::aes(colour = .data[["group"]],
                    na.rm = TRUE),
                shape = 20,
                position = ggplot2::position_dodge(.75),
                size = plotSettings$pointSize
            )
            p <- p + ggplot2::geom_boxplot(na.rm = TRUE)
            p <- p + ggplot2::stat_summary(ggplot2::aes(colour = .data[["group"]]),
                fun = "mean", geom = "point",
                shape = 21, position = ggplot2::position_dodge(.75), size = 4, fill = "white",
                show.legend = FALSE,
                na.rm = TRUE
            )
        } else if (x$isDatasetRates()) {
            p <- ggplot2::ggplot(show.legend = FALSE)

            # plot sample size
            p <- p + ggplot2::geom_bar(
                ggplot2::aes(
                    y = .data[["sampleSize"]],
                    x = factor(.data[["stage"]]), fill = factor(.data[["group"]])
                ),
                data = data, position = "dodge", stat = "identity", alpha = 0.4,
                na.rm = TRUE
            )

            # plot events
            p <- p + ggplot2::geom_bar(
                data = data,
                ggplot2::aes(
                    y = .data[["event"]], x = factor(.data[["stage"]]),
                    fill = factor(.data[["group"]])
                ),
                position = "dodge", stat = "identity",
                na.rm = TRUE
            )
        } else if (x$isDatasetSurvival()) {
            # implement survival plot here
        }
    }

    # hide second legend
    if (x$getNumberOfGroups() == 1) {
        p <- p + ggplot2::guides(fill = "none", colour = "none")
    } else {
        p <- p + ggplot2::guides(colour = "none")
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

    return(p)
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
DatasetRates <- R6::R6Class("DatasetRates",
    inherit = Dataset,
    public = list(
        sampleSizes = NULL,
        events = NULL,
        overallSampleSizes = NULL,
        overallEvents = NULL,
        getSampleSize = function(stage, group = 1, subset = NA_character_) {
            return(self$.data$sampleSize[self$.getIndices(stage = stage, group = group, subset = subset)])
        },
        getSampleSizes = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(self$.data$sampleSize[self$.getIndices(stage = self$.getValidatedStage(stage), group = group, subset = subset)])
        },
        getSampleSizesUpTo = function(to, group = 1, subset = NA_character_) {
            return(self$.data$sampleSize[self$.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getEvent = function(stage, group = 1, subset = NA_character_) {
            return(self$.data$event[self$.getIndices(stage = stage, group = group, subset = subset)])
        },
        getEvents = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(self$.data$event[self$.getIndices(stage = self$.getValidatedStage(stage), group = group, subset = subset)])
        },
        getEventsUpTo = function(to, group = 1, subset = NA_character_) {
            return(self$.data$event[self$.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getOverallSampleSize = function(stage, group = 1, subset = NA_character_) {
            return(self$.data$overallSampleSize[self$.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallSampleSizes = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(self$.data$overallSampleSize[self$.getIndices(stage = self$.getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallSampleSizesUpTo = function(to, group = 1, subset = NA_character_) {
            return(self$.data$overallSampleSize[self$.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getOverallEvent = function(stage, group = 1, subset = NA_character_) {
            return(self$.data$overallEvent[self$.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallEvents = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(self$.data$overallEvent[self$.getIndices(stage = self$.getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallEventsUpTo = function(to, group = 1, subset = NA_character_) {
            return(self$.data$overallEvent[self$.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        .initByDataFrame = function(dataFrame) {
            super$.initByDataFrame(dataFrame)

            # case: one rate - stage wise
            if (self$.paramExists(dataFrame, C_KEY_WORDS_SAMPLE_SIZES)) {
                self$.inputType <- "stagewise"

                self$sampleSizes <- self$.getValidatedFloatingPointNumbers(
                    self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_SAMPLE_SIZES),
                    parameterName = "Sample sizes"
                )
                self$.validateValues(self$sampleSizes, "n")
                if (any(stats::na.omit(self$sampleSizes) <= 0)) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "all sample sizes must be > 0, but 'n' = ",
                        self$.arrayToString(self$sampleSizes, vectorLookAndFeelEnabled = TRUE)
                    )
                }

                self$events <- self$.getValidatedFloatingPointNumbers(
                    self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_EVENTS),
                    parameterName = "Events"
                )
                self$.validateValues(self$events, "events")
                if (any(stats::na.omit(self$events) < 0)) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all events must be >= 0, but 'events' = ",
                        self$.arrayToString(self$events, vectorLookAndFeelEnabled = TRUE)
                    )
                }

                kMax <- length(self$sampleSizes)
                stageNumber <- length(stats::na.omit(self$sampleSizes))
                dataInput <- data.frame(
                    sampleSizes = self$sampleSizes,
                    events = self$events
                )
                dataInput <- self$.getOverallData(dataInput, kMax, stage = stageNumber)
                self$overallSampleSizes <- dataInput$overallSampleSizes
                self$overallEvents <- dataInput$overallEvents

                self$.setParameterType("sampleSizes", C_PARAM_USER_DEFINED)
                self$.setParameterType("events", C_PARAM_USER_DEFINED)

                self$.setParameterType("overallSampleSizes", C_PARAM_GENERATED)
                self$.setParameterType("overallEvents", C_PARAM_GENERATED)
            }

            # case: one rate - overall
            else if (self$.paramExists(dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES)) {
                self$.inputType <- "overall"
                self$overallSampleSizes <- self$.getValidatedFloatingPointNumbers(
                    self$.getValuesByParameterName(
                        dataFrame,
                        C_KEY_WORDS_OVERALL_SAMPLE_SIZES
                    ),
                    parameterName = "Cumulative sample sizes"
                )
                self$.validateValues(self$overallSampleSizes, "overallSampleSizes")
                .assertValuesAreStrictlyIncreasing(self$overallSampleSizes, "overallSampleSizes", endingNasAllowed = TRUE)

                self$overallEvents <- self$.getValidatedFloatingPointNumbers(
                    self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_EVENTS),
                    parameterName = "Cumulative events"
                )
                self$.validateValues(self$overallEvents, "overallEvents")
                .assertValuesAreMonotoneIncreasing(self$overallEvents, "overallEvents", endingNasAllowed = TRUE)

                kMax <- length(self$overallSampleSizes)
                stageNumber <- length(stats::na.omit(self$overallSampleSizes))
                stageWiseData <- self$.getStageWiseData(data.frame(
                    overallSampleSizes = self$overallSampleSizes,
                    overallEvents = self$overallEvents
                ), kMax, stage = stageNumber)
                self$sampleSizes <- stageWiseData$sampleSizes
                self$events <- stageWiseData$events

                self$.setParameterType("sampleSizes", C_PARAM_GENERATED)
                self$.setParameterType("events", C_PARAM_GENERATED)

                self$.setParameterType("overallSampleSizes", C_PARAM_USER_DEFINED)
                self$.setParameterType("overallEvents", C_PARAM_USER_DEFINED)
            }

            # case: two or more rates - stage wise
            else if (self$.paramExists(dataFrame, paste0(C_KEY_WORDS_SAMPLE_SIZES, 1)) &&
                    self$.paramExists(dataFrame, paste0(C_KEY_WORDS_SAMPLE_SIZES, 2))) {
                self$.inputType <- "stagewise"

                numberOfTreatmentGroups <- self$.getNumberOfGroups(dataFrame, C_KEY_WORDS_SAMPLE_SIZES)

                self$stages <- rep(self$stages, numberOfTreatmentGroups)

                self$groups <- integer(0)
                self$sampleSizes <- numeric(0)
                self$events <- numeric(0)
                self$overallSampleSizes <- numeric(0)
                self$overallEvents <- numeric(0)
                for (group in 1:numberOfTreatmentGroups) {
                    sampleSizesTemp <- self$.getValidatedFloatingPointNumbers(
                        self$.getValuesByParameterName(
                            dataFrame, C_KEY_WORDS_SAMPLE_SIZES,
                            suffix = group
                        ),
                        parameterName = "Sample sizes"
                    )
                    self$.validateValues(sampleSizesTemp, paste0("n", group))
                    if (any(stats::na.omit(sampleSizesTemp) <= 0)) {
                        stop(
                            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                            "all sample sizes must be > 0, but 'n", group, "' = ",
                            self$.arrayToString(sampleSizesTemp, vectorLookAndFeelEnabled = TRUE)
                        )
                    }
                    self$sampleSizes <- c(self$sampleSizes, sampleSizesTemp)

                    eventsTemp <- self$.getValidatedFloatingPointNumbers(
                        self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_EVENTS, suffix = group),
                        parameterName = "Events"
                    )
                    self$.validateValues(eventsTemp, paste0("events", group))
                    if (any(stats::na.omit(eventsTemp) < 0)) {
                        stop(
                            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all events must be >= 0, but 'events", group, "' = ",
                            self$.arrayToString(eventsTemp, vectorLookAndFeelEnabled = TRUE)
                        )
                    }
                    self$events <- c(self$events, eventsTemp)

                    self$groups <- c(self$groups, rep(as.integer(group), length(sampleSizesTemp)))

                    kMax <- length(sampleSizesTemp)
                    numberOfValidStages <- length(stats::na.omit(sampleSizesTemp))
                    overallData <- self$.getOverallData(data.frame(
                        sampleSizes = sampleSizesTemp,
                        events = eventsTemp
                    ), kMax, stage = numberOfValidStages)

                    self$overallSampleSizes <- c(self$overallSampleSizes, overallData$overallSampleSizes)
                    self$overallEvents <- c(self$overallEvents, overallData$overallEvents)
                }
                if (sum(stats::na.omit(self$sampleSizes) < 0) > 0) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all sample sizes must be >= 0")
                }

                self$.setParameterType("sampleSizes", C_PARAM_USER_DEFINED)
                self$.setParameterType("events", C_PARAM_USER_DEFINED)

                self$.setParameterType("overallSampleSizes", C_PARAM_GENERATED)
                self$.setParameterType("overallEvents", C_PARAM_GENERATED)
            }

            # case: two or more rates - overall
            else if (self$.paramExists(dataFrame, paste0(C_KEY_WORDS_OVERALL_SAMPLE_SIZES, 1)) &&
                    self$.paramExists(dataFrame, paste0(C_KEY_WORDS_OVERALL_SAMPLE_SIZES, 2))) {
                self$.inputType <- "overall"

                numberOfTreatmentGroups <- self$.getNumberOfGroups(dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES)

                self$stages <- rep(self$stages, numberOfTreatmentGroups)

                self$groups <- integer(0)
                self$sampleSizes <- numeric(0)
                self$events <- numeric(0)
                self$overallSampleSizes <- numeric(0)
                self$overallEvents <- numeric(0)
                for (group in 1:numberOfTreatmentGroups) {
                    overallSampleSizesTemp <- self$.getValidatedFloatingPointNumbers(
                        self$.getValuesByParameterName(
                            dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES,
                            suffix = group
                        ),
                        parameterName = "Cumulative sample sizes"
                    )
                    self$.validateValues(overallSampleSizesTemp, paste0("overallSampleSizes", group))
                    .assertValuesAreStrictlyIncreasing(overallSampleSizesTemp,
                        paste0("overallSampleSizes", group),
                        endingNasAllowed = TRUE
                    )
                    self$overallSampleSizes <- c(self$overallSampleSizes, overallSampleSizesTemp)

                    overallEventsTemp <- self$.getValidatedFloatingPointNumbers(
                        self$.getValuesByParameterName(dataFrame,
                            C_KEY_WORDS_OVERALL_EVENTS,
                            suffix = group
                        ),
                        parameterName = "Cumulative events"
                    )
                    self$.validateValues(overallEventsTemp, paste0("overallEvents", group))
                    .assertValuesAreMonotoneIncreasing(overallEventsTemp,
                        paste0("overallEvents", group),
                        endingNasAllowed = TRUE
                    )
                    self$overallEvents <- c(self$overallEvents, overallEventsTemp)

                    self$groups <- c(self$groups, rep(as.integer(group), length(overallSampleSizesTemp)))

                    kMax <- length(overallSampleSizesTemp)
                    numberOfValidStages <- length(stats::na.omit(overallSampleSizesTemp))
                    stageWiseData <- self$.getStageWiseData(data.frame(
                        overallSampleSizes = overallSampleSizesTemp,
                        overallEvents = overallEventsTemp
                    ), kMax, stage = numberOfValidStages)

                    validatedSampleSizes <- stageWiseData$sampleSizes
                    self$.validateValues(validatedSampleSizes, paste0("n", group))
                    self$sampleSizes <- c(self$sampleSizes, validatedSampleSizes)
                    self$events <- c(self$events, stageWiseData$events)

                    if (sum(stats::na.omit(self$sampleSizes) < 0) > 0) {
                        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all sample sizes must be >= 0")
                    }
                }

                self$.setParameterType("sampleSizes", C_PARAM_GENERATED)
                self$.setParameterType("events", C_PARAM_GENERATED)

                self$.setParameterType("overallSampleSizes", C_PARAM_USER_DEFINED)
                self$.setParameterType("overallEvents", C_PARAM_USER_DEFINED)
            } else {
                stop(
                    C_EXCEPTION_TYPE_MISSING_ARGUMENT,
                    "sample sizes are missing or not correctly specified"
                )
            }

            if (sum(stats::na.omit(self$events) < 0) > 0) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all events must be >= 0")
            }

            self$.recreateDataFrame()
            if (self$.enrichmentEnabled) {
                self$.createOverallDataEnrichment()
            }
        },
        .recreateDataFrame = function() {
            super$.recreateDataFrame()
            self$.data <- cbind(self$.data, data.frame(
                sampleSize = self$sampleSizes,
                event = self$events,
                overallSampleSize = self$overallSampleSizes,
                overallEvent = self$overallEvents
            ))
            self$.orderDataByStageAndGroup()
            self$.setDataToVariables()
        },
        .setDataToVariables = function() {
            super$.setDataToVariables()
            self$sampleSizes <- self$.data$sampleSize
            self$events <- self$.data$event
            self$overallSampleSizes <- self$.data$overallSampleSize
            self$overallEvents <- self$.data$overallEvent
        },
        .fillWithNAs = function(kMax) {
            super$.fillWithNAs(kMax)
            n <- self$.getNumberOfNAsToAdd(kMax)

            self$sampleSizes <- c(self$sampleSizes, rep(NA_real_, n))
            self$events <- c(self$events, rep(NA_real_, n))

            self$overallSampleSizes <- c(self$overallSampleSizes, rep(NA_real_, n))
            self$overallEvents <- c(self$overallEvents, rep(NA_real_, n))

            self$.recreateDataFrame()
        },
        .trim = function(kMax = NA_integer_) {
            indices <- super$.trim(kMax)
            if (length(indices) == 0) {
                return(invisible(FALSE))
            }

            self$sampleSizes <- self$sampleSizes[indices]
            self$events <- self$events[indices]

            self$overallSampleSizes <- self$overallSampleSizes[indices]
            self$overallEvents <- self$overallEvents[indices]

            self$.recreateDataFrame()

            return(invisible(TRUE))
        },
        getRandomData = function() {
            data <- NULL
            for (stage in 1:self$getNumberOfStages()) {
                for (group in 1:self$getNumberOfGroups()) {
                    if (self$.enrichmentEnabled) {
                        for (subset in levels(self$.data$subset)) {
                            n <- self$getSampleSize(stage = stage, group = group, subset = subset)
                            numberOfEvents <- self$getEvent(stage = stage, group = group, subset = subset)
                            randomIndices <- sample(x = c(1:n), size = numberOfEvents, replace = FALSE)
                            randomData <- rep(0, n)
                            randomData[randomIndices] <- 1

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
                        n <- self$getSampleSize(stage = stage, group = group)
                        numberOfEvents <- self$getEvent(stage = stage, group = group)
                        randomIndices <- sample(x = c(1:n), size = numberOfEvents, replace = FALSE)
                        randomData <- rep(0, n)
                        randomData[randomIndices] <- 1

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
            data$group <- factor(data$group, label = paste("Group", c(1:self$getNumberOfGroups())))
            return(data)
        },
        .createOverallDataEnrichment = function() {
            if (!self$.enrichmentEnabled) {
                return(invisible())
            }

            self$.data$overallSampleSize <- rep(NA_real_, nrow(self$.data))
            self$.data$overallEvent <- rep(NA_real_, nrow(self$.data))
            for (s in levels(self$.data$subset)) {
                for (g in levels(self$.data$group)) {
                    indices <- which(self$.data$subset == s & self$.data$group == g)
                    self$.data$overallSampleSize[indices] <- cumsum(self$.data$sampleSize[indices])
                    self$.data$overallEvent[indices] <- cumsum(self$.data$event[indices])
                }
            }

            self$.setDataToVariables()
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
DatasetSurvival <- R6::R6Class("DatasetSurvival",
    inherit = Dataset,
    public = list(
        overallEvents = NULL,
        overallAllocationRatios = NULL,
        overallLogRanks = NULL,
        events = NULL,
        allocationRatios = NULL,
        logRanks = numeric(),
        getEvent = function(stage, group = 1, subset = NA_character_) {
            return(self$.data$event[self$.getIndices(stage = stage, group = group, subset = subset)])
        },
        getEvents = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(self$.data$event[self$.getIndices(stage = self$.getValidatedStage(stage), group = group, subset = subset)])
        },
        getEventsUpTo = function(to, group = 1, subset = NA_character_) {
            return(self$.data$event[self$.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getAllocationRatio = function(stage, group = 1, subset = NA_character_) {
            return(self$.data$allocationRatio[self$.getIndices(stage = stage, group = group, subset = subset)])
        },
        getAllocationRatios = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(self$.data$allocationRatio[self$.getIndices(stage = self$.getValidatedStage(stage), group = group, subset = subset)])
        },
        getAllocationRatiosUpTo = function(to, group = 1, subset = NA_character_) {
            return(self$.data$allocationRatio[self$.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getLogRank = function(stage, group = 1, subset = NA_character_) {
            return(self$.data$logRank[self$.getIndices(stage = stage, group = group, subset = subset)])
        },
        getLogRanks = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(self$.data$logRank[self$.getIndices(stage = self$.getValidatedStage(stage), group = group, subset = subset)])
        },
        getLogRanksUpTo = function(to, group = 1, subset = NA_character_) {
            return(self$.data$logRank[self$.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getOverallEvent = function(stage, group = 1, subset = NA_character_) {
            return(self$.data$overallEvent[self$.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallEvents = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(self$.data$overallEvent[self$.getIndices(stage = self$.getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallEventsUpTo = function(to, group = 1, subset = NA_character_) {
            return(self$.data$overallEvent[self$.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getOverallAllocationRatio = function(stage, group = 1, subset = NA_character_) {
            return(self$.data$overallAllocationRatio[self$.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallAllocationRatios = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(self$.data$overallAllocationRatio[self$.getIndices(stage = self$.getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallAllocationRatiosUpTo = function(to, group = 1, subset = NA_character_) {
            return(self$.data$overallAllocationRatio[self$.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getOverallLogRank = function(stage, group = 1, subset = NA_character_) {
            return(self$.data$overallLogRank[self$.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallLogRanks = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(self$.data$overallLogRank[self$.getIndices(stage = self$.getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallLogRanksUpTo = function(to, group = 1, subset = NA_character_) {
            return(self$.data$overallLogRank[self$.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        .getAllocationRatioDefaultValues = function(stages, events, logRanks) {
            allocationRatioDefaultValues <- rep(C_ALLOCATION_RATIO_DEFAULT, length(stages))
            indices <- which(is.na(events) | is.na(logRanks))
            allocationRatioDefaultValues[indices] <- NA_real_
            return(allocationRatioDefaultValues)
        },
        .initByDataFrame = function(dataFrame) {
            super$.initByDataFrame(dataFrame)

            if (inherits(self, "DatasetEnrichmentSurvival")) {
                if (self$.paramExists(dataFrame, C_KEY_WORDS_EXPECTED_EVENTS) ||
                        self$.paramExists(dataFrame, C_KEY_WORDS_VARIANCE_EVENTS)) {
                    self$.inputType <- "stagewise"

                    self$events <- self$.getValidatedFloatingPointNumbers(
                        self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_EVENTS),
                        parameterName = "Events"
                    )
                    self$.validateValues(self$events, "events")

                    self$allocationRatios <- self$.getValuesByParameterName(
                        dataFrame, C_KEY_WORDS_ALLOCATION_RATIOS,
                        defaultValues = .getAllocationRatioDefaultValues(self$stages, self$events, self$expectedEvents)
                    )
                    self$.validateValues(self$allocationRatios, "allocationRatios")
                } else if (self$.paramExists(dataFrame, C_KEY_WORDS_OVERALL_EXPECTED_EVENTS) ||
                        self$.paramExists(dataFrame, C_KEY_WORDS_OVERALL_VARIANCE_EVENTS)) {
                    self$.inputType <- "overall"

                    self$overallEvents <- self$.getValidatedFloatingPointNumbers(
                        self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_EVENTS),
                        parameterName = "Cumulative events"
                    )
                    self$.validateValues(self$overallEvents, "overallEvents")

                    self$overallAllocationRatios <- self$.getValuesByParameterName(
                        dataFrame,
                        parameterNameVariants = C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS,
                        defaultValues = self$.getAllocationRatioDefaultValues(self$stages, self$overallEvents, self$overallExpectedEvents)
                    )
                    self$.validateValues(self$overallAllocationRatios, "overallAllocationRatios")
                }

                # stratified enrichment: do nothing more here
            }

            # case: survival, two groups - overall
            else if (self$.paramExists(dataFrame, C_KEY_WORDS_OVERALL_LOG_RANKS)) {
                self$.inputType <- "overall"
                self$overallEvents <- self$.getValidatedFloatingPointNumbers(
                    self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_EVENTS),
                    parameterName = "Cumulative events"
                )
                self$.validateValues(self$overallEvents, "overallEvents")
                if (!self$.enrichmentEnabled) {
                    .assertValuesAreStrictlyIncreasing(self$overallEvents, "overallEvents", endingNasAllowed = TRUE)
                }

                self$overallLogRanks <- self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_LOG_RANKS)
                self$.validateValues(self$overallLogRanks, "overallLogRanks")

                self$overallAllocationRatios <- self$.getValuesByParameterName(
                    dataFrame,
                    parameterNameVariants = C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS,
                    defaultValues = self$.getAllocationRatioDefaultValues(self$stages, self$overallEvents, self$overallLogRanks)
                )
                self$.validateValues(self$overallAllocationRatios, "overallAllocationRatios")

                self$.setParameterType("groups", C_PARAM_NOT_APPLICABLE)
            }

            # case: survival, two groups - stage wise
            else if (self$.paramExists(dataFrame, C_KEY_WORDS_LOG_RANKS)) {
                self$.inputType <- "stagewise"
                self$events <- self$.getValidatedFloatingPointNumbers(self$.getValuesByParameterName(
                    dataFrame, C_KEY_WORDS_EVENTS
                ), parameterName = "Events")
                self$.validateValues(self$events, "events")
                if (any(stats::na.omit(self$events) < 0)) {
                    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all events must be >= 0")
                }

                self$logRanks <- self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_LOG_RANKS)
                self$.validateValues(self$logRanks, "logRanks")

                self$allocationRatios <- self$.getValuesByParameterName(
                    dataFrame, C_KEY_WORDS_ALLOCATION_RATIOS,
                    defaultValues = self$.getAllocationRatioDefaultValues(self$stages, self$events, self$logRanks)
                )
                self$.validateValues(self$allocationRatios, "allocationRatios")

                self$.setParameterType("groups", C_PARAM_NOT_APPLICABLE)
            }

            # case: survival, three ore more groups - overall
            else if (self$.paramExists(dataFrame, paste0(C_KEY_WORDS_OVERALL_LOG_RANKS, 1)) &&
                    self$.paramExists(dataFrame, paste0(C_KEY_WORDS_OVERALL_LOG_RANKS, 2))) {
                self$.inputType <- "overall"

                numberOfTreatmentGroups <- self$.getNumberOfGroups(dataFrame, C_KEY_WORDS_OVERALL_LOG_RANKS)

                self$stages <- rep(self$stages, numberOfTreatmentGroups)

                self$groups <- integer(0)
                self$overallEvents <- numeric(0)
                self$overallAllocationRatios <- numeric(0)
                self$overallLogRanks <- numeric(0)
                for (group in 1:numberOfTreatmentGroups) {
                    overallEventsTemp <- self$.getValuesByParameterName(dataFrame,
                        C_KEY_WORDS_OVERALL_EVENTS,
                        suffix = group
                    )
                    self$.validateValues(overallEventsTemp, paste0("overallEvents", group))
                    if (is.null(dataFrame[["subset"]]) || length(unique(dataFrame[["subset"]])) <= 1) {
                        .assertValuesAreStrictlyIncreasing(overallEventsTemp,
                            paste0("overallEvents", group),
                            endingNasAllowed = TRUE
                        )
                    }
                    self$overallEvents <- c(self$overallEvents, overallEventsTemp)

                    overallLogRanksTemp <- self$.getValuesByParameterName(
                        dataFrame, C_KEY_WORDS_OVERALL_LOG_RANKS,
                        suffix = group
                    )
                    self$.validateValues(overallLogRanksTemp, paste0("overallLogRanks", group))
                    self$overallLogRanks <- c(self$overallLogRanks, overallLogRanksTemp)

                    overallAllocationRatiosTemp <- self$.getValuesByParameterName(
                        dataFrame, C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS,
                        suffix = group,
                        defaultValues = self$.getAllocationRatioDefaultValues(
                            overallEventsTemp,
                            overallEventsTemp, overallLogRanksTemp
                        )
                    )
                    self$.validateValues(overallAllocationRatiosTemp, paste0("overallAllocationRatios", group))
                    self$overallAllocationRatios <- c(self$overallAllocationRatios, overallAllocationRatiosTemp)

                    self$groups <- c(self$groups, rep(as.integer(group), length(overallLogRanksTemp)))
                }
            }

            # case: survival, three ore more groups - stage wise
            else if (self$.paramExists(dataFrame, paste0(C_KEY_WORDS_LOG_RANKS, 1)) &&
                    self$.paramExists(dataFrame, paste0(C_KEY_WORDS_LOG_RANKS, 2))) {
                self$.inputType <- "stagewise"
                numberOfTreatmentGroups <- self$.getNumberOfGroups(dataFrame, C_KEY_WORDS_LOG_RANKS)

                self$stages <- rep(self$stages, numberOfTreatmentGroups)

                self$groups <- integer(0)
                self$events <- numeric(0)
                self$allocationRatios <- numeric(0)
                self$logRanks <- numeric(0)
                for (group in 1:numberOfTreatmentGroups) {
                    eventsTemp <- self$.getValidatedFloatingPointNumbers(self$.getValuesByParameterName(
                        dataFrame, C_KEY_WORDS_EVENTS,
                        suffix = group
                    ), parameterName = "Events")
                    if (any(stats::na.omit(eventsTemp) < 0)) {
                        stop(
                            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all events must be >= 0, but 'events", group, "' = ",
                            self$.arrayToString(eventsTemp, vectorLookAndFeelEnabled = TRUE)
                        )
                    }
                    self$events <- c(self$events, eventsTemp)

                    logRanksTemp <- self$.getValuesByParameterName(
                        dataFrame, C_KEY_WORDS_LOG_RANKS,
                        suffix = group
                    )
                    self$.validateValues(logRanksTemp, paste0("n", group))
                    self$logRanks <- c(self$logRanks, logRanksTemp)

                    allocationRatiosTemp <- self$.getValuesByParameterName(
                        dataFrame, C_KEY_WORDS_ALLOCATION_RATIOS,
                        suffix = group,
                        defaultValues = self$.getAllocationRatioDefaultValues(
                            eventsTemp,
                            eventsTemp, logRanksTemp
                        )
                    )
                    self$.validateValues(allocationRatiosTemp, paste0("allocationRatios", group))
                    self$allocationRatios <- c(self$allocationRatios, allocationRatiosTemp)

                    self$groups <- c(self$groups, rep(as.integer(group), length(eventsTemp)))
                }
            } else {
                stop(
                    C_EXCEPTION_TYPE_RUNTIME_ISSUE, "unable to identify case for ", .getClassName(self), " and columns ",
                    self$.arrayToString(colnames(dataFrame))
                )
            }

            if (self$.inputType == "stagewise") {
                n <- length(self$events)
                self$overallEvents <- rep(NA_real_, n)
                self$overallAllocationRatios <- rep(NA_real_, n)
                self$overallLogRanks <- rep(NA_real_, n)

                self$.setParameterType("events", C_PARAM_USER_DEFINED)
                self$.setParameterType("allocationRatios", C_PARAM_USER_DEFINED)
                if (!inherits(self, "DatasetEnrichmentSurvival")) {
                    self$.setParameterType("logRanks", C_PARAM_USER_DEFINED)
                }

                self$.setParameterType("overallEvents", C_PARAM_GENERATED)
                self$.setParameterType("overallAllocationRatios", C_PARAM_GENERATED)
                if (!inherits(self, "DatasetEnrichmentSurvival")) {
                    self$.setParameterType("overallLogRanks", C_PARAM_GENERATED)
                }

                if (!inherits(self, "DatasetEnrichmentSurvival")) {
                    self$.recreateDataFrame()
                    self$.createOverallData()
                }
            } else {
                n <- length(self$overallEvents)
                self$events <- rep(NA_real_, n)
                self$allocationRatios <- rep(NA_real_, n)
                self$logRanks <- rep(NA_real_, n)

                self$.setParameterType("events", C_PARAM_GENERATED)
                self$.setParameterType("allocationRatios", C_PARAM_GENERATED)
                if (!inherits(self, "DatasetEnrichmentSurvival")) {
                    self$.setParameterType("logRanks", C_PARAM_GENERATED)
                }

                self$.setParameterType("overallEvents", C_PARAM_USER_DEFINED)
                self$.setParameterType("overallAllocationRatios", C_PARAM_USER_DEFINED)
                if (!inherits(self, "DatasetEnrichmentSurvival")) {
                    self$.setParameterType("overallLogRanks", C_PARAM_USER_DEFINED)
                }

                if (!inherits(self, "DatasetEnrichmentSurvival")) {
                    self$.recreateDataFrame()
                    self$.createStageWiseData()
                }
            }
        },
        .recreateDataFrame = function() {
            super$.recreateDataFrame()

            if (inherits(self, "DatasetEnrichmentSurvival")) {
                self$.data <- cbind(self$.data, data.frame(
                    overallEvent = self$overallEvents,
                    overallExpectedEvent = self$overallExpectedEvents,
                    overallVarianceEvent = self$overallVarianceEvents,
                    overallAllocationRatio = self$overallAllocationRatios,
                    event = self$events,
                    expectedEvent = self$expectedEvents,
                    # varianceEvent = varianceEvents, # maybe implemented later
                    allocationRatio = self$allocationRatios
                ))
            } else {
                self$.data <- cbind(self$.data, data.frame(
                    overallEvent = self$overallEvents,
                    overallAllocationRatio = self$overallAllocationRatios,
                    overallLogRank = self$overallLogRanks,
                    event = self$events,
                    allocationRatio = self$allocationRatios,
                    logRank = self$logRanks
                ))
            }
            self$.orderDataByStageAndGroup()
            self$.setDataToVariables()
        },
        .setDataToVariables = function() {
            super$.setDataToVariables()
            self$overallEvents <- self$.data$overallEvent
            self$overallAllocationRatios <- self$.data$overallAllocationRatio
            self$events <- self$.data$event
            self$allocationRatios <- self$.data$allocationRatio
            if (!inherits(self, "DatasetEnrichmentSurvival")) {
                self$overallLogRanks <- self$.data$overallLogRank
                self$logRanks <- self$.data$logRank
            }
        },
        .fillWithNAs = function(kMax) {
            super$.fillWithNAs(kMax)
            n <- self$.getNumberOfNAsToAdd(kMax)

            self$overallEvents <- c(self$overallEvents, rep(NA_real_, n))
            self$overallAllocationRatios <- c(self$overallAllocationRatios, rep(NA_real_, n))
            self$overallLogRanks <- c(self$overallLogRanks, rep(NA_real_, n))

            self$events <- c(self$events, rep(NA_real_, n))
            self$allocationRatios <- c(self$allocationRatios, rep(NA_real_, n))
            self$logRanks <- c(self$logRanks, rep(NA_real_, n))

            self$.recreateDataFrame()
        },
        .trim = function(kMax = NA_integer_) {
            indices <- super$.trim(kMax)
            if (length(indices) == 0) {
                return(invisible(FALSE))
            }

            self$events <- self$events[indices]
            self$allocationRatios <- self$allocationRatios[indices]
            self$logRanks <- self$logRanks[indices]

            self$overallEvents <- self$overallEvents[indices]
            self$overallAllocationRatios <- self$overallAllocationRatios[indices]
            self$overallLogRanks <- self$overallLogRanks[indices]

            self$.recreateDataFrame()

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
            self$.data$overallEvent <- rep(NA_real_, nrow(self$.data))
            if (inherits(self, "DatasetEnrichmentSurvival")) {
                self$.data$overallExpectedEvent <- rep(NA_real_, nrow(self$.data))
                self$.data$overallVarianceEvent <- rep(NA_real_, nrow(self$.data))
            } else {
                self$.data$overallLogRank <- rep(NA_real_, nrow(self$.data))
            }
            self$.data$overallAllocationRatio <- rep(NA_real_, nrow(self$.data))
            subsetLevels <- NA_character_
            if (self$.enrichmentEnabled) {
                subsetLevels <- levels(self$.data$subset)
            }
            for (s in subsetLevels) {
                for (g in levels(self$.data$group)) {
                    if (!is.na(s)) {
                        indices <- which(self$.data$subset == s & self$.data$group == g)
                    } else {
                        indices <- which(self$.data$group == g)
                    }
                    self$.data$overallEvent[indices] <- cumsum(self$.data$event[indices])
                    self$.data$overallExpectedEvent[indices] <- cumsum(self$.data$expectedEvent[indices])
                    # .data$overallVarianceEvent[indices] <<- # maybe implemented later
                    self$.data$overallLogRank[indices] <- self$.getOverallLogRanks(
                        self$.data$logRank[indices], self$.data$event[indices], self$.data$overallEvent[indices]
                    )
                    self$.data$overallAllocationRatio[indices] <- self$.getOverallAllocationRatios(
                        self$.data$allocationRatio[indices], self$.data$event[indices], self$.data$overallEvent[indices]
                    )
                }
            }
            self$.setDataToVariables()
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

            self$.data$event <- rep(NA_real_, nrow(self$.data))
            if (inherits(self, "DatasetEnrichmentSurvival")) {
                self$.data$expectedEvent <- rep(NA_real_, nrow(self$.data))
                self$.data$varianceEvent <- rep(NA_real_, nrow(self$.data))
            } else {
                self$.data$logRank <- rep(NA_real_, nrow(self$.data))
            }
            self$.data$allocationRatio <- rep(NA_real_, nrow(self$.data))

            subsetLevels <- NA_character_
            if (self$.enrichmentEnabled) {
                subsetLevels <- levels(self$.data$subset)
            }

            for (s in subsetLevels) {
                for (g in levels(self$.data$group)) {
                    if (!is.na(s)) {
                        indices <- which(self$.data$subset == s & self$.data$group == g)
                    } else {
                        indices <- which(self$.data$group == g)
                    }

                    groupNumber <- ifelse(levels(self$.data$group) > 1, g, "")
                    if (self$.enrichmentEnabled) {
                        .assertValuesAreStrictlyIncreasing(self$.data$overallEvent[indices],
                            paste0("overallEvents", groupNumber, "[subset == \"", s, "\"]"),
                            endingNasAllowed = TRUE
                        )
                    } else {
                        .assertValuesAreStrictlyIncreasing(self$.data$overallEvent[indices],
                            paste0("overallEvents", groupNumber),
                            endingNasAllowed = TRUE
                        )
                    }

                    self$.data$event[indices] <- self$.getStageWiseEvents(self$.data$overallEvent[indices])
                    if (inherits(self, "DatasetEnrichmentSurvival")) {
                        self$.data$expectedEvent[indices] <- self$.getStageWiseEvents(self$.data$overallExpectedEvent[indices])
                        # .data$varianceEvent[indices] <<- # maybe implemented later
                    } else {
                        self$.data$logRank[indices] <- self$.getStageWiseLogRanks(
                            self$.data$overallLogRank[indices], self$.data$overallEvent[indices]
                        )
                    }
                    self$.data$allocationRatio[indices] <- self$.getStageWiseAllocationRatios(
                        self$.data$overallAllocationRatio[indices],
                        self$.data$event[indices], self$.data$overallEvent[indices]
                    )
                }
            }
            self$.setDataToVariables()
        }
    )
)

#'
#' @rdname DatasetSurvival
#'
#' @keywords internal
#'
DatasetEnrichmentSurvival <- R6::R6Class("DatasetEnrichmentSurvival",
    inherit = DatasetSurvival,
    public = list(
        expectedEvents = NULL,
        varianceEvents = NULL,
        overallExpectedEvents = NULL,
        overallVarianceEvents = NULL,
        .initByDataFrame = function(dataFrame) {
            super$.initByDataFrame(dataFrame)

            if (self$.paramExists(dataFrame, C_KEY_WORDS_OVERALL_EXPECTED_EVENTS) ||
                    self$.paramExists(dataFrame, C_KEY_WORDS_OVERALL_VARIANCE_EVENTS)) {
                if (!self$.paramExists(dataFrame, C_KEY_WORDS_OVERALL_EXPECTED_EVENTS)) {
                    stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'overallExpectedEvents' or 'cumExpectedEvents' is missing")
                }
                if (!self$.paramExists(dataFrame, C_KEY_WORDS_OVERALL_VARIANCE_EVENTS)) {
                    stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'overallVarianceEvents' or 'cumVarianceEvents' is missing")
                }

                self$.inputType <- "overall"

                self$overallEvents <- self$.getValidatedFloatingPointNumbers(
                    self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_EVENTS),
                    parameterName = "Cumulative events"
                )
                self$.validateValues(self$overallEvents, "overallEvents")

                self$overallExpectedEvents <- self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_EXPECTED_EVENTS)
                self$.validateValues(self$overallExpectedEvents, "overallExpectedEvents")

                self$overallVarianceEvents <- self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_VARIANCE_EVENTS)
                self$.validateValues(self$overallVarianceEvents, "overallVarianceEvents")

                self$overallAllocationRatios <- self$.getValuesByParameterName(
                    dataFrame,
                    parameterNameVariants = C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS,
                    defaultValues = self$.getAllocationRatioDefaultValues(self$stages, self$overallEvents, self$overallExpectedEvents)
                )
                self$.validateValues(self$overallAllocationRatios, "overallAllocationRatios")
            } else if (self$.paramExists(dataFrame, C_KEY_WORDS_EXPECTED_EVENTS) ||
                    self$.paramExists(dataFrame, C_KEY_WORDS_VARIANCE_EVENTS)) {
                if (!self$.paramExists(dataFrame, C_KEY_WORDS_EXPECTED_EVENTS)) {
                    stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'expectedEvents' is missing")
                }
                if (!self$.paramExists(dataFrame, C_KEY_WORDS_VARIANCE_EVENTS)) {
                    stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'varianceEvents' is missing")
                }

                self$.inputType <- "stagewise"

                self$events <- self$.getValidatedFloatingPointNumbers(
                    self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_EVENTS),
                    parameterName = "Events"
                )
                self$.validateValues(self$events, "events")

                self$expectedEvents <- self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_EXPECTED_EVENTS)
                self$.validateValues(self$expectedEvents, "expectedEvents")

                self$varianceEvents <- self$.getValuesByParameterName(dataFrame, C_KEY_WORDS_VARIANCE_EVENTS)
                self$.validateValues(varianceEvents, "varianceEvents")

                self$allocationRatios <- self$.getValuesByParameterName(
                    dataFrame,
                    parameterNameVariants = C_KEY_WORDS_ALLOCATION_RATIOS,
                    defaultValues = self$.getAllocationRatioDefaultValues(self$stages, self$events, self$expectedEvents)
                )
                self$.validateValues(self$allocationRatios, "allocationRatios")
            }

            self$.setParameterType("groups", C_PARAM_NOT_APPLICABLE)

            if (self$.inputType == "stagewise") {
                n <- length(self$events)
                self$overallExpectedEvents <- rep(NA_real_, n)
                self$overallVarianceEvents <- rep(NA_real_, n)

                self$.setParameterType("events", C_PARAM_USER_DEFINED)
                self$.setParameterType("allocationRatios", C_PARAM_USER_DEFINED)
                self$.setParameterType("expectedEvents", C_PARAM_USER_DEFINED)
                self$.setParameterType("varianceEvents", C_PARAM_USER_DEFINED)

                self$.setParameterType("overallEvents", C_PARAM_GENERATED)
                self$.setParameterType("overallAllocationRatios", C_PARAM_GENERATED)
                self$.setParameterType("overallExpectedEvents", C_PARAM_GENERATED)
                self$.setParameterType("overallVarianceEvents", C_PARAM_GENERATED)

                self$.recreateDataFrame()
                self$.createOverallData()
            } else {
                n <- length(self$overallEvents)
                self$expectedEvents <- rep(NA_real_, n)
                self$varianceEvents <- rep(NA_real_, n)

                self$.setParameterType("events", C_PARAM_GENERATED)
                self$.setParameterType("allocationRatios", C_PARAM_GENERATED)
                self$.setParameterType("expectedEvents", C_PARAM_GENERATED)
                self$.setParameterType("varianceEvents", C_PARAM_GENERATED)

                self$.setParameterType("overallEvents", C_PARAM_USER_DEFINED)
                self$.setParameterType("overallAllocationRatios", C_PARAM_USER_DEFINED)
                self$.setParameterType("overallExpectedEvents", C_PARAM_USER_DEFINED)
                self$.setParameterType("overallVarianceEvents", C_PARAM_USER_DEFINED)

                self$.recreateDataFrame()
                self$.createStageWiseData()
            }
        },
        .getVisibleFieldNames = function() {
            visibleFieldNames <- super$.getVisibleFieldNames()
            visibleFieldNames <- visibleFieldNames[!(visibleFieldNames %in% c("logRanks", "overallLogRanks"))]
            return(visibleFieldNames)
        },
        .setDataToVariables = function() {
            super$.setDataToVariables()
            self$overallExpectedEvents <- self$.data$overallExpectedEvent
            self$overallVarianceEvents <- self$.data$overallVarianceEvent
            self$expectedEvents <- self$.data$expectedEvent
        },
        getOverallExpectedEvent = function(stage, group = 1, subset = NA_character_) {
            return(self$.data$overallExpectedEvent[self$.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallExpectedEvents = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(self$.data$overallExpectedEvent[self$.getIndices(stage = self$.getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallExpectedEventsUpTo = function(to, group = 1, subset = NA_character_) {
            return(self$.data$overallExpectedEvent[self$.getIndices(stage = c(1:to), group = group, subset = subset)])
        },
        getOverallVarianceEvent = function(stage, group = 1, subset = NA_character_) {
            return(self$.data$overallVarianceEvent[self$.getIndices(stage = stage, group = group, subset = subset)])
        },
        getOverallVarianceEvents = function(..., stage = NA_integer_, group = NA_integer_, subset = NA_character_) {
            return(self$.data$overallVarianceEvent[self$.getIndices(stage = self$.getValidatedStage(stage), group = group, subset = subset)])
        },
        getOverallVarianceEventsUpTo = function(to, group = 1, subset = NA_character_) {
            return(self$.data$overallVarianceEvent[self$.getIndices(stage = c(1:to), group = group, subset = subset)])
        }
    )
)

.isFloatingPointSampleSize <- function(object, param) {
    values <- object[[param]]
    if (is.null(values)) {
        return(FALSE)
    }

    values <- na.omit(values)
    if (length(values) == 0) {
        return(FALSE)
    }

    if (any(floor(values) != values)) {
        return(TRUE)
    }

    return(FALSE)
}

.getMaxDigits <- function(values) {
    values <- na.omit(values)
    if (length(values) == 0) {
        return(0)
    }

    values <- trimws(format(values, scientific = FALSE, digits = 15))
    values <- gsub("^\\d*\\.", "", values)
    values <- gsub("\\D", "", values)
    max(nchar(values))
}


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

    summaryFactory <- SummaryFactory$new(object = object, intervalFormat = intervalFormat)

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

    paramsToCheck <- character()
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

.getDatasetArgumentsRCodeLines <- function(x, complete = FALSE, digits = 4) {
    m <- getWideFormat(x)
    lines <- character()
    paramNames <- colnames(m)
    if (!complete) {
        if (x$.inputType == "stagewise") {
            paramNames <- paramNames[!grepl("^overall", paramNames)]
        } else {
            paramNames <- paramNames[grepl("^(stage|group|subset|overall)", paramNames)]
        }
    }

    for (paramName in paramNames) {
        encapsulate <- grepl("^subset", paramName)
        if (!encapsulate || isTRUE(x$.enrichmentEnabled)) {
            values <- m[[paramName]]
            if (!encapsulate && is.numeric(values) && !is.null(digits) && length(digits) == 1 && !is.na(digits)) {
                values <- round(values, digits = digits)
            }
            lines <- c(lines, paste0(paramName, " = ", .arrayToString(values,
                vectorLookAndFeelEnabled = TRUE, encapsulate = encapsulate, digits = NA_integer_
            )))
        }
    }

    return(lines)
}

.isPrintSummaryCall <- function(sysCalls) {
    if (is.null(sysCalls) || length(sysCalls) == 0) {
        return(FALSE)
    }
    
    callText <- character()
    for (i in length(sysCalls):1) {
        callObj <- sysCalls[[i]]
        if (!is.null(callObj) && is.call(callObj)) {
            callText <- c(callText, capture.output(print(callObj)))            
        }
    }
    callText <- paste(callText, collapse = " ")
    
    if (grepl("plot\\(", callText)) {
        return(FALSE)
    }
    
    if (grepl("summary\\(print\\(", callText) && !grepl("getAnalysisResults", callText)) {
        return(TRUE)
    }
    
    return(FALSE)
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
print.Dataset <- function(x, ..., markdown = NA, output = c("list", "long", "wide", "r", "rComplete")) {
    fCall <- match.call(expand.dots = FALSE)
    sysCalls <- sys.calls()
    
    datasetName <- deparse(fCall$x)
    
    if (is.na(markdown)) {
        markdown <- .isMarkdownEnabled("print")
    }
    
    output <- match.arg(output)

    if (isTRUE(markdown)) {
        if (output != "list") {
            warning("'output' (\"", output, "\") will be ignored ",
                "because only \"list\" is supported yet if markdown is enabled",
                call. = FALSE
            )
        }
        
        if (.isPrintCall(sysCalls)) { 
            result <- paste0(utils::capture.output(x$.catMarkdownText()), collapse = "\n")
            return(knitr::asis_output(result))
        }
        
        if (.isPrintSummaryCall(sysCalls)) {
            attr(x, "markdown") <- TRUE
            queue <- attr(x, "queue")
            if (is.null(queue)) {
                queue <- list()
            }
            queue[[length(queue) + 1]] <- x
            attr(x, "queue") <- queue
        }
        
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
