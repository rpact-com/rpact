## |
## |  *Analysis of multi-arm designs with adaptive test*
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
## |  File version: $Revision: 6862 $
## |  Last changed: $Date: 2023-03-10 08:37:03 +0100 (Fr, 10 Mrz 2023) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_utilities.R
NULL

.getGMaxFromAnalysisResult <- function(results) {
    return(nrow(results$.stageResults$testStatistics))
}

.setNPlanned <- function(results, nPlanned) {
    design <- results$.design
    if (design$kMax == 1) {
        if (.isConditionalPowerEnabled(nPlanned)) {
            warning("'nPlanned' (", .arrayToString(nPlanned), ") ",
                "will be ignored because design is fixed",
                call. = FALSE
            )
        }
        results$.setParameterType("nPlanned", C_PARAM_NOT_APPLICABLE)
    }
    .setValueAndParameterType(results, "nPlanned", nPlanned, NA_real_)
    while (length(results$nPlanned) < design$kMax) {
        results$nPlanned <- c(NA_real_, results$nPlanned)
    }
    if (all(is.na(results$nPlanned))) {
        results$.setParameterType("nPlanned", C_PARAM_NOT_APPLICABLE)
    }
}

.isConditionalPowerEnabled <- function(nPlanned) {
    return(!is.null(nPlanned) && length(nPlanned) > 0 && !all(is.na(nPlanned)))
}

.warnInCaseOfUnusedConditionalPowerArgument <- function(results, nPlanned, paramName, paramValues) {
    if (!.isConditionalPowerEnabled(nPlanned)) {
        if (length(paramValues) > 0 && !all(is.na(paramValues)) &&
                results$.getParameterType(paramName) != C_PARAM_GENERATED) {
            warning("'", paramName, "' (", .arrayToString(paramValues), ") ",
                "will be ignored because 'nPlanned' is not defined",
                call. = FALSE
            )
        }
        return(invisible())
    }
    if (results$.design$kMax == 1) {
        if (length(paramValues) > 0 && !all(is.na(paramValues)) &&
                results$.getParameterType(paramName) != C_PARAM_GENERATED) {
            warning("'", paramName, "' (", .arrayToString(paramValues), ") ",
                "will be ignored because design is fixed",
                call. = FALSE
            )
        }
        return(invisible())
    }
}

.setNPlannedAndThetaH1 <- function(results, nPlanned, thetaH1) {
    .setNPlanned(results, nPlanned)
    .warnInCaseOfUnusedConditionalPowerArgument(results, nPlanned, "thetaH1", thetaH1)
    if (!is.matrix(results$thetaH1)) {
        if (results$.getParameterType("thetaH1") %in% c(C_PARAM_TYPE_UNKNOWN, C_PARAM_NOT_APPLICABLE)) {
            .setValueAndParameterType(results, "thetaH1", thetaH1, NA_real_)
        } else {
            results$thetaH1 <- thetaH1
            if (results$.getParameterType("thetaH1") == C_PARAM_TYPE_UNKNOWN) {
                results$.setParameterType("thetaH1", C_PARAM_USER_DEFINED)
            }
        }
    } else {
        if (results$.getParameterType("thetaH1") %in% c(C_PARAM_TYPE_UNKNOWN, C_PARAM_NOT_APPLICABLE)) {
            .setValueAndParameterType(results, "thetaH1",
                value = matrix(thetaH1, ncol = 1),
                defaultValue = matrix(rep(NA_real_, .getGMaxFromAnalysisResult(results)), ncol = 1)
            )
        } else {
            results$thetaH1 <- matrix(thetaH1, ncol = 1)
            if (results$.getParameterType("thetaH1") == C_PARAM_TYPE_UNKNOWN) {
                results$.setParameterType("thetaH1", C_PARAM_USER_DEFINED)
            }
        }
    }
}

.setNPlannedAndThetaH1AndAssumedStDev <- function(results, nPlanned, thetaH1, assumedStDev) {
    .setNPlannedAndThetaH1(results, nPlanned, thetaH1)
    .warnInCaseOfUnusedConditionalPowerArgument(results, nPlanned, "assumedStDev", assumedStDev)
    if (results$.getParameterType("assumedStDev") %in% c(C_PARAM_TYPE_UNKNOWN, C_PARAM_NOT_APPLICABLE)) {
        .setValueAndParameterType(results, "assumedStDev", assumedStDev, NA_real_)
    } else {
        results$assumedStDev <- assumedStDev
        if (results$.getParameterType("assumedStDev") == C_PARAM_TYPE_UNKNOWN) {
            results$.setParameterType("assumedStDev", C_PARAM_USER_DEFINED)
        }
    }
}

.setNPlannedAndThetaH1AndAssumedStDevs <- function(results, nPlanned, thetaH1, assumedStDevs) {
    .setNPlannedAndThetaH1(results, nPlanned, thetaH1)
    .warnInCaseOfUnusedConditionalPowerArgument(results, nPlanned, "assumedStDevs", assumedStDevs)
    if (results$.getParameterType("assumedStDevs") %in% c(C_PARAM_TYPE_UNKNOWN, C_PARAM_NOT_APPLICABLE)) {
        .setValueAndParameterType(results, "assumedStDevs",
            value = matrix(assumedStDevs, ncol = 1),
            defaultValue = matrix(rep(NA_real_, .getGMaxFromAnalysisResult(results)), ncol = 1)
        )
    } else {
        results$assumedStDevs <- matrix(assumedStDevs, ncol = 1)
        if (results$.getParameterType("assumedStDevs") == C_PARAM_TYPE_UNKNOWN) {
            results$.setParameterType("assumedStDevs", C_PARAM_USER_DEFINED)
        }
    }
}

.setNPlannedAndPi <- function(results, nPlanned, piControlName, piControlValues, piTreatments) {
    .setNPlanned(results, nPlanned)
    .warnInCaseOfUnusedConditionalPowerArgument(results, nPlanned, piControlName, piControlValues)
    .warnInCaseOfUnusedConditionalPowerArgument(results, nPlanned, "piTreatments", piTreatments)
    if (results$.getParameterType(piControlName) %in% c(C_PARAM_TYPE_UNKNOWN, C_PARAM_NOT_APPLICABLE)) {
        .setValueAndParameterType(
            results, piControlName,
            matrix(piControlValues, ncol = 1),
            matrix(rep(NA_real_, .getGMaxFromAnalysisResult(results)), ncol = 1)
        )
    } else {
        results[[piControlName]] <- matrix(piControlValues, ncol = 1)
        if (results$.getParameterType(piControlName) == C_PARAM_TYPE_UNKNOWN) {
            results$.setParameterType(piControlName, C_PARAM_USER_DEFINED)
        }
    }
    if (results$.getParameterType("piTreatments") == C_PARAM_TYPE_UNKNOWN) {
        .setValueAndParameterType(
            results, "piTreatments",
            matrix(piTreatments, ncol = 1),
            matrix(rep(NA_real_, .getGMaxFromAnalysisResult(results)), ncol = 1)
        )
    } else {
        results$piTreatments <- matrix(piTreatments, ncol = 1)
        if (results$.getParameterType("piTreatments") == C_PARAM_TYPE_UNKNOWN) {
            results$.setParameterType("piTreatments", C_PARAM_USER_DEFINED)
        }
    }
}

.getSortedSubsets <- function(subsets) {
    return(subsets[with(data.frame(subsets = subsets, index = as.integer(sub("\\D", "", subsets))), order(index))])
}

.getAllAvailableSubsets <- function(numbers, ..., sort = TRUE, digits = NA_integer_) {
    if (length(numbers) == 0) {
        return(character(0))
    }

    results <- paste0(numbers, collapse = "")
    for (n in numbers) {
        results <- c(results, .getAllAvailableSubsets(numbers[numbers != n], sort = sort))
    }
    if (!is.na(digits)) {
        results <- results[nchar(results) == digits]
    }
    if (!sort) {
        return(unique(results))
    }

    return(.getSortedSubsets(unique(results)))
}

.createSubsetsByGMax <- function(gMax, ..., stratifiedInput = TRUE,
        subsetIdPrefix = "S", restId = ifelse(stratifiedInput, "R", "F"),
        all = TRUE) {
    .assertIsSingleInteger(gMax, "gMax", validateType = FALSE)
    .assertIsInClosedInterval(gMax, "gMax", lower = 1, upper = 10)
    if (gMax == 1) {
        subsetName <- paste0(subsetIdPrefix, 1)
        subsetName <- ifelse(stratifiedInput, subsetName, "F")
        if (!all) {
            return(subsetName)
        }

        return(list(subsetName))
    }

    numbers <- 1:(gMax - 1)
    subsets <- list()
    if (stratifiedInput) {
        availableSubsets <- paste0(subsetIdPrefix, .getAllAvailableSubsets(numbers))
    } else {
        availableSubsets <- paste0(subsetIdPrefix, numbers)
    }
    for (i in numbers) {
        subset <- availableSubsets[grepl(i, availableSubsets)]
        subsets[[length(subsets) + 1]] <- subset
    }
    if (stratifiedInput) {
        subsets[[length(subsets) + 1]] <- c(availableSubsets, restId)
    } else {
        subsets[[length(subsets) + 1]] <- restId
    }
    if (!all) {
        if (!stratifiedInput) {
            return(unlist(subsets))
        }

        return(subsets[[gMax]])
    }

    return(subsets)
}

.arraysAreEqual <- function(a1, a2) {
    if (length(a1) != length(a2)) {
        return(FALSE)
    }

    l <- length(a1)
    if (l > 0) {
        a1 <- sort(a1)
        a2 <- sort(a2)
        if (sum(a1 == a2) < l) {
            return(FALSE)
        }
    }

    return(TRUE)
}

.getNumberOfGroupsFromArgumentNames <- function(argNames) {
    numbers <- gsub("\\D", "", argNames)
    numbers <- numbers[numbers != ""]
    return(ifelse(length(numbers) == 0, 1, max(as.numeric(numbers))))
}

.getGroupNumberFromArgumentName <- function(argName) {
    n <- gsub("\\D", "", argName)
    return(ifelse(n == "", 1, as.numeric(n)))
}

.isControlGroupArgument <- function(argName, numberOfGroups) {
    if (numberOfGroups <= 2) {
        return(FALSE)
    }

    return(ifelse(numberOfGroups == 1, FALSE, .getGroupNumberFromArgumentName(argName) == numberOfGroups))
}

.naOmitBackward <- function(x) {
    indices <- which(is.na(x))
    if (length(indices) == 0) {
        return(x)
    }

    if (length(x) == 1 || !is.na(x[length(x)])) {
        return(x)
    }

    if (length(indices) == 1) {
        return(x[1:(length(x) - 1)])
    }

    indexBefore <- NA_real_
    for (i in length(indices):1) {
        index <- indices[i]
        if (!is.na(indexBefore) && index != indexBefore - 1) {
            return(x[1:(indexBefore - 1)])
        }
        indexBefore <- index
    }
    if (!is.na(indexBefore)) {
        return(x[1:(indexBefore - 1)])
    }
    return(x)
}

.getNumberOfStagesFromArguments <- function(args, argNames) {
    numberOfStages <- 1
    for (argName in argNames) {
        argValues <- args[[argName]]
        n <- length(.naOmitBackward(argValues))
        if (n > numberOfStages) {
            numberOfStages <- n
        }
    }
    return(numberOfStages)
}

.getNumberOfSubsetsFromArguments <- function(args, argNames) {
    numberOfSubsets <- 1
    for (argName in argNames) {
        argValues <- args[[argName]]
        n <- length(na.omit(argValues))
        if (n > numberOfSubsets) {
            numberOfSubsets <- n
        }
    }
    return(numberOfSubsets)
}

.assertIsValidTreatmentArmArgumentDefined <- function(args, argNames, numberOfGroups, numberOfStages) {
    tratmentArgNames <- argNames[!grepl(paste0(".*\\D{1}", numberOfGroups, "$"), argNames)]
    for (argName in tratmentArgNames) {
        argValues <- args[[argName]]
        if (!is.null(argValues) && length(.naOmitBackward(argValues)) == numberOfStages) {
            return(invisible())
        }
    }
    stop(
        C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
        "at least for one treatment arm the values for ", numberOfStages, " stages must be defined ",
        "because the control arm defines ", numberOfStages, " stages"
    )
}

.createDataFrame <- function(...) {
    args <- list(...)
    args <- .removeDesignFromArgs(args)
    argNames <- .getArgumentNames(...)
    if (length(args) == 0 || length(argNames) == 0) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data.frame or data vectors expected")
    }

    multiArmEnabled <- any(grep("3", argNames))
    numberOfGroups <- .getNumberOfGroupsFromArgumentNames(argNames)
    numberOfStages <- .getNumberOfStagesFromArguments(args, argNames)
    survivalDataEnabled <- .isDataObjectSurvival(...)
    enrichmentEnabled <- .isDataObjectEnrichment(...)
    numberOfSubsets <- 1
    if (enrichmentEnabled) {
        numberOfSubsets <- .getNumberOfSubsetsFromArguments(args, argNames)
    }
    if (multiArmEnabled) {
        .assertIsValidTreatmentArmArgumentDefined(args, argNames, numberOfGroups, numberOfStages)
    }

    numberOfValues <- length(args[[1]])
    naIndicesBefore <- NULL
    if (!survivalDataEnabled && multiArmEnabled) {
        naIndicesBefore <- list()
    }
    for (argName in argNames) {
        argValues <- args[[argName]]
        if (is.null(argValues) || length(argValues) == 0) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'", argName, "' is not a valid numeric vector"
            )
        }

        if (is.na(argValues[1])) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'", argName, "' is NA at first stage; a valid numeric value must be specified at stage 1"
            )
        }

        if (length(argValues) != numberOfValues) {
            stop(
                C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                "all data vectors must have the same length: '",
                argName, "' (", length(argValues), ") differs from '",
                argNames[1], "' (", numberOfValues, ")"
            )
        }

        if (.equalsRegexpIgnoreCase(argName, "^stages?$")) {
            if (length(stats::na.omit(argValues)) != length(argValues)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "NA's not allowed for '", argName, "'; stages must be defined completely"
                )
            }

            definedStages <- sort(intersect(unique(argValues), 1:numberOfValues))
            if (length(definedStages) < numberOfValues) {
                if (length(definedStages) == 0) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "no valid stages are defined; ",
                        "stages must be defined completely (", .arrayToString(1:numberOfValues), ")"
                    )
                }
                if (!enrichmentEnabled) {
                    msg <- ifelse(length(definedStages) == 1,
                        paste0("only stage ", definedStages, " is defined"),
                        paste0("only stages ", .arrayToString(definedStages), " are defined")
                    )
                    stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, msg, "; stages must be defined completely")
                }
            }
        }

        if (!survivalDataEnabled && .isControlGroupArgument(argName, numberOfGroups) &&
                length(na.omit(argValues)) < numberOfStages) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "control group '", argName, "' (", .arrayToString(argValues, digits = 2), ") must be defined for all stages"
            )
        }

        naIndices <- which(is.na(argValues))
        if (length(naIndices) > 0) {
            stageIndex <- naIndices[length(naIndices)]
            if (stageIndex != numberOfValues) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'", argName, "' contains a NA at stage ", stageIndex,
                    " followed by a value for a higher stage; NA's must be the last values"
                )
            }
        }

        if (length(naIndices) > 1 && !enrichmentEnabled) {
            indexBefore <- naIndices[length(naIndices)]
            for (i in (length(naIndices) - 1):1) {
                index <- naIndices[i]
                if (indexBefore - index > 1) {
                    stop(
                        C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                        "'", argName, "' contains alternating values and NA's; ",
                        "NA's must be the last values"
                    )
                }
                indexBefore <- index
            }
        }

        if (!enrichmentEnabled) {
            if (!multiArmEnabled && !survivalDataEnabled) {
                if (!is.null(naIndicesBefore) && !.equalsRegexpIgnoreCase(argName, "^stages?$")) {
                    if (!.arraysAreEqual(naIndicesBefore, naIndices)) {
                        stop(
                            C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                            "inconsistent NA definition; ",
                            "if NA's exist, then they are mandatory for each group at the same stage"
                        )
                    }
                }
                naIndicesBefore <- naIndices
            } else {
                groupNumber <- .getGroupNumberFromArgumentName(argName)
                if (!is.null(naIndicesBefore[[as.character(groupNumber)]]) &&
                        !.equalsRegexpIgnoreCase(argName, "^stages?$") &&
                        !.isControlGroupArgument(argName, numberOfGroups)) {
                    if (!.arraysAreEqual(naIndicesBefore[[as.character(groupNumber)]], naIndices)) {
                        stop(
                            C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                            "values of treatment ", groupNumber, " not correctly specified; ",
                            "if NA's exist, then they are mandatory for each parameter at the same stage"
                        )
                    }
                }
                if (!.isControlGroupArgument(argName, numberOfGroups)) {
                    naIndicesBefore[[as.character(groupNumber)]] <- naIndices
                }
            }
        }

        if (sum(is.infinite(argValues)) > 0) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all data values must be finite; ",
                "'", argName, "' contains infinite values"
            )
        }

        if (!any(grepl(paste0("^", sub("\\d*$", "", argName), "$"), C_KEY_WORDS_SUBSETS)) && !is.numeric(argValues)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all data vectors must be numeric ('",
                argName, "' is ", .getClassName(argValues), ")"
            )
        }

        if (length(argValues) > C_KMAX_UPPER_BOUND * numberOfSubsets) {
            stop(
                C_EXCEPTION_TYPE_ARGUMENT_LENGTH_OUT_OF_BOUNDS,
                "'", argName, "' is out of bounds [1, ", C_KMAX_UPPER_BOUND, "]"
            )
        }
    }

    if (!enrichmentEnabled) {
        for (groupNumber in 1:numberOfGroups) {
            groupVars <- argNames[grepl(paste0("\\D", groupNumber, "$"), argNames)]
            naIndicesBefore <- NULL
            for (argName in groupVars) {
                argValues <- args[[argName]]
                naIndices <- which(is.na(argValues))
                if (!is.null(naIndicesBefore) && !.equalsRegexpIgnoreCase(argName, "^stages?$")) {
                    if (!.arraysAreEqual(naIndicesBefore, naIndices)) {
                        stop(
                            C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
                            "inconsistent NA definition for group ", groupNumber, "; ",
                            "if NA's exist, then they are mandatory for each group at the same stage"
                        )
                    }
                }
                naIndicesBefore <- naIndices
            }
        }
    }

    dataFrame <- as.data.frame(args)
    if (length(intersect(tolower(names(dataFrame)), c("stage", "stages"))) == 0) {
        dataFrame$stages <- 1:nrow(dataFrame)
    }
    return(dataFrame)
}

.getDataFrameFromArgs <- function(...) {
    args <- list(...)
    if (length(args) == 0) {
        stop(
            C_EXCEPTION_TYPE_MISSING_ARGUMENT,
            "cannot initialize dataset because no data are defined"
        )
    }

    dataFrame <- NULL
    dataFrameCounter <- 0
    for (arg in args) {
        if (is.data.frame(arg)) {
            dataFrameCounter <- dataFrameCounter + 1
            if (is.null(dataFrame)) {
                dataFrame <- arg
            }
        }
    }

    if (dataFrameCounter > 1) {
        warning("Found ", dataFrameCounter, ", data.frame arguments; ",
            "only the first data.frame will be used for the initialization of the dataset",
            call. = FALSE
        )
    }

    return(dataFrame)
}

.getDesignFromArgs <- function(...) {
    args <- list(...)
    if (length(args) == 0) {
        return(NULL)
    }

    for (arg in args) {
        if (.isTrialDesign(arg)) {
            return(arg)
        }
    }

    return(NULL)
}

.getDatasetFromArgs <- function(...) {
    args <- list(...)
    if (length(args) == 0) {
        return(NULL)
    }

    for (arg in args) {
        if (.isDataset(arg)) {
            return(arg)
        }
    }

    return(NULL)
}

.removeDesignFromArgs <- function(args) {
    for (i in 1:length(args)) {
        if (.isTrialDesign(args[[i]])) {
            return(args[-i])
        }
    }
    return(args)
}

.getArgumentNames <- function(...) {
    dataFrame <- .getDataFrameFromArgs(...)
    if (!is.null(dataFrame)) {
        return(names(dataFrame))
    }

    args <- list(...)
    if (length(args) == 0) {
        return(character(0))
    }
    
    args <- .removeDesignFromArgs(args)

    return(names(args))
}

.assertIsValidDatasetArgument <- function(...) {
    argNames <- .getArgumentNames(...)
    if (length(argNames) == 0) {
        return(TRUE)
    }

    argNamesLower <- tolower(argNames)
    dataObjectkeyWords <- unique(tolower(C_KEY_WORDS))

    multiArmKeywords <- tolower(c(
        C_KEY_WORDS_SUBSETS,
        C_KEY_WORDS_EVENTS,
        C_KEY_WORDS_OVERALL_EVENTS,
        C_KEY_WORDS_SAMPLE_SIZES,
        C_KEY_WORDS_OVERALL_SAMPLE_SIZES,
        C_KEY_WORDS_MEANS,
        C_KEY_WORDS_OVERALL_MEANS,
        C_KEY_WORDS_ST_DEVS,
        C_KEY_WORDS_OVERALL_ST_DEVS,
        C_KEY_WORDS_ALLOCATION_RATIOS,
        C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS,
        C_KEY_WORDS_LOG_RANKS,
        C_KEY_WORDS_OVERALL_LOG_RANKS
    ))
    enrichmentKeywords <- tolower(c(
        C_KEY_WORDS_EXPECTED_EVENTS,
        C_KEY_WORDS_VARIANCE_EVENTS,
        C_KEY_WORDS_OVERALL_EXPECTED_EVENTS,
        C_KEY_WORDS_OVERALL_VARIANCE_EVENTS
    ))
    unknownArgs <- setdiff(argNamesLower, dataObjectkeyWords)
    unknownArgsChecked <- unknownArgs
    unknownArgs <- c()
    for (unknownArg in unknownArgsChecked) {
        unknown <- TRUE
        for (multiArmKeyword in multiArmKeywords) {
            if (grepl(paste0(multiArmKeyword, "\\d{1,4}"), unknownArg)) {
                unknown <- FALSE
            }
        }
        for (enrichmentKeyword in enrichmentKeywords) {
            if (grepl(enrichmentKeyword, unknownArg)) {
                unknown <- FALSE
            }
        }
        if (unknown) {
            unknownArgs <- c(unknownArgs, unknownArg)
        }
    }

    if (length(unknownArgs) > 0) {
        for (i in 1:length(unknownArgs)) {
            unknownArgs[i] <- argNames[argNamesLower == unknownArgs[i]][1]
        }
        if (length(unknownArgs) == 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "the argument '", unknownArgs, "' is not a valid dataset argument"
            )
        } else {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "the arguments ", .arrayToString(unknownArgs, encapsulate = TRUE),
                " are no valid dataset arguments"
            )
        }
    }

    invisible(TRUE)
}

.getParameterNameVariant <- function(x, sep = ".") { # x <- "overallExpectedEvents"
    if (identical(x, tolower(x))) {
        return(x)
    }

    indices <- gregexpr("[A-Z]", x)[[1]]
    parts <- strsplit(x, "[A-Z]")[[1]]
    result <- ""
    for (i in 1:length(indices)) {
        index <- indices[i]
        y <- tolower(substring(x, index, index))
        result <- paste0(result, parts[i], sep, y)
    }
    if (length(parts) > length(indices)) {
        result <- paste0(result, parts[length(parts)])
    }
    return(trimws(result))
}

.getAllParameterNameVariants <- function(parameterNameVariants) {
    overallParameterNameVariants <- parameterNameVariants[grepl("^overall", parameterNameVariants)]
    if (length(overallParameterNameVariants) > 0) {
        overallParameterNameVariants <- c(
            gsub("^overall", "cumulative", overallParameterNameVariants),
            gsub("^overall", "cum", overallParameterNameVariants)
        )
    }
    parameterNameVariants <- c(parameterNameVariants, overallParameterNameVariants)
    otherVariants <- character(0)
    for (parameterNameVariant in parameterNameVariants) {
        otherVariants <- c(otherVariants, .getParameterNameVariant(parameterNameVariant, "."))
        otherVariants <- c(otherVariants, .getParameterNameVariant(parameterNameVariant, "_"))
    }
    return(unique(c(parameterNameVariants, otherVariants)))
}

.isDataObject <- function(..., dataObjectkeyWords) {
    .assertIsValidDatasetArgument(...)
    argNames <- .getArgumentNames(...)
    if (length(argNames) == 0) {
        return(FALSE)
    }
    
    dataObjectkeyWords <- .getAllParameterNameVariants(dataObjectkeyWords)
    matching <- intersect(argNames, dataObjectkeyWords)
    return(length(matching) > 0)
}

.isDataObjectEnrichment <- function(...) {
    enrichmentEnabled <- .isDataObject(...,
        dataObjectkeyWords = c(C_KEY_WORDS_SUBSETS, paste0(C_KEY_WORDS_SUBSETS, "1"))
    )
    if (!enrichmentEnabled) {
        return(FALSE)
    }
    
    args <- list(...)
    if (length(args) == 1 && is.data.frame(args[[1]])) {
        data <- args[[1]]
        if ("subsets" %in% colnames(data) && all(is.na(data[["subsets"]]))) {
            return(FALSE)
        }
    }
    
    return(enrichmentEnabled)
}

.isDataObjectMeans <- function(...) {
    dataObjectkeyWords <- c(
        C_KEY_WORDS_MEANS,
        C_KEY_WORDS_ST_DEVS,
        C_KEY_WORDS_OVERALL_MEANS,
        C_KEY_WORDS_OVERALL_ST_DEVS
    )
    dataObjectkeyWords <- c(dataObjectkeyWords, paste0(dataObjectkeyWords, c(1, 2)))
    return(.isDataObject(..., dataObjectkeyWords = dataObjectkeyWords))
}

.isDataObjectRates <- function(...) {
    dataObjectkeyWordsExpected <- c(
        C_KEY_WORDS_EVENTS,
        C_KEY_WORDS_OVERALL_EVENTS
    )
    dataObjectkeyWordsForbidden <- c(
        C_KEY_WORDS_OVERALL_LOG_RANKS,
        C_KEY_WORDS_LOG_RANKS,
        C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS,
        C_KEY_WORDS_ALLOCATION_RATIOS,
        C_KEY_WORDS_EXPECTED_EVENTS,
        C_KEY_WORDS_VARIANCE_EVENTS,
        C_KEY_WORDS_OVERALL_EXPECTED_EVENTS,
        C_KEY_WORDS_OVERALL_VARIANCE_EVENTS
    )

    dataObjectkeyWordsExpected <- c(dataObjectkeyWordsExpected, paste0(dataObjectkeyWordsExpected, c(1, 2)))
    dataObjectkeyWordsForbidden <- c(dataObjectkeyWordsForbidden, paste0(dataObjectkeyWordsForbidden, c(1, 2)))

    return(.isDataObject(..., dataObjectkeyWords = dataObjectkeyWordsExpected) &&
        !.isDataObject(..., dataObjectkeyWords = dataObjectkeyWordsForbidden))
}

.isDataObjectSurvival <- function(...) {
    dataObjectkeyWords <- c(
        C_KEY_WORDS_OVERALL_LOG_RANKS,
        C_KEY_WORDS_LOG_RANKS,
        C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS,
        C_KEY_WORDS_ALLOCATION_RATIOS
    )
    dataObjectkeyWords <- c(dataObjectkeyWords, paste0(dataObjectkeyWords, c(1, 2)))
    return(.isDataObject(..., dataObjectkeyWords = dataObjectkeyWords))
}

.isDataObjectNonStratifiedEnrichmentSurvival <- function(...) {
    dataObjectkeyWords <- c(
        C_KEY_WORDS_EXPECTED_EVENTS,
        C_KEY_WORDS_VARIANCE_EVENTS,
        C_KEY_WORDS_OVERALL_EXPECTED_EVENTS,
        C_KEY_WORDS_OVERALL_VARIANCE_EVENTS
    )
    return(.isDataObject(..., dataObjectkeyWords = dataObjectkeyWords))
}

#'
#' @title
#' Get Wide Format
#'
#' @description
#' Returns the specified dataset as a \code{\link[base]{data.frame}} in so-called wide format.
#'
#' @details
#' In the wide format (unstacked), the data are presented with each different data variable in a separate column, i.e.,
#' the different groups are in separate columns.
#'
#' @seealso
#' \code{\link[=getLongFormat]{getLongFormat()}} for returning the dataset as a \code{\link[base]{data.frame}} in long format.
#'
#' @return A \code{\link[base]{data.frame}} will be returned.
#'
#' @keywords internal
#'
#' @export
#'
getWideFormat <- function(dataInput) {
    .assertIsDataset(dataInput)
    paramNames <- names(dataInput)
    paramNames <- paramNames[!(paramNames %in% c("groups"))]
    if (!dataInput$.enrichmentEnabled) {
        paramNames <- paramNames[!(paramNames %in% c("subsets"))]
    }
    
    numberOfSubsets <- dataInput$getNumberOfSubsets()
    numberOfGroups <- dataInput$getNumberOfGroups(survivalCorrectionEnabled = FALSE)
    if (numberOfSubsets <= 1) {
        numberOfStages <- dataInput$getNumberOfStages()
        df <- data.frame(stages = 1:numberOfStages)
    } else {
        numberOfStages <- length(dataInput$subsets) / numberOfGroups / numberOfSubsets
        df <- data.frame(stages = rep(1:numberOfStages, numberOfSubsets))
    }
    for (paramName in paramNames) {
        if (numberOfGroups == 1) {
            df[[paramName]] <- dataInput[[paramName]]
        } else {
            for (group in 1:numberOfGroups) {
                if (paramName %in% c("stages", "subsets")) {
                    varName <- paramName
                } else {
                    varName <- paste0(paramName, group)
                }
                df[[varName]] <- dataInput[[paramName]][dataInput$groups == group]
            }
        }
    }
    return(df)
}

.getNumberOfStages <- function(dataFrame, naOmitEnabled = TRUE) {
    if (naOmitEnabled) {
        colNames <- colnames(dataFrame)
        validColNames <- character(0)
        for (colName in colNames) {
            colValues <- dataFrame[, colName]
            if (length(colValues) > 0 && !all(is.na(colValues))) {
                validColNames <- c(validColNames, colName)
            }
        }
        subData <- stats::na.omit(dataFrame[, validColNames])
        numberOfStages <- length(unique(as.character(subData$stage)))
        if (numberOfStages == 0) {
            print(dataFrame[, validColNames])
            stop(
                C_EXCEPTION_TYPE_RUNTIME_ISSUE,
                "'dataFrame' seems to contain an invalid column"
            )
        }
        return(numberOfStages)
    }
    return(length(levels(dataFrame$stage)))
}

.getWideFormat <- function(dataFrame) {
    if (!is.data.frame(dataFrame)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dataFrame' must be a data.frame (is ", .getClassName(dataFrame), ")")
    }

    paramNames <- names(dataFrame)
    paramNames <- paramNames[!(paramNames %in% c("stage", "group", "subset"))]
    numberOfSubsets <- ifelse(is.factor(dataFrame$subset),
        length(levels(dataFrame$subset)), length(unique(na.omit(dataFrame$subset)))
    )
    numberOfGroups <- ifelse(is.factor(dataFrame$group),
        length(levels(dataFrame$group)), length(unique(na.omit(dataFrame$group)))
    )
    if (numberOfSubsets <= 1) {
        df <- data.frame(stage = 1:.getNumberOfStages(dataFrame))
    } else {
        df <- data.frame(stage = 1:(length(dataFrame$subset) / numberOfGroups))
    }
    for (paramName in paramNames) {
        if (numberOfGroups == 1) {
            df[[paramName]] <- dataFrame[[paramName]]
        } else {
            for (group in 1:numberOfGroups) {
                varName <- paste0(paramName, group)
                values <- dataFrame[[paramName]][dataFrame$group == group]
                df[[varName]] <- values
            }
        }
    }

    if (numberOfSubsets > 1) {
        stages <- dataFrame$stage[dataFrame$group == 1]
        df$stage <- stages # sort(rep(stages, multiplier))

        subsets <- dataFrame$subset[dataFrame$group == 1]
        if (nrow(df) != length(subsets)) {
            stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "something went wrong: ", nrow(df), " != ", length(subsets))
        }
        df$subset <- subsets
        df <- .moveColumn(df, "subset", "stage")
        # 		df <- df[with(data.frame(subset = df$subset, index = as.integer(sub("\\D", "", df$subset))), order(index)), ]
    }

    return(df)
}

#'
#' @title
#' Get Long Format
#'
#' @description
#' Returns the specified dataset as a \code{\link[base]{data.frame}} in so-called long format.
#'
#' @details
#' In the long format (narrow, stacked), the data are presented with one column containing
#' all the values and another column listing the context of the value, i.e.,
#' the data for the different groups are in one column and the dataset contains an additional "group" column.
#'
#' @seealso
#' \code{\link[=getWideFormat]{getWideFormat()}} for returning the dataset as a \code{\link[base]{data.frame}} in wide format.
#'
#' @return A \code{\link[base]{data.frame}} will be returned.
#'
#' @keywords internal
#'
#' @export
#'
getLongFormat <- function(dataInput) {
    .assertIsDataset(dataInput)
    return(as.data.frame(dataInput, niceColumnNamesEnabled = FALSE))
}

.setConditionalPowerArguments <- function(results, dataInput, nPlanned, allocationRatioPlanned) {
    .assertIsAnalysisResults(results)
    .setNPlanned(results, nPlanned)
    numberOfGroups <- dataInput$getNumberOfGroups()
    .assertIsValidAllocationRatioPlanned(allocationRatioPlanned, numberOfGroups)

    if (!.isConditionalPowerEnabled(nPlanned) || numberOfGroups == 1) {
        if (numberOfGroups == 1) {
            if (length(allocationRatioPlanned) == 1 && !identical(allocationRatioPlanned, 1)) {
                warning("'allocationRatioPlanned' (", allocationRatioPlanned, ") ",
                    "will be ignored because the specified data has only one group",
                    call. = FALSE
                )
            }
        } else if (!identical(allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT)) {
            warning("'allocationRatioPlanned' (", allocationRatioPlanned, ") ",
                "will be ignored because 'nPlanned' is not defined",
                call. = FALSE
            )
        }
        results$.setParameterType("allocationRatioPlanned", C_PARAM_NOT_APPLICABLE)
        return(invisible(results))
    }

    .setValueAndParameterType(results, "allocationRatioPlanned", allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT)
    return(invisible(results))
}

.getRecalculatedInformationRates <- function(dataInput, maxInformation, stage = NA_integer_) {
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
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'dataInput' class ", .getClassName(dataInput), " is not supported")
    }
    return(list(informationRates = informationRates, absoluteInformations = absoluteInformations, stage = stage))
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
#'   \item https://www.rpact.com/vignettes/rpact_boundary_update_example
#' }
#'
#' @examples
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
#' 
#' @return Returns a list that summarizes the observed information rates.
#' 
#' @export
#'
getObservedInformationRates <- function(dataInput, ...,
        maxInformation = NULL, informationEpsilon = NULL, stage = NA_integer_) {
    .assertIsDataset(dataInput)
    .assertIsSingleInteger(maxInformation, "maxInformation", validateType = FALSE)

    information <- .getRecalculatedInformationRates(dataInput, maxInformation, stage = stage)
    informationRates <- information$informationRates
    absoluteInformations <- information$absoluteInformations
    stage <- information$stage

    status <- "interim-stage"

    showObservedInformationRatesMessage <- .getOptionalArgument("showObservedInformationRatesMessage", ...)
    if (is.null(showObservedInformationRatesMessage) || !is.logical(showObservedInformationRatesMessage)) {
        showObservedInformationRatesMessage <- TRUE
    }

    # Updates at the final analysis in case the observed information at the final analysis
    # is larger ("over-running") or smaller ("under-running") than the planned maximum information
    if (informationRates[length(informationRates)] < 1) {
        underRunningEnabled <- FALSE
        if (!is.null(informationEpsilon)) {
            .assertIsSingleNumber(informationEpsilon, "informationEpsilon")
            .assertIsInOpenInterval(informationEpsilon, "informationEpsilon", lower = 0, upper = maxInformation)

            lastInformationRate <- informationRates[length(informationRates)]
            lastInformationNumber <- absoluteInformations[length(absoluteInformations)]

            if (informationEpsilon < 1) {
                if (lastInformationRate >= (1 - informationEpsilon)) {
                    message(
                        "Under-running: relative information epsilon ", round(informationEpsilon, 4), " is applicable; ",
                        "use observed information ", lastInformationNumber, " instead of planned information ", maxInformation
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
                        "Under-running: absolute information epsilon ", round(informationEpsilon, 1), " is applicable; ",
                        "use observed information ", lastInformationNumber, " instead of planned information ", maxInformation
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
                "Over-running: observed information ", lastInformationNumber, " at stage ", length(absoluteInformations),
                " is larger than the maximum planned information ", maxInformation, "; information rates will be recalculated"
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

.synchronizeIterationsAndSeed <- function(results) {
    if (is.null(results[[".conditionalPowerResults"]])) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, sQuote(.getClassName(results)), 
            " does not contain field ", sQuote(".conditionalPowerResults"))
    }
    
    if (results$.design$kMax == 1) {
        return(invisible(results))
    }
    
    if (results$.conditionalPowerResults$simulated) {
        results$conditionalPowerSimulated <- results$.conditionalPowerResults$conditionalPower
        results$.setParameterType("conditionalPower", C_PARAM_NOT_APPLICABLE)
        results$.setParameterType("conditionalPowerSimulated", C_PARAM_GENERATED)
    
        results$.setParameterType("seed", results$.conditionalPowerResults$.getParameterType("seed"))
        results$seed <- results$.conditionalPowerResults$seed
        results$.setParameterType("iterations",
            results$.conditionalPowerResults$.getParameterType("iterations")
        )
        results$iterations <- results$.conditionalPowerResults$iterations
    } else {
        results$conditionalPower <- results$.conditionalPowerResults$conditionalPower
        if (is.matrix(results$conditionalPowerSimulated)) {
            results$conditionalPowerSimulated <- matrix()
        } else {
            results$conditionalPowerSimulated <- numeric(0)
        }
        results$.setParameterType("conditionalPower", C_PARAM_GENERATED)
        results$.setParameterType("conditionalPowerSimulated", C_PARAM_NOT_APPLICABLE)
        
        results$.setParameterType("seed", C_PARAM_NOT_APPLICABLE)
        results$.setParameterType("iterations", C_PARAM_NOT_APPLICABLE)
    }
    
    return(invisible(results))
}

.updateParameterTypeOfIterationsAndSeed <- function(results, ...) {
    if (!results$simulated) {
        results$iterations <- NA_integer_
        results$seed <- NA_real_
        results$.setParameterType("iterations", C_PARAM_NOT_APPLICABLE)
        results$.setParameterType("seed", C_PARAM_NOT_APPLICABLE)
        return(invisible(results))
    }
    
    iterations <- .getOptionalArgument("iterations", ...)
    results$.setParameterType("iterations", ifelse(is.null(iterations) || is.na(iterations) || 
            identical(iterations, C_ITERATIONS_DEFAULT),
        C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED
    ))
    
    seed <- .getOptionalArgument("seed", ...)
    results$.setParameterType("seed", ifelse(!is.null(seed) && !is.na(seed),
        C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE
    ))
    
    return(invisible(results))
}
