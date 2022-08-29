## |
## |  *Simulation of multi-arm design with combination test and conditional error approach*
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
## |  File version: $Revision: 6485 $
## |  Last changed: $Date: 2022-08-12 13:20:22 +0200 (Fr, 12 Aug 2022) $
## |  Last changed by: $Author: pahlke $
## |

#' @include f_core_utilities.R
NULL

#
# @title
# Set Seed
#
# @description
# Sets the seed, generates it if \code{is.na(seed) == TRUE} and returns it.
#
# @param seed the seed to set.
#
# @details
# Internal function.
#
# @return the (generated) seed.
#
# @examples
#
# .setSeed(12345)
#
# mySeed <- .setSeed()
#
# @keywords internal
#
.setSeed <- function(seed = NA_real_) {
    if (!is.null(seed) && !is.na(seed)) {
        if (is.na(as.integer(seed))) {
            stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'seed' must be a valid integer")
        }
        
        set.seed(seed = seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
        return(seed)
    }
    
    if (exists(".Random.seed") && length(.Random.seed) > 0) {
        seed <- .Random.seed[length(.Random.seed)]
    } else {
        seed <- round(stats::runif(1) * 1e8)
    }
    
    .logDebug("Set seed to %s", seed)
    
    tryCatch(
        {
            set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
        },
        error = function(e) {
            .logError("Failed to set seed to '%s' (%s): %s", seed, .getClassName(seed), e)
            seed <- NA_real_
            traceback()
        }
    )
    
    invisible(seed)
}

.getGMaxFromSubGroups <- function(subGroups) {
    .assertIsCharacter(subGroups, "subGroups")
    subGroups[subGroups == "S"] <- "S1"
    subGroups <- trimws(gsub("\\D", "", subGroups))
    subGroups <- subGroups[subGroups != ""]
    if (length(subGroups) == 0) {
        return(1)
    }

    gMax <- max(as.integer(unlist(strsplit(subGroups, "", fixed = TRUE)))) + 1
    return(gMax)
}

.getSimulationParametersFromRawData <- function(data, ..., variantName, maxNumberOfIterations = NA_integer_) {
    
    if (is.null(data) || length(data) != 1) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'data' must be a valid data.frame or a simulation result object")
    }
        
    if (inherits(data, "SimulationResults")) {
        data <- data[[".data"]]
    }
        
    if (!is.data.frame(data)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'data' (", .getClassName(data), ") must be a data.frame or a simulation result object")
    }
    
    if (is.na(maxNumberOfIterations)) {
        maxNumberOfIterations <- max(data$iterationNumber)
    }

    stageNumbers <- sort(unique(na.omit(data$stageNumber)))
    kMax <- max(stageNumbers)

    variantLevels <- sort(unique(na.omit(data[[variantName]])))
    numberOfVariants <- length(variantLevels)
    sampleSizes <- matrix(0, nrow = kMax, ncol = numberOfVariants)
    rejectPerStage <- matrix(0, nrow = kMax, ncol = numberOfVariants)
    futilityPerStage <- matrix(0, nrow = kMax - 1, ncol = numberOfVariants)
    expectedNumberOfSubjects <- rep(0, numberOfVariants)
    conditionalPowerAchieved <- matrix(NA_real_, nrow = kMax, ncol = numberOfVariants)

    index <- 1
    for (variantValue in variantLevels) {
        subData <- data[data[[variantName]] == variantValue, ]
        iterations <- table(subData$stageNumber)
        for (k in sort(unique(na.omit(subData$stageNumber)))) {
            subData2 <- subData[subData$stageNumber == k, ]
            sampleSizes[k, index] <- sum(subData2$numberOfSubjects) / iterations[k]
            rejectPerStage[k, index] <- sum(subData2$rejectPerStage) / maxNumberOfIterations
            if (k < kMax) {
                futilityPerStage[k, index] <- sum(na.omit(subData2$futilityPerStage)) / maxNumberOfIterations
            }
            expectedNumberOfSubjects[index] <- expectedNumberOfSubjects[index] +
                sum(subData2$numberOfSubjects) / maxNumberOfIterations
            if (k > 1) {
                conditionalPowerAchieved[k, index] <-
                    sum(subData$conditionalPowerAchieved[subData$stageNumber == k]) / iterations[k]
            }
        }

        index <- index + 1
    }
    overallReject <- colSums(rejectPerStage)
    futilityStop <- colSums(futilityPerStage)
    iterations <- table(data$stageNumber, data[[variantName]])

    if (kMax > 1) {
        if (numberOfVariants == 1) {
            earlyStop <- sum(futilityPerStage) + sum(rejectPerStage[1:(kMax - 1)])
        } else {
            if (kMax > 2) {
                rejectPerStageColSum <- colSums(rejectPerStage[1:(kMax - 1), ])
            } else {
                rejectPerStageColSum <- rejectPerStage[1, ]
            }
            earlyStop <- colSums(futilityPerStage) + rejectPerStageColSum
        }
    } else {
        earlyStop <- rep(0, numberOfVariants)
    }

    sampleSizes[is.na(sampleSizes)] <- 0

    return(list(
        sampleSizes = sampleSizes,
        rejectPerStage = rejectPerStage,
        overallReject = overallReject,
        futilityPerStage = futilityPerStage,
        futilityStop = futilityStop,
        iterations = iterations,
        earlyStop = earlyStop,
        expectedNumberOfSubjects = expectedNumberOfSubjects,
        conditionalPowerAchieved = conditionalPowerAchieved
    ))
}

.assertArgumentFitsWithSubGroups <- function(arg, argName, subGroups) {
    if (is.null(arg) || length(arg) == 0 || all(is.na(arg))) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'effectList' must contain ", sQuote(argName))
    }

    argName <- paste0("effectList$", argName)
    len <- ifelse(is.matrix(arg), ncol(arg), length(arg))
    if (len != length(subGroups)) {
        argName <- sQuote(argName)
        if (!is.matrix(arg)) {
            argName <- paste0(argName, " (", .arrayToString(arg), ")")
        }
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, argName, " must have ", length(subGroups),
            " columns given by the number of sub-groups"
        )
    }
}

C_EFFECT_LIST_NAMES_EXPECTED_MEANS <- c("subGroups", "prevalences", "effects", "stDevs")
C_EFFECT_LIST_NAMES_EXPECTED_RATES <- c("subGroups", "prevalences", "piControls", "piTreatments")
C_EFFECT_LIST_NAMES_EXPECTED_SURVIVAL <- c("subGroups", "prevalences", "piControls", "hazardRatios")

.getEffectData <- function(effectList, ..., 
        endpoint = NA_character_, 
        gMax = NA_integer_, 
        nullAllowed = TRUE,
        parameterNameWarningsEnabled = TRUE) {
        
    if (nullAllowed && is.null(effectList)) {
        return(NULL)
    }

    .assertIsSingleInteger(gMax, "gMax", naAllowed = TRUE, validateType = FALSE)

    if (is.null(effectList) || length(effectList) == 0 || !is.list(effectList)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote("effectList"), " must be a non-empty list")
    }

    effectListNames <- names(effectList)
    if (is.null(effectListNames) || any(nchar(trimws(effectListNames)) == 0)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote("effectList"), " must be named. Current names are ",
            .arrayToString(effectListNames, encapsulate = TRUE)
        )
    }

    for (singularName in c(
        "subGroup", "effect", "piTreatment", "piControl",
        "hazardRatio", "prevalence", "stDev"
    )) {
        names(effectList)[names(effectList) == singularName] <- paste0(singularName, "s")
    }
    effectListNames <- names(effectList)

    if (!("subGroups" %in% effectListNames)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote("effectList"), " must contain ", sQuote("subGroups"))
    }

    subGroups <- effectList[["subGroups"]]
    if (is.null(subGroups) || length(subGroups) == 0 || (!is.character(subGroups) && !is.factor(subGroups))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote("effectList$subGroups"),
            " must be a non-empty character vector or factor"
        )
    }
    if (is.factor(subGroups)) {
        subGroups <- as.character(subGroups)
    }

    expectedSubGroups <- "F"
    if (length(subGroups) > 1) {
        if (is.na(gMax)) {
            if (length(subGroups) > 2) {
                gMax <- max(as.integer(strsplit(gsub("\\D", "", paste0(subGroups, collapse = "")), "",
                    fixed = TRUE
                )[[1]]), na.rm = TRUE) + 1
            } else {
                gMax <- length(subGroups)
            }
        }
        if ("F" %in% subGroups) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "definition of full population 'F' ", 
                "together with sub-groups", ifelse(length(subGroups) == 2, "", "s"), " ",
                .arrayToString(subGroups[subGroups != "F"], encapsulate = TRUE, mode = "and"), 
                " makes no sense and is not allowed (use remaining population 'R' instead of 'F')"
            )
        }
        expectedSubGroups <- .createSubsetsByGMax(gMax, stratifiedInput = TRUE, all = FALSE) 
        if (gMax < 3) {
            expectedSubGroups <- gsub("\\d", "", expectedSubGroups)
        }
    }

    missingSubGroups <- expectedSubGroups[!(expectedSubGroups %in% subGroups)]
    if (length(missingSubGroups) > 0) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote("effectList$subGroups"),
            " must contain ", .arrayToString(dQuote(missingSubGroups))
        )
    }

    unknownSubGroups <- subGroups[!(subGroups %in% expectedSubGroups)]
    if (length(unknownSubGroups) > 0) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote("effectList$subGroups"),
            " must not contain ", .arrayToString(dQuote(unknownSubGroups)),
            " (valid sub-group names: ", .arrayToString(dQuote(expectedSubGroups)), ")"
        )
    }

    matrixName <- NA_character_
    matrixNames <- c("effects", "piTreatments", "hazardRatios")
    
    if (!is.na(endpoint)) {
        if (endpoint == "means") {
            matrixNames <- "effects"
        }
        else if (endpoint == "rates") {
            matrixNames <- "piTreatments"
        }
        else if (endpoint == "survival") {
            matrixNames <- "hazardRatios"
        }
    }
    
    for (m in matrixNames) {
        if (m %in% effectListNames) {
            matrixName <- m
            break
        }
    }
    
    if (is.na(matrixName)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote("effectList"), " must contain ",
            .arrayToString(matrixNames, mode = "or", encapsulate = TRUE)
        )
    }

    matrixValues <- effectList[[matrixName]]
    if (is.vector(matrixValues)) {
        matrixValues <- matrix(matrixValues, nrow = 1)
    }

    if (is.matrix(matrixValues)) {
        .assertIsValidMatrix(matrixValues, paste0("effectList$", matrixName), naAllowed = TRUE)
    }

    if (!is.matrix(matrixValues) && !is.data.frame(matrixValues)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote(paste0("effectList$", matrixName)),
            " must be a matrix or data.frame"
        )
    }

    if (!is.data.frame(matrixValues)) {
        matrixValues <- as.data.frame(matrixValues)
    }

    if (nrow(matrixValues) == 0) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote(paste0("effectList$", matrixName)),
            " must have one or more rows ",
            "reflecting the different situations to consider"
        )
    }
    .assertArgumentFitsWithSubGroups(matrixValues, matrixName, subGroups)

    colNames <- paste0(matrixName, 1:ncol(matrixValues))
    colnames(matrixValues) <- colNames
    matrixValues$situation <- 1:nrow(matrixValues)
    longData <- stats::reshape(data = matrixValues, direction = "long", varying = colNames, idvar = "situation", sep = "")
    timeColumnIndex <- which(colnames(longData) == "time")
    colnames(longData)[timeColumnIndex] <- "subGroupNumber"
    longData$subGroups <- rep(NA_character_, nrow(longData))
    indices <- sort(unique(longData$subGroupNumber))
    for (i in indices) {
        longData$subGroups[longData$subGroupNumber == i] <- subGroups[i]
    }

    longData$prevalences <- rep(NA_real_, nrow(longData))
    prevalences <- effectList[["prevalences"]]
    if (is.null(prevalences)) {
        stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, sQuote("effectList$prevalences"), " must be specified")
    }
    .assertIsNumericVector(prevalences, "effectList$prevalences")
    .assertArgumentFitsWithSubGroups(prevalences, "prevalences", subGroups)
    if (abs(sum(prevalences) - 1) > 1e-04) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote("effectList$prevalences"), " must sum to 1")
    }
    for (i in indices) {
        longData$prevalences[longData$subGroupNumber == i] <- prevalences[i]
    }

    # means only
    if (matrixName == "effects") {
        longData$stDevs <- rep(NA_real_, nrow(longData))
        stDevs <- effectList[["stDevs"]]
        if (is.null(stDevs)) {
            stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, sQuote("effectList$stDevs"), " must be specified")
        }
        .assertIsNumericVector(stDevs, "effectList$stDevs")
        if (!is.null(stDevs) && length(stDevs) == 1) {
            stDevs <- rep(stDevs, length(prevalences))
        }
        .assertArgumentFitsWithSubGroups(stDevs, "stDevs", subGroups)
        for (i in indices) {
            longData$stDevs[longData$subGroupNumber == i] <- stDevs[i]
        }
    }

    # rates and survival only
    else if (matrixName == "piTreatments" ||
            matrixName == "hazardRatios" && "piControls" %in% effectListNames) {
        longData$piControls <- rep(NA_real_, nrow(longData))
        piControls <- effectList[["piControls"]]
        if (is.null(piControls)) {
            stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, sQuote("effectList$piControls"), " must be specified")
        }
        .assertIsNumericVector(piControls, "effectList$piControls")
        .assertArgumentFitsWithSubGroups(piControls, "piControls", subGroups)
        for (i in indices) {
            longData$piControls[longData$subGroupNumber == i] <- piControls[i]
        }
    }

    rownames(longData) <- NULL

    # order by subGroup
    longData$subGroupNumber <- as.integer(gsub("\\D", "", gsub("^S$", "S1", longData$subGroups)))
    longData$subGroupNumber[is.na(longData$subGroupNumber)] <- 99999

    longData <- longData[order(longData$subGroupNumber, longData$situation), ]

    longData <- .moveColumn(longData, matrixName, colnames(longData)[length(colnames(longData))])

    for (singularName in c(
        "subGroup", "effect", "piTreatment", "piControl",
        "hazardRatio", "prevalence", "stDev"
    )) {
        colnames(longData)[colnames(longData) == paste0(singularName, "s")] <- singularName
    }

    longData <- longData[, colnames(longData) != "subGroupNumber"]
    
    if (parameterNameWarningsEnabled && !is.na(endpoint)) {
        if (endpoint == "means") {
            ignore <- effectListNames[!(effectListNames %in% C_EFFECT_LIST_NAMES_EXPECTED_MEANS)]
        }
        else if (endpoint == "rates") {
            ignore <- effectListNames[!(effectListNames %in% C_EFFECT_LIST_NAMES_EXPECTED_RATES)]
        }
        else if (endpoint == "survival") {
            ignore <- effectListNames[!(effectListNames %in% C_EFFECT_LIST_NAMES_EXPECTED_SURVIVAL)]
        }
        if (length(ignore) > 0) {
            warning("The parameter", ifelse(length(ignore) == 1, "", "s"), " ", .arrayToString(ignore, encapsulate = TRUE),
                " will be ignored", call. = FALSE)
        }
    }

    return(longData)
}

.getSimulationEnrichmentEffectMatrixName <- function(obj) {
    if (!grepl("SimulationResultsEnrichment", .getClassName(obj))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote("obj"),
            " must be a SimulationResultsEnrichment object (is ", .getClassName(obj), ")"
        )
    }

    if (grepl("Means", .getClassName(obj))) {
        return("effects")
    }

    if (grepl("Rates", .getClassName(obj))) {
        return("piTreatments")
    }

    if (grepl("Survival", .getClassName(obj))) {
        return("hazardRatios")
    }

    stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "class ", .getClassName(obj), " not supported")
}

.getSimulationEnrichmentEffectData <- function(simulationResults, validatePlotCapability = TRUE) {
    effectMatrixName <- .getSimulationEnrichmentEffectMatrixName(simulationResults)
    effectData <- simulationResults$effectList[[effectMatrixName]]
    discreteXAxis <- FALSE
    if (ncol(effectData) == 1) {
        xValues <- effectData[, 1]
    } else {
        xValues <- 1:nrow(effectData)
        discreteXAxis <- TRUE
    }
    valid <- TRUE
    if (length(xValues) <= 1) {
        if (validatePlotCapability) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "2 ore more situations must be specifed in ",
                sQuote(paste0("effectList$", effectMatrixName))
            )
        }
        valid <- FALSE
    }

    return(list(
        effectMatrixName = effectMatrixName,
        effectData = effectData,
        xValues = xValues,
        discreteXAxis = discreteXAxis,
        valid = valid
    ))
}

.getEffectList <- function(effectData, ..., 
        parameterName = "effectData", 
        endpoint = NA_character_,
        parameterNameWarningsEnabled = TRUE) {
        
    if (is.null(effectData) || length(effectData) == 0 || !is.data.frame(effectData)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote(parameterName), " must be a non-empty data.frame")
    }

    effectList <- list(subGroups = character(0), prevalences = numeric(0))
    matrixName <- NA_character_
    matrixNames <- c("effect", "piTreatment", "hazardRatio")
    names(matrixNames) <- c("means", "rates", "survival")
    expectedMatrixName <- ifelse(is.na(endpoint), NA_character_, matrixNames[[endpoint]])
    effectDataNames <- colnames(effectData)
    for (m in matrixNames) {
        if (m %in% effectDataNames && (is.na(endpoint) || identical(m, expectedMatrixName))) {
            matrixName <- m
            break
        }
    }
    if (is.na(matrixName)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote(parameterName), " must contain ",
            ifelse(!is.na(expectedMatrixName), 
                sQuote(expectedMatrixName), 
                .arrayToString(matrixNames, mode = "or", encapsulate = TRUE))
        )
    }

    matrixNameNew <- paste0(matrixName, "s")
    effectList[[matrixNameNew]] <- NULL
    if (matrixName == "effect") {
        effectList$stDevs <- numeric(0)
    } else if (matrixName %in% c("piTreatment", "hazardRatio")) {
        effectList$piControls <- numeric(0)
    }
    for (subGroup in unique(effectData$subGroup)) {
        effectList$subGroups <- c(effectList$subGroups, subGroup)
        subData <- effectData[effectData$subGroup == subGroup, ]
        effectList$prevalences <- c(effectList$prevalences, subData$prevalence[1])
        if (matrixName == "effect") {
            if (!("stDev" %in% effectDataNames)) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote(parameterName), " must contain ", sQuote("stDev"))
            }
            effectList$stDevs <- c(effectList$stDevs, subData$stDev[1])
        } else if (matrixName == "piTreatment" || (matrixName == "hazardRatio" && "piControl" %in% effectDataNames)) {
            if (!("piControl" %in% effectDataNames)) {
                stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, sQuote(parameterName), " must contain ", sQuote("piControl"))
            }
            effectList$piControls <- c(effectList$piControls, subData$piControl[1])
        }
        if (is.null(effectList[[matrixNameNew]])) {
            effectList[[matrixNameNew]] <- subData[[matrixName]]
        } else {
            effectList[[matrixNameNew]] <- cbind(effectList[[matrixNameNew]], subData[[matrixName]])
        }
    }
    if (!is.matrix(effectList[[matrixNameNew]])) {
        effectList[[matrixNameNew]] <- matrix(effectList[[matrixNameNew]], ncol = 1)
    }
    
    if (parameterNameWarningsEnabled && !is.na(endpoint)) {
        if (endpoint == "means") {
            ignore <- effectDataNames[!(effectDataNames %in% gsub("s$", "", C_EFFECT_LIST_NAMES_EXPECTED_MEANS))]
        }
        else if (endpoint == "rates") {
            ignore <- effectDataNames[!(effectDataNames %in% gsub("s$", "", C_EFFECT_LIST_NAMES_EXPECTED_RATES))]
        }
        else if (endpoint == "survival") {
            ignore <- effectDataNames[!(effectDataNames %in% gsub("s$", "", C_EFFECT_LIST_NAMES_EXPECTED_SURVIVAL))]
        }
        if (length(ignore) > 0) {
            warning("The parameter", ifelse(length(ignore) == 1, "", "s"), " ", .arrayToString(ignore, encapsulate = TRUE),
                " will be ignored", call. = FALSE)
        }
    }
    
    if (!is.null(effectList[["prevalences"]])) {
        .assertIsInClosedInterval(effectList$prevalences, "effectList$prevalences", 
            lower = 0, upper = 1, call. = FALSE)
    }
    if (!is.null(effectList[["effects"]])) {
        .assertIsNumericVector(effectList$effects, "effectList$effects", call. = FALSE)
    }
    for (piParam in c("piControls", "piTreatments")) {
        if (!is.null(effectList[[piParam]])) {
            if (piParam == matrixNameNew && is.matrix(effectList[[piParam]])) {
                for (i in 1:nrow(effectList[[piParam]])) {
                    .assertIsInOpenInterval(effectList[[piParam]][i, ], paste0("effectList$", piParam), 
                        lower = 0, upper = 1, call. = FALSE)
                }
            } else {
                .assertIsInOpenInterval(effectList[[piParam]], paste0("effectList$", piParam), 
                    lower = 0, upper = 1, call. = FALSE)
            }
        }
    }
    for (ratioParam in c("hazardRatios", "stDevs")) {
        if (!is.null(effectList[[ratioParam]])) {
            .assertIsInOpenInterval(effectList[[ratioParam]], paste0("effectList$", ratioParam), 
                lower = 0, upper = NULL, call. = FALSE)
        }
    }
    
    return(effectList)
}

.getValidatedEffectList <- function(effectList, ..., endpoint, gMax = NA_integer_, nullAllowed = TRUE) {
    if (is.null(endpoint) || !(endpoint %in% c("means", "rates", "survival"))) {
        stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'endpoint' (", endpoint, ") must be one of 'means', 'rates', or 'survival'")
    }
    
    if (is.null(effectList) || length(effectList) == 0 || (!is.list(effectList) && !is.data.frame(effectList))) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'effectList' must be a valid list or data.frame")
    }
    
    if (is.data.frame(effectList)) {
        return(.getEffectList(effectList, parameterName = "effectList", endpoint = endpoint))
    }

    effectData <- .getEffectData(effectList, endpoint = endpoint, gMax = gMax, nullAllowed = nullAllowed)
    return(.getEffectList(effectData))
}
