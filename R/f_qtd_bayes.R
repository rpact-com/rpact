## Bayesian QTD calculations; arm order at the public API is control, treatment.

.assertQtdRBest <- function(functionName) {
    if (!.isPackageNamespaceLoaded("RBesT", quietly = TRUE)) {
        stopRuntimeIssue("Install package 'RBesT' to calculate Bayesian operating characteristics",
            functionName = functionName, constraint = "RBesT must be installed and loadable",
            diagnosticId = "qtd.bayes.rbest_unavailable", context = list(package = "RBesT"))
    }
}

.assertQtdSampleSizes <- function(numberOfSubjects, functionName) {
    if (!is.matrix(numberOfSubjects) || ncol(numberOfSubjects) != 2L || nrow(numberOfSubjects) == 0L) {
        stopIllegalArgument("'numberOfSubjects' must have two columns (control, treatment) and at least one row",
            functionName = functionName, parameter = "numberOfSubjects", value = numberOfSubjects,
            constraint = "two-column matrix with at least one row",
            diagnosticId = "qtd.bayes.invalid_sample_sizes")
    }
    .assertQtdPlotNumeric(numberOfSubjects, "numberOfSubjects", functionName, lower = 1, integer = TRUE)
}

.assertQtdPrior <- function(prior, parameter, priorClass, functionName) {
    link <- attr(prior, "link")
    invalidLink <- !is.null(link) && (!is.list(link) || !identical(link$name, "identity"))
    if (invalidLink ||
            !inherits(prior, priorClass) || !is.matrix(prior) ||
            !is.numeric(prior) || anyNA(prior) || any(!is.finite(prior)) ||
            ncol(prior) < 1L || nrow(prior) != 3L ||
            any(prior[1L, ] < 0) || abs(sum(prior[1L, ]) - 1) > 1e-8 ||
            any(prior[3L, ] <= 0) || (priorClass == "betaMix" && any(prior[2L, ] <= 0))) {
        stopIllegalArgument("'", parameter, "' must be a valid RBesT ", priorClass, " prior",
            functionName = functionName, parameter = parameter, value = prior,
            constraint = "finite mixture with normalized nonnegative weights and positive scale or shape parameters",
            diagnosticId = "qtd.bayes.invalid_prior", context = list(expectedClass = priorClass))
    }
}

.getQtdBayesDecision <- function(criteria, twoSamples, directionUpper = TRUE) {
    quantiles <- criteria$quantiles
    lowerTail <- criteria$lowerTail
    if (!directionUpper) {
        quantiles <- -quantiles
        lowerTail <- !lowerTail
    }
    arguments <- list(pc = criteria$probabilities, qc = quantiles, lower.tail = lowerTail)
    if (twoSamples) {
        return(do.call(RBesT::decision2S, arguments))
    }
    return(do.call(RBesT::decision1S, arguments))
}

.checkQtdBayesProbabilities <- function(probabilities, functionName, scenario) {
    tolerance <- 1e-6
    if (any(!is.finite(probabilities)) || any(probabilities < -tolerance) ||
            any(probabilities > 1 + tolerance) || any(colSums(probabilities) > 1 + tolerance)) {
        stopRuntimeIssue("Bayesian category probabilities are invalid; inspect criteria and numerical integration",
            functionName = functionName, constraint = "finite, exclusive probabilities with total at most one",
            diagnosticId = "qtd.bayes.invalid_probabilities",
            context = list(numberOfSubjects = scenario, probabilities = probabilities, tolerance = tolerance))
    }
    probabilities[] <- pmin(1, pmax(0, probabilities))
    # Only remove integration roundoff, never a material category overlap.
    totals <- colSums(probabilities)
    if (any(totals > 1)) {
        probabilities[, totals > 1] <- sweep(probabilities[, totals > 1, drop = FALSE],
            2L, totals[totals > 1], "/")
    }
    return(rbind(probabilities[1L, ], probabilities[3L, ],
        pmax(0, 1 - colSums(probabilities)), probabilities[2L, ]))
}

.calcQtdBayes <- function(design, numberOfSubjects, alternatives, endpoint,
        priorDist = NULL, stDev = 1, controlMean = NULL,
        priorDist1 = NULL, priorDist2 = NULL, pi2 = NULL, directionUpper = TRUE) {
    functionName <- paste0("getOperatingCharacteristics", endpoint)
    .assertQtdRBest(functionName)
    .validateQtdBayesRule(design, functionName)
    rules <- c("go", "stop", "indeterminate1")
    categories <- c("Go", "Indeterminate 1", "Indeterminate 2", "Stop")
    twoSamples <- endpoint == "Rates" || is.list(priorDist)
    decisions <- lapply(rules, function(rule) {
        return(.getQtdBayesDecision(design[[rule]], twoSamples, directionUpper))
    })
    result <- vector("list", nrow(numberOfSubjects))
    for (scenario in seq_len(nrow(numberOfSubjects))) {
        sizes <- unname(numberOfSubjects[scenario, ])
        probabilities <- tryCatch({
            if (endpoint == "Rates") {
                t(.calcQtdBayesBinary(design, sizes, alternatives, pi2,
                    directionUpper, priorDist1, priorDist2))
            } else if ((!twoSamples && ncol(priorDist) == 1L) ||
                    (twoSamples && all(vapply(priorDist, ncol, integer(1)) == 1L))) {
                .calcQtdConjugateNormal(design, sizes, alternatives, stDev, priorDist, controlMean)
            } else {
                vapply(decisions, function(decision) {
                    sigma <- rep(stDev, length.out = 2L)
                    if (!twoSamples) {
                        oc <- RBesT::oc1S(prior = priorDist, n = 1, decision = decision,
                            sigma = sqrt(sum(sigma^2 / sizes)))
                        return(as.numeric(oc(alternatives)))
                    }
                    oc <- RBesT::oc2S(prior1 = priorDist[[2L]], prior2 = priorDist[[1L]],
                        n1 = sizes[2L], n2 = sizes[1L], decision = decision,
                        sigma1 = sigma[2L], sigma2 = sigma[1L])
                    return(as.numeric(oc(controlMean + alternatives, controlMean)))
                }, numeric(length(alternatives)))
            }
        }, error = function(e) {
            stopRuntimeIssue("RBesT calculation failed: ", conditionMessage(e),
                functionName = functionName, constraint = "successful Bayesian OC calculation",
                diagnosticId = "qtd.bayes.calculation_failed",
                context = list(numberOfSubjects = sizes, endpoint = endpoint,
                    originalMessage = conditionMessage(e)))
        })
        probabilities <- matrix(probabilities, nrow = length(alternatives), ncol = 3L)
        probabilities <- .checkQtdBayesProbabilities(t(probabilities), functionName, sizes)
        data <- data.frame(category = rep(categories, each = length(alternatives)),
            probability = as.vector(t(probabilities)), nControl = sizes[1L], nTreatment = sizes[2L],
            alternative = rep(alternatives, 4L))
        if (endpoint == "Means") {
            data$stDev <- sqrt(mean(stDev^2))
            data$stDevControl <- stDev[1L]
            data$stDevTreatment <- rep(stDev, length.out = 2L)[2L]
            if (!is.null(controlMean)) {
                data$controlMean <- controlMean
            }
        } else {
            data$pi1 <- data$alternative
            data$pi2 <- pi2
            data$alternative <- (data$pi1 - pi2) * ifelse(directionUpper, 1, -1)
        }
        result[[scenario]] <- data
    }
    return(do.call(rbind, result))
}

.getQtdBayesMeans <- function(design, numberOfSubjects, alternative, stDev, controlMean, priorDist, groups) {
    functionName <- "getOperatingCharacteristicsMeans"
    .assertQtdRBest(functionName)
    .assertQtdSampleSizes(numberOfSubjects, functionName)
    .assertQtdPlotChoice(groups, "groups", 2L, functionName)
    .assertIsNumericVector(alternative, "alternative")
    .assertQtdPlotNumeric(alternative, "alternative", functionName)
    .assertQtdPlotNumeric(stDev, "stDev", functionName, lower = 0, openLower = TRUE)
    if (!(length(stDev) %in% c(1L, 2L))) {
        stopArgumentLengthOutOfBounds("'stDev' must have length one or two",
            functionName = functionName, parameter = "stDev", value = stDev,
            constraint = "common SD or control and treatment SDs", expectedLength = c(1L, 2L),
            actualLength = length(stDev), diagnosticId = "qtd.bayes.invalid_sd_length")
    }
    if (is.null(priorDist)) {
        priorDist <- RBesT::mixnorm(c(1, 0, 100))
    }
    if (is.list(priorDist)) {
        if (length(priorDist) != 2L) {
            stopArgumentLengthOutOfBounds("Supply control and treatment priors",
                functionName = functionName, parameter = "priorDist", value = priorDist,
                constraint = "two group priors", expectedLength = 2L, actualLength = length(priorDist),
                diagnosticId = "qtd.bayes.invalid_group_prior_length")
        }
        for (i in seq_len(2L)) {
            .assertQtdPrior(priorDist[[i]], "priorDist", "normMix", functionName)
        }
        .assertQtdPlotNumeric(controlMean, "controlMean", functionName, expectedLength = 1L)
    } else {
        .assertQtdPrior(priorDist, "priorDist", "normMix", functionName)
        if (!is.null(controlMean)) {
            stopConflictingArguments("'controlMean' is only used with separate group priors",
                functionName = functionName, parameter = "controlMean", value = controlMean,
                relatedParameter = "priorDist", relatedValue = priorDist,
                constraint = "controlMean requires group priors", diagnosticId = "qtd.bayes.control_mean_without_groups")
        }
    }
    probabilities <- .calcQtdBayes(design, numberOfSubjects, alternative, "Means",
        priorDist = priorDist, stDev = stDev, controlMean = controlMean)
    result <- OperatingCharcteristicsMeans$new(design = design, numberOfSubjects = numberOfSubjects,
        alternative = alternative, stDev = stDev, probabilities = probabilities)
    result$priorDist <- priorDist
    result$controlMean <- controlMean
    result$.setParameterType("priorDist", C_PARAM_USER_DEFINED)
    if (!is.null(controlMean)) {
        result$.setParameterType("controlMean", C_PARAM_USER_DEFINED)
    }
    return(result)
}

.getQtdBayesRates <- function(design, numberOfSubjects, pi1, pi2, directionUpper, priorDist1, priorDist2) {
    functionName <- "getOperatingCharacteristicsRates"
    .assertQtdRBest(functionName)
    .assertQtdSampleSizes(numberOfSubjects, functionName)
    .assertIsNumericVector(pi1, "pi1")
    .assertQtdPlotNumeric(pi1, "pi1", functionName, lower = 0, upper = 1)
    .assertIsNumericVector(pi2, "pi2")
    .assertQtdPlotNumeric(pi2, "pi2", functionName, lower = 0, upper = 1, expectedLength = 1L)
    .assertIsSingleLogical(directionUpper, "directionUpper")
    if (is.null(priorDist1)) {
        priorDist1 <- RBesT::mixbeta(c(1, 1 / 3, 1 / 3))
    }
    if (is.null(priorDist2)) {
        priorDist2 <- RBesT::mixbeta(c(1, 1 / 3, 1 / 3))
    }
    if (any((numberOfSubjects[, 1L] + 1) * (numberOfSubjects[, 2L] + 1) > 1e6)) {
        stopArgumentOutOfRange("The binary count grid may contain at most one million cells",
            functionName = functionName, parameter = "numberOfSubjects", value = numberOfSubjects,
            constraint = "at most 1e6 count pairs per scenario", upperBound = 1e6,
            diagnosticId = "qtd.bayes.count_grid_too_large")
    }
    .assertQtdPrior(priorDist1, "priorDist1", "betaMix", functionName)
    .assertQtdPrior(priorDist2, "priorDist2", "betaMix", functionName)
    probabilities <- .calcQtdBayes(design, numberOfSubjects, pi1, "Rates",
        priorDist1 = priorDist1, priorDist2 = priorDist2, pi2 = pi2, directionUpper = directionUpper)
    result <- OperatingCharcteristicsRates$new(design, numberOfSubjects, pi1, pi2,
        directionUpper = directionUpper, intervalMethod = NA_character_, probabilities = probabilities)
    result$priorDist1 <- priorDist1
    result$priorDist2 <- priorDist2
    result$.setParameterType("priorDist1", C_PARAM_USER_DEFINED)
    result$.setParameterType("priorDist2", C_PARAM_USER_DEFINED)
    return(result)
}

.getQtdBayesPlotData <- function(object, plotCat, stDev, pi2) {
    functionName <- "plot.OperatingCharcteristics"
    .validateQtdBayesRule(object$.design, functionName)
    settings <- .getQtdEndpointSettings(object)
    supplied <- list(stDev = stDev, pi2 = pi2)
    parameter <- settings$assumptionColumn
    other <- setdiff(names(supplied), parameter)
    if (!is.null(supplied[[other]])) {
        stopIllegalArgument("The supplied sensitivity parameter does not match the endpoint",
            functionName = functionName, parameter = other, value = supplied[[other]],
            constraint = paste0("use ", parameter), diagnosticId = "qtd.plot.endpoint_argument_not_applicable")
    }
    values <- supplied[[parameter]]
    if (is.null(values)) {
        values <- NA_real_
    } else {
        upper <- Inf
        if (parameter == "pi2") {
            upper <- 1
        }
        .assertQtdPlotNumeric(values, parameter, functionName, lower = 0, upper = upper,
            openLower = parameter == "stDev")
    }
    result <- vector("list", length(values))
    for (i in seq_along(values)) {
        data <- object$probabilities
        if (!is.na(values[i])) {
            if (parameter == "stDev") {
                data <- .calcQtdBayes(object$.design, object$numberOfSubjects, object$alternative, "Means",
                    priorDist = object$priorDist, stDev = values[i], controlMean = object$controlMean)
            } else {
                data <- .calcQtdBayes(object$.design, object$numberOfSubjects, object$pi1, "Rates",
                    priorDist1 = object$priorDist1, priorDist2 = object$priorDist2,
                    pi2 = values[i], directionUpper = object$directionUpper)
            }
        }
        data$scenario <- rep(seq_len(nrow(object$numberOfSubjects)), each = length(object$alternative) * 4L)
        data$effectIndex <- rep(rep(seq_along(object$alternative), 4L), nrow(object$numberOfSubjects))
        if (plotCat == 3L) {
            data$category <- .getQtdCoarseCategory(data$category)
            columns <- c("category", "scenario", "effectIndex", "nControl", "nTreatment", "alternative", parameter)
            if (settings$endpoint == "rates") {
                columns <- c(columns, "pi1")
            }
            data <- stats::aggregate(stats::reformulate(columns, response = "probability"), data, sum)
        }
        data$category <- factor(data$category, levels = .getQtdCategories(plotCat))
        data$sampleSize <- paste0("C: ", data$nControl, " / T: ", data$nTreatment)
        data$scenarioLabel <- paste0(data$scenario, ": ", data$sampleSize)
        data$totalSubjects <- data$nControl + data$nTreatment
        data$allocationRatio <- data$nTreatment / data$nControl
        result[[i]] <- data
    }
    return(do.call(rbind, result))
}

.getQtdRCodeExpression <- function(object, newArgumentValues = list()) {
    literal <- function(value) {
        if (inherits(value, "normMix") || inherits(value, "betaMix")) {
            constructor <- "mixnorm"
            if (inherits(value, "betaMix")) {
                constructor <- "mixbeta"
            }
            components <- vapply(seq_len(ncol(value)), function(i) {
                return(paste(deparse(as.numeric(value[, i])), collapse = ""))
            }, character(1))
            referenceScale <- attr(value, "sigma")
            if (!is.null(referenceScale) && constructor == "mixnorm") {
                components <- c(components, paste0("sigma = ", paste(deparse(referenceScale), collapse = "")))
            }
            return(paste0("RBesT::", constructor, "(", paste(components, collapse = ", "), ")"))
        }
        if (is.list(value) && length(value) == 2L && all(vapply(value, inherits, logical(1), "normMix"))) {
            return(paste0("list(", paste(vapply(value, literal, character(1)), collapse = ", "), ")"))
        }
        return(paste(capture.output(dput(value)), collapse = "\n"))
    }
    if (inherits(object, "QuantitativeDecisionThresholds")) {
        arguments <- list(quantiles = object$quantiles, probabilities = object$probabilities,
            lowerTail = object$lowerTail)
        constructor <- "getQuantitativeDecisionThresholds"
    } else if (inherits(object, "TrialDesignPlanQuantitativeBayes")) {
        arguments <- list(go = object$go, stop = object$stop, indeterminate1 = object$indeterminate1)
        constructor <- "getDesignQuantitative"
    } else if (inherits(object, "TrialDesignPlanQuantitative")) {
        arguments <- list(targetValue = object$targetValue, lowerReferenceValue = object$lowerReferenceValue,
            upperPercentile = object$upperPercentile, lowerPercentile = object$lowerPercentile)
        constructor <- "getDesignQuantitative"
    } else {
        arguments <- list(design = object$.design, numberOfSubjects = object$numberOfSubjects)
        if (inherits(object, "OperatingCharcteristicsMeans")) {
            constructor <- "getOperatingCharacteristicsMeans"
            arguments <- c(arguments, list(alternative = object$alternative, stDev = object$stDev))
            if (inherits(object$.design, "TrialDesignPlanQuantitativeBayes")) {
                arguments <- c(arguments, list(priorDist = object$priorDist, controlMean = object$controlMean))
            } else {
                arguments$normalApproximation <- object$normalApproximation
            }
        } else {
            constructor <- "getOperatingCharacteristicsRates"
            arguments <- c(arguments, list(pi1 = object$pi1, pi2 = object$pi2, directionUpper = object$directionUpper))
            if (inherits(object$.design, "TrialDesignPlanQuantitativeBayes")) {
                arguments <- c(arguments, list(priorDist1 = object$priorDist1, priorDist2 = object$priorDist2))
            } else {
                arguments$intervalMethod <- object$intervalMethod
            }
        }
    }
    # Reference-based Bayes rules retain their reference lines on reconstruction.
    if (inherits(object, "TrialDesignPlanQuantitativeBayes") && !is.na(object$targetValue)) {
        arguments <- list(targetValue = object$targetValue, lowerReferenceValue = object$lowerReferenceValue,
            upperPercentile = object$upperPercentile, lowerPercentile = object$lowerPercentile,
            bayesian = TRUE, percentileMapping = object$percentileMapping)
    }
    for (name in names(newArgumentValues)) {
        arguments[name] <- newArgumentValues[name]
    }
    values <- vapply(arguments, function(value) {
        if (inherits(value, "TrialDesignPlanQuantitativeBase") || inherits(value, "QuantitativeDecisionThresholds")) {
            return(.getQtdRCodeExpression(value))
        }
        return(literal(value))
    }, character(1))
    return(paste0("rpact::", constructor, "(\n", paste0("    ", names(values), " = ", values, collapse = ",\n"), "\n)"))
}

.calcQtdBayesBinary <- function(design, sizes, pi1, pi2, directionUpper, priorDist1, priorDist2) {
    grid <- .getQtdBayesBinaryGrid(design, sizes, directionUpper, priorDist1, priorDist2,
        functionName = "getOperatingCharacteristicsRates")
    return(vapply(pi1, function(rate) {
        mass <- stats::dbinom(grid$control, sizes[1L], pi2) * stats::dbinom(grid$treatment, sizes[2L], rate)
        return(vapply(c("Go", "Stop", "Indeterminate 1"), function(category) {
            return(sum(mass[grid$category == category]))
        }, numeric(1)))
    }, numeric(3L)))
}

.getQtdBayesBinaryGrid <- function(design, sizes, directionUpper, priorDist1, priorDist2,
        functionName = ".getQtdBayesBinaryGrid") {
    if (prod(sizes + 1) > 1e6) {
        stopArgumentOutOfRange("The binary count grid may contain at most one million cells",
            functionName = functionName, parameter = "numberOfSubjects", value = sizes,
            constraint = "(nControl + 1) * (nTreatment + 1) <= 1e6", upperBound = 1e6,
            diagnosticId = "qtd.bayes.count_grid_too_large")
    }
    controlPosteriors <- lapply(0:sizes[1L], function(events) {
        return(RBesT::postmix(priorDist2, n = sizes[1L], r = events))
    })
    treatmentPosteriors <- lapply(0:sizes[2L], function(events) {
        return(RBesT::postmix(priorDist1, n = sizes[2L], r = events))
    })
    decisions <- lapply(c("go", "stop", "indeterminate1"), function(rule) {
        return(.getQtdBayesDecision(design[[rule]], TRUE, directionUpper))
    })
    grid <- expand.grid(control = 0:sizes[1L], treatment = 0:sizes[2L])
    masks <- vapply(decisions, function(decision) {
        return(vapply(seq_len(nrow(grid)), function(i) {
            return(as.logical(decision(treatmentPosteriors[[grid$treatment[i] + 1L]],
                controlPosteriors[[grid$control[i] + 1L]])))
        }, logical(1)))
    }, logical(nrow(grid)))
    if (any(rowSums(masks) > 1L)) {
        stopRuntimeIssue("Posterior decisions overlap on the binary count grid",
            functionName = functionName, constraint = "at most one explicit category per outcome",
            diagnosticId = "qtd.bayes.posterior_decisions_overlap", context = list(numberOfSubjects = sizes))
    }
    category <- rep("Indeterminate 2", nrow(grid))
    for (i in seq_len(3L)) {
        category[masks[, i]] <- c("Go", "Stop", "Indeterminate 1")[i]
    }
    grid$category <- category
    return(grid)
}

.calcQtdConjugateNormal <- function(design, sizes, alternative, stDev, priorDist, controlMean) {
    samplingVariances <- rep(stDev, length.out = 2L)^2 / sizes
    if (is.list(priorDist)) {
        priorMeans <- vapply(priorDist, function(prior) {
            return(prior[2L, 1L])
        }, numeric(1))
        priorVariances <- vapply(priorDist, function(prior) {
            return(prior[3L, 1L]^2)
        }, numeric(1))
        posteriorVariances <- 1 / (1 / priorVariances + 1 / samplingVariances)
        weights <- posteriorVariances / samplingVariances
        intercept <- posteriorVariances * priorMeans / priorVariances
        posteriorMean <- intercept[2L] - intercept[1L] +
            weights[2L] * (controlMean + alternative) - weights[1L] * controlMean
        posteriorSD <- sqrt(sum(posteriorVariances))
        samplingSD <- sqrt(sum(weights^2 * samplingVariances))
    } else {
        samplingVariance <- sum(samplingVariances)
        posteriorVariance <- 1 / (1 / priorDist[3L, 1L]^2 + 1 / samplingVariance)
        weight <- posteriorVariance / samplingVariance
        posteriorMean <- posteriorVariance * priorDist[2L, 1L] / priorDist[3L, 1L]^2 + weight * alternative
        posteriorSD <- sqrt(posteriorVariance)
        samplingSD <- weight * sqrt(samplingVariance)
    }
    return(vapply(c("go", "stop", "indeterminate1"), function(name) {
        criteria <- design[[name]]
        boundaries <- criteria$quantiles + ifelse(criteria$lowerTail, -1, 1) *
            stats::qnorm(criteria$probabilities) * posteriorSD
        lower <- max(c(-Inf, boundaries[!criteria$lowerTail]))
        upper <- min(c(Inf, boundaries[criteria$lowerTail]))
        if (lower >= upper) {
            return(rep(0, length(alternative)))
        }
        return(pmax(0, stats::pnorm(upper, posteriorMean, samplingSD) -
            stats::pnorm(lower, posteriorMean, samplingSD)))
    }, numeric(length(alternative))))
}
