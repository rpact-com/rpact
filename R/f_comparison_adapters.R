## |
## |  rpact: Confirmatory Adaptive Clinical Trial Design and Analysis
## |  This file is part of the rpact R package.
## |
## |  RPACT — Statistical Tools for Drug Development
## |
## |  Authors: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  SPDX-FileCopyrightText: RPACT GmbH, Germany <info@rpact.com>
## |  SPDX-License-Identifier: LGPL-3.0-only
## |
## |  Licensed under the GNU Lesser General Public License, version 3.
## |  License text: https://www.gnu.org/licenses/lgpl-3.0.html
## |
## |  Package documentation: https://www.rpact.org
## |  Resources and customer portal: https://connect.rpact.app
## |  Consulting and services: https://www.rpact.com
## |  Contact: info@rpact.com
## |


.getComparisonAdapter <- function(object) {
    classes <- c("EventProbabilities", "NumberOfSubjects",
        "TrialDesignOptimalConditionalErrorCharacteristics", "ConditionalPowerResults",
        "PerformanceScore", "FisherInformation")
    for (className in classes) {
        if (inherits(object, className)) {
            return(className)
        }
    }
    return(NULL)
}

.getComparisonDesign <- function(object) {
    if (inherits(object, "PerformanceScore")) {
        return(object$.simulationResults$.design)
    }
    if (inherits(object, "FisherInformation")) {
        plan <- object$.getDesignPlan()
        if (!is.null(plan)) {
            return(plan$.design)
        }
        return(NULL)
    }
    if (inherits(object, "TrialDesignOptimalConditionalErrorCharacteristics")) {
        return(object$.design)
    }
    return(.getTrialDesign(object))
}

.getComparisonMetadataSources <- function(object) {
    sources <- list(object)
    design <- .getComparisonDesign(object)
    if (!is.null(design)) {
        sources <- c(sources, list(design))
    }
    if (inherits(object, "PerformanceScore")) {
        sources <- c(sources, list(object$.simulationResults))
    }
    if (inherits(object, "FisherInformation") && !is.null(object$.getDesignPlan())) {
        sources <- c(sources, list(object$.getDesignPlan()))
    }
    return(sources)
}

.comparisonContextValue <- function(value) {
    if (length(value) == 1L && is.atomic(value) && is.null(dim(value))) {
        return(value)
    }
    return(paste(capture.output(dput(value)), collapse = " "))
}

.addComparisonContext <- function(data, source, prefix = "", parameters = NULL) {
    if (is.null(source)) {
        return(data)
    }
    if (is.null(parameters)) {
        parameters <- c(source$.getUserDefinedParameters(), source$.getDefaultParameters())
    }
    for (parameter in intersect(parameters, names(source))) {
        if (source$isNotApplicableParameter(parameter)) {
            next
        }
        value <- source[[parameter]]
        name <- paste0(prefix, parameter)
        if (!name %in% names(data) && length(value) && is.atomic(value)) {
            data[[name]] <- rep(.comparisonContextValue(value), nrow(data))
        }
    }
    return(data)
}

.getComparisonAdapterContext <- function(object) {
    adapter <- .getComparisonAdapter(object)
    if (is.null(adapter)) {
        return(NULL)
    }
    target <- switch(adapter,
        EventProbabilities = "Event probability",
        NumberOfSubjects = "Recruitment",
        TrialDesignOptimalConditionalErrorCharacteristics = "Adaptive design characteristics",
        ConditionalPowerResults = "Conditional power",
        PerformanceScore = "Performance score",
        FisherInformation = "Fisher information"
    )
    endpoint <- if (adapter == "EventProbabilities") {
        "Survival"
    } else {
        "Undefined"
    }
    contextObject <- if (adapter == "PerformanceScore") {
        object$.simulationResults
    } else if (adapter == "FisherInformation") {
        object$.getDesignPlan()
    } else {
        object
    }
    if (!is.null(contextObject)) {
        for (endpointName in c("Means", "Rates", "Survival", "Count")) {
            if (grepl(endpointName, class(contextObject)[1L], fixed = TRUE)) {
                endpoint <- if (endpointName == "Count") {
                    "Counts"
                } else {
                    endpointName
                }
            }
        }
    }
    return(list(target = target, endpoint = endpoint))
}

.getComparisonAdapterData <- function(object, includeAllParameters = FALSE) {
    adapter <- .getComparisonAdapter(object)
    if (is.null(adapter)) {
        return(NULL)
    }
    if (adapter == "TrialDesignOptimalConditionalErrorCharacteristics") {
        n <- length(object$theta)
        data <- data.frame(stages = rep(1:2, each = n), theta = rep(object$theta, 2L),
            overallReject = rep(object$overallReject, 2L),
            rejectPerStage = as.vector(t(object$rejectPerStage)),
            futilityPerStage = c(as.vector(object$futilityPerStage), rep(NA_real_, n)))
        data <- .addComparisonContext(data, object$.design, prefix = "optimal_",
            parameters = object$.design$.getVisibleFieldNames())
        data$alpha <- rep(object$.design$alpha, nrow(data))
        data$kMax <- 2L
        return(data)
    }
    if (adapter == "FisherInformation") {
        data <- as.data.frame(object)
        names(data) <- c("stage", "situations", "information")
        data$type <- rep(object$type, nrow(data))
        plan <- object$.getDesignPlan()
        if (!is.null(plan)) {
            matrix <- as.matrix(object)
            for (parameter in intersect(c("alternative", "pi1", "lambda1", "hazardRatio", "theta"), names(plan))) {
                value <- plan[[parameter]]
                if (length(value) == ncol(matrix)) {
                    data[[parameter]] <- rep(value, each = nrow(matrix))
                }
            }
        }
        return(.addComparisonContext(data, plan))
    }
    if (adapter == "ConditionalPowerResults") {
        return(.getComparisonConditionalPowerData(object))
    }
    data <- as.data.frame(object, niceColumnNamesEnabled = FALSE,
        includeAllParameters = includeAllParameters)
    if (adapter == "PerformanceScore") {
        simulation <- object$.simulationResults
        alternative <- if (inherits(simulation, "SimulationResultsMeans")) {
            "alternative"
        } else {
            "pi1"
        }
        data[[alternative]] <- object$.alternative
        parameters <- setdiff(c(simulation$.getUserDefinedParameters(), simulation$.getDefaultParameters()), alternative)
        data <- .addComparisonContext(data, simulation, prefix = "simulation_", parameters = parameters)
        return(.addComparisonContext(data, simulation$.design, prefix = "design_",
            parameters = simulation$.design$.getVisibleFieldNames()))
    }
    if (adapter %in% c("EventProbabilities", "NumberOfSubjects")) {
        data <- .addComparisonContext(data, object, parameters = object$.getVisibleFieldNames())
    }
    return(data)
}

.getComparisonConditionalPowerData <- function(object) {
    kMax <- object$.design$kMax
    multi <- inherits(object, "ConditionalPowerResultsMultiHypotheses")
    gMax <- if (multi) {
        nrow(object$conditionalPower)
    } else {
        1L
    }
    data <- data.frame(stages = rep(seq_len(kMax), each = gMax))
    if (multi) {
        if (grepl("Enrichment", class(object)[1L], fixed = TRUE)) {
            populations <- c(paste0("S", seq_len(gMax - 1L)), "F")
            data$populations <- rep(populations, times = kMax)
        } else {
            data$treatmentArm <- rep(seq_len(gMax), times = kMax)
        }
    }
    for (parameter in object$.getVisibleFieldNames()) {
        value <- object[[parameter]]
        if (!is.atomic(value) || !length(value)) {
            next
        }
        if (is.matrix(value) && identical(dim(value), c(gMax, kMax))) {
            data[[parameter]] <- as.vector(value)
        } else if (length(value) == 1L) {
            data[[parameter]] <- rep(value, nrow(data))
        } else if (parameter %in% c("nPlanned", "allocationRatioPlanned") && length(value) == kMax) {
            data[[parameter]] <- rep(value, each = gMax)
        } else if (multi && length(value) == gMax && is.null(dim(value))) {
            data[[parameter]] <- rep(value, times = kMax)
        } else if (!multi && length(value) == kMax && is.null(dim(value))) {
            data[[parameter]] <- value
        } else {
            data[[parameter]] <- rep(.comparisonContextValue(value), nrow(data))
        }
    }
    stageResults <- object$.stageResults
    if (!is.null(stageResults)) {
        data$observedStage <- rep(stageResults$stage, nrow(data))
        data <- .addComparisonContext(data, stageResults, prefix = "observed_",
            parameters = stageResults$.getVisibleFieldNames())
        if (!is.null(stageResults$.dataInput)) {
            inputs <- as.data.frame(stageResults$.dataInput)
            for (parameter in names(inputs)) {
                data[[paste0("observedData_", parameter)]] <- rep(
                    .comparisonContextValue(inputs[[parameter]]), nrow(data))
            }
        }
    }
    return(data)
}

.getComparisonCoordinateNames <- function(data) {
    coordinates <- intersect(c("stages", "stage", "time", "treatmentArm", "populations", "situations"), names(data))
    coordinates <- coordinates[vapply(data[coordinates], function(values) {
        return(any(!is.na(values)))
    }, logical(1))]
    return(coordinates)
}


.comparisonValueRCode <- function(value) {
    return(paste(capture.output(dput(value)), collapse = "\n"))
}

.getComparisonSpecialRCode <- function(object, leadingArguments, includeDefaultParameters,
        prefix, postfix, stringWrapPrefix, newArgumentValues, tolerance,
        pipeOperator, pipeOperatorPostfix, output, explicitPrint) {
    if (length(leadingArguments) || !is.list(newArgumentValues) || length(newArgumentValues)) {
        stopIllegalArgument("Comparison adapter reconstruction does not support argument overrides",
            functionName = "getObjectRCode", parameter = c("leadingArguments", "newArgumentValues"),
            value = list(leadingArguments = leadingArguments, newArgumentValues = newArgumentValues),
            constraint = "leadingArguments and newArgumentValues must be empty",
            diagnosticId = "comparison.adapter_r_code_overrides_unsupported",
            context = list(objectClass = class(object)[1L]))
    }
    if (inherits(object, "FisherInformation")) {
        plan <- object$.getDesignPlan()
        if (is.null(plan)) {
            # Standalone information objects have no source plan to recalculate.
            code <- paste0("getFromNamespace(\"FisherInformation\", \"rpact\")$new(",
                "information = ", .comparisonValueRCode(object$information),
                ", type = ", .comparisonValueRCode(object$type),
                ", stage = ", .comparisonValueRCode(object$stage),
                ", situations = ", .comparisonValueRCode(object$situations), ")")
        } else {
            planCode <- .getComparisonObjectRCode(plan, includeDefaultParameters, tolerance, pipeOperator)
            code <- paste0("getFisherInformation(", paste(planCode, collapse = "\n"),
                ", stage = ", .comparisonValueRCode(object$stage),
                ", type = ", .comparisonValueRCode(object$type), ")")
        }
    } else {
        design <- object$.design
        constructor <- getFromNamespace("getDesignOptimalConditionalError", "rpact")
        parameters <- design$.getUserDefinedParameters()
        if (includeDefaultParameters) {
            parameters <- c(parameters, design$.getDefaultParameters())
        }
        constructorParameters <- union(names(formals(constructor)),
            names(formals(TrialDesignOptimalConditionalError$public_methods$initialize)))
        parameters <- intersect(constructorParameters, parameters)
        arguments <- vapply(parameters, function(parameter) {
            return(paste0(parameter, " = ", .comparisonValueRCode(design[[parameter]])))
        }, character(1))
        code <- paste0("getFromNamespace(\".getOptimalDesignCharacteristics\", \"rpact\")(",
            "design = getFromNamespace(\"getDesignOptimalConditionalError\", \"rpact\")(",
            paste(arguments, collapse = ",\n"), "), theta = ", .comparisonValueRCode(object$theta), ")")
    }
    return(.formatRCode(paste0(prefix, code), precondition = character(),
        stringWrapParagraphWidth = NULL, postfix = postfix, stringWrapPrefix = stringWrapPrefix,
        pipeOperator = pipeOperator, pipeOperatorPostfix = pipeOperatorPostfix,
        output = output, explicitPrint = explicitPrint))
}
