## |
## |  *Quantitative Trial Design (QTD)*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Author: Friedrich Pahlke, PhD and Daniel Sabanés Bové, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |
## |  RPACT company website: https://www.rpact.com
## |  rpact package website: https://www.rpact.org
## |
## |  Contact us for information about our services: info@rpact.com
## |

#'
#' @title
#' Create a Frequentist or Bayesian Quantitative Decision Rule
#'
#' @description
#' Constructs a fixed-design quantitative decision rule. Its class selects the
#' calculation in the common operating-characteristics functions.
#'
#' @param targetValue Single target effect on the beneficial-effect scale.
#' @param lowerReferenceValue Single lower reference effect; default zero.
#' @param upperPercentile Upper percentile in (0, 1); default 0.9.
#' @param lowerPercentile Lower percentile in (0, upperPercentile); default 0.2.
#' @param bayesian Whether to construct a Bayesian rule. Explicit criteria imply
#'   TRUE when this argument is omitted.
#' @param useSignificanceAndTarget Construct a dual significance/target rule.
#' @param alpha Two-sided frequentist significance level or Bayesian posterior
#'   tail probability in (0, 0.5); default 0.05. These are different conventions.
#' @param go,stop,indeterminate1 Criteria from [getQuantitativeDecisionThresholds()].
#'   Supply all three without reference values, percentiles or shortcut options.
#' @param percentileMapping Bayesian interpretation: "quantile" (default) or
#'   "posteriorProbability". Stored in the design for reproducibility.
#'
#' @details
#' For mapping "quantile", Go requires P(effect > LRV) > 1-lowerPercentile
#' and P(effect > TV) > 1-upperPercentile. Stop requires P(effect <= LRV) >
#' lowerPercentile and P(effect <= TV) > upperPercentile. This follows by comparing
#' posterior quantiles with the references, and agrees with normal CI decisions
#' in the diffuse-prior limit. It differs from the exploratory prototype mapping.
#'
#' Mapping "posteriorProbability" is a different rule: the beneficial-tail Go
#' thresholds are upperPercentile at LRV and lowerPercentile at TV, and Stop
#' uses their complements. For both mappings, Indeterminate 1 fails the LRV
#' criterion and passes TV. Indeterminate 2 is the complement of Go, Stop and
#' Indeterminate 1, including exact probability-threshold ties. Comparisons are
#' strict, following RBesT; ties can have positive sampling probability for rates.
#'
#' The significance/target shortcut uses lowerPercentile = alpha/2 and
#' upperPercentile = 0.5 for frequentist rules. Bayesian rules require posterior
#' probabilities 1-alpha at LRV and 0.5 at TV, independently of percentileMapping.
#' The shortcut stores the canonical quantile mapping for reproducibility.
#' The two shortcuts are not claimed to be equivalent: for example 95 percent
#' two-sided confidence corresponds to a 97.5 percent one-sided tail probability.
#'
#' All explicit categories must be provably pairwise incompatible by monotonicity
#' of a continuous posterior CDF. Potential overlaps are rejected; a particular
#' prior might permit additional disjoint rules that this conservative validation
#' cannot certify. No category precedence is introduced. When defining custom
#' intermediate criteria, explicitly exclude futility, as in the RBesT examples.
#' Reference effects always use the beneficial direction, also for decreasing rates.
#'
#' @return A TrialDesignPlanQuantitative or TrialDesignPlanQuantitativeBayes,
#'   inheriting from TrialDesignPlanQuantitativeBase and ParameterSet.
#'
#' @seealso [getOperatingCharacteristicsMeans()], [getOperatingCharacteristicsRates()],
#'   <https://opensource.nibr.com/RBesT/reference/decision2S.html>
#'
#' @examples
#' rule <- getDesignQuantitative(targetValue = 1)
#' bayes <- getDesignQuantitative(targetValue = 1, bayesian = TRUE)
#' dual <- getDesignQuantitative(targetValue = 1, bayesian = TRUE,
#'     useSignificanceAndTarget = TRUE)
#'
#' @export
#'
getDesignQuantitative <- function(targetValue, lowerReferenceValue = 0,
        upperPercentile = 0.9, lowerPercentile = 0.2, bayesian = FALSE,
        useSignificanceAndTarget = FALSE, alpha = 0.05,
        go = NULL, stop = NULL, indeterminate1 = NULL, percentileMapping = "quantile") {
    functionName <- "getDesignQuantitative"
    explicit <- !vapply(list(go, stop, indeterminate1), is.null, logical(1))
    if (any(explicit) && missing(bayesian)) {
        bayesian <- TRUE
    }
    .assertIsSingleLogical(bayesian, "bayesian")
    .assertIsSingleLogical(useSignificanceAndTarget, "useSignificanceAndTarget")
    .assertQtdPlotChoice(percentileMapping, "percentileMapping",
        c("quantile", "posteriorProbability"), functionName)
    .assertQtdPlotNumeric(alpha, "alpha", functionName, lower = 0, upper = 0.5,
        openLower = TRUE, openUpper = TRUE, expectedLength = 1L)
    if (any(explicit)) {
        if (!all(explicit) || !bayesian || !missing(targetValue) ||
                !missing(lowerReferenceValue) || !missing(upperPercentile) ||
                !missing(lowerPercentile) || useSignificanceAndTarget || !missing(alpha) ||
                !missing(percentileMapping)) {
            stopConflictingArguments("Supply all three Bayesian criteria without reference-value or percentile settings",
                functionName = functionName, parameter = c("go", "stop", "indeterminate1"),
                value = list(go, stop, indeterminate1), constraint = "complete explicit Bayesian rule only",
                relatedParameter = "bayesian", relatedValue = bayesian,
                diagnosticId = "qtd.bayes.conflicting_rule_specification")
        }
        .validateQtdBayesRule(list(go = go, stop = stop, indeterminate1 = indeterminate1), functionName)
        return(TrialDesignPlanQuantitativeBayes$new(go, stop, indeterminate1))
    }
    if (missing(targetValue)) {
        stopMissingArgument("Supply 'targetValue' or an explicit Bayesian rule",
            functionName = functionName, parameter = "targetValue",
            constraint = "required for reference-value rules", diagnosticId = "qtd.design.missing_target")
    }
    .assertQtdPlotNumeric(targetValue, "targetValue", functionName, expectedLength = 1L)
    .assertQtdPlotNumeric(lowerReferenceValue, "lowerReferenceValue", functionName, expectedLength = 1L)
    .assertQtdPlotNumeric(upperPercentile, "upperPercentile", functionName,
        lower = 0, upper = 1, openLower = TRUE, openUpper = TRUE, expectedLength = 1L)
    .assertQtdPlotNumeric(lowerPercentile, "lowerPercentile", functionName,
        lower = 0, upper = upperPercentile, openLower = TRUE, openUpper = TRUE, expectedLength = 1L)
    if (targetValue < lowerReferenceValue) {
        stopConflictingArguments("'targetValue' must be at least 'lowerReferenceValue'",
            functionName = functionName, parameter = "targetValue", value = targetValue,
            relatedParameter = "lowerReferenceValue", relatedValue = lowerReferenceValue,
            constraint = "targetValue >= lowerReferenceValue", diagnosticId = "qtd.design.unordered_references")
    }
    if (useSignificanceAndTarget && (!missing(lowerPercentile) || !missing(upperPercentile))) {
        stopConflictingArguments("Do not combine explicit percentiles with the significance/target shortcut",
            functionName = functionName, parameter = "useSignificanceAndTarget", value = useSignificanceAndTarget,
            relatedParameter = c("lowerPercentile", "upperPercentile"),
            relatedValue = c(lowerPercentile, upperPercentile), constraint = "one rule specification",
            diagnosticId = "qtd.design.conflicting_percentiles")
    }
    if (useSignificanceAndTarget) {
        lowerPercentile <- alpha / 2
        upperPercentile <- 0.5
        if (bayesian) {
            lowerPercentile <- alpha
            percentileMapping <- "quantile"
        }
    }
    if (!bayesian) {
        return(TrialDesignPlanQuantitative$new(targetValue = targetValue,
            lowerReferenceValue = lowerReferenceValue, upperPercentile = upperPercentile,
            lowerPercentile = lowerPercentile))
    }
    goProbabilities <- c(1 - lowerPercentile, 1 - upperPercentile)
    if (percentileMapping == "posteriorProbability" && !useSignificanceAndTarget) {
        goProbabilities <- c(upperPercentile, lowerPercentile)
    }
    quantiles <- c(lowerReferenceValue, targetValue)
    go <- getQuantitativeDecisionThresholds(quantiles, goProbabilities, FALSE)
    stop <- getQuantitativeDecisionThresholds(quantiles, 1 - goProbabilities, TRUE)
    indeterminate1 <- getQuantitativeDecisionThresholds(quantiles,
        c(1 - goProbabilities[1L], goProbabilities[2L]), c(TRUE, FALSE))
    result <- TrialDesignPlanQuantitativeBayes$new(go, stop, indeterminate1,
        targetValue = targetValue, lowerReferenceValue = lowerReferenceValue,
        upperPercentile = upperPercentile, lowerPercentile = lowerPercentile,
        percentileMapping = percentileMapping)
    .validateQtdBayesRule(result, functionName)
    return(result)
}

#'
#' @title
#' Specify Posterior Probability Criteria
#'
#' @description
#' Creates a conjunction of strict posterior probability criteria, as in RBesT.
#'
#' @param quantiles Finite effect thresholds on the beneficial-effect scale.
#' @param probabilities Probability thresholds in (0, 1), one per quantile.
#' @param lowerTail Logical scalar or vector. TRUE specifies
#'   P(effect <= quantile) > probability; FALSE specifies
#'   P(effect > quantile) > probability.
#'
#' @return A QuantitativeDecisionThresholds object.
#'
#' @seealso [getDesignQuantitative()]
#'
#' @export
#'
getQuantitativeDecisionThresholds <- function(quantiles, probabilities, lowerTail = FALSE) {
    functionName <- "getQuantitativeDecisionThresholds"
    if (missing(quantiles) || missing(probabilities)) {
        missingParameters <- c("quantiles", "probabilities")[c(missing(quantiles), missing(probabilities))]
        stopMissingArgument("Supply ", paste(missingParameters, collapse = ", "),
            functionName = functionName, parameter = missingParameters,
            constraint = "quantiles and probabilities are required", diagnosticId = "qtd.bayes.missing_thresholds")
    }
    .assertIsNumericVector(quantiles, "quantiles")
    .assertIsNumericVector(probabilities, "probabilities")
    .assertQtdPlotNumeric(quantiles, "quantiles", functionName)
    .assertQtdPlotNumeric(probabilities, "probabilities", functionName, lower = 0, upper = 1,
        expectedLength = length(quantiles), openLower = TRUE, openUpper = TRUE)
    if (!is.logical(lowerTail) || !is.null(dim(lowerTail)) || anyNA(lowerTail) ||
            !(length(lowerTail) %in% c(1L, length(quantiles)))) {
        stopIllegalArgument("'lowerTail' must be a logical scalar or one value per quantile",
            functionName = functionName, parameter = "lowerTail", value = lowerTail,
            constraint = "nonmissing logical scalar or vector matching quantiles",
            relatedParameter = "quantiles", relatedValue = quantiles,
            diagnosticId = "qtd.bayes.invalid_tail")
    }
    return(QuantitativeDecisionThresholds$new(quantiles, probabilities,
        rep(lowerTail, length.out = length(quantiles))))
}

.validateQtdBayesRule <- function(design, functionName) {
    rules <- lapply(c("go", "stop", "indeterminate1"), function(parameter) {
        value <- design[[parameter]]
        if (!inherits(value, "QuantitativeDecisionThresholds")) {
            stopIllegalArgument("Bayesian criteria must be created with getQuantitativeDecisionThresholds()",
                functionName = functionName, parameter = parameter, value = value,
                constraint = "QuantitativeDecisionThresholds", diagnosticId = "qtd.bayes.invalid_criteria")
        }
        return(getQuantitativeDecisionThresholds(value$quantiles, value$probabilities, value$lowerTail))
    })
    names(rules) <- c("go", "stop", "indeterminate1")
    # Convert to strict lower/upper bounds on a continuous posterior CDF.
    # Monotonicity proves exclusion for every prior, not only sampled outcomes.
    for (pair in list(c(1L, 2L), c(1L, 3L), c(2L, 3L))) {
        a <- rules[[pair[1L]]]
        b <- rules[[pair[2L]]]
        quantiles <- c(a$quantiles, b$quantiles)
        tails <- c(a$lowerTail, b$lowerTail)
        probabilities <- c(a$probabilities, b$probabilities)
        disjoint <- FALSE
        for (i in which(tails)) {
            for (j in which(!tails)) {
                if (quantiles[i] <= quantiles[j] && probabilities[i] >= 1 - probabilities[j]) {
                    disjoint <- TRUE
                }
            }
        }
        if (!disjoint) {
            stopConflictingArguments("Cannot prove that Bayesian categories are mutually exclusive: ",
                paste(names(rules)[pair], collapse = " and "),
                functionName = functionName, parameter = names(rules)[pair[1L]], value = a,
                relatedParameter = names(rules)[pair[2L]], relatedValue = b,
                constraint = "pairwise incompatible strict posterior CDF constraints",
                diagnosticId = "qtd.bayes.overlapping_categories",
                context = list(rule = "monotonicity of a continuous posterior CDF"))
        }
    }
    return(invisible(TRUE))
}
