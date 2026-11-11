## |
## |  *Quantitative Trial Design (QTD)*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Author: Friedrich Pahlke, PhD and Daniel Saban<U+00E9>s Bov<U+00E9>, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |
## |  RPACT company website: https://www.rpact.com
## |  rpact package website: https://www.rpact.org
## |
## |  Contact us for information about our services: info@rpact.com
## |

# This is basically the equivalent of the OC.ct.GO() function in the original code
.calcOperatingCharacteristicsMeansSingleN <- function(
        design,
        numberOfSubjects,
        alternative,
        stDev
        ) {
            
    .assertQtdPlotNumeric(numberOfSubjects, "numberOfSubjects", ".calcOperatingCharacteristicsMeansSingleN",
        lower = 1, integer = TRUE, expectedLength = 2L)

    # Matrix column names identify arms, not result-table row names.
    numberOfSubjects <- unname(numberOfSubjects)

    s <- stDev * sqrt((1 / numberOfSubjects[2]) + (1 / numberOfSubjects[1]))

    qL <- qnorm(design$lowerPercentile)
    qU <- qnorm(design$upperPercentile)

    tvMinusQLs <- design$targetValue - qL * s
    tvMinusQUs <- design$targetValue - qU * s
    lrvMinusQLs <- design$lowerReferenceValue - qL * s
    lrvMinusQUs <- design$lowerReferenceValue - qU * s

    p1 <- pnorm(tvMinusQLs, mean = alternative, sd = s, lower.tail = FALSE)

    tM <- max(lrvMinusQLs, tvMinusQUs)
    p2 <- pnorm(tvMinusQLs, mean = alternative, sd = s) -
        pnorm(tM, mean = alternative, sd = s)

    tm <- min(lrvMinusQLs, tvMinusQUs)
    p3 <- pnorm(lrvMinusQLs, mean = alternative, sd = s) -
        pnorm(tm, mean = alternative, sd = s)

    p4 <- pnorm(tvMinusQUs, mean = alternative, sd = s) -
        pnorm(tm, mean = alternative, sd = s)

    p5 <- pnorm(tm, mean = alternative, sd = s) -
        pnorm(lrvMinusQUs, mean = alternative, sd = s)

    p6 <- pnorm(lrvMinusQUs, mean = alternative, sd = s)

    categories <- c(
        "Go (Fixed)",
        "Go",
        "Indeterminate 1",
        "Indeterminate 2",
        "Stop",
        "Stop (Fixed)"
    )
    return(data.frame(
        category = rep(categories, each = length(alternative)),
        probability = c(p1, p2, p3, p4, p5, p6),
        nControl = numberOfSubjects[1],
        nTreatment = numberOfSubjects[2],
        alternative = alternative,
        stDev = stDev
    ))
}

# This is just going through all numberOfSubjects scenarios
.calcOperatingCharacteristicsMeans <- function(
        design,
        numberOfSubjects,
        alternative,
        stDev
        ) {
    res <- apply(numberOfSubjects, 1, function(singleRow) {
        .calcOperatingCharacteristicsMeansSingleN(
            design = design,
            numberOfSubjects = singleRow,
            alternative = alternative,
            stDev = stDev
        )
    })
    return(do.call(rbind, res))
}

#'
#' @title
#' Operating Characteristics for Quantitative Decisions Comparing Means
#'
#' @description
#' Computes frequentist or Bayesian decision probabilities, selected by the class
#' of the quantitative design rule.
#'
#' @param design A rule from [getDesignQuantitative()].
#' @param numberOfSubjects Positive integer matrix with columns control, treatment;
#'   each row is a sample-size scenario.
#' @param normalApproximation Frequentist option, currently TRUE. Omit for Bayes.
#' @param groups Number of groups; currently two.
#' @param alternative Finite vector of true treatment-minus-control effects.
#' @param stDev Positive common SD or, for Bayes, two known SDs ordered control,
#'   treatment. Defaults to one. Sampling uncertainty of SDs is not modeled.
#' @param priorDist Bayesian RBesT normMix prior on the effect or a list of two
#'   normMix priors ordered control, treatment. NULL selects mixnorm(c(1, 0, 100)).
#'   Only identity-link normal mixtures are supported.
#' @param controlMean True control mean, required only with separate group priors.
#' @param ... Additional arguments are checked for unknown names.
#'
#' @details
#' Bayesian calculations require RBesT. Single-component normal priors use the
#' analytic conjugate normal calculation; normal mixtures use RBesT oc1S (effect
#' prior) or oc2S (group priors). The effect-prior sampling variance is
#' stDevControl^2/nControl + stDevTreatment^2/nTreatment. For group priors,
#' the true treatment mean is controlMean + alternative.
#'
#' RBesT mixture OCs use numerical decision-boundary searches and integration.
#' Agreement with independent references is therefore approximate, typically around
#' 1e-4 with the package defaults; exact threshold ties and near-boundary values
#' require care. No t-based or unknown-variance Bayesian model is implied.
#' Bayesian probabilities are conditional sampling OCs at fixed true effects,
#' not prior-predictive probabilities. All explicit categories are validated for
#' exclusion before the residual category is calculated.
#'
#' There are six frequentist categories, or four Bayesian categories: Go,
#' Indeterminate 1, Indeterminate 2 and Stop. Three-category plots merge the two
#' intermediate categories. Sample-size scenarios, effect order and repeats are
#' retained. Bayesian priors and controlMean are stored for summaries, sensitivity
#' recalculation and [getObjectRCode()].
#'
#' @return An OperatingCharcteristicsMeans object with the rule, assumptions and
#'   category-block probability data frame. Bayesian data additionally retain
#'   stDevControl and stDevTreatment; the scalar stDev facet is their root mean square.
#'
#' @seealso [getDesignQuantitative()], [getOperatingCharacteristicsRates()]
#'
#' @examples
#' rule <- getDesignQuantitative(1)
#' getOperatingCharacteristicsMeans(rule, numberOfSubjects = matrix(c(20, 30), ncol = 2))
#' if (requireNamespace("RBesT", quietly = TRUE)) {
#'     bayes <- getDesignQuantitative(1, bayesian = TRUE)
#'     getOperatingCharacteristicsMeans(bayes,
#'         numberOfSubjects = matrix(c(20, 30), ncol = 2), alternative = c(0, 1, 2))
#' }
#'
#' @export
#'
getOperatingCharacteristicsMeans <- function(
        design,
        ...,
        numberOfSubjects,
        normalApproximation = TRUE,
        groups = 2,
        alternative = seq(0, 1, 0.2),
        stDev = 1,
        priorDist = NULL,
        controlMean = NULL) {
    if (missing(design) || missing(numberOfSubjects)) {
        missingParameters <- c("design", "numberOfSubjects")[c(missing(design), missing(numberOfSubjects))]
        stopMissingArgument("Supply ", paste(missingParameters, collapse = ", "),
            functionName = "getOperatingCharacteristicsMeans", parameter = missingParameters,
            constraint = "design and numberOfSubjects are required",
            diagnosticId = "qtd.means.missing_argument")
    }
    if (inherits(design, "TrialDesignPlanQuantitativeBayes")) {
        .warnInCaseOfUnknownArguments(functionName = "getOperatingCharacteristicsMeans", ...)
        if (!missing(normalApproximation)) {
            stopIllegalArgument("'normalApproximation' is not a Bayesian calculation option",
                functionName = "getOperatingCharacteristicsMeans", parameter = "normalApproximation",
                value = normalApproximation, constraint = "omit for Bayesian rules",
                diagnosticId = "qtd.bayes.frequentist_option")
        }
        return(.getQtdBayesMeans(design, numberOfSubjects, alternative, stDev, controlMean, priorDist, groups))
    }
    if (!is.null(priorDist) || !is.null(controlMean)) {
        stopConflictingArguments("Priors and controlMean require a Bayesian design",
            functionName = "getOperatingCharacteristicsMeans", parameter = "priorDist", value = priorDist,
            relatedParameter = "design", relatedValue = design, constraint = "Bayesian design required",
            diagnosticId = "qtd.bayes.prior_without_bayes_design")
    }
    functionName <- "getOperatingCharacteristicsMeans"
    .warnInCaseOfUnknownArguments(functionName = functionName, ...)
    .assertQtdPlotRule(design, functionName)
    .assertQtdSampleSizes(numberOfSubjects, functionName)
    .assertQtdPlotChoice(normalApproximation, "normalApproximation", TRUE, functionName)
    .assertQtdPlotChoice(groups, "groups", 2L, functionName)
    .assertQtdPlotNumeric(alternative, "alternative", functionName)
    .assertQtdPlotNumeric(stDev, "stDev", functionName, lower = 0, openLower = TRUE, expectedLength = 1L)

    meanOCresults <- .calcOperatingCharacteristicsMeans(
        design = design,
        numberOfSubjects = numberOfSubjects,
        alternative = alternative,
        stDev = stDev
    )
    return(OperatingCharcteristicsMeans$new(
        design = design,
        numberOfSubjects = numberOfSubjects,
        normalApproximation = normalApproximation,
        alternative = alternative,
        stDev = stDev,
        probabilities = meanOCresults
    ))
}
