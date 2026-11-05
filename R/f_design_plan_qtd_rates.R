## Operating characteristics for quantitative trial decisions with binary endpoints.

#'
#' @name OperatingCharcteristicsRates
#'
#' @title
#' Operating Characteristics for Quantitative Decisions Comparing Rates
#'
#' @description
#' Stores exact binomial decision-category probabilities for two independent
#' groups. Obtain instances with [getOperatingCharcteristicsRates()].
#'
#' @template field_numberOfSubjects_qtd
#' @template field_probabilities_qtd_rates
#' @template field_groups_qtd
#' @template field_pi1_qtd_rates
#' @template field_pi2
#' @template field_directionUpper_qtd
#' @template field_intervalMethod_qtd
#' @template field_alternative_qtd_rates
#'
#' @details
#' Inherits the sample-size matrix and probability table from
#' `OperatingCharcteristics`. The originating quantitative decision rule is
#' stored internally. Each sample-size scenario contains all six categories
#' and the supplied treatment rates in their original order, including repeats.
#' Console and Markdown printing use the standard ParameterSet mechanism.
#'
#' @seealso [getOperatingCharcteristicsRates()], [getDesignQuantitative()]
#'
#' @keywords internal
#'
OperatingCharcteristicsRates <- R6::R6Class("OperatingCharcteristicsRates",
    inherit = OperatingCharcteristics,
    public = list(
        groups = NULL,
        pi1 = NULL,
        pi2 = NULL,
        directionUpper = NULL,
        intervalMethod = NULL,
        alternative = NULL,
        #'
        #' @description
        #' Initializes a rates result. Use getOperatingCharcteristicsRates() to
        #' validate inputs and calculate probabilities.
        #'
        #' @param design The originating quantitative decision rule.
        #' @param numberOfSubjects Matrix of control and treatment sample sizes.
        #' @param pi1 Vector of treatment event probabilities.
        #' @param pi2 Single control event probability.
        #' @param directionUpper Whether larger treatment event rates are beneficial.
        #' @param intervalMethod Risk-difference interval construction.
        #' @param probabilities Computed probability data frame.
        #'
        initialize = function(design, numberOfSubjects, pi1, pi2, directionUpper = TRUE,
                intervalMethod = "qtd1", probabilities = NA_real_) {
            super$initialize(design = design, numberOfSubjects = numberOfSubjects,
                probabilities = probabilities)
            .setValueAndParameterType(self, "groups", 2L, 2L)
            self$pi1 <- pi1
            self$pi2 <- pi2
            self$.setParameterType("pi1", C_PARAM_USER_DEFINED)
            self$.setParameterType("pi2", C_PARAM_USER_DEFINED)
            .setValueAndParameterType(self, "directionUpper", directionUpper, TRUE)
            .setValueAndParameterType(self, "intervalMethod", intervalMethod, "qtd1")
            self$alternative <- pi1 - pi2
            if (!directionUpper) {
                self$alternative <- -self$alternative
            }
            self$.setParameterType("alternative", C_PARAM_DERIVED)
        },
        #'
        #' @description
        #' Prints the result to the console.
        #'
        #' @param showType Output mode passed to ParameterSet.
        #' @param digits Optional display precision.
        #'
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        #'
        #' @description
        #' Internal rendering method used by standard console and Markdown output.
        #'
        #' @param showType Output mode passed to ParameterSet.
        #' @param digits Optional display precision.
        #' @param consoleOutputEnabled Whether to print or collect the output.
        #'
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            self$.resetCat()
            if (showType != 1) {
                super$.show(showType = showType, digits = digits,
                    consoleOutputEnabled = consoleOutputEnabled)
            } else {
                self$.cat("Parameters and output of ", self$.toString(), ":\n\n",
                    heading = 1, consoleOutputEnabled = consoleOutputEnabled)
                self$.showParametersOfOneGroup(self$.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
                self$.showParametersOfOneGroup(self$.getDerivedParameters(), "Derived from user defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
                self$.showParametersOfOneGroup(self$.getDefaultParameters(), "Default parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
                self$.cat("Output:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
                .showDataFrame(self$probabilities, self, niceColumnNamesEnabled = TRUE,
                    digits = digits, consoleOutputEnabled = consoleOutputEnabled)
                self$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        #'
        #' @description
        #' Returns the result description used in output headings.
        #'
        #' @param startWithUpperCase Whether to capitalize the first character.
        #'
        .toString = function(startWithUpperCase = FALSE) {
            text <- "operating-characteristics for comparing rates"
            if (startWithUpperCase) {
                text <- .firstCharacterToUpperCase(text)
            }
            return(text)
        }
    )
)

.calcOperatingCharacteristicsRates <- function(design, numberOfSubjects, pi1, pi2,
        directionUpper, intervalMethod) {
    categories <- .getQtdCategories()
    alternative <- pi1 - pi2
    if (!directionUpper) {
        alternative <- -alternative
    }
    result <- vector("list", nrow(numberOfSubjects))
    decisionMaps <- new.env(parent = emptyenv())
    for (scenario in seq_len(nrow(numberOfSubjects))) {
        sizes <- unname(numberOfSubjects[scenario, ])
        key <- paste(sizes, collapse = "/")
        if (!exists(key, envir = decisionMaps, inherits = FALSE)) {
            decisionMaps[[key]] <- .getQtdBinaryDecisionData(design, sizes, directionUpper,
                intervalMethod = intervalMethod, functionName = "getOperatingCharcteristicsRates")$category
        }
        decisions <- decisionMaps[[key]]
        controlProbabilities <- stats::dbinom(0:sizes[1], sizes[1], pi2)
        probabilities <- vapply(pi1, function(treatmentRate) {
            treatmentProbabilities <- stats::dbinom(0:sizes[2], sizes[2], treatmentRate)
            cellProbabilities <- as.vector(outer(controlProbabilities, treatmentProbabilities))
            return(vapply(categories, function(category) {
                return(sum(cellProbabilities[decisions == category]))
            }, numeric(1)))
        }, numeric(length(categories)))
        result[[scenario]] <- data.frame(
            category = rep(categories, each = length(pi1)),
            probability = as.vector(t(probabilities)),
            nControl = sizes[1], nTreatment = sizes[2],
            alternative = rep(alternative, times = length(categories)),
            pi1 = rep(pi1, times = length(categories)), pi2 = pi2
        )
    }
    return(do.call(rbind, result))
}

#'
#' @title
#' Operating Characteristics for Quantitative Decisions Comparing Rates
#'
#' @description
#' Calculates probabilities of all six quantitative decision categories by
#' exact summation over two independent binomial event counts.
#'
#' @param design A decision rule from [getDesignQuantitative()]. Reference
#'   values refer to the risk difference in the beneficial direction.
#' @param numberOfSubjects Numeric matrix with two columns, control followed by
#'   treatment, and at least one row. Each row is a sample-size scenario; all
#'   entries must be positive integers.
#' @param pi1 Numeric vector of true treatment event probabilities in `[0, 1]`.
#' @param pi2 Single true control event probability in `[0, 1]`.
#' @param directionUpper Whether increasing treatment event rates are beneficial.
#'   If TRUE (default), the effect is `pi1 - pi2`; if FALSE, it is `pi2 - pi1`.
#' @param intervalMethod One of `"qtd1"` (default), `"farringtonManning"`,
#'   `"miettinenNurminen"`, or `"newcombe"`, as described in
#'   [plot.TrialDesignPlanQuantitative()].
#' @param ... Additional arguments are checked for unknown names.
#'
#' @details
#' Decision boundaries use the same interval calculations and equality rules
#' as the binary decision map. The QTD1 method reproduces the original program,
#' including Wilson limits rounded to four decimal places in its variance
#' construction, and requires `lowerPercentile <= 0.5 <= upperPercentile`.
#' The other methods support ordered percentiles anywhere in (0, 1).
#' Exact summation describes the sampling distribution of the counts; the
#' interval methods themselves are approximate frequentist constructions.
#'
#' The six exclusive categories are Go (Fixed), Go, Indeterminate 1,
#' Indeterminate 2, Stop, and Stop (Fixed). Add the two Go, Indeterminate, or
#' Stop categories for the respective three-category probabilities. Each
#' scenario contains six blocks, with the supplied treatment rates in order
#' within each block. Repeated rates and sample-size scenarios are retained.
#' A count grid may contain at most one million cells per sample-size scenario.
#' Counts and probabilities are computed without requiring ggplot2 or Java.
#'
#' @return An `OperatingCharcteristicsRates` object supporting console and
#'   Markdown printing. Its `probabilities` data frame contains `category`,
#'   `probability`, `nControl`, `nTreatment`, `alternative`, `pi1`, and `pi2`.
#'   `alternative` is the true risk difference in the beneficial direction.
#'
#' @examples
#' # Binary example from the original QTD1 demonstration:
#' rule <- getDesignQuantitative(targetValue = 0, lowerReferenceValue = -0.05)
#' oc <- getOperatingCharcteristicsRates(rule,
#'     numberOfSubjects = matrix(c(75, 75, 75, 150), ncol = 2, byrow = TRUE),
#'     pi1 = c(0.1, 0.15, 0.175, 0.2, 0.25), pi2 = 0.15,
#'     directionUpper = FALSE)
#' print(oc)
#' cat(print(oc, markdown = TRUE)[1], "\n")
#'
#' @seealso [getDesignQuantitative()], [getOperatingCharcteristicsMeans()],
#'   [OperatingCharcteristicsRates]
#'
#' @export
#'
getOperatingCharcteristicsRates <- function(design, ..., numberOfSubjects, pi1, pi2,
        directionUpper = TRUE, intervalMethod = "qtd1") {
    functionName <- "getOperatingCharcteristicsRates"
    .warnInCaseOfUnknownArguments(functionName = functionName, ...)
    missingInputs <- c(design = missing(design), numberOfSubjects = missing(numberOfSubjects),
        pi1 = missing(pi1), pi2 = missing(pi2))
    if (any(missingInputs)) {
        stopMissingArgument("Supply required arguments: ", paste(names(missingInputs)[missingInputs], collapse = ", "),
            functionName = functionName, parameter = names(missingInputs)[missingInputs],
            constraint = "design, numberOfSubjects, pi1 and pi2 are required",
            diagnosticId = "qtd.rates.missing_argument")
    }
    if (!inherits(design, "TrialDesignPlanQuantitative")) {
        stopIllegalArgument("'design' must be a quantitative decision rule",
            functionName = functionName, parameter = "design", value = design,
            constraint = "TrialDesignPlanQuantitative", diagnosticId = "qtd.rates.invalid_design")
    }
    .assertQtdPlotRule(design, functionName)
    if (!is.matrix(numberOfSubjects) || ncol(numberOfSubjects) != 2L || nrow(numberOfSubjects) == 0L) {
        stopIllegalArgument("'numberOfSubjects' must be a matrix with two columns and at least one row",
            functionName = functionName, parameter = "numberOfSubjects", value = numberOfSubjects,
            constraint = "matrix with columns control and treatment and at least one row",
            diagnosticId = "qtd.rates.invalid_sample_size_matrix")
    }
    .assertQtdPlotNumeric(numberOfSubjects, "numberOfSubjects", functionName, lower = 1, integer = TRUE)
    for (parameter in c("pi1", "pi2")) {
        value <- get(parameter)
        if (!is.null(dim(value))) {
            stopIllegalArgument("'", parameter, "' must be a numeric vector, not an array or matrix",
                functionName = functionName, parameter = parameter, value = value,
                constraint = "numeric input without dimensions",
                diagnosticId = "qtd.rates.rate_must_be_vector")
        }
    }
    .assertIsNumericVector(pi1, "pi1")
    .assertIsNumericVector(pi2, "pi2")
    .assertQtdPlotNumeric(pi1, "pi1", functionName, lower = 0, upper = 1)
    .assertQtdPlotNumeric(pi2, "pi2", functionName, lower = 0, upper = 1, expectedLength = 1L)
    .assertIsSingleLogical(directionUpper, "directionUpper")
    .assertQtdPlotChoice(intervalMethod, "intervalMethod",
        c("qtd1", "farringtonManning", "miettinenNurminen", "newcombe"), functionName)
    probabilities <- .calcOperatingCharacteristicsRates(design, numberOfSubjects, pi1, pi2,
        directionUpper, intervalMethod)
    return(OperatingCharcteristicsRates$new(design = design, numberOfSubjects = numberOfSubjects,
        pi1 = pi1, pi2 = pi2, directionUpper = directionUpper, intervalMethod = intervalMethod,
        probabilities = probabilities))
}
