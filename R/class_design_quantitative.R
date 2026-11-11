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
#' @name TrialDesignPlanQuantitativeBase
#'
#' @title
#' Common Quantitative Trial Decision Rule
#'
#' @description
#' Internal ParameterSet base for frequentist and Bayesian quantitative decision
#' rules. Obtain instances of its concrete subclasses with [getDesignQuantitative()].
#'
#' @template field_targetValue_qtd
#' @template field_lowerReferenceValue_qtd
#' @template field_upperPercentile_qtd
#' @template field_lowerPercentile_qtd
#'
#' @details
#' The reference values and percentiles are optional for explicit Bayesian rules.
#' The concrete subclasses are TrialDesignPlanQuantitative and
#' TrialDesignPlanQuantitativeBayes. ParameterSet supplies shared parameter and
#' output management; internal dot-prefixed fields are implementation details.
#'
#' @seealso [getDesignQuantitative()]
#'
#' @keywords internal
#'
TrialDesignPlanQuantitativeBase <- R6::R6Class(
    "TrialDesignPlanQuantitativeBase",
    inherit = ParameterSet,
    public = list(
        targetValue = NULL,
        lowerReferenceValue = NULL,
        upperPercentile = NULL,
        lowerPercentile = NULL,
        #'
        #' @description Initializes shared reference parameters.
        #' @param ... Reserved for ParameterSet compatibility.
        #' @param targetValue,lowerReferenceValue Reference effects or NA.
        #' @param upperPercentile,lowerPercentile Reference percentiles or NA.
        #'
        initialize = function(
                ...,
                targetValue = NA_real_, 
                lowerReferenceValue = NA_real_, 
                upperPercentile = 0.9, 
                lowerPercentile = 0.2) {
                
            .setValueAndParameterType(self, "targetValue", targetValue, NA_real_)
            .setValueAndParameterType(self, "lowerReferenceValue", lowerReferenceValue, NA_real_)
            .setValueAndParameterType(self, "upperPercentile", upperPercentile, 0.9)
            .setValueAndParameterType(self, "lowerPercentile", lowerPercentile, 0.2)
        },
        #'
        #' @description Prints the quantitative rule.
        #' @param showType Output mode.
        #' @param digits Display precision.
        #'
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        #'
        #' @description Renders the quantitative rule.
        #' @param showType Output mode.
        #' @param digits Display precision.
        #' @param consoleOutputEnabled Print or collect output.
        #'
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            "Method for automatically printing trial design objects"
            self$.resetCat()
            if (inherits(self, "TrialDesignPlanQuantitativeBayes") && showType != 2) {
                result <- .createSummaryQuantitative(self, digits = digits, output = "all")
                .showSummaryQuantitative(result, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
                self$.catLines <- result$.catLines
                return(invisible())
            }
            if (showType == 3) {
                .createSummary(self, digits = digits)$.show(
                    showType = 1,
                    digits = digits, consoleOutputEnabled = consoleOutputEnabled
                )
            } else if (showType == 2) {
                super$.show(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            } else {
                self$.cat("Design parameters and output of ", self$.toString(), ":\n\n",
                    heading = 1,
                    consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getUserDefinedParameters(), "User defined parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showParametersOfOneGroup(self$.getDefaultParameters(), "Default parameters",
                    orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled
                )
                self$.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        #'
        #' @description Returns the object description.
        #' @param startWithUpperCase Capitalize the description.
        #'
        .toString = function(startWithUpperCase = FALSE) {
            s <- "quantitative trial design (decision rule)"
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
        }
    )
)

#'
#' @name TrialDesignPlanQuantitative
#'
#' @title
#' Frequentist Quantitative Trial Decision Rule
#'
#' @description
#' Represents the reference-value and percentile rule for a fixed two-group trial.
#' Obtain instances with [getDesignQuantitative()].
#'
#'
#' @eval local({
#'     templates <- c("targetValue_qtd", "lowerReferenceValue_qtd", "upperPercentile_qtd", "lowerPercentile_qtd")
#'     entries <- vapply(templates, function(template) {
#'         lines <- readLines(file.path("man-roxygen", paste0("field_", template, ".R")), warn = FALSE)
#'         field <- sub("^#' @field ", "", grep("^#' @field ", lines, value = TRUE))
#'         name <- sub(" .*", "", field)
#'         description <- substring(field, nchar(name) + 2L)
#'         return(paste0("\\item{\\code{", name, "}}{", description, "}"))
#'     }, character(1))
#'     paste0("@section Inherited fields:\n\\describe{", paste(entries, collapse = "\n"), "}")
#' })
#'
#' @details
#' Inherits all public data fields from TrialDesignPlanQuantitativeBase.
#' Operating characteristics use the frequentist sampling model and six categories.
#'
#' @seealso [getOperatingCharacteristicsMeans()], [getOperatingCharacteristicsRates()]
#'
#' @keywords internal
#'
#' @export
#'
TrialDesignPlanQuantitative <- R6::R6Class(
    "TrialDesignPlanQuantitative",
    inherit = TrialDesignPlanQuantitativeBase
)

#'
#' @name QuantitativeDecisionThresholds
#'
#' @title
#' Posterior Probability Criteria for Quantitative Decisions
#'
#' @description
#' Internal ParameterSet holding a conjunction of posterior probability criteria.
#' Obtain instances through [getQuantitativeDecisionThresholds()].
#'
#' @template field_quantiles_qtd
#' @template field_probabilities_thresholds_qtd
#' @template field_lowerTail_qtd
#'
#' @details
#' All criteria must hold, using strict probability comparisons as in RBesT.
#' Thresholds refer to the effect in the beneficial direction.
#'
#' @seealso [getDesignQuantitative()]
#'
#' @keywords internal
#'
QuantitativeDecisionThresholds <- R6::R6Class(
    "QuantitativeDecisionThresholds",
    inherit = ParameterSet,
    public = list(
        quantiles = NULL,
        probabilities = NULL,
        lowerTail = NULL,
        #'
        #' @description Stores validated posterior criteria.
        #' @param quantiles Effect thresholds.
        #' @param probabilities Posterior probability thresholds.
        #' @param lowerTail Directions of the probability inequalities.
        #'
        initialize = function(quantiles, probabilities, lowerTail) {
            self$quantiles <- quantiles
            self$probabilities <- probabilities
            self$lowerTail <- lowerTail
            for (parameter in c("quantiles", "probabilities", "lowerTail")) {
                self$.setParameterType(parameter, C_PARAM_USER_DEFINED)
            }
        },
        #'
        #' @description Returns the object description.
        #' @param startWithUpperCase Capitalize the description.
        #'
        .toString = function(startWithUpperCase = FALSE) {
            return("posterior probability criteria")
        }
    )
)

#'
#' @name TrialDesignPlanQuantitativeBayes
#'
#' @title
#' Bayesian Quantitative Trial Decision Rule
#'
#' @description
#' Internal Bayesian decision-rule class. Obtain instances through
#' [getDesignQuantitative()] with bayesian = TRUE or explicit criteria.
#'
#' @template field_go_qtd
#' @template field_stop_qtd
#' @template field_indeterminate1_qtd
#' @template field_percentileMapping_qtd
#'
#' @eval local({
#'     templates <- c("targetValue_qtd", "lowerReferenceValue_qtd", "upperPercentile_qtd", "lowerPercentile_qtd")
#'     entries <- vapply(templates, function(template) {
#'         lines <- readLines(file.path("man-roxygen", paste0("field_", template, ".R")), warn = FALSE)
#'         field <- sub("^#' @field ", "", grep("^#' @field ", lines, value = TRUE))
#'         name <- sub(" .*", "", field)
#'         description <- substring(field, nchar(name) + 2L)
#'         return(paste0("\\item{\\code{", name, "}}{", description, "}"))
#'     }, character(1))
#'     paste0("@section Inherited fields:\n\\describe{", paste(entries, collapse = "\n"), "}")
#' })
#'
#' @details
#' Inherits optional reference values and percentiles from
#' TrialDesignPlanQuantitativeBase. These are NA for freely specified criteria.
#' Go, Stop and Indeterminate 1 must be provably mutually exclusive for every
#' continuous posterior CDF; Indeterminate 2 includes all remaining outcomes,
#' including exact probability-threshold ties. Priors are supplied to the OC
#' functions, rather than stored in the decision rule.
#'
#' @seealso [getOperatingCharacteristicsMeans()], [getOperatingCharacteristicsRates()]
#'
#' @keywords internal
#'
TrialDesignPlanQuantitativeBayes <- R6::R6Class(
    "TrialDesignPlanQuantitativeBayes",
    inherit = TrialDesignPlanQuantitativeBase,
    public = list(
        go = NULL,
        stop = NULL,
        indeterminate1 = NULL,
        percentileMapping = NULL,
        #'
        #' @description Stores validated Bayesian rules and optional references.
        #' @param go,stop,indeterminate1 Exclusive posterior criteria.
        #' @param targetValue,lowerReferenceValue Reference effects or NA.
        #' @param upperPercentile,lowerPercentile Reference percentiles or NA.
        #' @param percentileMapping Interpretation of reference percentiles.
        #'
        initialize = function(go, stop, indeterminate1, targetValue = NA_real_,
                lowerReferenceValue = NA_real_, upperPercentile = NA_real_,
                lowerPercentile = NA_real_, percentileMapping = "quantile") {
            super$initialize(targetValue = targetValue, lowerReferenceValue = lowerReferenceValue,
                upperPercentile = upperPercentile, lowerPercentile = lowerPercentile)
            self$go <- go$clone(deep = TRUE)
            self$stop <- stop$clone(deep = TRUE)
            self$indeterminate1 <- indeterminate1$clone(deep = TRUE)
            self$percentileMapping <- percentileMapping
            for (parameter in c("go", "stop", "indeterminate1", "percentileMapping")) {
                self$.setParameterType(parameter, C_PARAM_USER_DEFINED)
            }
        },
        #'
        #' @description Returns the object description.
        #' @param startWithUpperCase Capitalize the description.
        #'
        .toString = function(startWithUpperCase = FALSE) {
            text <- "Bayesian quantitative trial design (decision rule)"
            if (startWithUpperCase) {
                text <- .firstCharacterToUpperCase(text)
            }
            return(text)
        }
    )
)
