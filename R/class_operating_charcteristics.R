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

#'
#' @name OperatingCharcteristics
#'
#' @title
#' Operating Characteristics for Quantitative Decisions
#'
#' @description
#' Internal ParameterSet container shared by means and rates results.
#' Obtain concrete instances through the endpoint-specific OC functions.
#'
#' @template field_numberOfSubjects_qtd
#' @template field_probabilities_qtd
#' @template field_priorDist_qtd
#' @template field_priorDist1_qtd
#' @template field_priorDist2_qtd
#' @template field_controlMean_qtd
#'
#' @details
#' The originating rule is stored in the internal .design field. Priors and the
#' control mean are NULL when not applicable. Bayesian results store four exclusive
#' categories; frequentist results store six. Sampling probabilities are conditional
#' on the supplied true effect or response rates; they are not predictive assurance.
#'
#' @seealso [getOperatingCharacteristicsMeans()], [getOperatingCharacteristicsRates()]
#'
#' @keywords internal
#'
OperatingCharcteristics <- R6::R6Class(
    "OperatingCharcteristics",
    inherit = ParameterSet,
    public = list(
        .design = NULL,
        numberOfSubjects = NULL,
        probabilities = NULL,
        priorDist = NULL,
        priorDist1 = NULL,
        priorDist2 = NULL,
        controlMean = NULL,
        #'
        #' @description
        #' Initializes the shared quantitative operating-characteristics fields.
        #'
        #' @param design The originating quantitative decision rule.
        #' @param numberOfSubjects Matrix of control and treatment sample sizes.
        #' @param probabilities Computed probability data frame.
        #'
        initialize = function(
                design,
                numberOfSubjects,
                probabilities = NA_real_
                ) {
            self$.design <- design
            self$numberOfSubjects <- numberOfSubjects
            self$probabilities <- probabilities
            self$.setParameterType("numberOfSubjects", C_PARAM_USER_DEFINED)
            self$.setParameterType("probabilities", C_PARAM_GENERATED)
        }
    )
)

#'
#' @name OperatingCharcteristicsMeans
#'
#' @title
#' Operating Characteristics for Comparing Means
#'
#' @description
#' Stores fixed-design quantitative decision probabilities for means. Obtain
#' instances through [getOperatingCharacteristicsMeans()].
#'
#' @template field_normalApproximation_qtd
#' @template field_groups_qtd
#' @template field_alternative_qtd_means
#' @template field_stDev_qtd
#'
#' @eval local({
#'     templates <- c("numberOfSubjects_qtd", "probabilities_qtd", "priorDist_qtd", "priorDist1_qtd", "priorDist2_qtd", "controlMean_qtd")
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
#' Inherits sample sizes, probabilities, priors and controlMean from
#' OperatingCharcteristics. The rates priors priorDist1 and priorDist2 remain NULL.
#'
#' @seealso [getDesignQuantitative()], [getOperatingCharacteristicsMeans()]
#'
#' @keywords internal
#'
#' @export
#'
OperatingCharcteristicsMeans <- R6::R6Class(
    "OperatingCharcteristicsMeans",
    inherit = OperatingCharcteristics,
    public = list(
        normalApproximation = NULL,
        groups = NULL,
        alternative = NULL,
        stDev = NULL,
        #'
        #' @description Initializes means OC assumptions and probability output.
        #' @param ... Reserved for ParameterSet compatibility.
        #' @param design Originating quantitative rule.
        #' @param numberOfSubjects Control and treatment sample sizes.
        #' @param normalApproximation Normal sampling model flag.
        #' @param groups Number of groups.
        #' @param alternative True effect vector.
        #' @param stDev Known standard deviations.
        #' @param probabilities Category-block probability table.
        #'
        initialize = function(
                ...,
                design,
                numberOfSubjects,
                normalApproximation = TRUE,
                groups = 2L,
                alternative = seq(0, 1, 0.2),
                stDev = 1,
                probabilities = NA_real_
                ) {      
            super$initialize(
                design = design, 
                numberOfSubjects = numberOfSubjects, 
                probabilities = probabilities, ...)
            
            .setValueAndParameterType(self, "normalApproximation", normalApproximation, TRUE)
            .setValueAndParameterType(self, "groups", groups, 2L)
            .setValueAndParameterType(self, "alternative", alternative, seq(0, 1, 0.2))
            .setValueAndParameterType(self, "stDev", stDev, 1)
        },
        #'
        #' @description Prints the result.
        #' @param showType Output mode.
        #' @param digits Display precision.
        #'
        show = function(showType = 1, digits = NA_integer_) {
            self$.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
        },
        #'
        #' @description Renders the result.
        #' @param showType Output mode.
        #' @param digits Display precision.
        #' @param consoleOutputEnabled Print or collect output.
        #'
        .show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
            if (!.showQtdOperatingCharacteristics(self, showType, digits, consoleOutputEnabled)) {
                super$.show(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
            }
        },
        #'
        #' @description Returns the result description.
        #' @param startWithUpperCase Capitalize the description.
        #'
        .toString = function(startWithUpperCase = FALSE) {
            s <- "operating-characteristics for comparing means"
            return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
        }
    )
)
