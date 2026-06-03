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
            
    stopifnot(is.numeric(numberOfSubjects) && length(numberOfSubjects) == 2)

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
    data.frame(
        category = rep(categories, each = length(alternative)),
        probability = c(p1, p2, p3, p4, p5, p6),
        nControl = numberOfSubjects[1],
        nTreatment = numberOfSubjects[2],
        alternative = alternative,
        stDev = stDev
    )
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
    do.call(rbind, res)
}

#' 
#' Compute operating characteristics for a fixed quantitative design
#'
#' getOperatingCharcteristicsMeans computes the operating characteristics
#' (probabilities of the decision categories) for a fixed (non-adaptive)
#' two-group design with a quantitative endpoint using a normal
#' approximation. The decision boundaries are defined by a
#' \code{TrialDesignPlanQuantitative} object (created by
#' \code{getDesignQuantitative}).
#'
#' @param design An object of class \code{TrialDesignPlanQuantitative}
#'   describing the decision rule (target value, lower reference value,
#'   and percentiles defining the decision boundaries).
#' @param numberOfSubjects Numeric matrix with two columns (control, treatment)
#'   and one or more rows. Each row defines a scenario for the per-arm sample
#'   sizes to evaluate. All entries must be positive integers.
#' @param normalApproximation Logical scalar. Whether to use the normal
#'   approximation for the sampling distribution of the difference in means.
#'   Must be TRUE for this implementation. Default: TRUE.
#' @param groups Integer scalar. Number of groups; currently only 2 is
#'   supported (control and treatment). Default: 2.
#' @param alternative Numeric vector. Grid of true mean differences (treatment -
#'   control) at which to evaluate the operating characteristics. Default:
#'   seq(0, 1, 0.2).
#' @param stDev Numeric scalar. Common standard deviation for the endpoint in
#'   each group. Must be positive. Default: 1.
#'
#' @details
#' The function validates inputs and then calls internal helpers to compute
#' category probabilities for each combination of sample sizes and
#' alternatives. The categories follow the fixed-design decision rule
#' convention used by the QTD module: "Go (Fixed)", "Go", "Indeterminate 1",
#' "Indeterminate 2", "Stop", and "Stop (Fixed)". Probabilities are
#' returned as part of an \code{OperatingCharcteristicsMeans} R6 object which
#' contains both the design and a data.frame of probabilities for plotting and
#' reporting.
#'
#' @return An object of class \code{OperatingCharcteristicsMeans} (R6). The
#'   object contains fields: \code{design}, \code{numberOfSubjects},
#'   \code{normalApproximation}, \code{alternative}, \code{stDev}, and
#'   \code{probabilities} (a data.frame with columns \code{category},
#'   \code{probability}, \code{nControl}, \code{nTreatment}, \code{alternative},
#'   and \code{stDev}).
#'
#' @examples
#' # Create a decision rule
#' decisionRule <- getDesignQuantitative(targetValue = 1.5, lowerReferenceValue = 0)
#'
#' # Evaluate operating characteristics for two sample-size scenarios
#' nMatrix <- matrix(c(50, 100, 75, 150), ncol = 2, byrow = TRUE)
#' oc <- getOperatingCharcteristicsMeans(
#'     design = decisionRule,
#'     numberOfSubjects = nMatrix,
#'     alternative = seq(0, 2, 0.5),
#'     stDev = 1
#' )
#' oc$probabilities
#'
#' @export
#' 
getOperatingCharcteristicsMeans <- function(
    design,
        numberOfSubjects,
        normalApproximation = TRUE,
        groups = 2,
        alternative = seq(0, 1, 0.2),
        stDev = 1) {
    stopifnot(is(design, "TrialDesignPlanQuantitative"))
    stopifnot(
        is.matrix(numberOfSubjects) &&
            ncol(numberOfSubjects) == groups &&
            nrow(numberOfSubjects) > 0 &&
            all(numberOfSubjects > 0)
    )
    stopifnot(isTRUE(normalApproximation))
    stopifnot(groups == 2)
    stopifnot(is.numeric(alternative) && length(alternative) > 0)
    .assertIsValidStandardDeviation(stDev, groups = groups)

    meanOCresults <- .calcOperatingCharacteristicsMeans(
        design = design,
        numberOfSubjects = numberOfSubjects,
        alternative = alternative,
        stDev = stDev
    )
    OperatingCharcteristicsMeans$new(
        design = design,
        design = design,
        numberOfSubjects = numberOfSubjects,
        normalApproximation = normalApproximation,
        alternative = alternative,
        stDev = stDev,
        probabilities = meanOCresults
    )
}
