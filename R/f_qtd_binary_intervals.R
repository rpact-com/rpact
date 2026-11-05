## |
## |  *One-sided risk-difference limits for quantitative binary decision maps*
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

.getQtdWilsonLimits <- function(events, n, percentile) {
    z <- abs(stats::qnorm(percentile))
    rate <- events / n
    denominator <- 1 + z^2 / n
    center <- (rate + z^2 / (2 * n)) / denominator
    radius <- z * sqrt(rate * (1 - rate) / n + z^2 / (4 * n^2)) / denominator
    return(list(lower = pmax(0, center - radius), upper = pmin(1, center + radius)))
}

.getQtdNewcombeBound <- function(events1, n1, events2, n2, percentile) {
    rate1 <- events1 / n1
    rate2 <- events2 / n2
    difference <- rate1 - rate2
    if (percentile == 0.5) {
        return(difference)
    }
    first <- .getQtdWilsonLimits(events1, n1, percentile)
    second <- .getQtdWilsonLimits(events2, n2, percentile)
    if (percentile < 0.5) {
        return(pmax(-1, difference - sqrt((rate1 - first$lower)^2 + (second$upper - rate2)^2)))
    }
    return(pmin(1, difference + sqrt((first$upper - rate1)^2 + (rate2 - second$lower)^2)))
}

.getQtdConstrainedRates <- function(events1, n1, events2, n2, difference) {
    lower <- max(0, -difference)
    upper <- min(1, 1 - difference)
    contribution <- function(count, probability) {
        if (count == 0) {
            return(0)
        }
        return(count / probability)
    }
    slope <- function(rate2) {
        rate1 <- rate2 + difference
        return(contribution(events1, rate1) - contribution(n1 - events1, 1 - rate1) +
            contribution(events2, rate2) - contribution(n2 - events2, 1 - rate2))
    }
    if (lower == upper || slope(lower) <= 0) {
        return(c(lower + difference, lower))
    }
    if (slope(upper) >= 0) {
        return(c(upper + difference, upper))
    }

    # Reuse rpact's analytic constrained estimate. A concavity/KKT check protects
    # boundary and floating-point cases; a bracketed score root is the fallback.
    candidate <- suppressWarnings(.getFarringtonManningValues(
        rate1 = events1 / n1, rate2 = events2 / n2,
        theta = difference, allocation = n1 / n2, method = "diff"
    ))$ml2
    if (is.finite(candidate) && candidate > lower && candidate < upper &&
            abs(slope(candidate)) <= 1e-7 * (n1 + n2)) {
        return(c(candidate + difference, candidate))
    }
    rate2 <- stats::uniroot(slope, c(lower, upper), tol = 1e-12)$root
    return(c(rate2 + difference, rate2))
}

.getQtdScoreBound <- function(events1, n1, events2, n2, percentile, correction = FALSE) {
    estimate <- events1 / n1 - events2 / n2
    if (percentile == 0.5 || (percentile < 0.5 && estimate == -1) ||
            (percentile > 0.5 && estimate == 1)) {
        return(estimate)
    }
    varianceCorrection <- 1
    if (correction) {
        varianceCorrection <- (n1 + n2) / (n1 + n2 - 1)
    }
    targetScore <- -stats::qnorm(percentile)
    scoreEquation <- function(difference) {
        if (difference == estimate) {
            return(-targetScore)
        }
        if (difference == -1) {
            return(Inf)
        }
        if (difference == 1) {
            return(-Inf)
        }
        rates <- .getQtdConstrainedRates(events1, n1, events2, n2, difference)
        variance <- varianceCorrection * (rates[1] * (1 - rates[1]) / n1 + rates[2] * (1 - rates[2]) / n2)
        return((estimate - difference) / sqrt(variance) - targetScore)
    }
    interval <- c(-1, estimate)
    if (percentile > 0.5) {
        interval <- c(estimate, 1)
    }
    result <- tryCatch(
        {
            stats::uniroot(scoreEquation, interval, tol = 1e-10, maxiter = 200L, check.conv = TRUE)$root
        },
        error = function(e) {
            stopRuntimeIssue("Risk-difference score inversion failed: ", conditionMessage(e),
                functionName = ".getQtdScoreBound", diagnosticId = "qtd.plot.score_inversion_failed",
                context = list(events1 = events1, n1 = n1, events2 = events2, n2 = n2,
                    percentile = percentile, correction = correction, interval = interval)
            )
        }
    )
    return(result)
}

.getQtdRiskDifferenceBounds <- function(events1, n1, events2, n2, lowerPercentile,
        upperPercentile, intervalMethod) {
    if (intervalMethod == "newcombe") {
        return(list(
            lower = .getQtdNewcombeBound(events1, n1, events2, n2, lowerPercentile),
            upper = .getQtdNewcombeBound(events1, n1, events2, n2, upperPercentile)
        ))
    }
    correction <- intervalMethod == "miettinenNurminen"
    lower <- vapply(seq_along(events1), function(i) {
        return(.getQtdScoreBound(events1[i], n1, events2[i], n2, lowerPercentile, correction))
    }, numeric(1))
    upper <- vapply(seq_along(events1), function(i) {
        return(.getQtdScoreBound(events1[i], n1, events2[i], n2, upperPercentile, correction))
    }, numeric(1))
    return(list(lower = lower, upper = upper))
}
