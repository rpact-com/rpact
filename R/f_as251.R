## |
## |  *Algorithm AS 251.1*
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

.sigmaToBPD <- function(sigma) {
    bpd <- rep(NA_real_, ncol(sigma))
    if (ncol(sigma) == 2) {
        bpd[1:2] <- sqrt(sigma[1, 2])
        return(bpd)
    }

    bpd[1] <- sqrt(sigma[1, 2] * sigma[1, 3] / sigma[2, 3])
    bpd[2:ncol(sigma)] <- sigma[1, 2:ncol(sigma)] / bpd[1]
    return(bpd)
}

#'
#' @title
#' Original Algorithm AS 251: Normal Distribution
#'
#' @description
#' Calculates the Multivariate Normal Distribution with Product Correlation Structure published
#' by Charles Dunnett, Algorithm AS 251.1 Appl.Statist. (1989), Vol.38, No.3, \doi{10.2307/2347754}.
#'
#' @details
#' This is a wrapper function for the original Fortran 77 code.
#' For a multivariate normal vector with correlation structure
#' defined by RHO(I,J) = BPD(I) * BPD(J), computes the probability
#' that the vector falls in a rectangle in n-space with error
#' less than eps.
#'
#' @inheritParams param_three_dots
#' @param A Upper limits of integration. Array of N dimensions
#' @param B Lower limits of integration. Array of N dimensions
#' @param BPD Values defining correlation structure. Array of N dimensions
#' @param INF Determines where integration is done to infinity. Array of N dimensions.
#' Valid values for INF(I):
#'   0 = c(B(I), Inf),
#'   1 = c(-Inf, A(I)),
#'   2 = c(B(I), A(I))
#' @param EPS desired accuracy.  Defaults to 1e-06
#' @param IERC error control. If set to 1, strict error control based on
#' fourth derivative is used.  If set to zero, error control based on halving intervals is used
#' @param HINC Interval width for Simpson's rule. Value of zero caused a default .24 to be used
#'
#' @export
#'
mvnprd <- function(..., A, B, BPD, EPS = 1e-06, INF, IERC = 1, HINC = 0) {
    if (length(A) != length(B) || length(B) != length(BPD) || length(BPD) != length(INF)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "input vectors must have the same length (",
            paste0(sort(unique(c(length(A), length(B), length(BPD), length(INF)))), collapse = " != "), ")", 
            call. = FALSE
        )
    }

    result <- .mvnprd(A, B, BPD, EPS, INF, IERC, HINC)
    value <- result[1]
    attr(value, "bound") <- result[2]
    attr(value, "iFault") <- as.integer(result[3])
    return(value)
}

#'
#' @title
#' Algorithm AS 251: Normal Distribution
#'
#' @description
#' Calculates the Multivariate Normal Distribution with Product Correlation Structure published
#' by Charles Dunnett, Algorithm AS 251.1 Appl.Statist. (1989), Vol.38, No.3, \doi{10.2307/2347754}.
#'
#' @details
#' For a multivariate normal vector with correlation structure
#' defined by rho(i,j) = bpd(i) * bpd(j), computes the probability
#' that the vector falls in a rectangle in n-space with error
#' less than eps.
#'
#' This function calculates the `bdp` value from `sigma`, determines the right `inf` value and calls \code{\link{mvnprd}}.
#'
#' @param lower Lower limits of integration. Array of N dimensions
#' @param upper Upper limits of integration. Array of N dimensions
#' @param sigma Values defining correlation structure. Array of N dimensions
#' @inheritParams param_three_dots
#' @param eps desired accuracy. Defaults to 1e-06
#' @param errorControl error control. If set to 1, strict error control based on
#'        fourth derivative is used. If set to zero, error control based on halving intervals is used
#' @param intervalSimpsonsRule Interval width for Simpson's rule. Value of zero caused a default .24 to be used
#'
#' @export
#'
as251Normal <- function(lower, upper, sigma, ...,
        eps = 1e-06,
        errorControl = c("strict", "halvingIntervals"),
        intervalSimpsonsRule = 0) {
    errorControl <- match.arg(errorControl)
    if (errorControl == "strict") {
        errorControl <- 1
    } else {
        errorControl <- 0
    }

    bpd <- .sigmaToBPD(sigma)
    n <- length(bpd)
    lower <- rep(lower, n)
    upper <- rep(upper, n)

    inf <- rep(2, n)
    inf[is.infinite(upper) & upper > 0] <- 0
    inf[is.infinite(lower) & lower < 0] <- 1

    result <- mvnprd(
        A = upper,
        B = lower,
        BPD = bpd,
        EPS = eps,
        INF = inf,
        IERC = errorControl,
        HINC = intervalSimpsonsRule
    )
    iFault <- attr(result, "iFault")
    return(result)
}

#'
#' @title
#' Original Algorithm AS 251: Student T Distribution
#'
#' @description
#' Calculates the Multivariate Normal Distribution with Product Correlation Structure published
#' by Charles Dunnett, Algorithm AS 251.1 Appl.Statist. (1989), Vol.38, No.3, \doi{10.2307/2347754}.
#'
#' @details
#' This is a wrapper function for the original Fortran 77 code.
#' For a multivariate normal vector with correlation structure
#' defined by RHO(I,J) = BPD(I) * BPD(J), computes the probability
#' that the vector falls in a rectangle in n-space with error
#' less than eps.
#'
#' @inheritParams param_three_dots
#' @param NDF Degrees of Freedom. Use 0 for infinite D.F.
#' @param A Upper limits of integration. Array of N dimensions
#' @param B Lower limits of integration. Array of N dimensions
#' @param BPD Values defining correlation structure. Array of N dimensions
#' @param INF Determines where integration is done to infinity. Array of N dimensions.
#' Valid values for INF(I):
#'   0 = c(B(I), Inf),
#'   1 = c(-Inf, A(I)),
#'   2 = c(B(I), A(I))
#' @param D Non-Centrality Vector
#' @param EPS desired accuracy.  Defaults to 1e-06
#' @param IERC error control. If set to 1, strict error control based on
#' fourth derivative is used.  If set to zero, error control based on halving intervals is used
#' @param HINC Interval width for Simpson's rule. Value of zero caused a default .24 to be used
#'
#' @examples
#' \dontrun{
#' N <- 3
#' RHO <- 0.5
#' B <- rep(-5.0, length = N)
#' A <- rep(5.0, length = N)
#' INF <- rep(2, length = N)
#' BPD <- rep(sqrt(RHO), length = N)
#' D <- rep(0.0, length = N)
#' result <- mvstud(NDF = 0, A = A, B = B, BPD = BPD, INF = INF, D = D)
#' result
#' }
#'
#' @export
#'
mvstud <- function(..., NDF, A, B, BPD, D, EPS = 1e-06, INF, IERC = 1, HINC = 0) {
    if (length(A) != length(B) || length(B) != length(BPD) || length(BPD) != length(INF) || length(INF) != length(D)) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "input vectors must have the same length (",
            paste0(sort(unique(c(length(A), length(B), length(BPD), length(INF), length(D)))), collapse = " != "), ")", 
            call. = FALSE
        )
    }

    result <- .mvstud(NDF, A, B, BPD, D, EPS, INF, IERC, HINC)
    value <- result[1]
    attr(value, "bound") <- result[2]
    attr(value, "iFault") <- as.integer(result[3])
    return(value)
}

#'
#' @title
#' Algorithm AS 251: Student T Distribution
#'
#' @description
#' Calculates the Multivariate Normal Distribution with Product Correlation Structure published
#' by Charles Dunnett, Algorithm AS 251.1 Appl.Statist. (1989), Vol.38, No.3, \doi{10.2307/2347754}.
#'
#' @details
#' For a multivariate normal vector with correlation structure
#' defined by rho(i,j) = bpd(i) * bpd(j), computes the probability
#' that the vector falls in a rectangle in n-space with error
#' less than eps.
#'
#' This function calculates the `bdp` value from `sigma`, determines the right `inf` value and calls \code{\link{mvstud}}.
#'
#' @param lower Lower limits of integration. Array of N dimensions
#' @param upper Upper limits of integration. Array of N dimensions
#' @param sigma Values defining correlation structure. Array of N dimensions
#' @inheritParams param_three_dots
#' @param df Degrees of Freedom. Use 0 for infinite D.F.
#' @param eps desired accuracy. Defaults to 1e-06
#' @param errorControl error control. If set to 1, strict error control based on
#'        fourth derivative is used. If set to zero, error control based on halving intervals is used
#' @param intervalSimpsonsRule Interval width for Simpson's rule. Value of zero caused a default .24 to be used
#'
#' @export
#'
as251StudentT <- function(lower, upper, sigma, ...,
        df,
        eps = 1e-06,
        errorControl = c("strict", "halvingIntervals"),
        intervalSimpsonsRule = 0) {
    errorControl <- match.arg(errorControl)
    if (errorControl == "strict") {
        errorControl <- 1
    } else {
        errorControl <- 0
    }

    bpd <- .sigmaToBPD(sigma)
    n <- length(bpd)
    lower <- rep(lower, n)
    upper <- rep(upper, n)

    inf <- rep(2, n)
    inf[is.infinite(upper) & upper > 0] <- 0
    inf[is.infinite(lower) & lower < 0] <- 1

    d <- rep(0.0, n)

    result <- mvstud(
        NDF = df,
        A = upper,
        B = lower,
        BPD = bpd,
        D = d,
        EPS = eps,
        INF = inf,
        IERC = errorControl,
        HINC = intervalSimpsonsRule
    )
    iFault <- attr(result, "iFault")
    return(result)
}
