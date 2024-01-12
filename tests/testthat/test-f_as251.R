## |
## |  *Unit tests*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |
## |  RPACT company website: https://www.rpact.com
## |  RPACT package website: https://www.rpact.org
## |
## |  Contact us for information about our services: info@rpact.com
## |
## |  File name: test-f_as251.R
## |  Creation date: 21 December 2023, 08:48:20
## |  File version: $Revision: 7554 $
## |  Last changed: $Date: 2024-01-12 10:19:05 +0100 (Fr, 12 Jan 2024) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing Dunnett AS 251 Functions")


test_that("mvnprd and sadmvn are equal", {
    A <- rep(2, 3)
    B <- rep(-3, 3)
    BPD <- sqrt(c(0.3, 0.4, 0.5))
    eps <- 1e-06
    INF <- rep(2, 3)
    ierc <- 0
    hinc <- 0
    x1 <- mvnprd(A = A, B = B, BPD = BPD, EPS = eps, INF = INF, IERC = ierc, HINC = hinc)

    ## Comparison of the results of numeric object 'x1' with expected results
    expect_equal(x1[1], 0.93576282, tolerance = 1e-07, label = paste0(x1[1]))
})

test_that("mvstud and sadmvn are equal", {
    NDF <- 4
    A <- rep(2, 3)
    B <- rep(-3, 3)
    BPD <- sqrt(c(0.3, 0.4, 0.5))
    eps <- 1e-06
    INF <- rep(2, 3)
    ierc <- 0
    hinc <- 0
    D <- rep(0, 3)
    x1 <- mvstud(NDF = NDF, A = A, B = B, BPD = BPD, D = D, EPS = eps, INF = INF, IERC = ierc, HINC = hinc)

    ## Comparison of the results of numeric object 'x1' with expected results
    expect_equal(x1[1], 0.82702172, tolerance = 1e-07, label = paste0(x1[1]))
})

test_that("mvstud, sadmvn, and as251StudentT are equal", {
    NDF <- 4
    A <- rep(2, 3)
    B <- rep(-3, 3)
    BPD <- sqrt(c(0.3, 0.4, 0.5))
    eps <- 1e-06
    INF <- rep(2, 3)
    ierc <- 0
    hinc <- 0
    D <- rep(0, 3)
    x1 <- mvstud(NDF = NDF, A = A, B = B, BPD = BPD, D = D, EPS = eps, INF = INF, IERC = ierc, HINC = hinc)

    sigma <- BPD %*% t(BPD)
    diag(sigma) <- 1

    x2 <- as251StudentT(lower = -3, upper = 2, sigma = sigma, df = NDF, inf = INF, eps = eps, ierc = ierc, hinc = 0)

    expect_equal(x1[1], x2[1], tolerance = eps)

    ## Comparison of the results of numeric object 'x1' with expected results
    expect_equal(x1[1], 0.82702172, tolerance = 1e-07, label = paste0(x1[1]))

    ## Comparison of the results of numeric object 'x2' with expected results
    expect_equal(x2[1], 0.82702184, tolerance = 1e-07, label = paste0(x2[1]))
})

test_that("binary case: sadmvn and as251Normal are equal", {
    frac <- rep(0.7, 2)
    sigma <- sqrt(frac) %*% sqrt(t(frac))
    diag(sigma) <- 1
    as251StudentT(lower = -Inf, upper = 2, sigma = sigma, df = 500000)

    eps <- 1e-06
    frac <- rep(0.7, 2)
    sigma <- sqrt(frac) %*% sqrt(t(frac))
    diag(sigma) <- 1
    x1 <- as251Normal(lower = -Inf, upper = 2, sigma = sigma, eps = eps)[1]

    ## Comparison of the results of numeric object 'x1' with expected results
    expect_equal(x1[1], 0.96186131, tolerance = 1e-07, label = paste0(x1[1]))
})

test_that("binary case: sadmvn and as251StudentT are equal", {
    eps <- 1e-06
    frac <- rep(0.7, 2)
    sigma <- sqrt(frac) %*% sqrt(t(frac))
    diag(sigma) <- 1
    x1 <- as251StudentT(lower = -Inf, upper = 2, sigma = sigma, df = 22, eps = eps)[1]

    ## Comparison of the results of numeric object 'x1' with expected results
    expect_equal(x1[1], 0.95259881, tolerance = 1e-07, label = paste0(x1[1]))
})

test_that("mvnprd, sadmvn, and as251Normal are equal", {
    A <- rep(2, 3)
    B <- rep(-3, 3)
    BPD <- sqrt(c(0.3, 0.4, 0.5))
    eps <- 1e-06
    INF <- rep(2, 3)
    ierc <- 0
    hinc <- 0
    x1 <- mvnprd(A = A, B = B, BPD = BPD, EPS = eps, INF = INF, IERC = ierc, HINC = hinc)

    sigma <- BPD %*% t(BPD)
    diag(sigma) <- 1

    x2 <- as251Normal(lower = -3, upper = 2, sigma = sigma, inf = INF, eps = eps, ierc = ierc, hinc = 0)

    expect_equal(x1[1], x2[1], tolerance = eps)

    ## Comparison of the results of numeric object 'x1' with expected results
    expect_equal(x1[1], 0.93576282, tolerance = 1e-07, label = paste0(x1[1]))

    ## Comparison of the results of numeric object 'x2' with expected results
    expect_equal(x2[1], 0.93576294, tolerance = 1e-07, label = paste0(x2[1]))
})

test_that("mvnprd, sadmvn, and as251Normal are equal for -Inf lower bound", {
    A <- rep(2, 3)
    B <- rep(-Inf, 3)
    BPD <- sqrt(c(0.3, 0.4, 0.6))
    eps <- 1e-06
    INF <- rep(1, 3)
    ierc <- 0
    hinc <- 0
    x1 <- mvnprd(A = A, B = B, BPD = BPD, EPS = eps, INF = INF, IERC = ierc, HINC = hinc)

    sigma <- BPD %*% t(BPD)
    diag(sigma) <- 1

    x2 <- as251Normal(lower = -Inf, upper = 2, sigma = sigma)

    expect_equal(x1[1], x2[1], tolerance = eps)

    ## Comparison of the results of numeric object 'x1' with expected results
    expect_equal(x1[1], 0.94042701, tolerance = 1e-07, label = paste0(x1[1]))

    ## Comparison of the results of numeric object 'x2' with expected results
    expect_equal(x2[1], 0.94042718, tolerance = 1e-07, label = paste0(x2[1]))
})
