test_that("NuPrime correctly calculated", {
    values <- seq(0, 1, 0.001)
    mdnu <- function(u, cp) {
        -2 * (qnorm(cp) - qnorm(u)) / dnorm(qnorm(u))
    }
    expect_equal(mdnu(values, 0.8), .getNuPrime(values, conditionalPower = 0.8), tolerance = 1e-8)
    expect_equal(mdnu(values, 0.9), .getNuPrime(values, conditionalPower = 0.9), tolerance = 1e-8)
})
