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


test_that("Run just one basic rpact test", {
    design <- getDesignGroupSequential()
    expect_equal(design$alphaSpent, c(0.00025917372, 0.0071600594, 0.02499999), tolerance = 1e-07)
    expect_equal(design$criticalValues, c(3.4710914, 2.4544323, 2.0040356), tolerance = 1e-07)
    expect_equal(design$stageLevels, c(0.00025917372, 0.0070553616, 0.022533125), tolerance = 1e-07)
})

test_that("'.arrayToString' optionally compacts consecutive integers", {
    expect_equal(
        rpact:::.arrayToString(c(1:9, "all"), mode = "or"),
        "1, 2, 3, 4, 5, 6, 7, 8, 9, or all"
    )
    expect_equal(
        rpact:::.arrayToString(c(1:9, "all"), mode = "or", compactEnabled = TRUE),
        '1, 2, ..., 9, or "all"'
    )
    expect_equal(
        rpact:::.arrayToString(c("first", 3:7, 10:13, "last"), compactEnabled = TRUE),
        '"first", 3, 4, ..., 7, 10, 11, 12, 13, "last"'
    )
    expect_equal(rpact:::.arrayToString(1:4, compactEnabled = TRUE), "1, 2, 3, 4")
    expect_equal(rpact:::.arrayToString(1:5, compactEnabled = TRUE), "1, 2, ..., 5")
    expect_equal(
        rpact:::.arrayToString(c(1:5, 7.5), compactEnabled = TRUE),
        '1, 2, ..., 5, "7.5"'
    )
})

test_that("rpact unit test information", {
    cat("\n\n")
    cat("
            RRRRRRR    PPPPPPP    AAAAAAA   CCCCCCC   TTTTTTTT
            R      R   P      P   A      A  C              TT  
            RRRRRRR    PPPPPPP    AAAAAAAA  C              TT  
            R    R     P          A      A  C              TT  
            R     R    P          A      A   CCCCCCC       TT  
            ")
    cat("\n\n")
    message("NOTE: The full set of unit tests for rpact is now stored in a private repository.")
    message("Only members of the 'RPACT User Group' have access to the tests.")
    message("For more information, please visit: www.rpact.org/iq")
    expect_true(TRUE)
})
