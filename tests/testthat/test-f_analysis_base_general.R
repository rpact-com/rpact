## |
## |  *Unit tests*
## |
## |  This file is part of the rpact R package.
## |

test_that("Two-sided repeated confidence intervals for DatasetGeneral match normal bounds", {
    data <- getDataset(est = c(0.5, 0.8), se = c(0.3, 0.3))
    expectedEst <- c(0.5, 0.65)
    expectedSe <- c(0.3, 0.3 / sqrt(2))

    designGs <- getDesignGroupSequential(kMax = 2, sided = 2, alpha = 0.05)
    resGs <- getAnalysisResults(designGs, data)
    critGs <- designGs$criticalValues

    expect_equal(resGs$repeatedConfidenceIntervalLowerBounds, expectedEst - critGs * expectedSe, tolerance = 1e-05)
    expect_equal(resGs$repeatedConfidenceIntervalUpperBounds, expectedEst + critGs * expectedSe, tolerance = 1e-05)
    expect_equal(resGs$medianUnbiasedEstimates[2], expectedEst[2], tolerance = 1e-05)

    rciGs <- getRepeatedConfidenceIntervals(designGs, data)
    expect_equal(rciGs[1, ], expectedEst - critGs * expectedSe, tolerance = 1e-05)
    expect_equal(rciGs[2, ], expectedEst + critGs * expectedSe, tolerance = 1e-05)

    designIn <- getDesignInverseNormal(kMax = 2, sided = 2, alpha = 0.05)
    resIn <- getAnalysisResults(designIn, data)
    critIn <- designIn$criticalValues
    expect_equal(resIn$repeatedConfidenceIntervalLowerBounds, expectedEst - critIn * expectedSe, tolerance = 1e-05)
    expect_equal(resIn$repeatedConfidenceIntervalUpperBounds, expectedEst + critIn * expectedSe, tolerance = 1e-05)

    designFix <- getDesignFixed(sided = 2, alpha = 0.05)
    dataFix <- getDataset(est = 0.5, se = 0.3)
    resFix <- getAnalysisResults(designFix, dataFix)
    critFix <- designFix$criticalValues
    expect_equal(resFix$repeatedConfidenceIntervalLowerBounds, 0.5 - critFix * 0.3, tolerance = 1e-05)
    expect_equal(resFix$repeatedConfidenceIntervalUpperBounds, 0.5 + critFix * 0.3, tolerance = 1e-05)
})

test_that("One-sided DatasetGeneral analysis still honors directionUpper = FALSE", {
    design <- getDesignGroupSequential(kMax = 2, sided = 1, alpha = 0.025, directionUpper = FALSE)
    data <- getDataset(est = c(-0.5, -0.8), se = c(0.3, 0.3))
    res <- getAnalysisResults(design, data)

    expect_equal(res$repeatedConfidenceIntervalLowerBounds, c(-0.5, -0.65) - abs(design$criticalValues) * c(0.3, 0.3 / sqrt(2)), tolerance = 1e-05)
    expect_equal(res$repeatedConfidenceIntervalUpperBounds, c(-0.5, -0.65) + abs(design$criticalValues) * c(0.3, 0.3 / sqrt(2)), tolerance = 1e-05)
    expect_equal(res$testActions, c("continue", "reject"))
})
