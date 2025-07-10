test_that("Correct results for expected second stage information", {
    designFixedDelta <- getDesignOptimalConditionalErrorFunction(
        alpha = 0.025,
        alpha1 = 0.000158,
        alpha0 = 0.5,
        conditionalPower = 0.9,
        likelihoodRatioDistribution = "fixed",
        ncp1 = sqrt(170 / 2) * 0.25,
        deltaLR = 0.25,
        firstStageInformation = 170 / 2,
        useInterimEstimate = FALSE
    )

    expectedInformationH0 <- getExpectedSecondStageInformation(
        design = designFixedDelta,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 0
    )
    expectedInformationH01 <- getExpectedSecondStageInformation(
        design = designFixedDelta,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 0.125
    )
    expectedInformationH1 <- getExpectedSecondStageInformation(
        design = designFixedDelta,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 0.25
    )

    # Skip remaining tests on CRAN
    skip_on_cran()

    expect_equal(expectedInformationH0, 95.89078, tolerance = 1e-4)
    expect_equal(expectedInformationH01, 115.9472, tolerance = 1e-4)
    expect_equal(expectedInformationH1, 55.40431, tolerance = 1e-4)

    designFixedDelta0 <- getDesignOptimalConditionalErrorFunction(
        alpha = 0.025,
        alpha1 = 0.000158,
        alpha0 = 0.5,
        conditionalPower = 0.9,
        likelihoodRatioDistribution = "fixed",
        ncp1 = sqrt(170 / 2) * 0.25,
        deltaLR = 0,
        firstStageInformation = 170 / 2,
        useInterimEstimate = FALSE
    )

    expectedInformationH0 <- getExpectedSecondStageInformation(
        design = designFixedDelta0,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 0
    )
    expectedInformationH01 <- getExpectedSecondStageInformation(
        design = designFixedDelta0,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 0.125
    )
    expectedInformationH1 <- getExpectedSecondStageInformation(
        design = designFixedDelta0,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 0.25
    )

    expect_equal(expectedInformationH0, 68.62581, tolerance = 1e-4)
    expect_equal(expectedInformationH01, 119.2098, tolerance = 1e-4)
    expect_equal(expectedInformationH1, 122.479, tolerance = 1e-4)

    designUniform <- getDesignOptimalConditionalErrorFunction(
        alpha = 0.025,
        alpha1 = 0.000158,
        alpha0 = 0.5,
        conditionalPower = 0.9,
        likelihoodRatioDistribution = "unif",
        ncp1 = sqrt(170 / 2) * 0.25,
        deltaMaxLR = 2 * sqrt(170 / 2) * 0.25,
        firstStageInformation = 170 / 2,
        useInterimEstimate = FALSE
    )

    expectedInformationH0 <- getExpectedSecondStageInformation(
        design = designUniform,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 0
    )
    expectedInformationH01 <- getExpectedSecondStageInformation(
        design = designUniform,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 0.125
    )
    expectedInformationH1 <- getExpectedSecondStageInformation(
        design = designUniform,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 0.25
    )

    expect_equal(expectedInformationH0, 81.44857, tolerance = 1e-4)
    expect_equal(expectedInformationH01, 107.1427, tolerance = 1e-4)
    expect_equal(expectedInformationH1, 58.24021, tolerance = 1e-4)

    designNormal <- getDesignOptimalConditionalErrorFunction(
        alpha = 0.025,
        alpha1 = 0.000158,
        alpha0 = 0.5,
        conditionalPower = 0.9,
        likelihoodRatioDistribution = "normal",
        deltaLR = 0.25,
        tauLR = 1.4 / sqrt(170 / 2), # tauLR standard deviation
        ncp1 = sqrt(170 / 2) * 0.25,
        firstStageInformation = 170 / 2,
        useInterimEstimate = FALSE
    )

    expectedInformationH0 <- getExpectedSecondStageInformation(
        design = designNormal,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 0
    )
    expectedInformationH01 <- getExpectedSecondStageInformation(
        design = designNormal,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 0.125
    )
    expectedInformationH1 <- getExpectedSecondStageInformation(
        design = designNormal,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 0.25
    )

    expect_equal(expectedInformationH0, 83.1371, tolerance = 1e-4)
    expect_equal(expectedInformationH01, 107.9471, tolerance = 1e-4)
    expect_equal(expectedInformationH1, 57.40248, tolerance = 1e-4)

    designExponential <- getDesignOptimalConditionalErrorFunction(
        alpha = 0.025,
        alpha1 = 0.000158,
        alpha0 = 0.5,
        conditionalPower = 0.9,
        likelihoodRatioDistribution = "exp",
        kappaLR = (1 / 2.3) / sqrt(170 / 2),
        ncp1 = sqrt(170 / 2) * 0.25,
        firstStageInformation = 170 / 2,
        useInterimEstimate = FALSE
    )

    expectedInformationH0 <- getExpectedSecondStageInformation(
        design = designExponential,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 0
    )
    expectedInformationH01 <- getExpectedSecondStageInformation(
        design = designExponential,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 0.125
    )
    expectedInformationH1 <- getExpectedSecondStageInformation(
        design = designExponential,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 0.25
    )

    expect_equal(expectedInformationH0, 77.32785, tolerance = 1e-4)
    expect_equal(expectedInformationH01, 105.2796, tolerance = 1e-4)
    expect_equal(expectedInformationH1, 61.74755, tolerance = 1e-4)

    designMaxlr <- getDesignOptimalConditionalErrorFunction(
        alpha = 0.025,
        alpha1 = 0.000158,
        alpha0 = 0.5,
        conditionalPower = 0.9,
        likelihoodRatioDistribution = "maxlr",
        deltaLR = sqrt(170 / 2) * 0.25,
        ncp1 = sqrt(170 / 2) * 0.25,
        firstStageInformation = 170 / 2,
        useInterimEstimate = FALSE
    )

    expectedInformationH0 <- getExpectedSecondStageInformation(
        design = designMaxlr,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 0
    )
    expectedInformationH01 <- getExpectedSecondStageInformation(
        design = designMaxlr,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 0.125
    )
    expectedInformationH1 <- getExpectedSecondStageInformation(
        design = designMaxlr,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 0.25
    )

    expect_equal(expectedInformationH0, 78.01145, tolerance = 1e-4)
    expect_equal(expectedInformationH01, 106.3943, tolerance = 1e-4)
    expect_equal(expectedInformationH1, 60.6867, tolerance = 1e-4)
})
