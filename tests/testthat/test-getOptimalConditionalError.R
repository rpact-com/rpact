test_that("Correct result for optimal conditional error function", {
    load("testdata/testdataOptimalConditionalError.RData")
    # Comparison conditional error functions - fixed delta
    designFixedDelta <- getDesignOptimalConditionalErrorFunction(
        alpha = 0.025,
        alpha1 = 0.000158,
        alpha0 = 0.5,
        conditionalPower = 0.8,
        likelihoodRatioDistribution = "fixed",
        ncp1 = 1,
        deltaLR = 1 / sqrt(170 / 2),
        firstStageInformation = 170 / 2,
        useInterimEstimate = FALSE
    )
    conditionalError <- getOptimalConditionalError(
        testdata$firstStagePValue[-1],
        designFixedDelta
    )
    expect_equal(conditionalError, testdata$fixed[-1], tolerance = 1e-4)

    # Skip remaining tests on CRAN
    skip_on_cran()

    # Comparison conditional error functions - maxlr

    designMaxlr <- getDesignOptimalConditionalErrorFunction(
        alpha = 0.025,
        alpha1 = 0.000158,
        alpha0 = 0.5,
        conditionalPower = 0.8,
        likelihoodRatioDistribution = "maxlr",
        ncp1 = 1,
        deltaLR = 1 / sqrt(170 / 2),
        firstStageInformation = 170 / 2,
        useInterimEstimate = FALSE
    )
    conditionalError <- getOptimalConditionalError(testdata$firstStagePValue[-1], designMaxlr)
    expect_equal(conditionalError, testdata$maxlr[-1], tolerance = 1e-4)

    # Comparison conditional error functions - normal

    designNormal <- getDesignOptimalConditionalErrorFunction(
        alpha = 0.025,
        alpha1 = 0.000158,
        alpha0 = 0.5,
        conditionalPower = 0.8,
        likelihoodRatioDistribution = "normal",
        ncp1 = 1,
        deltaLR = 1 / sqrt(170 / 2),
        tauLR = 0.6 / sqrt(170 / 2),
        firstStageInformation = 170 / 2,
        useInterimEstimate = FALSE
    )
    conditionalError <- getOptimalConditionalError(testdata$firstStagePValue[-1], designNormal)
    expect_equal(conditionalError, testdata$normal[-1], tolerance = 1e-4)

    # Comparison conditional error functions - exponential

    designExponential <- getDesignOptimalConditionalErrorFunction(
        alpha = 0.025,
        alpha1 = 0.000158,
        alpha0 = 0.5,
        conditionalPower = 0.8,
        likelihoodRatioDistribution = "exp",
        ncp1 = 1,
        deltaLR = 1 / sqrt(170 / 2),
        kappaLR = 1 / sqrt(170 / 2),
        firstStageInformation = 170 / 2,
        useInterimEstimate = FALSE
    )
    conditionalError <- getOptimalConditionalError(
        testdata$firstStagePValue[-1],
        designExponential
    )
    expect_equal(conditionalError, testdata$exponential[-1], tolerance = 1e-4)

    # Comparison conditional error functions - uniform

    designUniform <- getDesignOptimalConditionalErrorFunction(
        alpha = 0.025,
        alpha1 = 0.000158,
        alpha0 = 0.5,
        conditionalPower = 0.8,
        likelihoodRatioDistribution = "unif",
        ncp1 = 1,
        deltaLR = 1 / sqrt(170 / 2),
        deltaMaxLR = 2 * 1 / sqrt(170 / 2),
        firstStageInformation = 170 / 2,
        useInterimEstimate = FALSE
    )
    conditionalError <- getOptimalConditionalError(testdata$firstStagePValue[-1], designUniform)
    expect_equal(conditionalError, testdata$uniform[-1], tolerance = 1e-4)

    # Comparison conditional error functions - maxlr with interim estimate

    designMaxlrInterim <- getDesignOptimalConditionalErrorFunction(
        alpha = 0.025,
        alpha1 = 0.000158,
        alpha0 = 0.5,
        conditionalPower = 0.8,
        likelihoodRatioDistribution = "maxlr",
        firstStageInformation = 1,
        delta1Min = (qnorm(0.8) - qnorm(0.025)) / 4,
        useInterimEstimate = TRUE
    )
    conditionalError <- getOptimalConditionalError(
        testdata$firstStagePValue[-1],
        designMaxlrInterim
    )
    expect_equal(conditionalError, testdata$maxlrInterim[-1], tolerance = 1e-4)

    # Comparison conditional error functions - maxlr with constraints

    C_min <- pnorm(qnorm(0.9) - sqrt(2) * 2.3)
    C_max <- pnorm(qnorm(0.9) - sqrt(0.5) * 2.3)

    designMaxlrConstraints <- getDesignOptimalConditionalErrorFunction(
        alpha = 0.025,
        alpha1 = 0.000158,
        alpha0 = 0.5,
        conditionalPower = 0.9,
        likelihoodRatioDistribution = "maxlr",
        ncp1 = 1,
        firstStageInformation = 1,
        useInterimEstimate = FALSE,
        minimumConditionalError = C_min,
        maximumConditionalError = C_max
    )
    conditionalError <- getOptimalConditionalError(
        testdata$firstStagePValue[-1],
        designMaxlrConstraints
    )
    expect_equal(conditionalError, testdata$maxlrConstraints[-1], tolerance = 1e-4)

    # Comparison conditional error functions - uniform with constraints

    designUniformConstraints <- getDesignOptimalConditionalErrorFunction(
        alpha = 0.025,
        alpha1 = 0.000158,
        alpha0 = 0.5,
        conditionalPower = 0.9,
        likelihoodRatioDistribution = "unif",
        ncp1 = 1,
        deltaMaxLR = 2 / sqrt(170 / 2),
        firstStageInformation = 170 / 2,
        useInterimEstimate = FALSE,
        minimumConditionalError = C_min,
        maximumConditionalError = C_max
    )
    conditionalError <- getOptimalConditionalError(
        testdata$firstStagePValue[-1],
        designUniformConstraints
    )
    expect_equal(conditionalError, testdata$uniformConstraints[-1], tolerance = 1e-4)
})
