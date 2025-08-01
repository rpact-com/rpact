test_that("Class objects correctly created", {
    # Create a design object using interim estimate
    designInterim <- getDesignOptimalConditionalErrorFunction(
        alpha = 0.025,
        alpha1 = 0.001,
        alpha0 = 0.5,
        conditionalPower = 0.9,
        useInterimEstimate = TRUE,
        delta1Min = 0.2,
        delta1Max = 1,
        firstStageInformation = 4,
        likelihoodRatioDistribution = "maxlr",
        enforceMonotonicity = TRUE
    )
    # Class correct?
    expect_true(is(designInterim, "R6"))
    expect_true(is(designInterim, "TrialDesignOptimalConditionalError"))

    # Fields correct?
    expect_equal(designInterim$alpha, 0.025)
    expect_equal(designInterim$alpha1, 0.001)
    expect_equal(designInterim$alpha0, 0.5)
    expect_equal(designInterim$conditionalPower, 0.9)
    expect_equal(designInterim$useInterimEstimate, TRUE)
    expect_equal(designInterim$firstStageInformation, 4)
    expect_equal(designInterim$likelihoodRatioDistribution, "maxlr")
    expect_equal(designInterim$enforceMonotonicity, TRUE)
    expect_equal(designInterim$delta1Min, 0.2)
    expect_equal(designInterim$delta1Max, 1)
    expect_equal(designInterim$ncp1Min, 0.2 * sqrt(4), tolerance = 1e-8)
    expect_equal(designInterim$ncp1Max, 1 * sqrt(4), tolerance = 1e-8)

    # Skip remaining tests on CRAN
    skip_on_cran()

    # Create a design object using a fixed effect for CP
    designFixed <- getDesignOptimalConditionalErrorFunction(
        alpha = 0.025,
        alpha1 = 0.001,
        alpha0 = 0.5,
        conditionalPower = 0.9,
        delta1 = 0.5,
        useInterimEstimate = FALSE,
        firstStageInformation = 4,
        likelihoodRatioDistribution = "fixed",
        deltaLR = 1,
        enforceMonotonicity = FALSE
    )
    # Class correct?
    expect_true(is(designFixed, "R6"))
    expect_true(is(designFixed, "TrialDesignOptimalConditionalError"))

    # Fields correct?
    expect_equal(designFixed$alpha, 0.025)
    expect_equal(designFixed$alpha1, 0.001)
    expect_equal(designFixed$alpha0, 0.5)
    expect_equal(designFixed$conditionalPower, 0.9)
    expect_equal(designFixed$delta1, 0.5)
    expect_equal(designFixed$useInterimEstimate, FALSE)
    expect_equal(designFixed$firstStageInformation, 4)
    expect_equal(designFixed$likelihoodRatioDistribution, "fixed")
    expect_equal(designFixed$deltaLR, 1)
    expect_equal(designFixed$enforceMonotonicity, FALSE)
    expect_equal(designFixed$ncp1, 0.5 * sqrt(4), tolerance = 1e-8)
})
