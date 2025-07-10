test_that("SecondStageInformation correctly calculated", {
  # Without using the interim estimate

  Info_compare <- c(101.7700, 141.9709, 168.1198, 188.4785, 205.6950, 220.9849, 235.0241, 248.2418, 260.9416, 273.3614)

  design_fixed_delta <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025, alpha1 = 0.000158, alpha0 = 0.5, conditionalPower = 0.9,
    likelihoodRatioDistribution = "fixed", ncp1 = sqrt(170 / 2) * 0.25, deltaLR = 0.25,
    firstStageInformation = 170 / 2, useInterimEstimate = FALSE
  )

  Info_fixed_delta <- getSecondStageInformation(firstStagePValue = seq(0.05, 0.5, 0.05), design = design_fixed_delta)

  expect_equal(Info_compare, Info_fixed_delta, tolerance = 1e-4)

  # Skip remaining tests on cran
  skip_on_cran()

  # With using the interim estimate

  Info_compare <- c(2.327416, 3.834037, 5.861998, 8.889864, 12.836328)

  design_maxlr <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025, alpha1 = 0.000158, alpha0 = 0.5, conditionalPower = 0.8,
    likelihoodRatioDistribution = "maxlr", delta1Min = 0.7003963,
    firstStageInformation = 1, useInterimEstimate = TRUE
  )

  Info_maxlr <- getSecondStageInformation(firstStagePValue = seq(0.05, 0.25, 0.05), design = design_maxlr)

  expect_equal(Info_compare, Info_maxlr, tolerance = 1e-4)
})
