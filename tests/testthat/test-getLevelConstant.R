test_that("Simple level constant correct", {
  # 1)---
  # Design without interim estimate and with early stopping rules
  design <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5,
    conditionalPower = 0.9, delta1 = 0.5, useInterimEstimate = FALSE,
    firstStageInformation = 4, likelihoodRatioDistribution = "maxlr",
    enforceMonotonicity = TRUE
  )

  # This is the type I error at the second stage
  secondStageIntegral <- integrate(
    f = getOptimalConditionalError, lower = 0.001, upper = 0.5,
    design
  )$value

  # The type I error at the second stage must be alpha-alpha1
  expect_equal(secondStageIntegral, 0.024, tolerance = 1e-10)

  # Skip remaining tests on CRAN
  skip_on_cran()

  # 2)---
  # Design without interim estimate and without early stopping rules
  design <- getDesignOptimalConditionalErrorFunction(
    alpha = 0.05, alpha1 = 0, alpha0 = 1,
    conditionalPower = 0.9, delta1 = 0.5, useInterimEstimate = FALSE,
    firstStageInformation = 4, likelihoodRatioDistribution = "maxlr",
    enforceMonotonicity = TRUE
  )

  # This is the type I error at the second stage
  secondStageIntegral <- integrate(
    f = getOptimalConditionalError, lower = 0, upper = 1,
    design
  )$value

  # The type I error at the second stage must be alpha-alpha1
  expect_equal(secondStageIntegral, 0.05, tolerance = 1e-10)

  # 3)---
  # A design WITH interim estimate, but without monotonicity (uses normal integration routine)
  # This is expected to cause a warning that is not informative in this case and thus suppressed
  design <- suppressWarnings(getDesignOptimalConditionalErrorFunction(
    alpha = 0.025, alpha1 = 0.001, alpha0 = 0.5,
    conditionalPower = 0.9, useInterimEstimate = TRUE,
    delta1Min = 0.2, delta1Max = 0.4,
    firstStageInformation = 4, likelihoodRatioDistribution = "maxlr",
    enforceMonotonicity = FALSE
  ))

  # This is the type I error at the second stage
  secondStageIntegral <- integrate(
    f = getOptimalConditionalError, lower = 0.001, upper = 0.5,
    design
  )$value

  # The type I error at the second stage must be alpha-alpha1
  expect_equal(secondStageIntegral, 0.024, tolerance = 1e-10)
})
