## |
## |  *Analysis of general estimates*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Licensed under "GNU Lesser General Public License" version 3
## |

test_that("getDataset creates a DatasetEstimates object", {
    dataset <- getDataset(
        df = c(21.646, 28.5),
        est = c(-0.20842, -0.1),
        se = c(0.13961, 0.12)
    )

    expect_s3_class(dataset, "DatasetEstimates")
    expect_s3_class(dataset, "DatasetMeans")
    expect_equal(dataset$degreesOfFreedom, c(21.646, 28.5))
    expect_equal(dataset$estimates, c(-0.20842, -0.1))
    expect_equal(dataset$standardErrors, c(0.13961, 0.12))
    expect_equal(dataset$sampleSizes, dataset$degreesOfFreedom + 1)
    expect_equal(dataset$means, dataset$estimates)
    expect_equal(
        dataset$stDevs,
        dataset$standardErrors * sqrt(dataset$degreesOfFreedom + 1)
    )

    output <- capture.output(print(dataset))
    expect_true(any(grepl("Dataset of estimates", output, fixed = TRUE)))
    expect_true(any(grepl("Degrees of freedom", output, fixed = TRUE)))
    expect_true(any(grepl("Standard errors", output, fixed = TRUE)))
    expect_false(any(grepl("Calculated data", output, fixed = TRUE)))

    recreatedDataset <- eval(parse(text = paste(rcmd(dataset), collapse = "\n")))
    expect_s3_class(recreatedDataset, "DatasetEstimates")
    expect_equal(recreatedDataset$degreesOfFreedom, dataset$degreesOfFreedom)
    expect_equal(recreatedDataset$estimates, dataset$estimates)
    expect_equal(recreatedDataset$standardErrors, dataset$standardErrors)
})

test_that("general estimates use the means analysis calculations without rounding df", {
    design <- getDesignInverseNormal(
        typeOfDesign = "OF",
        kMax = 2,
        informationRates = c(0.7, 1),
        sided = 1,
        alpha = 0.0125,
        beta = 0.1,
        futilityBounds = 0
    )
    dataset <- getDataset(df = 21.646, est = -0.20842, se = 0.13961)

    result <- getAnalysisResults(
        design = design,
        dataInput = dataset,
        directionUpper = FALSE,
        stage = 1
    )

    expect_s3_class(result, "AnalysisResultsInverseNormal")
    expect_equal(
        result$.stageResults$overallPValues[1],
        pt(-0.20842 / 0.13961, df = 21.646),
        tolerance = 1e-12
    )
    expect_equal(dataset$degreesOfFreedom, 21.646)

    output <- capture.output(print(result))
    expect_true(any(grepl(
        "Analysis results (general estimates, inverse normal combination test design)",
        output,
        fixed = TRUE
    )))
    expect_true(any(grepl("Estimate", output, fixed = TRUE)))
    expect_true(any(grepl("Degrees of freedom", output, fixed = TRUE)))
    expect_true(any(grepl("Standard error", output, fixed = TRUE)))
    analysisSection <- output[match("Analysis results:", output):length(output)]
    expect_false(any(grepl("Assumed standard deviation", analysisSection, fixed = TRUE)))
    expect_equal(result$getDataInput()$estimates, -0.20842)
    expect_equal(result$getDataInput()$degreesOfFreedom, 21.646)
    expect_equal(result$getDataInput()$standardErrors, 0.13961)
})

test_that("general estimate inputs are validated", {
    expect_error(
        getDataset(df = 0, est = 0.2, se = 0.1),
        "degrees of freedom must be > 0"
    )
    expect_error(
        getDataset(df = 10, est = 0.2, se = 0),
        "standard errors must be > 0"
    )
    expect_error(
        getDataset(df = 10, est = 0.2),
        "parameter 'se' is missing"
    )
})
