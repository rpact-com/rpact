## |
## |  *Analysis of general estimates*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Licensed under "GNU Lesser General Public License" version 3
## |

test_that("getDataset creates a DatasetGeneral object", {
    dataset <- getDataset(
        df = c(21.646, 28.5),
        est = c(-0.20842, -0.1),
        se = c(0.13961, 0.12)
    )

    expect_s3_class(dataset, "DatasetGeneral")
    expect_s3_class(dataset, "Dataset")
    expect_false(inherits(dataset, "DatasetMeans"))
    expect_true(dataset$isDatasetGeneral())
    expect_false(dataset$isDatasetMeans())
    expect_equal(dataset$degreesOfFreedom, c(21.646, 28.5))
    expect_equal(dataset$estimates, c(-0.20842, -0.1))
    expect_equal(dataset$standardErrors, c(0.13961, 0.12))
    expect_null(dataset$sampleSizes)
    expect_null(dataset$means)
    expect_null(dataset$stDevs)
    expect_equal(dataset$getEstimates(), dataset$estimates)
    expect_equal(dataset$getStandardErrors(), dataset$standardErrors)
    expect_equal(dataset$getDegreesOfFreedom(), dataset$degreesOfFreedom)

    output <- capture.output(print(dataset))
    expect_true(any(grepl("Dataset of general estimates", output, fixed = TRUE)))
    expect_true(any(grepl("Degrees of freedom", output, fixed = TRUE)))
    expect_true(any(grepl("Standard errors", output, fixed = TRUE)))
    expect_false(any(grepl("Calculated data", output, fixed = TRUE)))

    recreatedDataset <- eval(parse(text = paste(rcmd(dataset), collapse = "\n")))
    expect_s3_class(recreatedDataset, "DatasetGeneral")
    expect_equal(recreatedDataset$degreesOfFreedom, dataset$degreesOfFreedom)
    expect_equal(recreatedDataset$estimates, dataset$estimates)
    expect_equal(recreatedDataset$standardErrors, dataset$standardErrors)
})

test_that("general estimates are analyzed directly without rounding df", {
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
    expect_s3_class(result$.stageResults, "StageResultsGeneral")
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

test_that("general cumulative results use endpoint-independent inverse-variance calculations", {
    design <- getDesignInverseNormal(kMax = 2, sided = 1)
    dataset <- getDataset(
        df = c(10, 20),
        est = c(0.2, 0.4),
        se = c(0.1, 0.2)
    )

    stageResults <- getStageResults(
        design = design,
        dataInput = dataset,
        stage = 2,
        directionUpper = TRUE
    )

    information <- c(100, 25)
    expectedOverallEstimate <- c(0.2, sum(information * c(0.2, 0.4)) / sum(information))
    expectedOverallStandardError <- 1 / sqrt(cumsum(information))
    expectedOverallDf <- cumsum(information)^2 / cumsum(information^2 / c(10, 20))

    expect_s3_class(stageResults, "StageResultsGeneral")
    expect_false(inherits(stageResults, "StageResultsMeans"))
    expect_equal(stageResults$effectSizes, expectedOverallEstimate)
    expect_equal(stageResults$testStatistics, c(2, 2))
    expect_equal(stageResults$overallEstimates, expectedOverallEstimate)
    expect_equal(stageResults$overallStandardErrors, expectedOverallStandardError)
    expect_equal(stageResults$overallDegreesOfFreedom, expectedOverallDf)
    observedInformation <- suppressMessages(getObservedInformationRates(
        dataset,
        maxInformation = sum(information)
    ))
    expect_equal(observedInformation$absoluteInformations, cumsum(information))
    expect_equal(observedInformation$informationRates[1:2], c(0.8, 1))
    expect_null(dataset$means)
    expect_null(dataset$sampleSizes)
    expect_null(dataset$stDevs)
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
