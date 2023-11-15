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
## |  File name: test-f_analysis_utilities.R
## |  Creation date: 08 November 2023, 09:09:34
## |  File version: $Revision: 7411 $
## |  Last changed: $Date: 2023-11-09 14:47:29 +0100 (Do, 09 Nov 2023) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing the Function Get Observed Information Rates")


test_that("'getObservedInformationRates': final-stage", {
    data1 <- getDataset(overallN = c(22, 45), overallEvents = c(11, 28))
    # @refFS[Formula]{fs:getObservedInformationRates}
    # @refFS[Formula]{fs:getObservedInformationRates:finalStageReached}
    result1 <- getObservedInformationRates(data1, maxInformation = 45)

    ## Comparison of the results of list object 'result1' with expected results
    expect_equal(result1$absoluteInformations, c(22, 45), label = paste0("c(", paste0(result1$absoluteInformations, collapse = ", "), ")"))
    expect_equal(result1$maxInformation, 45, label = paste0("c(", paste0(result1$maxInformation, collapse = ", "), ")"))
    expect_equal(result1$informationRates, c(0.48888889, 1), tolerance = 1e-07, label = paste0("c(", paste0(result1$informationRates, collapse = ", "), ")"))
    expect_equal(result1$status, "final-stage", label = paste0("c(", paste0(result1$status, collapse = ", "), ")"))
})

test_that("'getObservedInformationRates': over-running", {
    data2 <- getDataset(overallN = c(22, 45), overallEvents = c(11, 28))
    # @refFS[Formula]{fs:getObservedInformationRates}
    # @refFS[Formula]{fs:getObservedInformationRates:overRunning}
    result2 <- getObservedInformationRates(data2, maxInformation = 44)

    ## Comparison of the results of list object 'result2' with expected results
    expect_equal(result2$absoluteInformations, c(22, 45), label = paste0("c(", paste0(result2$absoluteInformations, collapse = ", "), ")"))
    expect_equal(result2$maxInformation, 45, label = paste0("c(", paste0(result2$maxInformation, collapse = ", "), ")"))
    expect_equal(result2$informationRates, c(0.48888889, 1), tolerance = 1e-07, label = paste0("c(", paste0(result2$informationRates, collapse = ", "), ")"))
    expect_equal(result2$status, "over-running", label = paste0("c(", paste0(result2$status, collapse = ", "), ")"))
})

test_that("'getObservedInformationRates': interim-stage", {
    data3 <- getDataset(overallN = c(22, 45), overallEvents = c(11, 28))
    # @refFS[Formula]{fs:getObservedInformationRates}
    # @refFS[Formula]{fs:getObservedInformationRates:interimStage}
    result3 <- getObservedInformationRates(data3, maxInformation = 46)

    ## Comparison of the results of list object 'result3' with expected results
    expect_equal(result3$absoluteInformations, c(22, 45), label = paste0("c(", paste0(result3$absoluteInformations, collapse = ", "), ")"))
    expect_equal(result3$maxInformation, 46, label = paste0("c(", paste0(result3$maxInformation, collapse = ", "), ")"))
    expect_equal(result3$informationRates, c(0.47826087, 0.97826087, 1), tolerance = 1e-07, label = paste0("c(", paste0(result3$informationRates, collapse = ", "), ")"))
    expect_equal(result3$status, "interim-stage", label = paste0("c(", paste0(result3$status, collapse = ", "), ")"))
})

test_that("'getObservedInformationRates': under-running with absolute information epsilon", {
    data4 <- getDataset(overallN = c(22, 45), overallEvents = c(11, 28))
    # @refFS[Formula]{fs:getObservedInformationRates}
    # @refFS[Formula]{fs:getObservedInformationRates:underRunning}
    result4 <- getObservedInformationRates(data4, maxInformation = 46, informationEpsilon = 1)

    ## Comparison of the results of list object 'result4' with expected results
    expect_equal(result4$absoluteInformations, c(22, 45), label = paste0("c(", paste0(result4$absoluteInformations, collapse = ", "), ")"))
    expect_equal(result4$maxInformation, 45, label = paste0("c(", paste0(result4$maxInformation, collapse = ", "), ")"))
    expect_equal(result4$informationEpsilon, 1, label = paste0("c(", paste0(result4$informationEpsilon, collapse = ", "), ")"))
    expect_equal(result4$informationRates, c(0.48888889, 1), tolerance = 1e-07, label = paste0("c(", paste0(result4$informationRates, collapse = ", "), ")"))
    expect_equal(result4$status, "under-running", label = paste0("c(", paste0(result4$status, collapse = ", "), ")"))
})

test_that("'getObservedInformationRates': under-running with relative information epsilon", {
    data5 <- getDataset(overallN = c(22, 45), overallEvents = c(11, 28))
    # @refFS[Formula]{fs:getObservedInformationRates}
    # @refFS[Formula]{fs:getObservedInformationRates:underRunningRelative}
    result5 <- getObservedInformationRates(data5, maxInformation = 46, informationEpsilon = 0.03)

    ## Comparison of the results of list object 'result5' with expected results
    expect_equal(result5$absoluteInformations, c(22, 45), label = paste0("c(", paste0(result5$absoluteInformations, collapse = ", "), ")"))
    expect_equal(result5$maxInformation, 45, label = paste0("c(", paste0(result5$maxInformation, collapse = ", "), ")"))
    expect_equal(result5$informationEpsilon, 0.03, tolerance = 1e-07, label = paste0("c(", paste0(result5$informationEpsilon, collapse = ", "), ")"))
    expect_equal(result5$informationRates, c(0.48888889, 1), tolerance = 1e-07, label = paste0("c(", paste0(result5$informationRates, collapse = ", "), ")"))
    expect_equal(result5$status, "under-running", label = paste0("c(", paste0(result5$status, collapse = ", "), ")"))
})

