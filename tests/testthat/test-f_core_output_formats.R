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
## |  File name: test-f_core_output_formats.R
## |  Creation date: 06 February 2023, 12:11:55
## |  File version: $Revision: 7132 $
## |  Last changed: $Date: 2023-06-26 14:15:08 +0200 (Mon, 26 Jun 2023) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing the Output Format Functions")


test_that("'.formatPValues'", {
    # @refFS[Sec.]{fs:sec:outputFormats}
    # @refFS[Tab.]{fs:tab:outputFormats}
    x <- .formatPValues(0.0000234)

    ## Comparison of the results of character object 'x' with expected results
    expect_equal(x, "<0.0001")

    x <- .formatPValues(c(0.0000234, 0.0000134, 0.1234))

    ## Comparison of the results of character object 'x' with expected results
    expect_equal(x, c("<0.0001", "<0.0001", "0.1234"))

    x <- .formatPValues(c(0.0002345678, 0.0000134, 0.1234, 0.000000000001, .00000009999))

    ## Comparison of the results of character object 'x' with expected results
    expect_equal(x, c("0.0002346", "0.0000134", "0.1234000", "<0.000001", "<0.000001"))

    x <- .formatPValues(c(0.00234, 0.000013, 0.1234, 0.000000000001, .00000009999))

    ## Comparison of the results of character object 'x' with expected results
    expect_equal(x, c("0.00234", "<0.0001", "0.12340", "<0.0001", "<0.0001"))

    x <- .formatPValues(c(6.244e-05, 4.906e-02, 1.446e-02, NA_real_))

    ## Comparison of the results of character object 'x' with expected results
    expect_equal(x, c("<0.0001", "0.04906", "0.01446", "NA"))

    x <- .formatPValues(c(6.24408201934656e-05, 7.55449751868031e-05, 1.23207030919836e-05, NA_real_))

    ## Comparison of the results of character object 'x' with expected results
    expect_equal(x, c("<0.0001", "<0.0001", "<0.0001", "NA"))
})

test_that("'.formatRepeatedPValues'", {
    # @refFS[Sec.]{fs:sec:outputFormats}
    # @refFS[Tab.]{fs:tab:outputFormats}
    x <- .formatRepeatedPValues(c(0.0000234, 0.0000134, 0.1234))

    ## Comparison of the results of character object 'x' with expected results
    expect_equal(x, c("<0.0001", "<0.0001", "0.1234"))

    x <- .formatRepeatedPValues(c(0.0000234, 0.0000134, 0.5234))

    ## Comparison of the results of character object 'x' with expected results
    expect_equal(x, c("<0.0001", "<0.0001", ">0.5"))

    x <- .formatRepeatedPValues(c(0.0000234, 0.0000134, 0.5234, NA_real_))

    ## Comparison of the results of character object 'x' with expected results
    expect_equal(x, c("<0.0001", "<0.0001", ">0.5", "NA"))
})

test_that("'.formatConditionalPower'", {
    # @refFS[Sec.]{fs:sec:outputFormats}
    # @refFS[Tab.]{fs:tab:outputFormats}
    x <- .formatConditionalPower(c(0.0000234, 0.0000134, 0.5234, NA_real_))

    ## Comparison of the results of character object 'x' with expected results
    expect_equal(x, c("0", "0", "0.5234", "NA"))

    x <- .formatConditionalPower(c(0.234, 0.123456, 0.6, 0.000001))

    ## Comparison of the results of character object 'x' with expected results
    expect_equal(x, c("0.2340", "0.1235", "0.6000", "0"))
})

test_that("'.formatProbabilities'", {
    # @refFS[Sec.]{fs:sec:outputFormats}
    # @refFS[Tab.]{fs:tab:outputFormats}
    x <- .formatProbabilities(c(NA_real_, NA_real_, 0.4536623, 0.7713048))

    ## Comparison of the results of character object 'x' with expected results
    expect_equal(x, c("NA", "NA", "0.4537", "0.7713"))
})

test_that("'.getDecimalPlaces'", {
    # @refFS[Sec.]{fs:sec:outputFormats}
    # @refFS[Tab.]{fs:tab:outputFormats}
    x <- .getDecimalPlaces(NA)

    ## Comparison of the results of integer object 'x' with expected results
    expect_equal(x, 0)

    x <- .getDecimalPlaces(12.123)

    ## Comparison of the results of integer object 'x' with expected results
    expect_equal(x, 3)

    x <- .getDecimalPlaces(c(6.661338e-16, 8.000000e-01, NA_real_))

    ## Comparison of the results of integer object 'x' with expected results
    expect_equal(x, c(15, 1, 0))

    x <- .getDecimalPlaces(c(6.661338e-16, 8.12300000e-02))

    ## Comparison of the results of integer object 'x' with expected results
    expect_equal(x, c(15, 5))
})

test_that("Internal output format functions throw errors when arguments are missing or wrong", {
    expect_equal(.getFormattedValue(), "NA")
    expect_error(.assertIsValitOutputFormatOptionValue())
    expect_error(.getOutputFormatOptions())
    expect_error(.getOptionBasedFormattedValue())
    expect_no_error(getOutputFormat())
    expect_no_error(.getOutputFormat())
    expect_error(.addFieldsToOutputFormatList())
    expect_error(.getOutputFormatParameterNames())
    expect_error(.getOutputFormatFunctionName())
    expect_null(.getOutputFormatKeyByFieldName("xxx"))
    expect_error(.getOutputFormatKeyByFunctionName())
})

test_that(".assertIsValidOutputFormatOptionValue handles valid option value", {
    # Valid option value
    optionKey <- "exampleKey"
    optionValue <- "roundFunction = ceiling"

    # Call the function being tested
    result <- .assertIsValidOutputFormatOptionValue(optionKey, optionValue)

    # Expect no error or exception
    expect_null(result)
})

test_that(".assertIsValidOutputFormatOptionValue handles invalid empty option value", {
    # Invalid empty option value
    optionKey <- "exampleKey"
    optionValue <- ""

    # Call the function being tested
    result <- capture_output(.assertIsValidOutputFormatOptionValue(optionKey, optionValue))

    # Expect an error message
    expect_match(result, "")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.p.value'", {
    key <- "rpact.output.format.p.value"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatPValues")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.repeated.p.value'", {
    key <- "rpact.output.format.repeated.p.value"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatRepeatedPValues")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.probability'", {
    key <- "rpact.output.format.probability"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatProbabilities")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.futility.probability'", {
    key <- "rpact.output.format.futility.probability"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatFutilityProbabilities")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.sample.size'", {
    key <- "rpact.output.format.sample.size"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatSampleSizes")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.event'", {
    key <- "rpact.output.format.event"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatEvents")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.event.time'", {
    key <- "rpact.output.format.event.time"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatEventTime")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.conditional.power'", {
    key <- "rpact.output.format.conditional.power"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatConditionalPower")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.critical.value'", {
    key <- "rpact.output.format.critical.value"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatCriticalValues")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.critical.value.fisher'", {
    key <- "rpact.output.format.critical.value.fisher"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatCriticalValuesFisher")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.test.statistic.fisher'", {
    key <- "rpact.output.format.test.statistic.fisher"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatTestStatisticsFisher")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.test.statistic'", {
    key <- "rpact.output.format.test.statistic"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatTestStatistics")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.rate'", {
    key <- "rpact.output.format.rate"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatRates")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.rate1'", {
    key <- "rpact.output.format.rate1"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatRatesDynamic")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.accrual.intensity'", {
    key <- "rpact.output.format.accrual.intensity"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatAccrualIntensities")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.mean'", {
    key <- "rpact.output.format.mean"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatMeans")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.ratio'", {
    key <- "rpact.output.format.ratio"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatRatios")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.st.dev'", {
    key <- "rpact.output.format.st.dev"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatStDevs")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.duration'", {
    key <- "rpact.output.format.duration"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatDurations")
})

test_that(".getOutputFormatFunctionName returns correct function name for key 'rpact.output.format.time'", {
    key <- "rpact.output.format.time"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect the correct function name
    expect_equal(result, ".formatTime")
})

test_that(".getOutputFormatFunctionName returns NULL for unknown key", {
    key <- "unknown.key"

    # Call the function being tested
    result <- .getOutputFormatFunctionName(key)

    # Expect NULL as the result
    expect_null(result)
})

test_that(".getOptionBasedFormattedValue returns NULL for unknown option key", {
    optionKey <- "unknown.key"
    value <- 0.123

    # Call the function being tested
    result <- .getOptionBasedFormattedValue(optionKey, value)

    # Expect NULL as the result
    expect_null(result)
})
