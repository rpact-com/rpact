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
## |  File name: test-f_quality_assurance.R
## |  Creation date: 23 May 2024, 11:59:52
## |  File version: $Revision: 7928 $
## |  Last changed: $Date: 2024-05-23 16:35:16 +0200 (Do, 23 Mai 2024) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing the Get Object R Code Function")


test_that("'getObjectRCode': varied input arguments", {
    .skipTestIfDisabled()

    obj <- getSampleSizeMeans(getDesignGroupSequential())

    result1 <- getObjectRCode(obj, stringWrapParagraphWidth = 20, stringWrapPrefix = "    ", pipeOperator = "R")

    ## Comparison of the results of character object 'result1' with expected results
    expect_equal(result1, c("getDesignGroupSequential() |> ", "getSampleSizeMeans()"), label = paste0(result1))

    result2 <- getObjectRCode(obj, output = "markdown", pipeOperator = "R")

    ## Comparison of the results of character object 'result2' with expected results
    expect_equal(result2, "getDesignGroupSequential() |>
    getSampleSizeMeans()", label = paste0(result2))

    result3 <- getObjectRCode(obj, explicitPrint = TRUE, pipeOperator = "R")

    ## Comparison of the results of character object 'result3' with expected results
    expect_equal(result3, c("getDesignGroupSequential() |>", "getSampleSizeMeans() |>", "print()"), label = paste0(result3))

    result4 <- getObjectRCode(obj, postfix = c("|>", "print()"), pipeOperator = "R")

    ## Comparison of the results of character object 'result4' with expected results
    expect_equal(result4, c("getDesignGroupSequential() |>", "getSampleSizeMeans()|>", "print()"), label = paste0(result4))

    result5 <- getObjectRCode(obj, leadingArguments = "", pipeOperator = "R")

    ## Comparison of the results of character object 'result5' with expected results
    expect_equal(result5, c("getDesignGroupSequential() |>", "getSampleSizeMeans()"), label = paste0(result5))

    result6 <- getObjectRCode(summary(obj), pipeOperator = "R")

    ## Comparison of the results of character object 'result6' with expected results
    expect_equal(result6, c("getDesignGroupSequential() |>", "getSampleSizeMeans() |>", "summary()"), label = paste0(result6))

    result7 <- getObjectRCode(obj, pipeOperator = "none")

    ## Comparison of the results of character object 'result7' with expected results
    expect_equal(result7, c("design <- getDesignGroupSequential()", "getSampleSizeMeans(design = design)"), label = paste0(result7))

    result8 <- getObjectRCode(obj, pipeOperator = "magrittr")

    ## Comparison of the results of character object 'result8' with expected results
    expect_equal(result8, c("getDesignGroupSequential() %>%", "getSampleSizeMeans()"), label = paste0(result8))

    expect_output(print(getObjectRCode(obj)))

    expect_output(getObjectRCode(obj, output = "cat", pipeOperator = "R"))

    suppressMessages(expect_output(print(getObjectRCode(obj, output = "test", pipeOperator = "R"))))
})
