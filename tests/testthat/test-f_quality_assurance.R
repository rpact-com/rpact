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
## |  File name: test-f_design_utilities.R
## |  Creation date: 06 February 2023, 12:13:45
## |  File version: $Revision: 7403 $
## |  Last changed: $Date: 2023-11-08 16:12:00 +0100 (Mi, 08 Nov 2023) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing Quality Assurance Functions")

test_that("Quality assurance functions throw errors when arguments are missing or wrong", {
    rVersion <- .isMinimumRVersion4()
    expect_true(rVersion)

    dummyContent <- "[ OK: 6 ]  [FAILED: 3]"
    res_1 <- .getTestthatResultLine(dummyContent)
    expect_type(res_1, "character")
    expect_equal(2 * 2, 4)

    res_2 <- .getTestthatResultNumberOfFailures(dummyContent)
    expect_type(res_2, "character")
    expect_equal(2 * 2, 4)

    res_3 <- .getTestthatResultNumberOfSkippedTests(dummyContent)
    expect_type(res_3, "character")
    expect_equal(2 * 2, 4)

    expect_error(.downloadUnitTests(
        testFileTargetDirectory = NULL,
        token = "token",
        secret = "secret",
        connectionType = "pkg"
    ))

    expect_error(.prepareUnitTestFiles())

    expect_error(.downloadUnitTestsViaHttp())

    expect_error(.downloadUnitTestsViaFtp())

    expect_error(.getConnectionArgument())

    expect_error(testPackage(NULL))

    expect_error(.testInstalledPackage(NULL))

    expect_type(.isCompleteUnitTestSetEnabled(), "logical")
})
