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
## |  File name: test-f_logger.R
## |  Creation date: 06 February 2023, 12:13:45
## |  File version: $Revision: 8087 $
## |  Last changed: $Date: 2024-08-15 16:34:30 +0200 (Do, 15 Aug 2024) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing Logger Functions")

test_that("Logger functions throw errors when arguments are missing or wrong", {
    currentLogLevel <- getLogLevel()
    tryCatch(
        {
            setLogLevel(C_LOG_LEVEL_TRACE)
            expect_error(.logBase())
            expect_error(.logInfo())
            expect_error(.getRuntimeString())
            expect_error(.logProgress(enforceLogging = TRUE))
            expect_no_error(setLogLevel())
            expect_no_error(resetLogLevel())
        },
        finally = function() {
            setLogLevel(currentLogLevel)
        }
    )
})
