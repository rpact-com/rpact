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

test_plan_section("Testing Logger Functions")

test_that("Logger functions throw errors when arguments are missing or wrong", {
    currentLogLevel <- getLogLevel()
    tryCatch(
        {
            setLogLevel(C_LOG_LEVEL_TRACE)
            expect_error(.logBase())
            expect_error(.logInfo())
            expect_error(.getRuntimeString())
            expect_error(.logProgress())
            expect_no_error(setLogLevel())
            expect_no_error(resetLogLevel())
        },
        finally = function() {
            setLogLevel(currentLogLevel)
        }
    )
})
