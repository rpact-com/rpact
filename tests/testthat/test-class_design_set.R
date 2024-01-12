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
## |  File name: test-class_design_set.R
## |  Creation date: 08 January 2024, 11:50:12
## |  File version: $Revision: 7540 $
## |  Last changed: $Date: 2024-01-08 16:06:46 +0100 (Mo, 08 Jan 2024) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing Class 'TrialDesignSet'")


test_that("Testing '.getHarmonizedColumnNames'", {
    df1 <- data.frame(
        x = 4:6,
        y = 1:3,
        z = rep(1, 3)
    )
    colnames(df1) <- c("x", "Futility bound (non-binding)", "z")
    df2 <- data.frame(
        x = 4:6,
        y = 1:3,
        z = rep(1, 3)
    )
    colnames(df2) <- c("x", "Futility bound (binding)", "z")

    expect_equal(.getHarmonizedColumnNames(df1, df2), c("x", "Futility bound", "z"))
})
