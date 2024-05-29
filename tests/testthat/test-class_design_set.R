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
## |  Creation date: 23 February 2024, 12:33:48
## |  File version: $Revision: 7928 $
## |  Last changed: $Date: 2024-05-23 16:35:16 +0200 (Do, 23 Mai 2024) $
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

test_that("Testing '.getHarmonizedDataFrames': small df1 vs large df2", {
    df1 <- data.frame(
        x2 = 1,
        x3 = 1,
        x5 = 1
    )

    df2 <- data.frame(
        x1 = 2,
        x2 = 2,
        x3 = 2,
        x4 = 2,
        x5 = 2
    )

    result <- .getHarmonizedDataFrames(df1, df2)
    expect_equal(result$df2, df2)
    expect_equal(result$df1, data.frame(x1 = NA, x2 = 1, x3 = 1, x4 = NA, x5 = 1))
})

test_that("Testing '.getHarmonizedDataFrames': large df1 vs small df2", {
    .skipTestIfDisabled()

    df1 <- data.frame(
        x1 = 1,
        x2 = 1,
        x3 = 1,
        x4 = 1,
        x5 = 1
    )
    df2 <- data.frame(
        x2 = 2,
        x3 = 2,
        x5 = 2
    )

    result <- .getHarmonizedDataFrames(df1, df2)
    expect_equal(result$df1, df1)
    expect_equal(result$df2, data.frame(x1 = NA, x2 = 2, x3 = 2, x4 = NA, x5 = 2))
})

test_that("Testing 'TrialDesignSet' functions, warnings, and errors", {
    .skipTestIfDisabled()

    design <- getDesignInverseNormal(
        alpha = 0.05, kMax = 4, sided = 1,
        typeOfDesign = "WT", deltaWT = 0.1
    )
    designSet <- getDesignSet(design = design, deltaWT = c(0.3, 0.4))
    designSet$addVariedParameters("deltaWT")
    expect_s3_class(plot(designSet), "gg")
    expect_warning(designSet$.validateOptionalArguments(design))
    expect_error(getDesignSet()$.validateOptionalArguments(design = NULL, alpha = 0.1))
    expect_warning(getDesignSet()$.getArgumentNames(validatedDesign = design, 1))
    expect_error(getDesignSet()$add())
    expect_error(getDesignSet()$add(x = 1))
    expect_error(getDesignSet()$assertHaveEqualSidedValues(), NA)
    expect_error(getDesignSet(designs = c(getDesignGroupSequential(sided = 1), getDesignGroupSequential(sided = 2)))$assertHaveEqualSidedValues())
    expect_true(R6::is.R6(designSet$getPlotSettings()))
})
