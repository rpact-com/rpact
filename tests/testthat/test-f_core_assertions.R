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
## |  File name: test-f_core_assertions.R
## |  Creation date: 06 February 2023, 12:11:54
## |  File version: $Revision: 6801 $
## |  Last changed: $Date: 2023-02-06 15:29:57 +0100 (Mon, 06 Feb 2023) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing Assertion Functions")


test_that("Testing '.assertIsInClosedInterval'", {
	invisible(capture.output(expect_error(.assertIsInClosedInterval(x = 0, xName = "x", lower = 0, upper = 1, naAllowed = FALSE), NA)))
	invisible(capture.output(expect_error(.assertIsInClosedInterval(x = 1, xName = "x", lower = 0, upper = 1, naAllowed = FALSE), NA)))
	invisible(capture.output(expect_error(.assertIsInClosedInterval(x = 0.0001, xName = "x", lower = 0, upper = 1, naAllowed = FALSE), NA)))
	invisible(capture.output(expect_error(.assertIsInClosedInterval(x = 0.9999, xName = "x", lower = 0, upper = 1, naAllowed = FALSE), NA)))
	invisible(capture.output(expect_error(.assertIsInClosedInterval(x = NA_real_, xName = "x", lower = 0, upper = 1, naAllowed = FALSE), NA)))
	invisible(capture.output(expect_error(.assertIsInClosedInterval(x = NA_real_, xName = "x", lower = 0, upper = 1, naAllowed = TRUE), NA)))
	invisible(capture.output(expect_error(.assertIsInClosedInterval(x = c(1, NA_real_), xName = "x", lower = 0, upper = 1, naAllowed = TRUE), NA)))
	invisible(capture.output(expect_error(.assertIsInClosedInterval(x = c(NA_real_, 0, NA_real_), xName = "x", lower = 0, upper = 1, naAllowed = TRUE), NA)))

	expect_error(.assertIsInClosedInterval(x = -0.0001, xName = "x", lower = 0, upper = 1, naAllowed = FALSE))
	expect_error(.assertIsInClosedInterval(x = -0.0001, xName = "x", lower = 0, upper = NA_real_, naAllowed = FALSE))
	expect_error(.assertIsInClosedInterval(x = 1.0001, xName = "x", lower = 0, upper = 1, naAllowed = FALSE))
	expect_error(.assertIsInClosedInterval(x = c(1, NA_real_), xName = "x", lower = 0, upper = 1, naAllowed = FALSE))
	expect_error(.assertIsInClosedInterval(x = c(-1, NA_real_), xName = "x", lower = 0, upper = 1, naAllowed = TRUE))
	expect_error(.assertIsInClosedInterval(x = c(NA_real_, NA_real_), xName = "x", lower = 0, upper = 1, naAllowed = FALSE))
	expect_error(.assertIsInClosedInterval(x = c(0, -1, 1, 2), xName = "x", lower = 0, upper = 1, naAllowed = FALSE))

})

test_that("Testing '.assertIsInOpenInterval'", {

	invisible(capture.output(expect_error(.assertIsInOpenInterval(x = 0.0001, xName = "x", lower = 0, upper = 1, naAllowed = FALSE), NA)))
	invisible(capture.output(expect_error(.assertIsInOpenInterval(x = 0.9999, xName = "x", lower = 0, upper = 1, naAllowed = FALSE), NA)))
	invisible(capture.output(expect_error(.assertIsInOpenInterval(x = NA_real_, xName = "x", lower = 0, upper = 1, naAllowed = FALSE), NA)))
	invisible(capture.output(expect_error(.assertIsInOpenInterval(x = NA_real_, xName = "x", lower = 0, upper = 1, naAllowed = TRUE), NA)))
	invisible(capture.output(expect_error(.assertIsInOpenInterval(x = c(0.9999, NA_real_), xName = "x", lower = 0, upper = 1, naAllowed = TRUE), NA)))
	invisible(capture.output(expect_error(.assertIsInOpenInterval(x = c(NA_real_, 0.0001, NA_real_), xName = "x", lower = 0, upper = 1, naAllowed = TRUE), NA)))

	expect_error(.assertIsInOpenInterval(x = 0, xName = "x", lower = 0, upper = 1, naAllowed = FALSE))
	expect_error(.assertIsInOpenInterval(x = 0, xName = "x", lower = 0, upper = NA_real_, naAllowed = FALSE))
	expect_error(.assertIsInOpenInterval(x = 1, xName = "x", lower = 0, upper = 1, naAllowed = FALSE))
	expect_error(.assertIsInOpenInterval(x = c(1.0001, NA_real_), xName = "x", lower = 0, upper = 1, naAllowed = FALSE))
	expect_error(.assertIsInOpenInterval(x = c(-1, NA_real_), xName = "x", lower = 0, upper = 1, naAllowed = TRUE))
	expect_error(.assertIsInOpenInterval(x = c(NA_real_, NA_real_), xName = "x", lower = 0, upper = 1, naAllowed = FALSE))
	expect_error(.assertIsInOpenInterval(x = c(0, -1, 1, 2), xName = "x", lower = 0, upper = 1, naAllowed = FALSE))

})

test_that("Testing '.assertDesignParameterExists'", {

	expect_error(.assertDesignParameterExists(),
	    "Missing argument: 'design' must be defined",
	    fixed = TRUE
	)

	expect_error(.assertDesignParameterExists(design = getAssertionTestDesign()),
	    "Missing argument: 'parameterName' must be defined",
	    fixed = TRUE
	)

	expect_error(.assertDesignParameterExists(design = getAssertionTestDesign(), parameterName = "kMax"),
	    "Missing argument: 'defaultValue' must be defined",
	    fixed = TRUE
	)

	expect_error(.assertDesignParameterExists(
	    design = getAssertionTestDesign(),
	    parameterName = "kMax", defaultValue = C_KMAX_DEFAULT
	),
	"Missing argument: parameter 'kMax' must be specified in design",
	fixed = TRUE
	)

	expect_error(.assertDesignParameterExists(
	    design = getAssertionTestDesign(kMax = NA_integer_),
	    parameterName = "kMax", defaultValue = C_KMAX_DEFAULT
	),
	"Missing argument: parameter 'kMax' must be specified in design",
	fixed = TRUE
	)

})

test_that("Testing '.assertIsValidThetaRange'", {

	expect_error(.assertIsValidThetaRange(thetaRange = c()),
	    "Illegal argument: 'thetaRange' (NULL) must be a vector with two entries defining minimum and maximum or a sequence of numeric values with length > 2",
	    fixed = TRUE
	)

	expect_error(.assertIsValidThetaRange(thetaRange = c(1, -2)),
	    "Illegal argument: 'thetaRange' with length 2 must contain minimum < maximum (1 >= -2)",
	    fixed = TRUE
	)

	expect_equal(.assertIsValidThetaRange(thetaRange = c(1, 2, 3)), c(1, 2, 3))

	expect_equal(.assertIsValidThetaRange(thetaRange = c(-1, 2)), seq(-1, 2, 3 / C_THETA_RANGE_SEQUENCE_LENGTH_DEFAULT))

})

test_that("Testing '.assertIsSingleNumber'", {

	expect_error(.assertIsSingleNumber(NA, "x"),
	    "Illegal argument: 'x' (NA) must be a valid numeric value",
	    fixed = TRUE
	)

	expect_error(.assertIsSingleNumber(NULL, "x"),
	    "Missing argument: 'x' must be a valid numeric value",
	    fixed = TRUE
	)

	expect_error(.assertIsSingleNumber(c(1, 2), "x"),
	    "Illegal argument: 'x' c(1, 2) must be a single numeric value",
	    fixed = TRUE
	)

	expect_error(.assertIsSingleNumber(numeric(0), "x"),
	    "Missing argument: 'x' must be a valid numeric value",
	    fixed = TRUE
	)

})

test_that("Testing '.assertAssociatedArgumentsAreDefined'", {

	expect_error(.assertAssociatedArgumentsAreDefined(a = NA, b = 1),
	    "Missing argument: 'a' must be defined because 'b' is defined",
	    fixed = TRUE
	)

	expect_error(.assertAssociatedArgumentsAreDefined(a = NA, b = 1, c = NA),
	    "Missing argument: 'a', 'c' must be defined because 'b' is defined",
	    fixed = TRUE
	)

	expect_error(.assertAssociatedArgumentsAreDefined(a = NA, b = 1, c = 2),
	    "Missing argument: 'a' must be defined because 'b', 'c' are defined",
	    fixed = TRUE
	)

})

test_that("Testing '.associatedArgumentsAreDefined'", {

	expect_equal(.associatedArgumentsAreDefined(nPlanned = NA_real_, thetaH1 = NA_real_), FALSE)

	expect_warning(expect_equal(.associatedArgumentsAreDefined(nPlanned = NA_real_, thetaH1 = 1), FALSE),
	    "Incomplete associated arguments: 'nPlanned' should be defined because 'thetaH1' is defined",
	    fixed = TRUE
	)

	expect_equal(.associatedArgumentsAreDefined(nPlanned = 1, thetaH1 = 1), TRUE)

})

test_that("Testing '.isValidNPlanned'", {

	expect_equal(.isValidNPlanned(nPlanned = c(1, 2), kMax = 4, stage = 2), TRUE)

	expect_silent(.isValidNPlanned(nPlanned = NA_real_, kMax = 4, stage = 2))

	expect_warning(.isValidNPlanned(nPlanned = c(1), kMax = 4, stage = 2),
	    "'nPlanned' (1) will be ignored: length must be equal to 2 (kMax - stage = 4 - 2)",
	    fixed = TRUE
	)

	expect_warning(.isValidNPlanned(nPlanned = c(1, 2, 3), kMax = 4, stage = 2),
	    "'nPlanned' (1, 2, 3) will be ignored: length must be equal to 2 (kMax - stage = 4 - 2)",
	    fixed = TRUE
	)

})

test_that("Testing '.assertIsValidSummaryIntervalFormat'", {

	.assertIsValidSummaryIntervalFormat("[%s; %s]")
	.assertIsValidSummaryIntervalFormat("%s - %s")
	.assertIsValidSummaryIntervalFormat("(%s, %s)")

	expect_error(.assertIsValidSummaryIntervalFormat("[%s; %s; %s]"))
	expect_error(.assertIsValidSummaryIntervalFormat("[%s]"))
	expect_error(.assertIsValidSummaryIntervalFormat(""))
	expect_error(.assertIsValidSummaryIntervalFormat(1))

})

test_that("Testing '.assertIsSingleInteger'", {

	expect_error(.assertIsSingleInteger(NA_integer_, "x", naAllowed = FALSE))
	expect_error(.assertIsSingleInteger(-1, "x", naAllowed = FALSE))
	expect_error(.assertIsSingleInteger(-1, "x", naAllowed = FALSE, validateType = FALSE), NA)
	expect_error(.assertIsSingleInteger(NA_integer_, "x", naAllowed = TRUE), NA)
	expect_error(.assertIsSingleInteger(-1, "x", naAllowed = TRUE))
	expect_error(.assertIsSingleInteger("1", "x", naAllowed = TRUE))
	expect_error(.assertIsSingleInteger(1, "x", naAllowed = TRUE, validateType = TRUE))
	expect_error(.assertIsSingleInteger(1, "x", naAllowed = TRUE, validateType = FALSE), NA)

})

test_that("Testing '.assertIsSinglePositiveInteger'", {

	expect_error(.assertIsSinglePositiveInteger(NA_integer_, "x", naAllowed = FALSE))
	expect_error(.assertIsSinglePositiveInteger(-1, "x", naAllowed = FALSE))
	expect_error(.assertIsSinglePositiveInteger(NA_integer_, "x", naAllowed = TRUE), NA)
	expect_error(.assertIsSinglePositiveInteger(NA_real_, "x", naAllowed = TRUE))
	expect_error(.assertIsSinglePositiveInteger(-1, "x", naAllowed = TRUE))
	expect_error(.assertIsSinglePositiveInteger("1", "x", naAllowed = TRUE))
	expect_error(.assertIsSinglePositiveInteger(1, "x", naAllowed = TRUE, validateType = TRUE))
	expect_error(.assertIsSinglePositiveInteger(1, "x", naAllowed = TRUE, validateType = FALSE), NA)

})

test_that("Testing '.assertIsSingleLogical'", {

	expect_error(.assertIsSingleLogical("TRUE", "x", naAllowed = FALSE))
	expect_error(.assertIsSingleLogical("FALSE", "x", naAllowed = FALSE))
	expect_error(.assertIsSingleLogical(TRUE, "x", naAllowed = FALSE), NA)
	expect_error(.assertIsSingleLogical(FALSE, "x", naAllowed = FALSE), NA)
	expect_error(.assertIsSingleLogical(NA, "x", naAllowed = TRUE), NA)
	expect_error(.assertIsSingleLogical(NA, "x", naAllowed = FALSE))

})

test_that("Testing '.assertIsValidMatrix'", {

	expect_error(.assertIsValidMatrix(c(), "x", naAllowed = FALSE))
	expect_error(.assertIsValidMatrix(NULL, "x", naAllowed = FALSE))
	expect_error(.assertIsValidMatrix(1:3, "x", naAllowed = FALSE))
	expect_error(.assertIsValidMatrix(1:3, "x", naAllowed = TRUE))
	expect_error(.assertIsValidMatrix("a", "x", naAllowed = FALSE))
	expect_error(.assertIsValidMatrix("a", "x", naAllowed = TRUE))
	expect_error(.assertIsValidMatrix(NA, "x", naAllowed = FALSE))
	expect_error(.assertIsValidMatrix(NA, "x", naAllowed = TRUE))
})

