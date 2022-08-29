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
## |  File name: test-f_parameter_set_utilities.R
## |  Creation date: 12 August 2022, 09:11:30
## |  File version: $Revision: 6485 $
## |  Last changed: $Date: 2022-08-12 13:20:22 +0200 (Fr, 12 Aug 2022) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing Parameter Set Utility Functions")


test_that("'.getParameterValueFormatted' produce correct results if parameter is an array", {
	x1 <- getSimulationMultiArmMeans(getDesignInverseNormal(kMax = 2), plannedSubjects = c(30, 60), muMaxVector = 0, seed = 123, maxNumberOfIterations = 50L)
	y1 <- .getParameterValueFormatted(x1, "sampleSizes")

	expect_equal("sampleSizes", y1$paramName)
	expect_equal(c(x1$.design$kMax, length(x1$muMaxVector), x1$activeArms + 1), dim(y1$paramValue))
	expect_equal(length(as.vector(y1$paramValue)), length(y1$paramValueFormatted))
	expect_equal("character", class(y1$paramValueFormatted)[1])
	expect_equal("array", y1$type)

	x2 <- getSimulationMultiArmMeans(getDesignInverseNormal(kMax = 2), plannedSubjects = c(50, 100), muMaxVector = c(0, 1), seed = 123, maxNumberOfIterations = 50L)
	lines2a <- capture.output(print(x2))
	lines2 <- lines2a[grepl("Sample sizes ", lines2a)]
	expect_match(lines2[1], "^ *Sample sizes \\(1\\) \\[1\\] *: 50, 50 *$")
	expect_match(lines2[2], "^ *Sample sizes \\(1\\) \\[2\\] *: 17, 0 *$")
	expect_match(lines2[3], "^ *Sample sizes \\(2\\) \\[1\\] *: 50, 50 *$")
	expect_match(lines2[4], "^ *Sample sizes \\(2\\) \\[2\\] *: 17, 3.3 *$")
	expect_match(lines2[5], "^ *Sample sizes \\(3\\) \\[1\\] *: 50, 50 *$")
	expect_match(lines2[6], "^ *Sample sizes \\(3\\) \\[2\\] *: 16, 46.7 *$")
	expect_match(lines2[7], "^ *Sample sizes \\(4\\) \\[1\\] *: 50, 50 *$")
	expect_match(lines2[8], "^ *Sample sizes \\(4\\) \\[2\\] *: 50, 50 *$")

	x3 <- getSimulationMultiArmMeans(getDesignInverseNormal(kMax = 1), plannedSubjects = 50, muMaxVector = c(0, 1), seed = 123, maxNumberOfIterations = 50L)

	y3 <- .getParameterValueFormatted(x3, "sampleSizes")

	expect_equal("sampleSizes", y3$paramName)
	expect_equal(c(x3$.design$kMax, length(x3$muMaxVector), x3$activeArms + 1), dim(y3$paramValue))
	expect_equal(length(as.vector(y3$paramValue)), length(y3$paramValueFormatted) * 2)
	expect_equal("character", class(y3$paramValueFormatted)[1])
	expect_equal("array", y3$type)

	lines3a <- capture.output(print(x3))
	lines3 <- lines3a[grepl("Sample sizes ", lines3a)]
	expect_match(lines3[1], "^ *Sample sizes \\(1\\) *: 50, 50 *$")
	expect_match(lines3[2], "^ *Sample sizes \\(2\\) *: 50, 50 *$")
	expect_match(lines3[3], "^ *Sample sizes \\(3\\) *: 50, 50 *$")
	expect_match(lines3[4], "^ *Sample sizes \\(4\\) *: 50, 50 *$")

	x4 <- getSimulationMultiArmMeans(getDesignInverseNormal(kMax = 2), plannedSubjects = c(50, 100), muMaxVector = 0, seed = 123, maxNumberOfIterations = 50L)

	y4 <- .getParameterValueFormatted(x4, "sampleSizes")

	expect_equal("sampleSizes", y4$paramName)
	expect_equal(c(x4$.design$kMax, length(x4$muMaxVector), x4$activeArms + 1), dim(y4$paramValue))
	expect_equal(length(as.vector(y4$paramValue)), length(y4$paramValueFormatted))
	expect_equal("character", class(y4$paramValueFormatted)[1])
	expect_equal("array", y4$type)

	lines4a <- capture.output(print(x4))
	lines4 <- lines4a[grepl("Sample sizes ", lines4a)]
	expect_match(lines4[1], "^ *Sample sizes \\(1\\) \\[1\\] *: 50 *$")
	expect_match(lines4[2], "^ *Sample sizes \\(1\\) \\[2\\] *: 17 *$")
	expect_match(lines4[3], "^ *Sample sizes \\(2\\) \\[1\\] *: 50 *$")
	expect_match(lines4[4], "^ *Sample sizes \\(2\\) \\[2\\] *: 17 *$")
	expect_match(lines4[5], "^ *Sample sizes \\(3\\) \\[1\\] *: 50 *$")
	expect_match(lines4[6], "^ *Sample sizes \\(3\\) \\[2\\] *: 16 *$")
	expect_match(lines4[7], "^ *Sample sizes \\(4\\) \\[1\\] *: 50 *$")
	expect_match(lines4[8], "^ *Sample sizes \\(4\\) \\[2\\] *: 50 *$")
})

