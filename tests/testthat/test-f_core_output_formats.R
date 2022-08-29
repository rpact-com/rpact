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
## |  Creation date: 12 August 2022, 09:09:46
## |  File version: $Revision: 6485 $
## |  Last changed: $Date: 2022-08-12 13:20:22 +0200 (Fr, 12 Aug 2022) $
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

