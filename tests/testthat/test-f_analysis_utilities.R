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
## |  File name: test-f_analysis_utilities.R
## |  Creation date: 18 March 2022, 10:58:32
## |  File version: $Revision: 6279 $
## |  Last changed: $Date: 2022-06-09 17:48:13 +0200 (Thu, 09 Jun 2022) $
## |  Last changed by: $Author: pahlke $
## |  

context("Testing the Function Get Observed Information Rates")


test_that("'getObservedInformationRates': final-stage", {
	data1 <- getDataset(overallN = c(22, 45), overallEvents = c(11, 28))
	# @refFS[Formula]{fs:getObservedInformationRates}
	# @refFS[Formula]{fs:getObservedInformationRates:finalStageReached}
	result1 <- getObservedInformationRates(data1, maxInformation = 45)

	## Comparison of the results of list object 'result1' with expected results
	expect_equal(result1$absoluteInformations, c(22, 45))
	expect_equal(result1$maxInformation, 45)
	expect_equal(result1$informationRates, c(0.48888889, 1), tolerance = 1e-07)
	expect_equal(result1$status, "final-stage")

})

test_that("'getObservedInformationRates': over-running", {

	data2 <- getDataset(overallN = c(22, 45), overallEvents = c(11, 28))
	# @refFS[Formula]{fs:getObservedInformationRates}
	# @refFS[Formula]{fs:getObservedInformationRates:overRunning}
	result2 <- getObservedInformationRates(data2, maxInformation = 44)

	## Comparison of the results of list object 'result2' with expected results
	expect_equal(result2$absoluteInformations, c(22, 45))
	expect_equal(result2$maxInformation, 45)
	expect_equal(result2$informationRates, c(0.48888889, 1), tolerance = 1e-07)
	expect_equal(result2$status, "over-running")

})

test_that("'getObservedInformationRates': interim-stage", {

	data3 <- getDataset(overallN = c(22, 45), overallEvents = c(11, 28))
	# @refFS[Formula]{fs:getObservedInformationRates}
	# @refFS[Formula]{fs:getObservedInformationRates:interimStage}
	result3 <- getObservedInformationRates(data3, maxInformation = 46)

	## Comparison of the results of list object 'result3' with expected results
	expect_equal(result3$absoluteInformations, c(22, 45))
	expect_equal(result3$maxInformation, 46)
	expect_equal(result3$informationRates, c(0.47826087, 0.97826087, 1), tolerance = 1e-07)
	expect_equal(result3$status, "interim-stage")

})

test_that("'getObservedInformationRates': under-running with absolute information epsilon", {

	data4 <- getDataset(overallN = c(22, 45), overallEvents = c(11, 28))
	# @refFS[Formula]{fs:getObservedInformationRates}
	# @refFS[Formula]{fs:getObservedInformationRates:underRunning}
	result4 <- getObservedInformationRates(data4, maxInformation = 46, informationEpsilon = 1)

	## Comparison of the results of list object 'result4' with expected results
	expect_equal(result4$absoluteInformations, c(22, 45))
	expect_equal(result4$maxInformation, 45)
	expect_equal(result4$informationEpsilon, 1)
	expect_equal(result4$informationRates, c(0.48888889, 1), tolerance = 1e-07)
	expect_equal(result4$status, "under-running")

})

test_that("'getObservedInformationRates': under-running with relative information epsilon", {

	data5 <- getDataset(overallN = c(22, 45), overallEvents = c(11, 28))
	# @refFS[Formula]{fs:getObservedInformationRates}
	# @refFS[Formula]{fs:getObservedInformationRates:underRunningRelative}
	result5 <- getObservedInformationRates(data5, maxInformation = 46, informationEpsilon = 0.03) 

	## Comparison of the results of list object 'result5' with expected results
	expect_equal(result5$absoluteInformations, c(22, 45))
	expect_equal(result5$maxInformation, 45)
	expect_equal(result5$informationEpsilon, 0.03, tolerance = 1e-07)
	expect_equal(result5$informationRates, c(0.48888889, 1), tolerance = 1e-07)
	expect_equal(result5$status, "under-running")

})

