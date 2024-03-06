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
## |  File name: test-f_core_utilities.R
## |  Creation date: 08 November 2023, 09:09:36
## |  File version: $Revision$
## |  Last changed: $Date$
## |  Last changed by: $Author$
## |  

test_plan_section("Testing Result Object Print Output")


test_that("The output does not contain any issues", {
	expect_equal(sum(grepl("ISSUES", capture.output(getDesignGroupSequential()$show()))), 0)
	expect_equal(sum(grepl("ISSUES", capture.output(getDesignInverseNormal(kMax = 4)$show()))), 0)
	expect_equal(sum(grepl("ISSUES", capture.output(getDesignFisher()$show()))), 0)

	expect_equal(sum(grepl("ISSUES", capture.output(getSampleSizeMeans(getDesignGroupSequential())$show()))), 0)
	expect_equal(sum(grepl("ISSUES", capture.output(getSampleSizeRates()$show()))), 0)
	expect_equal(sum(grepl("ISSUES", capture.output(getSampleSizeSurvival(getDesignInverseNormal(kMax = 2))$show()))), 0)

})

test_plan_section("Testing Core Utility Functions")


test_that("'getValidatedInformationRates': 'informationRates' must be generated correctly based on specified 'kMax'", {
	.skipTestIfDisabled()

	design1 <- getTestDesign(kMax = 1L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design1), 1, tolerance = 1e-08)

	design2 <- getTestDesign(kMax = 2L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design2), c(0.5, 1), tolerance = 1e-08)

	design3 <- getTestDesign(kMax = 3L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design3), c(0.33333333, 0.66666667, 1), tolerance = 1e-08)

	design4 <- getTestDesign(kMax = 4L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design4), c(0.25, 0.5, 0.75, 1), tolerance = 1e-08)

	design5 <- getTestDesign(kMax = 5L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design5), c(0.2, 0.4, 0.6, 0.8, 1), tolerance = 1e-08)

	design6 <- getTestDesign(kMax = 6L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design6), c(0.16666667, 0.33333333, 0.5, 0.66666667, 0.83333333, 1), tolerance = 1e-08)

	design7 <- getTestDesign(kMax = 1L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design7), 1, tolerance = 1e-08)

	design8 <- getTestDesign(kMax = 2L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design8), c(0.5, 1), tolerance = 1e-08)

	design9 <- getTestDesign(kMax = 3L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design9), c(0.33333333, 0.66666667, 1), tolerance = 1e-08)

	design10 <- getTestDesign(kMax = 4L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design10), c(0.25, 0.5, 0.75, 1), tolerance = 1e-08)

	design11 <- getTestDesign(kMax = 5L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design11), c(0.2, 0.4, 0.6, 0.8, 1), tolerance = 1e-08)

	design12 <- getTestDesign(kMax = 6L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design12), c(0.16666667, 0.33333333, 0.5, 0.66666667, 0.83333333, 1), tolerance = 1e-08)

	design13 <- getTestDesign(kMax = 1L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design13), 1, tolerance = 1e-08)

	design14 <- getTestDesign(kMax = 2L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design14), c(0.5, 1), tolerance = 1e-08)

	design15 <- getTestDesign(kMax = 3L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design15), c(0.33333333, 0.66666667, 1), tolerance = 1e-08)

	design16 <- getTestDesign(kMax = 4L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design16), c(0.25, 0.5, 0.75, 1), tolerance = 1e-08)

	design17 <- getTestDesign(kMax = 5L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design17), c(0.2, 0.4, 0.6, 0.8, 1), tolerance = 1e-08)

	design18 <- getTestDesign(kMax = 6L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design18), c(0.16666667, 0.33333333, 0.5, 0.66666667, 0.83333333, 1), tolerance = 1e-08)




})

test_that("'getValidatedInformationRates': 'informationRates' must be set correctly based on specified 'informationRates'", {

	.skipTestIfDisabled()

	design19 <- getTestDesign(informationRates = 1, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design19), 1, tolerance = 1e-07)

	design20 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design20), c(0.4, 1), tolerance = 1e-07)

	design21 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design21), c(0.26666667, 0.53333333, 1), tolerance = 1e-07)

	design22 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design22), c(0.2, 0.4, 0.6, 1), tolerance = 1e-07)

	design23 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design23), c(0.16, 0.32, 0.48, 0.64, 1), tolerance = 1e-07)

	design24 <- getTestDesign(informationRates = c(0.1333, 0.26667, 0.4, 0.53333, 0.8667, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design24), c(0.1333, 0.26667, 0.4, 0.53333, 0.8667, 1), tolerance = 1e-07)

	design25 <- getTestDesign(informationRates = 1, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design25), 1, tolerance = 1e-07)

	design26 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design26), c(0.4, 1), tolerance = 1e-07)

	design27 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design27), c(0.26666667, 0.53333333, 1), tolerance = 1e-07)

	design28 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design28), c(0.2, 0.4, 0.6, 1), tolerance = 1e-07)

	design29 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design29), c(0.16, 0.32, 0.48, 0.64, 1), tolerance = 1e-07)

	design30 <- getTestDesign(informationRates = c(0.1333, 0.26667, 0.4, 0.53333, 0.8667, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design30), c(0.1333, 0.26667, 0.4, 0.53333, 0.8667, 1), tolerance = 1e-07)

	design31 <- getTestDesign(informationRates = 1, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design31), 1, tolerance = 1e-07)

	design32 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design32), c(0.4, 1), tolerance = 1e-07)

	design33 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design33), c(0.26666667, 0.53333333, 1), tolerance = 1e-07)

	design34 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design34), c(0.2, 0.4, 0.6, 1), tolerance = 1e-07)

	design35 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design35), c(0.16, 0.32, 0.48, 0.64, 1), tolerance = 1e-07)

	design36 <- getTestDesign(informationRates = c(0.1333, 0.26667, 0.4, 0.53333, 0.8667, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design36), c(0.1333, 0.26667, 0.4, 0.53333, 0.8667, 1), tolerance = 1e-07)




	design37 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design37), c(0.5, 1), tolerance = 1e-07)

	design38 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design38), c(0.33333333, 0.66666667, 1), tolerance = 1e-07)

	design39 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design39), c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)

	design40 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design40), c(0.2, 0.4, 0.6, 0.8, 1), tolerance = 1e-07)

	design41 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design41), c(0.16666667, 0.33333333, 0.5, 0.66666667, 0.83333333, 1), tolerance = 1e-07)

	design42 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design42), c(0.5, 1), tolerance = 1e-07)

	design43 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design43), c(0.33333333, 0.66666667, 1), tolerance = 1e-07)

	design44 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design44), c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)

	design45 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design45), c(0.2, 0.4, 0.6, 0.8, 1), tolerance = 1e-07)

	design46 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design46), c(0.16666667, 0.33333333, 0.5, 0.66666667, 0.83333333, 1), tolerance = 1e-07)

	design47 <- getTestDesign(futilityBounds = 0.5, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design47), c(0.5, 1), tolerance = 1e-07)

	design48 <- getTestDesign(futilityBounds = c(0.5, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design48), c(0.33333333, 0.66666667, 1), tolerance = 1e-07)

	design49 <- getTestDesign(futilityBounds = c(0.01, 0.5, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design49), c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)

	design50 <- getTestDesign(futilityBounds = c(0.01, 0.01, 0.5, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design50), c(0.2, 0.4, 0.6, 0.8, 1), tolerance = 1e-07)

	design51 <- getTestDesign(futilityBounds = c(0.01, 0.01, 0.01, 0.5, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design51), c(0.16666667, 0.33333333, 0.5, 0.66666667, 0.83333333, 1), tolerance = 1e-07)




})

test_that("'getValidatedInformationRates': 'kMax' must be set correctly based on specified 'informationRates'", {

	.skipTestIfDisabled()

	design52 <- getTestDesign(informationRates = 1, designClass = "TrialDesignGroupSequential")
	expect_equal(design52$kMax, 1, tolerance = 1e-07)

	design53 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(design53$kMax, 2, tolerance = 1e-07)

	design54 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(design54$kMax, 3, tolerance = 1e-07)

	design55 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(design55$kMax, 4, tolerance = 1e-07)

	design56 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(design56$kMax, 5, tolerance = 1e-07)

	design57 <- getTestDesign(informationRates = c(0.1333, 0.26667, 0.4, 0.53333, 0.8667, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(design57$kMax, 6, tolerance = 1e-07)

	design58 <- getTestDesign(informationRates = 1, designClass = "TrialDesignInverseNormal")
	expect_equal(design58$kMax, 1, tolerance = 1e-07)

	design59 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(design59$kMax, 2, tolerance = 1e-07)

	design60 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(design60$kMax, 3, tolerance = 1e-07)

	design61 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(design61$kMax, 4, tolerance = 1e-07)

	design62 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(design62$kMax, 5, tolerance = 1e-07)

	design63 <- getTestDesign(informationRates = c(0.1333, 0.26667, 0.4, 0.53333, 0.8667, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(design63$kMax, 6, tolerance = 1e-07)

	design64 <- getTestDesign(informationRates = 1, designClass = "TrialDesignFisher")
	expect_equal(design64$kMax, 1, tolerance = 1e-07)

	design65 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignFisher")
	expect_equal(design65$kMax, 2, tolerance = 1e-07)

	design66 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignFisher")
	expect_equal(design66$kMax, 3, tolerance = 1e-07)

	design67 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignFisher")
	expect_equal(design67$kMax, 4, tolerance = 1e-07)

	design68 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignFisher")
	expect_equal(design68$kMax, 5, tolerance = 1e-07)

	design69 <- getTestDesign(informationRates = c(0.1333, 0.26667, 0.4, 0.53333, 0.8667, 1), designClass = "TrialDesignFisher")
	expect_equal(design69$kMax, 6, tolerance = 1e-07)




	design70 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignGroupSequential")
	expect_equal(design70$kMax, 2, tolerance = 1e-07)

	design71 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(design71$kMax, 3, tolerance = 1e-07)

	design72 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(design72$kMax, 4, tolerance = 1e-07)

	design73 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(design73$kMax, 5, tolerance = 1e-07)

	design74 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(design74$kMax, 6, tolerance = 1e-07)

	design75 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignInverseNormal")
	expect_equal(design75$kMax, 2, tolerance = 1e-07)

	design76 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(design76$kMax, 3, tolerance = 1e-07)

	design77 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(design77$kMax, 4, tolerance = 1e-07)

	design78 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(design78$kMax, 5, tolerance = 1e-07)

	design79 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(design79$kMax, 6, tolerance = 1e-07)

	design80 <- getTestDesign(futilityBounds = 0.5, designClass = "TrialDesignFisher")
	expect_equal(design80$kMax, 2, tolerance = 1e-07)

	design81 <- getTestDesign(futilityBounds = c(0.5, 1), designClass = "TrialDesignFisher")
	expect_equal(design81$kMax, 3, tolerance = 1e-07)

	design82 <- getTestDesign(futilityBounds = c(0.01, 0.5, 1), designClass = "TrialDesignFisher")
	expect_equal(design82$kMax, 4, tolerance = 1e-07)

	design83 <- getTestDesign(futilityBounds = c(0.01, 0.01, 0.5, 1), designClass = "TrialDesignFisher")
	expect_equal(design83$kMax, 5, tolerance = 1e-07)

	design84 <- getTestDesign(futilityBounds = c(0.01, 0.01, 0.01, 0.5, 1), designClass = "TrialDesignFisher")
	expect_equal(design84$kMax, 6, tolerance = 1e-07)




})

test_that("'getValidatedInformationRates': 'futilityBounds' must be generated correctly based on specified 'kMax'", {

	.skipTestIfDisabled()

	design85 <- getTestDesign(kMax = 1L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design85), numeric(0), tolerance = 1e-08)

	design86 <- getTestDesign(kMax = 2L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design86), -6, tolerance = 1e-08)

	design87 <- getTestDesign(kMax = 3L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design87), c(-6, -6), tolerance = 1e-08)

	design88 <- getTestDesign(kMax = 4L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design88), c(-6, -6, -6), tolerance = 1e-08)

	design89 <- getTestDesign(kMax = 5L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design89), c(-6, -6, -6, -6), tolerance = 1e-08)

	design90 <- getTestDesign(kMax = 6L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design90), c(-6, -6, -6, -6, -6), tolerance = 1e-08)

	design91 <- getTestDesign(kMax = 7L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design91), c(-6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design92 <- getTestDesign(kMax = 8L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design92), c(-6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design93 <- getTestDesign(kMax = 9L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design93), c(-6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design94 <- getTestDesign(kMax = 10L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design94), c(-6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design95 <- getTestDesign(kMax = 11L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design95), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design96 <- getTestDesign(kMax = 12L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design96), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design97 <- getTestDesign(kMax = 13L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design97), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design98 <- getTestDesign(kMax = 14L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design98), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design99 <- getTestDesign(kMax = 15L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design99), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design100 <- getTestDesign(kMax = 16L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design100), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design101 <- getTestDesign(kMax = 17L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design101), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design102 <- getTestDesign(kMax = 18L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design102), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design103 <- getTestDesign(kMax = 19L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design103), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design104 <- getTestDesign(kMax = 20L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design104), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design105 <- getTestDesign(kMax = 1L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design105), numeric(0), tolerance = 1e-08)

	design106 <- getTestDesign(kMax = 2L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design106), -6, tolerance = 1e-08)

	design107 <- getTestDesign(kMax = 3L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design107), c(-6, -6), tolerance = 1e-08)

	design108 <- getTestDesign(kMax = 4L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design108), c(-6, -6, -6), tolerance = 1e-08)

	design109 <- getTestDesign(kMax = 5L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design109), c(-6, -6, -6, -6), tolerance = 1e-08)

	design110 <- getTestDesign(kMax = 6L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design110), c(-6, -6, -6, -6, -6), tolerance = 1e-08)

	design111 <- getTestDesign(kMax = 7L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design111), c(-6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design112 <- getTestDesign(kMax = 8L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design112), c(-6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design113 <- getTestDesign(kMax = 9L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design113), c(-6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design114 <- getTestDesign(kMax = 10L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design114), c(-6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design115 <- getTestDesign(kMax = 11L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design115), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design116 <- getTestDesign(kMax = 12L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design116), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design117 <- getTestDesign(kMax = 13L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design117), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design118 <- getTestDesign(kMax = 14L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design118), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design119 <- getTestDesign(kMax = 15L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design119), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design120 <- getTestDesign(kMax = 16L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design120), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design121 <- getTestDesign(kMax = 17L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design121), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design122 <- getTestDesign(kMax = 18L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design122), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design123 <- getTestDesign(kMax = 19L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design123), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design124 <- getTestDesign(kMax = 20L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design124), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)




	design125 <- getTestDesign(kMax = 1L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design125), numeric(0), tolerance = 1e-08)

	design126 <- getTestDesign(kMax = 2L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design126), 1, tolerance = 1e-08)

	design127 <- getTestDesign(kMax = 3L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design127), c(1, 1), tolerance = 1e-08)

	design128 <- getTestDesign(kMax = 4L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design128), c(1, 1, 1), tolerance = 1e-08)

	design129 <- getTestDesign(kMax = 5L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design129), c(1, 1, 1, 1), tolerance = 1e-08)

	design130 <- getTestDesign(kMax = 6L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design130), c(1, 1, 1, 1, 1), tolerance = 1e-08)



})

test_that("'getValidatedInformationRates': 'futilityBounds' must be set correctly based on specified 'futilityBounds'", {

	.skipTestIfDisabled()

	design131 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design131), 2, tolerance = 1e-07)

	design132 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design132), c(1, 2), tolerance = 1e-07)

	design133 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design133), c(0, 1, 2), tolerance = 1e-07)

	design134 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design134), c(0, 0, 1, 2), tolerance = 1e-07)

	design135 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design135), c(0, 0, 0, 1, 2), tolerance = 1e-07)

	design136 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design136), c(0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design137 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design137), c(0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design138 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design138), c(0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design139 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design139), c(0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design140 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design140), c(0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design141 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design141), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design142 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design142), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design143 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design143), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design144 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design144), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design145 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design145), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design146 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design146), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design147 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design147), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design148 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design148), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design149 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design149), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design150 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design150), 2, tolerance = 1e-07)

	design151 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design151), c(1, 2), tolerance = 1e-07)

	design152 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design152), c(0, 1, 2), tolerance = 1e-07)

	design153 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design153), c(0, 0, 1, 2), tolerance = 1e-07)

	design154 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design154), c(0, 0, 0, 1, 2), tolerance = 1e-07)

	design155 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design155), c(0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design156 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design156), c(0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design157 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design157), c(0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design158 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design158), c(0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design159 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design159), c(0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design160 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design160), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design161 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design161), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design162 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design162), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design163 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design163), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design164 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design164), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design165 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design165), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design166 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design166), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design167 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design167), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design168 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design168), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)




	design169 <- getTestDesign(futilityBounds = 0.5, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design169), 0.5, tolerance = 1e-07)

	design170 <- getTestDesign(futilityBounds = c(0.5, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design170), c(0.5, 1), tolerance = 1e-07)

	design171 <- getTestDesign(futilityBounds = c(0.01, 0.5, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design171), c(0.01, 0.5, 1), tolerance = 1e-07)

	design172 <- getTestDesign(futilityBounds = c(0.01, 0.01, 0.5, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design172), c(0.01, 0.01, 0.5, 1), tolerance = 1e-07)

	design173 <- getTestDesign(futilityBounds = c(0.01, 0.01, 0.01, 0.5, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design173), c(0.01, 0.01, 0.01, 0.5, 1), tolerance = 1e-07)



	design174 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design174), -6, tolerance = 1e-07)

	design175 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design175), c(-6, -6), tolerance = 1e-07)

	design176 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design176), c(-6, -6, -6), tolerance = 1e-07)

	design177 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design177), c(-6, -6, -6, -6), tolerance = 1e-07)

	design178 <- getTestDesign(informationRates = c(0.1333, 0.26667, 0.4, 0.53333, 0.8667, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design178), c(-6, -6, -6, -6, -6), tolerance = 1e-07)

	design179 <- getTestDesign(informationRates = c(0.11428571, 0.22857143, 0.34285714, 0.45714286, 0.57142857, 0.68571429, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design179), c(-6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design180 <- getTestDesign(informationRates = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design180), c(-6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design181 <- getTestDesign(informationRates = c(0.088888889, 0.17777778, 0.26666667, 0.35555556, 0.44444444, 0.53333333, 0.62222222, 0.71111111, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design181), c(-6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design182 <- getTestDesign(informationRates = c(0.08, 0.16, 0.24, 0.32, 0.4, 0.48, 0.56, 0.64, 0.72, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design182), c(-6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design183 <- getTestDesign(informationRates = c(0.072727273, 0.14545455, 0.21818182, 0.29090909, 0.36363636, 0.43636364, 0.50909091, 0.58181818, 0.65454545, 0.72727273, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design183), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design184 <- getTestDesign(informationRates = c(0.066666667, 0.13333333, 0.2, 0.26666667, 0.33333333, 0.4, 0.46666667, 0.53333333, 0.6, 0.66666667, 0.73333333, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design184), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design185 <- getTestDesign(informationRates = c(0.061538462, 0.12307692, 0.18461538, 0.24615385, 0.30769231, 0.36923077, 0.43076923, 0.49230769, 0.55384615, 0.61538462, 0.67692308, 0.73846154, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design185), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design186 <- getTestDesign(informationRates = c(0.057142857, 0.11428571, 0.17142857, 0.22857143, 0.28571429, 0.34285714, 0.4, 0.45714286, 0.51428571, 0.57142857, 0.62857143, 0.68571429, 0.74285714, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design186), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design187 <- getTestDesign(informationRates = c(0.053333333, 0.10666667, 0.16, 0.21333333, 0.26666667, 0.32, 0.37333333, 0.42666667, 0.48, 0.53333333, 0.58666667, 0.64, 0.69333333, 0.74666667, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design187), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design188 <- getTestDesign(informationRates = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design188), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design189 <- getTestDesign(informationRates = c(0.047058824, 0.094117647, 0.14117647, 0.18823529, 0.23529412, 0.28235294, 0.32941176, 0.37647059, 0.42352941, 0.47058824, 0.51764706, 0.56470588, 0.61176471, 0.65882353, 0.70588235, 0.75294118, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design189), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design190 <- getTestDesign(informationRates = c(0.044444444, 0.088888889, 0.13333333, 0.17777778, 0.22222222, 0.26666667, 0.31111111, 0.35555556, 0.4, 0.44444444, 0.48888889, 0.53333333, 0.57777778, 0.62222222, 0.66666667, 0.71111111, 0.75555556, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design190), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design191 <- getTestDesign(informationRates = c(0.042105263, 0.084210526, 0.12631579, 0.16842105, 0.21052632, 0.25263158, 0.29473684, 0.33684211, 0.37894737, 0.42105263, 0.46315789, 0.50526316, 0.54736842, 0.58947368, 0.63157895, 0.67368421, 0.71578947, 0.75789474, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design191), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design192 <- getTestDesign(informationRates = c(0.04, 0.08, 0.12, 0.16, 0.2, 0.24, 0.28, 0.32, 0.36, 0.4, 0.44, 0.48, 0.52, 0.56, 0.6, 0.64, 0.68, 0.72, 0.76, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design192), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design193 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design193), -6, tolerance = 1e-07)

	design194 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design194), c(-6, -6), tolerance = 1e-07)

	design195 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design195), c(-6, -6, -6), tolerance = 1e-07)

	design196 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design196), c(-6, -6, -6, -6), tolerance = 1e-07)

	design197 <- getTestDesign(informationRates = c(0.1333, 0.26667, 0.4, 0.53333, 0.8667, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design197), c(-6, -6, -6, -6, -6), tolerance = 1e-07)

	design198 <- getTestDesign(informationRates = c(0.11428571, 0.22857143, 0.34285714, 0.45714286, 0.57142857, 0.68571429, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design198), c(-6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design199 <- getTestDesign(informationRates = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design199), c(-6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design200 <- getTestDesign(informationRates = c(0.088888889, 0.17777778, 0.26666667, 0.35555556, 0.44444444, 0.53333333, 0.62222222, 0.71111111, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design200), c(-6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design201 <- getTestDesign(informationRates = c(0.08, 0.16, 0.24, 0.32, 0.4, 0.48, 0.56, 0.64, 0.72, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design201), c(-6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design202 <- getTestDesign(informationRates = c(0.072727273, 0.14545455, 0.21818182, 0.29090909, 0.36363636, 0.43636364, 0.50909091, 0.58181818, 0.65454545, 0.72727273, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design202), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design203 <- getTestDesign(informationRates = c(0.066666667, 0.13333333, 0.2, 0.26666667, 0.33333333, 0.4, 0.46666667, 0.53333333, 0.6, 0.66666667, 0.73333333, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design203), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design204 <- getTestDesign(informationRates = c(0.061538462, 0.12307692, 0.18461538, 0.24615385, 0.30769231, 0.36923077, 0.43076923, 0.49230769, 0.55384615, 0.61538462, 0.67692308, 0.73846154, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design204), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design205 <- getTestDesign(informationRates = c(0.057142857, 0.11428571, 0.17142857, 0.22857143, 0.28571429, 0.34285714, 0.4, 0.45714286, 0.51428571, 0.57142857, 0.62857143, 0.68571429, 0.74285714, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design205), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design206 <- getTestDesign(informationRates = c(0.053333333, 0.10666667, 0.16, 0.21333333, 0.26666667, 0.32, 0.37333333, 0.42666667, 0.48, 0.53333333, 0.58666667, 0.64, 0.69333333, 0.74666667, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design206), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design207 <- getTestDesign(informationRates = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design207), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design208 <- getTestDesign(informationRates = c(0.047058824, 0.094117647, 0.14117647, 0.18823529, 0.23529412, 0.28235294, 0.32941176, 0.37647059, 0.42352941, 0.47058824, 0.51764706, 0.56470588, 0.61176471, 0.65882353, 0.70588235, 0.75294118, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design208), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design209 <- getTestDesign(informationRates = c(0.044444444, 0.088888889, 0.13333333, 0.17777778, 0.22222222, 0.26666667, 0.31111111, 0.35555556, 0.4, 0.44444444, 0.48888889, 0.53333333, 0.57777778, 0.62222222, 0.66666667, 0.71111111, 0.75555556, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design209), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design210 <- getTestDesign(informationRates = c(0.042105263, 0.084210526, 0.12631579, 0.16842105, 0.21052632, 0.25263158, 0.29473684, 0.33684211, 0.37894737, 0.42105263, 0.46315789, 0.50526316, 0.54736842, 0.58947368, 0.63157895, 0.67368421, 0.71578947, 0.75789474, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design210), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design211 <- getTestDesign(informationRates = c(0.04, 0.08, 0.12, 0.16, 0.2, 0.24, 0.28, 0.32, 0.36, 0.4, 0.44, 0.48, 0.52, 0.56, 0.6, 0.64, 0.68, 0.72, 0.76, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design211), c(-6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)




	design212 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design212), 1, tolerance = 1e-07)

	design213 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design213), c(1, 1), tolerance = 1e-07)

	design214 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design214), c(1, 1, 1), tolerance = 1e-07)

	design215 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design215), c(1, 1, 1, 1), tolerance = 1e-07)

	design216 <- getTestDesign(informationRates = c(0.1333, 0.26667, 0.4, 0.53333, 0.8667, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design216), c(1, 1, 1, 1, 1), tolerance = 1e-07)



})

test_that("'getValidatedInformationRates': 'kMax' must be set correctly based on specified 'futilityBounds'", {

	.skipTestIfDisabled()

	design217 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design217)
	expect_equal(design217$kMax, 2, tolerance = 1e-07)

	design218 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design218)
	expect_equal(design218$kMax, 3, tolerance = 1e-07)

	design219 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design219)
	expect_equal(design219$kMax, 4, tolerance = 1e-07)

	design220 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design220)
	expect_equal(design220$kMax, 5, tolerance = 1e-07)

	design221 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design221)
	expect_equal(design221$kMax, 6, tolerance = 1e-07)

	design222 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design222)
	expect_equal(design222$kMax, 7, tolerance = 1e-07)

	design223 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design223)
	expect_equal(design223$kMax, 8, tolerance = 1e-07)

	design224 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design224)
	expect_equal(design224$kMax, 9, tolerance = 1e-07)

	design225 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design225)
	expect_equal(design225$kMax, 10, tolerance = 1e-07)

	design226 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design226)
	expect_equal(design226$kMax, 11, tolerance = 1e-07)

	design227 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design227)
	expect_equal(design227$kMax, 12, tolerance = 1e-07)

	design228 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design228)
	expect_equal(design228$kMax, 13, tolerance = 1e-07)

	design229 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design229)
	expect_equal(design229$kMax, 14, tolerance = 1e-07)

	design230 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design230)
	expect_equal(design230$kMax, 15, tolerance = 1e-07)

	design231 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design231)
	expect_equal(design231$kMax, 16, tolerance = 1e-07)

	design232 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design232)
	expect_equal(design232$kMax, 17, tolerance = 1e-07)

	design233 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design233)
	expect_equal(design233$kMax, 18, tolerance = 1e-07)

	design234 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design234)
	expect_equal(design234$kMax, 19, tolerance = 1e-07)

	design235 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design235)
	expect_equal(design235$kMax, 20, tolerance = 1e-07)

	design236 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design236)
	expect_equal(design236$kMax, 2, tolerance = 1e-07)

	design237 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design237)
	expect_equal(design237$kMax, 3, tolerance = 1e-07)

	design238 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design238)
	expect_equal(design238$kMax, 4, tolerance = 1e-07)

	design239 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design239)
	expect_equal(design239$kMax, 5, tolerance = 1e-07)

	design240 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design240)
	expect_equal(design240$kMax, 6, tolerance = 1e-07)

	design241 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design241)
	expect_equal(design241$kMax, 7, tolerance = 1e-07)

	design242 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design242)
	expect_equal(design242$kMax, 8, tolerance = 1e-07)

	design243 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design243)
	expect_equal(design243$kMax, 9, tolerance = 1e-07)

	design244 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design244)
	expect_equal(design244$kMax, 10, tolerance = 1e-07)

	design245 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design245)
	expect_equal(design245$kMax, 11, tolerance = 1e-07)

	design246 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design246)
	expect_equal(design246$kMax, 12, tolerance = 1e-07)

	design247 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design247)
	expect_equal(design247$kMax, 13, tolerance = 1e-07)

	design248 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design248)
	expect_equal(design248$kMax, 14, tolerance = 1e-07)

	design249 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design249)
	expect_equal(design249$kMax, 15, tolerance = 1e-07)

	design250 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design250)
	expect_equal(design250$kMax, 16, tolerance = 1e-07)

	design251 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design251)
	expect_equal(design251$kMax, 17, tolerance = 1e-07)

	design252 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design252)
	expect_equal(design252$kMax, 18, tolerance = 1e-07)

	design253 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design253)
	expect_equal(design253$kMax, 19, tolerance = 1e-07)

	design254 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design254)
	expect_equal(design254$kMax, 20, tolerance = 1e-07)




	design255 <- getTestDesign(futilityBounds = 0.5, designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design255)
	expect_equal(design255$kMax, 2, tolerance = 1e-07)

	design256 <- getTestDesign(futilityBounds = c(0.5, 1), designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design256)
	expect_equal(design256$kMax, 3, tolerance = 1e-07)

	design257 <- getTestDesign(futilityBounds = c(0.01, 0.5, 1), designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design257)
	expect_equal(design257$kMax, 4, tolerance = 1e-07)

	design258 <- getTestDesign(futilityBounds = c(0.01, 0.01, 0.5, 1), designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design258)
	expect_equal(design258$kMax, 5, tolerance = 1e-07)

	design259 <- getTestDesign(futilityBounds = c(0.01, 0.01, 0.01, 0.5, 1), designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design259)
	expect_equal(design259$kMax, 6, tolerance = 1e-07)



	design260 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design260)
	expect_equal(design260$kMax, 2, tolerance = 1e-07)

	design261 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design261)
	expect_equal(design261$kMax, 3, tolerance = 1e-07)

	design262 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design262)
	expect_equal(design262$kMax, 4, tolerance = 1e-07)

	design263 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design263)
	expect_equal(design263$kMax, 5, tolerance = 1e-07)

	design264 <- getTestDesign(informationRates = c(0.1333, 0.26667, 0.4, 0.53333, 0.8667, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design264)
	expect_equal(design264$kMax, 6, tolerance = 1e-07)

	design265 <- getTestDesign(informationRates = c(0.11428571, 0.22857143, 0.34285714, 0.45714286, 0.57142857, 0.68571429, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design265)
	expect_equal(design265$kMax, 7, tolerance = 1e-07)

	design266 <- getTestDesign(informationRates = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design266)
	expect_equal(design266$kMax, 8, tolerance = 1e-07)

	design267 <- getTestDesign(informationRates = c(0.088888889, 0.17777778, 0.26666667, 0.35555556, 0.44444444, 0.53333333, 0.62222222, 0.71111111, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design267)
	expect_equal(design267$kMax, 9, tolerance = 1e-07)

	design268 <- getTestDesign(informationRates = c(0.08, 0.16, 0.24, 0.32, 0.4, 0.48, 0.56, 0.64, 0.72, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design268)
	expect_equal(design268$kMax, 10, tolerance = 1e-07)

	design269 <- getTestDesign(informationRates = c(0.072727273, 0.14545455, 0.21818182, 0.29090909, 0.36363636, 0.43636364, 0.50909091, 0.58181818, 0.65454545, 0.72727273, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design269)
	expect_equal(design269$kMax, 11, tolerance = 1e-07)

	design270 <- getTestDesign(informationRates = c(0.066666667, 0.13333333, 0.2, 0.26666667, 0.33333333, 0.4, 0.46666667, 0.53333333, 0.6, 0.66666667, 0.73333333, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design270)
	expect_equal(design270$kMax, 12, tolerance = 1e-07)

	design271 <- getTestDesign(informationRates = c(0.061538462, 0.12307692, 0.18461538, 0.24615385, 0.30769231, 0.36923077, 0.43076923, 0.49230769, 0.55384615, 0.61538462, 0.67692308, 0.73846154, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design271)
	expect_equal(design271$kMax, 13, tolerance = 1e-07)

	design272 <- getTestDesign(informationRates = c(0.057142857, 0.11428571, 0.17142857, 0.22857143, 0.28571429, 0.34285714, 0.4, 0.45714286, 0.51428571, 0.57142857, 0.62857143, 0.68571429, 0.74285714, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design272)
	expect_equal(design272$kMax, 14, tolerance = 1e-07)

	design273 <- getTestDesign(informationRates = c(0.053333333, 0.10666667, 0.16, 0.21333333, 0.26666667, 0.32, 0.37333333, 0.42666667, 0.48, 0.53333333, 0.58666667, 0.64, 0.69333333, 0.74666667, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design273)
	expect_equal(design273$kMax, 15, tolerance = 1e-07)

	design274 <- getTestDesign(informationRates = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design274)
	expect_equal(design274$kMax, 16, tolerance = 1e-07)

	design275 <- getTestDesign(informationRates = c(0.047058824, 0.094117647, 0.14117647, 0.18823529, 0.23529412, 0.28235294, 0.32941176, 0.37647059, 0.42352941, 0.47058824, 0.51764706, 0.56470588, 0.61176471, 0.65882353, 0.70588235, 0.75294118, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design275)
	expect_equal(design275$kMax, 17, tolerance = 1e-07)

	design276 <- getTestDesign(informationRates = c(0.044444444, 0.088888889, 0.13333333, 0.17777778, 0.22222222, 0.26666667, 0.31111111, 0.35555556, 0.4, 0.44444444, 0.48888889, 0.53333333, 0.57777778, 0.62222222, 0.66666667, 0.71111111, 0.75555556, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design276)
	expect_equal(design276$kMax, 18, tolerance = 1e-07)

	design277 <- getTestDesign(informationRates = c(0.042105263, 0.084210526, 0.12631579, 0.16842105, 0.21052632, 0.25263158, 0.29473684, 0.33684211, 0.37894737, 0.42105263, 0.46315789, 0.50526316, 0.54736842, 0.58947368, 0.63157895, 0.67368421, 0.71578947, 0.75789474, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design277)
	expect_equal(design277$kMax, 19, tolerance = 1e-07)

	design278 <- getTestDesign(informationRates = c(0.04, 0.08, 0.12, 0.16, 0.2, 0.24, 0.28, 0.32, 0.36, 0.4, 0.44, 0.48, 0.52, 0.56, 0.6, 0.64, 0.68, 0.72, 0.76, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design278)
	expect_equal(design278$kMax, 20, tolerance = 1e-07)

	design279 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design279)
	expect_equal(design279$kMax, 2, tolerance = 1e-07)

	design280 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design280)
	expect_equal(design280$kMax, 3, tolerance = 1e-07)

	design281 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design281)
	expect_equal(design281$kMax, 4, tolerance = 1e-07)

	design282 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design282)
	expect_equal(design282$kMax, 5, tolerance = 1e-07)

	design283 <- getTestDesign(informationRates = c(0.1333, 0.26667, 0.4, 0.53333, 0.8667, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design283)
	expect_equal(design283$kMax, 6, tolerance = 1e-07)

	design284 <- getTestDesign(informationRates = c(0.11428571, 0.22857143, 0.34285714, 0.45714286, 0.57142857, 0.68571429, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design284)
	expect_equal(design284$kMax, 7, tolerance = 1e-07)

	design285 <- getTestDesign(informationRates = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design285)
	expect_equal(design285$kMax, 8, tolerance = 1e-07)

	design286 <- getTestDesign(informationRates = c(0.088888889, 0.17777778, 0.26666667, 0.35555556, 0.44444444, 0.53333333, 0.62222222, 0.71111111, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design286)
	expect_equal(design286$kMax, 9, tolerance = 1e-07)

	design287 <- getTestDesign(informationRates = c(0.08, 0.16, 0.24, 0.32, 0.4, 0.48, 0.56, 0.64, 0.72, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design287)
	expect_equal(design287$kMax, 10, tolerance = 1e-07)

	design288 <- getTestDesign(informationRates = c(0.072727273, 0.14545455, 0.21818182, 0.29090909, 0.36363636, 0.43636364, 0.50909091, 0.58181818, 0.65454545, 0.72727273, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design288)
	expect_equal(design288$kMax, 11, tolerance = 1e-07)

	design289 <- getTestDesign(informationRates = c(0.066666667, 0.13333333, 0.2, 0.26666667, 0.33333333, 0.4, 0.46666667, 0.53333333, 0.6, 0.66666667, 0.73333333, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design289)
	expect_equal(design289$kMax, 12, tolerance = 1e-07)

	design290 <- getTestDesign(informationRates = c(0.061538462, 0.12307692, 0.18461538, 0.24615385, 0.30769231, 0.36923077, 0.43076923, 0.49230769, 0.55384615, 0.61538462, 0.67692308, 0.73846154, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design290)
	expect_equal(design290$kMax, 13, tolerance = 1e-07)

	design291 <- getTestDesign(informationRates = c(0.057142857, 0.11428571, 0.17142857, 0.22857143, 0.28571429, 0.34285714, 0.4, 0.45714286, 0.51428571, 0.57142857, 0.62857143, 0.68571429, 0.74285714, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design291)
	expect_equal(design291$kMax, 14, tolerance = 1e-07)

	design292 <- getTestDesign(informationRates = c(0.053333333, 0.10666667, 0.16, 0.21333333, 0.26666667, 0.32, 0.37333333, 0.42666667, 0.48, 0.53333333, 0.58666667, 0.64, 0.69333333, 0.74666667, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design292)
	expect_equal(design292$kMax, 15, tolerance = 1e-07)

	design293 <- getTestDesign(informationRates = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design293)
	expect_equal(design293$kMax, 16, tolerance = 1e-07)

	design294 <- getTestDesign(informationRates = c(0.047058824, 0.094117647, 0.14117647, 0.18823529, 0.23529412, 0.28235294, 0.32941176, 0.37647059, 0.42352941, 0.47058824, 0.51764706, 0.56470588, 0.61176471, 0.65882353, 0.70588235, 0.75294118, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design294)
	expect_equal(design294$kMax, 17, tolerance = 1e-07)

	design295 <- getTestDesign(informationRates = c(0.044444444, 0.088888889, 0.13333333, 0.17777778, 0.22222222, 0.26666667, 0.31111111, 0.35555556, 0.4, 0.44444444, 0.48888889, 0.53333333, 0.57777778, 0.62222222, 0.66666667, 0.71111111, 0.75555556, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design295)
	expect_equal(design295$kMax, 18, tolerance = 1e-07)

	design296 <- getTestDesign(informationRates = c(0.042105263, 0.084210526, 0.12631579, 0.16842105, 0.21052632, 0.25263158, 0.29473684, 0.33684211, 0.37894737, 0.42105263, 0.46315789, 0.50526316, 0.54736842, 0.58947368, 0.63157895, 0.67368421, 0.71578947, 0.75789474, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design296)
	expect_equal(design296$kMax, 19, tolerance = 1e-07)

	design297 <- getTestDesign(informationRates = c(0.04, 0.08, 0.12, 0.16, 0.2, 0.24, 0.28, 0.32, 0.36, 0.4, 0.44, 0.48, 0.52, 0.56, 0.6, 0.64, 0.68, 0.72, 0.76, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design297)
	expect_equal(design297$kMax, 20, tolerance = 1e-07)




	design298 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design298)
	expect_equal(design298$kMax, 2, tolerance = 1e-07)

	design299 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design299)
	expect_equal(design299$kMax, 3, tolerance = 1e-07)

	design300 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design300)
	expect_equal(design300$kMax, 4, tolerance = 1e-07)

	design301 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design301)
	expect_equal(design301$kMax, 5, tolerance = 1e-07)

	design302 <- getTestDesign(informationRates = c(0.1333, 0.26667, 0.4, 0.53333, 0.8667, 1), designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design302)
	expect_equal(design302$kMax, 6, tolerance = 1e-07)



})

test_plan_section("Testing Utilities")


test_that("Testing '.moveValue'", {
	expect_equal(.moveValue(c("A", "B", "C", "D", "E"), "E", "B"), c("A", "B", "E", "C", "D"))
	expect_equal(.moveValue(c("A", "B", "C", "D", "E"), "E", "A"), c("A", "E", "B", "C", "D"))
	expect_equal(.moveValue(c("A", "B", "C", "D", "E"), "A", "E"), c("B", "C", "D", "E", "A"))
	expect_equal(.moveValue(c("A", "B", "C", "D", "E"), "E", "E"), c("A", "B", "C", "D", "E"))
	expect_equal(.moveValue(c("A", "B", "C", "D", "E"), "A", "A"), c("A", "B", "C", "D", "E"))
	expect_equal(.moveValue(c("A"), "A", "A"), c("A"))

})

test_that("Testing '.toCapitalized'", {

	expect_equal(.toCapitalized("zip code"), "Zip Code")
	expect_equal(.toCapitalized("state of the art"), "State of the Art")
	expect_equal(.toCapitalized("final and count"), "Final and Count")
	expect_equal(.toCapitalized("Hazard Ratio"), "Hazard Ratio") 
	expect_equal(.toCapitalized("hazard ratio function"), "Hazard Ratio Function")

})

test_that("Testing '.formatCamelCase'", {

	expect_equal(.formatCamelCase("hazardRatio", title = TRUE), "Hazard Ratio")
	expect_equal(.formatCamelCase("hazardRatio and informationRates", title = TRUE), "Hazard Ratio and Information Rates")
	expect_equal(.formatCamelCase("hazardRatio", title = FALSE), "hazard ratio")
	expect_equal(.formatCamelCase(" hazardRatio ", title = TRUE), " Hazard Ratio ")
	expect_equal(.formatCamelCase("Hazard", title = TRUE), "Hazard")
	expect_equal(.formatCamelCase("hazard", title = TRUE), "Hazard")
	expect_equal(.formatCamelCase("hazard", title = FALSE), "hazard")
	expect_equal(.formatCamelCase("Hazard", title = FALSE), "hazard")
	expect_equal(.formatCamelCase("Hazard Ratio", title = TRUE), "Hazard Ratio")
	expect_equal(.formatCamelCase(" hazard ratio ", title = TRUE), " Hazard Ratio ")
	expect_equal(.formatCamelCase("HazardRatio", title = FALSE), "hazard ratio")

})

test_that("Testing '.equalsRegexpIgnoreCase'", {

	expect_equal(.equalsRegexpIgnoreCase("stage2", "^stages?$"), FALSE)
	expect_equal(.equalsRegexpIgnoreCase("stage", "^stages?$"), TRUE)
	expect_equal(.equalsRegexpIgnoreCase("stages", "^stages?$"), TRUE)
	expect_equal(.equalsRegexpIgnoreCase("Stage", "^stages?$"), TRUE)
	expect_equal(.equalsRegexpIgnoreCase("STAGES", "^stages?$"), TRUE)
	expect_equal(.equalsRegexpIgnoreCase("stages2", "^stages?$"), FALSE)
	expect_equal(.equalsRegexpIgnoreCase(" stages", "^stages?$"), FALSE)

	expect_equal(.equalsRegexpIgnoreCase("stages2", "stages?"), TRUE)
	expect_equal(.equalsRegexpIgnoreCase("1stage2", "stages?"), TRUE)

})

test_that("Testing 'isUndefinedArgument' and 'isValidArgument'", {

	expect_equal(.isUndefinedArgument(NULL), TRUE)
	expect_equal(.isUndefinedArgument(numeric(0)), TRUE)
	expect_equal(.isUndefinedArgument(NA), TRUE)
	expect_equal(.isUndefinedArgument(NA_integer_), TRUE)
	expect_equal(.isUndefinedArgument(NA_real_), TRUE)
	expect_equal(.isUndefinedArgument(NA_complex_), TRUE)
	expect_equal(.isUndefinedArgument(NA_character_), TRUE)
	expect_equal(.isUndefinedArgument(c(NA, NA)), FALSE)
	expect_equal(.isUndefinedArgument(c(1, NA, NA)), FALSE)
	expect_equal(.isUndefinedArgument(c(NA, NA, 1)), FALSE)
	expect_equal(.isUndefinedArgument(1), FALSE)

	expect_equal(.isDefinedArgument(NULL), FALSE)
	expect_equal(.isDefinedArgument(numeric(0)), FALSE)
	expect_equal(.isDefinedArgument(NA), FALSE)
	expect_equal(.isDefinedArgument(NA_integer_), FALSE)
	expect_equal(.isDefinedArgument(NA_real_), FALSE)
	expect_equal(.isDefinedArgument(NA_complex_), FALSE)
	expect_equal(.isDefinedArgument(NA_character_), FALSE)
	expect_equal(.isDefinedArgument(c(NA, NA)), TRUE)
	expect_equal(.isDefinedArgument(c(1, NA, NA)), TRUE)
	expect_equal(.isDefinedArgument(c(NA, NA, 1)), TRUE)
	expect_equal(.isDefinedArgument(1), TRUE)

	expect_error(.isDefinedArgument(notExistingTestVariable, argumentExistsValidationEnabled = FALSE))
	expect_error(.isDefinedArgument(notExistingTestVariable))

	# skip_if_translated()
	# expect_error(.isDefinedArgument(notExistingTestVariable),
	# 	paste0("Missing argument: the object 'notExistingTestVariable' has not been defined anywhere. ",
	# 	"Please define it first, e.g., run 'notExistingTestVariable <- 1'"), fixed = TRUE)

})

test_that("Result of 'setSeed(seed)' is working for different arguments, incl. NULL and NA", {

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	expect_false(is.null(.setSeed()))
	expect_false(is.na(.setSeed()))
	expect_true(is.numeric(.setSeed()))

	expect_false(is.null(.setSeed(NULL)))
	expect_false(is.na(.setSeed(NULL)))
	expect_true(is.numeric(.setSeed(NULL)))

	expect_false(is.null(.setSeed(NA)))
	expect_false(is.na(.setSeed(NA)))
	expect_true(is.numeric(.setSeed(NA)))

	expect_true(.setSeed() != .setSeed())

	expect_equal(.setSeed(123), 123)
	expect_equal(.setSeed(0), 0)
	expect_equal(.setSeed(5e-5), 5e-5)

})

test_that("Testing '.getInputForZeroOutputInsideTolerance''", {

	input <- 99
	tolerance <- 1e-05
	epsilon <- 1e-08

	expect_equal(.getInputForZeroOutputInsideTolerance(input, tolerance, tolerance), input)
	expect_equal(.getInputForZeroOutputInsideTolerance(input, tolerance + epsilon, tolerance), NA_real_)
	expect_equal(.getInputForZeroOutputInsideTolerance(input, tolerance - epsilon, tolerance), input)

})

test_that("Testing '.arrayToString'", {

	expect_equal(.arrayToString(NA, vectorLookAndFeelEnabled = TRUE), "NA")
	expect_equal(.arrayToString(NULL, vectorLookAndFeelEnabled = TRUE), "NULL")
	expect_equal(.arrayToString(c(1, 2, 3), vectorLookAndFeelEnabled = TRUE), "c(1, 2, 3)")
	expect_equal(.arrayToString(c(NA, 2, 3), vectorLookAndFeelEnabled = TRUE), "c(NA, 2, 3)")
	expect_equal(.arrayToString(c(1, 2, NA), vectorLookAndFeelEnabled = TRUE), "c(1, 2, NA)")
	expect_equal(.arrayToString(c(NA, NA, NA), vectorLookAndFeelEnabled = TRUE), "c(NA, NA, NA)")
	expect_equal(.arrayToString(c(1, NULL, 3), vectorLookAndFeelEnabled = TRUE), "c(1, 3)")

})

test_that("Testing '.getQNorm'", {

	expect_equal(sign(.getQNorm(1)), sign(qnorm(1)))
	expect_equal(.getQNorm(1 - 1e-12), qnorm(1 - 1e-12))
	expect_equal(sign(.getQNorm(0)), sign(qnorm(0)))
	expect_equal(.getQNorm(1e-12), qnorm(1e-12))

})

test_that("Testing '.getOneMinusQNorm'", {

	expect_equal(sign(.getOneMinusQNorm(1)), sign(1 - qnorm(1)))
	expect_equal(.getOneMinusQNorm(1 - 1e-12), -qnorm(1 - 1e-12))
	expect_equal(sign(.getOneMinusQNorm(0)), sign(1 - qnorm(0)))
	expect_equal(.getOneMinusQNorm(1e-12), -qnorm(1e-12))

})

test_that("Testing '.getInputProducingZeroOutput'", {

	tolerance <- 1e-05
	epsilon <- 1e-08

	expect_equal(.getInputProducingZeroOutput(1, 0, 2, 99, tolerance), 1)
	expect_equal(.getInputProducingZeroOutput(1, 99, 2, 0, tolerance), 2)

	expect_equal(.getInputProducingZeroOutput(1, 0, NA, 0, tolerance), 1)
	expect_equal(.getInputProducingZeroOutput(NA, 0, 2, 0, tolerance), 2)

	expect_equal(.getInputProducingZeroOutput(1, 0, NA, NA, tolerance), 1)
	expect_equal(.getInputProducingZeroOutput(NA, NA, 2, 0, tolerance), 2)

	expect_equal(.getInputProducingZeroOutput(1, 0, 2, NA, tolerance), 1)
	expect_equal(.getInputProducingZeroOutput(1, NA, 2, 0, tolerance), 2)

	expect_equal(.getInputProducingZeroOutput(1, tolerance, 2, 99, tolerance), 1)
	expect_equal(.getInputProducingZeroOutput(1, 99, 2, tolerance, tolerance), 2)

	expect_equal(.getInputProducingZeroOutput(1, tolerance, 2, tolerance + epsilon, tolerance), 1)
	expect_equal(.getInputProducingZeroOutput(1, tolerance + epsilon, 2, tolerance, tolerance), 2)

	expect_equal(.getInputProducingZeroOutput(1, tolerance, 2, tolerance - epsilon, tolerance), 2)
	expect_equal(.getInputProducingZeroOutput(1, tolerance - epsilon, 2, tolerance, tolerance), 1)

	expect_equal(.getInputProducingZeroOutput(1, tolerance - epsilon, 2, tolerance, tolerance), 1)
	expect_equal(.getInputProducingZeroOutput(1, tolerance, 2, tolerance - epsilon, tolerance), 2)

})

test_that("Testing '.getOneDimensionalRoot'", {

	.skipTestIfDisabled()

	tolerance <- 1e-08

	expect_equal(.getOneDimensionalRoot(f = function(x) {
	    x - 2
	}, lower = -1, upper = 1, tolerance = tolerance), NA_real_)
	expect_equal(.getOneDimensionalRoot(f = function(x) {
	    x + 2
	}, lower = -1, upper = 1, tolerance = tolerance), NA_real_)

	expect_equal(.getOneDimensionalRoot(f = function(x) {
	    x - 1 - tolerance
	}, lower = -1, upper = 1, tolerance = tolerance), 1)
	expect_equal(.getOneDimensionalRoot(f = function(x) {
	    x + 1 + tolerance
	}, lower = -1, upper = 1, tolerance = tolerance), -1)
	expect_equal(.getOneDimensionalRoot(f = function(x) {
	    x - 1
	}, lower = -1, upper = 1, tolerance = tolerance), 1)
	expect_equal(.getOneDimensionalRoot(f = function(x) {
	    x + 1
	}, lower = -1, upper = 1, tolerance = tolerance), -1)

	expect_equal(.getOneDimensionalRoot(f = function(x) {
	    x - 1
	}, lower = 0, upper = 1, tolerance = tolerance), 1)
	expect_equal(.getOneDimensionalRoot(f = function(x) {
	    x - 1
	}, lower = tolerance, upper = 1, tolerance = tolerance), 1)
	expect_equal(.getOneDimensionalRoot(f = function(x) {
	    x + 1
	}, lower = -1, upper = 0, tolerance = tolerance), -1)
	expect_equal(.getOneDimensionalRoot(f = function(x) {
	    x + 1
	}, lower = -1, upper = 1 - tolerance, tolerance = tolerance), -1)

	expect_equal(.getOneDimensionalRoot(f = function(x) {
	    x - 3
	}, lower = 1, upper = 5, tolerance = tolerance), 3)
	expect_equal(.getOneDimensionalRoot(f = function(x) {
	    x + 3
	}, lower = -5, upper = -1, tolerance = tolerance), -3)

	expect_equal(.getOneDimensionalRoot(f = function(x) {
	    3 * x - 700
	}, lower = 100, upper = 1000, tolerance = tolerance), 233.33333333)
	expect_equal(.getOneDimensionalRoot(f = function(x) {
	    3 * x + 700
	}, lower = -1000, upper = -100, tolerance = tolerance), -233.33333333)

	expect_equal(.getOneDimensionalRoot(f = function(x) {
	    x - 4
	}, lower = -10, upper = 10), 4, tolerance = tolerance)
	expect_equal(.getOneDimensionalRoot(f = function(x) {
	    x + 4
	}, lower = -10, upper = 10), -4, tolerance = tolerance)

	dataExample1 <- getDataset(
	    overallEvents = c(33, 55, 129),
	    overallAllocationRatios = c(1, 1, 4),
	    overallLogRanks = c(1.02, 1.38, 2.2)
	)
	design1 <- getDesignGroupSequential(kMax = 3, alpha = 0.025, typeOfDesign = "WT", deltaWT = 0.25)
	result1 <- getRepeatedConfidenceIntervals(design1, dataExample1, stage = 3)

	## Comparison of the results of matrixarray object 'result1' with expected results
	expect_equal(result1[1, ], c(0.54923831, 0.77922365, 1.0261298), tolerance = 1e-07, label = paste0("c(", paste0(result1[1, ], collapse = ", "), ")"))
	expect_equal(result1[2, ], c(3.7041718, 2.7014099, 2.5669073), tolerance = 1e-07, label = paste0("c(", paste0(result1[2, ], collapse = ", "), ")"))

	design2 <- getDesignGroupSequential(
	    kMax = 3, alpha = 0.025, informationRates = c(0.4, 0.7, 1),
	    typeOfDesign = "WT", deltaWT = 0.35
	)
	dataExample2 <- getDataset(
	    overallN2 = c(30, 80, 100),
	    overallN1 = c(30, 80, 100),
	    overallEvents2 = c(10, 25, 36),
	    overallEvents1 = c(14, 35, 53)
	)
	result2 <- getRepeatedConfidenceIntervals(
	    design = design2, dataInput = dataExample2,
	    stage = 3, normalApproximation = TRUE, directionUpper = TRUE
	)

	## Comparison of the results of matrixarray object 'result2' with expected results
	expect_equal(result2[1, ], c(-0.17491833, -0.048575314, 0.018957987), tolerance = 1e-07, label = paste0("c(", paste0(result2[1, ], collapse = ", "), ")"))
	expect_equal(result2[2, ], c(0.41834377, 0.2916876, 0.31353674), tolerance = 1e-07, label = paste0("c(", paste0(result2[2, ], collapse = ", "), ")"))

	design3 <- getDesignInverseNormal(
	    kMax = 2, alpha = 0.025, informationRates = c(0.5, 1),
	    typeOfDesign = "WT", deltaWT = 0.25
	)
	dataExample3 <- getDataset(
	    events1 = c(7, 57),
	    events2 = c(7, 57),
	    n1 = c(30, 300),
	    n2 = c(30, 300)
	)
	result3 <- getRepeatedConfidenceIntervals(design3, dataExample3)

	## Comparison of the results of matrixarray object 'result3' with expected results
	expect_equal(result3[1, ], c(-0.26729325, -0.071746001), tolerance = 1e-07, label = paste0("c(", paste0(result3[1, ], collapse = ", "), ")"))
	expect_equal(result3[2, ], c(0.26729325, 0.071746001), tolerance = 1e-07, label = paste0("c(", paste0(result3[2, ], collapse = ", "), ")"))

	design4 <- getDesignInverseNormal(
	    kMax = 2, alpha = 0.025, informationRates = c(0.5, 1),
	    typeOfDesign = "WT", deltaWT = 0.25
	)
	dataExample4 <- getDataset(
	    events1 = c(4, 55),
	    events2 = c(4, 46),
	    n1 = c(30, 300),
	    n2 = c(30, 300)
	)
	result4 <- getRepeatedConfidenceIntervals(design4, dataExample4)

	## Comparison of the results of matrixarray object 'result4' with expected results
	expect_equal(result4[1, ], c(-0.23589449, -0.043528426), tolerance = 1e-07, label = paste0("c(", paste0(result4[1, ], collapse = ", "), ")"))
	expect_equal(result4[2, ], c(0.23589449, 0.088472144), tolerance = 1e-07, label = paste0("c(", paste0(result4[2, ], collapse = ", "), ")"))
})

