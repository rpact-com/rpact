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
## |  File name: test-rpact.R
## |  Creation date: 21 April 2021, 15:04:49
## |  File version: $Revision: 5577 $
## |  Last changed: $Date: 2021-11-19 09:14:42 +0100 (Fr, 19 Nov 2021) $
## |  Last changed by: $Author: pahlke $
## |  


context("Testing the rpact package")

test_that("'getDesignInverseNormal' with default parameters: parameters and results are as expected", {
	designInverseNormal <- getDesignInverseNormal()
	
	expect_equal(designInverseNormal$alphaSpent, c(0.00025917372, 0.0071600594, 0.02499999), tolerance = 1e-07)
	expect_equal(designInverseNormal$criticalValues, c(3.4710914, 2.4544323, 2.0040356), tolerance = 1e-07)
	expect_equal(designInverseNormal$stageLevels, c(0.00025917372, 0.0070553616, 0.022533125), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
    	invisible(capture.output(expect_error(print(designInverseNormal), NA)))
    	expect_output(print(designInverseNormal)$show())
    	invisible(capture.output(expect_error(summary(designInverseNormal), NA)))
    	expect_output(summary(designInverseNormal)$show())
	}
})

test_that("'getDesignFisher' with default parameters: parameters and results are as expected", {
	designFisher <- getDesignFisher()
	
	expect_equal(designFisher$alphaSpent, c(0.012308547, 0.01962413, 0.025), tolerance = 1e-07)
	expect_equal(designFisher$criticalValues, c(0.012308547, 0.0016635923, 0.00029106687), tolerance = 1e-07)
	expect_equal(designFisher$stageLevels, c(0.012308547, 0.012308547, 0.012308547), tolerance = 1e-07)
	expect_equal(designFisher$scale, c(1, 1))
	expect_equal(designFisher$nonStochasticCurtailment, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
    	invisible(capture.output(expect_error(print(designFisher), NA)))
    	expect_output(print(designFisher)$show())
    	invisible(capture.output(expect_error(summary(designFisher), NA)))
    	expect_output(summary(designFisher)$show())
	}
})

test_that("Testing 'getPiecewiseSurvivalTime': simple vector based definition", {
	pwSurvivalTime1 <- getPiecewiseSurvivalTime(lambda2 = 0.5, hazardRatio = 0.8)
	
	expect_equal(pwSurvivalTime1$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime1$lambda1, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$lambda2, 0.5, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$pi1, NA_real_)
	expect_equal(pwSurvivalTime1$pi2, NA_real_)
	expect_equal(pwSurvivalTime1$median1, 1.732868, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$median2, 1.3862944, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$eventTime, NA_real_)
	expect_equal(pwSurvivalTime1$kappa, 1)
	expect_equal(pwSurvivalTime1$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime1$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime1$delayedResponseEnabled, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
    	invisible(capture.output(expect_error(print(pwSurvivalTime1), NA)))
    	expect_output(print(pwSurvivalTime1)$show())
    	invisible(capture.output(expect_error(summary(pwSurvivalTime1), NA)))
    	expect_output(summary(pwSurvivalTime1)$show())
	}
})

test_that("'getSampleSizeMeans': Sample size calculation of testing means for one sided group sequential design", {
	designGS1pretest <- getDesignGroupSequential(informationRates = c(0.2,0.5,1), sided = 1, 
		beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3)
	
	expect_equal(designGS1pretest$alphaSpent, c(0.0020595603, 0.0098772988, 0.02499999), tolerance = 1e-07)
	expect_equal(designGS1pretest$criticalValues, c(2.8688923, 2.3885055, 2.0793148), tolerance = 1e-07)
	expect_equal(designGS1pretest$stageLevels, c(0.0020595603, 0.0084585282, 0.018794214), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
    	invisible(capture.output(expect_error(print(designGS1pretest), NA)))
    	expect_output(print(designGS1pretest)$show())
    	invisible(capture.output(expect_error(summary(designGS1pretest), NA)))
    	expect_output(summary(designGS1pretest)$show())
	}
	
	designGS1 <- getDesignGroupSequential(informationRates = c(0.2,0.5,1), sided = 1, 
		beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3)
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 1, thetaH0 = 0.5, 
		stDev = 2, normalApproximation = FALSE, alternative = 0.8)
	
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 494.6455, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 98.929099, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 247.32275, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 494.6455, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 491.89699, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 462.87248, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 360.24062, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 1.090771, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.80583608, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.68748891, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
    	invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
    	expect_output(print(sampleSizeResult)$show())
    	invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
    	expect_output(summary(sampleSizeResult)$show())
	}
})

test_that("Testing generic functions: no errors occur", {
	.skipTestIfDisabled()
	
	design <- getDesignGroupSequential(alpha = 0.05, kMax = 4, 
		sided = 1, typeOfDesign = "WT", deltaWT = 0.1)
	
	designFisher <- getDesignFisher(kMax = 4, alpha = 0.025, 
		informationRates = c(0.2, 0.5, 0.8, 1), alpha0Vec = rep(0.4, 3))
	
	designCharacteristics <- getDesignCharacteristics(design)
	
	powerAndASN <- getPowerAndAverageSampleNumber(design, theta = 1, nMax = 100)
	
	designSet <- getDesignSet(design = design, deltaWT = c(0.3, 0.4))
	
	dataset <- getDataset(
		n1 = c(22, 11, 22, 11),
		n2 = c(22, 13, 22, 13),
		means1 = c(1, 1.1, 1, 1),
		means2 = c(1.4, 1.5, 3, 2.5),
		stDevs1 = c(1, 2, 2, 1.3),
		stDevs2 = c(1, 2, 2, 1.3)
	)
	
	stageResults <- getStageResults(design, dataset)
	
	suppressWarnings(designPlan <- getSampleSizeMeans(design))
	
	simulationResults <- getSimulationSurvival(design, 
		maxNumberOfSubjects = 1200, plannedEvents = c(50, 100, 150, 200), seed = 12345)
	
	piecewiseSurvivalTime <- getPiecewiseSurvivalTime(list(
			"0 - <6"   = 0.025, 
			"6 - <9"   = 0.04, 
			"9 - <15"  = 0.015, 
			"15 - <21" = 0.01, 
			">=21"     = 0.007), hazardRatio = 0.8)
	
	accrualTime <- getAccrualTime(list(
			"0  - <12" = 15,
			"12 - <13" = 21,
			"13 - <14" = 27,
			"14 - <15" = 33,
			"15 - <16" = 39,
			">=16"     = 45), maxNumberOfSubjects = 1400)
	
	expect_vector(names(design))
	expect_vector(names(designFisher))
	expect_vector(names(designCharacteristics))
	expect_vector(names(powerAndASN))
	expect_vector(names(designSet))
	expect_vector(names(dataset))
	expect_vector(names(stageResults))
	expect_vector(names(designPlan))
	expect_vector(names(simulationResults))
	expect_vector(names(piecewiseSurvivalTime))
	expect_vector(names(accrualTime))
	
	expect_output(print(design))
	expect_output(print(designFisher))
	expect_output(print(designCharacteristics))
	expect_output(print(powerAndASN))
	expect_output(print(designSet))
	expect_output(print(dataset))
	expect_output(print(stageResults))
	expect_output(print(designPlan))
	expect_output(print(simulationResults))
	expect_output(print(piecewiseSurvivalTime))
	expect_output(print(accrualTime))
	
	expect_output(summary(design)$show())
	expect_output(summary(designFisher)$show())
	expect_output(summary(designCharacteristics)$show())
	expect_output(summary(powerAndASN))
	expect_output(print(summary(designSet)))
	expect_output(summary(dataset)$show())
	expect_output(summary(stageResults))
	expect_output(summary(designPlan)$show())
	expect_output(summary(simulationResults)$show())
	expect_output(summary(piecewiseSurvivalTime))
	expect_output(summary(accrualTime))
	
	expect_named(as.data.frame(design))
	expect_named(as.data.frame(designFisher))
	expect_named(as.data.frame(designCharacteristics))
	expect_named(as.data.frame(powerAndASN))
	expect_named(as.data.frame(designSet))
	expect_named(as.data.frame(dataset))
	expect_named(as.data.frame(stageResults))
	expect_named(as.data.frame(designPlan))
	expect_named(as.data.frame(simulationResults))
	expect_named(as.data.frame(piecewiseSurvivalTime))
	expect_named(as.data.frame(accrualTime))
	
	expect_is(as.data.frame(design, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_is(as.data.frame(designFisher, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_is(as.data.frame(designCharacteristics, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_is(as.data.frame(powerAndASN, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_is(as.data.frame(designSet, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_is(as.data.frame(dataset, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_is(as.data.frame(stageResults, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_is(as.data.frame(designPlan, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_is(as.data.frame(simulationResults, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_is(as.data.frame(piecewiseSurvivalTime, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_is(as.data.frame(accrualTime, niceColumnNamesEnabled = FALSE), "data.frame")
	
	expect_is(as.matrix(design), "matrix")
	expect_is(as.matrix(designFisher), "matrix")
	expect_is(as.matrix(designCharacteristics), "matrix")
	expect_is(as.matrix(powerAndASN), "matrix")
	expect_is(as.matrix(designSet), "matrix")
	expect_is(as.matrix(dataset), "matrix")
	expect_is(as.matrix(stageResults), "matrix")
	expect_is(as.matrix(designPlan), "matrix")
	expect_is(as.matrix(simulationResults), "matrix")
	expect_is(as.matrix(piecewiseSurvivalTime), "matrix")
	expect_is(as.matrix(accrualTime), "matrix")
	
	suppressWarnings(analysisResults <- getAnalysisResults(design, dataset))
	expect_vector(names(analysisResults))
	expect_output(print(analysisResults))
	expect_output(summary(analysisResults)$show())
	expect_named(as.data.frame(analysisResults))
	expect_is(as.data.frame(analysisResults, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_is(as.matrix(analysisResults), "matrix")
	
})