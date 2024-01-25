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
## |  File name: test-generic_functions.R
## |  Creation date: 08 November 2023, 09:12:05
## |  File version: $Revision: 7560 $
## |  Last changed: $Date: 2024-01-15 14:20:32 +0100 (Mo, 15 Jan 2024) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing Class 'SummaryFactory'")


test_that("Testing 'summary.ParameterSet': no errors occur", {
	.skipTestIfDisabled()

	design <- getDesignGroupSequential(
	    alpha = 0.05, kMax = 4,
	    sided = 1, typeOfDesign = "WT", deltaWT = 0.1
	)

	designFisher <- getDesignFisher(
	    kMax = 4, alpha = 0.025,
	    informationRates = c(0.2, 0.5, 0.8, 1), alpha0Vec = rep(0.4, 3)
	)

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
	    maxNumberOfSubjects = 1200, plannedEvents = c(50, 100, 150, 200), seed = 12345
	)

	piecewiseSurvivalTime <- getPiecewiseSurvivalTime(list(
	    "0 - <6"   = 0.025,
	    "6 - <9"   = 0.04,
	    "9 - <15"  = 0.015,
	    "15 - <21" = 0.01,
	    ">=21"     = 0.007
	), hazardRatio = 0.8)

	accrualTime <- getAccrualTime(list(
	    "0  - <12" = 15,
	    "12 - <13" = 21,
	    "13 - <14" = 27,
	    "14 - <15" = 33,
	    "15 - <16" = 39,
	    ">=16"     = 45
	), maxNumberOfSubjects = 1400)

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

	expect_s3_class(as.data.frame(design, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_s3_class(as.data.frame(designFisher, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_s3_class(as.data.frame(designCharacteristics, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_s3_class(as.data.frame(powerAndASN, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_s3_class(as.data.frame(designSet, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_s3_class(as.data.frame(dataset, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_s3_class(as.data.frame(stageResults, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_s3_class(as.data.frame(designPlan, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_s3_class(as.data.frame(simulationResults, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_s3_class(as.data.frame(piecewiseSurvivalTime, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_s3_class(as.data.frame(accrualTime, niceColumnNamesEnabled = FALSE), "data.frame")

	expect_type(as.matrix(design), "character")
	expect_type(as.matrix(designFisher), "character")
	expect_type(as.matrix(designCharacteristics), "double")
	expect_type(as.matrix(powerAndASN), "double")
	expect_type(as.matrix(designSet), "character")
	expect_type(as.matrix(dataset), "double")
	expect_type(as.matrix(stageResults), "character")
	expect_type(as.matrix(designPlan), "double")
	expect_type(as.matrix(simulationResults), "double")
	expect_type(as.matrix(piecewiseSurvivalTime), "double")
	expect_type(as.matrix(accrualTime), "double")

	suppressWarnings(analysisResults <- getAnalysisResults(design, dataset))
	expect_vector(names(analysisResults))
	expect_output(print(analysisResults))
	expect_output(summary(analysisResults)$show())
	expect_named(as.data.frame(analysisResults))
	expect_s3_class(as.data.frame(analysisResults, niceColumnNamesEnabled = FALSE), "data.frame")
	expect_type(as.matrix(analysisResults), "character")
})

