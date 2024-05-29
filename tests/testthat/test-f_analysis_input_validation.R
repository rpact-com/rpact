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
## |  File name: test-f_analysis_input_validation.R
## |  Creation date: 08 November 2023, 08:56:03
## |  File version: $Revision: 7928 $
## |  Last changed: $Date: 2024-05-23 16:35:16 +0200 (Do, 23 Mai 2024) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing the Correct Input Validation of All Analysis Functions")


test_that("Errors and warnings for calculation of analysis results with dataset of means", {
	.skipTestIfDisabled()

	design1 <- getDesignInverseNormal(
	    kMax = 4, alpha = 0.02, futilityBounds = c(-0.5, 0, 0.5),
	    bindingFutility = FALSE, typeOfDesign = "asKD", gammaA = 1.2, informationRates = c(0.15, 0.4, 0.7, 1)
	)

	design3 <- getDesignConditionalDunnett(alpha = 0.02, informationAtInterim = 0.4, secondStageConditioning = TRUE)

	dataExample1 <- getDataset(
	    n = c(13, 25),
	    means = c(24.2, 22.2),
	    stDevs = c(24.4, 22.1)
	)

	dataExample2 <- getDataset(
	    n1 = c(13, 25),
	    n2 = c(15, 27),
	    means1 = c(24.2, 22.2),
	    means2 = c(18.8, 27.7),
	    stDevs1 = c(24.4, 22.1),
	    stDevs2 = c(21.2, 23.7)
	)

	dataExample4 <- getDataset(
	    n1 = c(13, 25),
	    n2 = c(15, NA),
	    n3 = c(14, 27),
	    n4 = c(12, 29),
	    means1 = c(24.2, 22.2),
	    means2 = c(18.8, NA),
	    means3 = c(26.7, 27.7),
	    means4 = c(9.2, 12.2),
	    stDevs1 = c(24.4, 22.1),
	    stDevs2 = c(21.2, NA),
	    stDevs3 = c(25.6, 23.2),
	    stDevs4 = c(21.5, 22.7)
	)

	expect_error(getAnalysisResults(
	    design = design1, dataInput = dataExample4,
	    intersectionTest = "", varianceOption = "notPooled", nPlanned = c(20, 20)
	))
	expect_error(getAnalysisResults(
	    design = design1, dataInput = dataExample4,
	    intersectionTest = "Simes", varianceOption = "X", nPlanned = c(20, 20)
	))
	expect_error(getAnalysisResults(
	    design = design1, dataInput = dataExample4,
	    intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = c(20, 20, 30)
	))
	expect_error(getAnalysisResults(
	    design = design1, dataInput = dataExample4,
	    intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = 20
	))
    expect_warning(getAnalysisResults(
	    design = design1, dataInput = dataExample4,
	    intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = c()
	))
	expect_error(getAnalysisResults(
	    design = design1, dataInput = dataExample4,
	    intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = numeric(0)
	))

	expect_error(getAnalysisResults(
	    design = design3, dataInput = dataExample4,
	    intersectionTest = "Dunnett", varianceOption = "pairwisePooled"
	),
	paste0(
	    "Illegal argument: variance option ('pairwisePooled') must be 'overallPooled' ",
	    "because conditional Dunnett test was specified as design"
	),
	fixed = TRUE
	)

	expect_error(getAnalysisResults(
	    design = design1, dataInput = dataExample4,
	    intersectionTest = "Dunnett", varianceOption = "pairwisePooled", nPlanned = c(20, 20)
	),
	"Dunnett t test can only be performed with overall variance estimation",
	fixed = TRUE
	)

	expect_error(getConditionalPower(getStageResults(design1, dataInput = dataExample2),
	    nPlanned = c(20, 20), allocationRatioPlanned = -1
	))
})

