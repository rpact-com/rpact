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
## |  File name: test-f_core_plot.R
## |  Creation date: 08 November 2023, 09:09:36
## |  File version: $Revision: 7583 $
## |  Last changed: $Date: 2024-01-19 18:29:57 +0100 (Fr, 19 Jan 2024) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing .reconstructSequenceCommand")


test_that("The output is as exptected", {
	expect_equal(.reconstructSequenceCommand(seq(-1, 1, 0.02)), "seq(-1, 1, 0.02)")
	expect_equal(.reconstructSequenceCommand(c()), NA_character_)
	expect_equal(.reconstructSequenceCommand(c(1)), "1")
	expect_equal(.reconstructSequenceCommand(c(1, 2)), "c(1, 2)")
	expect_equal(.reconstructSequenceCommand(c(1, 2, 3)), "c(1, 2, 3)")
	expect_equal(.reconstructSequenceCommand(c(1, 2, 3, 4)), "seq(1, 4, 1)")
	expect_equal(.reconstructSequenceCommand(c(1, 2, 3, 5)), "c(1, 2, 3, 5)")

	expect_true(grepl(.getRexepSaveCharacter("x$.design"), "x$.design"))
	expect_true(grepl(.getRexepSaveCharacter("x$.design"), "c(x$.design, xxx)"))
	expect_false(grepl(.getRexepSaveCharacter("x$.design"), "c(x$design, xxx)"))

})

test_that("Internal core plot functions throw errors when arguments are missing or wrong", {

	expect_equal(.addNumberToPlotCaption(caption = "hello", type = "character"), "hello")

	expect_error(.getPlotCaption())

	expect_error(.getPlotTypeNumber())
	expect_error(.getPlotTypeNumber(type = "test"))

	expect_error(.createPlotResultObject())
	expect_error(.createPlotResultObject(list(x = 1), grid = -1))
	expect_error(.createPlotResultObject(list(x = 1), grid = 101))
	expect_error(.createPlotResultObject(list(x = 1), grid = 101))

	expect_error(.printPlotShowSourceSeparator())

	expect_error(plotTypes())

	expect_error(.isValidVariedParameterVectorForPlotting())

	expect_error(.removeInvalidPlotTypes())

	expect_error(getAvailablePlotTypes())

	expect_error(.getVariedParameterHint())

	expect_error(.createValidParameterName())
	expect_equal(.createValidParameterName(NULL, "param"), "param")
	expect_equal(.createValidParameterName("object", "param"), "object$param")

	expect_null(.showPlotSourceInformation())

	expect_error(.testPlotCommand())

	expect_error(.getParameterSetAsDataFrame())

	expect_error(.getCategories())

	expect_error(.getAxisLabel())
	expect_equal(.getAxisLabel("heho", NULL), "%heho%")

	expect_error(.allGroupValuesEqual())

	expect_error(.plotParameterSet())

	expect_error(.naAndNaNOmit())
	expect_null(.naAndNaNOmit(NULL))

	expect_error(.getScalingFactors())

	expect_error(.plotDataFrame())

	expect_error(.getPointBorder())

	expect_error(.getLegendPosition())

	expect_error(.addQnormAlphaLine())

	expect_equal(.getLambdaStepFunctionByTime(3, NA, 5), 5)

	expect_error(.getLambdaStepFunction())

	expect_error(getLambdaStepFunction())

	expect_type(.getRelativeFigureOutputPath(), "character")

	expect_error(saveLastPlot())

	expect_error(.getGridPlotSettings())

	expect_error(.getGridLegendPosition())

	expect_error(.formatSubTitleValue())
})

