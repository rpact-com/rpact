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
## |  File name: test-class_analysis_dataset.R
## |  Creation date: 06 February 2023, 12:04:06
## |  File version: $Revision: 7139 $
## |  Last changed: $Date: 2023-06-28 08:15:31 +0200 (Mi, 28 Jun 2023) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing the Class 'PlotSettings'")

test_that("Test plot settings", {
  expect_error(PlotSubTitleItem())
  
  expect_type(PlotSubTitleItems(), "S4")
  
  expect_type(getPlotSettings(), "S4")
  
  expect_type(PlotSettings(), "S4")
})