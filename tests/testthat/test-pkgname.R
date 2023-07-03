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
## |  File name: test-f_simulation_multiarm_survival.R
## |  Creation date: 06 February 2023, 12:14:51
## |  File version: $Revision: 7139 $
## |  Last changed: $Date: 2023-06-28 08:15:31 +0200 (Mi, 28 Jun 2023) $
## |  Last changed by: $Author: pahlke $
## |  

test_plan_section("Testing Package Functions")

test_that("pkgname.R", {
  expect_true(is.function(.onAttach))
  expect_true(is.function(.onUnload))
  expect_true(is.function(.onDetach))
})