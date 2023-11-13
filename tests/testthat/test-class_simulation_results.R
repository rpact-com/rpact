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
## |  File version: $Revision: 7403 $
## |  Last changed: $Date: 2023-11-08 16:12:00 +0100 (Mi, 08 Nov 2023) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing the Class 'SimulationResults'")

test_that("Test that simulation result class generics and utility functions throw errors outside of context", {
    expect_error(.assertIsValidVariedParameterVectorForSimulationResultsPlotting())
    expect_error(.getSimulationPlotXAxisParameterName())
    expect_error(.getSimulationPlotXAxisLabel())
    expect_error(.getPowerAndStoppingProbabilities())
    expect_error(.plotSimulationResults())
    expect_error(plot.SimulationResults())
    expect_error(getData(1))
    expect_error(.getData.SimulationResults())
    expect_error(.getAggregatedDataByIterationNumber())
    expect_error(.getAggregatedData())
    expect_error(getRawData())
})
