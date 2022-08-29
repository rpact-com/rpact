## |
## |  *Unit tests helper functions*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |
## |  RPACT company website: https://www.rpact.com
## |  rpact package website: https://www.rpact.org
## |
## |  Contact us for information about our services: info@rpact.com
## |
## |  File version: $Revision: 6117 $
## |  Last changed: $Date: 2022-05-04 15:55:23 +0200 (Mi, 04 Mai 2022) $
## |  Last changed by: $Author: pahlke $
## |

testGetStageResultsPlotData <- function(x, ..., nPlanned,
        stage = NA_integer_, allocationRatioPlanned = 1) {
    if (x$getDataInput()$isDatasetMeans()) {
        assumedStDev <- .getOptionalArgument("assumedStDev", ...)
        if (is.null(assumedStDev)) {
            assumedStDev <- x$assumedStDev
            return(.getConditionalPowerPlot(
                stageResults = x,
                nPlanned = nPlanned,
                allocationRatioPlanned = allocationRatioPlanned,
                assumedStDev = assumedStDev, ...
            ))
        }
    } else if (x$getDataInput()$isDatasetRates()) {
        pi2 <- .getOptionalArgument("pi2", ...)
        if (is.null(pi2)) {
            pi2 <- x$pi2
            return(.getConditionalPowerPlot(
                stageResults = x,
                nPlanned = nPlanned,
                allocationRatioPlanned = allocationRatioPlanned,
                pi2 = pi2, ...
            ))
        }
    }

    return(.getConditionalPowerPlot(
        stageResults = x,
        nPlanned = nPlanned,
        allocationRatioPlanned = allocationRatioPlanned, ...
    ))
}
