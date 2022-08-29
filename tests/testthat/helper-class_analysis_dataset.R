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

getMultipleStageResultsForDataset <- function(dataset, thetaH0 = NA_real_) {
    stage <- dataset$getNumberOfStages()
    kMax <- stage + 1

    design1 <- getDesignGroupSequential(kMax = kMax)
    design2 <- getDesignInverseNormal(kMax = kMax)
    design3 <- getDesignFisher(kMax = kMax)

    stageResults1 <- getStageResults(design = design1, dataInput = dataset, stage = stage, thetaH0 = thetaH0)
    stageResults2 <- getStageResults(design = design2, dataInput = dataset, stage = stage, thetaH0 = thetaH0)
    stageResults3 <- getStageResults(design = design3, dataInput = dataset, stage = stage, thetaH0 = thetaH0)

    return(list(
        stageResults1 = stageResults1,
        stageResults2 = stageResults2,
        stageResults3 = stageResults3
    ))
}

