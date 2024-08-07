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
## |  File version: $Revision: 7742 $
## |  Last changed: $Date: 2024-03-22 13:46:29 +0100 (Fr, 22 Mrz 2024) $
## |  Last changed by: $Author: pahlke $
## |

getAssertionTestDesign <- function(..., kMax = NA_integer_, informationRates = NA_real_, futilityBounds = NA_real_,
        designClass = "TrialDesignInverseNormal") {
    if (designClass == "TrialDesignFisher") {
        return(TrialDesignFisher$new(
            kMax = kMax,
            alpha = 0.025,
            method = "equalAlpha",
            alpha0Vec = futilityBounds,
            informationRates = informationRates,
            tolerance = 1e-14,
            iterations = 0,
            seed = 9498485
        ))
    }

    return(.createDesign(
        designClass             = designClass,
        kMax                    = kMax,
        alpha                   = 0.025,
        beta                    = 0.2,
        sided                   = 1,
        informationRates        = informationRates,
        futilityBounds          = futilityBounds,
        typeOfDesign            = "OF",
        delta                   = 0,
        optimizationCriterion   = "ASNH1",
        gammaA                  = 1,
        typeBetaSpending        = "none",
        userAlphaSpending       = NA_real_,
        userBetaSpending        = NA_real_,
        gammaB                  = 1,
        tolerance               = 1e-06
    ))
}
