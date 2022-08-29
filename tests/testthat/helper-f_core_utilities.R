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
## |  File version: $Revision: 6291 $
## |  Last changed: $Date: 2022-06-13 08:36:13 +0200 (Mo, 13 Jun 2022) $
## |

getTestInformationRatesDefault <- function(kMax) {
    return((1:kMax) / kMax)
}

getTestFutilityBoundsDefault <- function(kMax) {
    return(rep(-6, kMax - 1))
}

getTestAlpha0VecDefault <- function(kMax) {
    return(rep(1, kMax - 1))
}

getTestInformationRates <- function(kMax) {
    if (kMax == 1L) {
        return(1)
    }
    
    if (kMax == 6L) {
        return(c(0.1333, 0.26667, 0.4, 0.53333, 0.8667, 1))
    }
    
    a <- 0.8 / kMax
    
    b <- c()
    for (i in 1:(kMax - 1)) {
        b <- c(b, a * i)
    }
    
    return(c(b, 1))
}

getTestFutilityBounds <- function(kMax, fisherDesignEnabled = FALSE) {
    if (kMax < 2) {
        stop("Illegal argument: 'kMax' must be >= 2")
    }

    if (kMax == 2 && fisherDesignEnabled) {
        return(0.5)
    }

    k <- kMax - 1
    futilityBounds <- c(2)
    k <- k - 1
    if (k > 0) {
        futilityBounds <- c(1, futilityBounds)
        k <- k - 1
    }
    if (k > 0) {
        futilityBounds <- c(rep(0, k), futilityBounds)
    }

    if (fisherDesignEnabled) {
        futilityBounds[futilityBounds > 0] <- futilityBounds[futilityBounds > 0] / max(futilityBounds)
        futilityBounds[futilityBounds == 0] <- 0.01
    }

    return(futilityBounds)
}

getTestDesign <- function(kMax = NA_integer_, informationRates = NA_real_, futilityBounds = NA_real_,
        designClass = "TrialDesignInverseNormal") {
    design <- NULL

    currentWarningOption <- getOption("warn")
    options(warn = -1)
    if (designClass == "TrialDesignFisher") {
        design <- getDesignFisher(
            kMax = as.integer(kMax),
            alpha0Vec = futilityBounds,
            informationRates = informationRates
        )
    } else if (designClass == "TrialDesignInverseNormal") {
        design <- getDesignGroupSequential(
            kMax             = as.integer(kMax),
            informationRates = informationRates,
            futilityBounds   = futilityBounds,
            tolerance        = 1e-06
        )
    } else {
        design <- getDesignInverseNormal(
            kMax             = as.integer(kMax),
            informationRates = informationRates,
            futilityBounds   = futilityBounds,
            tolerance        = 1e-06
        )
    }
    options(warn = currentWarningOption)

    return(design)
}
