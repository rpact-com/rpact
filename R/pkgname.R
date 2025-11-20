## |
## |  *rpact*
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

#'
#' @title
#' rpact - Confirmatory Adaptive Clinical Trial Design and Analysis
#'
#' @description
#' rpact (R Package for Adaptive Clinical Trials) is a comprehensive package that enables
#' the design, simulation, and analysis of confirmatory adaptive group sequential designs.
#' Particularly, the methods described in the recent monograph by Wassmer and Brannath
#' (published by Springer, 2025) are implemented. It also comprises advanced methods for sample
#' size calculations for fixed sample size designs incl., e.g., sample size calculation for survival
#' trials with piecewise exponentially distributed survival times and staggered patients entry.
#'
#' @details
#' rpact includes the classical group sequential designs (incl. user spending function approaches)
#' where the sample sizes per stage (or the time points of interim analysis) cannot be changed
#' in a data-driven way.
#' Confirmatory adaptive designs explicitly allow for this under control of the Type I error rate.
#' They are either based on the combination testing or the conditional rejection
#' probability (CRP) principle.
#' Both are available, for the former the inverse normal combination test and
#' Fisher's combination test can be used.
#'
#' Specific techniques of the adaptive methodology are also available, e.g.,
#' overall confidence intervals, overall p-values, and conditional and predictive power assessments.
#' Simulations can be performed to assess the design characteristics of a (user-defined) sample size
#' recalculation strategy. Designs are available for trials with continuous, binary, and survival endpoint.
#'
#' For more information please visit \href{https://www.rpact.org}{www.rpact.org}.
#' If you are interested in professional services round about the package or need
#' a comprehensive validation documentation to fulfill regulatory requirements
#' please visit \href{https://www.rpact.com}{www.rpact.com}.
#'
#' rpact is developed by
#' \itemize{
#'   \item Gernot Wassmer (\email{gernot.wassmer@@rpact.com}) and
#'   \item Friedrich Pahlke (\email{friedrich.pahlke@@rpact.com}).
#' }
#'
#' @references
#' Wassmer, G., Brannath, W. (2025) Group Sequential and Confirmatory Adaptive Designs
#' in Clinical Trials (Springer Series in Pharmaceutical Statistics; \doi{10.1007/978-3-031-89669-9})
#'
#' @docType package
#' @author Gernot Wassmer, Friedrich Pahlke
#' @importFrom Rcpp evalCpp
#' @useDynLib rpact,.registration = TRUE
#' @name rpact
#'
#' @import methods
#' @import stats
#' @import utils
#' @import graphics
#' @import tools
#' @importFrom rlang .data
#' @importFrom knitr kable
#' @importFrom knitr knit_print
#'
"_PACKAGE"
#> [1] "_PACKAGE"

.onLoad <- function(libname, pkgname) {
    .loadOptions()
}

.onAttach <- function(libname, pkgname) {
    if (grepl("^\\d\\.\\d\\.\\d\\.\\d{4,4}$", .getPackageVersionString())) {
        packageStartupMessage(paste0("rpact developer version ", 
            .getPackageVersionString(), " loaded"))
    } 
    if (!isTRUE(.installationQualificationDone()) && 
            .isStartupMessagingEnabled()) {
        packageStartupMessage(paste0("Installation qualification for rpact ", 
            .getPackageVersionString(), " has not yet been performed."))
        packageStartupMessage(paste0("Please run testPackage() before ",
            "using the package in GxP relevant environments."))
    }
}

.onDetach <- function(libpath) {
    if (grepl("^\\d\\.\\d\\.\\d\\.\\d{4,4}$", .getPackageVersionString())) {
        packageStartupMessage(paste0("rpact developer version ", .getPackageVersionString(), 
            " successfully unloaded"))
    }
}

.onUnload <- function(libpath) {
    saveOptions()
    tryCatch(
        {
            library.dynam.unload("rpact", libpath)
        },
        error = function(e) {
            .logWarn("Failed to unload dynamic C library", e)
        }
    )
}

