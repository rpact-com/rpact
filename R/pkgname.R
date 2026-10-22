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
#' rpact (R Package for Adaptive Clinical Trials) supports the design, simulation,
#' and analysis of confirmatory clinical trials. It provides sample size and power
#' calculations for standard fixed-sample and classical group sequential designs,
#' with methods for continuous, binary, survival (time-to-event), and count data endpoints.
#' Adaptive methods include multi-stage combination tests, sample size reassessment,
#' multi-arm multi-stage (MAMS) designs, and population enrichment designs.
#' The package implements a broad range of methods described by Wassmer and
#' Brannath (2025).
#'
#' @details
#' Planning and simulation functions are available for means, rates, survival,
#' and count data. Survival planning supports staggered recruitment and piecewise
#' exponentially distributed survival times. Multi-arm and population enrichment
#' simulation and analysis methods are available for means, rates, and survival
#' endpoints. General estimates and standard errors can also be analyzed through
#' [getDataset()] and [getAnalysisResults()].
#'
#' Classical group sequential designs specify rules for interim and final analyses.
#' Alpha-spending approaches allow boundary recalculation for observed information,
#' including under- and over-running. Adaptations based on interim treatment effects
#' require methods that preserve Type I error control, such as combination testing
#' or the conditional rejection probability (CRP) principle. The package provides
#' inverse normal and Fisher combination tests and conditional Dunnett methods.
#' Simulations can assess the operating characteristics of sample size or
#' event-number reassessment strategies.
#'
#' Analysis methods provide p-values, conditional power, and confidence intervals
#' where supported by the selected design and endpoint. In particular, final
#' confidence intervals and median unbiased estimates are not available for
#' Fisher combination tests or multi-arm and enrichment analyses. See
#' [getAnalysisResults()] for further conditions and limitations.
#' [getFutilityBounds()] converts futility boundaries between different scales,
#' including conditional and predictive power scales.
#'
#' rpact is free of charge and open source under the LGPL-3 license.
#' Package information and tutorials are available at
#' \href{https://www.rpact.org}{www.rpact.org}; function documentation is available at
#' \href{https://docs.rpact.org}{docs.rpact.org}.
#'
#' RPACT GmbH develops Statistical Tools for Drug Development and offers software
#' development, validation, technical support, consulting, and training.
#' Formal validation documentation is available to customers and supporting
#' members. It includes credentials for the full installation qualification test
#' suite run by [testPackage()]. For services, see
#' \href{https://www.rpact.com}{www.rpact.com}. Browser-based tools are available at
#' \href{https://demo.rpact.cloud}{demo.rpact.cloud} (RPACT Cloud) and
#' \href{https://connect.rpact.app}{connect.rpact.app} (RPACT Connect).
#'
#' rpact is developed by
#' \itemize{
#'   \item Gernot Wassmer (\email{gernot.wassmer@@rpact.com}), 
#'   \item Friedrich Pahlke (\email{friedrich.pahlke@@rpact.com}), and 
#'   \item Daniel Sabanés Bové (\email{daniel.sabanes_bove@@rpact.com}).
#' }
#'
#' @references
#' Wassmer, G., Brannath, W. (2025) Group Sequential and Confirmatory Adaptive Designs
#' in Clinical Trials (Springer Series in Pharmaceutical Statistics; \doi{10.1007/978-3-031-89669-9})
#'
#' @seealso
#' \href{https://www.rpact.org}{Package website},
#' \href{https://docs.rpact.org}{Function reference},
#' \href{https://demo.rpact.cloud}{RPACT Cloud},
#' \href{https://connect.rpact.app}{RPACT Connect},
#' \href{https://www.rpact.com}{RPACT GmbH}, and
#' \href{https://github.com/rpact-com/rpact/issues}{Bug reports}.
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
        packageStartupMessage(paste0(
            "rpact developer version ",
            .getPackageVersionString(), " loaded"
        ))
    }
    if (!isTRUE(.installationQualificationDone()) &&
            .isStartupMessagingEnabled()) {
        for (msg in .getInstallationQualificationMissingText()) {
            packageStartupMessage(msg)
        }
    }
}

.onDetach <- function(libpath) {
    if (grepl("^\\d\\.\\d\\.\\d\\.\\d{4,4}$", .getPackageVersionString())) {
        packageStartupMessage(paste0(
            "rpact developer version ", .getPackageVersionString(),
            " successfully unloaded"
        ))
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
