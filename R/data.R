## |
## |  rpact: Confirmatory Adaptive Clinical Trial Design and Analysis
## |  This file is part of the rpact R package.
## |
## |  RPACT — Statistical Tools for Drug Development
## |
## |  Authors: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  SPDX-FileCopyrightText: RPACT GmbH, Germany <info@rpact.com>
## |  SPDX-License-Identifier: LGPL-3.0-only
## |
## |  Licensed under the GNU Lesser General Public License, version 3.
## |  License text: https://www.gnu.org/licenses/lgpl-3.0.html
## |
## |  Package documentation: https://www.rpact.org
## |  Resources and customer portal: https://connect.rpact.app
## |  Consulting and services: https://www.rpact.com
## |  Contact: info@rpact.com
## |


#' One-Arm Dataset of Means
#'
#' A dataset containing the sample sizes, means, and standard deviations of one group.
#' Use \code{getDataset(dataMeans)} to create a dataset object that can
#' be processed by \code{\link[=getAnalysisResults]{getAnalysisResults()}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#'
#' @keywords internal
#'
"dataMeans"

#' One-Arm Dataset of Rates
#'
#' A dataset containing the sample sizes and events of one group.
#' Use \code{getDataset(dataRates)} to create a dataset object that can
#' be processed by \code{\link[=getAnalysisResults]{getAnalysisResults()}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#'
#' @keywords internal
#'
"dataRates"

#' One-Arm Dataset of Survival Data
#'
#' A dataset containing the log-rank statistics, events, and allocation ratios of one group.
#' Use \code{getDataset(dataSurvival)} to create a dataset object that can b
#' e processed by \code{\link[=getAnalysisResults]{getAnalysisResults()}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#'
#' @keywords internal
#'
"dataSurvival"

## Mulit-arm

#' Multi-Arm Dataset of Means
#'
#' A dataset containing the sample sizes, means, and standard deviations of four groups.
#' Use \code{getDataset(dataMultiArmMeans)} to create a dataset object that can
#' be processed by \code{\link[=getAnalysisResults]{getAnalysisResults()}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#'
#' @keywords internal
#'
"dataMultiArmMeans"

#' Multi-Arm Dataset of Rates
#'
#' A dataset containing the sample sizes and events of three groups.
#' Use \code{getDataset(dataMultiArmRates)} to create a dataset object that can
#' be processed by \code{\link[=getAnalysisResults]{getAnalysisResults()}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#'
#' @keywords internal
#'
"dataMultiArmRates"

#' Multi-Arm Dataset of Survival Data
#'
#' A dataset containing the log-rank statistics, events, and allocation ratios of three groups.
#' Use \code{getDataset(dataMultiArmSurvival)} to create a dataset object that can
#' be processed by \code{\link[=getAnalysisResults]{getAnalysisResults()}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#'
#' @keywords internal
#'
"dataMultiArmSurvival"

## Enrichment

#' Enrichment Dataset of Means
#'
#' A dataset containing the sample sizes, means, and standard deviations of two groups.
#' Use \code{getDataset(dataEnrichmentMeans)} to create a dataset object that can
#' be processed by \code{\link[=getAnalysisResults]{getAnalysisResults()}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#'
#' @keywords internal
#'
"dataEnrichmentMeans"

#' Enrichment Dataset of Rates
#'
#' A dataset containing the sample sizes and events of two groups.
#' Use \code{getDataset(dataEnrichmentRates)} to create a dataset object that can
#' be processed by \code{\link[=getAnalysisResults]{getAnalysisResults()}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#'
#' @keywords internal
#'
"dataEnrichmentRates"

#' Enrichment Dataset of Survival Data
#'
#' A dataset containing the log-rank statistics, events, and allocation ratios of two groups.
#' Use \code{getDataset(dataEnrichmentSurvival)} to create a dataset object that can
#' be processed by \code{\link[=getAnalysisResults]{getAnalysisResults()}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#'
#' @keywords internal
#'
"dataEnrichmentSurvival"

## Enrichment Stratified

#' Stratified Enrichment Dataset of Means
#'
#' A dataset containing the sample sizes, means, and standard deviations of two groups.
#' Use \code{getDataset(dataEnrichmentMeansStratified)} to create a dataset object that can
#' be processed by \code{\link[=getAnalysisResults]{getAnalysisResults()}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#'
#' @keywords internal
#'
"dataEnrichmentMeansStratified"

#' Stratified Enrichment Dataset of Rates
#'
#' A dataset containing the sample sizes and events of two groups.
#' Use \code{getDataset(dataEnrichmentRatesStratified)} to create a dataset object that
#' can be processed by \code{\link[=getAnalysisResults]{getAnalysisResults()}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#'
#' @keywords internal
#'
"dataEnrichmentRatesStratified"

#' Stratified Enrichment Dataset of Survival Data
#'
#' A dataset containing the log-rank statistics, events, and allocation ratios of two groups.
#' Use \code{getDataset(dataEnrichmentSurvivalStratified)} to create a dataset object that
#' can be processed by \code{\link[=getAnalysisResults]{getAnalysisResults()}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#'
#' @keywords internal
#'
"dataEnrichmentSurvivalStratified"

#'
#' @title
#' Raw Dataset Of A Two Arm Continuous Outcome With Covariates
#'
#' @description
#' An artificial dataset that was randomly generated
#' with simulated normal data. The data set has six variables:
#'
#' 1. Subject id
#' 2. Stage number
#' 3. Group name
#' 4. An example outcome in that we are interested in
#' 5. The first covariate *gender*
#' 6. The second covariate *covariate*
#'
#' @details
#' See the vignette "Two-arm analysis for continuous data with covariates from raw data"
#' to learn how to
#'
#' * import raw data from a csv file,
#' * calculate estimated adjusted (marginal) means (EMMs, least-squares means) for a linear model, and
#' * perform two-arm interim analyses with these data.
#'
#' You can use \code{rawDataTwoArmNormal} to reproduce the examples in the vignette.
#'
#' @format A \code{\link[base]{data.frame}} object.
#'
#' @keywords internal
#'
"rawDataTwoArmNormal"
