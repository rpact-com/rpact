 <!-- badges: start -->
[![CRAN Status](https://www.r-pkg.org/badges/version/rpact)](https://cran.r-project.org/package=rpact)
[![R-CMD-check](https://github.com/rpact-com/rpact/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rpact-com/rpact/actions/workflows/R-CMD-check.yaml)
[![Total downloads](https://cranlogs.r-pkg.org/badges/grand-total/rpact?color=blue)](https://CRAN.R-project.org/package=rpact)
[![Monthly downloads](https://cranlogs.r-pkg.org/badges/rpact?color=blue)](https://CRAN.R-project.org/package=rpact)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: LGPL v3](https://img.shields.io/badge/License-LGPL_v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![shinyapps.io](https://img.shields.io/badge/Shiny-shinyapps.io-blue?style=flat&labelColor=white&logo=RStudio&logoColor=blue)](https://rpact.shinyapps.io/cloud/)
 <!-- badges: end -->

# rpact

Confirmatory Adaptive Clinical Trial Design, Simulation, and Analysis.

## Functional Range

-   Fixed sample design and designs with interim analysis stages
-   Sample size and power calculation for
    -   means (continuous endpoint)
    -   rates (binary endpoint)
    -   survival trials with flexible recruitment and survival time
        options
    -   count data
-   Simulation tool for means, rates, and survival data
    -   Assessment of adaptive sample size/event number recalculations
        based on conditional power
    -   Assessment of treatment selection strategies in multi-arm trials
-   Adaptive analysis of means, rates, and survival data
-   Adaptive designs and analysis for multi-arm trials
-   Adaptive analysis and simulation tools for enrichment design testing
    means, rates, and hazard ratios
-   Automatic boundary recalculations during the trial for analysis with
    alpha spending approach, including under- and over-running

## Installation

Install the latest CRAN release via

``` r
install.packages("rpact")
```

## Development version

To use a feature from the development version, you can install the
development version of rpact from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("rpact-com/rpact", ref = "dev/3.5.0")
```

## Documentation

The documentation is hosted at <https://www.rpact.org>

## Vignettes

The vignettes are hosted at
[www.rpact.org/vignettes](https://www.rpact.org/vignettes/)

## The rpact user group

The *rpact project* has an active user group consisting of
decision-makers and users from the pharmaceutical industry and CROs, who
meet regularly and, e.g., discuss best practices.

We invite you to be part of the *rpact user group*: benefit from
know-how, shape open source development in Pharma!

## Use on corporate computer systems

Please [contact](https://www.rpact.com/contact) us to learn how to use
`rpact` on FDA/GxP-compliant validated corporate computer systems and
how to get a copy of the formal validation documentation that is
customized and licensed for exclusive use by your company, e.g., to
fulfill regulatory requirements. The validation documentation contains
the personal access data for performing the installation qualification
with `testPackage()`.

> [www.rpact.com/contact](https://www.rpact.com/contact)

# About

-   **rpact** is a comprehensive validated[^1] R package for clinical
    research which
    -   enables the design and analysis of confirmatory adaptive group
        sequential designs
    -   is a powerful sample size calculator
    -   is a free of charge open-source software licensed under
        [LGPL-3](https://cran.r-project.org/web/licenses/LGPL-3)
    -   particularly, implements the methods described in the recent
        monograph by [Wassmer and Brannath
        (2016)](https://doi.org/10.1007%2F978-3-319-32562-0)

> For more information please visit
> [www.rpact.org](https://www.rpact.org)

-   **RPACT** is a company which offers
    -   enterprise R/Shiny software development services
    -   technical support for the
        [rpact](https://cran.r-project.org/package=rpact) package
    -   consultancy and user training for scientists using R
    -   validated software solutions and R package development for
        clinical research

> For more information please visit
> [www.rpact.com](https://www.rpact.com)

[^1]: The rpact validation documentation is available exclusively for
    our customers and supporting members. For more information visit
    [www.rpact.com/services/sla](https://www.rpact.com/services/sla)
