# rpact

[![](https://www.r-pkg.org/badges/version/rpact)](https://cran.r-project.org/package=rpact)
[![R-CMD-check](https://github.com/rpact-com/rpact/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rpact-com/rpact/actions/workflows/R-CMD-check.yaml)
[![Total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/rpact?color=blue)](https://CRAN.R-project.org/package=rpact)
[![Monthly
downloads](https://cranlogs.r-pkg.org/badges/rpact?color=blue)](https://CRAN.R-project.org/package=rpact)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: LGPL
v3](https://img.shields.io/badge/License-LGPL_v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Codecov test
coverage](https://codecov.io/gh/rpact-com/rpact/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rpact-com/rpact?branch=main)
[![](https://img.shields.io/badge/DOI-10.1007/978--3--031--89669--9-yellow.svg?logo=doi)](https://doi.org/10.1007/978-3-031-89669-9)
[![](https://img.shields.io/badge/RPACT-Cloud-blue.svg?logo=r)](https://rpact-cloud.share.connect.posit.cloud)
[![](https://img.shields.io/badge/RPACT-Connect-669999.svg?labelColor=336699)](https://rpact-connect.share.connect.posit.cloud)

Confirmatory Adaptive Clinical Trial Design, Simulation, and Analysis.

## Functional Range

- Fixed sample design and designs with interim analysis stages
- Sample size and power calculation for
  - means (continuous endpoint)
  - rates (binary endpoint)
  - survival trials with flexible recruitment and survival time options
  - count data
- Simulation tool for means, rates, survival data, and count data
  - Assessment of adaptive sample size/event number recalculations based
    on conditional power
  - Assessment of treatment selection strategies in multi-arm trials
- Adaptive analysis of means, rates, and survival data
- Adaptive designs and analysis for multi-arm trials
- Adaptive analysis and simulation tools for enrichment design testing
  means, rates, and hazard ratios
- Automatic boundary recalculations during the trial for analysis with
  alpha spending approach, including under- and over-running

## Installation

Install the latest CRAN release via

``` r
install.packages("rpact")
```

### Development version

To use a feature from the development version, you can install the
development version of rpact from
[GitHub](https://github.com/rpact-com/rpact).

``` r
# install.packages("pak")
pak::pak("rpact-com/rpact")
```

## Documentation

The documentation is hosted at [www.rpact.org](https://www.rpact.org)

## Vignettes

The vignettes are hosted at
[www.rpact.org/vignettes](https://www.rpact.org/vignettes/)

## RPACT Connect

Connecting you to insights, downloads, and premium support:
[connect.rpact.com](https://rpact-connect.share.connect.posit.cloud)

## The RPACT User Group

The *rpact project* has an active user group consisting of
decision-makers and users from the pharmaceutical industry and CROs, who
meet regularly and, e.g., discuss best practices.

We invite you to be part of the *RPACT User Group*: benefit from
know-how, shape open source development in Pharma!

## Use on corporate computer systems

Please [contact](https://www.rpact.com/contact/) us to learn how to use
`rpact` on FDA/GxP-compliant validated corporate computer systems and
how to get a copy of the formal validation documentation that is
customized and licensed for exclusive use by your company, e.g., to
fulfill regulatory requirements. The validation documentation contains
the personal access data for performing the installation qualification
with
[`testPackage()`](https://rpact-com.github.io/rpact/reference/testPackage.md).

> [www.rpact.com/contact](https://www.rpact.com/contact/)

# About

- **rpact** is a comprehensive validated[¹](#fn1) R package for clinical
  research which
  - enables the design and analysis of confirmatory adaptive group
    sequential designs
  - is a powerful sample size calculator
  - is a free of charge open-source software licensed under
    [LGPL-3](https://cran.r-project.org/web/licenses/LGPL-3)
  - particularly, implements the methods described in the recent
    monograph by [Wassmer and Brannath
    (2025)](https://doi.org/10.1007%2F978-3-031-89669-9)

> For more information please visit
> [www.rpact.org](https://www.rpact.org)

- **RPACT** is a company which offers
  - enterprise R/Shiny software development services
  - technical support for the
    [rpact](https://cran.r-project.org/package=rpact) package
  - consultancy and user training for scientists using R
  - validated software solutions and R package development for clinical
    research

> For more information please visit
> [www.rpact.com](https://www.rpact.com)

------------------------------------------------------------------------

1.  The rpact validation documentation is available exclusively for our
    customers and supporting members. For more information visit
    [www.rpact.com/services/sla](https://www.rpact.com/services/service-level-agreement/)
