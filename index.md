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
v3](https://img.shields.io/badge/License-LGPL_v3-blue.svg)](https://cran.r-project.org/web/licenses/LGPL-3)
[![Codecov test
coverage](https://codecov.io/gh/rpact-com/rpact/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rpact-com/rpact?branch=main)
[![](https://img.shields.io/badge/DOI-10.1007/978--3--031--89669--9-yellow.svg?logo=doi)](https://doi.org/10.1007/978-3-031-89669-9)
[![](https://img.shields.io/badge/RPACT-Cloud-blue.svg?logo=r)](https://demo.rpact.cloud)
[![](https://img.shields.io/badge/RPACT-Connect-669999.svg?labelColor=336699)](https://connect.rpact.app)

`rpact` is an R package for planning, simulating, and analyzing
confirmatory clinical trials. It supports common tasks such as sample
size and power calculations for standard fixed-sample trials, as well as
classical group sequential designs with interim analyses. Methods are
available for continuous, binary, survival (time-to-event), and count
data endpoints.

For innovative trial designs, `rpact` provides simulation-based
evaluation of power and other operating characteristics, multi-stage
adaptive hypothesis tests based on the combination testing principle,
adaptive sample size or event-number reassessment, multi-arm multi-stage
(MAMS) designs, and population enrichment designs. This allows users to
start with routine trial planning and use the same package for more
advanced adaptive methods when needed.

## What rpact can do

`rpact` covers the main steps of confirmatory trial planning,
simulation, and analysis:

### Sample size and power planning

- Standard fixed-sample designs without interim analyses
- Classical group sequential designs with planned interim analyses
- Sample size and power calculations for
  - means (continuous endpoints)
  - rates (binary endpoints)
  - survival (time-to-event) endpoints with flexible recruitment and
    survival-time options
  - count data endpoints

### Simulation and adaptive trial design

- Power and operating-characteristic simulations for means, rates,
  survival data, and count data
- Assessment of adaptive sample size or event-number reassessment based
  on conditional power
- Multi-stage adaptive hypothesis testing based on the combination
  testing principle
- Assessment of treatment selection strategies in multi-arm trials,
  including multi-arm multi-stage (MAMS) settings for means, rates, and
  survival endpoints
- Simulation and analysis methods for population enrichment designs with
  means, rates, and hazard ratios

### Analysis and trial conduct

- Confirmatory analysis for means and rates in one-arm and two-arm
  trials, and for survival endpoints in treatment comparisons
- Multi-arm and population enrichment analysis for means, rates, and
  survival endpoints
- Analysis of general estimates and standard errors supplied through
  `getDataset(est = ..., se = ...)`
- Support for fixed-sample designs, classical group sequential designs,
  and adaptive multi-arm multi-stage (MAMS) designs based on the inverse
  normal or Fisher combination test
- Automatic boundary recalculation during a trial using alpha-spending
  approaches, including under- and over-running

## Installation

Install the latest stable release from CRAN:

``` r

install.packages("rpact")
```

### Development version

To try features that are not yet available in the CRAN release, install
the development version of `rpact` from
[GitHub](https://github.com/rpact-com/rpact):

``` r

# install.packages("pak")
pak::pak("rpact-com/rpact")
```

## Documentation

Package information and tutorials are available at
[www.rpact.org](https://www.rpact.org). For function arguments, return
values, and examples, see the [function
reference](https://docs.rpact.org/reference/).

## Vignettes

Step-by-step examples and tutorials are available in the `rpact`
vignettes at
[www.rpact.org/vignettes](https://www.rpact.org/vignettes/).

## Quick start

### A simple sample size calculation

This deliberately simple example illustrates a basic sample size
calculation with `rpact`.

Suppose a study compares two treatments for high blood pressure. We
expect the new treatment to lower systolic blood pressure by **15 mmHg**
on average, compared with **10 mmHg** under standard treatment—a
difference of **5 mmHg**. We assume a standard deviation of **12 mmHg**
in each group. These numbers are illustrative assumptions, not
recommendations for a clinical study.

How many participants are needed in two equally sized groups to detect
this difference with **80% power**?

``` r

library(rpact)

getSampleSizeMeans(
    groups = 2, # Two treatment groups
    alternative = 5, # Expected difference: 15 - 10 mmHg
    stDev = 12, # Assumed standard deviation in each group
    alpha = 0.05, # 5% significance level
    beta = 0.20, # 80% power
    sided = 2 # Two-sided test
) |>
    summary()
```

*Sample size calculation for a continuous endpoint*

Fixed sample analysis, two-sided significance level 5%, power 80%. The
results were calculated for a two-sample t-test, H0: mu(1) - mu(2) = 0,
H1: effect = 5, standard deviation = 12.

| Stage                             | Fixed  |
|-----------------------------------|--------|
| Stage level (two-sided)           | 0.0500 |
| Efficacy boundary (z-value scale) | 1.960  |
| Lower efficacy boundary (t)       | -3.503 |
| Upper efficacy boundary (t)       | 3.503  |
| Number of subjects                | 182.8  |

Legend:

- *(t)*: treatment effect scale

This example uses a conventional **two-sided test at the 5% significance
level**, without interim analyses. The calculated total sample size is
approximately 183 participants. Rounding up to equally sized groups
gives **92 participants per group**, before allowing for missing data or
dropouts. Power of 80% means an 80% chance of detecting the assumed
difference if it truly exists under the planning assumptions.

### A more advanced example: adding an interim analysis

We now make the study objective more explicit: we want to demonstrate
that the new treatment lowers blood pressure **more than the standard
treatment**. We therefore use a **one-sided test at the 2.5%
significance level**, a common choice for confirmatory superiority
trials. The direction of benefit is specified before the study begins.
Blood pressure reductions are recorded as positive values, so
`alternative = 5` represents the expected additional reduction with the
new treatment.

We keep the same illustrative assumptions and 80% power, but add **one
interim analysis**, when approximately half of the total statistical
information is available. If the evidence is sufficiently strong, the
study can stop early for efficacy. Otherwise, it continues to the final
analysis.

Simply testing twice at the usual significance level would increase the
chance of a false-positive conclusion. A **group sequential design**
accounts for both analyses. Here, we use O’Brien–Fleming boundaries,
which require particularly strong evidence to stop early.

``` r

getDesignGroupSequential(
    kMax = 2, # One interim and one final analysis
    informationRates = c(0.5, 1), # 50% information at interim, 100% at final
    typeOfDesign = "OF", # O'Brien–Fleming boundaries
    alpha = 0.025, # One-sided 2.5% significance level
    beta = 0.20, # 80% power
    sided = 1 # One-sided test
) |>
    getSampleSizeMeans(
        groups = 2, # Two treatment groups
        alternative = 5, # Additional blood pressure reduction (mmHg)
        stDev = 12 # Assumed standard deviation (mmHg)
    ) |>
    summary()
```

*Sample size calculation for a continuous endpoint*

Sequential analysis with a maximum of two looks (group sequential
design), one-sided overall significance level 2.5%, power 80%. The
results were calculated for a two-sample t-test, H0: mu(1) - mu(2) = 0,
H1: effect = 5, standard deviation = 12.

| Stage                                             | 1      | 2      |
|---------------------------------------------------|--------|--------|
| Planned information rate                          | 50%    | 100%   |
| Cumulative alpha spent                            | 0.0026 | 0.0250 |
| Stage levels (one-sided)                          | 0.0026 | 0.0240 |
| Efficacy boundary (z-value scale)                 | 2.797  | 1.977  |
| Efficacy boundary (t)                             | 7.169  | 3.520  |
| Cumulative power                                  | 0.2096 | 0.8000 |
| Number of subjects                                | 92.1   | 184.2  |
| Expected number of subjects under H1              | 164.9  |        |
| Stopping probability                              | 0.2096 |        |
| Efficacy stopping probability by stage (under H0) | 0.0026 |        |
| Efficacy stopping probability by stage            | 0.2096 |        |

Legend:

- *(t)*: treatment effect scale

The output shows the planned cumulative sample sizes and the efficacy
boundaries at each analysis. Allowing an early stop usually requires a
slightly larger **maximum sample size** than a corresponding study
without interim analyses, but may reduce the number of participants
actually needed.

This is an illustration of the workflow; a clinical study requires
additional planning, including the timing and conduct of the interim
analysis.

## Issues and Feature Requests

If you find a bug or would like to suggest an improvement, please use
the `rpact` GitHub issue tracker.

- 🐞 [Report a
  bug](https://github.com/rpact-com/rpact/issues/new?template=bug_report.yml)  
  Please include a minimal reproducible R example, or a runnable R
  Markdown (`.Rmd`) or Quarto (`.qmd`) document.

- ✨ [Request a
  feature](https://github.com/rpact-com/rpact/issues/new?template=feature_request.yml)  
  Please describe the use case, affected function(s), and the expected
  benefit.

Before opening a new issue, please [search existing
issues](https://github.com/rpact-com/rpact/issues) to avoid duplicates.

Because GitHub issues are public, please do not include confidential,
customer-specific, personal, regulated, or security-sensitive
information.

## RPACT Cloud

**RPACT Cloud** is a web-based graphical user interface for `rpact`. It
provides guided, browser-based access to selected `rpact` functionality,
including study design, sample size calculation, simulation, and
reporting, without requiring users to write R code.

[rpact.cloud](https://demo.rpact.cloud)

## RPACT Connect

**RPACT Connect** provides access to insights, downloads, and premium
support for users and organizations working with RPACT software.

[connect.rpact.app](https://connect.rpact.app)

## The RPACT User Group

The **RPACT User Group** brings together users and decision-makers from
the pharmaceutical industry and CROs. Members meet regularly to exchange
experience, discuss best practices, and provide input that helps shape
the open-source development of `rpact`.

We invite interested users and organizations to join the community,
share know-how, and contribute to the future development of statistical
tools for clinical trials.

## Use in regulated corporate environments

For organizations using `rpact` in FDA/GxP-regulated environments, RPACT
offers support and formal validation documentation for use on validated
corporate computer systems. The documentation can be customized and
licensed for exclusive use by your company to support applicable
regulatory requirements.

The validation documentation also contains the personal access data
required to run the full test suite for installation qualification with
[`testPackage()`](https://docs.rpact.org/reference/testPackage.md).

> [Contact RPACT](https://www.rpact.com/contact/) to learn more.

## About

**rpact** is free of charge and open source, licensed under
[LGPL-3](https://cran.r-project.org/web/licenses/LGPL-3). It implements
a broad range of methods described in the monograph by [Wassmer and
Brannath (2025)](https://doi.org/10.1007/978-3-031-89669-9). Formal
validation documentation is available to customers and supporting
members through the [RPACT service level
agreement](https://www.rpact.com/services/service-level-agreement/).

**RPACT GmbH** develops **Statistical Tools for Drug Development**. We
develop and support R packages, Shiny applications, and cloud-based
tools for clinical research, with services covering software
development, validation, technical support, consulting, and training.

Our products include [rpact](https://cran.r-project.org/package=rpact)
and [RPACT Cloud](https://demo.rpact.cloud). We also maintain and
support [crmPack](https://www.rpact.com/products/r-package-crmpack/),
the open-source package for dose escalation trial design, simulation,
and analysis.

For more information, visit [www.rpact.com](https://www.rpact.com).
