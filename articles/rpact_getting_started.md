# Getting started with rpact

**Confirmatory Adaptive Clinical Trial Design, Simulation, and
Analysis**

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

## Learn to use rpact

We recommend three ways to learn how to use `rpact`:

> 1.  Use RPACT Cloud:
>     [rpact.cloud](https://rpact-cloud.share.connect.posit.cloud)
> 2.  Use the Vignettes:
>     [rpact.org/vignettes](https://www.rpact.org/vignettes/)
> 3.  Book a training:
>     [rpact.com](https://www.rpact.com/services/learning-and-training/)

### RPACT Cloud

A graphical user interface (GUI) for the rpact R package, designed to
simplify its use through an intuitive interface:
[rpact.cloud](https://rpact-cloud.share.connect.posit.cloud)

### Vignettes

Developed for practical use: our collection of practical examples and
use-cases, the so-called rpact vignettes, are hosted at
[rpact.org/vignettes](https://www.rpact.org/vignettes/)

## User Concept

### Workflow

- Everything is starting with a design, e.g.:
  `design <- getDesignGroupSequential()`
- Find the optimal design parameters with help of
  [rpact](https://cran.r-project.org/package=rpact) comparison tools:
  `getDesignSet`
- Calculate the required sample size, e.g.:
  [`getSampleSizeMeans()`](https://docs.rpact.org/reference/getSampleSizeMeans.md),
  [`getPowerMeans()`](https://docs.rpact.org/reference/getPowerMeans.md)
- Simulate specific characteristics of an adaptive design, e.g.:
  [`getSimulationMeans()`](https://docs.rpact.org/reference/getSimulationMeans.md)
- Collect your data, import it into R and create a dataset:
  `data <- getDataset()`
- Analyze your data: `getAnalysisResults(design, data)`

### Focus on Usability

The most important [rpact](https://cran.r-project.org/package=rpact)
functions have intuitive names:

- `getDesign`\[`GroupSequential`/`InverseNormal`/`Fisher`\]`()`
- [`getDesignCharacteristics()`](https://docs.rpact.org/reference/getDesignCharacteristics.html)
- `getSampleSize`\[`Means`/`Rates`/`Survival`/`Counts`\]`()`
- `getPower`\[`Means`/`Rates`/`Survival`/`Counts`\]`()`
- `getSimulation`\[`MultiArm`/`Enrichment`\]\[`Means`/`Rates`/`Survival`/`Counts`\]`()`
- [`getDataset()`](https://docs.rpact.org/reference/getDataset.html)
- [`getAnalysisResults()`](https://docs.rpact.org/reference/getAnalysisResults.html)
- [`getStageResults()`](https://docs.rpact.org/reference/getStageResults.html)

[RStudio](https://posit.co/downloads/) /
[Positron](https://positron.posit.co/) /
[Eclipse](https://www.eclipse.org): auto code completion makes it easy
to use these functions.

### R generics

In general, everything runs with the R standard functions which are
always present in R: so-called R generics, e.g., `print`, `summary`,
`plot`, `as.data.frame`, `names`, `length`

### Utilities

Several utility functions are available, e.g.

- [`getAccrualTime()`](https://docs.rpact.org/reference/getAccrualTime.html)
- [`getPiecewiseSurvivalTime()`](https://docs.rpact.org/reference/getPiecewiseSurvivalTime.html)
- [`getNumberOfSubjects()`](https://docs.rpact.org/reference/getNumberOfSubjects.html)
- [`getEventProbabilities()`](https://docs.rpact.org/reference/getEventProbabilities.html)
- [`getPiecewiseExponentialDistribution()`](https://docs.rpact.org/reference/utilitiesForPiecewiseExponentialDistribution.html)
- survival helper functions for conversion of `pi`, `lambda` and
  `median`, e.g.,
  [`getLambdaByMedian()`](https://docs.rpact.org/reference/utilitiesForSurvivalTrials.html)
- [`testPackage()`](https://docs.rpact.org/reference/testPackage.html):
  installation qualification on a client computer or company server (see
  [Installation Qualification of
  rpact](https://www.rpact.org/vignettes/utilities/rpact_installation_qualification/))

## Validation

Please [contact](https://www.rpact.com/contact/) us to learn how to use
`rpact` on FDA/GxP-compliant validated corporate computer systems and
how to get a copy of the formal validation documentation that is
customized and licensed for exclusive use by your company, e.g., to
fulfill regulatory requirements.

## RPACT Connect

Connecting you to insights, downloads, and premium support:
[connect.rpact.com](https://rpact-connect.share.connect.posit.cloud)

## About

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

> For more information please visit [rpact.org](https://www.rpact.org)

- **RPACT** is a company which offers
  - enterprise software development services
  - technical support for the `rpact` package
  - consultancy and user training for clinical research using R
  - validated software solutions and R package development for clinical
    research

> For more information please visit [rpact.com](https://www.rpact.com)

## Contact

- <info@rpact.com>
- [rpact.com/contact](https://www.rpact.com/contact/)

------------------------------------------------------------------------

1.  The rpact validation documentation is available exclusively for our
    customers and supporting companies. For more information visit
    [rpact.com/services/service-level-agreement](https://www.rpact.com/services/service-level-agreement/)
