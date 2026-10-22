# Getting started with rpact

**Confirmatory Adaptive Clinical Trial Design and Analysis**

`rpact` supports the design, simulation, and analysis of confirmatory
clinical trials, from standard fixed-sample planning to group sequential
and adaptive methods. Sample size and power planning and simulation are
available for continuous, binary, survival (time-to-event), and count
data endpoints.

## Functional Range

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

## Learn to use rpact

We recommend three ways to learn how to use `rpact`:

> 1.  Use RPACT Cloud: [demo.rpact.cloud](https://demo.rpact.cloud)
> 2.  Use the Vignettes:
>     [rpact.org/vignettes](https://www.rpact.org/vignettes/)
> 3.  Book a training:
>     [rpact.com](https://www.rpact.com/services/learning-and-training/)

### RPACT Cloud

**RPACT Cloud** provides guided, browser-based access to selected
`rpact` functionality, including study design, sample size calculation,
simulation, and reporting, without requiring users to write R code:
[demo.rpact.cloud](https://demo.rpact.cloud).

### Vignettes and function reference

Practical examples and tutorials are available at
[rpact.org/vignettes](https://www.rpact.org/vignettes/). For function
arguments, return values, and examples, see the [function
reference](https://docs.rpact.org).

## User Concept

### Workflow

1.  Create a fixed-sample, group sequential, or adaptive design.
2.  Compare alternative designs with
    [`getDesignSet()`](https://docs.rpact.org/reference/getDesignSet.md).
3.  Calculate the required sample size with, for example,
    [`getSampleSizeMeans()`](https://docs.rpact.org/reference/getSampleSizeMeans.md),
    or evaluate power for a specified sample size with
    [`getPowerMeans()`](https://docs.rpact.org/reference/getPowerMeans.md).
4.  Assess operating characteristics and adaptation strategies using
    simulation, for example,
    [`getSimulationMeans()`](https://docs.rpact.org/reference/getSimulationMeans.md).
5.  Prepare aggregated trial data with
    [`getDataset()`](https://docs.rpact.org/reference/getDataset.md) and
    analyze them with
    [`getAnalysisResults()`](https://docs.rpact.org/reference/getAnalysisResults.md).

### Planning example

Plan a two-arm trial with a continuous endpoint, a one-sided
significance level of 0.025, 90% power, and three analyses at 50%, 75%,
and 100% of the planned information. Assume a mean difference of 0.5 and
a common standard deviation of 1.

``` r

library(rpact)
design <- getDesignGroupSequential(
    kMax = 3, informationRates = c(0.5, 0.75, 1),
    alpha = 0.025, beta = 0.1, sided = 1, typeOfDesign = "OF"
)
sampleSize <- getSampleSizeMeans(
    design = design, groups = 2, alternative = 0.5,
    stDev = 1, allocationRatioPlanned = 1
)
summary(sampleSize)
```

*Sample size calculation for a continuous endpoint*

Sequential analysis with a maximum of three looks (group sequential
design), one-sided overall significance level 2.5%, power 90%. The
results were calculated for a two-sample t-test, H0: mu(1) - mu(2) = 0,
H1: effect = 0.5, standard deviation = 1.

| Stage                                             | 1      | 2      | 3      |
|---------------------------------------------------|--------|--------|--------|
| Planned information rate                          | 50%    | 75%    | 100%   |
| Cumulative alpha spent                            | 0.0021 | 0.0105 | 0.0250 |
| Stage levels (one-sided)                          | 0.0021 | 0.0097 | 0.0215 |
| Efficacy boundary (z-value scale)                 | 2.863  | 2.337  | 2.024  |
| Efficacy boundary (t)                             | 0.631  | 0.415  | 0.309  |
| Cumulative power                                  | 0.2928 | 0.6960 | 0.9000 |
| Number of subjects                                | 86.9   | 130.4  | 173.8  |
| Expected number of subjects under H1              | 130.9  |        |        |
| Stopping probability                              | 0.6960 |        |        |
| Efficacy stopping probability by stage (under H0) | 0.0021 | 0.0084 |        |
| Efficacy stopping probability by stage            | 0.2928 | 0.4033 |        |

Legend:

- *(t)*: treatment effect scale

For a design without interim analyses, use
[`getDesignFixed()`](https://docs.rpact.org/reference/getDesignFixed.md)
instead.

### Analysis example

The following illustrative data represent two independent groups at each
of three stages. Sample sizes, means, and standard deviations are
**stage-wise**: each entry describes only the new observations collected
in that stage. The sample sizes are chosen for illustration,
independently of the planning result above.

``` r

trialData <- getDataset(
    sampleSizes1 = c(40, 20, 20), sampleSizes2 = c(40, 20, 20),
    means1 = c(0.2, 0.3, 0.5), means2 = c(0, 0, 0),
    stDevs1 = c(1, 1, 1), stDevs2 = c(1, 1, 1)
)
analysisResults <- getAnalysisResults(
    design = design, dataInput = trialData, directionUpper = TRUE
)
summary(analysisResults)
```

*Analysis results for a continuous endpoint*

Sequential analysis with three looks (group sequential design),
one-sided overall significance level 2.5%. The results were calculated
using a two-sample t-test, equal variances option. H0: mu(1) - mu(2) = 0
against H1: mu(1) - mu(2) \> 0.

| Stage | 1 | 2 | 3 |
|----|----|----|----|
| Planned information rate | 50% | 75% | 100% |
| Cumulative alpha spent | 0.0021 | 0.0105 | 0.0250 |
| Stage levels (one-sided) | 0.0021 | 0.0097 | 0.0215 |
| Efficacy boundary (z-value scale) | 2.863 | 2.337 | 2.024 |
| Cumulative effect size | 0.200 | 0.233 | 0.300 |
| Cumulative (pooled) standard deviation | 1.000 | 0.992 | 0.991 |
| Overall test statistic | 0.894 | 1.288 | 1.914 |
| Overall p-value | 0.1869 | 0.1001 | 0.0287 |
| Test action | continue | continue | accept |
| Conditional rejection probability | 0.0252 | 0.0337 |  |
| 95% repeated confidence interval | \[-0.459; 0.859\] | \[-0.196; 0.663\] | \[-0.020; 0.620\] |
| Repeated p-value | 0.3515 | 0.1719 | 0.0338 |
| Final p-value |  |  | 0.0314 |
| Final confidence interval |  |  | \[-0.016; 0.602\] |
| Median unbiased estimate |  |  | 0.294 |

If the available summaries are cumulative, use the corresponding
`cumulative` prefixes, such as `cumulativeSampleSizes1`,
`cumulativeMeans1`, and `cumulativeStDevs1`. For survival data,
cumulative event counts and log-rank statistics are commonly used; see
[`getDataset()`](https://docs.rpact.org/reference/getDataset.md) for the
required inputs. Analysis results and the availability of confidence
intervals depend on the chosen design and endpoint; see
[`getAnalysisResults()`](https://docs.rpact.org/reference/getAnalysisResults.md)
for details.

### Focus on Usability

The terms *survival* and *time-to-event* refer to the same endpoint type
here. The corresponding function names use the suffix `Survival`.

The most important [rpact](https://cran.r-project.org/package=rpact)
functions have intuitive names:

- `getDesign`\[`Fixed`/`GroupSequential`/`InverseNormal`/`Fisher`\]`()`
- [`getDesignCharacteristics()`](https://docs.rpact.org/reference/getDesignCharacteristics.html)
- `getSampleSize`\[`Means`/`Rates`/`Survival`/`Counts`\]`()`
- `getPower`\[`Means`/`Rates`/`Survival`/`Counts`\]`()`
- `getSimulation`\[`Means`/`Rates`/`Survival`/`Counts`\]`()`
- `getSimulationMultiArm`\[`Means`/`Rates`/`Survival`\]`()`
- `getSimulationEnrichment`\[`Means`/`Rates`/`Survival`\]`()`
- [`getDesignConditionalDunnett()`](https://docs.rpact.org/reference/getDesignConditionalDunnett.md)
  for adaptive two-stage multi-arm designs
- [`getDataset()`](https://docs.rpact.org/reference/getDataset.html)
- [`getAnalysisResults()`](https://docs.rpact.org/reference/getAnalysisResults.html)
- [`getStageResults()`](https://docs.rpact.org/reference/getStageResults.html)

[RStudio](https://posit.co/downloads/) /
[Positron](https://positron.posit.co/) / [VS
Code](https://code.visualstudio.com) /
[Eclipse](https://www.eclipse.org): auto code completion makes it easy
to use these functions.

### R generics

Result objects support familiar R functions such as
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html),
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html),
[`names()`](https://rdrr.io/r/base/names.html), and
[`length()`](https://rdrr.io/r/base/length.html). The available methods
depend on the object class; see the corresponding function reference.

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

For organizations using `rpact` in FDA/GxP-regulated environments, RPACT
offers support and formal validation documentation for use on validated
corporate computer systems. The documentation can be customized and
licensed for exclusive use by your company to support applicable
regulatory requirements.

The validation documentation also contains the personal access data
required to run the full test suite for installation qualification with
[`testPackage()`](https://docs.rpact.org/reference/testPackage.md).

> [Contact RPACT](https://www.rpact.com/contact/) to learn more.

## RPACT Connect

**RPACT Connect** provides access to insights, downloads, and premium
support for users and organizations working with RPACT software:
[connect.rpact.app](https://connect.rpact.app).

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

## Contact

- <info@rpact.com>
- [rpact.com/contact](https://www.rpact.com/contact/)
