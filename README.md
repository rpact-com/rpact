# rpact

Confirmatory Adaptive Clinical Trial Design, Simulation, and Analysis.

## Functional Range

  - Sample size and power calculation for
      - means (continuous endpoint)
      - rates (binary endpoint)
      - survival trials with
          - piecewise accrual time and intensity
          - piecewise exponential survival time
          - survival times that follow a Weibull distribution
  - Fixed sample design and designs with interim analysis stages
  - Simulation tool for means, rates, and survival data
      - Assessment of adaptive sample size/event number recalculations
        based on conditional power
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

## Documentation

The documentation is hosted at <https://www.rpact.org>

## Vignettes

The vignettes are hosted at <https://www.rpact.com/vignettes>

## The rpact user group

The *rpact project* has an active user group consisting of
decision-makers and users from the pharmaceutical industry and CROs, who
meet regularly and, e.g., discuss best practices.

We invite you to be part of the *rpact user group*: benefit from
know-how, shape open source development in Pharma\!

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

  - **rpact** is a comprehensive validated\[1\] R package for clinical
    research which
      - enables the design and analysis of confirmatory adaptive group
        sequential designs
      - is a powerful sample size calculator
      - is a free of charge open-source software licensed under
        [LGPL-3](https://cran.r-project.org/web/licenses/LGPL-3)
      - particularly, implements the methods described in the recent
        monograph by [Wassmer and Brannath
        (2016)](https://doi.org/10.1007%2F978-3-319-32562-0)

> For more information please visit
> [www.rpact.org](https://www.rpact.org)

  - **RPACT** is a company which offers
      - enterprise software development services
      - technical support for the `rpact` package
      - consultancy and user training for clinical research using R
      - validated software solutions and R package development for
        clinical research

> For more information please visit
> [www.rpact.com](https://www.rpact.com)

1.  The rpact validation documentation is available exclusively for our
    customers and supporting members. For more information visit
    [www.rpact.com/services/sla](https://www.rpact.com/services/sla)
