# rpact - Confirmatory Adaptive Clinical Trial Design and Analysis

rpact (R Package for Adaptive Clinical Trials) supports the design,
simulation, and analysis of confirmatory clinical trials. It provides
sample size and power calculations for standard fixed-sample and
classical group sequential designs, with methods for continuous, binary,
survival (time-to-event), and count data endpoints. Adaptive methods
include multi-stage combination tests, sample size reassessment,
multi-arm multi-stage (MAMS) designs, and population enrichment designs.
The package implements a broad range of methods described by Wassmer and
Brannath (2025).

## Details

Planning and simulation functions are available for means, rates,
survival, and count data. Survival planning supports staggered
recruitment and piecewise exponentially distributed survival times.
Multi-arm and population enrichment simulation and analysis methods are
available for means, rates, and survival endpoints. General estimates
and standard errors can also be analyzed through
[`getDataset()`](https://docs.rpact.org/reference/getDataset.md) and
[`getAnalysisResults()`](https://docs.rpact.org/reference/getAnalysisResults.md).

Classical group sequential designs specify rules for interim and final
analyses. Alpha-spending approaches allow boundary recalculation for
observed information, including under- and over-running. Adaptations
based on interim treatment effects require methods that preserve Type I
error control, such as combination testing or the conditional rejection
probability (CRP) principle. The package provides inverse normal and
Fisher combination tests and conditional Dunnett methods. Simulations
can assess the operating characteristics of sample size or event-number
reassessment strategies.

Analysis methods provide p-values, conditional power, and confidence
intervals where supported by the selected design and endpoint. In
particular, final confidence intervals and median unbiased estimates are
not available for Fisher combination tests or multi-arm and enrichment
analyses. See
[`getAnalysisResults()`](https://docs.rpact.org/reference/getAnalysisResults.md)
for further conditions and limitations.
[`getFutilityBounds()`](https://docs.rpact.org/reference/getFutilityBounds.md)
converts futility boundaries between different scales, including
conditional and predictive power scales.

rpact is free of charge and open source under the LGPL-3 license.
Package information and tutorials are available at
[www.rpact.org](https://www.rpact.org); function documentation is
available at [docs.rpact.org](https://docs.rpact.org).

RPACT GmbH develops Statistical Tools for Drug Development and offers
software development, validation, technical support, consulting, and
training. Formal validation documentation is available to customers and
supporting members. It includes credentials for the full installation
qualification test suite run by
[`testPackage()`](https://docs.rpact.org/reference/testPackage.md). For
services, see [www.rpact.com](https://www.rpact.com). Browser-based
tools are available at [demo.rpact.cloud](https://demo.rpact.cloud)
(RPACT Cloud) and [connect.rpact.app](https://connect.rpact.app) (RPACT
Connect).

rpact is developed by

- Gernot Wassmer (<gernot.wassmer@rpact.com>),

- Friedrich Pahlke (<friedrich.pahlke@rpact.com>), and

- Daniel Sabanés Bové (<daniel.sabanes_bove@rpact.com>).

## References

Wassmer, G., Brannath, W. (2025) Group Sequential and Confirmatory
Adaptive Designs in Clinical Trials (Springer Series in Pharmaceutical
Statistics;
[doi:10.1007/978-3-031-89669-9](https://doi.org/10.1007/978-3-031-89669-9)
)

## See also

[Package website](https://www.rpact.org), [Function
reference](https://docs.rpact.org), [RPACT
Cloud](https://demo.rpact.cloud), [RPACT
Connect](https://connect.rpact.app), [RPACT
GmbH](https://www.rpact.com), and [Bug
reports](https://github.com/rpact-com/rpact/issues).

## Author

Gernot Wassmer, Friedrich Pahlke
