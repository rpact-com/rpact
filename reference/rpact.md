# rpact - Confirmatory Adaptive Clinical Trial Design and Analysis

rpact (R Package for Adaptive Clinical Trials) is a comprehensive
package that enables the design, simulation, and analysis of
confirmatory adaptive group sequential designs. Particularly, the
methods described in the recent monograph by Wassmer and Brannath
(published by Springer, 2025) are implemented. It also comprises
advanced methods for sample size calculations for fixed sample size
designs incl., e.g., sample size calculation for survival trials with
piecewise exponentially distributed survival times and staggered
patients entry.

## Details

rpact includes the classical group sequential designs (incl. user
spending function approaches) where the sample sizes per stage (or the
time points of interim analysis) cannot be changed in a data-driven way.
Confirmatory adaptive designs explicitly allow for this under control of
the Type I error rate. They are either based on the combination testing
or the conditional rejection probability (CRP) principle. Both are
available, for the former the inverse normal combination test and
Fisher's combination test can be used.

Specific techniques of the adaptive methodology are also available,
e.g., overall confidence intervals, overall p-values, and conditional
and predictive power assessments. Simulations can be performed to assess
the design characteristics of a (user-defined) sample size recalculation
strategy. Designs are available for trials with continuous, binary, and
survival endpoint.

For more information please visit
[www.rpact.org](https://www.rpact.org). If you are interested in
professional services round about the package or need a comprehensive
validation documentation to fulfill regulatory requirements please visit
[www.rpact.com](https://www.rpact.com).

rpact is developed by

- Gernot Wassmer (<gernot.wassmer@rpact.com>) and

- Friedrich Pahlke (<friedrich.pahlke@rpact.com>).

## References

Wassmer, G., Brannath, W. (2025) Group Sequential and Confirmatory
Adaptive Designs in Clinical Trials (Springer Series in Pharmaceutical
Statistics;
[doi:10.1007/978-3-031-89669-9](https://doi.org/10.1007/978-3-031-89669-9)
)

## See also

Useful links:

- <https://www.rpact.org>

- <https://www.rpact.com>

- <https://docs.rpact.org>

- <https://github.com/rpact-com/rpact>

- <https://rpact-cloud.share.connect.posit.cloud>

- Report bugs at <https://github.com/rpact-com/rpact/issues>

## Author

Gernot Wassmer, Friedrich Pahlke
