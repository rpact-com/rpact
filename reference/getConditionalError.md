# Calculate the Optimal Conditional Error

Calculate the Optimal Conditional Error

## Usage

``` r
getConditionalError(design, pValue, stage = 1)
```

## Arguments

- design:

  An object of class `TrialDesignOptimalConditionalError` created by
  [`getDesignOptimalConditionalError()`](https://docs.rpact.org/reference/getDesignOptimalConditionalError.md).
  Contains all necessary arguments to calculate the optimal conditional
  error function for the specified case.

- pValue:

  First-stage p-value or p-values. Must be a numeric vector between 0
  and 1.

- stage:

  Completed interim stage. Currently only `1` is supported.

## Value

A numeric vector of conditional errors, one per first-stage p-value.

## Details

The optimal conditional error \\\alpha_2\\ given a first-stage p-value
\\p_1\\ is calculated as: \$\$\alpha_2(p_1)=\psi(-e^{c_0} \cdot
\frac{\Delta_1^2}{l(p_1)}).\$\$

The level constant \\c_0\\ as well as the specification of the effect
size \\\Delta_1\\ and the likelihood ratio \\l(p_1)\\ must be contained
in the `design` object (see
[`?getDesignOptimalConditionalError`](https://docs.rpact.org/reference/getDesignOptimalConditionalError.md)).
Early stopping rules are supported, i.e., for \\p_1 \leq \alpha_1\\ with
\\\alpha_1 \> 0\\, the returned conditional error is 1 and for \\p_1 \>
\alpha_0\\, the returned conditional error is 0.

## References

Brannath, W. & Bauer, P. (2004). Optimal conditional error functions for
the control of conditional power. Biometrics, 60(3), 715-723.
[doi:10.1111/j.0006-341X.2004.00221.x](https://doi.org/10.1111/j.0006-341X.2004.00221.x)

Brannath, W., zur Verth, J., Dreher, M. & Scharpenberg, M. (2024;
revised 2026). Optimal monotone conditional error functions.
[doi:10.48550/arXiv.2402.00814](https://doi.org/10.48550/arXiv.2402.00814)

The related R package [optconerrf: Optimal Monotone Conditional Error
Functions](https://CRAN.R-project.org/package=optconerrf) provides a
standalone implementation of this methodology. The rpact implementation
uses rpact design objects and interfaces; optconerrf is not required to
run these functions.

## See also

[Optimal Conditional Error Designs with
rpact](https://www.rpact.org/vignettes/planning/rpact_optimal_conditional_error/)
for a worked example with stopping rules, information constraints,
interim estimates, and conditional power functions.

[`getDesignOptimalConditionalError()`](https://docs.rpact.org/reference/getDesignOptimalConditionalError.md)

## Examples

``` r
# Create a design
design <- getDesignOptimalConditionalError(
    alpha = 0.025, efficacyBounds = 0.001, futilityBounds = 0.5, conditionalPower = 0.9,
    thetaH1 = 0.5, firstStageInformation = 40, useInterimEstimate = FALSE,
    likelihoodRatioDistribution = "fixed", thetaLR = 0.5
)

# Early efficacy gives 1, continuation gives the stage-two threshold,
# and binding futility gives 0.
getConditionalError(
    pValue = c(0.0005, 0.1, 0.3, 0.8), design = design
)
#> [1] 1.000000000 0.029470114 0.002541869 0.000000000
```
