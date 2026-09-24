# Calculate Stage Information for an Adaptive Design

Calculate the additional information required after an interim analysis,
or its unconditional expectation, for an optimal conditional error
design.

## Usage

``` r
getStageInformation(
  design,
  stage = 2,
  type = c("conditional", "expected"),
  pValue = NULL,
  likelihoodRatioDistribution = NULL,
  ...
)
```

## Arguments

- design:

  An object of class `TrialDesignOptimalConditionalError` created by
  [`getDesignOptimalConditionalError()`](https://docs.rpact.org/reference/getDesignOptimalConditionalError.md).
  Contains all necessary arguments to calculate the optimal conditional
  error function for the specified case.

- stage:

  Target stage for which additional information is required. Currently
  only `2` is supported; the interim p-value comes from stage one.

- type:

  `"conditional"` (default) for information given `pValue`, or
  `"expected"` for the unconditional expected additional information.

- pValue:

  First-stage p-value or p-values. Must be a numeric vector between 0
  and 1.

- likelihoodRatioDistribution:

  The distribution to be used for the effect size of the likelihood
  ratio in the calculation of the expected second-stage information.
  Options are `"fixed", "normal", "exp", "unif"` for fixed effect size,
  normally distributed, exponentially distributed, and uniformly
  distributed prior of the effect size, respectively. Each case requires
  different additional specifications:  

  - `likelihoodRatioDistribution="fixed"` uses one (or more) fixed
    effect sizes for the likelihood ratio and requires the parameter
    `thetaLR` which provides the mean difference under which to
    calculate the likelihood ratio. If `thetaLR` contains multiple
    values, they may be weighted using an additional argument
    `weightsLR`. Omitting `weightsLR` automatically leads to equal
    weighting.

  - `likelihoodRatioDistribution="normal"` uses a normal prior for the
    effect size and requires parameters `thetaLR` and `stDevLR` for the
    mean and standard deviation of the normal distribution (both on mean
    difference scale).

  - `likelihoodRatioDistribution="exp"` uses an exponential prior for
    the effect size and requires the parameter `kappaLR` which specifies
    the rate on the non-centrality scale divided by
    `sqrt(firstStageInformation)`. The mean effect on the mean
    difference scale is `1 / (kappaLR * firstStageInformation)`.

  - `likelihoodRatioDistribution="unif"` uses a uniform prior for the
    effect size and requires the specification of `maxThetaLR`, which is
    the maximum of the support for the uniform likelihood ratio
    distribution (on the mean difference scale).

  The default is `likelihoodRatioDistribution=NULL`. In this case, the
  likelihood ratio distribution under which the expected second-stage
  information is calculated is taken directly from the design object.

- ...:

  Distribution parameters for `type = "expected"`: `thetaLR`,
  `weightsLR`, `stDevLR`, `kappaLR`, and `maxThetaLR`, as applicable.

## Value

For `type = "conditional"`, a numeric vector with one information value
per p-value, including zero after early stopping. For
`type = "expected"`, a numeric scalar including zero information for
trials stopped at stage one. Neither result includes first-stage
information. Designs using `"maxlr"` require an explicit probability
distribution for the expectation.

## Conditional information

The second-stage information \\I\_{2}\\ is calculated given a
first-stage p-value \\p_1\\ as: \$\$I\_{2}(p_1) =
\frac{(\Phi^{-1}(1-\alpha_2(p_1)) + \Phi^{-1}(CP))^2}{\Delta_1^2} =
\frac{\nu(\alpha_2(p_1))}{\Delta_1^2},\$\$ where

- \\\alpha_2(p_1)\\ is the conditional error function

- \\CP\\ is the target conditional power

- \\\Delta_1\\ is the assumed treatment effect (expressed as a mean
  difference).

The conditional error is calculated according to the specification
provided in the `design` argument. For p-values smaller or equal to the
first-stage efficacy boundary as well as p-values greater than the
first-stage futility boundary, the returned information is 0 (since the
trial is ended early in both cases). When `efficacyBounds = 0`, early
efficacy stopping is disabled, including at a p-value of zero.

## Expected information

The expected second-stage information is calculated as:
\$\$\mathbb{E}(I\_{2})=\int\_{\alpha_1}^{\alpha_0}\frac{\nu(\alpha_2(p_1))
\cdot l(p_1)}{\Delta_1^2} dp_1,\$\$ where

- \\\alpha_1, \alpha_0\\ are the first-stage efficacy and futility
  boundaries

- \\\alpha_2(p_1)\\ is the optimal conditional error calculated for
  \\p_1\\

- \\l(p_1)\\ is the "true" likelihood ratio under which to calculate the
  expected sample size. This can be different from the likelihood ratio
  used to calibrate the optimal conditional error function.

- \\\Delta_1\\ is the assumed treatment effect to power for, expressed
  as a mean difference. It may depend on the interim data (i.e.,
  \\p_1\\) in case `useInterimEstimate = TRUE` was specified for the
  design object.

- \\\nu(\alpha_2(p_1)) = (\Phi^{-1}(1-\alpha_2(p_1))+\Phi^{-1}(CP))^2\\
  is a factor calculated for the specific assumptions about the optimal
  conditional error function and the target conditional power \\CP\\.

Add `design$firstStageInformation` to obtain expected total information.
This expectation is unconditional, not conditional on reaching stage
two. Changing the evaluation distribution here does not recalibrate the
design.

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

[`getDesignOptimalConditionalError()`](https://docs.rpact.org/reference/getDesignOptimalConditionalError.md),
[`getConditionalError()`](https://docs.rpact.org/reference/getConditionalError.md),
[`getFisherInformation()`](https://docs.rpact.org/reference/getFisherInformation.md)
for information at planned analyses of conventional designs.

[Optimal Conditional Error Designs with
rpact](https://www.rpact.org/vignettes/planning/rpact_optimal_conditional_error/)
for a worked example with stopping rules, information constraints,
interim estimates, and conditional power functions.

## Examples

``` r
# Get a design
design <- getDesignOptimalConditionalError(
    alpha = 0.025, efficacyBounds = 0.001, futilityBounds = 0.5, 
    conditionalPower = 0.9, thetaH1 = 0.25, 
    likelihoodRatioDistribution = "fixed", thetaLR = 0.25,
    firstStageInformation = 80, useInterimEstimate = FALSE
)
# Calculate expected information under correct specification
getStageInformation(type = "expected", design = design)
#> [1] 59.45876

# Compare operating characteristics under a different true effect.
getStageInformation(type = "expected", design = design, 
    likelihoodRatioDistribution = "fixed", thetaLR = 0.15)
#> [1] 108.4258

# Calculate expected information under the null hypothesis
getStageInformation(type = "expected",
    design = design, likelihoodRatioDistribution = "fixed", thetaLR = 0
)
#> [1] 94.61795

# Required additional information given the interim results
getStageInformation(design, pValue = c(0.05, 0.1, 0.3))
#> [1] 101.8229 140.8483 217.6371
```
