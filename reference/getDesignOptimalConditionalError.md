# Create an Optimal Conditional Error Design

This function returns a design object which contains all important
parameters for the specification of the optimal conditional error
function. The returned object is of class
`TrialDesignOptimalConditionalError` and can be passed to other package
functions.

## Usage

``` r
getDesignOptimalConditionalError(
  alpha,
  efficacyBounds,
  futilityBounds,
  conditionalPower = NA_real_,
  thetaH1 = NA_real_,
  minThetaH1 = NA_real_,
  maxThetaH1 = Inf,
  useInterimEstimate = TRUE,
  firstStageInformation,
  likelihoodRatioDistribution,
  minInformationPerStage = 0,
  maxInformationPerStage = Inf,
  minConditionalError = 0,
  maxConditionalError = 1,
  conditionalPowerFunction = NULL,
  minLevelConstant = 0,
  maxLevelConstant = 10,
  enforceMonotonicity = TRUE,
  efficacyBoundsScale = "pValue",
  futilityBoundsScale = "pValue",
  nonCentralityParameterH1 = NULL,
  minNonCentralityParameterH1 = NULL,
  maxNonCentralityParameterH1 = Inf,
  ...
)
```

## Arguments

- alpha:

  The overall type I error rate \\\alpha\\ of the design. Must be a
  numeric value between 0 and 1.

- efficacyBounds:

  Stage 1 efficacy boundary \\\alpha_1\\ (p-value scale). Must be a
  numeric value between 0 and 1. Should be smaller than
  `futilityBounds`.

- futilityBounds:

  Binding stage 1 futility boundary \\\alpha_0\\ (p-value scale). Must
  be a numeric value between 0 and 1. Should be greater than
  `efficacyBounds`. Specify `futilityBounds=1` to disable early stopping
  for futility.

- conditionalPower:

  The target conditional power \\CP\\ of the design. Must be a numeric
  scalar strictly between 0 and 1. Takes precedence over
  `conditionalPowerFunction`.

- thetaH1:

  Fixed effect assumption at which the conditional power should be
  achieved, expressed on the mean difference scale. Is only used if
  `useInterimEstimate=FALSE`. Must be a numeric value greater than 0.

- minThetaH1:

  The minimum for an interim estimate of the treatment effect, specified
  on the mean difference scale. If the interim estimate (on the mean
  difference scale) yields a value smaller than `minThetaH1`,
  `minThetaH1` is used for it. Is only used if
  `useInterimEstimate=TRUE`. Must be a numeric value.

- maxThetaH1:

  The maximum for an interim estimate of the treatment effect, specified
  on the mean difference scale. If the interim estimate (on the mean
  difference scale) yields a value larger than `maxThetaH1`,
  `maxThetaH1` is used for it. Is only used if
  `useInterimEstimate=TRUE`. Must be a numeric value. Default value is
  `Inf`, i.e., no upper restriction.

- useInterimEstimate:

  Logical. Defines whether or not an interim estimate should be used for
  conditional power. If `TRUE`, a lower cut-off for the interim estimate
  must be specified by `minThetaH1` or `minNonCentralityParameterH1`. An
  upper cut-off may also be specified by `maxThetaH1` or
  `maxNonCentralityParameterH1`. If `FALSE`, the fixed effect size must
  be specified by `thetaH1` or `nonCentralityParameterH1`.

- firstStageInformation:

  Information of the first stage of the trial. Must be a positive
  numeric value.

- likelihoodRatioDistribution:

  The distribution to be used for the effect size of the likelihood
  ratio in the optimal conditional error function. Options are
  `"fixed", "normal", "exp", "unif", "maxlr"` for fixed effect size,
  normally distributed, exponentially distributed, uniformly distributed
  prior of the effect size and maximum likelihood ratio, respectively.
  Each case requires different additional specifications:  

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

  - `likelihoodRatioDistribution="maxlr"` estimates the non-centrality
    parameter to be used for the likelihood ratio from the data. No
    additional parameters must be specified.

- minInformationPerStage:

  The minimum additional information allowed in stage two. A single
  numeric value; does not apply to firstStageInformation. Default value
  is `0`, i.e., no restriction.

- maxInformationPerStage:

  The maximum additional information allowed in stage two. A single
  numeric value; does not apply to firstStageInformation. Default value
  is `Inf`, i.e., no restriction.

- minConditionalError:

  Lower boundary for the optimal conditional error function. Default 0
  (no restriction).

- maxConditionalError:

  Upper boundary for the optimal conditional error function. Default
  value is 1, however, the optimal conditional error function is
  inherently bounded by the conditional power.

- conditionalPowerFunction:

  A function accepting a single first-stage p-value and returning a
  numeric scalar strictly between 0 and 1. The function is evaluated
  separately for each p-value. This function should not be increasing in
  the first-stage p-value, otherwise monotonicity issues may occur. With
  a conditional power function, the resulting conditional error function
  is not necessarily optimal for expected second-stage information.

- minLevelConstant:

  The minimum of the interval on which the value for the level constant
  should be searched. Default value is 0.

- maxLevelConstant:

  The maximum of the interval on which the value for the level constant
  should be searched. Default value is 10.

- enforceMonotonicity:

  Logical. Determines whether or not the optimal conditional error
  function should automatically be modified to be non-increasing.
  Default is `TRUE`.

- efficacyBoundsScale, futilityBoundsScale:

  Scale of the interim boundaries. Currently only `"pValue"` is
  supported. These are p-value cutoffs, not the test-statistic
  boundaries used by conventional group sequential designs.

- nonCentralityParameterH1:

  Fixed conditional-power effect on the non-centrality scale;
  alternative to `thetaH1` when `useInterimEstimate = FALSE`.

- minNonCentralityParameterH1, maxNonCentralityParameterH1:

  Lower and upper limits for the interim effect on the non-centrality
  scale; alternatives to `minThetaH1` and `maxThetaH1` when
  `useInterimEstimate = TRUE`. Non-centrality equals the mean difference
  times `sqrt(firstStageInformation)`. Mean-difference arguments take
  precedence when both scales are supplied. The default upper limit is
  `Inf`.

- ...:

  Distribution parameters `thetaLR`, `weightsLR`, `stDevLR`, `kappaLR`,
  and `maxThetaLR`, as described under Likelihood ratio distribution.

## Value

An object of class `TrialDesignOptimalConditionalError`, which can be
passed to
[`getConditionalError()`](https://docs.rpact.org/reference/getConditionalError.md),
[`getStageInformation()`](https://docs.rpact.org/reference/getStageInformation.md),
and
[`getDesignCharacteristics()`](https://docs.rpact.org/reference/getDesignCharacteristics.md).
This adaptive design is not interchangeable with a conventional
`TrialDesign`.

## Details

The design object contains the information required to determine the
specific setting of the optimal conditional error function and can be
passed to other package functions. From the given user specifications,
the constant to achieve level condition for control of the overall type
I error rate as well as the constants to ensure a non-increasing optimal
CEF (if required) are automatically calculated.

## Hypotheses and trial decisions

The one-sided hypotheses are \\H_0: \Delta \leq 0\\ versus \\H_1: \Delta
\> 0\\. With a first-stage p-value \\p_1\\, stop for efficacy if \\p_1
\leq \alpha_1\\ and \\\alpha_1 \> 0\\; stop for futility if \\p_1 \>
\alpha_0\\. Futility is binding: continuing beyond this boundary is not
part of the calibrated design. In the continuation region, reject at
stage two if its p-value is no larger than
[`getConditionalError()`](https://docs.rpact.org/reference/getConditionalError.md).
The second-stage p-value must be based on the new, independent
second-stage data, not the cumulative data. Setting `efficacyBounds = 0`
disables early efficacy and `futilityBounds = 1` disables early
futility. Here, \\\alpha_1\\ denotes `efficacyBounds` and \\\alpha_0\\
denotes `futilityBounds`; both are single interim cutoffs on the p-value
scale. The calibration satisfies \$\$\alpha = \alpha_1 +
\int\_{\alpha_1}^{\alpha_0} \alpha_2(p_1) \\ dp_1.\$\$ Target
conditional power refers to a specified planning effect after
continuation; it is not the overall power returned by
[`getDesignCharacteristics()`](https://docs.rpact.org/reference/getDesignCharacteristics.md).

## Likelihood ratio distribution

To calculate the optimal conditional error function, an assumption about
the true parameter under which the second-stage information is to be
minimised is required. Various options are available and can be
specified via the argument `likelihoodRatioDistribution`:

- `likelihoodRatioDistribution="fixed"`: calculates the likelihood ratio
  for a fixed \\\Delta\\. The non-centrality parameter of the likelihood
  ratio \\\vartheta\\ is then computed as
  `thetaLR`\*`sqrt(firstStageInformation)` and the likelihood ratio is
  calculated as: \$\$l(p_1) = e^{\Phi^{-1}(1-p_1)\vartheta -
  \vartheta^2/2}.\$\$ `thetaLR` may also contain multiple elements, in
  which case a weighted likelihood ratio is calculated for the given
  values. Unless positive weights that sum to 1 are provided by the
  argument `weightsLR`, equal weights are assumed.

- `likelihoodRatioDistribution="normal"`: calculates the likelihood
  ratio for a normally distributed prior of \\\vartheta\\ with mean
  `thetaLR`\*`sqrt(firstStageInformation)` (\\\mu\\) and standard
  deviation `stDevLR`\*`sqrt(firstStageInformation)` (\\\sigma\\). The
  parameters `thetaLR` and `stDevLR` must be specified on the mean
  difference scale. \$\$l(p_1) = (1+\sigma^2)^{-\frac{1}{2}}\cdot
  e^{-(\mu/\sigma)^2/2 + (\sigma\Phi^{-1}(1-p_1) + \mu/\sigma)^2 /
  (2\cdot (1+\sigma^2))}\$\$

- `likelihoodRatioDistribution="exp"`: calculates the likelihood ratio
  for an exponentially distributed prior of \\\vartheta\\ with rate
  `kappaLR`\*`sqrt(firstStageInformation)` (\\\eta\\). The likelihood
  ratio is then calculated as: \$\$l(p_1) = \eta \cdot \sqrt{2\pi} \cdot
  e^{(\Phi^{-1}(1-p_1)-\eta)^2/2} \cdot \Phi(\Phi^{-1}(1-p_1)-\eta)\$\$

- `likelihoodRatioDistribution="unif"`: calculates the likelihood ratio
  for a uniformly distributed prior of \\\vartheta\\ on the support
  \\\[0, \Delta\cdot\sqrt{I_1}\]\\, where \\\Delta\\ is specified as
  `maxThetaLR` and \\I_1\\ is the `firstStageInformation`. \$\$l(p_1) =
  \frac{\sqrt{2\pi}}{\Delta\cdot\sqrt{I_1}} \cdot
  e^{\Phi^{-1}(1-p_1)^2/2} \cdot (\Phi(\Delta\cdot\sqrt{I_1} -
  \Phi^{-1}(1-p_1))-p_1)\$\$

- `likelihoodRatioDistribution="maxlr"`: the non-centrality parameter
  \\\vartheta\\ is estimated from the data and no additional parameters
  must be specified. The likelihood ratio is estimated from the data as:
  \$\$l(p_1) = e^{max(0, \Phi^{-1}(1-p_1))^2/2}\$\$ The maximum
  likelihood ratio is always restricted to effect sizes \\\vartheta \geq
  0\\ (corresponding to \\p_1 \leq 0.5\\).

## Effect for conditional power

For the treatment effect at which the target conditional power should be
achieved, either a fixed effect or an interim estimate can be used. The
planning effect `thetaH1` need not equal `thetaLR`: the former sets the
conditional power target, whereas the latter specifies the effect or
mixture under which expected second-stage information is minimised. The
usage of a fixed effect is indicated by setting
`useInterimEstimate=FALSE`, in which case the fixed effect is provided
by `thetaH1` on the mean difference scale. For an interim estimate,
specified by `useInterimEstimate=TRUE`, a lower cut-off for the interim
estimate must be provided, by `minThetaH1` on the mean difference scale.
In addition, an upper limit of the estimate may be analogously provided
by `maxThetaH1`. These effects may alternatively be specified on the
non-centrality parameter scale as `nonCentralityParameterH1`,
`minNonCentralityParameterH1`, and `maxNonCentralityParameterH1`.

## Sample size and information

The first-stage information of the trial design must be specified to
allow for calculations between the mean difference and non-centrality
parameter scale. It is provided to the design object via
`firstStageInformation`.  
Listed below are some examples for the calculation between information
(\\I_1\\) and sample size:

- One-sample z-test with \\n\\ total patients: \\I_1 =
  \frac{n}{\sigma^2}\\, where \\\sigma^2\\ is the variance of an
  individual observation

- Balanced two-sample z-test with \\n_1\\ patients per group: \\I_1 =
  \frac{1}{2}\cdot\frac{n_1}{\sigma^2}\\, where \\\sigma^2\\ is the
  common variance

- General two-sample z-test with \\n_1\\, \\n_2\\ patients per group:
  \\I_1 = 1/(\frac{\sigma_1^2}{n_1}+\frac{\sigma_2^2}{n_2})\\, where
  \\\sigma_1^2\\, \\\sigma_2^2\\ are the group-wise variances

## Monotonicity

By default, the function Q (likelihood ratio divided by the squared
effect) is transformed to be non-increasing in the first-stage p-value.
For constant conditional power this yields a non-increasing conditional
error function. A conditional power callback combined with interim
effect estimates and information constraints can still produce a
non-monotone conditional error function; `enforceMonotonicity` does not
guarantee monotonicity in that setting. The necessary intervals and
constants for the transformation are calculated by an internal
monotonisation routine. Although not recommended for the operating
characteristics of the design, the transformation may be omitted by
setting `enforceMonotonicity=FALSE`.

## Constraints

In some applications, it may be feasible to restrict the optimal
conditional error function by a lower and/or upper limit. These
constraints can be directly implemented on the function by using the
arguments `minConditionalError` and `maxConditionalError`. By default,
`minConditionalError=0` and `maxConditionalError=1`, i.e., no
constraints are applied. The constraints may also be specified on the
second-stage information via `minInformationPerStage` and
`maxInformationPerStage`. If both `minConditionalError` and
`maxInformationPerStage` respectively `maxConditionalError` and
`minInformationPerStage` are provided, both constraints will be applied.
Set these constraints when creating the design, so that calibration
accounts for them. For fixed \\\Delta_1\\ and conditional power \\CP\\,
an upper information bound corresponds to the lower conditional error
bound \\\Phi(\Phi^{-1}(CP) - \Delta_1 \sqrt{I\_{2,\max}})\\; a lower
information bound analogously corresponds to an upper conditional error
bound. Information bounds apply only when the trial continues:
information is zero after early stopping. They refer to additional
stage-two information, not cumulative information.
`minInformationPerStage` and `maxInformationPerStage` are scalar limits
for stage two only; they do not constrain `firstStageInformation`. In
the continuation region, conditional error cannot exceed the conditional
power; an upper bound above it has no additional effect. Before
calibration, the integrated lower and upper bounds are checked for
compatibility with the overall alpha level.

## Level constant

The level constant is determined by an internal root-finding routine. It
is identified using the
[`uniroot()`](https://rdrr.io/r/stats/uniroot.html) function and by
default, the interval between 0 and 10 is searched for the level
constant. In specific settings, the level constant may lie outside of
this interval. In such cases, the search interval can be changed by
altering the parameters `minLevelConstant` and `maxLevelConstant`.  
If inappropriate constraints to the optimal conditional error function
are provided via `minConditionalError` and `maxConditionalError` or
`minInformationPerStage` and `maxInformationPerStage`, it may be
impossible to find a level constant which exhausts the full alpha level.

Numerical integration uses an adapted routine for piecewise constant
functions where applicable. Set
`options(rpact.design.optimal.enforce.basic.integration = TRUE)` to use
standard adaptive integration for comparison.

## Generic functions

The [`print()`](https://rdrr.io/r/base/print.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) functions are
available for objects of class `TrialDesignOptimalConditionalError`. For
details, see
[`?print.TrialDesignOptimalConditionalError`](https://docs.rpact.org/reference/print.TrialDesignOptimalConditionalError.md)
and
[`?plot.TrialDesignOptimalConditionalError`](https://docs.rpact.org/reference/plot.TrialDesignOptimalConditionalError.md).

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

## Examples

``` r
# Create a single-arm design with fixed parameter for the likelihood ratio
# and a fixed effect for conditional power. 80 patients are observed in the
# first-stage (firstStageInformation = 80 in the one-sample test, variance 1).
# The second-stage information is restricted to be between 40 and 160.
getDesignOptimalConditionalError(
    alpha = 0.025, efficacyBounds = 0.001, futilityBounds = 0.5, conditionalPower = 0.9,
    thetaH1 = 0.25, likelihoodRatioDistribution = "fixed", thetaLR = 0.25,
    firstStageInformation = 80, useInterimEstimate = FALSE,
    minInformationPerStage = 40, maxInformationPerStage = 160
)
#> Optimal Conditional Error Design: 
#>  
#> General design parameters: 
#>   Overall significance level: 0.025 
#>   First-stage efficacy boundary (p-value scale): 0.001 
#>   Binding first-stage futility boundary (p-value scale): 0.5 
#>   Constraints on second-stage information: [40, 160] 
#> 
#> Conditional power specification: 
#>   Target conditional power: 0.9 
#>   Alternative: 0.25 
#>   First-stage non-centrality parameter: 2.236068 
#>   First-stage information: 80 
#> 
#> Likelihood ratio specification: 
#>   Fixed parameter(s) in likelihood ratio:  0.25 
#>   Parameter weights:  1 
#> 
#> Level constant: 
#>   Constant: 7.61398 
#>   Searched on interval: [0, 10] 

# Create a design comparing two groups using the maximum likelihood ratio
# and an interim estimate for the effect for conditional power.
# 160 patients per arm are observed in the first stage
# (firstStageInformation = 80 in the balanced two-sample test, variance 1).
getDesignOptimalConditionalError(
    alpha = 0.025, efficacyBounds = 0.001, futilityBounds = 0.5, conditionalPower = 0.9,
    minThetaH1 = 0.25, likelihoodRatioDistribution = "maxlr",
    firstStageInformation = 80, useInterimEstimate = TRUE
)
#> Optimal Conditional Error Design: 
#>  
#> General design parameters: 
#>   Overall significance level: 0.025 
#>   First-stage efficacy boundary (p-value scale): 0.001 
#>   Binding first-stage futility boundary (p-value scale): 0.5 
#> 
#> Conditional power specification: 
#>   Target conditional power: 0.9 
#>   Alternative: interim estimate restricted to [0.25, Inf] 
#>   First-stage non-centrality parameter restricted to [2.23606797749979, Inf] 
#>   First-stage information: 80 
#> 
#> Likelihood ratio specification: 
#>   Maximum likelihood ratio 
#> 
#> Level constant: 
#>   Constant: 7.68814 
#>   Searched on interval: [0, 10] 

# Weight several effects for the optimisation; retain a separate power target.
weightedDesign <- getDesignOptimalConditionalError(
    alpha = 0.025, efficacyBounds = 0.001, futilityBounds = 0.5, conditionalPower = 0.9,
    thetaH1 = 0.25, useInterimEstimate = FALSE, firstStageInformation = 80,
    likelihoodRatioDistribution = "fixed", thetaLR = c(0, 0.25, 0.5),
    weightsLR = c(0.25, 0.5, 0.25)
)
getStageInformation(pValue = c(0.05, 0.1, 0.3), design = weightedDesign)
#> [1] 110.6716 142.3435 185.2514

# Allow a lower conditional power target for less promising interim results.
# Leave conditionalPower unspecified when supplying a callback.
flexibleDesign <- getDesignOptimalConditionalError(
    alpha = 0.025, efficacyBounds = 0.001, futilityBounds = 0.5,
    conditionalPowerFunction = function(p) pnorm(1 - p),
    thetaH1 = 0.25, useInterimEstimate = FALSE, firstStageInformation = 80,
    likelihoodRatioDistribution = "maxlr"
)
getConditionalError(pValue = c(0.05, 0.1, 0.3), design = flexibleDesign)
#> [1] 0.08343571 0.04767596 0.02220325
```
