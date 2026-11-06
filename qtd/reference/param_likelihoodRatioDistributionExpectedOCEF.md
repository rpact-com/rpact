# Parameter Description: Likelihood ratio distribution for expected information (optimal conditional error design)

Parameter Description: Likelihood ratio distribution for expected
information (optimal conditional error design)

## Arguments

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
