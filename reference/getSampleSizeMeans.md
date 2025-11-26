# Get Sample Size Means

Returns the sample size for testing means in one or two samples.

## Usage

``` r
getSampleSizeMeans(
  design = NULL,
  ...,
  groups = 2L,
  normalApproximation = FALSE,
  meanRatio = FALSE,
  thetaH0 = ifelse(meanRatio, 1, 0),
  alternative = seq(0.2, 1, 0.2),
  stDev = 1,
  allocationRatioPlanned = NA_real_
)
```

## Arguments

- design:

  The trial design. If no trial design is specified, a fixed sample size
  design is used. In this case, Type I error rate `alpha`, Type II error
  rate `beta`, `twoSidedPower`, and `sided` can be directly entered as
  argument where necessary.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- groups:

  The number of treatment groups (1 or 2), default is `2`.

- normalApproximation:

  The type of computation of the p-values. If `TRUE`, the variance is
  assumed to be known, default is `FALSE`, i.e., the calculations are
  performed with the t distribution.

- meanRatio:

  If `TRUE`, the sample size for one-sided testing of H0:
  `mu1 / mu2 = thetaH0` is calculated, default is `FALSE`.

- thetaH0:

  The null hypothesis value, default is `0` for the normal and the
  binary case (testing means and rates, respectively), it is `1` for the
  survival case (testing the hazard ratio).  
    
  For non-inferiority designs, `thetaH0` is the non-inferiority bound.
  That is, in case of (one-sided) testing of

  - *means*: a value `!= 0` (or a value `!= 1` for testing the mean
    ratio) can be specified.

  - *rates*: a value `!= 0` (or a value `!= 1` for testing the risk
    ratio `pi1 / pi2`) can be specified.

  - *survival data*: a bound for testing H0:
    `hazard ratio = thetaH0 != 1` can be specified.

  - *count data*: a bound for testing H0:
    `lambda1 / lambda2 = thetaH0 != 1` can be specified.

  For testing a rate in one sample, a value `thetaH0` in (0, 1) has to
  be specified for defining the null hypothesis H0: `pi = thetaH0`.

- alternative:

  The alternative hypothesis value for testing means. This can be a
  vector of assumed alternatives, default is `seq(0, 1, 0.2)` (power
  calculations) or `seq(0.2, 1, 0.2)` (sample size calculations).

- stDev:

  The standard deviation under which the sample size or power
  calculation is performed, default is `1`. For two-armed trials, it is
  allowed to specify the standard deviations separately, i.e., as vector
  with two elements. If `meanRatio = TRUE` is specified, `stDev` defines
  the coefficient of variation `sigma / mu2`.

- allocationRatioPlanned:

  The planned allocation ratio `n1 / n2` for a two treatment groups
  design, default is `1`. If `allocationRatioPlanned = 0` is entered,
  the optimal allocation ratio yielding the smallest overall sample size
  is determined.

## Value

Returns a
[`TrialDesignPlan`](https://docs.rpact.org/reference/TrialDesignPlan.md)
object. The following generics (R generic functions) are available for
this result object:

- [`names()`](https://docs.rpact.org/reference/names.FieldSet.md) to
  obtain the field names,

- [`print()`](https://docs.rpact.org/reference/print.FieldSet.md) to
  print the object,

- [`summary()`](https://docs.rpact.org/reference/summary.TrialDesignSet.md)
  to display a summary of the object,

- [`plot()`](https://docs.rpact.org/reference/plot.TrialDesignPlan.md)
  to plot the object,

- [`as.data.frame()`](https://docs.rpact.org/reference/as.data.frame.TrialDesignPlan.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://docs.rpact.org/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

At given design the function calculates the stage-wise and maximum
sample size for testing means. In a two treatment groups design,
additionally, an allocation ratio = `n1 / n2` can be specified where
`n1` and `n2` are the number of subjects in the two treatment groups. A
null hypothesis value thetaH0 != 0 for testing the difference of two
means or thetaH0 != 1 for testing the ratio of two means can be
specified. Critical bounds and stopping for futility bounds are provided
at the effect scale (mean, mean difference, or mean ratio, respectively)
for each sample size calculation separately.

## How to get help for generic functions

Click on the link of a generic in the list above to go directly to the
help documentation of the `rpact` specific implementation of the
generic. Note that you can use the R function
[`methods`](https://rdrr.io/r/utils/methods.html) to get all the methods
of a generic and to identify the object specific name of it, e.g., use
`methods("plot")` to get all the methods for the `plot` generic. There
you can find, e.g., `plot.AnalysisResults` and obtain the specific help
documentation linked above by typing
[`?plot.AnalysisResults`](https://docs.rpact.org/reference/plot.AnalysisResults.md).

## See also

Other sample size functions:
[`getSampleSizeCounts()`](https://docs.rpact.org/reference/getSampleSizeCounts.md),
[`getSampleSizeRates()`](https://docs.rpact.org/reference/getSampleSizeRates.md),
[`getSampleSizeSurvival()`](https://docs.rpact.org/reference/getSampleSizeSurvival.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate sample sizes in a fixed sample size parallel group design 
# with allocation ratio \code{n1 / n2 = 2} for a range of 
# alternative values 1, ..., 5 with assumed standard deviation = 3.5; 
# two-sided alpha = 0.05, power 1 - beta = 90%:
getSampleSizeMeans(alpha = 0.05, beta = 0.1, sided = 2, groups = 2, 
    alternative = seq(1, 5, 1), stDev = 3.5, allocationRatioPlanned = 2)

# Calculate sample sizes in a three-stage Pocock paired comparison design testing 
# H0: mu = 2 for a range of alternative values 3,4,5 with assumed standard 
# deviation = 3.5; one-sided alpha = 0.05, power 1 - beta = 90%:
getSampleSizeMeans(getDesignGroupSequential(typeOfDesign = "P", alpha = 0.05, 
    sided = 1, beta = 0.1), groups = 1, thetaH0 = 2, 
    alternative = seq(3, 5, 1), stDev = 3.5)
    
# Calculate sample sizes in a three-stage Pocock two-armed design testing 
# H0: mu = 2 for a range of alternative values 3,4,5 with assumed standard 
# deviations = 3 and 4, respectively, in the two groups of observations; 
# one-sided alpha = 0.05, power 1 - beta = 90%:
getSampleSizeMeans(getDesignGroupSequential(typeOfDesign = "P", alpha = 0.05, 
    sided = 1, beta = 0.1), groups = 2,
    alternative = seq(3, 5, 1), stDev = c(3, 4))
} # }
```
