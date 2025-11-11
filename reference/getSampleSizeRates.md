# Get Sample Size Rates

Returns the sample size for testing rates in one or two samples.

## Usage

``` r
getSampleSizeRates(
  design = NULL,
  ...,
  groups = 2L,
  normalApproximation = TRUE,
  conservative = TRUE,
  riskRatio = FALSE,
  thetaH0 = ifelse(riskRatio, 1, 0),
  pi1 = c(0.4, 0.5, 0.6),
  pi2 = 0.2,
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

  If `FALSE`, the sample size for the case of one treatment group is
  calculated exactly using the binomial distribution, default is `TRUE`.

- conservative:

  For the case of one treatment group and `normalApproximation = FALSE`,
  if `TRUE`, the sample size is calculated such that for larger sample
  size than the calculated, the power is larger than 1 - beta, for
  `conservative = FALSE`, the minimum sample size, for which power
  exceeds 1 - beta is calculated, default is `TRUE`.

- riskRatio:

  If `TRUE`, the sample size for one-sided testing of H0:
  `pi1 / pi2 = thetaH0` is calculated, default is `FALSE`.

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

- pi1:

  A numeric value or vector that represents the assumed probability in
  the active treatment group if two treatment groups are considered, or
  the alternative probability for a one treatment group design, default
  is `seq(0.2, 0.5, 0.1)` (power calculations and simulations) or
  `seq(0.4, 0.6, 0.1)` (sample size calculations).

- pi2:

  A numeric value that represents the assumed probability in the
  reference group if two treatment groups are considered, default is
  `0.2`.

- allocationRatioPlanned:

  The planned allocation ratio `n1 / n2` for a two treatment groups
  design, default is `1`. If `allocationRatioPlanned = 0` is entered,
  the optimal allocation ratio yielding the smallest overall sample size
  is determined.

## Value

Returns a
[`TrialDesignPlan`](https://rpact-com.github.io/rpact/reference/TrialDesignPlan.md)
object. The following generics (R generic functions) are available for
this result object:

- [`names()`](https://rpact-com.github.io/rpact/reference/names.FieldSet.md)
  to obtain the field names,

- [`print()`](https://rpact-com.github.io/rpact/reference/print.FieldSet.md)
  to print the object,

- [`summary()`](https://rpact-com.github.io/rpact/reference/summary.TrialDesignSet.md)
  to display a summary of the object,

- [`plot()`](https://rpact-com.github.io/rpact/reference/plot.TrialDesignPlan.md)
  to plot the object,

- [`as.data.frame()`](https://rpact-com.github.io/rpact/reference/as.data.frame.TrialDesignPlan.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://rpact-com.github.io/rpact/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

At given design the function calculates the stage-wise and maximum
sample size for testing rates. In a two treatment groups design,
additionally, an allocation ratio = `n1 / n2` can be specified where
`n1` and `n2` are the number of subjects in the two treatment groups. If
a null hypothesis value thetaH0 != 0 for testing the difference of two
rates or thetaH0 != 1 for testing the risk ratio is specified, the
sample size formula according to Farrington & Manning (Statistics in
Medicine, 1990) is used. Critical bounds and stopping for futility
bounds are provided at the effect scale (rate, rate difference, or rate
ratio, respectively) for each sample size calculation separately. For
the two-sample case, the calculation here is performed at fixed pi2 as
given as argument in the function.

## How to get help for generic functions

Click on the link of a generic in the list above to go directly to the
help documentation of the `rpact` specific implementation of the
generic. Note that you can use the R function
[`methods`](https://rdrr.io/r/utils/methods.html) to get all the methods
of a generic and to identify the object specific name of it, e.g., use
`methods("plot")` to get all the methods for the `plot` generic. There
you can find, e.g., `plot.AnalysisResults` and obtain the specific help
documentation linked above by typing
[`?plot.AnalysisResults`](https://rpact-com.github.io/rpact/reference/plot.AnalysisResults.md).

## See also

Other sample size functions:
[`getSampleSizeCounts()`](https://rpact-com.github.io/rpact/reference/getSampleSizeCounts.md),
[`getSampleSizeMeans()`](https://rpact-com.github.io/rpact/reference/getSampleSizeMeans.md),
[`getSampleSizeSurvival()`](https://rpact-com.github.io/rpact/reference/getSampleSizeSurvival.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate the stage-wise sample sizes, maximum sample sizes, and the optimum 
# allocation ratios for a range of pi1 values when testing 
# H0: pi1 - pi2 = -0.1 within a two-stage O'Brien & Fleming design;
# alpha = 0.05 one-sided, power 1 - beta = 90%:
getSampleSizeRates(getDesignGroupSequential(kMax = 2, alpha = 0.05,  
    beta = 0.1), groups = 2, thetaH0 = -0.1, pi1 = seq(0.4, 0.55, 0.025), 
    pi2 = 0.4, allocationRatioPlanned = 0)

# Calculate the stage-wise sample sizes, maximum sample sizes, and the optimum 
# allocation ratios for a range of pi1 values when testing 
# H0: pi1 / pi2 = 0.80 within a three-stage O'Brien & Fleming design;
# alpha = 0.025 one-sided, power 1 - beta = 90%:
getSampleSizeRates(getDesignGroupSequential(kMax = 3, alpha = 0.025, 
    beta = 0.1), groups = 2, riskRatio = TRUE, thetaH0 = 0.80, 
    pi1 = seq(0.3, 0.5, 0.025), pi2 = 0.3, allocationRatioPlanned = 0)
} # }
```
