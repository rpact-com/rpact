# Get Power Counts

Returns the power, stopping probabilities, and expected sample size for
testing mean rates for negative binomial distributed event numbers in
two samples at given sample sizes.

## Usage

``` r
getPowerCounts(
  design = NULL,
  ...,
  directionUpper = NA,
  maxNumberOfSubjects = NA_real_,
  lambda1 = NA_real_,
  lambda2 = NA_real_,
  lambda = NA_real_,
  theta = NA_real_,
  thetaH0 = 1,
  overdispersion = 0,
  fixedExposureTime = NA_real_,
  accrualTime = NA_real_,
  accrualIntensity = NA_real_,
  followUpTime = NA_real_,
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

- directionUpper:

  Logical. Specifies the direction of the alternative, only applicable
  for one-sided testing; default is `TRUE` which means that larger
  values of the test statistics yield smaller p-values.

- maxNumberOfSubjects:

  `maxNumberOfSubjects > 0` needs to be specified for power calculations
  or calculation of necessary follow-up (count data). For two treatment
  arms, it is the maximum number of subjects for both treatment arms.

- lambda1:

  A numeric value or vector that represents the assumed rate of a
  homogeneous Poisson process in the active treatment group, there is no
  default.

- lambda2:

  A numeric value that represents the assumed rate of a homogeneous
  Poisson process in the control group, there is no default.

- lambda:

  A numeric value or vector that represents the assumed rate of a
  homogeneous Poisson process in the pooled treatment groups, there is
  no default.

- theta:

  A numeric value or vector that represents the assumed mean ratios
  lambda1/lambda2 of a homogeneous Poisson process, there is no default.

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

- overdispersion:

  A numeric value that represents the assumed overdispersion of the
  negative binomial distribution, default is `0`.

- fixedExposureTime:

  If specified, the fixed time of exposure per subject for count data,
  there is no default.

- accrualTime:

  If specified, the assumed accrual time interval(s) for the study,
  there is no default.

- accrualIntensity:

  If specified, the assumed accrual intensities for the study, there is
  no default.

- followUpTime:

  If specified, the assumed (additional) follow-up time for the study,
  there is no default. The total study duration is
  `accrualTime + followUpTime`.

- allocationRatioPlanned:

  The planned allocation ratio `n1 / n2` for a two treatment groups
  design, default is `1`. For multi-arm designs, it is the allocation
  ratio relating the active arm(s) to the control. For simulating means
  and rates for a two treatment groups design, it can be a vector of
  length `kMax`, the number of stages. It can be a vector of length
  `kMax`, too, for multi-arm and enrichment designs. In these cases, a
  change of allocating subjects to treatment groups over the stages can
  be assessed. Note that internally `allocationRatioPlanned` is treated
  as a vector of length `kMax`, not a scalar.

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

At given design the function calculates the power, stopping
probabilities, and expected sample size for testing the ratio of two
mean rates of negative binomial distributed event numbers in two samples
at given maximum sample size and effect. The power calculation is
performed either for a fixed exposure time or a variable exposure time
with fixed follow-up where the information over the stages is calculated
according to the specified information rate in the design. Additionally,
an allocation ratio = `n1 / n2` can be specified where `n1` and `n2` are
the number of subjects in the two treatment groups. A null hypothesis
value `thetaH0` can also be specified.

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

Other power functions:
[`getPowerMeans()`](https://docs.rpact.org/reference/getPowerMeans.md),
[`getPowerRates()`](https://docs.rpact.org/reference/getPowerRates.md),
[`getPowerSurvival()`](https://docs.rpact.org/reference/getPowerSurvival.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Fixed sample size trial where a therapy is assumed to decrease the 
# exacerbation rate from 1.4 to 1.05 (25% decrease) within an 
# observation period of 1 year, i.e., each subject has a equal 
# follow-up of 1 year.
# Calculate power at significance level 0.025 at given sample size = 180 
# for a range of lambda1 values if the overdispersion is assumed to be 
# equal to 0.5, is obtained by
getPowerCounts(alpha = 0.025, lambda1 = seq(1, 1.4, 0.05), lambda2 = 1.4, 
    maxNumberOfSubjects = 180, overdispersion = 0.5, fixedExposureTime = 1)

# Group sequential alpha and beta spending function design with O'Brien and 
# Fleming type boundaries: Power and test characteristics for N = 286, 
# under the assumption of a fixed exposure time, and for a range of 
# lambda1 values: 
getPowerCounts(design = getDesignGroupSequential(
        kMax = 3, alpha = 0.025, beta = 0.2, 
        typeOfDesign = "asOF", typeBetaSpending = "bsOF"), 
    lambda1 = seq(0.17, 0.23, 0.01), lambda2 = 0.3, 
    directionUpper = FALSE, overdispersion = 1, maxNumberOfSubjects = 286, 
    fixedExposureTime = 12, accrualTime = 6)

# Group sequential design alpha spending function design with O'Brien and 
# Fleming type boundaries: Power and test characteristics for N = 1976, 
# under variable exposure time with uniform recruitment over 1.25 months,
# study time (accrual + followup) = 4 (lambda1, lambda2, and overdispersion 
# as specified, no futility stopping):
getPowerCounts(design = getDesignGroupSequential(
        kMax = 3, alpha = 0.025, beta = 0.2, typeOfDesign = "asOF"),
    lambda1 = seq(0.08, 0.09, 0.0025), lambda2 = 0.125, 
    overdispersion = 5, directionUpper = FALSE, maxNumberOfSubjects = 1976, 
    followUpTime = 2.75, accrualTime = 1.25)
} # }
```
