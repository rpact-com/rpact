# Get Sample Size Counts

Returns the sample size for testing the ratio of mean rates of negative
binomial distributed event numbers in two samples at given effect.

## Usage

``` r
getSampleSizeCounts(
  design = NULL,
  ...,
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
  maxNumberOfSubjects = NA_integer_,
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

- maxNumberOfSubjects:

  `maxNumberOfSubjects > 0` needs to be specified for power calculations
  or calculation of necessary follow-up (count data). For two treatment
  arms, it is the maximum number of subjects for both treatment arms.

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

At given design the function calculates the information, and stage-wise
and maximum sample size for testing mean rates of negative binomial
distributed event numbers in two samples at given effect. The sample
size calculation is performed either for a fixed exposure time or a
variable exposure time with fixed follow-up. For the variable exposure
time case, at given maximum sample size the necessary follow-up time is
calculated. The planned calendar time of interim stages is calculated if
an accrual time is defined. Additionally, an allocation ratio =
`n1 / n2` can be specified where `n1` and `n2` are the number of
subjects in the two treatment groups. A null hypothesis value `thetaH0`
can also be specified.

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
[`getSampleSizeMeans()`](https://rpact-com.github.io/rpact/reference/getSampleSizeMeans.md),
[`getSampleSizeRates()`](https://rpact-com.github.io/rpact/reference/getSampleSizeRates.md),
[`getSampleSizeSurvival()`](https://rpact-com.github.io/rpact/reference/getSampleSizeSurvival.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Fixed sample size trial where a therapy is assumed to decrease the 
# exacerbation rate from 1.4 to 1.05 (25% decrease) within an observation 
# period of 1 year, i.e., each subject has an equal follow-up of 1 year.
# The sample size that yields 90% power at significance level 0.025 for 
# detecting such a difference, if the overdispersion is assumed to be 
# equal to 0.5, is obtained by
getSampleSizeCounts(alpha = 0.025, beta = 0.1, lambda2 = 1.4, 
    theta = 0.75, overdispersion = 0.5, fixedExposureTime = 1)

# Noninferiority test with blinded sample size reassessment to reproduce 
# Table 2 from Friede and Schmidli (2010):
getSampleSizeCounts(alpha = 0.025, beta = 0.2, lambda = 1, theta = 1,
    thetaH0 = 1.15, overdispersion = 0.4, fixedExposureTime = 1)

# Group sequential alpha and beta spending function design with O'Brien and 
# Fleming type boundaries: Estimate observation time under uniform 
# recruitment of patients over 6 months and a fixed exposure time of 12 
# months (lambda1, lambda2, and overdispersion as specified):
getSampleSizeCounts(design = getDesignGroupSequential(
        kMax = 3, alpha = 0.025, beta = 0.2, 
        typeOfDesign = "asOF", typeBetaSpending = "bsOF"), 
    lambda1 = 0.2, lambda2 = 0.3, overdispersion = 1, 
    fixedExposureTime = 12, accrualTime = 6)

# Group sequential alpha spending function design with O'Brien and Fleming 
# type boundaries: Sample size for variable exposure time with uniform 
# recruitment over 1.25 months and study time (accrual + followup) = 4 
# (lambda1, lambda2, and overdispersion as specified, no futility stopping):
getSampleSizeCounts(design = getDesignGroupSequential(
        kMax = 3, alpha = 0.025, beta = 0.2, typeOfDesign = "asOF"),
    lambda1 = 0.0875, lambda2 = 0.125, overdispersion = 5, 
    followUpTime = 2.75, accrualTime = 1.25)
} # }
```
