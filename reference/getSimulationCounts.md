# Get Simulation Counts

Returns the simulated power, stopping probabilities, conditional power,
and expected sample size for testing mean rates for negative binomial
distributed event numbers in the two treatment groups testing situation.

## Usage

``` r
getSimulationCounts(
  design = NULL,
  ...,
  plannedCalendarTime = NA_real_,
  maxNumberOfSubjects = NA_real_,
  lambda1 = NA_real_,
  lambda2 = NA_real_,
  lambda = NA_real_,
  theta = NA_real_,
  directionUpper = NA,
  thetaH0 = 1,
  overdispersion = 0,
  fixedExposureTime = NA_real_,
  accrualTime = NA_real_,
  accrualIntensity = NA_real_,
  followUpTime = NA_real_,
  allocationRatioPlanned = NA_real_,
  maxNumberOfIterations = 1000L,
  seed = NA_real_,
  showStatistics = FALSE
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

- plannedCalendarTime:

  For simulating count data, the time points where an analysis is
  planned to be performed. Should be a vector of length `kMax`

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

- directionUpper:

  Logical. Specifies the direction of the alternative, only applicable
  for one-sided testing; default is `TRUE` which means that larger
  values of the test statistics yield smaller p-values.

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

- maxNumberOfIterations:

  The number of simulation iterations, default is `1000`. Must be a
  positive integer of length 1.

- seed:

  The seed to reproduce the simulation, default is a random seed.

- showStatistics:

  Logical. If `TRUE`, summary statistics of the simulated data are
  displayed for the `print` command, otherwise the output is suppressed,
  default is `FALSE`.

## Value

Returns a
[`SimulationResults`](https://rpact-com.github.io/rpact/reference/SimulationResults.md)
object. The following generics (R generic functions) are available for
this object:

- [`names()`](https://rpact-com.github.io/rpact/reference/names.FieldSet.md)
  to obtain the field names,

- [`print()`](https://rpact-com.github.io/rpact/reference/print.FieldSet.md)
  to print the object,

- [`summary()`](https://rpact-com.github.io/rpact/reference/summary.ParameterSet.md)
  to display a summary of the object,

- [`plot()`](https://rpact-com.github.io/rpact/reference/plot.SimulationResults.md)
  to plot the object,

- [`as.data.frame()`](https://rpact-com.github.io/rpact/reference/as.data.frame.ParameterSet.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://rpact-com.github.io/rpact/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

At given design the function simulates the power, stopping
probabilities, conditional power, and expected sample size at given
number of subjects and parameter configuration. Additionally, an
allocation `ratio = n1/n2` and a null hypothesis value `thetaH0` can be
specified.

## Simulation Data

The summary statistics "Simulated data" contains the following
parameters: median [range](https://rdrr.io/r/base/range.html); mean
+/-sd  

`$show(showStatistics = FALSE)` or `$setShowStatistics(FALSE)` can be
used to disable the output of the aggregated simulated data.  

[`getData()`](https://rpact-com.github.io/rpact/reference/getData.md)
can be used to get the aggregated simulated data from the object as
[`data.frame`](https://rdrr.io/r/base/data.frame.html). The data frame
contains the following columns:

1.  `iterationNumber`: The number of the simulation iteration.

2.  `stageNumber`: The stage.

3.  `lambda1`: The assumed or derived event rate in the treatment group.

4.  `lambda2`: The assumed or derived event rate in the control group.

5.  `accrualTime`: The assumed accrualTime.

6.  `followUpTime`: The assumed followUpTime.

7.  `overdispersion`: The assumed overdispersion.

8.  `fixedFollowUp`: The assumed fixedFollowUp.

9.  `numberOfSubjects`: The number of subjects under consideration when
    the (interim) analysis takes place.

10. `rejectPerStage`: 1 if null hypothesis can be rejected, 0 otherwise.

11. `futilityPerStage`: 1 if study should be stopped for futility, 0
    otherwise.

12. `testStatistic`: The test statistic that is used for the test
    decision

13. `estimatedLambda1`: The estimated rate in treatment group 1.

14. `estimatedLambda2`: The estimated rate in treatment group 2.

15. `estimatedOverdispersion`: The estimated overdispersion.

16. `infoAnalysis`: The Fisher information at interim stage.

17. `trialStop`: `TRUE` if study should be stopped for efficacy or
    futility or final stage, `FALSE` otherwise.

18. `conditionalPowerAchieved`: Not yet available

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

## Examples

``` r
if (FALSE) { # \dontrun{
# Fixed sample size design with two groups, fixed exposure time
getSimulationCounts(
    theta = 1.8,
    lambda2 = 0.2,
    maxNumberOfSubjects = 200,
    plannedCalendarTime = 8,
    maxNumberOfIterations = 1000,
    fixedExposureTime = 6,
    accrualTime = 3,
    overdispersion = 2)

# Group sequential design alpha spending function design with O'Brien and 
# Fleming type boundaries: Power and test characteristics for N = 264, 
# under variable exposure time with uniform recruitment over 1.25 months,
# study time (accrual + followup) = 4, interim analysis take place after
# equidistant time points (lambda1, lambda2, and overdispersion as specified,
# no futility stopping):
dOF <- getDesignGroupSequential(
    kMax = 3,
    alpha = 0.025,
    beta = 0.2,
    typeOfDesign = "asOF")

getSimulationCounts(design = dOF,
    lambda1 = seq(0.04, 0.12, 0.02),
    lambda2 = 0.12,
    directionUpper = FALSE,
    overdispersion = 5,
    plannedCalendarTime = (1:3)/3*4,
    maxNumberOfSubjects = 264,
    followUpTime = 2.75,
    accrualTime = 1.25,
    maxNumberOfIterations = 1000)
} # }
```
