# Get Power Rates

Returns the power, stopping probabilities, and expected sample size for
testing rates in one or two samples at given maximum sample size.

## Usage

``` r
getPowerRates(
  design = NULL,
  ...,
  groups = 2L,
  riskRatio = FALSE,
  thetaH0 = ifelse(riskRatio, 1, 0),
  pi1 = seq(0.2, 0.5, 0.1),
  pi2 = 0.2,
  directionUpper = NA,
  maxNumberOfSubjects = NA_real_,
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

- riskRatio:

  If `TRUE`, the power for one-sided testing of H0:
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

- directionUpper:

  Logical. Specifies the direction of the alternative, only applicable
  for one-sided testing; default is `TRUE` which means that larger
  values of the test statistics yield smaller p-values.

- maxNumberOfSubjects:

  `maxNumberOfSubjects > 0` needs to be specified for power calculations
  or calculation of necessary follow-up (count data). For two treatment
  arms, it is the maximum number of subjects for both treatment arms.

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

At given design the function calculates the power, stopping
probabilities, and expected sample size for testing rates at given
maximum sample size. The sample sizes over the stages are calculated
according to the specified information rate in the design. In a two
treatment groups design, additionally, an allocation ratio = `n1 / n2`
can be specified where `n1` and `n2` are the number of subjects in the
two treatment groups. If a null hypothesis value thetaH0 != 0 for
testing the difference of two rates or `thetaH0 != 1` for testing the
risk ratio is specified, the formulas according to Farrington & Manning
(Statistics in Medicine, 1990) are used (only one-sided testing).
Critical bounds and stopping for futility bounds are provided at the
effect scale (rate, rate difference, or rate ratio, respectively). For
the two-sample case, the calculation here is performed at fixed pi2 as
given as argument in the function. Note that the power calculation for
rates is always based on the normal approximation.

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

Other power functions:
[`getPowerCounts()`](https://rpact-com.github.io/rpact/reference/getPowerCounts.md),
[`getPowerMeans()`](https://rpact-com.github.io/rpact/reference/getPowerMeans.md),
[`getPowerSurvival()`](https://rpact-com.github.io/rpact/reference/getPowerSurvival.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate the power, stopping probabilities, and expected sample size in a
# two-armed design at given maximum sample size N = 200 in a three-stage 
# O'Brien & Fleming design with information rate vector (0.2,0.5,1), 
# non-binding futility boundaries (0,0), i.e., the study stops for futility 
# if the p-value exceeds 0.5 at interm, and allocation ratio = 2 for a range 
# of pi1 values when testing H0: pi1 - pi2 = -0.1:
getPowerRates(getDesignGroupSequential(informationRates = c(0.2, 0.5, 1), 
    futilityBounds = c(0, 0)), groups = 2, thetaH0 = -0.1, 
    pi1 = seq(0.3, 0.6, 0.1), directionUpper = FALSE, 
    pi2 = 0.7, allocationRatioPlanned = 2, maxNumberOfSubjects = 200)

# Calculate the power, stopping probabilities, and expected sample size in a single 
# arm design at given maximum sample size N = 60 in a three-stage two-sided 
# O'Brien & Fleming design with information rate vector (0.2, 0.5,1) 
# for a range of pi1 values when testing H0: pi = 0.3:
getPowerRates(getDesignGroupSequential(informationRates = c(0.2, 0.5,1), 
    sided = 2), groups = 1, thetaH0 = 0.3, pi1 = seq(0.3, 0.5, 0.05),  
    maxNumberOfSubjects = 60)
} # }
```
