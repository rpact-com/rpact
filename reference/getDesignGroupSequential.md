# Get Design Group Sequential

Provides adjusted boundaries and defines a group sequential design.

## Usage

``` r
getDesignGroupSequential(
  ...,
  kMax = NA_integer_,
  alpha = NA_real_,
  beta = NA_real_,
  sided = 1L,
  informationRates = NA_real_,
  futilityBounds = NA_real_,
  typeOfDesign = c("OF", "P", "WT", "PT", "HP", "WToptimum", "asP", "asOF", "asKD",
    "asHSD", "asUser", "noEarlyEfficacy"),
  deltaWT = NA_real_,
  deltaPT1 = NA_real_,
  deltaPT0 = NA_real_,
  optimizationCriterion = c("ASNH1", "ASNIFH1", "ASNsum"),
  gammaA = NA_real_,
  typeBetaSpending = c("none", "bsP", "bsOF", "bsKD", "bsHSD", "bsUser"),
  userAlphaSpending = NA_real_,
  userBetaSpending = NA_real_,
  efficacyStops = NA,
  futilityStops = NA,
  gammaB = NA_real_,
  bindingFutility = NA,
  futilityBoundsScale = c("zValue", "pValue", "reverseCondPower", "condPowerAtObserved",
    "predictivePower"),
  directionUpper = NA,
  betaAdjustment = NA,
  constantBoundsHP = 3,
  twoSidedPower = NA,
  delayedInformation = NA_real_,
  tolerance = 1e-08
)
```

## Arguments

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- kMax:

  The maximum number of stages `K`. Must be a positive integer of length
  1 (default value is `3`). The maximum selectable `kMax` is `20` for
  group sequential or inverse normal and `6` for Fisher combination test
  designs.

- alpha:

  The significance level alpha, default is `0.025`. Must be a positive
  numeric of length 1.

- beta:

  Type II error rate, necessary for providing sample size calculations
  (e.g.,
  [`getSampleSizeMeans()`](https://docs.rpact.org/reference/getSampleSizeMeans.md)),
  beta spending function designs, or optimum designs, default is `0.20`.
  Must be a positive numeric of length 1.

- sided:

  Is the alternative one-sided (`1`) or two-sided (`2`), default is `1`.
  Must be a positive integer of length 1.

- informationRates:

  The information rates t_1, ..., t_kMax (that must be fixed prior to
  the trial), default is `(1:kMax) / kMax`. For the weighted inverse
  normal design, the weights are derived through w_1 = sqrt(t_1), and
  w_k = sqrt(t_k - t\_(k-1)). For the weighted Fisher's combination
  test, the weights (scales) are w_k = sqrt((t_k - t\_(k-1)) / t_1) (see
  the documentation).

- futilityBounds:

  The futility bounds, defined on the scale defined by
  `futilityBoundsScale`. (numeric vector of length `kMax - 1`).

- typeOfDesign:

  The type of design. Type of design is one of the following: O'Brien &
  Fleming (`"OF"`), Pocock (`"P"`), Wang & Tsiatis Delta class (`"WT"`),
  Pampallona & Tsiatis (`"PT"`), Haybittle & Peto ("HP"), Optimum design
  within Wang & Tsiatis class (`"WToptimum"`), O'Brien & Fleming type
  alpha spending (`"asOF"`), Pocock type alpha spending (`"asP"`), Kim &
  DeMets alpha spending (`"asKD"`), Hwang, Shi & DeCani alpha spending
  (`"asHSD"`), user defined alpha spending (`"asUser"`), no early
  efficacy stop (`"noEarlyEfficacy"`), default is `"OF"`.

- deltaWT:

  Delta for Wang & Tsiatis Delta class.

- deltaPT1:

  Delta1 for Pampallona & Tsiatis class rejecting H0 boundaries.

- deltaPT0:

  Delta0 for Pampallona & Tsiatis class rejecting H1 boundaries.

- optimizationCriterion:

  Optimization criterion for optimum design within Wang & Tsiatis class
  (`"ASNH1"`, `"ASNIFH1"`, `"ASNsum"`), default is `"ASNH1"`, see
  details.

- gammaA:

  Parameter for alpha spending function.

- typeBetaSpending:

  Type of beta spending. Type of of beta spending is one of the
  following: O'Brien & Fleming type beta spending, Pocock type beta
  spending, Kim & DeMets beta spending, Hwang, Shi & DeCani beta
  spending, user defined beta spending (`"bsOF"`, `"bsP"`, `"bsKD"`,
  `"bsHSD"`, `"bsUser"`, default is `"none"`).

- userAlphaSpending:

  The user defined alpha spending. Numeric vector of length `kMax`
  containing the cumulative alpha-spending (Type I error rate) up to
  each interim stage: `0 <= alpha_1 <= ... <= alpha_K <= alpha`.

- userBetaSpending:

  The user defined beta spending. Vector of length `kMax` containing the
  cumulative beta-spending up to each interim stage.

- efficacyStops:

  Logical vector of length `kMax - 1` indicating efficacy stops. Default
  is `NA`.

- futilityStops:

  Logical vector of length `kMax - 1` indicating futility stops. Default
  is `NA`.

- gammaB:

  Parameter for beta spending function.

- bindingFutility:

  Logical. If `bindingFutility = TRUE` is specified the calculation of
  the critical values is affected by the futility bounds and the
  futility threshold is binding in the sense that the study must be
  stopped if the futility condition was reached (default is `FALSE`).

- futilityBoundsScale:

  Character. The scale of the futility bounds. Must be one of
  `"zValue"`, `"pValue"`, `"reverseCondPower"`, `"condPowerAtObserved"`,
  or `"predictivePower"`. Default is `"zValue"`.

- directionUpper:

  Logical. Specifies the direction of the alternative, only applicable
  for one-sided testing; default is `TRUE` which means that larger
  values of the test statistics yield smaller p-values.

- betaAdjustment:

  For two-sided beta spending designs, if `betaAdjustement = TRUE` a
  linear adjustment of the beta spending values is performed if an
  overlapping of decision regions for futility stopping at earlier
  stages occurs, otherwise no adjustment is performed (default is
  `TRUE`).

- constantBoundsHP:

  The constant bounds up to stage `kMax - 1` for the Haybittle & Peto
  design (default is `3`).

- twoSidedPower:

  For two-sided testing, if `twoSidedPower = TRUE` is specified the
  sample size calculation is performed by considering both tails of the
  distribution. Default is `FALSE`, i.e., it is assumed that one tail
  probability is equal to 0 or the power should be directed to one part.

- delayedInformation:

  Delay of information for delayed response designs. Can be a numeric
  value or a numeric vector of length `kMax - 1`

- tolerance:

  The numerical tolerance, default is `1e-08`.

## Value

Returns a
[`TrialDesign`](https://docs.rpact.org/reference/TrialDesign.md) object.
The following generics (R generic functions) are available for this
result object:

- [`names()`](https://docs.rpact.org/reference/names.FieldSet.md) to
  obtain the field names,

- [`print()`](https://docs.rpact.org/reference/print.FieldSet.md) to
  print the object,

- [`summary()`](https://docs.rpact.org/reference/summary.ParameterSet.md)
  to display a summary of the object,

- [`plot()`](https://docs.rpact.org/reference/plot.TrialDesign.md) to
  plot the object,

- [`as.data.frame()`](https://docs.rpact.org/reference/as.data.frame.TrialDesign.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://docs.rpact.org/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

Depending on `typeOfDesign` some parameters are specified, others not.
For example, only if `typeOfDesign` `"asHSD"` is selected, `gammaA`
needs to be specified.

If an alpha spending approach was specified (`"asOF"`, `"asP"`,
`"asKD"`, `"asHSD"`, or `"asUser"`) additionally a beta spending
function can be specified to produce futility bounds.

For optimum designs, `"ASNH1"` minimizes the expected sample size under
H1, `"ASNIFH1"` minimizes the sum of the maximum sample and the expected
sample size under H1, and `"ASNsum"` minimizes the sum of the maximum
sample size, the expected sample size under a value midway H0 and H1,
and the expected sample size under H1.

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

[`getDesignSet()`](https://docs.rpact.org/reference/getDesignSet.md) for
creating a set of designs to compare different designs.

[`getFutilityBounds()`](https://docs.rpact.org/reference/getFutilityBounds.md)
for the specification of futility bounds on scales other than the
z-value scale.

[Vignette: Enhanced Futility Bounds
Specification](https://www.rpact.org/vignettes/planning/rpact_futility_bounds/)

Other design functions:
[`getDesignCharacteristics()`](https://docs.rpact.org/reference/getDesignCharacteristics.md),
[`getDesignConditionalDunnett()`](https://docs.rpact.org/reference/getDesignConditionalDunnett.md),
[`getDesignFisher()`](https://docs.rpact.org/reference/getDesignFisher.md),
[`getDesignInverseNormal()`](https://docs.rpact.org/reference/getDesignInverseNormal.md),
[`getGroupSequentialProbabilities()`](https://docs.rpact.org/reference/getGroupSequentialProbabilities.md),
[`getPowerAndAverageSampleNumber()`](https://docs.rpact.org/reference/getPowerAndAverageSampleNumber.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate two-sided critical values for a four-stage 
# Wang & Tsiatis design with Delta = 0.25 at level alpha = 0.05
getDesignGroupSequential(kMax = 4, alpha = 0.05, sided = 2, 
    typeOfDesign = "WT", deltaWT = 0.25) 

# Calculate one-sided critical values and binding futility bounds for a three-stage 
# design with alpha- and beta-spending functions according to Kim & DeMets with gamma = 2.5
# (planned informationRates as specified, default alpha = 0.025 and beta = 0.2)
getDesignGroupSequential(kMax = 3, informationRates = c(0.3, 0.75, 1), 
    typeOfDesign = "asKD", gammaA = 2.5, typeBetaSpending = "bsKD", 
    gammaB = 2.5, bindingFutility = TRUE)

# Calculate the Pocock type alpha spending critical values if the first 
# interim analysis was performed after 40% of the maximum information was observed
# and the second after 70% of the maximum information was observed (default alpha = 0.025)
getDesignGroupSequential(informationRates = c(0.4, 0.7), typeOfDesign = "asP") 
} # }
```
