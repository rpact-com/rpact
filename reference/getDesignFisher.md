# Get Design Fisher

Performs Fisher's combination test and returns critical values for this
design.

## Usage

``` r
getDesignFisher(
  ...,
  kMax = NA_integer_,
  alpha = NA_real_,
  method = c("equalAlpha", "fullAlpha", "noInteraction", "userDefinedAlpha"),
  userAlphaSpending = NA_real_,
  alpha0Vec = NA_real_,
  alpha0Scale = c("pValue", "zValue", "condPowerAtObserved", "predictivePower"),
  informationRates = NA_real_,
  sided = 1,
  bindingFutility = NA,
  directionUpper = NA,
  tolerance = 1e-14,
  iterations = 0,
  seed = NA_real_
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

- method:

  `"equalAlpha"`, `"fullAlpha"`, `"noInteraction"`, or
  `"userDefinedAlpha"`, default is `"equalAlpha"` (for details, see
  Wassmer, 1999).

- userAlphaSpending:

  The user defined alpha spending. Numeric vector of length `kMax`
  containing the cumulative alpha-spending (Type I error rate) up to
  each interim stage: `0 <= alpha_1 <= ... <= alpha_K <= alpha`.

- alpha0Vec:

  Stopping for futility bounds for stage-wise p-values.

- alpha0Scale:

  Character. The scale of the futility bounds. Must be one of
  `"pValue"`, `"zValue"`, `"condPowerAtObserved"`, or
  `"predictivePower"`. Default is `"pValue"`.

- informationRates:

  The information rates t_1, ..., t_kMax (that must be fixed prior to
  the trial), default is `(1:kMax) / kMax`. For the weighted inverse
  normal design, the weights are derived through w_1 = sqrt(t_1), and
  w_k = sqrt(t_k - t\_(k-1)). For the weighted Fisher's combination
  test, the weights (scales) are w_k = sqrt((t_k - t\_(k-1)) / t_1) (see
  the documentation).

- sided:

  Is the alternative one-sided (`1`) or two-sided (`2`), default is `1`.
  Must be a positive integer of length 1.

- bindingFutility:

  If `bindingFutility = TRUE` is specified the calculation of the
  critical values is affected by the futility bounds (default is
  `TRUE`).

- directionUpper:

  Logical. Specifies the direction of the alternative, only applicable
  for one-sided testing; default is `TRUE` which means that larger
  values of the test statistics yield smaller p-values.

- tolerance:

  The numerical tolerance, default is `1e-14`.

- iterations:

  The number of simulation iterations, e.g.,
  `getDesignFisher(iterations = 100000)` checks the validity of the
  critical values for the design. The default value of `iterations` is
  0, i.e., no simulation will be executed.

- seed:

  Seed for simulating the power for Fisher's combination test. See
  above, default is a random seed.

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

`getDesignFisher()` calculates the critical values and stage levels for
Fisher's combination test as described in Bauer (1989), Bauer and Koehne
(1994), Bauer and Roehmel (1995), and Wassmer (1999) for equally and
unequally sized stages.

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
creating a set of designs to compare.

[`getFutilityBounds()`](https://docs.rpact.org/reference/getFutilityBounds.md)
for the specification of futility bounds on scales other than the
p-value scale.

[Vignette: Enhanced Futility Bounds
Specification](https://www.rpact.org/vignettes/planning/rpact_futility_bounds/)

Other design functions:
[`getDesignCharacteristics()`](https://docs.rpact.org/reference/getDesignCharacteristics.md),
[`getDesignConditionalDunnett()`](https://docs.rpact.org/reference/getDesignConditionalDunnett.md),
[`getDesignGroupSequential()`](https://docs.rpact.org/reference/getDesignGroupSequential.md),
[`getDesignInverseNormal()`](https://docs.rpact.org/reference/getDesignInverseNormal.md),
[`getGroupSequentialProbabilities()`](https://docs.rpact.org/reference/getGroupSequentialProbabilities.md),
[`getPowerAndAverageSampleNumber()`](https://docs.rpact.org/reference/getPowerAndAverageSampleNumber.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate critical values for a two-stage Fisher's combination test 
# with full level alpha = 0.05 at the final stage and stopping for 
# futility bound alpha0 = 0.50, as described in Bauer and Koehne (1994). 
getDesignFisher(kMax = 2, method = "fullAlpha", alpha = 0.05, alpha0Vec = 0.50) 
} # }
```
