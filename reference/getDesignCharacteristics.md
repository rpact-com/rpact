# Get Design Characteristics

Calculates the characteristics of a design and returns it.

## Usage

``` r
getDesignCharacteristics(design = NULL, ...)
```

## Arguments

- design:

  A conventional `TrialDesign` object or an optimal conditional error
  design created with
  [`getDesignOptimalConditionalError()`](https://docs.rpact.org/reference/getDesignOptimalConditionalError.md).
  If omitted, a default conventional design is created.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

## Value

For an optimal conditional error design, a
`TrialDesignOptimalConditionalErrorCharacteristics` parameter set with
`theta`, `overallReject`, `rejectPerStage` (two rows, one per stage),
and `futilityPerStage` (one row for interim futility). Matrix columns
correspond to entries in `theta`, including for a single effect. Final
non-rejection is not counted as early futility. `overallReject` is
overall power under an alternative and the type I error probability at
`theta = 0`. See
[`getDesignOptimalConditionalError()`](https://docs.rpact.org/reference/getDesignOptimalConditionalError.md)
for the method and its assumptions.

For conventional designs, returns a
[`TrialDesignCharacteristics`](https://docs.rpact.org/reference/TrialDesignCharacteristics.md)
object. The following generics (R generic functions) are available for
this result object:

- [`names()`](https://docs.rpact.org/reference/names.FieldSet.md) to
  obtain the field names,

- [`print()`](https://docs.rpact.org/reference/print.FieldSet.md) to
  print the object,

- [`summary()`](https://docs.rpact.org/reference/summary.ParameterSet.md)
  to display a summary of the object,

- [`plot()`](https://docs.rpact.org/reference/plot.ParameterSet.md) to
  plot the object,

- [`as.data.frame()`](https://docs.rpact.org/reference/as.data.frame.TrialDesignCharacteristics.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://docs.rpact.org/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

Calculates the inflation factor (IF), the expected reduction in sample
size under H1, under H0, and under a value in between H0 and H1.
Furthermore, absolute information values are calculated under the
prototype case testing H0: mu = 0 against H1: mu = 1.

For an optimal conditional error design, supply `theta` through `...` as
a finite numeric vector of treatment effects on the mean difference
scale. The design is evaluated at those effects without recalibration.

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

Other design functions:
[`getDesignConditionalDunnett()`](https://docs.rpact.org/reference/getDesignConditionalDunnett.md),
[`getDesignFisher()`](https://docs.rpact.org/reference/getDesignFisher.md),
[`getDesignFixed()`](https://docs.rpact.org/reference/getDesignFixed.md),
[`getDesignGroupSequential()`](https://docs.rpact.org/reference/getDesignGroupSequential.md),
[`getDesignInverseNormal()`](https://docs.rpact.org/reference/getDesignInverseNormal.md),
[`getGroupSequentialProbabilities()`](https://docs.rpact.org/reference/getGroupSequentialProbabilities.md),
[`getPowerAndAverageSampleNumber()`](https://docs.rpact.org/reference/getPowerAndAverageSampleNumber.md)

## Examples

``` r
design <- getDesignOptimalConditionalError(
    alpha = 0.025, efficacyBounds = 0.001, futilityBounds = 0.5, conditionalPower = 0.9,
    thetaH1 = 0.5, useInterimEstimate = FALSE, firstStageInformation = 4,
    likelihoodRatioDistribution = "maxlr"
)
getDesignCharacteristics(design, theta = c(0, 0.25, 0.5))
#> User defined parameters:
#>   Effect                      : 0.00, 0.25, 0.50 
#> 
#> Operating characteristics:
#>   Overall reject              : 0.0250, 0.2927, 0.7590 
#>   Reject per stage [1]        : 0.001000, 0.004796, 0.018298 
#>   Reject per stage [2]        : 0.024000, 0.287861, 0.740742 
#>   Futility stop per stage     : 0.5000, 0.3085, 0.1587 
#> 

if (FALSE) { # \dontrun{
# Calculate design characteristics for a three-stage O'Brien & Fleming 
# design at power 90% and compare it with Pocock's design.  
getDesignCharacteristics(getDesignGroupSequential(beta = 0.1))
getDesignCharacteristics(getDesignGroupSequential(beta = 0.1, typeOfDesign = "P")) 
} # }
```
