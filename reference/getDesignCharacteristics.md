# Get Design Characteristics

Calculates the characteristics of a design and returns it.

## Usage

``` r
getDesignCharacteristics(design = NULL, ...)
```

## Arguments

- design:

  A design object. If omitted, a default design is created.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

## Value

Returns a
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
if (FALSE) { # \dontrun{
# Calculate design characteristics for a three-stage O'Brien & Fleming 
# design at power 90% and compare it with Pocock's design.  
getDesignCharacteristics(getDesignGroupSequential(beta = 0.1))
getDesignCharacteristics(getDesignGroupSequential(beta = 0.1, typeOfDesign = "P")) 
} # }
```
