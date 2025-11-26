# Get Design Set

Creates a trial design set object and returns it.

## Usage

``` r
getDesignSet(...)
```

## Arguments

- ...:

  `designs` or `design` and one or more design parameters, e.g.,
  `deltaWT = c(0.1, 0.3, 0.4)`.

  - `design` The master design (optional, you need to specify an
    additional parameter that shall be varied).

  - `designs` The designs to compare (optional, you need to specify the
    variable `variedParameters`).

## Value

Returns a
[`TrialDesignSet`](https://docs.rpact.org/reference/TrialDesignSet.md)
object. The following generics (R generic functions) are available for
this result object:

- [`names`](https://docs.rpact.org/reference/names.TrialDesignSet.md) to
  obtain the field names,

- [`length`](https://docs.rpact.org/reference/length.TrialDesignSet.md)
  to obtain the number of design,

- [`print()`](https://docs.rpact.org/reference/print.FieldSet.md) to
  print the object,

- [`summary()`](https://docs.rpact.org/reference/summary.TrialDesignSet.md)
  to display a summary of the object,

- [`plot()`](https://docs.rpact.org/reference/plot.TrialDesignSet.md) to
  plot the object,

- [`as.data.frame()`](https://docs.rpact.org/reference/as.data.frame.TrialDesignSet.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://docs.rpact.org/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

Specify a master design and one or more design parameters or a list of
designs.

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

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1
design <- getDesignGroupSequential(
    alpha = 0.05, kMax = 6,
    sided = 2, typeOfDesign = "WT", deltaWT = 0.1
)
designSet <- getDesignSet()
designSet$add(design = design, deltaWT = c(0.3, 0.4))
if (require(ggplot2)) plot(designSet, type = 1)

# Example 2 (shorter script)
design <- getDesignGroupSequential(
    alpha = 0.05, kMax = 6,
    sided = 2, typeOfDesign = "WT", deltaWT = 0.1
)
designSet <- getDesignSet(design = design, deltaWT = c(0.3, 0.4))
if (require(ggplot2)) plot(designSet, type = 1)

# Example 3 (use of designs instead of design)
d1 <- getDesignGroupSequential(
    alpha = 0.05, kMax = 2,
    sided = 1, beta = 0.2, typeOfDesign = "asHSD",
    gammaA = 0.5, typeBetaSpending = "bsHSD", gammaB = 0.5
)
d2 <- getDesignGroupSequential(
    alpha = 0.05, kMax = 4,
    sided = 1, beta = 0.2, typeOfDesign = "asP",
    typeBetaSpending = "bsP"
)
designSet <- getDesignSet(
    designs = c(d1, d2),
    variedParameters = c("typeOfDesign", "kMax")
)
if (require(ggplot2)) plot(designSet, type = 8, nMax = 20)
} # }
```
