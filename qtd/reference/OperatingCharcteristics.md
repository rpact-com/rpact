# Operating Characteristics for Quantitative Trial Decisions

A lightweight container for operating-characteristics results produced
by the QTD helpers. Instances store the originating design, tested
sample-size scenarios and the resulting category probabilities used for
plotting and reporting.

Initializes the shared quantitative operating-characteristics fields.

## Arguments

- design:

  The originating quantitative decision rule.

- numberOfSubjects:

  Matrix of control and treatment sample sizes.

- probabilities:

  Computed probability data frame.

## Value

An R6 object of class `OperatingCharcteristics`.

## Details

This internal base class supplies inherited fields for
`OperatingCharcteristicsMeans` and `OperatingCharcteristicsRates`.
Obtain result objects through
[`getOperatingCharacteristicsMeans()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsMeans.md)
or
[`getOperatingCharacteristicsRates()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsRates.md).
The originating design is stored internally.

## Fields

- `numberOfSubjects`:

  Supplied sample-size scenarios. Rows correspond to scenarios; the two
  columns contain the positive integer numbers of control and treatment
  subjects, in that order. Repeated scenarios are retained. Is a numeric
  matrix.

- `probabilities`:

  Computed data frame of decision-category probabilities. Contains one
  block per sample-size scenario, with all six categories and all
  supplied effect or rate scenarios. Columns identify the category,
  probability, control and treatment sample sizes, true effect and
  endpoint-specific assumptions.

## See also

[`getOperatingCharacteristicsMeans()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsMeans.md),
[`getOperatingCharacteristicsRates()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsRates.md)

## Examples

``` r
rule <- getDesignQuantitative(targetValue = 1, lowerReferenceValue = 0)
nMatrix <- matrix(c(50, 100), ncol = 2, byrow = TRUE)
# oc <- getOperatingCharacteristicsMeans(design = rule, numberOfSubjects = nMatrix)
```
