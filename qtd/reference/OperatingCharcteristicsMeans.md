# OperatingCharcteristicsMeans R6 class

Specialised operating-characteristics container for comparing means
(two-group designs). Inherits from `OperatingCharcteristics` and adds
fields relevant for mean comparisons and plotting.

## Value

An R6 object of class `OperatingCharcteristicsMeans`.

## Fields

- `normalApproximation`:

  Logical. Indicates the use of a normal approximation for the sampling
  distribution.

- `alternative`:

  Numeric vector. Grid of true mean differences used when computing
  probabilities.

- `stDev`:

  Numeric scalar. Assumed common standard deviation used for the normal
  approximation.

## Examples

``` r
# See getOperatingCharacteristicsMeans for an end-to-end example.
```
