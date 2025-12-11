# Summarize Futility Bounds

S3 summary method for objects of class `FutilityBounds`.

## Usage

``` r
# S3 method for class 'FutilityBounds'
summary(object, ...)
```

## Arguments

- object:

  An object of class `FutilityBounds`.

- ...:

  Additional arguments (currently not used).

## Details

Prints a categorized summary of futility bound parameters, including
user-defined, derived, default, and generated values.

## Examples

``` r
if (FALSE) { # \dontrun{
futilityBounds <- getFutilityBounds(
    design = getDesignInverseNormal(kMax = 2),
    sourceValue = 0.5,
    sourceScale = "condPowerAtObserved",
    targetScale = "zValue"
)
summary(futilityBounds)
} # }
```
