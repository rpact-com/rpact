# Length of Trial Design Set

Returns the number of designs in a `TrialDesignSet`.

## Usage

``` r
# S3 method for class 'TrialDesignSet'
length(x)
```

## Arguments

- x:

  A
  [`TrialDesignSet`](https://docs.rpact.org/reference/TrialDesignSet.md)
  object.

## Value

Returns a non-negative [`integer`](https://rdrr.io/r/base/integer.html)
of length 1 representing the number of design in the `TrialDesignSet`.

## Details

Is helpful for iteration over all designs in a design set.

## Examples

``` r
if (FALSE) { # \dontrun{
designSet <- getDesignSet(design = getDesignGroupSequential(), alpha = c(0.01, 0.05))
length(designSet)
} # }
```
