# Names of a Trial Design Set Object

Function to get the names of a
[`TrialDesignSet`](https://docs.rpact.org/reference/TrialDesignSet.md)
object.

## Usage

``` r
# S3 method for class 'TrialDesignSet'
names(x)
```

## Arguments

- x:

  A
  [`TrialDesignSet`](https://docs.rpact.org/reference/TrialDesignSet.md)
  object.

## Value

Returns a [`character`](https://rdrr.io/r/base/character.html) vector
containing the names of the
[`AnalysisResults`](https://docs.rpact.org/reference/AnalysisResults.md)
object.

## Details

Returns the names of a design set that can be accessed by the user.

## Examples

``` r
if (FALSE) { # \dontrun{
designSet <- getDesignSet(design = getDesignGroupSequential(), alpha = c(0.01, 0.05))
names(designSet)
} # }
```
