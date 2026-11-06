# Get Test Label

Returns a string representation of the input value for use as a test
label. Handles various types, including NULL, NA, vectors, and custom
objects.

## Usage

``` r
getTestLabel(x)
```

## Arguments

- x:

  The value to be converted into a test label.

## Value

A character string representing the input value.

## Examples

``` r
if (FALSE) { # \dontrun{
getTestLabel(NULL)
getTestLabel(NA)
getTestLabel(1:3)
getTestLabel(6)
getTestLabel(getDesignFisher())
} # }
```
