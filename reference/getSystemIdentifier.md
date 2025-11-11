# Get System Identifier

This function generates a unique system identifier based on the
platform, R version, and rpact package version.

## Usage

``` r
getSystemIdentifier(date = NULL)
```

## Arguments

- date:

  A character string or `Date` representing the date. Default is
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

## Value

A character string representing the unique system identifier.

## Examples

``` r
if (FALSE) { # \dontrun{
getSystemIdentifier()
} # }
```
