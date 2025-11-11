# Extract a single parameter

Fetch a parameter from a parameter set.

## Usage

``` r
obtain(x, ..., output)

# S3 method for class 'ParameterSet'
obtain(x, ..., output = c("named", "labeled", "value", "list"))

fetch(x, ..., output)

# S3 method for class 'ParameterSet'
fetch(x, ..., output = c("named", "labeled", "value", "list"))
```

## Arguments

- x:

  The
  [`ParameterSet`](https://rpact-com.github.io/rpact/reference/ParameterSet.md)
  object to fetch from.

- ...:

  One or more variables specified as:

  - a literal variable name

  - a positive integer, giving the position counting from the left

  - a negative integer, giving the position counting from the right. The
    default returns the last parameter. This argument is taken by
    expression and supports quasiquotation (you can unquote column names
    and column locations).

- output:

  A character defining the output type as follows:

  - "named" (default) returns the named value if the value is a single
    value, the value inside a named list otherwise

  - "value" returns only the value itself

  - "list" returns the value inside a named list

## Examples

``` r
if (FALSE) { # \dontrun{
getDesignInverseNormal() |> fetch(kMax)
getDesignInverseNormal() |> fetch(kMax, output = "list")
} # }
```
