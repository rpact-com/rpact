# Create output in Markdown

The `kable()` function returns the output of the specified object
formatted in Markdown.

## Usage

``` r
# S3 method for class 'ParameterSet'
kable(x, ...)

# S3 method for class 'FieldSet'
kable(x, ..., enforceRowNames = TRUE, niceColumnNamesEnabled = TRUE)

# S3 method for class 'data.frame'
kable(x, ...)

# S3 method for class 'table'
kable(x, ...)

# S3 method for class 'matrix'
kable(x, ...)

# S3 method for class 'array'
kable(x, ...)

# S3 method for class 'numeric'
kable(x, ...)

# S3 method for class 'character'
kable(x, ...)

# S3 method for class 'logical'
kable(x, ...)

kable(x, ...)
```

## Arguments

- x:

  A `ParameterSet`. If x does not inherit from class
  [`ParameterSet`](https://docs.rpact.org/reference/ParameterSet.md),
  `knitr::kable(x)` will be returned.

- ...:

  Other arguments (see
  [`kable`](https://rdrr.io/pkg/knitr/man/kable.html)).

## Details

This function is deprecated and should no longer be used. Manual use of
kable() for rpact result objects is no longer needed, as the formatting
and display will be handled automatically by the rpact package. Please
remove any manual kable() calls from your code to avoid redundancy and
potential issues. The results will be displayed in a consistent format
automatically.
