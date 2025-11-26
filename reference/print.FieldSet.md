# Print Field Set Values

`print` prints its
[`FieldSet`](https://docs.rpact.org/reference/FieldSet.md) argument and
returns it invisibly (via `invisible(x)`).

## Usage

``` r
# S3 method for class 'FieldSet'
print(x, ..., markdown = NA)
```

## Arguments

- x:

  The [`FieldSet`](https://docs.rpact.org/reference/FieldSet.md) object
  to print.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- markdown:

  If `TRUE`, the object `x` will be printed using markdown syntax;
  normal representation will be used otherwise (default is `FALSE`)

## Details

Prints the parameters and results of a field set.
