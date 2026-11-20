# Print Parameter Set Values

`print` prints its `ParameterSet` argument and returns it invisibly (via
`invisible(x)`).

## Usage

``` r
# S3 method for class 'ParameterSet'
print(x, ..., markdown = NA)
```

## Arguments

- x:

  The
  [`ParameterSet`](https://docs.rpact.org/qtd/reference/ParameterSet.md)
  object to print.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- markdown:

  If `TRUE`, the object `x` will be printed using markdown syntax;
  normal representation will be used otherwise (default is `FALSE`)

## Details

Prints the parameters and results of a parameter set. For quantitative
operating characteristics, the probability table is limited to six rows
by default. Pass `maxRows` through `...` to change this limit in console
or Markdown output. Supply a positive integer, or use `Inf` to request
the full table, subject to the console's `max.print` option.
