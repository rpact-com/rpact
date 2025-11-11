# Summary Factory Printing

Prints the result object stored inside a summary factory.

## Usage

``` r
# S3 method for class 'SummaryFactory'
print(x, ..., markdown = NA, sep = NA_character_)
```

## Arguments

- x:

  The summary factory object.

- ...:

  Optional plot arguments. At the moment `xlim` and `ylim` are
  implemented for changing x or y axis limits without dropping data
  observations.

- markdown:

  If `TRUE`, the object `x` will be printed using markdown syntax;
  normal representation will be used otherwise (default is `FALSE`)

- sep:

  The separator line between the summary and the print output, default
  is `"\n\n-----\n\n"`.

## Details

Generic function to print all kinds of summary factories.
