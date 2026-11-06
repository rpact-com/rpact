# Trial Design Characteristics Printing

Prints the design characteristics object.

## Usage

``` r
# S3 method for class 'TrialDesignCharacteristics'
print(x, ..., markdown = NA, showDesign = TRUE)
```

## Arguments

- x:

  The trial design characteristics object.

- ...:

  Optional plot arguments. At the moment `xlim` and `ylim` are
  implemented for changing x or y axis limits without dropping data
  observations.

- markdown:

  If `TRUE`, the object `x` will be printed using markdown syntax;
  normal representation will be used otherwise (default is `FALSE`)

- showDesign:

  Show the design print output above the design characteristics, default
  is `TRUE`.

## Details

Generic function to print all kinds of design characteristics.
