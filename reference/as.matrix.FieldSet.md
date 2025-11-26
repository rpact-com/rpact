# Coerce Field Set to a Matrix

Returns the `FrameSet` as matrix.

## Usage

``` r
# S3 method for class 'FieldSet'
as.matrix(x, ..., enforceRowNames = TRUE, niceColumnNamesEnabled = TRUE)
```

## Arguments

- x:

  A [`FieldSet`](https://docs.rpact.org/reference/FieldSet.md) object.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- enforceRowNames:

  If `TRUE`, row names will be created depending on the object type,
  default is `TRUE`.

- niceColumnNamesEnabled:

  Logical. If `TRUE`, nice looking column names will be used; syntactic
  names (variable names) otherwise (see
  [`make.names`](https://rdrr.io/r/base/make.names.html)).

## Value

Returns a [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

Coerces the frame set to a matrix.
