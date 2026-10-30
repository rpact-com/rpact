# Access Objects in a Trial Comparison

Extracts a contained comparison object by name or position.

`$` accesses named comparison objects while preserving public R6
members.

## Usage

``` r
# S3 method for class 'TrialComparison'
x[[i, ..., exact = TRUE]]

# S3 method for class 'TrialComparison'
x$name
```

## Arguments

- x:

  A `TrialComparison`.

- i:

  A single object name or a positive integer position.

- ...:

  Further indices are not supported.

- exact:

  Whether character indices must match exactly, as for list `[[`. `$`
  always matches exactly.

- name:

  An object name, supplied literally after `$`.

## Value

The contained object by reference, or NULL for an unknown name.

## Details

Object names are unique. Numeric positions refer to the stored object
order. R6 members take precedence for `$`: `x$add` is the method even
when an object is named "add"; use `x[["add"]]` to select that object.
`[[` selects contained objects first and falls back to existing R6
members for other character names. This also preserves existing internal
field access. These methods provide extraction; replacing collection
entries is not supported. Use
[`getComparison()`](https://docs.rpact.org/reference/getComparison.md)
or `$add()` to build or extend the collection.

## Examples

``` r
comparison <- getComparison(fixed = getDesignFixed(),
    sequential = getDesignGroupSequential(kMax = 2))
identical(comparison$fixed, comparison[["fixed"]])
#> [1] TRUE
identical(comparison[[1]], comparison$fixed)
#> [1] TRUE
```
