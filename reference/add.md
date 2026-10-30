# Add Objects to a Collection

S3 generic for adding objects. The `TrialComparison` method delegates to
`$add()` and modifies the existing comparison by reference.

The `TrialComparison` method appends objects to the comparison.

## Usage

``` r
add(x, ...)

# S3 method for class 'TrialComparison'
add(x, ...)
```

## Arguments

- x:

  An object supporting an `add()` method, such as a `TrialComparison`.

- ...:

  For `TrialComparison`, designs, plans, simulations, lists or
  comparisons to append. The optional named argument `objects` is
  appended after the other arguments, with the same rules as
  [`getComparison()`](https://docs.rpact.org/reference/getComparison.md).

## Value

The modified comparison invisibly for `TrialComparison`.

## See also

[`getComparison()`](https://docs.rpact.org/reference/getComparison.md),
[`delete()`](https://docs.rpact.org/reference/delete.md)

## Examples

``` r
comparison <- getComparison(fixed = getDesignFixed())
comparison |> add(sequential = getDesignGroupSequential(kMax = 2))
comparison |> add(objects = list(other = getDesignFixed(alpha = 0.05)))
```
