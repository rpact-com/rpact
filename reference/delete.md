# Delete an Object from a Collection

S3 generic for removing objects. The `TrialComparison` method delegates
to `$delete()` and modifies the existing comparison by reference.

The `TrialComparison` method deletes one contained object by position or
name.

## Usage

``` r
delete(x, ...)

# S3 method for class 'TrialComparison'
delete(x, ...)
```

## Arguments

- x:

  An object supporting a `delete()` method, such as a `TrialComparison`.

- ...:

  For `TrialComparison`, exactly one `index`, supplied by position or
  name: a positive integer position or an exact object name. Invalid
  selections leave the comparison unchanged. Removing the last object
  leaves an empty comparison.

## Value

The modified comparison invisibly for `TrialComparison`.

## See also

[`getComparison()`](https://docs.rpact.org/reference/getComparison.md),
[`add()`](https://docs.rpact.org/reference/add.md)

## Examples

``` r
comparison <- getComparison(fixed = getDesignFixed(),
    sequential = getDesignGroupSequential(kMax = 2))
comparison |> delete("fixed")
comparison |> delete(index = 1)
comparison |> add(getDesignFixed()) |> delete(1)
```
