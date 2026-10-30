# Get Objects from a Collection

S3 generic for retrieving the objects contained in a collection. The
`TrialComparison` method delegates to `$getObjects()`.

The `TrialComparison` method returns the list of contained objects.

## Usage

``` r
getObjects(x, ...)

# S3 method for class 'TrialComparison'
getObjects(x, ...)
```

## Arguments

- x:

  An object supporting a `getObjects()` method, such as a
  `TrialComparison`.

- ...:

  Further arguments. Currently unused for `TrialComparison`.

## Value

For `TrialComparison`, the list of contained objects in their stored
order, retaining supplied names, or an empty list for an empty
comparison.

## Details

The returned list contains references to the original R6 objects.
Changing a contained object affects subsequent comparisons. Replacing or
removing entries in the returned list does not change the comparison's
membership; use [`add()`](https://docs.rpact.org/reference/add.md) and
[`delete()`](https://docs.rpact.org/reference/delete.md) to modify the
collection.

## See also

[`getComparison()`](https://docs.rpact.org/reference/getComparison.md),
[`fetch.TrialComparison()`](https://docs.rpact.org/reference/fetch.TrialComparison.md)

## Examples

``` r
comparison <- getComparison(fixed = getDesignFixed(),
    sequential = getDesignGroupSequential(kMax = 2))
objects <- comparison |> getObjects()
names(objects)
#> [1] "fixed"      "sequential"
identical(objects$fixed, comparison$fixed)
#> [1] TRUE
```
