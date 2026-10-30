# Fetch Objects from a Trial Comparison

Extracts contained objects by name or position without modifying the
comparison or copying its objects. A single selection returns the object
itself by default.

## Usage

``` r
# S3 method for class 'TrialComparison'
fetch(x, ..., output = c("value", "named", "labeled", "list"))
```

## Arguments

- x:

  A `TrialComparison`.

- ...:

  Object names or integer positions, including vectors and multiple
  selections. Names may be quoted or unquoted. Positive positions count
  from the beginning and negative positions from the end. With no
  selection, the last object is returned. Quasi-quotation and
  programmatic selectors are supported; an unquoted name matching a
  contained object takes precedence over a variable of the same name in
  the calling environment.

- output:

  One of "value" (default), "named", "labeled", or "list". "value"
  returns a single selected object directly and multiple selections as a
  named list. The other modes always return a named list. Collection
  labels are the supplied object names, with positions for unnamed
  objects; consequently "labeled" and "named" are equivalent for
  comparisons.

## Value

A contained object by reference or a list of objects. An unknown name
returns NULL (retained as a NULL entry in list output). Empty selector
vectors return an empty list.

## Details

`fetch(x, "fixed")`, `fetch(x, fixed)` and `fetch(x, 1)` provide the
named and positional object access supported by `[[`. Only contained
objects are selected; R6 members are never returned as a fallback.
Numeric positions must be finite, nonzero integers within the collection
bounds. Selecting the last object of an empty comparison raises an index
error.

## See also

[`fetch()`](https://docs.rpact.org/reference/fetch.ParameterSet.md) and
[`getComparison()`](https://docs.rpact.org/reference/getComparison.md).

## Examples

``` r
comparison <- getComparison(fixed = getDesignFixed(),
    sequential = getDesignGroupSequential(kMax = 2))
identical(comparison |> fetch("fixed"), comparison[["fixed"]])
#> [1] TRUE
identical(comparison |> fetch(fixed), comparison$fixed)
#> [1] TRUE
identical(comparison |> fetch(1), comparison[[1]])
#> [1] TRUE
comparison |> fetch(-1)
#> Design parameters and output of group sequential design:
#> 
#> User defined parameters:
#>   Maximum number of stages                     : 2 
#>   Stages                                       : 1, 2 
#> 
#> Default parameters:
#>   Type of design                               : O'Brien & Fleming 
#>   Significance level                           : 0.0250 
#>   Type II error rate                           : 0.2000 
#>   Binding futility                             : FALSE 
#>   Test                                         : one-sided 
#>   Tolerance                                    : 1e-08 
#> 
#> Derived parameters:
#>   Information rates                            : 0.500, 1.000 
#>   Futility bounds (non-binding)                : -Inf 
#> 
#> Output:
#>   Cumulative alpha spending                    : 0.002583, 0.025000 
#>   Critical values                              : 2.797, 1.977 
#>   Stage levels (one-sided)                     : 0.002583, 0.023996 
#> 
comparison |> fetch(1:2, output = "list")
#> $fixed
#> Design parameters and output of fixed sample size design:
#> 
#> User defined parameters: not available
#> 
#> Default parameters:
#>   Significance level              : 0.0250 
#>   Type II error rate              : 0.2000 
#>   Test                            : one-sided 
#> 
#> Output:
#>   Critical values                 : 1.960 
#>   Stage levels (one-sided)        : 0.0250 
#> 
#> 
#> $sequential
#> Design parameters and output of group sequential design:
#> 
#> User defined parameters:
#>   Maximum number of stages                     : 2 
#>   Stages                                       : 1, 2 
#> 
#> Default parameters:
#>   Type of design                               : O'Brien & Fleming 
#>   Significance level                           : 0.0250 
#>   Type II error rate                           : 0.2000 
#>   Binding futility                             : FALSE 
#>   Test                                         : one-sided 
#>   Tolerance                                    : 1e-08 
#> 
#> Derived parameters:
#>   Information rates                            : 0.500, 1.000 
#>   Futility bounds (non-binding)                : -Inf 
#> 
#> Output:
#>   Cumulative alpha spending                    : 0.002583, 0.025000 
#>   Critical values                              : 2.797, 1.977 
#>   Stage levels (one-sided)                     : 0.002583, 0.023996 
#> 
#> 
```
