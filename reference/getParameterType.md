# Get Parameter Type

Returns the parameter type for a given object and parameter name.

## Usage

``` r
getParameterType(obj, var)
```

## Arguments

- obj:

  The rpact result object.

- var:

  The variable/parameter name.

## Value

Returns a [`character`](https://rdrr.io/r/base/character.html) of
specifying the corresponding type of a given parameter name. Returns
`NULL` if the specified `parameterName` does not exist.

## Details

This function identifies and returns the type that will be used in print
outputs of an rpact result object.

## See also

[`getParameterName()`](https://rpact-com.github.io/rpact/reference/getParameterName.md)
for getting the parameter name for a given caption.
[`getParameterCaption()`](https://rpact-com.github.io/rpact/reference/getParameterCaption.md)
for getting the parameter caption for a given name.

## Examples

``` r
if (FALSE) { # \dontrun{
getParameterType(getDesignInverseNormal(), "kMax")
} # }
```
