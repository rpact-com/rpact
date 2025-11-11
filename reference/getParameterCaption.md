# Get Parameter Caption

Returns the parameter caption for a given object and parameter name.

## Usage

``` r
getParameterCaption(obj, var)
```

## Arguments

- obj:

  The rpact result object.

- var:

  The variable/parameter name.

## Value

Returns a [`character`](https://rdrr.io/r/base/character.html) of
specifying the corresponding caption of a given parameter name. Returns
`NULL` if the specified `parameterName` does not exist.

## Details

This function identifies and returns the caption that will be used in
print outputs of an rpact result object.

## See also

[`getParameterName()`](https://rpact-com.github.io/rpact/reference/getParameterName.md)
for getting the parameter name for a given caption.

## Examples

``` r
if (FALSE) { # \dontrun{
getParameterCaption(getDesignInverseNormal(), "kMax")
} # }
```
