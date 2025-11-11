# Get Parameter Name

Returns the parameter name for a given object and parameter caption.

## Usage

``` r
getParameterName(obj, parameterCaption)
```

## Arguments

- obj:

  The rpact result object.

- parameterCaption:

  The parameter caption.

## Value

Returns a [`character`](https://rdrr.io/r/base/character.html) of
specifying the corresponding name of a given parameter caption. Returns
`NULL` if the specified `parameterCaption` does not exist.

## Details

This function identifies and returns the parameter name for a given
caption that will be used in print outputs of an rpact result object.

## See also

[`getParameterCaption()`](https://rpact-com.github.io/rpact/reference/getParameterCaption.md)
for getting the parameter caption for a given name.

## Examples

``` r
if (FALSE) { # \dontrun{
getParameterName(getDesignInverseNormal(), "Maximum number of stages")
} # }
```
