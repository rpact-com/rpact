# Print Installation Qualification Result

This function prints the details of an `InstallationQualificationResult`
object in a user-friendly format.

## Usage

``` r
# S3 method for class 'InstallationQualificationResult'
print(x, ...)
```

## Arguments

- x:

  An object of class `InstallationQualificationResult` containing the
  results of the installation qualification.

- ...:

  Additional arguments passed to or from other methods.

## Value

This function does not return a value. It is called for its side effects
of printing the result.

## Details

The function displays the result message, followed by the parameters and
their values. It skips parameters with `NULL` or `NA` values.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- testPackage()
print(result)
} # }
```
