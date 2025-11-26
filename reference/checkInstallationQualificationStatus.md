# Check Installation Qualification Status

This function checks whether the installation qualification for the
`rpact` package has been completed. If not, it provides a message
prompting the user to run the
[`testPackage()`](https://docs.rpact.org/reference/testPackage.md)
function to perform the qualification.

## Usage

``` r
checkInstallationQualificationStatus(showMessage = TRUE)
```

## Arguments

- showMessage:

  A logical value indicating whether to display a message if the
  installation qualification has not been completed. Default is `TRUE`.

## Value

Invisibly returns `TRUE` if the installation qualification has been
completed, otherwise returns `FALSE`.

## Details

The installation qualification is a critical step in ensuring that the
`rpact` package is correctly installed and validated for use in
GxP-relevant environments. This function verifies the qualification
status and informs the user if further action is required.

## Examples

``` r
if (FALSE) { # \dontrun{
checkInstallationQualificationStatus()
} # }
```
