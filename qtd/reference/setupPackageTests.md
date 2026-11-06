# Setup Package Tests

This function sets up the package tests by downloading the test files
and copying them to the rpact installation directory.

## Usage

``` r
setupPackageTests(token, secret)
```

## Arguments

- token:

  A character string representing the token for authentication.

- secret:

  A character string representing the secret for authentication.

## Value

The function returns `TRUE` if all test files are downloaded and copied
successfully to the rpact installation directory; otherwise, it returns
`FALSE`.

## Details

The function first checks if the `rpact` package directory and its
`tests` and `testthat` subdirectories exist. If they do not exist, it
stops with an error. It then downloads the test files to a temporary
directory and copies them to the `tests` directory of the `rpact`
package. If all test files are copied successfully, it removes the
default test file.

## References

For more information, please visit:
<https://www.rpact.org/vignettes/utilities/rpact_installation_qualification/>
