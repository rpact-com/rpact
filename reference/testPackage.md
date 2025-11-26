# Test and Validate the rpact Package Installation

This function ensures the correct installation of the `rpact` package by
performing various tests. It supports a comprehensive validation
process, essential for GxP compliance and other regulatory requirements.

## Usage

``` r
testPackage(
  outDir = ".",
  ...,
  completeUnitTestSetEnabled = TRUE,
  connection = list(token = NULL, secret = NULL),
  testFileDirectory = NA_character_,
  downloadTestsOnly = FALSE,
  addWarningDetailsToReport = TRUE,
  reportType = c("compact", "detailed", "Rout"),
  testInstalledBasicPackages = TRUE,
  scope = c("basic", "devel", "both", "internet", "all"),
  openHtmlReport = TRUE,
  keepSourceFiles = FALSE
)
```

## Arguments

- outDir:

  The absolute path to the output directory where all test results will
  be saved. By default, the current working directory is used.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- completeUnitTestSetEnabled:

  If `TRUE` (default), all existing unit tests will be executed; if
  `FALSE`, only a subset of tests is run.

- connection:

  A `list` allowing owners of the rpact validation documentation to
  provide `token` and `secret` credentials for full access to unit
  tests, enabling them to meet regulatory requirements (see
  [www.rpact.com](https://www.rpact.com) for more information).

- testFileDirectory:

  An optional path pointing to a local directory containing test files.

- downloadTestsOnly:

  If `TRUE`, the unit test files are only downloaded and not executed.
  Default is `FALSE`.

- addWarningDetailsToReport:

  If `TRUE`, additional warning details are included in the test report.
  Default is `TRUE`.

- reportType:

  The type of report to generate. Can be `"compact"`, `"detailed"`, or
  `"Rout"`.

- testInstalledBasicPackages:

  If `TRUE`, tests for installed basic R packages are included, default
  is `TRUE`. For more information, see
  [`testInstalledBasic`](https://rdrr.io/r/tools/testInstalledPackage.html).

- scope:

  The scope of the basic R package tests to run. Can be `"basic"`,
  `"devel"`, `"both"`, `"internet"`, or `"all"`. Default is `"basic"`.
  For more information, see
  [`testInstalledBasic`](https://rdrr.io/r/tools/testInstalledPackage.html).
  Only available if `testInstalledBasicPackages = TRUE`.

- openHtmlReport:

  If `TRUE`, the HTML report is opened after the tests are completed,
  default is `TRUE`.

- keepSourceFiles:

  If `TRUE`, the source files are kept after the tests are completed. A
  copy of them can be found in the subdirectory `src`.

## Value

Invisibly returns an
[`InstallationQualificationResult`](https://docs.rpact.org/reference/InstallationQualificationResult.md))
object.

## Details

This function is integral to the installation qualification (IQ) process
of the `rpact` package, ensuring it meets quality standards and
functions as expected. A directory named `rpact-tests` is created within
the specified output directory, where all test files are downloaded from
a secure resource and executed. Results are saved in the file
`testthat.Rout`, located in the `rpact-tests` directory.

Installation qualification is a critical step in the validation process.
Without successful IQ, the package cannot be considered fully validated.
To gain access to the full set of unit tests, users must provide `token`
and `secret` credentials, which are distributed to members of the rpact
user group as part of the validation documentation. For more
information, see vignette
[rpact_installation_qualification](https://www.rpact.org/vignettes/utilities/rpact_installation_qualification/).

## References

For more information, please visit:
<https://www.rpact.org/vignettes/utilities/rpact_installation_qualification/>

## Examples

``` r
if (FALSE) { # \dontrun{
# Set the output directory
setwd("/path/to/output")

# Basic usage
testPackage()

# Perform all unit tests with access credentials
testPackage(
    connection = list(
        token = "your_token_here",
        secret = "your_secret_here"
    )
)

# Download test files without executing them
testPackage(downloadTestsOnly = TRUE)
} # }
```
