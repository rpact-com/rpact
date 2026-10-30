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
  keepSourceFiles = FALSE,
  reportFileBaseName = "rpact_test_result_report",
  metaData = list(),
  metaDataFile = NULL,
  workers = NULL
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

- reportFileBaseName:

  The base name for the report files (without path or extension).

- metaData:

  A named `list` containing additional metadata to be included in the
  test report. This can include information such as `company`,
  `container name`, `host name`, etc. Default is an empty list. For more
  information, see
  [`writeKeyValueFile`](https://docs.rpact.org/reference/writeKeyValueFile.md)
  and
  [`readKeyValueFile`](https://docs.rpact.org/reference/readKeyValueFile.md).

- metaDataFile:

  An optional path to a text file containing metadata: one entry per
  line in the form `KEY=VALUE`. For more information, see
  [`writeKeyValueFile`](https://docs.rpact.org/reference/writeKeyValueFile.md)
  and
  [`readKeyValueFile`](https://docs.rpact.org/reference/readKeyValueFile.md).

- workers:

  Number of parallel R test processes. The default `NULL` uses the
  available logical CPUs minus two, with a minimum of one and a maximum
  of 16, limited to the number of test files. Uses
  [`parallelly::availableCores()`](https://parallelly.futureverse.org/reference/availableCores.html)
  when installed (including resource limits), otherwise
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html).
  An explicit positive integer overrides the automatic selection; use
  `1` for serial execution. Parallel tests are supported on Windows,
  Linux, and macOS.

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

Unit test files are distributed across separate R processes. On Windows,
standard output and error use files to avoid the pipe-related access
violation in R bug 19104. Linux and macOS use the testthat parallel
runner. Scheduling priorities are read from the `DESCRIPTION` above a
local test directory, when present, and otherwise from the installed
rpact package. Report sections follow file completion order. Personal
rpact configuration files are isolated for the run. R basic package
tests run in a separate R process before parallel workers start, using a
temporary output directory. If execution aborts, partial results are
marked as incomplete and the diagnostic log path is printed; use
`workers = 1` to investigate persistent parallel-worker failures. The
returned object includes `workers` and `timings`, with elapsed seconds
for unit tests (including worker startup), R basic package tests, HTML
rendering, and PDF rendering.

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
