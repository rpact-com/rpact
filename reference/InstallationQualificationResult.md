# Installation Qualification Result Object

This object represents the structured result of a full or partial
installation qualification test execution. It includes metadata about
the executed test suite, paths used, summary statistics, and status
messages.

## Format

An S3 object of class `InstallationQualificationResult` with the
following elements:

- completeUnitTestSetEnabled:

  Logical indicating whether the full test set was enabled

- testFileDirectory:

  Directory containing test scripts

- testFileTargetDirectory:

  Directory to which tests are copied or linked

- reportType:

  Report type selected (`"compact"`, `"detailed"`, or `"Rout"`)

- executionMode:

  Execution mode (`"default"`, `"downloadOnly"`,
  `"downloadAndRunTests"`, or `"runTestsInTestFileDirectory"`)

- scope:

  Scope of the qualification (`"basic"`, `"devel"`, `"both"`,
  `"internet"`, or `"all"`)

- resultDir:

  Directory where the result reports are stored

- resultOuputFile:

  Main output report filename

- reportFileNames:

  Vector of report files generated

- minNumberOfExpectedTests:

  Minimum number of expected tests

- totalNumberOfTests:

  Number of tests actually run

- numberOfFailedTests:

  Number of failed tests

- numberOfSkippedTests:

  Number of skipped tests

- resultMessage:

  Message summarizing the result

- statusMessage:

  Detailed status message

- status:

  Overall result status (`"success"`, `"incomplete"`, or `"failed"`)

## Details

The object is returned by the function
[`testPackage`](https://rpact-com.github.io/rpact/reference/testPackage.md)
and is of class `InstallationQualificationResult`.

## See also

[`testPackage`](https://rpact-com.github.io/rpact/reference/testPackage.md)
