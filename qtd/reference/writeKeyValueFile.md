# Write a key-value file (KEY=VALUE) from a named list

Writes a human-editable text file in a widely used key-value format
(INI/.env-like): one entry per line in the form `KEY=VALUE`. Blank lines
and comment lines (starting with `#` or `;`) are allowed. Values are
written as a single line; special characters are escaped so that the
file remains one key per line.

## Usage

``` r
writeKeyValueFile(
  keyValueList,
  filePath,
  ...,
  writeHeader = TRUE,
  sortKeys = FALSE,
  overwrite = TRUE,
  safeKeyCheck = TRUE
)
```

## Arguments

- keyValueList:

  A named list of scalar (length-1) atomic values. Supported value
  types: character, logical, integer, numeric. `NA` is supported and
  written as `NA`.

- filePath:

  Path to the output file (e.g. `"inst/tests/META.env"`).

- ...:

  Currently unused.

- writeHeader:

  Logical; if `TRUE`, writes a short header comment.

- sortKeys:

  Logical; if `TRUE`, keys are written in alphabetical order.

- overwrite:

  Logical; if `FALSE` and the file exists, an error is raised.

- safeKeyCheck:

  Logical; if `TRUE`, checks that keys only contain allowed characters.
  Set to `FALSE` to allow arbitrary keys, but be aware that this may
  lead to issues when reading the file back, as keys with special
  characters may not be parsed correctly.

## Value

Invisibly returns `filePath`.

## Details

Keys are restricted to `[A-Za-z0-9_.-]` to keep the file portable and
easy to edit.

UTF-8 handling: Values are normalized to UTF-8 on write. The file is
written with UTF-8 encoding when supported by the platform. On Windows,
UTF-8 is enforced via `fileEncoding = "UTF-8"`. On other platforms,
UTF-8 is typically the native encoding; we still normalize strings to
UTF-8.

## Examples

``` r
if (FALSE) { # \dontrun{
keyValueList <- list(
    STUDY_NAME = "Trial A",
    MAX_PATIENTS = 150L,
    THRESHOLD = 0.075,
    NOTES = "First phase\nSecond phase"
)
filePath <- tempfile(fileext = ".txt")
writeKeyValueFile(
    keyValueList = keyValueList,
    filePath = filePath,
    writeHeader = TRUE,
    sortKeys = TRUE,
    overwrite = TRUE
)
} # }
```
