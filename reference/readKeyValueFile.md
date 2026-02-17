# Read a key-value file (KEY=VALUE) into a named list

Reads a human-editable key-value file in a widely used format
(INI/.env-like): `KEY=VALUE`. Blank lines are ignored. Full-line
comments starting with `#` or `;` are ignored. Inline comments are
supported for unquoted values when preceded by whitespace, e.g.,
`KEY=123 # comment`.

## Usage

``` r
readKeyValueFile(
  filePath,
  ...,
  inferTypes = TRUE,
  duplicateKeys = c("error", "last", "first"),
  safeKeyCheck = FALSE
)
```

## Arguments

- filePath:

  Path to the key-value file.

- ...:

  Currently unused.

- inferTypes:

  Logical; if `TRUE`, attempts to convert values to
  logical/integer/numeric and `NA`. If `FALSE`, returns character
  values.

- duplicateKeys:

  How to handle duplicate keys: `"error"` (default), `"last"` (keep last
  occurrence), or `"first"` (keep first occurrence).

- safeKeyCheck:

  Logical; if `TRUE`, checks that keys only contain allowed characters.
  Set to `FALSE` (default) to allow arbitrary keys, but be aware that
  this may lead to issues when reading the file back, as keys with
  special characters may not be parsed correctly.

## Value

A named list with parsed values.

## Details

Values can be quoted with double quotes. Escape sequences `\n`, `\r`,
`\t`, `\\`, and `\"` are supported.

UTF-8 handling: The file is read as UTF-8 and all character values are
normalized to UTF-8 via
[`enc2utf8()`](https://rdrr.io/r/base/Encoding.html).

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
readKeyValueFile(filePath)
} # }
```
