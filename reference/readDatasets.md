# Read Multiple Datasets

Reads a data file and returns it as a list of dataset objects.

## Usage

``` r
readDatasets(
  file,
  ...,
  header = TRUE,
  sep = ",",
  quote = "\"",
  dec = ".",
  fill = TRUE,
  comment.char = "",
  fileEncoding = "UTF-8"
)
```

## Arguments

- file:

  A CSV file (see
  [`read.table`](https://rdrr.io/r/utils/read.table.html)).

- ...:

  Further arguments to be passed to
  [`read.table`](https://rdrr.io/r/utils/read.table.html).

- header:

  A logical value indicating whether the file contains the names of the
  variables as its first line.

- sep:

  The field separator character. Values on each line of the file are
  separated by this character. If sep = "," (the default for
  `readDatasets`) the separator is a comma.

- quote:

  The set of quoting characters. To disable quoting altogether, use
  quote = "". See scan for the behavior on quotes embedded in quotes.
  Quoting is only considered for columns read as character, which is all
  of them unless `colClasses` is specified.

- dec:

  The character used in the file for decimal points.

- fill:

  logical. If `TRUE` then in case the rows have unequal length, blank
  fields are implicitly added.

- comment.char:

  character: a character vector of length one containing a single
  character or an empty string. Use "" to turn off the interpretation of
  comments altogether.

- fileEncoding:

  character string: if non-empty declares the encoding used on a file
  (not a connection) so the character data can be re-encoded. See the
  'Encoding' section of the help for file, the 'R Data Import/Export
  Manual' and 'Note'.

## Value

Returns a [`list`](https://rdrr.io/r/base/list.html) of
[`Dataset`](https://rpact-com.github.io/rpact/reference/Dataset.md)
objects.

## Details

Reads a file that was written by
[`writeDatasets()`](https://rpact-com.github.io/rpact/reference/writeDatasets.md)
before.

## See also

- [`readDataset()`](https://rpact-com.github.io/rpact/reference/readDataset.md)
  for reading a single dataset,

- [`writeDatasets()`](https://rpact-com.github.io/rpact/reference/writeDatasets.md)
  for writing multiple datasets,

- [`writeDataset()`](https://rpact-com.github.io/rpact/reference/writeDataset.md)
  for writing a single dataset.

## Examples

``` r
if (FALSE) { # \dontrun{
dataFile <- system.file("extdata", "datasets_rates.csv", package = "rpact")
if (dataFile != "") {
    datasets <- readDatasets(dataFile)
    datasets
}
} # }
```
