# Write Dataset

Writes a dataset to a CSV file.

## Usage

``` r
writeDataset(
  dataset,
  file,
  ...,
  append = FALSE,
  quote = TRUE,
  sep = ",",
  eol = "\n",
  na = "NA",
  dec = ".",
  row.names = TRUE,
  col.names = NA,
  qmethod = "double",
  fileEncoding = "UTF-8"
)
```

## Arguments

- dataset:

  A dataset.

- file:

  The target CSV file.

- ...:

  Further arguments to be passed to
  [`write.table`](https://rdrr.io/r/utils/write.table.html).

- append:

  Logical. Only relevant if file is a character string. If `TRUE`, the
  output is appended to the file. If `FALSE`, any existing file of the
  name is destroyed.

- quote:

  The set of quoting characters. To disable quoting altogether, use
  quote = "". See scan for the behavior on quotes embedded in quotes.
  Quoting is only considered for columns read as character, which is all
  of them unless `colClasses` is specified.

- sep:

  The field separator character. Values on each line of the file are
  separated by this character. If sep = "," (the default for
  `writeDataset`) the separator is a comma.

- eol:

  The character(s) to print at the end of each line (row).

- na:

  The string to use for missing values in the data.

- dec:

  The character used in the file for decimal points.

- row.names:

  Either a logical value indicating whether the row names of `dataset`
  are to be written along with `dataset`, or a character vector of row
  names to be written.

- col.names:

  Either a logical value indicating whether the column names of
  `dataset` are to be written along with `dataset`, or a character
  vector of column names to be written. See the section on 'CSV files'
  for the meaning of `col.names = NA`.

- qmethod:

  A character string specifying how to deal with embedded double quote
  characters when quoting strings. Must be one of "double" (default in
  `writeDataset`) or "escape".

- fileEncoding:

  Character string: if non-empty declares the encoding used on a file
  (not a connection) so the character data can be re-encoded. See the
  'Encoding' section of the help for file, the 'R Data Import/Export
  Manual' and 'Note'.

## Details

`writeDataset()` is a wrapper function that coerces the dataset to a
data frame and uses  
[`write.table`](https://rdrr.io/r/utils/write.table.html) to write it to
a CSV file.

## See also

- [`writeDatasets()`](https://docs.rpact.org/reference/writeDatasets.md)
  for writing multiple datasets,

- [`readDataset()`](https://docs.rpact.org/reference/readDataset.md) for
  reading a single dataset,

- [`readDatasets()`](https://docs.rpact.org/reference/readDatasets.md)
  for reading multiple datasets.

## Examples

``` r
if (FALSE) { # \dontrun{
datasetOfRates <- getDataset(
    n1 = c(11, 13, 12, 13),
    n2 = c(8, 10, 9, 11),
    events1 = c(10, 10, 12, 12),
    events2 = c(3, 5, 5, 6)
)
writeDataset(datasetOfRates, "dataset_rates.csv")
} # }
```
