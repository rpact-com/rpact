# Read Dataset

Reads a data file and returns it as dataset object.

## Usage

``` r
readDataset(
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
  `readDataset`) the separator is a comma.

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

Returns a [`Dataset`](https://docs.rpact.org/reference/Dataset.md)
object. The following generics (R generic functions) are available for
this result object:

- [`names()`](https://docs.rpact.org/reference/names.FieldSet.md) to
  obtain the field names,

- [`print()`](https://docs.rpact.org/reference/print.FieldSet.md) to
  print the object,

- [`summary()`](https://docs.rpact.org/reference/summary.ParameterSet.md)
  to display a summary of the object,

- [`plot()`](https://docs.rpact.org/reference/plot.Dataset.md) to plot
  the object,

- [`as.data.frame()`](https://docs.rpact.org/reference/as.data.frame.ParameterSet.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://docs.rpact.org/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

`readDataset` is a wrapper function that uses
[`read.table`](https://rdrr.io/r/utils/read.table.html) to read the CSV
file into a data frame, transfers it from long to wide format with
[`reshape`](https://rdrr.io/r/stats/reshape.html) and puts the data to
[`getDataset()`](https://docs.rpact.org/reference/getDataset.md).

## See also

- [`readDatasets()`](https://docs.rpact.org/reference/readDatasets.md)
  for reading multiple datasets,

- [`writeDataset()`](https://docs.rpact.org/reference/writeDataset.md)
  for writing a single dataset,

- [`writeDatasets()`](https://docs.rpact.org/reference/writeDatasets.md)
  for writing multiple datasets.

## Examples

``` r
if (FALSE) { # \dontrun{
dataFileRates <- system.file("extdata",
    "dataset_rates.csv",
    package = "rpact"
)
if (dataFileRates != "") {
    datasetRates <- readDataset(dataFileRates)
    datasetRates
}

dataFileMeansMultiArm <- system.file("extdata",
    "dataset_means_multi-arm.csv",
    package = "rpact"
)
if (dataFileMeansMultiArm != "") {
    datasetMeansMultiArm <- readDataset(dataFileMeansMultiArm)
    datasetMeansMultiArm
}

dataFileRatesMultiArm <- system.file("extdata",
    "dataset_rates_multi-arm.csv",
    package = "rpact"
)
if (dataFileRatesMultiArm != "") {
    datasetRatesMultiArm <- readDataset(dataFileRatesMultiArm)
    datasetRatesMultiArm
}

dataFileSurvivalMultiArm <- system.file("extdata",
    "dataset_survival_multi-arm.csv",
    package = "rpact"
)
if (dataFileSurvivalMultiArm != "") {
    datasetSurvivalMultiArm <- readDataset(dataFileSurvivalMultiArm)
    datasetSurvivalMultiArm
}
} # }
```
