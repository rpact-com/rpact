# Dataset Plotting

Plots a dataset.

## Usage

``` r
# S3 method for class 'Dataset'
plot(
  x,
  y,
  ...,
  main = "Dataset",
  xlab = "Stage",
  ylab = NA_character_,
  legendTitle = "Group",
  palette = "Set1",
  showSource = FALSE,
  plotSettings = NULL
)
```

## Arguments

- x:

  The [`Dataset`](https://docs.rpact.org/reference/Dataset.md) object to
  plot.

- y:

  Not available for this kind of plot (is only defined to be compatible
  to the generic plot function).

- ...:

  Optional plot arguments. At the moment `xlim` and `ylim` are
  implemented for changing x or y axis limits without dropping data
  observations.

- main:

  The main title, default is `"Dataset"`.

- xlab:

  The x-axis label, default is `"Stage"`.

- ylab:

  The y-axis label.

- legendTitle:

  The legend title, default is `"Group"`.

- palette:

  The palette, default is `"Set1"`.

- showSource:

  Logical. If `TRUE`, the parameter names of the object will be printed
  which were used to create the plot; that may be, e.g., useful to check
  the values or to create own plots with the base R `plot` function.
  Alternatively `showSource` can be defined as one of the following
  character values:

  - `"commands"`: returns a character vector with plot commands

  - `"axes"`: returns a list with the axes definitions

  - `"test"`: all plot commands will be validated with `eval(parse())`
    and returned as character vector (function does not stop if an error
    occurs)

  - `"validate"`: all plot commands will be validated with
    `eval(parse())` and returned as character vector (function stops if
    an error occurs)

  Note: no plot object will be returned if `showSource` is a character.

- plotSettings:

  An object of class `PlotSettings` created by
  [`getPlotSettings()`](https://docs.rpact.org/reference/getPlotSettings.md).

## Value

Returns a `ggplot2` object.

## Details

Generic function to plot all kinds of datasets.

## Examples

``` r
if (FALSE) { # \dontrun{
# Plot a dataset of means
dataExample <- getDataset(
    n1 = c(22, 11, 22, 11),
    n2 = c(22, 13, 22, 13),
    means1 = c(1, 1.1, 1, 1),
    means2 = c(1.4, 1.5, 3, 2.5),
    stDevs1 = c(1, 2, 2, 1.3),
    stDevs2 = c(1, 2, 2, 1.3)
)
if (require(ggplot2)) plot(dataExample, main = "Comparison of Means")

# Plot a dataset of rates
dataExample <- getDataset(
    n1 = c(8, 10, 9, 11),
    n2 = c(11, 13, 12, 13),
    events1 = c(3, 5, 5, 6),
    events2 = c(8, 10, 12, 12)
)
if (require(ggplot2)) plot(dataExample, main = "Comparison of Rates")
} # }
```
