# Number Of Subjects Plotting

Plots an object that inherits from class
[`NumberOfSubjects`](https://docs.rpact.org/reference/NumberOfSubjects.md).

## Usage

``` r
# S3 method for class 'NumberOfSubjects'
plot(
  x,
  y,
  ...,
  allocationRatioPlanned = NA_real_,
  main = NA_character_,
  xlab = NA_character_,
  ylab = NA_character_,
  type = 1L,
  legendTitle = NA_character_,
  palette = "Set1",
  plotPointsEnabled = NA,
  legendPosition = NA_integer_,
  showSource = FALSE,
  plotSettings = NULL
)
```

## Arguments

- x:

  The object that inherits from
  [`NumberOfSubjects`](https://docs.rpact.org/reference/NumberOfSubjects.md).

- y:

  An optional object that inherits from
  [`EventProbabilities`](https://docs.rpact.org/reference/EventProbabilities.md).

- ...:

  Optional plot arguments. At the moment `xlim` and `ylim` are
  implemented for changing x or y axis limits without dropping data
  observations.

- allocationRatioPlanned:

  The planned allocation ratio `n1 / n2` for a two treatment groups
  design, default is `1`. Will be ignored if `y` is undefined.

- main:

  The main title.

- xlab:

  The x-axis label.

- ylab:

  The y-axis label.

- type:

  The plot type (default = 1). Note that at the moment only one type is
  available.

- legendTitle:

  The legend title, default is `""`.

- palette:

  The palette, default is `"Set1"`.

- plotPointsEnabled:

  Logical. If `TRUE`, additional points will be plotted.

- legendPosition:

  The position of the legend. By default (`NA_integer_`) the algorithm
  tries to find a suitable position. Choose one of the following values
  to specify the position manually:

  - `-1`: no legend will be shown

  - `NA`: the algorithm tries to find a suitable position

  - `0`: legend position outside plot

  - `1`: legend position left top

  - `2`: legend position left center

  - `3`: legend position left bottom

  - `4`: legend position right top

  - `5`: legend position right center

  - `6`: legend position right bottom

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

Generic function to plot an "number of subjects" object.

Generic function to plot a "number of subjects" object.
