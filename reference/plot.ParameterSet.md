# Parameter Set Plotting

Plots an object that inherits from class
[`ParameterSet`](https://docs.rpact.org/reference/ParameterSet.md).

## Usage

``` r
# S3 method for class 'ParameterSet'
plot(
  x,
  y,
  ...,
  main = NA_character_,
  xlab = NA_character_,
  ylab = NA_character_,
  type = 1L,
  palette = "Set1",
  legendPosition = NA_integer_,
  showSource = FALSE,
  plotSettings = NULL
)
```

## Arguments

- x:

  The object that inherits from
  [`ParameterSet`](https://docs.rpact.org/reference/ParameterSet.md).

- y:

  Not available for this kind of plot (is only defined to be compatible
  to the generic plot function).

- ...:

  Optional plot arguments. At the moment `xlim` and `ylim` are
  implemented for changing x or y axis limits without dropping data
  observations.

- main:

  The main title.

- xlab:

  The x-axis label.

- ylab:

  The y-axis label.

- type:

  The plot type (default = 1).

- palette:

  The palette, default is `"Set1"`.

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

Generic function to plot a parameter set.
