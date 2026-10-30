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
  legendPosition = "auto",
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

  The legend position. The default, `"auto"`, measures the legend and
  chooses among six inside positions by minimizing overlap with points,
  line segments, reference lines, bars, boxes, and error bars. Long
  labels are wrapped and entries arranged vertically in automatic mode.
  Oversized legends and faceted plots use the outside right position.
  This heuristic uses the current graphics device (seven inches square
  when no device is open); resizing may require recreating the plot or
  setting a position explicitly. Manual positions are:

  - `-1` or `"none"`: hide the legend

  - `"left"`, `"right"`, `"bottom"`, `"top"`: outside the plot

  - `0`: default outside position with a square aspect ratio

  - `1`, `2`, `3`: inside left, at the top, centre, or bottom

  - `4`, `5`, `6`: inside right, at the top, centre, or bottom

  - `NA`: retain the legacy plot-specific positioning rules

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
