# Get Plot Settings

Returns a plot settings object.

## Usage

``` r
getPlotSettings(
  lineSize = 0.8,
  pointSize = 3,
  pointColor = NA_character_,
  mainTitleFontSize = 14,
  axesTextFontSize = 10,
  legendFontSize = 11,
  scalingFactor = 1
)
```

## Arguments

- lineSize:

  The line size, default is `0.8`.

- pointSize:

  The point size, default is `3`.

- pointColor:

  The point color (character), default is `NA_character_`.

- mainTitleFontSize:

  The main title font size, default is `14`.

- axesTextFontSize:

  The axes text font size, default is `10`.

- legendFontSize:

  The legend font size, default is `11`.

- scalingFactor:

  The scaling factor, default is `1`.

## Details

Returns an object of class `PlotSettings` that collects typical plot
settings.
