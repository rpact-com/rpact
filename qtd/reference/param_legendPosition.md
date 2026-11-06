# Parameter Description: Legend Position on Plots

Controls automatic or explicit placement of plot legends.

## Arguments

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
