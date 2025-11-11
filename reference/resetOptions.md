# Reset Options

Resets the `rpact` options to their default values.

## Usage

``` r
resetOptions(persist = TRUE)
```

## Arguments

- persist:

  A logical value indicating whether the reset options should be saved
  persistently. If `TRUE`, the options will be saved after resetting.
  Default is `TRUE`.

## Value

Returns `TRUE` if the options were successfully reset, `FALSE`
otherwise.

## Details

This function resets all `rpact` options to their default values. If the
`persist` parameter is set to `TRUE`, the reset options will be saved to
a configuration file.

## Examples

``` r
if (FALSE) { # \dontrun{
resetOptions()
resetOptions(persist = FALSE)
} # }
```
