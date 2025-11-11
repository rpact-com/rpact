# Save Options

Saves the current `rpact` options to a configuration file.

## Usage

``` r
saveOptions()
```

## Value

Returns `TRUE` if the options were successfully saved, `FALSE`
otherwise.

## Details

This function attempts to save the current `rpact` options to a
configuration file located in the user's configuration directory. If the
`rappdirs` package is not installed, the function will not perform any
action. The options are saved in a YAML file.

## Examples

``` r
if (FALSE) { # \dontrun{
saveOptions()
} # }
```
