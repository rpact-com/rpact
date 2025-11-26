# Enable Startup Messages

This function enables the startup messages for the `rpact` package by
setting the `rpact.startup.message.enabled` option to `TRUE`.

## Usage

``` r
enableStartupMessages()
```

## Value

This function does not return a value. It is called for its side
effects.

## Details

Once this function is called, the startup messages will remain enabled
until explicitly disabled using the
[`disableStartupMessages()`](https://docs.rpact.org/reference/disableStartupMessages.md)
function. The current state is saved using the
[`saveOptions()`](https://docs.rpact.org/reference/saveOptions.md)
function.

## Examples

``` r
if (FALSE) { # \dontrun{
enableStartupMessages()
} # }
```
