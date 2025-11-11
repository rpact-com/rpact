# Disable Startup Messages

This function disables the startup messages for the `rpact` package by
setting the `rpact.startup.message.enabled` option to `FALSE`.

## Usage

``` r
disableStartupMessages()
```

## Value

This function does not return a value. It is called for its side
effects.

## Details

Once this function is called, the startup messages will remain disabled
until explicitly re-enabled using the
[`enableStartupMessages()`](https://rpact-com.github.io/rpact/reference/enableStartupMessages.md)
function. The current state is saved using the
[`saveOptions()`](https://rpact-com.github.io/rpact/reference/saveOptions.md)
function.

## Examples

``` r
if (FALSE) { # \dontrun{
disableStartupMessages()
} # }
```
