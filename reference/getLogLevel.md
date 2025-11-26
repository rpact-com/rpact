# Get Log Level

Returns the current `rpact` log level.

## Usage

``` r
getLogLevel()
```

## Value

Returns a [`character`](https://rdrr.io/r/base/character.html) of length
1 specifying the current log level.

## Details

This function gets the log level of the `rpact` internal log message
system.

## See also

- [`setLogLevel()`](https://docs.rpact.org/reference/setLogLevel.md) for
  setting the log level,

- [`resetLogLevel()`](https://docs.rpact.org/reference/resetLogLevel.md)
  for resetting the log level to default.

## Examples

``` r
# show current log level
getLogLevel()
#> [1] "PROGRESS"
```
