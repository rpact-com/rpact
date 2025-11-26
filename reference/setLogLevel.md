# Set Log Level

Sets the `rpact` log level.

## Usage

``` r
setLogLevel(
  logLevel = c("PROGRESS", "ERROR", "WARN", "INFO", "DEBUG", "TRACE", "DISABLED")
)
```

## Arguments

- logLevel:

  The new log level to set. Can be one of "PROGRESS", "ERROR", "WARN",
  "INFO", "DEBUG", "TRACE", "DISABLED". Default is "PROGRESS".

## Details

This function sets the log level of the `rpact` internal log message
system. By default only calculation progress messages will be shown on
the output console, particularly
[`getAnalysisResults()`](https://docs.rpact.org/reference/getAnalysisResults.md)
shows this kind of messages. The output of these messages can be
disabled by setting the log level to `"DISABLED"`.

## See also

- [`getLogLevel()`](https://docs.rpact.org/reference/getLogLevel.md) for
  getting the current log level,

- [`resetLogLevel()`](https://docs.rpact.org/reference/resetLogLevel.md)
  for resetting the log level to default.

## Examples

``` r
if (FALSE) { # \dontrun{
# show debug messages
setLogLevel("DEBUG")

# disable all log messages
setLogLevel("DISABLED")
} # }
```
